/// This module contains everything need for constructing a Unit, with its data
/// flow graph and its control flow graph.

use std::collections::{HashMap, HashSet};

use unit::{Unit, UnitId};
use data_flow::{Instruction, Opcode, Value};
use control_flow::{Sequence, SequenceIndex, SuccessorIndex};
use types::{ComplexType, ComplexTypeId};
use context;

/// A UnitContext should be used across multiple `UnitBuilder`, in order to have
/// different indexes and hashes for identical data. While this is not a
/// critical issue, this would cause additional hash collisions on Rehash
/// instructions when inlining.
pub struct ContextBuilder {
    /// Context which is being built by the context builder.
    ctx: context::Context,

    /// This HashMap is made to avoid the duplication of too many types, by
    /// making sure we have a unique identifier in the vector of types.
    types_lookup: HashMap<ComplexType, ComplexTypeId>,
}

pub struct UnitBuilder<'a> {
    /// Identifier of the constructed unit.
    unit: Unit,
    /// Context used for building the current unit.
    ctx: &'a mut ContextBuilder,
    /// Sequence which is currently being editted.
    sequence: Option<SequenceIndex>,
    /// List of sequences which are frozen, which implies that no predecessors
    /// should be added to them.
    frozen_seqs: HashSet<SequenceIndex>,
}

impl ContextBuilder {
    pub fn new() -> ContextBuilder {
        ContextBuilder {
            ctx: context::Context::new(),
            types_lookup: HashMap::new()
        }
    }

    pub fn get_rehash(&mut self) -> Opcode {
        Opcode::Rehash(self.ctx.get_hash_seed())
    }
    pub fn get_newhash(&mut self) -> Opcode {
        Opcode::Newhash(self.ctx.get_hash_seed())
    }

    /// Add a type and reuse a type which already got registered if any.
    pub fn add_type(&mut self, ty: ComplexType) -> ComplexTypeId {
        match self.types_lookup.get(&ty) {
            Some(id) => return *id,
            None => (),
        };
        let id = self.ctx.add_type(ty);
        // TODO: Can we avoid cloning here? Or maybe restrict it only to scalar
        // and vector types.
        let ty = self.ctx.get_type(id).clone();
        self.types_lookup.insert(ty, id);
        id
    }

    /// Add a type which would not be handled by the hash table, but would add
    /// the ability to be replaced with get_type_mut later.
    pub fn add_type_unshared(&mut self, ty: ComplexType) -> ComplexTypeId {
        self.ctx.add_type(ty)
    }

    /// Get a reference to the type corresponding to the given id.
    pub fn get_type(&self, id: ComplexTypeId) -> &ComplexType {
        self.ctx.get_type(id)
    }

    /// Finalize and return the context which hold the type information of
    /// multiple Units.
    pub fn finish(self) -> context::Context {
        self.ctx
    }
}

impl<'a> UnitBuilder<'a> {
    /// Create a new UnitBuilder, which will populate the Unit information
    /// incrementally.
    pub fn new(id: UnitId, ctx: &'a mut ContextBuilder) -> UnitBuilder<'a> {
        UnitBuilder {
            unit: Unit::new(id),
            ctx,
            sequence: None,
            frozen_seqs: HashSet::new(),
        }
    }

    /// Context accessor in order to be able to add additional types without
    /// repeating each ContextBuilder function.
    pub fn ctx(&mut self) -> &mut ContextBuilder { self.ctx }

    /// Set the signature of the Unit, and allocate the corresponding Values.
    /// This function does not create SSA bindings for the arguments.
    pub fn set_signature(&mut self, signature: ComplexTypeId) {
        self.unit.sig = signature;
        let ty = self.ctx.get_type(signature);
        let (ins, outs) = match ty {
            &ComplexType::Function(ref ins, ref outs, _) => (ins, outs),
            _ => panic!("Unit signatures are expected to be a Function.")
        };
        self.unit.inputs = ins.iter().map(|_| Value::dummy()).collect();
        self.unit.outputs = outs.iter().map(|_| Value::dummy()).collect();
    }

    /// Once the signature is defined with `set_signature`, and a block is
    /// entered. This function can be used to create a new SSA value for the
    /// argument at the index `arg_index`. If the argument was created
    /// previously, it would be reused.
    pub fn unit_arg(&mut self, arg_index: usize) -> Value {
        let arg = self.unit.inputs[arg_index];
        if !arg.is_dummy() {
            return arg
        }
        let opcode = self.ctx.get_newhash();
        let arg = self.dfg_add_ins(Instruction {
            opcode,
            operands: vec![],
            dependencies: vec![],
            replaced_by: None,
        });
        *(&mut self.unit.inputs[arg_index]) = arg;
        arg
    }

    /// Add one instruction in the data flow graph.
    fn dfg_add_ins(&mut self, ins: Instruction) -> Value {
        self.unit.dfg.add_ins(ins)
    }

    /// Create a new sequence to hold phi instructions and any effectful or
    /// garded instructions.
    pub fn create_sequence(&mut self) -> SequenceIndex {
        self.unit.cfg.sequences.push(Sequence::new());
        SequenceIndex(self.unit.cfg.sequences.len() - 1)
    }

    /// Switch to a sequence of code, such that newly added instructions are
    /// going to be added to this block by default.
    pub fn switch_to_sequence(&mut self, seq: SequenceIndex) {
        // TODO: Assert that the previous sequence is properly ended with a
        // control instruction, and a number of successors matching the number
        // the expected successors of the control instruction.
        self.sequence = Some(seq);
    }

    /// Add an instruction to both the data flow graph and the active sequence
    /// of the control flow graph.
    pub fn add_ins(&mut self, ins: Instruction) -> Value {
        let value = self.dfg_add_ins(ins);
        let SequenceIndex(index) = self.sequence.unwrap();
        debug_assert!(self.unit.cfg.sequences[index].control.is_dummy());
        self.unit.cfg.sequences[index].sequence.push(value);
        value
    }

    /// Add an instruction based on its opcode, operands and dependencies.
    pub fn add_op_deps(&mut self, opcode: Opcode, operands: &[Value], dependencies: &[Value]) -> Value {
        self.add_ins(Instruction {
            opcode,
            operands: operands.iter().map(|x| *x).collect(),
            dependencies: dependencies.iter().map(|x| *x).collect(),
            replaced_by: None,
        })
    }

    /// Add an instruction based only on its opcode, this function creates a
    /// conservative aliasing between load, store, calls and units.
    pub fn add_op(&mut self, opcode: Opcode, operands: &[Value]) -> Value {
        self.add_op_deps(opcode, operands, &[])
    }

    /// Add a control flow instruction to end the current sequence.
    pub fn end_ins(&mut self, ins: Instruction) {
        debug_assert!(ins.is_control());
        let is_return = ins.opcode.is_return();
        let value = self.dfg_add_ins(ins);
        {
            let SequenceIndex(index) = self.sequence.unwrap();
            let edit = &mut self.unit.cfg.sequences[index];
            debug_assert!(edit.control.is_dummy());
            edit.control = value;
        }
        // If the last instruction is a return statement, then add this return
        // statement in the list of outputs of the unit.
        if is_return {
            self.unit.outputs.push(value);
        }
    }

    // Add a control flow instruction based on its opcode, operands and dependencies.
    pub fn end_op_deps(&mut self, opcode: Opcode, operands: &[Value], dependencies: &[Value]) {
        self.end_ins(Instruction {
            opcode,
            operands: operands.iter().map(|x| *x).collect(),
            dependencies: dependencies.iter().map(|x| *x).collect(),
            replaced_by: None,
        })
    }

    /// Add a control flow instruction based only on its opcode, this function
    /// creates a conservative aliasing between load, store, calls and units.
    pub fn end_op(&mut self, opcode: Opcode, operands: &[Value]) {
        self.end_op_deps(opcode, operands, &[])
    }

    pub fn set_entry(&mut self) {
        debug_assert!(self.unit.cfg.entry.is_dummy());
        let seq = self.sequence.unwrap();
        self.unit.cfg.entry = seq;
        self.freeze_sequence_predecessors(seq);
    }

    /// Set conditional branch.
    pub fn sequence_value_jump(&mut self, value: isize, seq: SequenceIndex) {
        debug_assert!(!self.frozen_seqs.contains(&seq));
        let SequenceIndex(index) = self.sequence.unwrap();
        let succ_idx = {
            let edit = &mut self.unit.cfg.sequences[index];
            edit.successors.push(seq);
            let succ_idx = SuccessorIndex(edit.successors.len() - 1);
            debug_assert!(!edit.targets.iter().any(|&(v, _)| v == value));
            edit.targets.push((value, succ_idx));
            succ_idx
        };
        let SequenceIndex(seq) = seq;
        let edit = &mut self.unit.cfg.sequences[seq];
        edit.predecessors.push((SequenceIndex(index), succ_idx));
    }
    /// Set default branch.
    pub fn sequence_default_jump(&mut self, seq: SequenceIndex) {
        debug_assert!(!self.frozen_seqs.contains(&seq));
        let SequenceIndex(index) = self.sequence.unwrap();
        let succ_idx = {
            let edit = &mut self.unit.cfg.sequences[index];
            edit.successors.push(seq);
            let succ_idx = SuccessorIndex(edit.successors.len() - 1);
            debug_assert_eq!(edit.default, None);
            edit.default = Some(succ_idx);
            succ_idx
        };
        let SequenceIndex(seq) = seq;
        let edit = &mut self.unit.cfg.sequences[seq];
        edit.predecessors.push((SequenceIndex(index), succ_idx));
    }
    /// Set unwind branch.
    pub fn sequence_unwind_jump(&mut self, seq: SequenceIndex) {
        debug_assert!(!self.frozen_seqs.contains(&seq));
        let SequenceIndex(index) = self.sequence.unwrap();
        let succ_idx = {
            let edit = &mut self.unit.cfg.sequences[index];
            edit.successors.push(seq);
            let succ_idx = SuccessorIndex(edit.successors.len() - 1);
            debug_assert_eq!(edit.unwind, None);
            edit.unwind = Some(succ_idx);
            succ_idx
        };
        let SequenceIndex(seq) = seq;
        let edit = &mut self.unit.cfg.sequences[seq];
        edit.predecessors.push((SequenceIndex(index), succ_idx));
    }
    /// Prevent the addition of any predecessors to the given sequence. This is
    /// used for computing the automatic insertion of Phi instructions.
    pub fn freeze_sequence_predecessors(&mut self, seq: SequenceIndex) {
        self.frozen_seqs.insert(seq);
    }


    /// Finalize and (TODO) assert that the generated Unit is valid.
    pub fn finish(self) -> Unit {
        self.unit
    }
}
