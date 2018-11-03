/// This module contains everything need for constructing a Unit, with its data
/// flow graph and its control flow graph.

use std::collections::{HashMap, HashSet};
use std::mem::{align_of, size_of};

use unit::{Unit, UnitId};
use data_flow::{Instruction, Opcode, Value};
use control_flow::{Sequence, SequenceIndex, SuccessorIndex};
use types::{ComplexType, ComplexTypeId};
use context;
use bitset::BitSet;

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

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub struct Variable(u32);

/// This structure is used to capture variable states, and used to insert Phi
/// instructions if needed.
pub struct SSABuilder {
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

    // [Build SSA values]
    /// Number of variable which got allocated.
    nb_vars: u32,
    /// For each sequence, we have a list of variables which are live at the end
    /// of each sequence. This is used for automatically inserting Phi
    /// instructions in order to convert variables into an SSA form.
    seq_live_vars: Vec<BitSet>,
    /// For each live variable in a sequence, record which value it is
    /// associated to.
    seqvar_map: HashMap<(SequenceIndex, Variable), Value>,
    /// List of Rehash instructions which have to be fixed by inserting Phi
    /// instructions once all predecessors are known.
    rehash_map: HashMap<SequenceIndex, Vec<(Variable, Value)>>,
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

    /// Records the position of a static reference in the tuple with which this
    /// context is expected to be intialized with. Any references added to the
    /// context with this function should also be mirrored in the tuple which
    /// would be used to initialize the Context at runtime.
    ///
    /// Based on the alignment and size of the added value, this function
    /// reserves space and returns the offset of the StaticAddress from which
    /// the tuple value can be read.
    pub fn add_untyped_ref(&mut self, align: usize, size: usize) -> usize {
        let rest = self.ctx.expected_refs_size % align;
        let padding = if rest == 0 { 0 } else { align - rest };
        let base_offset = self.ctx.expected_refs_size + padding;
        self.ctx.expected_refs_size = base_offset + size;
        base_offset
    }

    /// Records the position of a static reference in the tuple with which this
    /// context is expected to be intialized with. Any references added to the
    /// context with this function should also be mirrored in the tuple which
    /// would be used to initialize the Context at runtime.
    ///
    /// Based on the type of the added value, this functions reserves space and
    /// returns the offset of the StaticAddress from which the tuple value can
    /// be read.
    pub fn add_typed_ref<T>(&mut self) -> usize {
        self.add_untyped_ref(align_of::<T>(), size_of::<T>())
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
            nb_vars: 0,
            seq_live_vars: vec![],
            seqvar_map: HashMap::new(),
            rehash_map: HashMap::new(),
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
        let (ins, _outs) = match ty {
            &ComplexType::Function(ref ins, ref outs, _) => (ins, outs),
            _ => panic!("Unit signatures are expected to be a Function.")
        };
        self.unit.inputs = ins.iter().map(|_| Value::dummy()).collect();
        self.unit.outputs = vec![];
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
        self.seq_live_vars.push(BitSet::new());
        SequenceIndex(self.unit.cfg.sequences.len() - 1)
    }

    /// Switch to a sequence of code, such that newly added instructions are
    /// going to be added to this block by default.
    pub fn switch_to_sequence(&mut self, seq: SequenceIndex) {
        self.sequence = Some(seq);
        // Create Phi instructions for all live variables if needed and if all
        // predecessors have a control instructions.
        if self.unit.cfg.entry != seq {
            self.create_seq_vars();
        }
    }

    pub fn end_sequence(&mut self) {
        // Assert that the previous sequence is properly ended with a control
        // instruction, and a number of successors matching the number the
        // expected successors of the control instruction.
        debug_assert!(!self.unit.cfg.sequences[self.current_sequence().0].control.is_dummy());
        self.sequence = None;
    }

    fn current_sequence(&self) -> SequenceIndex {
        self.sequence.expect(&"Should be between switch_to_sequence and end_sequence")
    }

    /// Add an instruction to both the data flow graph and the control flow
    /// sequence given as argument.
    fn add_ins_in_seq(&mut self, ins: Instruction, si: SequenceIndex) -> Value {
        debug_assert!(ins.is_phi() || self.unit.cfg.sequences[si.0].control.is_dummy());
        let value = self.dfg_add_ins(ins);
        self.unit.cfg.sequences[si.0].sequence.push(value);
        value
    }

    /// Add an instruction to both the data flow graph and the active sequence
    /// of the control flow graph.
    pub fn add_ins(&mut self, ins: Instruction) -> Value {
        let si = self.current_sequence();
        self.add_ins_in_seq(ins, si)
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
            let SequenceIndex(index) = self.current_sequence();
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

    // Set the current sequence as being the entry point of the control flow
    // graph. Also freeze the current sequence and assert that it has no
    // predecessors.
    pub fn set_entry(&mut self, seq: SequenceIndex) {
        debug_assert!(self.unit.cfg.entry.is_dummy());
        self.unit.cfg.entry = seq;
        debug_assert!(self.unit.cfg.sequences[seq.0].predecessors.is_empty());
        self.freeze_sequence_predecessors(seq);
    }

    /// Set conditional branch.
    pub fn sequence_value_jump(&mut self, value: isize, seq: SequenceIndex) {
        debug_assert!(!self.frozen_seqs.contains(&seq));
        let SequenceIndex(index) = self.current_sequence();
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
        let SequenceIndex(index) = self.current_sequence();
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
        let SequenceIndex(index) = self.current_sequence();
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
        // If the sequence already got generated, then we inserted Rehash
        // instruction for every live value. Call create_Seq_rehash_phis to set
        // the Rehash operands to Phi instruction which are considering all
        // predecessors.
        if !self.unit.cfg.sequences[seq.0].control.is_dummy() {
            self.create_seq_rehash_phis(seq)
        }
    }

    /// Allocate a new Variable.
    pub fn new_var(&mut self) -> Variable {
        let id = self.nb_vars;
        self.nb_vars += 1;
        Variable(id)
    }

    /// Within the current sequence, set the variable to a given value.
    pub fn set_var(&mut self, var: Variable, val: Value) {
        let SequenceIndex(index) = self.current_sequence();
        self.seq_live_vars[index].insert(var.0);
        self.seqvar_map.insert((SequenceIndex(index), var), val);
    }

    /// Within the current sequence, get the value corresponding to the given
    /// variable.
    pub fn use_var(&self, var: Variable) -> Value {
        let si = self.current_sequence();
        debug_assert!(self.seq_live_vars[si.0].contains(&var.0));
        let val = self.seqvar_map.get(&(si, var)).expect(&"Variable is live in the current sequence, missing a set_var in predecessor sequences?");
        *val
    }

    /// Declare the current variable as being no longer used. Calling this
    /// function will avoid iterating over short-live variable created with
    /// set_var.
    pub fn dead_var(&mut self, var: Variable) {
        let SequenceIndex(index) = self.current_sequence();
        self.seq_live_vars[index].remove(&var.0);
    }

    /// When switching to a new sequence, we look at all the predecessors to
    /// resolve the Phi of live variables, and potentially to create rehash in
    /// case of loops, where the list of predecessors is not yet known.
    fn create_seq_vars(&mut self) {
        let si = self.current_sequence();
        let create_phis = {
            if self.frozen_seqs.contains(&si) {
                // We know all the predecessors, check that they all have a
                // control instruction.
                self.unit.cfg.sequences[si.0].predecessors.iter()
                    .map(|&(pi, _)| pi).all(
                    |pseq| !self.unit.cfg.sequences[pseq.0].control.is_dummy()
                )
            } else {
                // We might add new predecessors, assume noting, and use rehash
                // instructions.
                false
            }
        };
        let live = self.entry_live_set();
        match create_phis {
            true => self.create_seq_phis(&live),
            false => self.create_seq_rehash(&live),
        }
        self.seq_live_vars[si.0] = live;
    }

    /// Compute live variables as the intersection of all predecessors.
    fn entry_live_set(&self) -> BitSet {
        let si = self.current_sequence();
        let pred = &self.unit.cfg.sequences[si.0].predecessors;
        debug_assert!(pred.len() >= 1);
        let mut pred = pred.iter().map(|&(pred, _)| pred);
        let first = match pred.next() {
            None => return BitSet::new(),
            Some(psi) => psi
        };
        let mut live = self.seq_live_vars[first.0].clone();
        for psi in pred {
            live = live.intersection(&self.seq_live_vars[psi.0]).cloned().collect();
        }
        live
    }

    /// All predecessors are known, create Phi instructions for all live
    /// variables.
    fn create_seq_phis(&mut self, live: &BitSet) {
        // For each live variable add seqvar_map bindings.
        let si = self.current_sequence();
        if self.unit.cfg.sequences[si.0].predecessors.len() == 1 {
            // We have a single predecessor, inherit all the bindings without
            // adding any Phi instruction.
            let (psi, _) = self.unit.cfg.sequences[si.0].predecessors[0];
            for var in live {
                let var = Variable(*var);
                let val = *self.seqvar_map.get(&(psi, var))
                     .expect(&"Impossible error: live set is the intersection of predecessors");
                self.seqvar_map.insert((si, var), val);
            }
        } else {
            // For each variable, collect the corresponding value from each
            // predecessor and ad them as operands to a Phi instruction which
            // it-self correspond to the value of the variable in the current
            // sequence.
            for var in live {
                let var = Variable(*var);
                let vals : Vec<_> = {
                    let pred = self.unit.cfg.sequences[si.0].predecessors.iter().map(|&(psi, _)| psi);
                    let vals = pred.map(|psi| *self.seqvar_map.get(&(psi, var)).unwrap());
                    vals.collect()
                };
                // Do not insert a Phi instruction if all the predecessors are
                // identical.
                let val = if vals.iter().all(|v| v == &vals[0]) {
                    vals[0]
                } else {
                    self.add_op(Opcode::Phi, &vals)
                };
                self.seqvar_map.insert((si, var), val);
            }
        }
    }

    /// Some predecessors might be added later, create Rehash instructions for
    /// all live variables.
    fn create_seq_rehash(&mut self, live: &BitSet) {
        // For each variable known to be live, add a Rehash instruction with a
        // dummy value operand.
        let si = self.current_sequence();
        let mut rehash_list = Vec::with_capacity(live.len());
        for var in live {
            let var = Variable(*var);
            let op = self.ctx.get_rehash();
            let val = self.add_op(op, &[Value::dummy()]);
            self.seqvar_map.insert((si, var), val);
            rehash_list.push((var, val));
        }
        self.rehash_map.insert(si, rehash_list);
    }

    /// We finally freeze a sequence after having generated rehash instructions
    /// in it.
    fn create_seq_rehash_phis(&mut self, si: SequenceIndex) {
        let rehash_list = self.rehash_map.remove(&si)
            .expect(&"create_seq_rehash should be called before, and the current fucntion should only be called once per sequence.");
        let mut inserted_phis = 0;
        for (var, rehash) in rehash_list {
            let vals : Vec<_> = {
                let pred = self.unit.cfg.sequences[si.0].predecessors.iter().map(|&(psi, _)| psi);
                let vals = pred.map(|psi| *self.seqvar_map.get(&(psi, var)).unwrap());
                vals.collect()
            };
            // Do not insert a Phi instruction if all the predecessors are
            // identical.
            let val = if vals.iter().all(|v| v == &vals[0]) {
                vals[0]
            } else {
                inserted_phis += 1;
                self.add_ins_in_seq(Instruction {
                    opcode: Opcode::Phi,
                    operands: vals,
                    dependencies: vec![],
                    replaced_by: None,
                }, si)
            };

            // Replace the Rehash operand by the newly computed value/phi.
            let rehash = &mut self.unit.dfg.instructions[rehash.index];
            debug_assert!(rehash.is_rehash());
            debug_assert!(rehash.operands.len() == 1);
            debug_assert!(rehash.operands[0].is_dummy());
            rehash.operands[0] = val;
        }
        // Move the inserted phis to the beginning of the sequence.
        self.unit.cfg.sequences[si.0].sequence.rotate_right(inserted_phis);
    }

    /// Finalize and (TODO) assert that the generated Unit is valid.
    pub fn finish(self) -> Unit {
        self.unit
    }
}
