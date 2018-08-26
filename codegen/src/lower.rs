use std::mem;
use std::collections::HashMap;

use frontend::{FunctionBuilderContext, FunctionBuilder, Variable};

use codegen::entity::EntityRef;
use codegen::ir::{Ebb, ExternalName, Function, Signature, AbiParam, InstBuilder, TrapCode};
use codegen::ir::immediates::{Ieee32, Ieee64, Imm64};
use codegen::ir::condcodes::IntCC;
use codegen::ir::types::*;
use codegen::ir::types;
use codegen::settings::{self, CallConv};
use codegen::verifier::verify_function;
use codegen::isa::TargetIsa;

use lir::unit::{Unit, UnitId};
use lir::context::Context;
use lir::types::{ComplexTypeId, ComplexType};
use lir::number::{NumberType, SignedType, NumberValue};
use lir::control_flow::{Sequence, SequenceIndex, SuccessorIndex};
use lir::data_flow::{Opcode, Instruction, ValueType, Value};
use error::{LowerResult, LowerError};

struct ConvertCtx<'a> {
    /// Used to know the size of a pointer.
    pub isa: &'a TargetIsa,
    /// Context used to resove the LIR types information.
    pub ctx: &'a Context,
    /// Unit of the function which is being compiled.
    pub unit: &'a Unit,
    /// Map each variable known with the type declared to cranelift. This is
    /// also used to allocate new temporary variables.
    pub var_types: Vec<OptVarType>,
    /// Map the math operation to the overflow variable.
    pub overflow_map: HashMap<Value, Variable>,
    /// Map the math operation to the carry variable.
    pub carry_map: HashMap<Value, Variable>,
}

type OptVarType = Option<(Variable, types::Type)>;

impl<'a> ConvertCtx<'a> {
    fn sign_mask(&self, ty: NumberType) -> u64 {
        match ty {
            NumberType::F64 | NumberType::I64 | NumberType::U64 => 0x8000_0000_0000_0000,
            NumberType::F32 | NumberType::I32 | NumberType::U32 => 0x8000_0000,
            NumberType::I16 | NumberType::U16 => 0x8000,
            NumberType::I8 | NumberType::U8 => 0x80,
            NumberType::B1 => 1,
        }
    }

    fn number_type(&self, ty: NumberType) -> types::Type {
        match ty {
            NumberType::B1 => types::B1,
            NumberType::I8 | NumberType::U8 => types::I8,
            NumberType::I16 | NumberType::U16 => types::I16,
            NumberType::I32 | NumberType::U32 => types::I32,
            NumberType::I64 | NumberType::U64 => types::I64,
            NumberType::F32 => types::F32,
            NumberType::F64 => types::F64,
        }
    }

    fn cltype(&self, ty: ComplexTypeId) -> LowerResult<types::Type> {
        use self::ComplexType::*;
        use self::NumberType::*;
        let ty = self.ctx.get_type(ty);
        match ty {
            &Pointer => Ok(self.isa.pointer_type()),
            &Scalar(B1) => Ok(types::B1),
            &Scalar(U8) | &Scalar(I8) => Ok(types::I8),
            &Scalar(U16) | &Scalar(I16) => Ok(types::I16),
            &Scalar(U32) | &Scalar(I32) => Ok(types::I32),
            &Scalar(U64) | &Scalar(I64) => Ok(types::I64),
            &Vector(_, _) => unimplemented!(),
            _ => Err(LowerError::ComplexTypeNotLowered),
        }
    }

    fn abiparam(&self, ty: ComplexTypeId) -> LowerResult<AbiParam> {
        Ok(AbiParam::new(self.cltype(ty)?))
    }

    fn signature_io(&self, sig: ComplexTypeId) -> LowerResult<(&'a Vec<ComplexTypeId>, &'a Vec<ComplexTypeId>)> {
        let ty = self.ctx.get_type(sig);
        match ty {
            &ComplexType::Function(ref ins, ref outs, ref _unwind) => Ok((ins, outs)),
            _ => Err(LowerError::UnitIsNotAFunction),
        }
    }

    /// Unit have a signature expressed as a type, we have to convert this signature
    /// into simpler types understood by Cranelift.
    fn signature(&self) -> LowerResult<Signature> {
        let (ins, outs) = self.signature_io(self.unit.sig)?;

        // At the moment, assume that all Units are going to be called with Rust
        // calling convention.
        let mut sig = Signature::new(CallConv::SystemV);

        for &ty in ins.iter() {
            sig.params.push(self.abiparam(ty)?);
        }
        for &ty in outs.iter() {
            sig.returns.push(self.abiparam(ty)?);
        }
        Ok(sig)
    }

    // Generate external name indexes based on the UnitId.
    fn external_name(&self, id: UnitId) -> ExternalName {
        let (d, i) = match id {
            UnitId::Intrinsic(i) => (0, i),
            UnitId::Function(i) => (1, i),
            UnitId::SubSet(i) => (2, i)
        };
        ExternalName::user(d, i)
    }

    /// Identify the type of each instructions and declare a variable with the same
    /// offset as the instruction in the data flow graph of the Unit.
    fn declare_vars(&mut self, bld: &mut FunctionBuilder<Variable>) -> LowerResult<()> {
        // Use pointer as a dummy type.
        let mut types : Vec<_> = self.unit.dfg.instructions.iter().map(|_| None).collect();

        // Give a type to the arguments of the Unit.
        let (params, _) = self.signature_io(self.unit.sig)?;
        for (value, &ty) in self.unit.inputs.iter().zip(params.iter()) {
            let index = value.index();
            let v = Variable::new(index);
            let ty = self.cltype(ty)?;
            bld.declare_var(v, ty);
            types[index] = Some((v, ty));
        }

        // Infer type from the operation.
        for (index, ref ins) in self.unit.dfg.instructions.iter().enumerate() {
            let ty = match ins.opcode.result_type() {
                ValueType::Boolean => types::B1,
                ValueType::Pointer => self.isa.pointer_type(),
                ValueType::Number(n) => self.number_type(n),
                ValueType::Complex(id) => self.cltype(id)?,
                ValueType::ResultOfUnit(_unit) => unimplemented!(),
                ValueType::ResultOfSig(id) => {
                    let (_, out) = self.signature_io(id)?;
                    match out.len() {
                        0 => continue,
                        1 => self.cltype(out[0])?,
                        _ => unimplemented!(),
                    }
                }
                ValueType::InheritFromOperands |
                ValueType::None => continue,
            };
            let v = Variable::new(index);
            bld.declare_var(v, ty);
            types[index] = Some((v, ty));

            // Record Overflow and Carry dependency and map it to overflow /
            // carry variable. Note, that we expect to have only a single
            // overflow and carry per math operation. TODO: To handle multiple
            // overflow flag and carry flag, should create new variable that
            // would be copied on overflow/carry encoding.
            let ins_res = match ins.opcode {
                Opcode::OverflowFlag => self.overflow_map.insert(ins.dependencies[0], v),
                Opcode::CarryFlag => self.carry_map.insert(ins.dependencies[0], v),
                _ => None,
            };
            debug_assert!(ins_res == None);
        }

        // Infer type from the operands.
        for (index, ref ins) in self.unit.dfg.instructions.iter().enumerate() {
            match ins.opcode.result_type() {
                ValueType::InheritFromOperands => (),
                _ => continue,
            };
            assert!(ins.operands.len() >= 1);
            let ty = match types[ins.operands[0].index] {
                Some((_, ty)) => ty,
                None => continue,
            };
            let v = Variable::new(index);
            bld.declare_var(v, ty);
            types[index] = Some((v, ty));
        }

        self.var_types = types;
        Ok(())
    }

    /// We are about to jump to the next block, convert the Phi operands into
    /// variable definitions, such that the FunctionBuilder can convert these
    /// into arguments of extended basic blocks.
    fn bind_phis(&self, idx: SequenceIndex, seq: &Sequence, succ: SuccessorIndex, bld: &mut FunctionBuilder<Variable>) {
        let SuccessorIndex(sidx) = succ;
        let SequenceIndex(sseq) = seq.successors[sidx];
        let sseq = &self.unit.cfg.sequences[sseq];
        // Find the index of this successor in the list of predecessors.
        let (pidx, _) = sseq.predecessors.iter().enumerate().find(|p| p.1 == &(idx, succ)).unwrap();

        // For each Phi, bind the variable corresponding to the Phi, with the
        // variable of the current sequence.
        for value in sseq.sequence.iter() {
            let ins = &self.unit.dfg.instructions[value.index];
            match ins.opcode {
                Opcode::Phi => (),
                _ => continue,
            };
            let input = bld.use_var(Variable::new(ins.operands[pidx].index));
            let res = bld.ins().copy(input);
            bld.def_var(Variable::new(value.index), res);
        }
    }

    fn instruction(&mut self, val: Value, ins: &Instruction, sidx: SequenceIndex, seq: &Sequence, ebbs: &Vec<Ebb>, bld: &mut FunctionBuilder<Variable>) -> LowerResult<()> {
        use self::Opcode::*;
        let res_var = Variable::new(val.index);
        match ins.opcode {
            Entry(_) => (),
            Newhash(_) => (),
            Rehash(_) => {
                let a0 = bld.use_var(Variable::new(ins.operands[0].index));
                let res = bld.ins().copy(a0);
                bld.def_var(Variable::new(val.index), res);
            }
            Phi => (), // Phi are handled at the end of predecessor blocks.
            Const(val) => {
                use self::NumberValue::*;
                let res = match val {
                    B1(i) => bld.ins().bconst(types::B1, i),
                    I8(i) => bld.ins().iconst(types::I8, i as i64),
                    I16(i) => bld.ins().iconst(types::I16, i as i64),
                    I32(i) => bld.ins().iconst(types::I32, i as i64),
                    I64(i) => bld.ins().iconst(types::I64, i),
                    U8(i) => bld.ins().iconst(types::I8, i as i64),
                    U16(i) => bld.ins().iconst(types::I16, i as i64),
                    U32(i) => bld.ins().iconst(types::I32, i as i64),
                    U64(i) => bld.ins().iconst(types::I64, i as i64),
                    F32(f) => bld.ins().f32const(Ieee32::with_float(f)),
                    F64(f) => bld.ins().f64const(Ieee64::with_float(f)),
                };
                bld.def_var(res_var, res);
            },
            Cast(_id) => unimplemented!(),
            OverflowFlag => (),
            CarryFlag => (),
            Add(n) => {
                // TODO: If any overflow/carry flag depends on this instruction, we
                // should change the encoding of this instruction to emit a carry
                // bits.
                let a0 = bld.use_var(Variable::new(ins.operands[0].index));
                let a1 = bld.use_var(Variable::new(ins.operands[1].index));
                let (of, cf) = (self.overflow_map.get(&val),
                                self.carry_map.get(&val));
                match (n, of, cf) {
                    (NumberType::F32, _, _) |
                    (NumberType::F64, _, _) => {
                        debug_assert!(of == None);
                        debug_assert!(cf == None);
                        let res = bld.ins().fadd(a0, a1);
                        bld.def_var(res_var, res);
                    }
                    (_, None, None) => {
                        let res = bld.ins().iadd(a0, a1);
                        bld.def_var(res_var, res);
                    }
                    (_, None, Some(cv)) => {
                        let (res, carry) = bld.ins().iadd_cout(a0, a1);
                        bld.def_var(res_var, res);
                        bld.def_var(*cv, carry);
                    }
                    (i, Some(ov), None) => {
                        let res = bld.ins().iadd(a0, a1);
                        bld.def_var(res_var, res);
                        // of = ((a0 | a1) ^ (a0 + a1)) > (max >> 1)
                        let bor = bld.ins().bor(a0, a1);
                        let xor = bld.ins().bxor(bor, res);
                        let sign_mask = self.sign_mask(i);
                        let cmp = bld.ins().icmp_imm(IntCC::UnsignedGreaterThanOrEqual, xor, Imm64::new(sign_mask as i64));
                        bld.def_var(*ov, cmp);
                    }
                    (i, Some(ov), Some(cv)) => {
                        let (res, carry) = bld.ins().iadd_cout(a0, a1);
                        bld.def_var(res_var, res);
                        bld.def_var(*cv, carry);
                        // of = ((a0 | a1) ^ (a0 + a1)) > (max >> 1)
                        let bor = bld.ins().bor(a0, a1);
                        let xor = bld.ins().bxor(bor, res);
                        let sign_mask = self.sign_mask(i);
                        let cmp = bld.ins().icmp_imm(IntCC::UnsignedGreaterThanOrEqual, xor, Imm64::new(sign_mask as i64));
                        bld.def_var(*ov, cmp);
                    }
                };
            }
            Sub(_n) => unimplemented!(),
            Mul(_n) => unimplemented!(),
            Div(_n) => unimplemented!(),
            Rem(n) => {
                let a0 = bld.use_var(Variable::new(ins.operands[0].index));
                let a1 = bld.use_var(Variable::new(ins.operands[1].index));
                let res = match n {
                    SignedType::I8 | SignedType::I16 |
                    SignedType::I32 | SignedType::I64
                        => bld.ins().srem(a0, a1),
                    SignedType::U8 | SignedType::U16 |
                    SignedType::U32 | SignedType::U64
                        => bld.ins().urem(a0, a1),
                };
                bld.def_var(res_var, res);
            },
            SignExt(_n) => unimplemented!(),
            ZeroExt(_n) => unimplemented!(),
            Truncate(_f) |
            Round(_f) |
            Floor(_f) |
            Ceil(_f) => unimplemented!(),
            BwXor(_b) |
            BwAnd(_b) |
            BwOr(_b) |
            BwNot(_b) => unimplemented!(),
            ShiftLeft(_i) => unimplemented!(),
            ShiftRight(_i) => unimplemented!(),
            Eq(_) | Lt(_) | Le(_) |
            Ne(_) | Gt(_) | Ge(_) => unimplemented!(),
            StaticAddress |
            Address => unimplemented!(),
            CPUAddress => unimplemented!(),
            Load(_) => unimplemented!(),
            Store(_ty) => unimplemented!(),
            LoadFenceLoad |
            LoadFenceStore |
            StoreFenceLoad |
            StoreFenceStore => unimplemented!(),
            Unit(_id) => unimplemented!(),
            Return => {
                let args : Vec<_> = ins.operands.iter().map(|v| bld.use_var(Variable::new(v.index))).collect();
                bld.ins().return_(&args);
            },
            Unwind | // TODO: properly implement unwind.
            Unreachable => {
                bld.ins().trap(TrapCode::User(0));
            }
            Goto => {
                let SuccessorIndex(idx) = seq.default.unwrap();
                self.bind_phis(sidx, seq, SuccessorIndex(idx), bld);
                let SequenceIndex(idx) = seq.successors[idx];
                bld.ins().jump(ebbs[idx], &[]);
            }
            Switch(_) => {
                // TODO: use data field and the number of successors to
                // determine if we should generate a jump table.
                let a0 = bld.use_var(Variable::new(ins.operands[0].index));
                assert!(seq.unwind == None, "Switch statements are cannot unwind");
                let nb_succ = seq.targets.len() + seq.default.map_or(0, |_| 1);
                assert!(nb_succ > 0, "Switch statement should at least have a successor,");
                match nb_succ {
                    0 => unreachable!(),
                    1 => {
                        // Unconditional jump to the only branch.
                        let SuccessorIndex(idx) = seq.default.unwrap_or_else(|| seq.targets[0].1);
                        self.bind_phis(sidx, seq, SuccessorIndex(idx), bld);
                        let SequenceIndex(idx) = seq.successors[idx];
                        bld.ins().jump(ebbs[idx], &[]);
                    }
                    2 => {
                        let (tv, t, f) = match seq.default {
                            Some(f) => (seq.targets[0].0, seq.targets[0].1, f),
                            None => {
                                let (tv, t) = seq.targets[0];
                                let (fv, f) = seq.targets[1];
                                assert_ne!(tv, fv);
                                if (0 <= tv && tv < fv) || (fv < tv && tv <= 0) {
                                    (tv, t, f)
                                } else {
                                    (fv, f, t)
                                }
                            }
                        };

                        // Conditional jump to the first branch.
                        let SuccessorIndex(t) = t;
                        self.bind_phis(sidx, seq, SuccessorIndex(t), bld);
                        let SequenceIndex(t) = seq.successors[t];
                        if tv == 0 {
                            bld.ins().brz(a0, ebbs[t], &[]);
                        } else {
                            // let t0 = Variable::new(self.var_types.len());
                            // self.var_types.push(Some((t0, types::B1)));
                            // bld.declare_var(t0, types::B1);
                            let cmp_res = bld.ins().icmp_imm(IntCC::Equal, a0, Imm64::new(tv as i64));
                            // bld.def_var(t0, cmp_res);
                            // let t0 = bld.use_var(t0);
                            bld.ins().brnz(cmp_res, ebbs[t], &[]);
                        }

                        // Unconditional jump to the second branch.
                        let SuccessorIndex(f) = f;
                        self.bind_phis(sidx, seq, SuccessorIndex(f), bld);
                        let SequenceIndex(f) = seq.successors[f];
                        bld.ins().jump(ebbs[f], &[]);
                    }
                    _ => unimplemented!("Switch with more than 2 branches"),
                }
            }
            Call(_id) => unimplemented!(),
            CallUnit(_id) => unimplemented!(),
        };

        Ok(())
    }

    /// Convert the sequences of the control flow graph into a graph of extended
    /// blocks expected by Cranelift.
    fn cfg(&mut self, bld: &mut FunctionBuilder<Variable>) -> LowerResult<()> {
        let _types = self.declare_vars(bld)?;

        // TODO: Create better extended basic blocks instead of creating a 1:1
        // mapping with the LIR.
        let ebbs : Vec<_> = self.unit.cfg.sequences.iter().map(|_| bld.create_ebb()).collect();

        // Anchor argument processing to the entry EBB.
        let SequenceIndex(entry) = self.unit.cfg.entry;
        bld.append_ebb_params_for_function_params(ebbs[entry]);

        // This is a queue of block which are waiting for all their inputs to be
        // finalized before sealing them.
        let mut seal_queue = vec![];

        // Iterate over each sequence and convert the instruction in each of them to
        // a Cranelift instruction.
        for (i, ref seq) in self.unit.cfg.sequences.iter().enumerate() {
            // Switch to the block in which we emit the sequence of instructions.
            bld.switch_to_block(ebbs[i]);
            // Seal the block, unless we are waiting for upcoming sequences to be
            // converted before freezing the list of predecessors.
            let get_seq_index = |pred : &(SequenceIndex, SuccessorIndex)| -> usize {
                let &(SequenceIndex(idx), _) = pred;
                idx
            };
            match seq.predecessors.iter().map(get_seq_index).max() {
                None => bld.seal_block(ebbs[i]),
                Some(idx) => {
                    if idx < i {
                        bld.seal_block(ebbs[i]);
                    } else {
                        seal_queue.push((idx, i));
                    }
                }
            };

            // Bind arguments in the entry block.
            if SequenceIndex(i) == self.unit.cfg.entry {
                for (a, value) in self.unit.inputs.iter().enumerate() {
                    let arg = bld.ebb_params(ebbs[i])[a];
                    bld.def_var(Variable::new(value.index), arg);
                }
            }

            // Convert instructions. NOTE: Assumes that all the instructions are
            // scheduled properly in the control flow graph.
            for value in seq.sequence.iter() {
                self.instruction(*value, &self.unit.dfg.instructions[value.index], SequenceIndex(i), seq, &ebbs, bld)?;
            }

            // Add conditional and jump instruction.
            let value = seq.control;
            self.instruction(value, &self.unit.dfg.instructions[value.index], SequenceIndex(i), seq, &ebbs, bld)?;

            // Seal blocks which were waiting on the current block to be ended.
            let rest = mem::replace(&mut seal_queue, vec![]);
            let (to_seal, rest) : (Vec<_>, _) =
                rest.into_iter().partition(|&(idx, _)| idx <= i);
            seal_queue = rest;
            for (_, j) in to_seal.into_iter() {
                bld.seal_block(ebbs[j])
            }
        }

        Ok(())
    }

    /// Convert a LIR Unit into a Cranelift IR (Function).
    fn convert(&mut self) -> LowerResult<Function> {
        let sig = self.signature()?;
        let mut fn_builder_ctx = FunctionBuilderContext::<Variable>::new();
        let mut func = Function::with_name_signature(self.external_name(self.unit.id), sig);
        {
            let mut builder = FunctionBuilder::<Variable>::new(&mut func, &mut fn_builder_ctx);
            self.cfg(&mut builder)?;
            builder.finalize();
        }

        let flags = settings::Flags::new(settings::builder());
        verify_function(&func, &flags)?;
        Ok(func)
    }
}

/// Convert a LIR Unit into a Cranelift IR (Function).
pub fn convert(isa: &TargetIsa, ctx: &Context, unit: &Unit) -> LowerResult<Function> {
    let mut cc = ConvertCtx {
        ctx, isa, unit,
        var_types: vec![],
        overflow_map: HashMap::new(),
        carry_map: HashMap::new(),
    };
    println!("{:?}", unit);
    let func = cc.convert()?;
    println!("{}", func.display(None));
    Ok(func)
}

/// Extracted from the documentation to compare against what is produced based
/// on the LIR.
#[allow(dead_code)]
pub fn convert_test(_isa: &TargetIsa, _ctx: &Context, _unit: &Unit) -> LowerResult<Function> {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.returns.push(AbiParam::new(I32));
    sig.params.push(AbiParam::new(I32));
    let mut fn_builder_ctx = FunctionBuilderContext::<Variable>::new();
    let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
    {
        let mut builder = FunctionBuilder::<Variable>::new(&mut func, &mut fn_builder_ctx);

        let block0 = builder.create_ebb();
        let block1 = builder.create_ebb();
        let block2 = builder.create_ebb();
        let x = Variable::new(0);
        let y = Variable::new(1);
        let z = Variable::new(2);
        builder.declare_var(x, I32);
        builder.declare_var(y, I32);
        builder.declare_var(z, I32);
        builder.append_ebb_params_for_function_params(block0);

        builder.switch_to_block(block0);
        builder.seal_block(block0);
        {
            let tmp = builder.ebb_params(block0)[0]; // the first function parameter
            builder.def_var(x, tmp);
        }
        {
            let tmp = builder.ins().iconst(I32, 2);
            builder.def_var(y, tmp);
        }
        {
            let arg1 = builder.use_var(x);
            let arg2 = builder.use_var(y);
            let tmp = builder.ins().iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        builder.ins().jump(block1, &[]);

        builder.switch_to_block(block1);
        {
            let arg1 = builder.use_var(y);
            let arg2 = builder.use_var(z);
            let tmp = builder.ins().iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(y);
            builder.ins().brnz(arg, block2, &[]);
        }
        {
            let arg1 = builder.use_var(z);
            let arg2 = builder.use_var(x);
            let tmp = builder.ins().isub(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(y);
            builder.ins().return_(&[arg]);
        }

        builder.switch_to_block(block2);
        builder.seal_block(block2);

        {
            let arg1 = builder.use_var(y);
            let arg2 = builder.use_var(x);
            let tmp = builder.ins().isub(arg1, arg2);
            builder.def_var(y, tmp);
        }
        builder.ins().jump(block1, &[]);
        builder.seal_block(block1);

        builder.finalize();
    }

    let flags = settings::Flags::new(settings::builder());
    let res = verify_function(&func, &flags);
    println!("{}", func.display(None));
    if let Err(errors) = res {
        panic!("{}", errors);
    }

    Ok(func)
}
