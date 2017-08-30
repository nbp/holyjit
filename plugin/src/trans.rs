/// This module converts the rustc::mir into a holyjit::lir.

use rustc::ty::{self, TyCtxt};
use rustc::mir;
use rustc::hir::def_id::DefId;
use rustc::middle::const_val::{ConstVal, ConstInt};
use rustc_const_math::ConstUsize;
use rustc_data_structures::indexed_vec::Idx;

use holyjit_lib::lir;

use std::collections::HashMap;

/// Contains all the logic for converting a Rust Mir graph into an HolyJit
/// Lir graph.
pub struct Transpiler<'a, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    lir: Option<lir::CompilationUnit>,

    // Needed to get the layout from a given type.
    param_env: ty::ParamEnv<'tcx>,

    /// Map indexes from the Mir to indexes of the Lir.
    blocks_map: HashMap<mir::BasicBlock, lir::BasicBlock>,
    last_block: lir::BasicBlock,

    // Map a local id, to its offsets from the beginning of the stack. If
    // the stack grows down, this value should be removed from the stack
    // pointer to obtain the desired location.
    locals_map: HashMap<mir::Local, Local<'tcx>>,
    fp: lir::Reg,

    // Collect the list of static operands which are not yet bound to any
    // addresses. These would be attached to the HolyJitFnWrapper constant,
    // in order to collect their values.m/
    statics: Vec<mir::Rvalue<'tcx>>,
    statics_size: usize,

    // The lir is an infinite register language.  Each register being
    // represented by a number. This number is used for allocating new
    // registers in the MIR.
    last_reg: lir::Reg,
}

struct Local<'tcx> {
    idx: mir::Local,
    ty: ty::Ty<'tcx>,
    off: usize,
    size: usize,
}

/// Determine if the address or the value should be returned to the caller.
#[derive(Debug)]
enum LvalueCtx {
    Value,
    Address,
}

#[derive(Debug)]
pub enum Error {
    UnknownType,
    TooLargeType,

    /// Inline Assembly code is not supported for the moment, and probably
    /// would not be in the future versions of HolyJit.
    InlineAssembly,

    /// Not Yet Implemented.
    NYI,
}

macro_rules! report_nyi {
    ($($msg:expr),*) => {
        |e| match e {
            Error::NYI => panic!($($msg),*),
            e => Err(e)
        }
    }
}    

impl<'tcx> From<ty::layout::LayoutError<'tcx>> for Error {
    fn from(err: ty::layout::LayoutError<'tcx>) -> Error {
        println!("error: {}", err);
        match err {
            ty::layout::LayoutError::Unknown(_) => Error::UnknownType,
            ty::layout::LayoutError::SizeOverflow(_) => Error::TooLargeType,
        }
    }
}

impl<'a, 'tcx> Transpiler<'a, 'tcx> {
    pub fn new(tcx: TyCtxt<'a, 'tcx, 'tcx>, fn_id: DefId) -> Self {
        Self {
            tcx: tcx,
            lir: None,
            param_env: tcx.param_env(fn_id),
            blocks_map: HashMap::new(),
            last_block: 0,
            locals_map: HashMap::new(),
            fp: 0,
            statics: vec![],
            statics_size: 0,
            last_reg: 1,
        }
    }
}

/// This type correspons to the sequences of instructions build by the
/// statements content. Each have a return type and a return register.
#[derive(Debug)]
struct InstSeq<'tcx>(Vec<lir::Inst>, lir::Reg, ty::Ty<'tcx>);
struct TermSeq(Vec<lir::Inst>, lir::Terminator);

impl<'a, 'tcx> Transpiler<'a, 'tcx> {
    pub fn convert(mut self, mir: &mir::Mir<'tcx>) -> Result<(Vec<u8>, Vec<mir::Rvalue<'tcx>>), Error> {
        self.locals(mir)?;
        self.graph(mir)?;

        Ok((
            vec![0,1,2,3,4,5,6,7,8,9],
            self.statics
        ))
    }

    fn mir(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn locals(&mut self, mir: &mir::Mir<'tcx>) -> Result<(), Error> {
        // Loop over arguments and locals to find out about their sizes
        // (librustc/ty/layout.rs), and where they should be allocated.
        //
        // see ty::layout::Layout::compute_uncached(tcx, ??, ty)
        //
        // let layout = ty.layout()?;
        //
        // see librustc_mir/transform/inline.rs  (type_size_of)
        //
        let mut stack : usize = 0;
        for (local, decl) in mir.local_decls.iter_enumerated() {
            let layout = decl.ty.layout(self.tcx, self.param_env)?;
            let size = layout.size(&self.tcx.data_layout).bytes() as usize;
            let align = layout.align(&self.tcx.data_layout).pref() as usize;
            println!("local {:?} : {} =>: size: {} ; align: {:?}", local, mir.local_decls[local].ty, size, align);

            // Record the aligned address of beginning of the local's
            // memory.  (assumes the stack is growing down)
            let data = {
                // If the
                match (size, mir.local_kind(local)) {
                    (0, _) => Local {
                        idx: local,
                        ty: decl.ty,
                        off: 0,
                        size: 0,
                    },
                    (_, mir::LocalKind::ReturnPointer) |
                    (_, mir::LocalKind::Arg) => Local {
                        idx: local,
                        ty: decl.ty,
                        off: 0, // FIXME
                        size: 0,
                    },
                    (_, mir::LocalKind::Var) |
                    (_, mir::LocalKind::Temp) => {
                        // Bump the stack pointer with the size of the local.
                        let (off, bump) = self.add_type(decl.ty, stack)?;
                        stack = bump;

                        Local {
                            idx: local,
                            ty: decl.ty,
                            off: off,
                            size: bump - off,
                        }
                    },
                }
            };

            self.locals_map.insert(local, data);
        }

        self.lir = Some(lir::CompilationUnit {
            stack_size: stack,
            blocks: vec![],
        });
        Ok(())
    }

    fn get_block(&mut self, mir: mir::BasicBlock) -> lir::BasicBlock {
        match self.blocks_map.get(&mir) {
            Some(lir) => return *lir,
            None => (),
        };

        let lir = self.last_block;
        self.last_block += 1;
        let id = self.blocks_map.insert(mir, lir);
        lir
    }

    fn graph(&mut self, mir: &mir::Mir<'tcx>) -> Result<(), Error> {
        // let mut lir_blocks : Vec<lir::Blocks> = vec![];
        for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
            let lbb = self.get_block(bb);
            let mut lblock_insts : Vec<lir::Inst> = vec![];

            // Append the instructions needed for the execution of every
            // staements.
            for statement in &bb_data.statements {
                let mut insts = self.statement(statement)?;
                lblock_insts.append(&mut insts);
            };

            let term = &bb_data.terminator;
            let term = term.as_ref().unwrap();
            let TermSeq(mut term_insts, terminator) =
                self.terminator(term).or_else(
                    report_nyi!("BBData( terminator: {:?} , .. )", bb_data.terminator))?;

            lblock_insts.append(&mut term_insts);
            // TODO: Create a basic block.
        }
        Ok(())
    }

    fn statement(&mut self, statement: &mir::Statement<'tcx>) -> Result<Vec<lir::Inst>, Error> {
        match statement.kind {
            mir::StatementKind::Assign(ref lvalue, ref rvalue) => {
                // Collect the sequence of instruction which fetch the
                // memory location.
                let InstSeq(mut lv_insts, lv_reg, lv_ty) =
                    self.lvalue(lvalue, LvalueCtx::Address).or_else(
                        report_nyi!("StatementKind::Assign({:?}, _)", lvalue))?;

                // Collect the sequence of instruction to fetch the value.
                let InstSeq(mut rv_insts, rv_reg, rv_ty) =
                    self.rvalue(rvalue).or_else(
                        report_nyi!("StatementKind::Assign(_, {:?})", rvalue))?;

                let (_, size) = self.add_type(rv_ty, 0)?;
                println!("Assign size: {}", size);
                let mut insts = rv_insts;
                insts.append(&mut lv_insts);
                insts.append(&mut vec![
                    lir::Inst::Store(lv_reg, rv_reg, size),
                    lir::Inst::Dead(lv_reg),
                    lir::Inst::Dead(rv_reg),
                ]);

                Ok(insts)
            }
            mir::StatementKind::SetDiscriminant{ ref lvalue, variant_index } => {
                Err(Error::NYI)
            }
            mir::StatementKind::InlineAsm { .. } => {
                Err(Error::InlineAssembly)
            }

            // For the moment, let's consider all storages to be always
            // live.
            mir::StatementKind::StorageLive(_) |
            mir::StatementKind::StorageDead(_) |
            mir::StatementKind::EndRegion(_) |
            mir::StatementKind::Nop => Ok(vec![]),
        }
    }

    fn get_new_reg(&mut self) -> lir::Reg {
        let r = self.last_reg.clone();
        self.last_reg += 1;
        r
    }

    fn type_of_local(&self, index: mir::Local) -> ty::Ty<'tcx> {
        self.locals_map[&index].ty
    }
    fn offset_of_local(&self, index: mir::Local) -> usize {
        self.locals_map[&index].off
    }
    fn size_of_local(&self, index: mir::Local) -> usize {
        self.locals_map[&index].size
    }

    fn offset_of(&self, ty: ty::Ty<'tcx>, field: &mir::Field) -> Result<usize, Error> {
        let layout = ty.layout(self.tcx, self.param_env)?;
        Ok(layout.field_offset(&self.tcx.data_layout, field.index(), None).bytes() as usize)
    }

    /// Given a type and the last bump size pointer, this function look at
    /// the alignment and the size of the type and returns a tuple which
    /// contains the offset of the given type, and the new bump-offset for
    /// futher allocations.
    fn add_type(&self, ty: ty::Ty<'tcx>, bump: usize) -> Result<(usize, usize), Error> {
        let layout = ty.layout(self.tcx, self.param_env)?;
        let size = layout.size(&self.tcx.data_layout).bytes() as usize;
        let align = layout.align(&self.tcx.data_layout).pref() as usize;

        // align the bump offset.
        let miss_align = bump & (align - 1);
        let missing = (align - miss_align) & (align - 1);
        let base = bump + missing;

        Ok((base, base + size))
    }

    fn register_static(&mut self, rvalue: &mir::Rvalue<'tcx>, ty: ty::Ty<'tcx>) -> Result<lir::Imm, Error> {
        println!("register_static: {:?}", rvalue);
        self.statics.push(rvalue.clone());
        let bump = self.statics_size;
        let (off, bump) = self.add_type(ty, bump)?;
        self.statics_size = bump;
        Ok(off as lir::Imm)
    }

    fn lvalue<'b>(&'b mut self, lvalue: &'b mir::Lvalue<'tcx>, lvctx: LvalueCtx) -> Result<InstSeq<'tcx>, Error>
    {
        match *lvalue {
            mir::Lvalue::Local(index) => {
                let imm = self.get_new_reg();
                let reg = self.get_new_reg();
                Ok(InstSeq(vec![
                    lir::Inst::Live(imm),
                    lir::Inst::CopyImm(imm, self.offset_of_local(index) as lir::Imm,
                                       self.size_of_local(index) as lir::Sz),
                    lir::Inst::Live(reg),
                    lir::Inst::Add(reg, self.fp, imm),
                    lir::Inst::Dead(imm),
                ], reg, self.type_of_local(index)))
            }
            mir::Lvalue::Static(ref def) => {
                // Lvalue::Static seems to be always wrapped under a
                // Rvalue::Ref, so forbid any lvalue of this type.
                println!("Lvalue::Static(def_id: {:?}, ty: {:?}) within {:?}", def.def_id,  def.ty, lvctx);
                unreachable!("Unexpected Lvalue::Static")
            }
            mir::Lvalue::Projection(ref proj) => {
                self.lvalue_projection(&proj, lvctx).or_else(
                    report_nyi!("mir::Lvalue::Projection(({:?})", proj))
            }
        }
    }

    fn lvalue_projection<'b>(&'b mut self, proj: &'b mir::LvalueProjection<'tcx>, lvctx: LvalueCtx) -> Result<InstSeq<'tcx>, Error>
    {
        // Projection<'tcx, Lvalue<'tcx>, Operand<'tcx>, Ty<'tcx>>
        let mir::Projection { ref base, ref elem } = *proj;
        let InstSeq(mut base_insts, base_reg, base_ty) = {
            self.lvalue(base, LvalueCtx::Value)?
        };
        match *elem {
            mir::ProjectionElem::Deref => {
                let InstSeq(addr_insts, addr_reg, addr_ty) = InstSeq(base_insts, base_reg, base_ty);
                match lvctx {
                    LvalueCtx::Value => {
                        let mut insts = addr_insts;
                        let reg = self.get_new_reg();
                        let (_, size) = self.add_type(addr_ty, 0)?;
                        let res_ty = addr_ty.builtin_deref(true, ty::LvaluePreference::NoPreference).unwrap().ty;
                        insts.append(&mut vec![
                            lir::Inst::Live(reg),
                            lir::Inst::Load(reg, addr_reg, size),
                            lir::Inst::Dead(addr_reg)
                        ]);
                        Ok(InstSeq(insts, reg, res_ty))
                    }
                    LvalueCtx::Address => {
                        Ok(InstSeq(addr_insts, addr_reg, addr_ty))
                    }
                }
            }
            mir::ProjectionElem::Field(ref field, ref ty) => {
                // Given a type, generate a field access for this type.
                match lvctx {
                    LvalueCtx::Value => {
                        let mut insts = base_insts;
                        let imm = self.get_new_reg();
                        let reg = self.get_new_reg();
                        let (_, size) = self.add_type(self.tcx.types.usize, 0)?;

                        insts.append(&mut vec![
                            lir::Inst::Live(imm),
                            lir::Inst::CopyImm(imm, self.offset_of(base_ty, field)? as lir::Imm, size as lir::Sz),
                            lir::Inst::Live(reg),
                            lir::Inst::Add(reg, base_reg, imm),
                            lir::Inst::Dead(imm),
                            lir::Inst::Dead(base_reg)
                        ]);
                        Ok(InstSeq(insts, reg, ty))
                    }
                    LvalueCtx::Address => Err(Error::NYI)
                }
            }
            mir::ProjectionElem::Index(ref operand) => {
                // base [ operand ]
                let InstSeq(idx_insts, idx_reg, idx_ty) =
                    self.operand(operand).or_else(
                        report_nyi!("mir::ProjectionElem::Index({:?})", operand))?;

                // Compute the re-aligned size of the element.
                let res_ty = base_ty.builtin_index().unwrap();
                let (_, elem_sz) = self.add_type(res_ty, 0)?;
                let (elem_sz, _) = self.add_type(res_ty, elem_sz)?;
                let (_, size) = self.add_type(self.tcx.types.usize, 0)?;

                match lvctx {
                    LvalueCtx::Value => {
                        let imm = self.get_new_reg();
                        let mul = self.get_new_reg();
                        let reg = self.get_new_reg();
                        let mut insts = idx_insts;
                        insts.append(&mut vec![
                            lir::Inst::Live(imm),
                            lir::Inst::CopyImm(imm, elem_sz as lir::Imm, size as lir::Sz),
                            lir::Inst::Live(mul),
                            lir::Inst::Mul(mul, idx_reg, imm),
                            lir::Inst::Dead(idx_reg),
                            lir::Inst::Dead(imm),
                        ]);
                        insts.append(&mut base_insts);
                        insts.append(&mut vec![
                            lir::Inst::Add(reg, base_reg, mul),
                            lir::Inst::Dead(base_reg),
                            lir::Inst::Dead(mul),
                        ]);

                        Ok(InstSeq(insts, reg, res_ty))
                    }
                    LvalueCtx::Address => Err(Error::NYI)
                }
            }
            _ => Err(Error::NYI)
        }
    }

    fn rvalue<'b>(&'b mut self, rvalue: &'b mir::Rvalue<'tcx>) -> Result<InstSeq<'tcx>, Error>
    {
        match *rvalue {
            mir::Rvalue::Use(ref operand) => {
                Ok(self.operand(operand).or_else(
                    report_nyi!("mir::Rvalue::Use({:?})", operand))?)
            }
            mir::Rvalue::Ref(ref region, _, ref lvalue) => {
                match lvalue {
                    // Static are references to a static variable or
                    // function, but the address of it is not knwon yet, as
                    // it depends on the assembly of the statically compiled
                    // program.
                    //
                    // Thus we have to fake a content of the same type, and
                    // in the place-holder structure added by the jit!
                    // macro, add a new reference to static variables.
                    &mir::Lvalue::Static(ref def) => {
                        let ty = self.tcx.mk_imm_ref(region.clone(), def.ty);
                        let reg = self.get_new_reg();
                        let off = self.register_static(rvalue, ty)?;

                        Ok(InstSeq(vec![
                            lir::Inst::Live(reg),
                            lir::Inst::Static(reg, off)
                        ], reg, ty))
                    },

                    // Local are already returning the address of the
                    // location in which the data is stored. So when a
                    // rvalue request the location of the lvalue, then we
                    // just return the unmodified lvalue.
                    &mir::Lvalue::Local(_) =>
                        self.lvalue(lvalue, LvalueCtx::Address).or_else(
                            report_nyi!("mir::Rvalue::Ref(_, _, {:?})", lvalue)),

                    _ => Err(Error::NYI)
                }
            }

            mir::Rvalue::Len(ref lvalue) => {
                let InstSeq(mut lv_insts, lv_reg, lv_ty) =
                    self.lvalue(lvalue, LvalueCtx::Address).or_else(
                        report_nyi!("mir::Rvalue::Len({:?})", lvalue))?;

                let layout = lv_ty.layout(self.tcx, self.param_env)?;
                println!("mir::Rvalue::Len({:?})", lvalue);
                println!("ty: {:?} / layout: {:?}", lv_ty, layout);
                // Assert that we have a FatPointer.
                //assert_eq!(layout.field_count(), 2);
                //assert_eq!(layout.field_type(&self.lcx, 1), self.tcx.types.usize);
                let offset = layout.field_offset(&self.tcx.data_layout, 1, None);
                let (_, size) = self.add_type(self.tcx.types.usize, 0)?;

                let off = self.get_new_reg();
                let tmp = self.get_new_reg();
                let reg = self.get_new_reg();
                let mut insts = lv_insts;
                insts.append(&mut vec![
                    lir::Inst::Live(off),
                    lir::Inst::CopyImm(off, offset.bytes() as lir::Imm, size as lir::Sz),
                    lir::Inst::Live(tmp),
                    lir::Inst::Add(tmp, lv_reg, off),
                    lir::Inst::Dead(off),
                    lir::Inst::Dead(lv_reg),
                    lir::Inst::Live(reg),
                    lir::Inst::Load(reg, tmp, size),
                    lir::Inst::Dead(tmp),
                ]);

                Ok(InstSeq(insts, reg, self.tcx.types.usize))
            }
            mir::Rvalue::Cast(ref kind, ref operand, ref cast_ty) => {
                let InstSeq(op_base, op_reg, _) =
                    self.operand(operand).or_else(
                        report_nyi!("mir::Rvalue::Cast(_, {:?}, _)", operand))?;
                match *kind {
                    mir::CastKind::ReifyFnPointer => Err(Error::NYI),
                    mir::CastKind::ClosureFnPointer => Err(Error::NYI),
                    mir::CastKind::UnsafeFnPointer => Err(Error::NYI),
                    mir::CastKind::Unsize => Err(Error::NYI),
                    mir::CastKind::Misc => {
                        let (_, size) = self.add_type(cast_ty, 0)?;
                        let reg = self.get_new_reg();
                        let mut insts = op_base;
                        insts.append(&mut vec![
                            lir::Inst::Resize(reg, op_reg, size),
                            lir::Inst::Dead(op_reg),
                        ]);
                        Ok(InstSeq(insts, reg, cast_ty))
                    }
                }
            }
            mir::Rvalue::CheckedBinaryOp(op, ref lhs, ref rhs) => {
                let InstSeq(mut lhs_insts, lhs_reg, lhs_ty) =
                    self.operand(lhs).or_else(
                        report_nyi!("mir::Rvalue::CheckedBinaryOp({:?}, {:?}, _)", op, lhs))?;
                let InstSeq(mut rhs_insts, rhs_reg, rhs_ty) =
                    self.operand(rhs).or_else(
                        report_nyi!("mir::Rvalue::CheckedBinaryOp({:?}, _, {:?})", op, rhs))?;
                println!("mir::Rvalue::CheckedBinaryOp({:?}, {:?}, {:?})", op, lhs, rhs);
                let val_ty = op.ty(self.tcx, lhs_ty, rhs_ty);
                let ty = self.tcx.intern_tup(&[val_ty, self.tcx.types.bool], false);

                let chk_reg = self.get_new_reg();
                let reg = self.get_new_reg();
                let mut insts = vec![];
                insts.append(&mut lhs_insts);
                insts.append(&mut rhs_insts);
                insts.push(lir::Inst::Live(reg));
                insts.append(&mut {match op {
                    mir::BinOp::Add => vec![ lir::Inst::Add(reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Sub => vec![ lir::Inst::Add(reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Eq  => vec![ lir::Inst::Eq (reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Lt  => vec![ lir::Inst::Lt (reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Ge  => vec![ lir::Inst::Ge (reg, lhs_reg, rhs_reg) ],
                    _ => return Err(Error::NYI)
                }});
                insts.append(&mut vec![
                    lir::Inst::Dead(lhs_reg),
                    lir::Inst::Dead(rhs_reg),
                    lir::Inst::Live(chk_reg),
                    lir::Inst::Chk(chk_reg, reg), // x -> (x, bool)
                    lir::Inst::Dead(reg),
                ]);

                Ok(InstSeq(insts, chk_reg, ty))
            }
            mir::Rvalue::BinaryOp(op, ref lhs, ref rhs) => {
                let InstSeq(mut lhs_insts, lhs_reg, lhs_ty) =
                    self.operand(lhs).or_else(
                        report_nyi!("mir::Rvalue::BinaryOp({:?}, {:?}, _)", op, lhs))?;
                let InstSeq(mut rhs_insts, rhs_reg, rhs_ty) =
                    self.operand(rhs).or_else(
                        report_nyi!("mir::Rvalue::BinaryOp({:?}, _, {:?})", op, rhs))?;

                let ty = op.ty(self.tcx, lhs_ty, rhs_ty);
                let reg = self.get_new_reg();
                let mut insts = vec![];
                insts.append(&mut lhs_insts);
                insts.append(&mut rhs_insts);
                insts.push(lir::Inst::Live(reg));
                insts.append(&mut {match op {
                    mir::BinOp::Add => vec![ lir::Inst::Add(reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Sub => vec![ lir::Inst::Add(reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Eq  => vec![ lir::Inst::Eq (reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Lt  => vec![ lir::Inst::Lt (reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Ge  => vec![ lir::Inst::Ge (reg, lhs_reg, rhs_reg) ],
                    _ => return Err(Error::NYI)
                }});
                insts.append(&mut vec![
                    lir::Inst::Dead(lhs_reg),
                    lir::Inst::Dead(rhs_reg)
                ]);

                Ok(InstSeq(insts, reg, ty))
            }
            mir::Rvalue::Aggregate(ref kind, ref operands) => {
                let mut operands : Vec<_> = {
                    let mut res = Vec::with_capacity(operands.len());
                    for op in operands {
                        res.push(
                            self.operand(op).or_else(
                                report_nyi!("mir::Rvalue::Aggregate(_, [.. {:?} ..])", op))?)
                    };
                    res
                };

                let (ty, variant) = match **kind {
                    mir::AggregateKind::Array(ty) =>
                        (self.tcx.mk_array(ty, operands.len()), 0),
                    mir::AggregateKind::Tuple => {
                        let tys : Vec<_> = operands.iter().map(|op| {
                            let &InstSeq(_, _, ty) = op;
                            ty
                        }).collect();
                        (self.tcx.intern_tup(&tys, true), 0)
                    }
                    mir::AggregateKind::Adt(def, variant, substs, _) =>
                        (self.tcx.mk_adt(def, substs), variant),
                    _ => return Err(Error::NYI)
                };

                let layout = ty.layout(self.tcx, self.param_env)?;
                let (_, size) = self.add_type(ty, 0)?;

                // Initialize the space for the type to 0.
                let reg = self.get_new_reg();
                let mut insts = vec![
                    lir::Inst::Live(reg),
                    lir::Inst::CopyImm(reg, 0 as lir::Imm, size as lir::Sz),
                ];

                match *layout {
                    ty::layout::Layout::Univariant { .. } => (),
                    ty::layout::Layout::General { discr, .. } => {
                        // Initialize the discriminant
                        let (dty, sz) = match discr {
                            ty::layout::Integer::I8 => (self.tcx.types.i8, 1),
                            _ => {
                                unreachable!("ty: {:?} / layout: {:?}", ty, layout);
                                return Err(Error::NYI)
                            }
                        };

                        let discr_reg = self.get_new_reg();
                        insts.append(&mut vec![
                            lir::Inst::Live(discr_reg),
                            lir::Inst::CopyImm(discr_reg, variant as lir::Imm, sz as lir::Sz),
                            lir::Inst::StoreInto(reg, discr_reg, 0, sz as lir::Sz),
                            lir::Inst::Dead(discr_reg),
                        ]);
                    }
                    _ => {
                        return Err(Error::NYI);
                    }
                }

                // Add each operand
                for (i, InstSeq(mut op_insts, op_reg, op_ty)) in operands.into_iter().enumerate() {
                    let (_, size) = self.add_type(op_ty, 0)?;
                    let off = layout.field_offset(&self.tcx.data_layout,
                                                  i, Some(variant)).bytes();
                    insts.append(&mut op_insts);
                    insts.append(&mut vec![
                        lir::Inst::StoreInto(reg, op_reg,
                                             off as lir::Sz,
                                             size as lir::Sz),
                        lir::Inst::Dead(op_reg),
                    ]);
                };

                Ok(InstSeq(insts, reg, ty))
            }
            _ => Err(Error::NYI)
        }
    }

    fn operand(&mut self, operand: &mir::Operand<'tcx>) -> Result<InstSeq<'tcx>, Error> {
        match *operand {
            mir::Operand::Consume(ref lvalue) => {
                Ok(self.lvalue(lvalue, LvalueCtx::Value).or_else(
                        report_nyi!("mir::Operand::Consume({:?})", lvalue))?)
            }
            mir::Operand::Constant(ref constant) => {
                let res = match constant.literal.clone() {
                    mir::Literal::Item { def_id, substs } => {
                        Err(Error::NYI)
                    },
                    mir::Literal::Value { ref value } if self.constval_use_static(value) => {
                        // Create a fake Rvalue which can be added in the
                        // list of statics.
                        let rv = mir::Rvalue::Use(operand.clone());

                        // Load the static in a register.
                        let reg = self.get_new_reg();
                        let ty = constant.ty;
                        let off = self.register_static(&rv, ty)?;
                        Ok(InstSeq(vec![
                            lir::Inst::Live(reg),
                            lir::Inst::Static(reg, off),
                        ], reg, ty))
                    },
                    mir::Literal::Value { value } => {
                        Ok(self.constval(value, constant.ty).or_else(
                            report_nyi!("mir::Literal::Value: {:?} ", constant.literal))?)
                    },
                    mir::Literal::Promoted { .. } => {
                        Err(Error::NYI)
                    },
                };

                Ok(res.or_else(report_nyi!("mir::Operand::Constant({:?})", constant))?)
            }
        }
    }

    fn constval_use_static(&self, cv: &ConstVal) -> bool {
        match cv {
            &ConstVal::Function(_, _) => true,
            &ConstVal::Str(_) => true,
            _ => false,
        }
    }

    fn constval(&mut self, cv: ConstVal, ty: ty::Ty<'tcx>) -> Result<InstSeq<'tcx>, Error> {
        let reg = self.get_new_reg();
        let mut insts = vec![
            lir::Inst::Live(reg),
        ];
        let (_, size) = self.add_type(ty, 0)?;
        let mut assign = match cv {
            ConstVal::Bool(v) => vec![
                lir::Inst::CopyImm(reg, v as lir::Imm, size)
            ],
            ConstVal::Integral(ConstInt::I32(i)) => vec![
                lir::Inst::CopyImm(reg, i as lir::Imm, size)
            ],
            ConstVal::Integral(ConstInt::U8(i)) => vec![
                lir::Inst::CopyImm(reg, i as lir::Imm, size)
            ],
            ConstVal::Integral(ConstInt::Usize(ConstUsize::Us64(i))) => vec![
                lir::Inst::CopyImm(reg, i as lir::Imm, size)
            ],
            ConstVal::Integral(ConstInt::Usize(ConstUsize::Us64(i))) => vec![
                lir::Inst::CopyImm(reg, i as lir::Imm, size)
            ],
            ConstVal::Function(_, _) |
            ConstVal::Str(_) =>
                unreachable!("constval use static: cv: {:?} / ty: {:?}", cv, ty),
            _ => {
                println!("cv: {:?} / ty: {:?}", cv, ty);
                return Err(Error::NYI)
            }
        };
        insts.append(&mut assign);

        Ok(InstSeq(insts, reg, ty))
    }

    fn terminator(&mut self, terminator: &mir::Terminator<'tcx>) -> Result<TermSeq, Error> {
        let &mir::Terminator{ ref source_info, ref kind } = terminator;
        match kind {
            &mir::TerminatorKind::Resume => {
                Ok(TermSeq(vec![], lir::Terminator::Unwind))
            }
            &mir::TerminatorKind::Return => {
                Ok(TermSeq(vec![], lir::Terminator::Return))
            }
            &mir::TerminatorKind::Goto { ref target } => {
                Ok(TermSeq(vec![], lir::Terminator::Goto {
                    target: self.get_block(*target)
                }))
            }
            &mir::TerminatorKind::SwitchInt {
                ref discr,
                ref switch_ty,
                ref values,
                ref targets,
            } => {
                let InstSeq(idx_insts, idx_reg, idx_ty) =
                    self.operand(discr).or_else(
                        report_nyi!("mir::TerminatorKind::SwitchInt(discr: {:?})", discr))?;
                let range : lir::RangeInclusive = {
                    if switch_ty.sty == ty::TypeVariants::TyBool {
                        (0 as lir::Imm, 1 as lir::Imm)
                    } else {
                        let (_, size) = self.add_type(switch_ty, 0)?;
                        if size > ((0 as lir::Imm).count_zeros() as usize / 8) {
                            return Err(Error::NYI)
                        }
                        (isize::max_value() >> (8 - size), isize::min_value() >> (8 - size))
                    }
                };

                assert_eq!(values.len() + 1, targets.len());
                Ok(TermSeq(idx_insts, lir::Terminator::SwitchInt {
                    range: range,
                    targets: values.iter().zip(targets.iter()).map(|(v, t)| {
                        (v.to_u128_unchecked() as lir::Imm, self.get_block(*t))
                    }).collect(),
                    otherwise: Some(self.get_block(*targets.iter().last().unwrap())),
                }))
            }
            &mir::TerminatorKind::Call {
                ref func,
                ref args,
                ref destination,
                ref cleanup,
            } => {
                let InstSeq(mut insts, fun_reg, fun_ty) =
                    self.operand(func).or_else(
                        report_nyi!("mir::TerminatorKind::Call(func: {:?})", func))?;

                // TODO: collect the ABI with:
                //    let sig = ty.fn_sig(self.tcx)
                //    let sig = bcx.tcx().erase_late_bound_regions_and_normalize(&sig);
                //    let abi = sig.abi;

                let mut args_reg = Vec::with_capacity(args.len());
                for arg in args {
                    let InstSeq(mut arg_insts, arg_reg, _) =
                        self.operand(arg).or_else(
                            report_nyi!("mir::TerminatorKind::Call(args: [.. {:?} ..])", arg))?;
                    insts.append(&mut arg_insts);
                    args_reg.push(arg_reg);
                }

                let unwind_target = match cleanup {
                    &Some(ref cleanup_bb) => Some(self.get_block(*cleanup_bb)),
                    &None => None,
                };

                let return_target = match destination {
                    &Some((ref lvalue, ref bb)) => {
                        let target = self.get_block(*bb);
                        let InstSeq(mut lv_insts, lv_reg, lv_ty) =
                            self.lvalue(lvalue, LvalueCtx::Address).or_else(
                                report_nyi!("Call(destination: {:?}, _)", lvalue))?;
                        // TODO: Record a block entry, which assign the
                        // output register in the lvalue.
                        let (_, size) = self.add_type(lv_ty, 0)?;
                        Some(( (lv_reg, size), target ))
                    }
                    &None => None,
                };

                Ok(TermSeq(insts, lir::Terminator::Call {
                    conditional: None,
                    function: fun_reg,
                    args: args_reg,
                    return_target,
                    unwind_target,
                }))
            }
            &mir::TerminatorKind::Drop { .. } => {
                Ok(TermSeq(vec![], lir::Terminator::Return))  // TODO
            }
            &mir::TerminatorKind::Assert { .. } => {
                Ok(TermSeq(vec![], lir::Terminator::Return))  // TODO
            }
            _ => Err(Error::NYI)
        }
    }
}
