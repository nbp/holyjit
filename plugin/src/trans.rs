/// This module converts the rustc::mir into a holyjit::lir.

use std::collections::HashMap;
use std::boxed::Box;
use std::mem;

use rustc::ty::{self, TyCtxt};
use rustc::ty::subst::Subst;
use rustc::mir;
use rustc::hir::def_id::DefId;
use rustc::middle::const_val::{ConstVal, ConstInt};
use rustc::middle::lang_items::DropInPlaceFnLangItem;
use rustc_const_math::ConstUsize;
use rustc_data_structures::indexed_vec::Idx;

use syntax_pos::DUMMY_SP;

use holyjit_lib::lir;

use bincode;

/// Contains all the logic for converting a Rust Mir graph into an HolyJit
/// Lir graph.
pub struct Transpiler<'a, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'tcx, 'tcx>,

    // Needed to get the layout from a given type.
    param_env: ty::ParamEnv<'tcx>,

    /// Map indexes from the Mir to indexes of the Lir.
    blocks_map: HashMap<mir::BasicBlock, lir::BasicBlock>,
    // last_block: lir::BasicBlock,
    blocks: Vec<lir::BasicBlockData>,

    // Map a local id, to its offsets from the beginning of the stack. If
    // the stack grows down, this value should be removed from the stack
    // pointer to obtain the desired location.
    locals_map: HashMap<mir::Local, Local<'tcx>>,
    fp: lir::Reg,
    ret_local: Option<mir::Local>,

    // Collect the list of static operands which are not yet bound to any
    // addresses. These would be attached to the HolyJitFnWrapper constant,
    // in order to collect their values.m/
    statics: Vec<mir::Rvalue<'tcx>>,
    statics_size: usize,

    // The lir is an infinite register language.  Each register being
    // represented by a number. This number is used for allocating new
    // registers in the MIR.
    last_reg: lir::Reg,

    /// Map indexes of the function promoted MIR, to indexes of the of the
    /// contant promoted MIR.
    promoted_map: HashMap<mir::Promoted, mir::Promoted>,
    // Number of promoted blocks which are already listed in the constant MIR
    // before attaching any new promoted MIR ot it.
    promoted_idx: usize,
}

#[derive(Debug)]
struct Local<'tcx> {
    _idx: mir::Local,
    ty: ty::Ty<'tcx>,
    off: usize,
    size: usize,
    reg: Option<lir::Reg>,
}

/// Determine if the address or the value should be returned to the caller.
#[derive(Debug, Copy, Clone)]
enum PlaceCtx {
    Value,
    Address,
    RefSlice,
}

#[derive(Debug)]
pub enum Error {
    UnknownType,
    TooLargeType,

    // Validate statement is used by Miri to ensure that the content of a given
    // place contains data which correspond to the type assumption. Maybe we
    // could later generate proper assertions as well.
    ValidateStmt,

    /// Inline Assembly code is not supported for the moment, and probably
    /// would not be in the future versions of HolyJit.
    InlineAssembly,

    /// Serialization error while converting the Lir into a vector of bytes.
    Serialize,

    /// PlaceCtx::RefSlice is used to indicate that a Deref projection is
    /// not supposed to do anything because it would depend on the consumer.
    UnexpectedRefSliceCtx,

    /// Not Yet Implemented.
    NYI,
}

macro_rules! report_nyi {
    ($($msg:expr),*) => {
        |e| match e {
            Error::UnexpectedRefSliceCtx |
            Error::NYI => panic!($($msg),*),
            e => Err(e)
        }
    }
}

impl<'tcx> From<ty::layout::LayoutError<'tcx>> for Error {
    fn from(err: ty::layout::LayoutError<'tcx>) -> Error {
        println!("ty::layout::LayoutError = {}", err);
        match err {
            ty::layout::LayoutError::Unknown(_) => Error::UnknownType,
            ty::layout::LayoutError::SizeOverflow(_) => Error::TooLargeType,
        }
    }
}

impl<'tcx> From<Box<bincode::ErrorKind>> for Error {
    fn from(err: Box<bincode::ErrorKind>) -> Error {
        println!("bincode::ErrorKind = {}", err);
        Error::Serialize
    }
}

// TODO: Have a look at Rust's src/librustc_trans/context.rs , and how CodegenCx is built.
impl<'a, 'tcx> Transpiler<'a, 'tcx> {
    pub fn new(tcx: TyCtxt<'a, 'tcx, 'tcx>, fn_id: DefId, nb_promoted: usize) -> Self {
        Self {
            tcx: tcx,
            param_env: tcx.param_env(fn_id),
            blocks_map: HashMap::new(),
            // last_block: 0,
            blocks: vec![],
            locals_map: HashMap::new(),
            fp: 0,
            ret_local: None,
            statics: vec![],
            statics_size: 0,
            last_reg: 1,
            promoted_map: HashMap::new(),
            promoted_idx: nb_promoted,
        }
    }
}

/*
impl<'a, 'tcx> ty::layout::HasDataLayout for &'a Transpiler<'a, 'tcx> {
    fn data_layout(&self) -> &ty::layout::TargetDataLayout {
        &self.tcx.data_layout
    }
}

impl<'a, 'tcx> ty::layout::HasTyCtxt<'tcx> for &'a Transpiler<'a, 'tcx> {
    fn tcx<'b>(&'b self) -> TyCtxt<'b, 'tcx, 'tcx> {
        self.tcx
    }
}
*/
/*
impl<'a, 'tcx> LayoutOf<Ty<'tcx>> for &'a Transpiler<'a, 'tcx> {
    type TyLayout = TyLayout<'tcx>;

    fn layout_of(self, ty: Ty<'tcx>) -> Self::TyLayout {
        self.tcx.layout_of(ty::ParamEnv::empty(traits::Reveal::All).and(ty))
            .unwrap_or_else(|e| match e {
                LayoutError::SizeOverflow(_) => panic!("LayoutError::SizeOverflow"),
                _ => panic!("failed to get layout for `{}`: {}", ty, e)
            })
    }
}
*/

/// This type correspons to the sequences of instructions build by the
/// statements content. Each have a return type and a return register.
#[derive(Debug)]
struct InstSeq<'tcx>(Vec<lir::Inst>, lir::Reg, ty::Ty<'tcx>);
struct TermSeq(Vec<lir::Inst>, lir::Terminator);

impl<'a, 'tcx> Transpiler<'a, 'tcx> {
    // This function output a vector of bytes which corresponds to the LIR, a
    // vector of rvalue which corresponds to all statics & promoted indexes, and
    // a vector of promoted blocks.
    pub fn convert(mut self, mir: &mir::Mir<'tcx>) -> Result<(Vec<u8>, Vec<mir::Rvalue<'tcx>>, Vec<mir::Mir<'tcx>>), Error> {
        let (args_defs, stack_size) = self.locals(mir)?;
        self.graph(mir, stack_size)?;

        let blocks = mem::replace(&mut self.blocks, Vec::new());

        let lir = lir::CompilationUnit {
            stack_size: stack_size,
            args_defs: args_defs,
            blocks: blocks,
        };
        println!("lir:\n{}\n", lir);
        let bytes : Vec<u8> = bincode::serialize(&lir, bincode::Infinite)?;

        // Copy promoted block to the constant, such that we can reference the
        // same value (which are not supposed to be mutable.)
        let mut promoted: Vec<_> = self.promoted_map.into_iter().map(|(tgt_idx, src_idx)|
            (tgt_idx, mir.promoted[src_idx].clone())
        ).collect();
        promoted.sort_by(|a, b| a.0.cmp(&b.0));
        let promoted : Vec<_> = promoted.into_iter().map(|a| a.1).collect();

        Ok((
            bytes,
            self.statics,
            promoted
        ))
    }

    /*
    fn mir(&mut self) -> Result<(), Error> {
        Ok(())
    }
     */

    fn locals(&mut self, mir: &mir::Mir<'tcx>) -> Result<(Vec<lir::ArgDef>, usize), Error> {
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
        let mut args_defs : Vec<lir::ArgDef> = Vec::with_capacity(mir.arg_count);
        for (local, decl) in mir.local_decls.iter_enumerated() {
            let layout = self.tcx.layout_of(self.param_env.and(decl.ty))?; // decl.ty.layout(self.tcx, self.param_env)?;
            let size = layout.size.bytes() as usize;
            let align = layout.align.pref() as usize; // (&self.tcx.data_layout)
            let local_kind = mir.local_kind(local);
            println!("local {:?} : {} =>: size: {} ; align: {:?} ; kind: {:?}", local, mir.local_decls[local].ty, size, align, local_kind);

            match local_kind {
                mir::LocalKind::ReturnPointer =>
                    self.ret_local = Some(local.clone()),
                _ => ()
            };

            // Record the aligned address of beginning of the local's
            // memory.  (assumes the stack is growing down)
            let data = {
                match local_kind {
                    mir::LocalKind::Arg => {
                        // Bump the stack pointer with the size of the local.
                        let (off, bump) = self.add_type(decl.ty, stack)?;
                        stack = bump;
                        let size = bump - off;

                        // Add the size of the content which is being copied,
                        // the JIT-compiler will have to infer if this is a
                        // pointer type or the content of it :/
                        args_defs.push((off as lir::Imm, size, (size <= 8) as lir::ByValue));

                        Local {
                            _idx: local,
                            ty: decl.ty,
                            off, size,
                            reg: None,
                        }
                    },
                    mir::LocalKind::ReturnPointer |
                    mir::LocalKind::Var |
                    mir::LocalKind::Temp => {
                        // Bump the stack pointer with the size of the local.
                        let (off, bump) = self.add_type(decl.ty, stack)?;
                        stack = bump;

                        Local {
                            _idx: local,
                            ty: decl.ty,
                            off: off,
                            size: bump - off,
                            reg: None,
                        }
                    },
                }
            };

            self.locals_map.insert(local, data);
        }

        Ok((args_defs, stack))
    }

    fn new_block(&mut self) -> lir::BasicBlock {
        // let lir = self.last_block;
        // self.last_block += 1;
        self.blocks.push(lir::BasicBlockData{
            input_regs: vec![],
            output_regs: vec![],
            insts: vec![],
            end: lir::Terminator::Unreachable,
        });
        self.blocks.len() - 1
    }

    fn get_block(&mut self, mir: mir::BasicBlock) -> lir::BasicBlock {
        match self.blocks_map.get(&mir) {
            Some(lir) => return *lir,
            None => (),
        };

        let lir = self.new_block();
        let _id = self.blocks_map.insert(mir, lir);
        lir
    }

    fn graph(&mut self, mir: &mir::Mir<'tcx>, stack_size: usize) -> Result<(), Error> {
        for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
            let lbb = self.get_block(bb);
            let mut lblock_insts : Vec<lir::Inst> = vec![];

            let inputs = if lbb == 0 {
                lblock_insts.append(&mut vec![
                    lir::Inst::SetFramePtr(self.fp, 8 /* size of frame ptr */, stack_size)
                ]);
                // The first block is assumed to copy all the arguments to
                // the stack, and these arguments should only be used once
                // in the entire graph, and marked as dead after the first
                // assignment.
                vec![]
            } else {
                vec![ (self.fp, 8 /* size of frame ptr */ ) ]
            };

            // Append the instructions needed for the execution of every
            // staements.
            for statement in &bb_data.statements {
                let mut insts = match self.statement(statement) {
                    Ok(res) => res,
                    Err(Error::ValidateStmt) => continue,
                    e => { e?; panic!("The impossible happened") }
                };
                lblock_insts.append(&mut insts);
            };

            let term = &bb_data.terminator;
            let term = term.as_ref().unwrap();
            let TermSeq(mut term_insts, terminator) =
                self.terminator(term).or_else(
                    report_nyi!("BBData( terminator: {:?} , .. )", bb_data.terminator))?;
            lblock_insts.append(&mut term_insts);

            let fp_reg = self.fp;
            self.blocks[lbb] = lir::BasicBlockData {
                input_regs: inputs,
                output_regs: vec![ fp_reg ],
                insts: lblock_insts,
                end: terminator,
            };
        }

        Ok(())
    }

    fn statement(&mut self, statement: &mir::Statement<'tcx>) -> Result<Vec<lir::Inst>, Error> {
        match statement.kind {
            mir::StatementKind::Validate(_, _) => {
                // Used by Miri as an assertion mechanism.
                Err(Error::ValidateStmt)
            }
            mir::StatementKind::Assign(ref lvalue, ref rvalue) => {
                // Collect the sequence of instruction to fetch the value.
                let InstSeq(rv_insts, rv_reg, rv_ty) =
                    self.rvalue(rvalue).or_else(
                        report_nyi!("StatementKind::Assign(_, {:?})", rvalue))?;

                // Collect the sequence of instruction which fetch the
                // memory location.
                let InstSeq(mut lv_insts, lv_reg, lv_ty) =
                    self.lvalue(lvalue, PlaceCtx::Address).or_else(
                        report_nyi!("StatementKind::Assign({:?}, _)", lvalue))?;

                let lv_ty = lv_ty.builtin_deref(true).unwrap().ty;
                let (_, lv_size) = self.add_type(lv_ty, 0)?;
                let (_, rv_size) = self.add_type(rv_ty, 0)?;

                self.check_same_size(lv_ty, rv_ty).or_else(
                    report_nyi!("Assign size: {} <- {} {{ {:?} }}", lv_size, rv_size, statement))?;
                let mut insts = rv_insts;
                insts.append(&mut lv_insts);
                insts.append(&mut vec![
                    lir::Inst::Store(lv_reg, rv_reg, rv_size),
                    lir::Inst::Dead(lv_reg),
                    lir::Inst::Dead(rv_reg),
                ]);

                Ok(insts)
            }
            mir::StatementKind::SetDiscriminant{ .. } => {
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

    fn reg_of_local(&self, index: mir::Local) -> Option<lir::Reg> {
        // println!("reg_of_local({:?}) == {:?}", index, self.locals_map[&index]);
        self.locals_map[&index].reg
    }
    fn type_of_local(&self, index: mir::Local) -> ty::Ty<'tcx> {
        self.locals_map[&index].ty
    }
    fn offset_of_local(&self, index: mir::Local) -> isize {
        0 - (self.locals_map[&index].off as isize) - (self.locals_map[&index].size as isize)
    }
    fn size_of_local(&self, index: mir::Local) -> usize {
        self.locals_map[&index].size
    }

    fn offset_of(&self, ty: ty::Ty<'tcx>, field: &mir::Field) -> Result<usize, Error> {
        let layout = self.tcx.layout_of(self.param_env.and(ty))?;
        let off = layout.fields.offset(field.index()).bytes();
        println!("offset_of({:?}, {:?}) -> offset: {:?}  {{ layout: {:?} }}", ty, field, off, layout);
        Ok(off as usize)
    }

    /// Given a type and the last bump size pointer, this function look at
    /// the alignment and the size of the type and returns a tuple which
    /// contains the offset of the given type, and the new bump-offset for
    /// futher allocations.
    fn add_type(&self, ty: ty::Ty<'tcx>, bump: usize) -> Result<(usize, usize), Error> {
        let layout = self.tcx.layout_of(self.param_env.and(ty))?;
        let size = layout.size.bytes() as usize;
        let align = layout.align.pref() as usize;

        // align the bump offset.
        let miss_align = bump & (align - 1);
        let missing = (align - miss_align) & (align - 1);
        let base = bump + missing;

        Ok((base, base + size))
    }

    fn check_same_size(&self, lhs: ty::Ty<'tcx>, rhs: ty::Ty<'tcx>) -> Result<(), Error> {
        let (_, lsz) = self.add_type(lhs, 0)?;
        let (_, rsz) = self.add_type(rhs, 0)?;
        if lsz != rsz {
            println!("left: {} ({:?}) ; right: {} ({:?})", lsz, lhs, rsz, rhs);
            return Err(Error::NYI);
        }

        Ok(())
    }

    fn register_static(&mut self, rvalue: &mir::Rvalue<'tcx>, ty: ty::Ty<'tcx>) -> Result<(lir::Imm, lir::Sz), Error> {
        self.statics.push(rvalue.clone());
        let bump = self.statics_size;
        let (off, bump) = self.add_type(ty, bump)?;
        self.statics_size = bump;
        let layout = self.tcx.layout_of(self.param_env.and(ty))?;
        println!("register_static:\n: {:?}\n: {:?}\n: {:?}\n: {:?}", rvalue, ty, bump - off, layout);
        Ok((off as lir::Imm, bump - off as lir::Sz))
    }

    fn register_promoted(&mut self, promoted: mir::Promoted) -> mir::Promoted {
        // This function does not clone the promoted MIR, but only keep the
        // index of which MIR needs to be cloned, such that when we reach back
        // the top of the function MIR, we can clone all the necessary promoted
        // MIR.
        let idx = mir::Promoted::new(self.promoted_idx);
        // (assume no duplicated?)
        self.promoted_map.insert(idx, promoted);
        self.promoted_idx += 1;
        idx
    }

    fn local(&mut self, index: mir::Local) -> Result<InstSeq<'tcx>, Error>
    {
        if let Some(reg) = self.reg_of_local(index) {
            let ty = self.type_of_local(index);
            let ptr_size = 8; // TODO: use proper pointer size.
            if self.size_of_local(index) > ptr_size {
                let val = self.get_new_reg();
                return Ok(InstSeq(vec![
                    lir::Inst::Live(val),
                    lir::Inst::Load(val, reg, self.size_of_local(index) as lir::Sz),
                    lir::Inst::Dead(reg),
                ], val, ty))
            }
            return Ok(InstSeq(vec![], reg, ty));
        }
        let off = self.get_new_reg();
        let ptr = self.get_new_reg();
        let reg = self.get_new_reg();
        Ok(InstSeq(vec![
            lir::Inst::Live(off),
            // Compute the offset within the stack frame.
            lir::Inst::CopyImm(off, self.offset_of_local(index) as lir::Imm,
                               8 as lir::Sz /* size of pointer */),
            lir::Inst::Live(ptr),
            // Add the offset to the stack frame pointer.
            lir::Inst::Add(ptr, self.fp, off),
            lir::Inst::Dead(off),
            lir::Inst::Live(reg),
            // Add the offset to the stack frame pointer.
            lir::Inst::Load(reg, ptr, self.size_of_local(index) as lir::Sz),
            lir::Inst::Dead(ptr),
        ], reg, self.type_of_local(index)))
    }

    fn lvalue<'b>(&'b mut self, lvalue: &'b mir::Place<'tcx>, lvctx: PlaceCtx) -> Result<InstSeq<'tcx>, Error>
    {
        match (lvalue, lvctx) {
            (&mir::Place::Local(index), PlaceCtx::Address) => {
                if let Some(_reg) = self.reg_of_local(index) {
                    return Err(Error::NYI);
                }
                let off = self.get_new_reg();
                let reg = self.get_new_reg();
                let ty = self.type_of_local(index);
                let ty = self.tcx.mk_imm_ref(self.tcx.types.re_erased, ty);
                // Check we return a pointer type.
                self.check_same_size(ty, self.tcx.types.usize)?;
                Ok(InstSeq(vec![
                    lir::Inst::Live(off),
                    // Compute the offset within the stack frame.
                    lir::Inst::CopyImm(off, self.offset_of_local(index) as lir::Imm,
                                       8 as lir::Sz /* size of pointer */),
                    lir::Inst::Live(reg),
                    // Add the offset to the stack frame pointer.
                    lir::Inst::Add(reg, self.fp, off),
                    lir::Inst::Dead(off),
                ], reg, ty))
            },
            (&mir::Place::Local(index), PlaceCtx::Value) => {
                if let Some(reg) = self.reg_of_local(index) {
                    let ty = self.type_of_local(index);
                    let ptr_size = 8; // TODO: use proper pointer size.
                    if self.size_of_local(index) > ptr_size {
                        let val = self.get_new_reg();
                        return Ok(InstSeq(vec![
                            lir::Inst::Live(val),
                            lir::Inst::Load(val, reg, self.size_of_local(index) as lir::Sz),
                            lir::Inst::Dead(reg),
                        ], val, ty))
                    }
                    return Ok(InstSeq(vec![], reg, ty));
                }
                let off = self.get_new_reg();
                let ptr = self.get_new_reg();
                let reg = self.get_new_reg();
                Ok(InstSeq(vec![
                    lir::Inst::Live(off),
                    // Compute the offset within the stack frame.
                    lir::Inst::CopyImm(off, self.offset_of_local(index) as lir::Imm,
                                       8 as lir::Sz /* size of pointer */),
                    lir::Inst::Live(ptr),
                    // Add the offset to the stack frame pointer.
                    lir::Inst::Add(ptr, self.fp, off),
                    lir::Inst::Dead(off),
                    lir::Inst::Live(reg),
                    // Add the offset to the stack frame pointer.
                    lir::Inst::Load(reg, ptr, self.size_of_local(index) as lir::Sz),
                    lir::Inst::Dead(ptr),
                ], reg, self.type_of_local(index)))
            },
            (&mir::Place::Static(ref def), _) => {
                // Place::Static seems to be always wrapped under a
                // Rvalue::Ref, so forbid any lvalue of this type.
                println!("Place::Static(def_id: {:?}, ty: {:?}) within {:?}", def.def_id,  def.ty, lvctx);
                unreachable!("Unexpected Place::Static")
            }
            (&mir::Place::Projection(ref proj), ref lvctx) => {
                self.lvalue_projection(&proj, *lvctx).or_else(
                    report_nyi!("mir::Place::Projection(({:?})", proj))
            }

            (_, PlaceCtx::RefSlice) => Err(Error::UnexpectedRefSliceCtx),
        }
    }

    fn lvalue_projection<'b>(&'b mut self, proj: &'b mir::PlaceProjection<'tcx>, lvctx: PlaceCtx) -> Result<InstSeq<'tcx>, Error>
    {
        // Projection<'tcx, Place<'tcx>, Operand<'tcx>, Ty<'tcx>>
        let mir::Projection { ref base, ref elem } = *proj;
        let InstSeq(mut base_insts, base_reg, base_ty) =
            match *elem {
                mir::ProjectionElem::Index(_) =>
                    self.lvalue(base, PlaceCtx::RefSlice)?,
                mir::ProjectionElem::Field(_, _) =>
                    self.lvalue(base, PlaceCtx::Address)?,
                _ => self.lvalue(base, PlaceCtx::Value)?
            };
        match *elem {
            mir::ProjectionElem::Deref => {
                let InstSeq(addr_insts, addr_reg, addr_ty) = InstSeq(base_insts, base_reg, base_ty);

                match lvctx {
                    PlaceCtx::Value => {
                        let mut insts = addr_insts;
                        let reg = self.get_new_reg();
                        let res_ty = addr_ty.builtin_deref(true).unwrap().ty;
                        let (_, size) = self.add_type(res_ty, 0)?;
                        insts.append(&mut vec![
                            lir::Inst::Live(reg),
                            lir::Inst::Load(reg, addr_reg, size),
                            lir::Inst::Dead(addr_reg)
                        ]);
                        Ok(InstSeq(insts, reg, res_ty))
                    }
                    PlaceCtx::RefSlice => {
                        // We are asking to dereference a fat-pointer, but
                        // we will either request the pointer of it or the
                        // length of it.
                        assert!(addr_ty.is_slice());
                        Ok(InstSeq(addr_insts, addr_reg, addr_ty))
                    }
                    PlaceCtx::Address => {
                        Ok(InstSeq(addr_insts, addr_reg, addr_ty))
                    }
                }
            }
            mir::ProjectionElem::Field(ref field, ref ty) => {
                // Given a type, generate a field access for this type.
                let struct_ty = base_ty.builtin_deref(true).unwrap().ty;
                match lvctx {
                    PlaceCtx::Value => {
                        let mut insts = base_insts;
                        let imm = self.get_new_reg();
                        let addr = self.get_new_reg();
                        let reg = self.get_new_reg();
                        let (_, size) = self.add_type(self.tcx.types.usize, 0)?;
                        let (_, ty_sz) = self.add_type(ty, 0)?;
                        // We are expecting a pointer, in both cases.
                        self.check_same_size(base_ty, self.tcx.types.usize)?;

                        insts.append(&mut vec![
                            lir::Inst::Live(imm),
                            lir::Inst::CopyImm(imm, self.offset_of(struct_ty, field)? as lir::Imm, size as lir::Sz),
                            lir::Inst::Live(addr),
                            lir::Inst::Add(addr, base_reg, imm),
                            lir::Inst::Dead(imm),
                            lir::Inst::Dead(base_reg),
                            lir::Inst::Live(reg),
                            lir::Inst::Load(reg, addr, ty_sz),
                            lir::Inst::Dead(addr),
                        ]);
                        Ok(InstSeq(insts, reg, ty))
                    }
                    PlaceCtx::Address => {
                        let mut insts = base_insts;
                        let imm = self.get_new_reg();
                        let reg = self.get_new_reg();
                        let (_, size) = self.add_type(self.tcx.types.usize, 0)?;
                        let res_ty = self.tcx.mk_mut_ptr(ty);
                        // We are expecting a pointer, in both cases.
                        self.check_same_size(base_ty, self.tcx.types.usize)?;
                        self.check_same_size(base_ty, res_ty)?;

                        insts.append(&mut vec![
                            lir::Inst::Live(imm),
                            lir::Inst::CopyImm(imm, self.offset_of(struct_ty, field)? as lir::Imm, size as lir::Sz),
                            lir::Inst::Live(reg),
                            lir::Inst::Add(reg, base_reg, imm),
                            lir::Inst::Dead(imm),
                            lir::Inst::Dead(base_reg)
                        ]);
                        Ok(InstSeq(insts, reg, res_ty))
                    },
                    PlaceCtx::RefSlice => Err(Error::UnexpectedRefSliceCtx),
                }
            }
            mir::ProjectionElem::Index(ref index) => {
                // base [ operand ]
                let InstSeq(idx_insts, idx_reg, _idx_ty) =
                    self.local(*index).or_else(
                        report_nyi!("mir::ProjectionElem::Index({:?})", index))?;

                let InstSeq(mut base_insts, base_reg, base_ty) = {
                    // Assert that base is FatPointer given by value. Note
                    // we use the RefSlice type, to skip the Defer operation
                    // made on the lvalue.
                    assert!(base_ty.is_slice()); // TODO: add TyArray support?
                    let slice_ty = base_ty.builtin_deref(true).unwrap().ty;
                    let elem_ty = slice_ty.builtin_index().unwrap();
                    let res_ty = self.tcx.mk_mut_ptr(elem_ty);

                    let layout = self.tcx.layout_of(self.param_env.and(base_ty))?;
                    let offset = layout.fields.offset(ty::layout::FAT_PTR_ADDR);
                    let (_, size) = self.add_type(res_ty, 0)?;

                    let reg = self.get_new_reg();
                    base_insts.append(&mut vec![
                        lir::Inst::Live(reg),
                        lir::Inst::LoadFrom(reg, base_reg, offset.bytes() as lir::Sz, size),
                        lir::Inst::Dead(base_reg),
                    ]);

                    InstSeq(base_insts, reg, res_ty)
                };

                // Compute the re-aligned size of the element.
                let res_ty = base_ty.builtin_deref(true).unwrap().ty;
                let elem_sz = self.tcx.layout_of(self.param_env.and(res_ty))?.size.bytes() as lir::Sz;

                // We are expecting a pointer, in both cases.
                self.check_same_size(base_ty, self.tcx.types.usize)?;
                let (_, idx_size) = self.add_type(self.tcx.types.usize, 0)?;

                match lvctx {
                    PlaceCtx::Value => {
                        let imm = self.get_new_reg();
                        let mul = self.get_new_reg();
                        let reg = self.get_new_reg();
                        let val = self.get_new_reg();
                        let mut insts = idx_insts;
                        insts.append(&mut vec![
                            lir::Inst::Live(imm),
                            lir::Inst::CopyImm(imm, elem_sz as lir::Imm, idx_size as lir::Sz),
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
                            lir::Inst::Load(val, reg, elem_sz),
                            lir::Inst::Dead(reg),
                        ]);

                        Ok(InstSeq(insts, val, res_ty))
                    }
                    PlaceCtx::RefSlice => Err(Error::UnexpectedRefSliceCtx),
                    PlaceCtx::Address => Err(Error::NYI),
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
                    &mir::Place::Static(ref def) => {
                        let ty = self.tcx.mk_imm_ref(region.clone(), def.ty);
                        let reg = self.get_new_reg();
                        let (off, sz) = self.register_static(rvalue, ty)?;

                        Ok(InstSeq(vec![
                            lir::Inst::Live(reg),
                            lir::Inst::Static(reg, off, sz)
                        ], reg, ty))
                    },

                    // Local are already returning the address of the
                    // location in which the data is stored. So when a
                    // rvalue request the location of the lvalue, then we
                    // just return the unmodified lvalue.
                    &mir::Place::Local(_) =>
                        self.lvalue(lvalue, PlaceCtx::Address).or_else(
                            report_nyi!("mir::Rvalue::Ref(_, _, {:?})", lvalue)),

                    _ => Err(Error::NYI)
                }
            }

            mir::Rvalue::Len(ref lvalue) => {
                let InstSeq(lv_insts, lv_reg, lv_ty) =
                    self.lvalue(lvalue, PlaceCtx::RefSlice).or_else(
                        report_nyi!("mir::Rvalue::Len({:?})", lvalue))?;

                let layout = self.tcx.layout_of(self.param_env.and(lv_ty))?;
                println!("mir::Rvalue::Len({:?})", lvalue);
                println!("ty: {:?} / layout: {:?}", lv_ty, layout);

                // Assert that base is FatPointer given by value. Note we
                // use the RefSlice type, to skip the Defer operation made
                // on the lvalue.
                assert!(lv_ty.is_slice());

                //assert_eq!(layout.field_count(), 2);
                //assert_eq!(layout.field_type(&self.lcx, 1), self.tcx.types.usize);
                let offset = layout.fields.offset(ty::layout::FAT_PTR_EXTRA);
                let (_, size) = self.add_type(self.tcx.types.usize, 0)?;
                // We are expecting a pointer, in both cases.
                //self.check_same_size(lv_ty, self.tcx.types.usize)?;

                let reg = self.get_new_reg();
                let mut insts = lv_insts;
                insts.append(&mut vec![
                    lir::Inst::Live(reg),
                    lir::Inst::LoadFrom(reg, lv_reg, offset.bytes() as lir::Sz, size),
                    lir::Inst::Dead(lv_reg),
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
                // Check that both operands have the same size.
                self.check_same_size(lhs_ty, rhs_ty)?;

                let chk_reg = self.get_new_reg();
                let reg = self.get_new_reg();
                let mut insts = vec![];
                insts.append(&mut lhs_insts);
                insts.append(&mut rhs_insts);
                insts.push(lir::Inst::Live(reg));
                insts.append(&mut {match op {
                    mir::BinOp::Add => vec![ lir::Inst::Add(reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Sub => vec![ lir::Inst::Sub(reg, lhs_reg, rhs_reg) ],
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
                // Check that both operands have the same size.
                self.check_same_size(lhs_ty, rhs_ty)?;
                let reg = self.get_new_reg();
                let mut insts = vec![];
                insts.append(&mut lhs_insts);
                insts.append(&mut rhs_insts);
                insts.push(lir::Inst::Live(reg));
                insts.append(&mut {match op {
                    mir::BinOp::Add => vec![ lir::Inst::Add(reg, lhs_reg, rhs_reg) ],
                    mir::BinOp::Sub => vec![ lir::Inst::Sub(reg, lhs_reg, rhs_reg) ],
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
                let operands : Vec<_> = {
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
                        (self.tcx.mk_array(ty, operands.len() as u64), 0),
                    mir::AggregateKind::Tuple => {
                        let tys : Vec<_> = operands.iter().map(|op| {
                            let &InstSeq(_, _, ty) = op;
                            ty
                        }).collect();
                        (self.tcx.intern_tup(&tys, false), 0)
                    }
                    mir::AggregateKind::Adt(def, variant, substs, _) =>
                        (self.tcx.mk_adt(def, substs), variant),
                    _ => return Err(Error::NYI)
                };

                let layout = self.tcx.layout_of(self.param_env.and(ty))?;
                let (_, size) = self.add_type(ty, 0)?;

                // Initialize the space for the type to 0.
                let reg = self.get_new_reg();
                let mut insts = vec![
                    lir::Inst::Live(reg),
                    lir::Inst::CopyImm(reg, 0 as lir::Imm, size as lir::Sz),
                ];

                let fields = match &layout.variants {
                    &ty::layout::Variants::Single { .. } => &layout.fields,
                    // Layout::Univariant -> Variants::Tagged { .. smae thing .. }
                    &ty::layout::Variants::Tagged { ref discr, ref variants, .. } => {
                        // Initialize the discriminant
                        let (_dty, sz) = match discr.value {
                            ty::layout::Primitive::Int(ty::layout::Integer::I8, _) => (self.tcx.types.i8, 1),
                            _ => {
                                unreachable!("ty: {:?} / layout: {:?}", ty, layout);
                            }
                        };

                        let discr_reg = self.get_new_reg();
                        insts.append(&mut vec![
                            lir::Inst::Live(discr_reg),
                            lir::Inst::CopyImm(discr_reg, variant as lir::Imm, sz as lir::Sz),
                            lir::Inst::StoreInto(reg, discr_reg, 0, sz as lir::Sz),
                            lir::Inst::Dead(discr_reg),
                        ]);

                        &variants[variant].fields
                    }
                    _ => { // Variants::NicheFilling
                        return Err(Error::NYI);
                    }
                };

                // Add each operand
                for (i, InstSeq(mut op_insts, op_reg, op_ty)) in operands.into_iter().enumerate() {
                    let (_, size) = self.add_type(op_ty, 0)?;
                    let off = fields.offset(i).bytes();
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
            mir::Rvalue::Discriminant(ref place) => {
                // Get the place as a value.
                let InstSeq(mut insts, reg, ty) = self.lvalue(place, PlaceCtx::Value)?;
                // Extract the field which corresponds to the discriminant.
                let layout = self.tcx.layout_of(self.param_env.and(ty))?;
                // Read the discriminant of an ADT.
                match &layout.variants {
                    // What?! Rustc produces a discriminant rvalue on a type
                    // which is neither a tagged place nor a niche, we should
                    // not generate anything for such assignment.
                    &ty::layout::Variants::Single { ref index } => {
                        let ret = self.get_new_reg();
                        insts.append(&mut vec![
                            lir::Inst::Dead(reg),
                            lir::Inst::CopyImm(ret, *index as lir::Imm, 1 as lir::Sz),
                        ]);
                        Ok(InstSeq(insts, ret, self.tcx.types.u8))
                    }
                    _ => {
                        println!("rvalue: {:?}", rvalue);
                        println!("Layout = {:?}", layout);
                        Err(Error::NYI)
                    }
                }
            }
            _ => {
                println!("rvalue: {:?}", rvalue);
                Err(Error::NYI)
            }
        }
    }

    fn operand(&mut self, operand: &mir::Operand<'tcx>) -> Result<InstSeq<'tcx>, Error> {
        match *operand {
            mir::Operand::Copy(ref place) => {
                Ok(self.lvalue(place, PlaceCtx::Value).or_else(
                    report_nyi!("mir::Operand::Copy({:?})", place))?)
            }
            mir::Operand::Move(ref place) => {
                Ok(self.lvalue(place, PlaceCtx::Value).or_else(
                    report_nyi!("mir::Operand::Move({:?})", place))?)
            }
            mir::Operand::Constant(ref constant) => {
                let res = match constant.literal.clone() {
                    mir::Literal::Value { ref value } if self.constval_use_static(&value.val) => {

                        // Load the static in a register.
                        let reg = self.get_new_reg();
                        let ty = {
                            if constant.ty.is_fn() {
                                // TyFnDecl has a 0-size, we want to save the
                                // address of the function, which has a
                                // pointer-size.
                                self.tcx.mk_fn_ptr(constant.ty.fn_sig(self.tcx))
                            } else {
                                constant.ty
                            }
                        };

                        // Create a fake Rvalue which can be added in the
                        // list of statics.
                        let rv = {
                            if let ty::TyFnDef(_, _) = constant.ty.sty {
                                mir::Rvalue::Cast(
                                    mir::CastKind::ReifyFnPointer,
                                    operand.clone(),
                                    ty
                                )
                            } else {
                                mir::Rvalue::Use(operand.clone())
                            }
                        };

                        let (off, sz) = self.register_static(&rv, ty)?;
                        Ok(InstSeq(vec![
                            lir::Inst::Live(reg),
                            lir::Inst::Static(reg, off, sz),
                        ], reg, ty))
                    },
                    mir::Literal::Value { value } => {
                        Ok(self.constval(value.val, constant.ty).or_else(
                            report_nyi!("mir::Literal::Value: {:?} ", constant.literal))?)
                    },
                    mir::Literal::Promoted { index } => {
                        // Duplicate the operand with the promoted value and
                        // register it to be known at runtime.

                        let ty = constant.ty;
                        let index = self.register_promoted(index);
                        let operand = mir::Operand::Constant(Box::new(mir::Constant {
                            literal: mir::Literal::Promoted { index },
                            ty: ty,
                            span: constant.span,
                        }));
                        let rv = mir::Rvalue::Use(operand);
                        let (off, sz) = self.register_static(&rv, ty)?;
                        let reg = self.get_new_reg();
                        Ok(InstSeq(vec![
                            lir::Inst::Live(reg),
                            lir::Inst::Static(reg, off, sz),
                        ], reg, ty))
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
        let &mir::Terminator{ /* ref source_info,*/ ref kind, .. } = terminator;
        match kind {
            &mir::TerminatorKind::Resume => {
                Ok(TermSeq(vec![], lir::Terminator::Unwind))
            }
            &mir::TerminatorKind::Return => {
                let (insts, value) = {
                    match self.ret_local {
                        Some(local) => {
                            let fake_rv = mir::Place::Local(local);
                            let InstSeq(insts, reg, _ty) =
                                self.lvalue(&fake_rv, PlaceCtx::Value).or_else(
                                    report_nyi!("mir::TerminatorKind::Return"))?;
                            (insts, Some(reg))
                        }
                        None => (vec![], None)
                    }
                };
                Ok(TermSeq(insts, lir::Terminator::Return {
                    value: value
                }))
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
                let InstSeq(idx_insts, idx_reg, _idx_ty) =
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
                    value: idx_reg,
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
                let InstSeq(mut insts, fun_reg, _fun_ty) =
                    self.operand(func).or_else(
                        report_nyi!("mir::TerminatorKind::Call(func: {:?})", func))?;

                // TODO: Follow what is done in librustc_trans/abi.rs to
                // choose wether arguments are being transfered by value or
                // by reference.

                // TODO: collect the ABI with:
                //    let sig = ty.fn_sig(self.tcx)
                //    let sig = bcx.tcx().erase_late_bound_regions_and_normalize(&sig);
                //    let abi = sig.abi;

                let mut args_reg = Vec::with_capacity(args.len() + 1);
                for arg in args {
                    let InstSeq(mut arg_insts, arg_reg, arg_ty) =
                        self.operand(arg).or_else(
                            report_nyi!("mir::TerminatorKind::Call(args: [.. {:?} ..])", arg))?;
                    let (_, size) = self.add_type(arg_ty, 0)?;
                    // 0-size arguments are ignored.
                    if size != 0 {
                        insts.append(&mut arg_insts);
                        args_reg.push(arg_reg);
                    }
                }

                let unwind_target = match cleanup {
                    &Some(ref cleanup_bb) => Some(self.get_block(*cleanup_bb)),
                    &None => None,
                };

                let return_target = match destination {
                    &Some((ref lvalue, ref bb)) => {
                        let resume = self.new_block();
                        let target = self.get_block(*bb);
                        let InstSeq(mut ret_insts, ret_reg, ret_ty) =
                            self.lvalue(lvalue, PlaceCtx::Address).or_else(
                                report_nyi!("Call(destination: {:?}, _)", lvalue))?;
                        let ret_ty = ret_ty.builtin_deref(true).unwrap().ty;

                        // TODO: check that the returned value is the same
                        // size as the lvalue storage space.
                        let (_, size) = self.add_type(ret_ty, 0)?;

                        let is_fat_ptr = match ret_ty.sty {
                            ty::TyRawPtr(ref elem) |
                            ty::TyRef(_, ref elem) => !elem.ty.is_sized(self.tcx, self.param_env, DUMMY_SP),
                            ty::TyAdt(def, _) if def.is_box() => !ret_ty.boxed_ty().is_sized(self.tcx, self.param_env, DUMMY_SP),
                            _ => false,
                        };

                        if size <= 8 || is_fat_ptr {
                            // The returned values is returned by value,
                            // and fat-pointer are returned on 2
                            // registers.
                            let val_reg = self.get_new_reg();
                            let ret_reg_def = (val_reg, size);
                            ret_insts.append(&mut vec![
                                lir::Inst::Store(ret_reg, val_reg, size),
                                lir::Inst::Dead(val_reg),
                                lir::Inst::Dead(ret_reg),
                            ]);

                            // Create a basic block in which the calls returns,
                            // and in which we set the lvalue with the returned
                            // value of the call.
                            let fp_reg = self.fp;
                            self.blocks[resume] = lir::BasicBlockData {
                                input_regs: vec![
                                    (fp_reg, 8 /* size of frame ptr */ ),
                                    ret_reg_def
                                ],
                                output_regs: vec![ fp_reg ],
                                insts: ret_insts,
                                end: lir::Terminator::Goto {
                                    target: target,
                                },
                            };

                            Some(( Some(ret_reg_def), resume ))
                        } else {
                            // The address where the content should be
                            // initialized is given as the first
                            // argument of the function, and should be
                            // initialized by the callee.
                            //
                            // Note that the call will also return the
                            // same value, but we do not yet care about
                            // making it more efficient to read, thus we
                            // simply ignore the returned value for now.
                            insts.append(&mut ret_insts);
                            args_reg.insert(0, ret_reg);

                            Some(( None, target ))
                        }
                    }
                    &None => None,
                };

                Ok(TermSeq(insts, lir::Terminator::Call {
                    function: fun_reg,
                    args: args_reg,
                    return_target,
                    unwind_target,
                }))
            }
            &mir::TerminatorKind::Drop { ref location, target, unwind } => {
                let InstSeq(lv_insts, lv_reg, lv_ty) =
                    self.lvalue(location, PlaceCtx::Address).or_else(
                        report_nyi!("Drop(location: {:?}, _)", location))?;

                // see resolve_drop_in_place
                let def_id = self.tcx.require_lang_item(DropInPlaceFnLangItem);
                let substs = self.tcx.intern_substs(&[ty::subst::Kind::from(lv_ty)]);
                // see def_ty
                let ty = self.tcx.type_of(def_id).subst(self.tcx, substs);
                assert!(ty.is_fn());
                let ty = self.tcx.mk_fn_ptr(ty.fn_sig(self.tcx));

                // Add the drop_in_place function address as a reference.
                let rv = mir::Rvalue::Cast(
                    mir::CastKind::ReifyFnPointer,
                    mir::Operand::function_handle(self.tcx, def_id, substs, Default::default()),
                    ty
                );
                let (off, sz) = self.register_static(&rv, ty)?;

                let fun_reg = self.get_new_reg();
                let mut insts = lv_insts;
                insts.append(&mut vec![
                    lir::Inst::Live(fun_reg),
                    lir::Inst::Static(fun_reg, off, sz),
                ]);

                Ok(TermSeq(insts, lir::Terminator::Call {
                    function: fun_reg,
                    args: vec![lv_reg],
                    return_target: Some((None, self.get_block(target))),
                    unwind_target: unwind.map(|ub| self.get_block(ub)),
                }))
            }
            &mir::TerminatorKind::Assert {
                ref cond, expected, target, ..
            } => {
                let InstSeq(cond_insts, cond_reg, _cond_ty) =
                    self.operand(cond).or_else(
                        report_nyi!("Assert(cond: {:?}, _)", cond))?;

                // TODO: Add support for clean-up blocks, as well as for
                // displaying error messages.
                let assert_block = self.new_block();
                let fp_reg = self.fp;
                self.blocks[assert_block] = lir::BasicBlockData {
                    input_regs: vec![ (fp_reg, 8 /* size of frame ptr */ ) ],
                    output_regs: vec![],
                    insts: vec![],
                    end: lir::Terminator::Unreachable,
                };

                let target_block = self.get_block(target);
                let targets = {
                    if expected {
                        vec![ (0, assert_block), (1, target_block) ]
                    } else {
                        vec![ (0, target_block), (1, assert_block) ]
                    }
                };

                Ok(TermSeq(cond_insts, lir::Terminator::SwitchInt {
                    value: cond_reg,
                    range: (0 as lir::Imm, 1 as lir::Imm),
                    targets: targets,
                    otherwise: None,
                }))
            }
            _ => Err(Error::NYI)
        }
    }
}
