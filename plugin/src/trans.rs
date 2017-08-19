/// This module converts the rustc::mir into a holyjit::lir.

use rustc::ty::{self, TyCtxt};
use rustc::mir;
use rustc::hir::def_id::DefId;

use holyjit_lib::lir;

use std::collections::HashMap;

/// Contains all the logic for converting a Rust Mir graph into an HolyJit
/// Lir graph.
pub struct Transpiler<'a, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    lir: Option<lir::CompilationUnit>,
    // Map a local id, to its offsets from the beginning of the stack. If
    // the stack grows down, this value should be removed from the stack
    // pointer to obtain the desired location.
    locals_map: HashMap<mir::Local, Local<'tcx>>,
}

struct Local<'tcx> {
    idx: mir::Local,
    layout: &'tcx ty::layout::Layout,
    off: usize,
}

#[derive(Debug)]
enum LvalueContext {
    Assign, Projection, Ref, Len, Consume,
}

// TODO: Remove Error suffix.
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
    pub fn new(tcx: TyCtxt<'a, 'tcx, 'tcx>) -> Self {
        Self {
            tcx: tcx,
            lir: None,
            locals_map: HashMap::new()
        }
    }
}

impl<'a, 'tcx> Transpiler<'a, 'tcx> {
    pub fn convert(mut self, fn_id: DefId, mir: &mir::Mir<'tcx>) -> Result<Vec<u8>, Error> {
        let param_env = self.tcx.param_env(fn_id);
        self.locals(mir, &param_env)?;
        self.graph(mir)?;

        Ok(vec![0,1,2,3,4,5,6,7,8,9])
    }

    fn mir(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn locals(&mut self, mir: &mir::Mir<'tcx>, param_env: &ty::ParamEnv<'tcx>) -> Result<(), Error> {
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
            let layout = decl.ty.layout(self.tcx, param_env.clone())?;
            let size = layout.size(&self.tcx.data_layout).bytes() as usize;
            let align = layout.align(&self.tcx.data_layout);
            println!("local {:?} : {} =>: size: {} ; align: {:?}", local, mir.local_decls[local].ty, size, align);

            // Record the aligned address of beginning of the local's
            // memory.  (assumes the stack is growing down)
            let data = {
                // If the
                match (size, mir.local_kind(local)) {
                    (0, _) => Local {
                        idx: local,
                        layout: layout,
                        off: 0,
                    },
                    (_, mir::LocalKind::ReturnPointer) |
                    (_, mir::LocalKind::Arg) => Local {
                        idx: local,
                        layout: layout,
                        off: 0, // FIXME
                    },
                    (_, mir::LocalKind::Var) |
                    (_, mir::LocalKind::Temp) => {
                        // allocate the enough space on the stack
                        stack = stack + size;

                        // pad the stack to have the necessary alignment.
                        let align = align.pref() as usize;
                        assert_eq!(align & (align - 1), 0); // power of 2.
                        stack = (stack | (align - 1)) + 1;

                        Local {
                            idx: local,
                            layout: layout,
                            off: stack,
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

    fn graph(&mut self, mir: &mir::Mir<'tcx>) -> Result<(), Error> {
        for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
            for statement in &bb_data.statements {
                self.statement(statement)?;
            }
        }
        Ok(())
    }

    fn statement(&mut self, statement: &mir::Statement<'tcx>) -> Result<(), Error> {
        match statement.kind {
            mir::StatementKind::Assign(ref lvalue, ref rvalue) => {
                self.lvalue(lvalue, LvalueContext::Assign).or_else(
                    report_nyi!("StatementKind::Assign({:?}, _)", lvalue))?;
                self.rvalue(rvalue).or_else(
                    report_nyi!("StatementKind::Assign(_, {:?})", rvalue))?;
                Ok(()) // TODO
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
            mir::StatementKind::Nop => Ok(()),
        }
    }

    fn lvalue(&mut self, lvalue: &mir::Lvalue<'tcx>, ctx: LvalueContext) -> Result<(), Error> {
        match *lvalue {
            mir::Lvalue::Local(index) => {
                Ok(()) // TODO
            }
            mir::Lvalue::Static(ref def) => {
                // Static are references to a static variable or function,
                // but the address of it is not knwon yet, as it depends on
                // the assembly of the statically compiled program.
                //
                // Thus we have to fake a content of the same type, and in
                // the place-holder structure added by the jit! macro, add a
                // new reference to static variables.
                println!("Lvalue::Static(def_id: {:?}, ty: {:?}) within {:?}", def.def_id,  def.ty, ctx);

                Ok(()) // TODO
            }
            mir::Lvalue::Projection(ref proj) => {
                self.lvalue_projection(&proj).or_else(
                    report_nyi!("mir::Lvalue::Projection(({:?})", proj))
            }
        }
    }

    fn lvalue_projection(&mut self, proj: &mir::LvalueProjection<'tcx>) -> Result<(), Error> {
        // Projection<'tcx, Lvalue<'tcx>, Operand<'tcx>, Ty<'tcx>>
        let mir::Projection { ref base, ref elem } = *proj;
        self.lvalue(base, LvalueContext::Projection)?;
        match *elem {
            mir::ProjectionElem::Deref => {
                Ok(()) // TODO
            }
            mir::ProjectionElem::Field(_field, _ty) => {
                // Given a type, generate a field access for this type.
                Ok(()) // TODO
            }
            mir::ProjectionElem::Index(ref operand) => {
                // Generate an array access (given no type?)
                // ??? [ operand ]
                self.operand(operand).or_else(
                    report_nyi!("mir::ProjectionElem::Index({:?})", operand))?;
                Ok(()) // TODO
            }
            _ => Err(Error::NYI)
        }
    }

    fn rvalue(&mut self, rvalue: &mir::Rvalue<'tcx>) -> Result<(), Error> {
        match *rvalue {
            mir::Rvalue::Use(ref operand) => {
                self.operand(operand).or_else(
                    report_nyi!("mir::Rvalue::Use({:?})", operand))?;
                Ok(()) // TODO
            }
            mir::Rvalue::Ref(_, _, ref lvalue) => {
                self.lvalue(lvalue, LvalueContext::Ref).or_else(
                    report_nyi!("mir::Rvalue::Ref(_, _, {:?})", lvalue))?;
                Ok(()) // TODO
            }
            mir::Rvalue::Len(ref lvalue) => {
                self.lvalue(lvalue, LvalueContext::Len).or_else(
                    report_nyi!("mir::Rvalue::Len({:?})", lvalue))?;
                Ok(()) // TODO
            }
            mir::Rvalue::Cast(_, ref operand, _) => {
                self.operand(operand).or_else(
                    report_nyi!("mir::Rvalue::Cast(_, {:?}, _)", operand))?;
                Ok(()) // TODO
            }
            mir::Rvalue::CheckedBinaryOp(ref op, ref lhs, ref rhs) => {
                self.operand(lhs).or_else(
                    report_nyi!("mir::Rvalue::CheckedBinaryOp({:?}, {:?}, _)", op, lhs))?;
                self.operand(rhs).or_else(
                    report_nyi!("mir::Rvalue::CheckedBinaryOp({:?}, _, {:?})", op, rhs))?;
                Ok(()) // TODO
            }
            mir::Rvalue::BinaryOp(ref op, ref lhs, ref rhs) => {
                self.operand(lhs).or_else(
                    report_nyi!("mir::Rvalue::BinaryOp({:?}, {:?}, _)", op, lhs))?;
                self.operand(rhs).or_else(
                    report_nyi!("mir::Rvalue::BinaryOp({:?}, _, {:?})", op, rhs))?;
                Ok(()) // TODO
            }
            // mir::Rvalue::NullaryOp(_, _) => {
            //     Ok(()) // TODO
            // }
            mir::Rvalue::Aggregate(_, ref operands) => {
                for op in operands {
                    self.operand(op).or_else(
                        report_nyi!("mir::Rvalue::Aggregate(_, [.. {:?} ..])", op))?;
                }
                Ok(()) // TODO
            }
            _ => Err(Error::NYI)
        }
    }

    fn operand(&mut self, operand: &mir::Operand<'tcx>) -> Result<(), Error> {
        match *operand {
            mir::Operand::Consume(ref lvalue) => {
                self.lvalue(lvalue, LvalueContext::Consume).or_else(
                    report_nyi!("mir::Operand::Consume(({:?})", lvalue))?;
                Ok(()) // TODO
            }
            mir::Operand::Constant(_) => {
                Ok(()) // TODO
            }
        }
    }
}
