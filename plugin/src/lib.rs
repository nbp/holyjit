#![crate_type="dylib"]
#![feature(plugin_registrar, rustc_private)]

// extern crate syntax;
extern crate rustc;
extern crate rustc_const_math;
extern crate rustc_plugin;

use rustc::mir::transform::{MirPass, MirSource};
use rustc::mir::{Mir, self};
// use rustc::mir::visit::MutVisitor;
use rustc::ty::{self, TyCtxt};
use rustc::middle::const_val::ConstVal;
use rustc_const_math::ConstInt;
use rustc::hir::def_id::DefId;

use rustc_plugin::Registry;
use std::rc::Rc;

extern crate holyjit_lib;
mod trans;

// This plugin works in 3 steps:
//  - Collect classes which are implementing a specific holyjit library trait, added by one of the
//    Macro of the holyjit library.  From these pieces of code, find the location of the function
//    which have to be implemented.
//  - For all the previous functions, found in the previous phase, copy the MIR graph content, and
//    convert into a format readable by the holyjit library.
//  - For each converted MIR graph generated in the previous phase, add the cloned MIR graph in the
//    trait implementation which implement the given function.

#[derive(Debug)]
enum Error {
    /// The HolyJitFnWrapper constant defined by the jit! macro should use a
    /// CurryN structure to wrap a function pointer. This error is returned
    /// if we cannot find any local with this function pointer.
    NoLocalWithFnType,
    /// We found a local with the fn type, but we cannot find any assignment
    /// to it.
    NoLocalAssignment,
    /// We found the assignment to the local but not the expected cast
    /// operator.
    NoFnCast,
    /// We found a cast operator, but it does not contain a constant.
    NoCastConstOperand,
    /// We found a cast operator, but it does not contain a constant.
    NoLiteralOperand,
    /// We found a cast operator, but it does not contain a constant.
    NoFnConstLiteral,
    /// We found a cast operator, but it does not contain a constant.
    FunctionIsNotAvailable,

    /// We fail while converting the Mir into the Lir.
    UnableToConvert,

    /// While replacing the array types, we found an array with a bad type,
    /// which does not match what is introduced by the jit! macro.
    UnexpectedArrayType,
    /// While replacing the array types, we found an array with a bad
    /// length, which does not match what is introduced by the jit! macro.
    UnexpectedArrayLen,
    /// While replacing the array types, we expected to find a single local
    /// with the array type, instead of a reference to it.
    MultipleArrayDefinitions,
    /// While replacing the array content, we did not found any local with
    /// the matching type.
    NoArrayDefinition,
}

impl From<trans::Error> for Error {
    fn from(err: trans::Error) -> Error {
        println!("error: {:?}", err);
        // All errors are converted into this one.
        Error::UnableToConvert
    }
}

struct AttachJitGraph<'a, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    source: MirSource,
}

impl<'a, 'tcx> AttachJitGraph<'a, 'tcx> {
    /// Check if the Mir graph is a place where we expect to be storing a
    /// copy of the Mir graph to be used for the Jit.  At the moment,
    /// placeholder are defined as constant instances of the
    /// HolyJitFnWrapper.
    fn is_placeholder(&self, mir: &mut Mir<'tcx>) -> bool {
        // Filter out any mir which does not define a constant.
        match self.source {
            MirSource::Const(_) => (),
            _ => return false
        };

        // Check that the constant type is defined as being an
        // HolyJitFnWrapper, which is only supposed to be created by the
        // jit! macro.
        //
        // mir.return_ty == "holyjit::HolyJitFnWrapper<...>"
        match mir.return_ty.ty_adt_def() {
            Some(adt) => {
                let wrpr = self.tcx.def_key(adt.did).disambiguated_data.data;
                let wrpr = wrpr.get_opt_name().unwrap().as_str();
                // println!("CollectFunctionsIndex: {}", wrpr);
                wrpr == "HolyJitFnWrapper"
            },
            None => false
        }
    }

    /// Returns the definition identifier of the function which graph should
    /// be saved for Jit compilations.  The placeholder should be wrap a
    /// reference to this function.
    ///
    /// If for some reasons we cannot find the definition identifer, then
    /// None is returned.
    fn wrapped_fn(&self, mir: &mut Mir<'tcx>) -> Result<(DefId, mir::SourceInfo), Error> {
        // Locate which local variable which has a "fn() -> " type.
        let mut fn_local : Option<mir::Local> = None;
        for temp in mir.temps_iter() {
            if mir.local_decls[temp].ty.is_fn() {
                fn_local = Some(temp);
                break;
            }
        }
        let fn_local = match fn_local {
            Some(l) => l,
            None => return Err(Error::NoLocalWithFnType)
        };
        // println!("local_decls: {:?};", fn_local);

        // Look for one assignment to the local which has the fn type.
        let mut fn_ref : Option<&mir::Rvalue> = None;
        let mut src_info : Option<mir::SourceInfo> = None;
        'search: for block in mir.basic_blocks().indices() {
            let data = &mir[block];
            for statement in &data.statements {
                match *statement {
                    mir::Statement { kind: mir::StatementKind::Assign(
                        mir::Lvalue::Local(assign_local),
                        ref rvalue
                    ), ref source_info}
                    if assign_local == fn_local => {
                        fn_ref = Some(rvalue);
                        src_info = Some(source_info.clone());
                        break 'search;
                    },
                    _ => {},
                }
            }
        }

        // Deconstruct the rvalue and the expected cast operator
        let fn_ref = match fn_ref {
            Some(&mir::Rvalue::Cast(_, mir::Operand::Constant(ref op), _)) => op,
            Some(&mir::Rvalue::Cast(_, _, _)) => return Err(Error::NoCastConstOperand),
            Some(&_) => return Err(Error::NoFnCast),
            None => return Err(Error::NoLocalAssignment),
        };

        // Deconstruct the operand of the cast operator to find the DefId of
        // the function.
        let fn_id = match fn_ref.literal {
            mir::Literal::Value { value: ConstVal::Function(did, _) } => did,
            mir::Literal::Value { .. } => return Err(Error::NoFnConstLiteral),
            _ => return Err(Error::NoLiteralOperand),
        };

        if !self.tcx.is_mir_available(fn_id) {
            return Err(Error::FunctionIsNotAvailable)
        }

        Ok((fn_id, src_info.unwrap()))
    }

    /// This function convert the Mir of the wrapped function into a vector
    /// of serialized graph, which would be deserialized by the Jit library.
    fn serialize_mir(&self, fn_id: DefId, src_info: mir::SourceInfo) -> Result<Vec<u8>, Error> {

        let fn_mir = ty::queries::optimized_mir::try_get(self.tcx, src_info.span, fn_id);
        let fn_mir = match fn_mir {
            Ok(ref callee_mir) => {
                // We are not supposed to have generics at the moment, thus
                // no need to carry a substitution list and substitute them
                // in the Mir of the function.
                //   callee_mir.subst(self.tcx, callsite.substs)
                callee_mir
            }
            _ => return Err(Error::FunctionIsNotAvailable),
        };

        let trans = trans::Transpiler::new(self.tcx);
        Ok(trans.convert(fn_id, fn_mir)?)
    }

    /// The placeholder should have a constant array of u8, which is
    /// supposed to be binary serialization of the converted Mir graph. This
    /// function replaces the type of the locals to match the type of size
    /// of the vector, and copy the content of the vector on the Mir graph.
    fn attach_on_placeholder(&self, mir: &mut Mir<'tcx>, bytes: Vec<u8>) -> Result<(), Error> {

        // This Loop ierates over all local types, and replace the [u8; 1]
        // and &[u8; 1] by [u8; x] and &[u8; x] where x corresponds to the
        // size of the encoded graph contains in bytes vector.
        //
        // This loop also collects the only local index with the array type
        // [u8; 1], in order to replace the its content with the content of
        // the bytes vector.
        let mut arr_local : Option<mir::Local> = None;
        let locals : Vec<_> = mir.temps_iter().collect();
        for idx in locals {
            let arr_ty = match mir.local_decls[idx].ty.sty {
                ty::TypeVariants::TyRef(ref region, ref tam) => {
                    match tam.ty.sty {
                        ty::TypeVariants::TyArray(t, l) => {
                            if t != self.tcx.types.u8 {
                                return Err(Error::UnexpectedArrayType)
                            }
                            if l != 1 {
                                return Err(Error::UnexpectedArrayLen)
                            }
                            let arr_ty = ty::TypeVariants::TyArray(t, bytes.len());
                            let ref_ty = ty::TypeVariants::TyRef(*region, ty::TypeAndMut{
                                ty: self.tcx.mk_ty(arr_ty), ..*tam
                            });
                            Some(ref_ty)
                        }
                        _ => None,
                    }
                },
                ty::TypeVariants::TyArray(t, l) => {
                    if t != self.tcx.types.u8 {
                        return Err(Error::UnexpectedArrayType)
                    }
                    if l != 1 {
                        return Err(Error::UnexpectedArrayLen)
                    }
                    if arr_local != None {
                        return Err(Error::MultipleArrayDefinitions)
                    }
                    arr_local = Some(idx);
                    let arr_ty = ty::TypeVariants::TyArray(t, bytes.len());
                    Some(arr_ty)
                },
                _ => None,
            };

            if let Some(arr_ty) = arr_ty {
                mir.local_decls[idx].ty = self.tcx.mk_ty(arr_ty);
            }
        }

        let arr_local = match arr_local {
            Some(l) => l,
            None => return Err(Error::NoArrayDefinition)
        };

        // Create operands for each element of the vector.
        let mk_literal = |i| mir::Literal::Value{ value: ConstVal::Integral(ConstInt::U8(i)) };
        let mk_operand = |cst: &mir::Constant<'tcx>, i| mir::Operand::Constant(Box::new(
            mir::Constant{ literal: mk_literal(i), ..cst.clone() }));

        // Find the statement which contains the assignment to arr_local,
        // and change its rvalue to be a vector which contains the content
        // of the bytes vector.
        'search: for block in mir.basic_blocks().indices() {
            let data = &mut mir[block];
            for statement in &mut data.statements {
                let cst = {
                    let op = match *statement {
                        mir::Statement { kind: mir::StatementKind::Assign(
                            mir::Lvalue::Local(assign_local),
                            mir::Rvalue::Aggregate(_, ref op)
                        ), ..}
                        if assign_local == arr_local && op.len() == 1 => {
                            &op[0]
                        },
                        _ => continue,
                    };

                    match *op {
                        mir::Operand::Constant(ref c) => c.clone(),
                        _ => continue,
                    }
                };

                statement.kind = mir::StatementKind::Assign(
                    mir::Lvalue::Local(arr_local),
                    mir::Rvalue::Aggregate(Box::new(mir::AggregateKind::Array(self.tcx.types.u8)),
                                           bytes.into_iter().map(|i| mk_operand(&cst, i)).collect())
                );
                break 'search;
            }
        }

        Ok(())
    }

    ///
    fn run_pass(&self, mir: &mut Mir<'tcx>) {
        // Step 1: Filter all references to holyjit data structures.
        if !self.is_placeholder(mir) {
            return
        }

        // Step 2: Find the function reference wrapped in the holyjit data
        // structure.
        let (fn_id, src_info) = self.wrapped_fn(mir).unwrap_or_else(|e|
            panic!("Fail to find wrapped function: {:?}", e),
        );

        // Print the NodeId corresponding the DefId of the function.
        //let fn_node = self.tcx.hir.as_local_node_id(fn_id).unwrap();
        //println!("wrapped function NodeId: {:?};", fn_node);

        // Step 3: Convert the Mir of function into HolyJit representation
        // and serialize it.
        let bytes = self.serialize_mir(fn_id, src_info).unwrap_or_else(|e|
            panic!("Fail to convert the wrapped function: {:?}", e),
        );

        // Step 4: Replace the constant array of the HolyJit data structure
        // by one which contains the serialized graph.
        self.attach_on_placeholder(mir, bytes).unwrap_or_else(|e|
            panic!("Fail to attach the serialized function: {:?}", e),
        );
    }
}

/// This structure is registered as a MirPass and is used to deletage to
/// AttachJitGraph implementation.
struct AttachFnGraphOnCst;
impl MirPass for AttachFnGraphOnCst {
    fn run_pass<'a, 'tcx>(&self, tcx: TyCtxt<'a, 'tcx, 'tcx>,
                          src: MirSource, mir: &mut Mir<'tcx>)
    {
        let attach_jit = AttachJitGraph {
            tcx: tcx,
            source: src,
        };

        attach_jit.run_pass(mir);
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_opt_mir_pass(Rc::new(AttachFnGraphOnCst));
}
