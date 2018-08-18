use frontend::{FunctionBuilderContext, FunctionBuilder, Variable};

use codegen::entity::EntityRef;
use codegen::ir::{ExternalName, Function, Signature, AbiParam, InstBuilder};
use codegen::ir::types::*;
use codegen::ir::types;
use codegen::settings::{self, CallConv};
use codegen::verifier::verify_function;
use codegen::isa::TargetIsa;

use lir::unit::Unit;
use lir::context::Context;
use lir::types::{ComplexTypeId, ComplexType};
use lir::number::{NumberType};
use error::{LowerResult, LowerError};

#[derive(Copy, Clone)]
struct CtxIsa<'a> {
    pub isa: &'a TargetIsa,
    pub ctx: &'a Context,
}

fn abiparam<'a>(ci: CtxIsa<'a>, ty: ComplexTypeId) -> LowerResult<AbiParam> {
    use self::ComplexType::*;
    use self::NumberType::*;
    let ty = ci.ctx.get_type(ty);
    let ty = match ty {
        &Pointer => ci.isa.pointer_type(),
        &Scalar(U8) | &Scalar(I8) => types::I8,
        &Scalar(U16) | &Scalar(I16) => types::I16,
        &Scalar(U32) | &Scalar(I32) => types::I32,
        &Scalar(U64) | &Scalar(I64) => types::I64,
        &Vector(_, _) => unimplemented!(),
        _ => return Err(LowerError::ParameterTypeNotLowered),
    };
    Ok(AbiParam::new(ty))
}

/// Unit have a signature expressed as a type, we have to convert this signature
/// into simpler types understood by Cranelift.
fn signature<'a>(ci: CtxIsa<'a>, unit: &Unit) -> LowerResult<Signature >{
    let ty = ci.ctx.get_type(unit.sig);
    let (ins, outs) = match ty {
        &ComplexType::Function(ref ins, ref outs, ref _unwind) => (ins, outs),
        _ => return Err(LowerError::UnitIsNotAFunction),
    };

    // At the moment, assume that all Units are going to be called with Rust
    // calling convention.
    let mut sig = Signature::new(CallConv::SystemV);

    for &ty in ins.iter() {
        sig.params.push(abiparam(ci, ty)?);
    }
    for &ty in outs.iter() {
        sig.returns.push(abiparam(ci, ty)?);
    }
    Ok(sig)
}

/// Convert a LIR Unit into a Cranelift IR (Function).
pub fn convert(isa: &TargetIsa, ctx: &Context, unit: &Unit) -> LowerResult<Function> {
    let ci = CtxIsa { ctx, isa };
    let sig = signature(ci, unit)?;
    let mut fn_builder_ctx = FunctionBuilderContext::<Variable>::new();
    let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
    {
        let mut builder = FunctionBuilder::<Variable>::new(&mut func, &mut fn_builder_ctx);

        let block0 = builder.create_ebb();
        let x = Variable::new(0);
        let y = Variable::new(1);
        builder.declare_var(x, I32);
        builder.declare_var(y, I32);
        builder.append_ebb_params_for_function_params(block0);

        builder.switch_to_block(block0);
        builder.seal_block(block0);
        {
            let tmp = builder.ebb_params(block0)[0]; // the first function parameter
            builder.def_var(x, tmp);
        }
        {
            let tmp = builder.ins().iconst(I32, 1);
            builder.def_var(y, tmp);
        }
        let z = Variable::new(2);
        builder.declare_var(z, I32);
        {
            let arg1 = builder.use_var(x);
            let arg2 = builder.use_var(y);
            let tmp = builder.ins().iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(z);
            builder.ins().return_(&[arg]);
        }

        builder.finalize();
    }

    let flags = settings::Flags::new(settings::builder());
    let res = verify_function(&func, &flags);
    println!("{}", func.display(None));
    if let Err(errors) = res {
        panic!("{}", errors);
    }

    Ok(func)
    /*
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
    */
}
