extern crate mmap;
extern crate region;
extern crate cranelift_codegen as codegen;
extern crate cranelift_frontend as frontend;
extern crate cranelift_native as native;
extern crate holyjit_lir as lir;

mod lower;
mod exec_alloc;
pub mod error;

use codegen::settings::Configurable;
use exec_alloc::{WrittableCode, ExecutableCode};

use lir::unit;
use lir::context;

/// This is a reusable code generator, which is used to compile a LIR Unit to
/// machine code.
pub struct CodeGenerator {
    ctx: codegen::Context,
    isa: Box<codegen::isa::TargetIsa>,
}

/// Result of a compiled lir::Unit.
pub struct JitCode {
    code: ExecutableCode,
}

impl CodeGenerator {
    /// Create a lowering (code generator and executable page allocation)
    /// context for the architecture on which this code is running.
    pub fn new() -> Self {
        // Extract configuration builders tuned for the architecture on which
        // this code is running.
        let (mut settings_bld, isa_bld) = native::builders().unwrap();
        // Optimize for compilation time.
        settings_bld.set("opt_level", "fastest").unwrap();
        // Check the emitted Cranelift IR.
        settings_bld.set("enable_verifier", "1").unwrap();
        // Use Rust call convention.
        settings_bld.set("call_conv", "system_v").unwrap();
        // Generate position independent code.
        settings_bld.set("is_pic", "1").unwrap();
        // No need to generate a single return per function.
        settings_bld.set("return_at_end", "0").unwrap();
        // Do not attempt to avoid trap on divisions. (TODO: double check that
        // this is what rust expects)
        settings_bld.set("avoid_div_traps", "0").unwrap();
        let flags = codegen::settings::Flags::new(settings_bld);
        let isa = isa_bld.finish(flags);

        Self {
            ctx: codegen::Context::new(),
            isa: isa,
        }
    }

    /// Given an HolyJIT LIR Unit, convert it to a Cranelift function in order
    /// to generate the corresponding bytes, then allocate memory pages and map
    /// them as executable.
    pub fn compile(&mut self, lir_ctx: &context::Context, unit: &unit::Unit) -> error::LowerResult<JitCode> {
        let &mut CodeGenerator { ref mut ctx, ref isa, .. } = self;
        ctx.func = lower::convert(isa.as_ref(), lir_ctx, unit)?;
        let mut reloc_sink = exec_alloc::NullRelocSink {};
        let mut trap_sink = exec_alloc::NullTrapSink {};
        let code_size = ctx.compile(isa.as_ref())?;
        let mut code = WrittableCode::with_capacity(code_size as usize)?;
        unsafe {
            ctx.emit_to_memory(isa.as_ref(), code.as_ptr(), &mut reloc_sink, &mut trap_sink);
        }
        let code = code.make_executable(reloc_sink, trap_sink)?;
        Ok(JitCode { code })
    }
}

impl JitCode {
    pub fn as_ptr(&self) -> *const u8 {
        self.code.as_ptr()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    use lir::unit::*;
    use lir::data_flow::*;
    use lir::number::*;
    use lir::builder::*;
    use lir::types::*;


    #[test]
    fn create_code_generator() {
        let _cg = CodeGenerator::new();
        assert!(true);
    }

    #[test]
    fn add1_test() {
        let mut ctx_bld = ContextBuilder::new();
        let add1_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_i32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I32));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_i32], vec![t_i32], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence();
            {
                bld.switch_to_sequence(s0);
                bld.set_entry();
                let a0 = bld.unit_arg(0);
                let v0 = bld.add_op(Opcode::Const(NumberValue::I32(1)), &[]);
                let v1 = bld.add_op(Opcode::Add(NumberType::I32), &[a0, v0]);
                bld.end_op(Opcode::Return, &[v1]);
            }
            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &add1_unit).unwrap();
        let add1 : fn(i32) -> i32 = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(add1(-5), -4);
        assert_eq!(add1(12), 13);
    }

    #[test]
    fn round_odd_up_test() {
        let mut ctx_bld = ContextBuilder::new();
        let round_odd_up_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_i32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I32));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_i32], vec![t_i32], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence(); // test x % 2 == 0
            let s1 = bld.create_sequence(); // x += 1
            let s2 = bld.create_sequence(); // return x

            // [sequence 0]
            bld.switch_to_sequence(s0);
            bld.set_entry();
            let a0 = bld.unit_arg(0);
            let v0 = bld.add_op(Opcode::Const(NumberValue::I32(2)), &[]);
            let v1 = bld.add_op(Opcode::Rem(SignedType::I32), &[a0, v0]);
            bld.end_op(Opcode::Switch(SwitchData { low: 0,  high: 1 }), &[v1]);
            bld.sequence_value_jump(0, s2);
            bld.sequence_value_jump(1, s1);

            // [sequence 1]
            bld.switch_to_sequence(s1);
            bld.freeze_sequence_predecessors(s1);
            let v2 = bld.add_op(Opcode::Const(NumberValue::I32(1)), &[]);
            let v3 = bld.add_op(Opcode::Add(NumberType::I32), &[a0, v2]);
            bld.end_op(Opcode::Goto, &[]);
            bld.sequence_default_jump(s2);

            // [sequence 2]
            bld.switch_to_sequence(s2);
            bld.freeze_sequence_predecessors(s2);
            let v4 = bld.add_op(Opcode::Phi, &[a0, v3]);
            bld.end_op(Opcode::Return, &[v4]);

            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &round_odd_up_unit).unwrap();
        let round_odd_up : fn(i32) -> i32 = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(round_odd_up(0), 0);
        assert_eq!(round_odd_up(9654), 9654);
        assert_eq!(round_odd_up(1618033), 1618034);
        assert_eq!(round_odd_up(-5), -4);
    }

    #[test]
    fn add_overflow_i32_test() {
        let mut ctx_bld = ContextBuilder::new();
        let add1_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_i32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I32));
            let t_bool = bld.ctx().add_type(ComplexType::Scalar(NumberType::B1));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_i32, t_i32], vec![t_bool], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence();
            {
                bld.switch_to_sequence(s0);
                bld.set_entry();
                let a0 = bld.unit_arg(0);
                let a1 = bld.unit_arg(1);
                let v1 = bld.add_op(Opcode::Add(NumberType::I32), &[a0, a1]);
                let v2 = bld.add_op_deps(Opcode::OverflowFlag, &[], &[v1]);
                bld.end_op(Opcode::Return, &[v2])
            }
            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &add1_unit).unwrap();
        let add_overflow : fn(i32, i32) -> bool = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(add_overflow(0, 0), false);
        assert_eq!(add_overflow(-1, 1), true);
        assert_eq!(add_overflow(i32::max_value() - 1, 1), false);
        assert_eq!(add_overflow(i32::max_value(), 1), true);
        assert_eq!(add_overflow(i32::min_value(), -1), true);
    }

    #[test]
    fn add_overflow_u32_test() {
        let mut ctx_bld = ContextBuilder::new();
        let add1_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_u32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U32));
            let t_bool = bld.ctx().add_type(ComplexType::Scalar(NumberType::B1));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u32, t_u32], vec![t_bool], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence();
            {
                bld.switch_to_sequence(s0);
                bld.set_entry();
                let a0 = bld.unit_arg(0);
                let a1 = bld.unit_arg(1);
                let v1 = bld.add_op(Opcode::Add(NumberType::U32), &[a0, a1]);
                let v2 = bld.add_op_deps(Opcode::OverflowFlag, &[], &[v1]);
                bld.end_op(Opcode::Return, &[v2])
            }
            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &add1_unit).unwrap();
        let add_overflow : fn(u32, u32) -> bool = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(add_overflow(0, 0), false);
        assert_eq!(add_overflow(u32::max_value() - 1, 1), false);
        assert_eq!(add_overflow(u32::max_value() >> 1, 1), true);
        assert_eq!(add_overflow(u32::max_value(), 1), true);
    }

    #[test]
    fn add_overflow_i64_test() {
        let mut ctx_bld = ContextBuilder::new();
        let add1_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_i64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I64));
            let t_bool = bld.ctx().add_type(ComplexType::Scalar(NumberType::B1));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_i64, t_i64], vec![t_bool], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence();
            {
                bld.switch_to_sequence(s0);
                bld.set_entry();
                let a0 = bld.unit_arg(0);
                let a1 = bld.unit_arg(1);
                let v1 = bld.add_op(Opcode::Add(NumberType::I64), &[a0, a1]);
                let v2 = bld.add_op_deps(Opcode::OverflowFlag, &[], &[v1]);
                bld.end_op(Opcode::Return, &[v2])
            }
            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &add1_unit).unwrap();
        let add_overflow : fn(i64, i64) -> bool = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(add_overflow(0, 0), false);
        assert_eq!(add_overflow(-1, 1), true);
        assert_eq!(add_overflow(i64::max_value() - 1, 1), false);
        assert_eq!(add_overflow(i64::max_value(), 1), true);
        assert_eq!(add_overflow(i64::min_value(), -1), true);
    }

    #[test]
    fn add_overflow_u64_test() {
        let mut ctx_bld = ContextBuilder::new();
        let add1_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_u64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U64));
            let t_bool = bld.ctx().add_type(ComplexType::Scalar(NumberType::B1));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u64, t_u64], vec![t_bool], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence();
            {
                bld.switch_to_sequence(s0);
                bld.set_entry();
                let a0 = bld.unit_arg(0);
                let a1 = bld.unit_arg(1);
                let v1 = bld.add_op(Opcode::Add(NumberType::U64), &[a0, a1]);
                let v2 = bld.add_op_deps(Opcode::OverflowFlag, &[], &[v1]);
                bld.end_op(Opcode::Return, &[v2])
            }
            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &add1_unit).unwrap();
        let add_overflow : fn(u64, u64) -> bool = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(add_overflow(0, 0), false);
        assert_eq!(add_overflow(u64::max_value() - 1, 1), false);
        assert_eq!(add_overflow(u64::max_value() >> 1, 1), true);
        assert_eq!(add_overflow(u64::max_value(), 1), true);
    }

    #[test]
    fn add_carry_i32_test() {
        let mut ctx_bld = ContextBuilder::new();
        let add1_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_i32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I32));
            let t_bool = bld.ctx().add_type(ComplexType::Scalar(NumberType::B1));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_i32, t_i32], vec![t_bool], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence();
            {
                bld.switch_to_sequence(s0);
                bld.set_entry();
                let a0 = bld.unit_arg(0);
                let a1 = bld.unit_arg(1);
                let v1 = bld.add_op(Opcode::Add(NumberType::I32), &[a0, a1]);
                let v2 = bld.add_op_deps(Opcode::CarryFlag, &[], &[v1]);
                bld.end_op(Opcode::Return, &[v2])
            }
            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &add1_unit).unwrap();
        let add_carry : fn(i32, i32) -> bool = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(add_carry(0, 0), false);
        assert_eq!(add_carry(-1, 1), true);
        assert_eq!(add_carry(i32::max_value() - 1, 1), false);
        assert_eq!(add_carry(i32::max_value(), 1), false);
        assert_eq!(add_carry(i32::min_value(), -1), true);
    }

    #[test]
    fn add_carry_u32_test() {
        let mut ctx_bld = ContextBuilder::new();
        let add1_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_u32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U32));
            let t_bool = bld.ctx().add_type(ComplexType::Scalar(NumberType::B1));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u32, t_u32], vec![t_bool], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence();
            {
                bld.switch_to_sequence(s0);
                bld.set_entry();
                let a0 = bld.unit_arg(0);
                let a1 = bld.unit_arg(1);
                let v1 = bld.add_op(Opcode::Add(NumberType::U32), &[a0, a1]);
                let v2 = bld.add_op_deps(Opcode::CarryFlag, &[], &[v1]);
                bld.end_op(Opcode::Return, &[v2])
            }
            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &add1_unit).unwrap();
        let add_carry : fn(u32, u32) -> bool = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(add_carry(0, 0), false);
        assert_eq!(add_carry(u32::max_value() - 1, 1), false);
        assert_eq!(add_carry(u32::max_value() >> 1, 1), false);
        assert_eq!(add_carry(u32::max_value(), 1), true);
    }

    #[test]
    fn add_carry_i64_test() {
        let mut ctx_bld = ContextBuilder::new();
        let add1_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_i64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I64));
            let t_bool = bld.ctx().add_type(ComplexType::Scalar(NumberType::B1));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_i64, t_i64], vec![t_bool], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence();
            {
                bld.switch_to_sequence(s0);
                bld.set_entry();
                let a0 = bld.unit_arg(0);
                let a1 = bld.unit_arg(1);
                let v1 = bld.add_op(Opcode::Add(NumberType::I64), &[a0, a1]);
                let v2 = bld.add_op_deps(Opcode::CarryFlag, &[], &[v1]);
                bld.end_op(Opcode::Return, &[v2])
            }
            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &add1_unit).unwrap();
        let add_carry : fn(i64, i64) -> bool = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(add_carry(0, 0), false);
        assert_eq!(add_carry(-1, 1), true);
        assert_eq!(add_carry(i64::max_value() - 1, 1), false);
        assert_eq!(add_carry(i64::max_value(), 1), false);
        assert_eq!(add_carry(i64::min_value(), -1), true);
    }

    #[test]
    fn add_carry_u64_test() {
        let mut ctx_bld = ContextBuilder::new();
        let add1_unit = {
            let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
            // Add the function signature.
            let t_u64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U64));
            let t_bool = bld.ctx().add_type(ComplexType::Scalar(NumberType::B1));
            let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u64, t_u64], vec![t_bool], CanUnwind(true)));
            bld.set_signature(t_sig);
            let s0 = bld.create_sequence();
            {
                bld.switch_to_sequence(s0);
                bld.set_entry();
                let a0 = bld.unit_arg(0);
                let a1 = bld.unit_arg(1);
                let v1 = bld.add_op(Opcode::Add(NumberType::U64), &[a0, a1]);
                let v2 = bld.add_op_deps(Opcode::CarryFlag, &[], &[v1]);
                bld.end_op(Opcode::Return, &[v2])
            }
            bld.finish()
        };
        let ctx = ctx_bld.finish();

        let mut cg = CodeGenerator::new();
        let code = cg.compile(&ctx, &add1_unit).unwrap();
        let add_carry : fn(u64, u64) -> bool = unsafe {
            mem::transmute(code.as_ptr())
        };
        assert_eq!(add_carry(0, 0), false);
        assert_eq!(add_carry(u64::max_value() - 1, 1), false);
        assert_eq!(add_carry(u64::max_value() >> 1, 1), false);
        assert_eq!(add_carry(u64::max_value(), 1), true);
    }
}
