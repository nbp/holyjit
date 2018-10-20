extern crate holyjit_codegen as codegen;
extern crate holyjit_lir as lir;

use codegen::*;
use std::mem;

use lir::unit::*;
use lir::data_flow::*;
use lir::number::*;
use lir::builder::*;
use lir::types::*;

mod lib;
use lib::*;

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
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
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
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
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
    for i in i32_values().into_iter() {
        for j in i32_values().into_iter() {
            println!("add_overflow({}, {}) == {}", i, j, add_overflow(i, j));
            assert_eq!(add_overflow(i, j), i.overflowing_add(j).1)
        }
    }
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
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
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
    for i in u32_values().into_iter() {
        for j in u32_values().into_iter() {
            println!("add_overflow({}, {}) == {}", i, j, add_overflow(i, j));
            assert_eq!(add_overflow(i, j), (i as i32).overflowing_add(j as i32).1)
        }
    }
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
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
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
    for i in i64_values().into_iter() {
        for j in i64_values().into_iter() {
            println!("add_overflow({}, {}) == {}", i, j, add_overflow(i, j));
            assert_eq!(add_overflow(i, j), i.overflowing_add(j).1)
        }
    }
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
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
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
    for i in u64_values().into_iter() {
        for j in u64_values().into_iter() {
            println!("add_overflow({}, {}) == {}", i, j, add_overflow(i, j));
            assert_eq!(add_overflow(i, j), (i as i64).overflowing_add(j as i64).1)
        }
    }
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
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
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
    for i in i32_values().into_iter() {
        for j in i32_values().into_iter() {
            println!("add_carry({}, {}) == {}", i, j, add_carry(i, j));
            assert_eq!(add_carry(i, j), (i as u32).overflowing_add(j as u32).1)
        }
    }
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
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
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
    for i in u32_values().into_iter() {
        for j in u32_values().into_iter() {
            println!("add_carry({}, {}) == {}", i, j, add_carry(i, j));
            assert_eq!(add_carry(i, j), i.overflowing_add(j).1)
        }
    }
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
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
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
    for i in i64_values().into_iter() {
        for j in i64_values().into_iter() {
            println!("add_carry({}, {}) == {}", i, j, add_carry(i, j));
            assert_eq!(add_carry(i, j), (i as u64).overflowing_add(j as u64).1)
        }
    }
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
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
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
    for i in u64_values().into_iter() {
        for j in u64_values().into_iter() {
            println!("add_carry({}, {}) == {}", i, j, add_carry(i, j));
            assert_eq!(add_carry(i, j), i.overflowing_add(j).1)
        }
    }
}
