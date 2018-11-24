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
fn mul_u32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U32));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u32, t_u32], vec![t_u32], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Mul(NumberType::U32), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u32, u32) -> u32 = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        for j in u32_values().into_iter() {
            println!("f({}, {}) == {}", i, j, f(i, j));
            assert_eq!(f(i, j), i.wrapping_mul(j));
        }
    }
}

#[test]
fn mul_i32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I32));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_i32, t_i32], vec![t_i32], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Mul(NumberType::I32), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i32, i32) -> i32 = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i32_values().into_iter() {
        for j in i32_values().into_iter() {
            println!("f({}, {}) == {}", i, j, f(i, j));
            assert_eq!(f(i, j), i.wrapping_mul(j));
        }
    }
}

#[test]
fn mul_u64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U64));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u64, t_u64], vec![t_u64], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Mul(NumberType::U64), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u64, u64) -> u64 = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u64_values().into_iter() {
        for j in u64_values().into_iter() {
            println!("f({}, {}) == {}", i, j, f(i, j));
            assert_eq!(f(i, j), i.wrapping_mul(j));
        }
    }
}

#[test]
fn mul_i64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I64));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_i64, t_i64], vec![t_i64], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Mul(NumberType::I64), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i64, i64) -> i64 = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i64_values().into_iter() {
        for j in i64_values().into_iter() {
            println!("f({}, {}) == {}", i, j, f(i, j));
            assert_eq!(f(i, j), i.wrapping_mul(j));
        }
    }
}

#[test]
fn mul_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::F32));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_f32, t_f32], vec![t_f32], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Mul(NumberType::F32), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> f32 = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            println!("f({}, {}) == {}" ,i, j, f(i, j));
            // NOTE: We cannot compare NaN against another NaN with assert_eq!
            // macro, unless we compare the raw bits of the NaN values.
            assert_eq!(f(i, j).to_bits(), (i * j).to_bits());
        }
    }
}

#[test]
fn mul_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::F64));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_f64, t_f64], vec![t_f64], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Mul(NumberType::F64), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> f64 = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            println!("f({}, {}) == {}", i, j, f(i, j));
            // NOTE: We cannot compare NaN against another NaN with assert_eq!
            // macro, unless we compare the raw bits of the NaN values.
            assert_eq!(f(i, j).to_bits(), (i * j).to_bits());
        }
    }
}
