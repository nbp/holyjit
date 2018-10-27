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
fn load_u8() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u8 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U8));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr], vec![t_u8], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let v0 = bld.add_op(Opcode::Load(t_u8), &[a0]);
            bld.end_op(Opcode::Return, &[v0]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&u8) -> u8 = unsafe {
        mem::transmute(code.as_ptr())
    };
    assert_eq!(f(&u8::min_value()), u8::min_value());
    assert_eq!(f(&u8::max_value()), u8::max_value());
}

#[test]
fn load_u16() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u16 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U16));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr], vec![t_u16], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let v0 = bld.add_op(Opcode::Load(t_u16), &[a0]);
            bld.end_op(Opcode::Return, &[v0]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&u16) -> u16 = unsafe {
        mem::transmute(code.as_ptr())
    };
    assert_eq!(f(&u16::min_value()), u16::min_value());
    assert_eq!(f(&u16::max_value()), u16::max_value());
}

#[test]
fn load_u32() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U32));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr], vec![t_u32], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let v0 = bld.add_op(Opcode::Load(t_u32), &[a0]);
            bld.end_op(Opcode::Return, &[v0]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&u32) -> u32 = unsafe {
        mem::transmute(code.as_ptr())
    };
    assert_eq!(f(&u32::min_value()), u32::min_value());
    assert_eq!(f(&u32::max_value()), u32::max_value());
}

#[test]
fn load_u64() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U64));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr], vec![t_u64], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let v0 = bld.add_op(Opcode::Load(t_u64), &[a0]);
            bld.end_op(Opcode::Return, &[v0]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&u64) -> u64 = unsafe {
        mem::transmute(code.as_ptr())
    };
    assert_eq!(f(&u64::min_value()), u64::min_value());
    assert_eq!(f(&u64::max_value()), u64::max_value());
}

#[test]
fn load_i8() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i8 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I8));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr], vec![t_i8], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let v0 = bld.add_op(Opcode::Load(t_i8), &[a0]);
            bld.end_op(Opcode::Return, &[v0]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&i8) -> i8 = unsafe {
        mem::transmute(code.as_ptr())
    };
    assert_eq!(f(&i8::min_value()), i8::min_value());
    assert_eq!(f(&i8::max_value()), i8::max_value());
}

#[test]
fn load_i16() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i16 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I16));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr], vec![t_i16], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let v0 = bld.add_op(Opcode::Load(t_i16), &[a0]);
            bld.end_op(Opcode::Return, &[v0]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&i16) -> i16 = unsafe {
        mem::transmute(code.as_ptr())
    };
    assert_eq!(f(&i16::min_value()), i16::min_value());
    assert_eq!(f(&i16::max_value()), i16::max_value());
}

#[test]
fn load_i32() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I32));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr], vec![t_i32], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let v0 = bld.add_op(Opcode::Load(t_i32), &[a0]);
            bld.end_op(Opcode::Return, &[v0]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&i32) -> i32 = unsafe {
        mem::transmute(code.as_ptr())
    };
    assert_eq!(f(&i32::min_value()), i32::min_value());
    assert_eq!(f(&i32::max_value()), i32::max_value());
}

#[test]
fn load_i64() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I64));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr], vec![t_i64], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let v0 = bld.add_op(Opcode::Load(t_i64), &[a0]);
            bld.end_op(Opcode::Return, &[v0]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&i64) -> i64 = unsafe {
        mem::transmute(code.as_ptr())
    };
    assert_eq!(f(&i64::min_value()), i64::min_value());
    assert_eq!(f(&i64::max_value()), i64::max_value());
}

#[test]
fn store_u8() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u8 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U8));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr, t_u8], vec![], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            bld.add_op(Opcode::Store(t_u8), &[a0, a1]);
            bld.end_op(Opcode::Return, &[]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&mut u8, u8) = unsafe {
        mem::transmute(code.as_ptr())
    };
    let mut val : u8 = 0;
    f(&mut val, u8::min_value());
    assert_eq!(val, u8::min_value());
    f(&mut val, u8::max_value());
    assert_eq!(val, u8::max_value());
}

#[test]
fn store_u16() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u16 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U16));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr, t_u16], vec![], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            bld.add_op(Opcode::Store(t_u16), &[a0, a1]);
            bld.end_op(Opcode::Return, &[]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&mut u16, u16) = unsafe {
        mem::transmute(code.as_ptr())
    };
    let mut val : u16 = 0;
    f(&mut val, u16::min_value());
    assert_eq!(val, u16::min_value());
    f(&mut val, u16::max_value());
    assert_eq!(val, u16::max_value());
}

#[test]
fn store_u32() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U32));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr, t_u32], vec![], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            bld.add_op(Opcode::Store(t_u32), &[a0, a1]);
            bld.end_op(Opcode::Return, &[]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&mut u32, u32) = unsafe {
        mem::transmute(code.as_ptr())
    };
    let mut val : u32 = 0;
    f(&mut val, u32::min_value());
    assert_eq!(val, u32::min_value());
    f(&mut val, u32::max_value());
    assert_eq!(val, u32::max_value());
}

#[test]
fn store_u64() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U64));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr, t_u64], vec![], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            bld.add_op(Opcode::Store(t_u64), &[a0, a1]);
            bld.end_op(Opcode::Return, &[]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&mut u64, u64) = unsafe {
        mem::transmute(code.as_ptr())
    };
    let mut val : u64 = 0;
    f(&mut val, u64::min_value());
    assert_eq!(val, u64::min_value());
    f(&mut val, u64::max_value());
    assert_eq!(val, u64::max_value());
}

#[test]
fn store_i8() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i8 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I8));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr, t_i8], vec![], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            bld.add_op(Opcode::Store(t_i8), &[a0, a1]);
            bld.end_op(Opcode::Return, &[]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&mut i8, i8) = unsafe {
        mem::transmute(code.as_ptr())
    };
    let mut val : i8 = 0;
    f(&mut val, i8::min_value());
    assert_eq!(val, i8::min_value());
    f(&mut val, i8::max_value());
    assert_eq!(val, i8::max_value());
}

#[test]
fn store_i16() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i16 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I16));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr, t_i16], vec![], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            bld.add_op(Opcode::Store(t_i16), &[a0, a1]);
            bld.end_op(Opcode::Return, &[]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&mut i16, i16) = unsafe {
        mem::transmute(code.as_ptr())
    };
    let mut val : i16 = 0;
    f(&mut val, i16::min_value());
    assert_eq!(val, i16::min_value());
    f(&mut val, i16::max_value());
    assert_eq!(val, i16::max_value());
}

#[test]
fn store_i32() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i32 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I32));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr, t_i32], vec![], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            bld.add_op(Opcode::Store(t_i32), &[a0, a1]);
            bld.end_op(Opcode::Return, &[]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&mut i32, i32) = unsafe {
        mem::transmute(code.as_ptr())
    };
    let mut val : i32 = 0;
    f(&mut val, i32::min_value());
    assert_eq!(val, i32::min_value());
    f(&mut val, i32::max_value());
    assert_eq!(val, i32::max_value());
}

#[test]
fn store_i64() {
    let mut ctx_bld = ContextBuilder::new();
    let add1_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::I64));
        let t_ptr = bld.ctx().add_type(ComplexType::Scalar(addr_type()));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_ptr, t_i64], vec![], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            bld.add_op(Opcode::Store(t_i64), &[a0, a1]);
            bld.end_op(Opcode::Return, &[]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &add1_unit).unwrap();
    let f : fn(&mut i64, i64) = unsafe {
        mem::transmute(code.as_ptr())
    };
    let mut val : i64 = 0;
    f(&mut val, i64::min_value());
    assert_eq!(val, i64::min_value());
    f(&mut val, i64::max_value());
    assert_eq!(val, i64::max_value());
}
