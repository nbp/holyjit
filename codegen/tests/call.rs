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

fn ret_tuple(x : u8) -> (u8, u8) {
    sub_add(x, 1)
}

fn sub_add(x : u8, y: u8) -> (u8, u8) {
    (x.wrapping_sub(y), x.wrapping_add(y))
}

fn diff_sub_add(x : u8, y : u8) -> u8 {
    let (a, b) = sub_add(x, y);
    b.wrapping_sub(a)
}


#[test]
fn call_ret_tuple() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u8 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U8));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u8], vec![t_u8], CanUnwind(true)));
        // TODO: Unwinding is not supported without DWARF at the moment and
        // eh_frame describing where to resume the execution when unwinding.
        let t_ret_tuple = bld.ctx().add_type(ComplexType::Function(vec![t_u8], vec![t_u8, t_u8], CanUnwind(false)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        let s1 = bld.create_sequence();
        let r0 = {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let v1 = bld.add_op(Opcode::Const((addr_type(), ret_tuple as usize).into()), &[]);
            let r0 = bld.end_op(Opcode::Call(t_ret_tuple), &[v1, a0]);
            bld.sequence_default_jump(s1);
            r0
        };
        {
            bld.freeze_sequence_predecessors(s1);
            bld.switch_to_sequence(s1);
            let v1 = bld.add_op(Opcode::Nth(0), &[r0]);
            let v2 = bld.add_op(Opcode::Nth(1), &[r0]);
            let v3 = bld.add_op(Opcode::Sub(NumberType::U8), &[v2, v1]);
            bld.end_op(Opcode::Return, &[v3]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u8) -> u8 = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        println!("f({}) == {}", i, f(i as u8));
        assert_eq!(f(i as u8), 2);
    }
}

#[test]
fn call_sub_add() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u8 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U8));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u8, t_u8], vec![t_u8], CanUnwind(true)));
        // TODO: Unwinding is not supported without DWARF at the moment and
        // eh_frame describing where to resume the execution when unwinding.
        let t_fun = bld.ctx().add_type(ComplexType::Function(vec![t_u8, t_u8], vec![t_u8, t_u8], CanUnwind(false)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        let s1 = bld.create_sequence();
        let r0 = {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Const((addr_type(), sub_add as usize).into()), &[]);
            let r0 = bld.end_op(Opcode::Call(t_fun), &[v1, a0, a1]);
            bld.sequence_default_jump(s1);
            r0
        };
        {
            bld.freeze_sequence_predecessors(s1);
            bld.switch_to_sequence(s1);
            let v1 = bld.add_op(Opcode::Nth(0), &[r0]);
            let v2 = bld.add_op(Opcode::Nth(1), &[r0]);
            let v3 = bld.add_op(Opcode::Sub(NumberType::U8), &[v2, v1]);
            bld.end_op(Opcode::Return, &[v3]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u8, u8) -> u8 = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        let i = i as u8;
        for j in u32_values().into_iter() {
            let j = j as u8;
            println!("f({}, {}) == {} (expect {})", i, j, f(i, j), j.wrapping_mul(2));
            assert_eq!(f(i, j), j.wrapping_mul(2));
        }
    }
}

#[test]
fn call_diff_sub_add() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u8 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U8));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u8, t_u8], vec![t_u8], CanUnwind(true)));
        // TODO: Unwinding is not supported without DWARF at the moment and
        // eh_frame describing where to resume the execution when unwinding.
        let t_fun = bld.ctx().add_type(ComplexType::Function(vec![t_u8, t_u8], vec![t_u8], CanUnwind(false)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        let s1 = bld.create_sequence();
        let r0 = {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Const((addr_type(), diff_sub_add as usize).into()), &[]);
            let r0 = bld.end_op(Opcode::Call(t_fun), &[v1, a0, a1]);
            bld.sequence_default_jump(s1);
            r0
        };
        {
            bld.freeze_sequence_predecessors(s1);
            bld.switch_to_sequence(s1);
            bld.end_op(Opcode::Return, &[r0]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u8, u8) -> u8 = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        let i = i as u8;
        for j in u32_values().into_iter() {
            let j = j as u8;
            println!("f({}, {}) == {} (expect {})", i, j, f(i, j), j.wrapping_mul(2));
            assert_eq!(f(i, j), j.wrapping_mul(2));
        }
    }
}
