extern crate holyjit_codegen as codegen;
extern crate holyjit_lir as lir;

use codegen::*;
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
        bld.set_entry(s0);
        bld.switch_to_sequence(s0);
        let a0 = bld.unit_arg(0);
        let v0 = bld.add_op(Opcode::Const(NumberValue::I32(2)), &[]);
        let v1 = bld.add_op(Opcode::Rem(SignedType::I32), &[a0, v0]);
        bld.end_op(Opcode::Switch(SwitchData { low: 0,  high: 1 }), &[v1]);
        bld.sequence_value_jump(0, s2);
        bld.sequence_value_jump(1, s1);

        // [sequence 1]
        bld.freeze_sequence_predecessors(s1);
        bld.switch_to_sequence(s1);
        let v2 = bld.add_op(Opcode::Const(NumberValue::I32(1)), &[]);
        let v3 = bld.add_op(Opcode::Add(NumberType::I32), &[a0, v2]);
        bld.end_op(Opcode::Goto, &[]);
        bld.sequence_default_jump(s2);

        // [sequence 2]
        bld.freeze_sequence_predecessors(s2);
        bld.switch_to_sequence(s2);
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
fn sum_loop_test() {
    let mut ctx_bld = ContextBuilder::new();
    let sum_unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::Scalar(NumberType::U64));
        let t_sig = bld.ctx().add_type(ComplexType::Function(vec![t_u64], vec![t_u64], CanUnwind(true)));
        let i = bld.new_var();
        let accu = bld.new_var();

        let s0 = bld.create_sequence();
        let s1 = bld.create_sequence();
        let s2 = bld.create_sequence();

        bld.set_signature(t_sig);
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            bld.set_var(i, a0);
            let v0 = bld.add_op(Opcode::Const(NumberValue::U64(0)), &[]);
            bld.set_var(accu, v0);
            bld.end_op(Opcode::Switch(SwitchData { low: 0,  high: 0 }), &[a0]);
            bld.sequence_value_jump(0, s2);
            bld.sequence_default_jump(s1);
        }
        {
            bld.switch_to_sequence(s1);
            let v0 = bld.add_op(Opcode::Const(NumberValue::U64(1)), &[]);
            let v1 = bld.use_var(i);
            let v2 = bld.add_op(Opcode::Sub(NumberType::U64), &[v1, v0]);
            bld.set_var(i, v2);
            let v3 = bld.use_var(accu);
            let v4 = bld.add_op(Opcode::Add(NumberType::U64), &[v1, v3]);
            bld.set_var(accu, v4);
            bld.end_op(Opcode::Switch(SwitchData { low: 0,  high: 0 }), &[v2]);
            bld.sequence_value_jump(0, s2);
            bld.sequence_default_jump(s1);
            bld.freeze_sequence_predecessors(s1);
        }
        {
            bld.freeze_sequence_predecessors(s2);
            bld.switch_to_sequence(s2);
            bld.dead_var(i);
            let v0 = bld.use_var(accu);
            bld.dead_var(accu);
            bld.end_op(Opcode::Return, &[v0])
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();

    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &sum_unit).unwrap();
    let sum : fn(u64) -> u64 = unsafe {
        mem::transmute(code.as_ptr())
    };
    assert_eq!(sum(0), 0);
    assert_eq!(sum(1), 1);
    assert_eq!(sum(2), 3);
    assert_eq!(sum(3), 6);
    assert_eq!(sum(4), 10);
}
