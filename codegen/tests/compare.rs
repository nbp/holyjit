extern crate holyjit_codegen as codegen;
extern crate holyjit_lir as lir;

use codegen::*;
use std::mem;
use std::cmp::Ordering;

use lir::unit::*;
use lir::data_flow::*;
use lir::number::*;
use lir::builder::*;
use lir::types::*;

mod lib;
use lib::*;


#[test]
fn eq_u32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u32, t_u32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Eq(OrderedType::Ordered(NumberType::U32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u32, u32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        for j in u32_values().into_iter() {
            println!("f({}, {}) == {}", i, j, f(i, j));
            assert_eq!(f(i, j), i == j);
        }
    }
}

#[test]
fn ne_u32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u32, t_u32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ne(OrderedType::Ordered(NumberType::U32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u32, u32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        for j in u32_values().into_iter() {
            println!("f({}, {}) != {}", i, j, f(i, j));
            assert_eq!(f(i, j), i != j);
        }
    }
}

#[test]
fn lt_u32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u32, t_u32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Lt(OrderedType::Ordered(NumberType::U32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u32, u32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        for j in u32_values().into_iter() {
            println!("f({}, {}) < {}", i, j, f(i, j));
            assert_eq!(f(i, j), i < j);
        }
    }
}

#[test]
fn le_u32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u32, t_u32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Le(OrderedType::Ordered(NumberType::U32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u32, u32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        for j in u32_values().into_iter() {
            println!("f({}, {}) <= {}", i, j, f(i, j));
            assert_eq!(f(i, j), i <= j);
        }
    }
}

#[test]
fn gt_u32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u32, t_u32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Gt(OrderedType::Ordered(NumberType::U32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u32, u32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        for j in u32_values().into_iter() {
            println!("f({}, {}) > {}", i, j, f(i, j));
            assert_eq!(f(i, j), i > j);
        }
    }
}

#[test]
fn ge_u32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u32, t_u32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ge(OrderedType::Ordered(NumberType::U32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u32, u32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u32_values().into_iter() {
        for j in u32_values().into_iter() {
            println!("f({}, {}) >= {}", i, j, f(i, j));
            assert_eq!(f(i, j), i >= j);
        }
    }
}

#[test]
fn eq_i32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i32, t_i32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Eq(OrderedType::Ordered(NumberType::I32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i32, i32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i32_values().into_iter() {
        for j in i32_values().into_iter() {
            println!("f({}, {}) == {}", i, j, f(i, j));
            assert_eq!(f(i, j), i == j);
        }
    }
}

#[test]
fn ne_i32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i32, t_i32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ne(OrderedType::Ordered(NumberType::I32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i32, i32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i32_values().into_iter() {
        for j in i32_values().into_iter() {
            println!("f({}, {}) != {}", i, j, f(i, j));
            assert_eq!(f(i, j), i != j);
        }
    }
}

#[test]
fn lt_i32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i32, t_i32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Lt(OrderedType::Ordered(NumberType::I32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i32, i32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i32_values().into_iter() {
        for j in i32_values().into_iter() {
            println!("f({}, {}) < {}", i, j, f(i, j));
            assert_eq!(f(i, j), i < j);
        }
    }
}

#[test]
fn le_i32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i32, t_i32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Le(OrderedType::Ordered(NumberType::I32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i32, i32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i32_values().into_iter() {
        for j in i32_values().into_iter() {
            println!("f({}, {}) <= {}", i, j, f(i, j));
            assert_eq!(f(i, j), i <= j);
        }
    }
}

#[test]
fn gt_i32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i32, t_i32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Gt(OrderedType::Ordered(NumberType::I32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i32, i32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i32_values().into_iter() {
        for j in i32_values().into_iter() {
            println!("f({}, {}) > {}", i, j, f(i, j));
            assert_eq!(f(i, j), i > j);
        }
    }
}

#[test]
fn ge_i32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i32, t_i32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ge(OrderedType::Ordered(NumberType::I32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i32, i32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i32_values().into_iter() {
        for j in i32_values().into_iter() {
            println!("f({}, {}) >= {}", i, j, f(i, j));
            assert_eq!(f(i, j), i >= j);
        }
    }
}

#[test]
fn eq_u64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u64, t_u64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Eq(OrderedType::Ordered(NumberType::U64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u64, u64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u64_values().into_iter() {
        for j in u64_values().into_iter() {
            println!("f({}, {}) == {}", i, j, f(i, j));
            assert_eq!(f(i, j), i == j);
        }
    }
}

#[test]
fn ne_u64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u64, t_u64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ne(OrderedType::Ordered(NumberType::U64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u64, u64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u64_values().into_iter() {
        for j in u64_values().into_iter() {
            println!("f({}, {}) != {}", i, j, f(i, j));
            assert_eq!(f(i, j), i != j);
        }
    }
}

#[test]
fn lt_u64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u64, t_u64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Lt(OrderedType::Ordered(NumberType::U64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u64, u64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u64_values().into_iter() {
        for j in u64_values().into_iter() {
            println!("f({}, {}) < {}", i, j, f(i, j));
            assert_eq!(f(i, j), i < j);
        }
    }
}

#[test]
fn le_u64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u64, t_u64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Le(OrderedType::Ordered(NumberType::U64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u64, u64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u64_values().into_iter() {
        for j in u64_values().into_iter() {
            println!("f({}, {}) <= {}", i, j, f(i, j));
            assert_eq!(f(i, j), i <= j);
        }
    }
}

#[test]
fn gt_u64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u64, t_u64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Gt(OrderedType::Ordered(NumberType::U64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u64, u64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u64_values().into_iter() {
        for j in u64_values().into_iter() {
            println!("f({}, {}) > {}", i, j, f(i, j));
            assert_eq!(f(i, j), i > j);
        }
    }
}

#[test]
fn ge_u64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_u64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::U64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_u64, t_u64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ge(OrderedType::Ordered(NumberType::U64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(u64, u64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in u64_values().into_iter() {
        for j in u64_values().into_iter() {
            println!("f({}, {}) >= {}", i, j, f(i, j));
            assert_eq!(f(i, j), i >= j);
        }
    }
}

#[test]
fn eq_i64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i64, t_i64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Eq(OrderedType::Ordered(NumberType::I64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i64, i64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i64_values().into_iter() {
        for j in i64_values().into_iter() {
            println!("f({}, {}) == {}", i, j, f(i, j));
            assert_eq!(f(i, j), i == j);
        }
    }
}

#[test]
fn ne_i64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i64, t_i64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ne(OrderedType::Ordered(NumberType::I64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i64, i64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i64_values().into_iter() {
        for j in i64_values().into_iter() {
            println!("f({}, {}) != {}", i, j, f(i, j));
            assert_eq!(f(i, j), i != j);
        }
    }
}

#[test]
fn lt_i64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i64, t_i64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Lt(OrderedType::Ordered(NumberType::I64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i64, i64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i64_values().into_iter() {
        for j in i64_values().into_iter() {
            println!("f({}, {}) < {}", i, j, f(i, j));
            assert_eq!(f(i, j), i < j);
        }
    }
}

#[test]
fn le_i64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i64, t_i64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Le(OrderedType::Ordered(NumberType::I64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i64, i64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i64_values().into_iter() {
        for j in i64_values().into_iter() {
            println!("f({}, {}) <= {}", i, j, f(i, j));
            assert_eq!(f(i, j), i <= j);
        }
    }
}

#[test]
fn gt_i64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i64, t_i64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Gt(OrderedType::Ordered(NumberType::I64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i64, i64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i64_values().into_iter() {
        for j in i64_values().into_iter() {
            println!("f({}, {}) > {}", i, j, f(i, j));
            assert_eq!(f(i, j), i > j);
        }
    }
}

#[test]
fn ge_i64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_i64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::I64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_i64, t_i64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ge(OrderedType::Ordered(NumberType::I64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(i64, i64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in i64_values().into_iter() {
        for j in i64_values().into_iter() {
            println!("f({}, {}) >= {}", i, j, f(i, j));
            assert_eq!(f(i, j), i >= j);
        }
    }
}

#[test]
fn ord_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ord(OrderedType::Ordered(NumberType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(_) => true,
                None => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn oeq_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Eq(OrderedType::Ordered(NumberType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Equal) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn one_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ne(OrderedType::Ordered(NumberType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Equal) | None => false,
                _ => true
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn olt_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Lt(OrderedType::Ordered(NumberType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Less) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ole_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Le(OrderedType::Ordered(NumberType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Less) | Some(Ordering::Equal) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ogt_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Gt(OrderedType::Ordered(NumberType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Greater) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn oge_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ge(OrderedType::Ordered(NumberType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Greater) | Some(Ordering::Equal) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn unord_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ord(OrderedType::Unordered(FloatType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(_) => false,
                None => true
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ueq_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Eq(OrderedType::Unordered(FloatType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Equal) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn une_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ne(OrderedType::Unordered(FloatType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Equal) => false,
                _ => true
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ult_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Lt(OrderedType::Unordered(FloatType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Less) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ule_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Le(OrderedType::Unordered(FloatType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Less) | Some(Ordering::Equal) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ugt_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Gt(OrderedType::Unordered(FloatType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Greater) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn uge_f32() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f32 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F32));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f32, t_f32], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ge(OrderedType::Unordered(FloatType::F32)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f32, f32) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f32_values().into_iter() {
        for j in f32_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Greater) | Some(Ordering::Equal) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ord_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ord(OrderedType::Ordered(NumberType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(_) => true,
                None => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn oeq_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Eq(OrderedType::Ordered(NumberType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Equal) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn one_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ne(OrderedType::Ordered(NumberType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Equal) | None => false,
                _ => true
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn olt_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Lt(OrderedType::Ordered(NumberType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Less) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ole_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Le(OrderedType::Ordered(NumberType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Less) | Some(Ordering::Equal) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ogt_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Gt(OrderedType::Ordered(NumberType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Greater) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn oge_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ge(OrderedType::Ordered(NumberType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Greater) | Some(Ordering::Equal) => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn unord_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ord(OrderedType::Unordered(FloatType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(_) => false,
                None => true
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ueq_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Eq(OrderedType::Unordered(FloatType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Equal) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn une_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ne(OrderedType::Unordered(FloatType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Equal) => false,
                _ => true
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ult_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Lt(OrderedType::Unordered(FloatType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Less) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ule_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Le(OrderedType::Unordered(FloatType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Less) | Some(Ordering::Equal) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn ugt_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Gt(OrderedType::Unordered(FloatType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Greater) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}

#[test]
fn uge_f64() {
    let mut ctx_bld = ContextBuilder::new();
    let unit = {
        let mut bld = UnitBuilder::new(UnitId::Function(0), &mut ctx_bld);
        // Add the function signature.
        let t_f64 = bld.ctx().add_type(ComplexType::new_scalar(NumberType::F64));
        let t_bool = bld.ctx().add_type(ComplexType::new_scalar(NumberType::B1));
        let t_sig = bld.ctx().add_type(ComplexType::new_fn(vec![t_f64, t_f64], vec![t_bool], CanUnwind(true)));
        bld.set_signature(t_sig);
        let s0 = bld.create_sequence();
        {
            bld.set_entry(s0);
            bld.switch_to_sequence(s0);
            let a0 = bld.unit_arg(0);
            let a1 = bld.unit_arg(1);
            let v1 = bld.add_op(Opcode::Ge(OrderedType::Unordered(FloatType::F64)), &[a0, a1]);
            bld.end_op(Opcode::Return, &[v1]);
        }
        bld.finish()
    };
    let ctx = ctx_bld.finish();
    let mut cg = CodeGenerator::new();
    let code = cg.compile(&ctx, &unit).unwrap();
    let f : fn(f64, f64) -> bool = unsafe {
        mem::transmute(code.as_ptr())
    };
    for i in f64_values().into_iter() {
        for j in f64_values().into_iter() {
            let expect = match i.partial_cmp(&j) {
                Some(Ordering::Greater) | Some(Ordering::Equal) | None => true,
                _ => false
            };
            assert_eq!(f(i, j), expect);
        }
    }
}
