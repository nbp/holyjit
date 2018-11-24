#![allow(dead_code)]
use std::mem::{size_of, transmute};
use lir::number::*;

/// Iterate over interesting u32 numbers.
pub fn u32_values() -> Vec<u32> {
    let mut res = vec![u32::min_value(), u32::max_value()];
    for i in [0u32, 1, 2, 29, 30, 31].into_iter() {
        res.push(1u32 << i);
        for j in [0u32, 1, 2, 29, 30, 31].into_iter() {
            res.push((1u32 << i).wrapping_sub(1u32 << j));
            res.push((1u32 << i).wrapping_add(1u32 << j));
        }
        res.push((1u32 << i).wrapping_neg());
    }
    res.sort();
    res.dedup();
    res
}

/// Iterate over interesting i32 numbers.
pub fn i32_values() -> Vec<i32> {
    u32_values().into_iter().map(|x| x as i32).collect()
}

/// Iterate over interesting u64 numbers.
pub fn u64_values() -> Vec<u64> {
    let mut res = vec![u64::min_value(), u64::max_value()];
    for i in [0u64, 1, 2, 61, 62, 63].into_iter() {
        res.push(1u64 << i);
        for j in [0u64, 1, 2, 61, 62, 63].into_iter() {
            res.push((1u64 << i).wrapping_sub(1u64 << j));
            res.push((1u64 << i).wrapping_add(1u64 << j));
        }
        res.push((1u64 << i).wrapping_neg());
    }
    res.sort();
    res.dedup();
    res
}

pub fn i64_values() -> Vec<i64> {
    u64_values().into_iter().map(|x| x as i64).collect()
}

pub fn f32_values() -> Vec<f32> {
    u32_values().into_iter().map(|x| unsafe { transmute::<u32, f32>(x) }).collect()
}

pub fn f64_values() -> Vec<f64> {
    u64_values().into_iter().map(|x| unsafe { transmute::<u64, f64>(x) }).collect()
}

pub fn addr_type() -> NumberType {
    match size_of::<usize>() {
        1 => NumberType::U8,
        2 => NumberType::U16,
        4 => NumberType::U32,
        8 => NumberType::U64,
        _ => panic!("Pointer size is not yet supported")
    }
}
