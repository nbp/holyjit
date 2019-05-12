/// This module implement all Number types, to specialize operations for a given
/// numerical type, and to encode constant with the proper numerical type too.
use std::hash::{Hash, Hasher};

/// NumberType are used for math and bitwise operators. All other number types
/// can be convert to this one, including NumberValue, using the `into()`
/// method.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum NumberType {
    B1,
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
}
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum IntType {
    U8, U16, U32, U64,
}
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum SignedType {
    U8, U16, U32, U64,
    I8, I16, I32, I64,
}
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum FloatType {
    F32, F64,
}

/// When comparing numerical types such as Floating point numbers, another
/// aspect appear which is called "Ordered", which basically means that value
/// can be compared. Unordered values would be values such as NaN.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum OrderedType {
    Ordered(NumberType),
    Unordered(FloatType)
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)] /* derive(Hash)-manually */
pub enum NumberValue {
    B1(bool),
    U8(u8), U16(u16), U32(u32), U64(u64),
    I8(i8), I16(i16), I32(i32), I64(i64),
    F32(f32), F64(f64),
}

macro_rules! from_with_same_prefix {
    (impl From<$from:ident> for $to:ident => $($as:ident,)*) => {
        impl From<$from> for $to {
            fn from(input: $from) -> $to {
                match input {
                    $($from::$as => $to::$as),*
                }
            }
        }
    }
}

from_with_same_prefix!(impl From<IntType> for SignedType =>
    U8, U16, U32, U64,
);
from_with_same_prefix!(impl From<IntType> for NumberType =>
    U8, U16, U32, U64,
);
from_with_same_prefix!(impl From<SignedType> for NumberType =>
    U8, U16, U32, U64,
    I8, I16, I32, I64,
);
from_with_same_prefix!(impl From<FloatType> for NumberType =>
    F32, F64,
);

macro_rules! from_with_same_prefix_remove_parent {
    (impl From<$from:ident> for $to:ident => $($as:ident,)* ) => {
        impl From<$from> for $to {
            fn from(input: $from) -> $to {
                match input {
                    $($from::$as(_) => $to::$as,)*
                }
            }
        }
    }
}

from_with_same_prefix_remove_parent!(impl From<NumberValue> for NumberType =>
    B1,
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
);


impl Hash for NumberValue {
    fn hash<H : Hasher>(&self, state: &mut H) {
        use std::mem;
        mem::discriminant(self).hash(state);
        match self {
            &NumberValue::B1(v) => v.hash(state),
            &NumberValue::U8(v) => v.hash(state),
            &NumberValue::U16(v) => v.hash(state),
            &NumberValue::U32(v) => v.hash(state),
            &NumberValue::U64(v) => v.hash(state),
            &NumberValue::I8(v) => v.hash(state),
            &NumberValue::I16(v) => v.hash(state),
            &NumberValue::I32(v) => v.hash(state),
            &NumberValue::I64(v) => v.hash(state),
            &NumberValue::F32(v) => {
                assert_eq!(mem::size_of::<f32>(), mem::size_of::<u32>());
                assert_eq!(mem::align_of::<f32>(), mem::align_of::<u32>());
                let v : u32 = unsafe { mem::transmute_copy(&v) };
                v.hash(state);
            }
            &NumberValue::F64(v) => {
                assert_eq!(mem::size_of::<f64>(), mem::size_of::<u64>());
                assert_eq!(mem::align_of::<f64>(), mem::align_of::<u64>());
                let v : u64 = unsafe { mem::transmute_copy(&v) };
                v.hash(state);
            }
        }
    }
}

impl From<(NumberType, usize)> for NumberValue {
    fn from(input: (NumberType, usize)) -> NumberValue {
        let val = input.1;
        match input.0 {
            NumberType::B1 => NumberValue::B1(val != 0),
            NumberType::U8 => NumberValue::U8(val as u8),
            NumberType::U16 => NumberValue::U16(val as u16),
            NumberType::U32 => NumberValue::U32(val as u32),
            NumberType::U64 => NumberValue::U64(val as u64),
            NumberType::I8 => NumberValue::I8(val as i8),
            NumberType::I16 => NumberValue::I16(val as i16),
            NumberType::I32 => NumberValue::I32(val as i32),
            NumberType::I64 => NumberValue::I64(val as i64),
            NumberType::F32 => NumberValue::F32(val as f32),
            NumberType::F64 => NumberValue::F64(val as f64),
        }
    }
}

impl NumberType {
    pub fn is_float(self) -> bool {
        use self::NumberType::*;
        match self {
            F32 | F64 => true,
            _ => false,
        }
    }
    pub fn is_signed(self) -> bool {
        use self::NumberType::*;
        match self {
            F32 | F64 | I8 | I16 | I32 | I64 => true,
            _ => false,
        }
    }
}
