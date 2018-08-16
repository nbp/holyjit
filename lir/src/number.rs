/// This module implement all Number types, to specialize operations for a given
/// numerical type, and to encode constant with the proper numerical type too.
use std::hash::{Hash, Hasher};

/// NumberType are used for math and bitwise operators. All other number types
/// can be convert to this one, including NumberValue, using the `into()`
/// method.
#[derive(Serialize, Deserialize, Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum NumberType {
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
}
#[derive(Serialize, Deserialize, Debug, Hash, Clone, Copy)]
pub enum IntType {
    U8, U16, U32, U64,
}
#[derive(Serialize, Deserialize, Debug, Hash, Clone, Copy)]
pub enum SignedType {
    U8, U16, U32, U64,
    I8, I16, I32, I64,
}
#[derive(Serialize, Deserialize, Debug, Hash, Clone, Copy)]
pub enum FloatType {
    F32, F64,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)] /* derive(Hash)-manually */
pub enum NumberValue {
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
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
);


impl Hash for NumberValue {
    fn hash<H : Hasher>(&self, state: &mut H) {
        use std::mem;
        mem::discriminant(self).hash(state);
        match self {
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

