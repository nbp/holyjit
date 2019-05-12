/// Complex types are used to help analysis running on the data flow graph solve
/// problem such as aliassing. Using types to dissambiguate aliasing issues is
/// useful as it reduces the depenency graph between load and store
/// instructions.
use number;

/// ComplexType indexes are used to index an arithmetic type or an aggregated
/// type, such as tuples, enums or structures within the Assembly.
#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub struct ComplexTypeId(pub usize);

/// Offset within an aggregated type.
#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub struct Offset(pub usize);

/// Determine if a given function can unwind or not, If not no unwind successors
/// would be exepected.
#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub struct CanUnwind(pub bool);

/// Define the preferred memory representation for one value. This is mainly
/// used to determine the way to transfer function's arguments.
#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Hash, Clone)]
pub enum MemoryRepr {
    None,
    Register,
    RegisterPair,
    Vector{ bytes: usize },
    PointerTo{ bytes: usize },
}

/// Define how a type is composed.
#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Hash, Clone)]
pub enum DataRepr {
    /// Functions are used to express the signature of Unit and external
    /// functions. At the moment, all functions are assumed to follow the same
    /// calling convention as rust functions.
    Function(Vec<ComplexTypeId>, Vec<ComplexTypeId>, CanUnwind),
    /// Structures are used to map each offsets with its corresponding type.
    /// Offsets are expected to be sorted by increasing order.
    Structure(Vec<(Offset, ComplexTypeId)>),
    /// Unions are used to select between multiple structures.
    Union(Vec<ComplexTypeId>),
    /// Untyped pointers, the type is carried by the load and store operations.
    /// This simplify the problem by not having to handle recursive types.
    Pointer,
    /// A Scalar represents a number.
    Number(number::NumberType),
}

/// A complex type is defined as a way to represent a value in memory as well as
/// the meaning of its compoenents. Types are recorded on the Context which is
/// shared across multiple Units, such that one Unit can call another and use
/// the same ComplexTypeId.
#[derive(Serialize, Deserialize, PartialEq, Eq, Debug, Hash, Clone)]
pub struct ComplexType {
    /// Defines the memory representation of this type and how it is to be
    /// stored and transfered.
    pub mem: MemoryRepr,
    /// Defines the data representation of this type and how it is to be
    /// accessed as well as the meaning associated with it.
    pub data: DataRepr,
}

impl ComplexType {
    /// Simple new function to create a number stored in a register.
    pub fn new_scalar(num: number::NumberType) -> ComplexType {
        ComplexType {
            mem: MemoryRepr::Register,
            data: DataRepr::Number(num),
        }
    }

    /// Simple new function to create a number stored in a register.
    pub fn new_ptr() -> ComplexType {
        ComplexType {
            mem: MemoryRepr::Register,
            data: DataRepr::Pointer,
        }
    }

    /// Simple new function to create a function pointer.
    pub fn new_fn(ins: Vec<ComplexTypeId>, outs: Vec<ComplexTypeId>, unwind: CanUnwind) -> ComplexType {
        ComplexType {
            mem: MemoryRepr::Register,
            data: DataRepr::Function(ins, outs, unwind),
        }
    }
}
