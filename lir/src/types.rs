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

/// A complex type is either a function signature, an structure, an union, a
/// pointer, a scalar or a vector of scalar. All these types should be aggregaed
/// globally, such that that can be used across multiple Units.
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Clone)]
pub enum ComplexType {
    /// Functions are used to express the signature of Unit and external
    /// functions.
    Function(Vec<ComplexTypeId>, Vec<ComplexTypeId>, CanUnwind),
    /// Structures are used to map each offsets with its corresponding type.
    Structure(Vec<(Offset, ComplexTypeId)>),
    /// Unions are used to select between multiple structures.
    Union(Vec<ComplexTypeId>),
    /// Untyped pointers, the type is carried by the load and store operations.
    /// This simplify the problem by not having to handle recursive types.
    Pointer,
    /// A Scalar represents a number.
    Scalar(number::NumberType),
    /// A Vector represents an aggregation of Scalar.
    Vector(number::NumberType, usize),
}
