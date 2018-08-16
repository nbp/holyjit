use codegen::CodegenError;
use mmap::MapError;
use region;

/// Simple wrapper type to wrap any lowerring error.
pub type LowerResult<T> = Result<T, LowerError>;

/// Lowering error can be any error involve either during code generation, or
/// during the allocation of code to memory.
#[derive(Debug)]
pub enum LowerError {
    CodeGen(CodegenError),
    Map(MapError),
    Protect,
}

// TODO: impl Error for LowerError
// TODO: impl Display for LowerError

impl From<CodegenError> for LowerError {
    /// Implictly convert Cranelift codegen errors into LowerError.
    fn from(err: CodegenError) -> LowerError {
        LowerError::CodeGen(err)
    }
}

impl From<MapError> for LowerError {
    /// Implictly convert mmap errors into LowerError.
    fn from(err: MapError) -> LowerError {
        LowerError::Map(err)
    }
}

impl From<region::Error> for LowerError {
    /// Implictly convert region errors into LowerError.
    fn from(_err: region::Error) -> LowerError {
        LowerError::Protect
    }
}
