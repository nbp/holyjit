/// A context contains a way to map a vector to its jit-compiled counter
/// part.  It provides an interface for tuning the JIT compiler parameters
/// as well as the heuristics for enterring the JIT.

/// Opaque structure which is used to store the function mapping, and tune
/// the JIT parameters.
#[derive(Copy, Clone)]
pub struct JitContext {
}

impl JitContext {
    pub fn lookup(&self, _bytes: &[u8], _defs: *const ()) -> () {
        ()
    }
}

impl Default for JitContext {
    fn default() -> JitContext {
        JitContext{}
    }
}
