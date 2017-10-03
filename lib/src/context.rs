/// A context contains a way to map a vector to its jit-compiled counter
/// part.  It provides an interface for tuning the JIT compiler parameters
/// as well as the heuristics for enterring the JIT.

use compile::JitCode;

/// Opaque structure which is used to store the function mapping, and tune
/// the JIT parameters.
pub struct JitContext {
    code: Option<JitCode>
}

impl JitContext {
    pub fn lookup<'ctx, Args, Output>(&self, bytes: &[u8], defs: *const ()) -> Option<&'ctx Fn<Args, Output = Output>> {
        match &self.code {
            &Some(ref jit) => Some(jit.get_fn::<'ctx, Args, Output>()),
            &None => match JitCode::compile(bytes, defs) {
                Ok(jit) => {
                    // TODO:
                    // self.code = Some(jit);
                    Some(jit.get_fn::<'ctx, Args, Output>())
                }
                Err(_) => None
            }
        }
    }
}

impl Default for JitContext {
    fn default() -> JitContext {
        JitContext{ code: None }
    }
}
