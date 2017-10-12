/// A context contains a way to map a vector to its jit-compiled counter
/// part.  It provides an interface for tuning the JIT compiler parameters
/// as well as the heuristics for enterring the JIT.

use compile::JitCode;
use std::rc::Rc;

/// Opaque structure which is used to store the function mapping, and tune
/// the JIT parameters.
pub struct JitContext {
    code: Option<Rc<JitCode>>
}

impl JitContext {
    pub fn lookup(&self, bytes: &[u8], defs: *const ()) -> Option<Rc<JitCode>> {
        match &self.code {
            &Some(ref jit) => {
                println!("Found JIT Code in the context");
                Some(jit.clone())
            }
            &None => {
                println!("Did not found JIT Code in the context.\nStart compiling ...");
                match JitCode::compile(bytes, defs) {
                    Ok(jit) => {
                        let jit = Rc::new(jit);
                        // TODO: Store the Jit code on the context.
                        // self.code = Some(jit);
                        Some(jit)
                    }
                    Err(e) => {
                        println!("JIT Compiler Error: {:?}", e);
                        None
                    }
                }
            }
        }
    }
}

impl Default for JitContext {
    fn default() -> JitContext {
        JitContext{ code: None }
    }
}
