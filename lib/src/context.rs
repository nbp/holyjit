/// A context contains a way to map a vector to its jit-compiled counter
/// part.  It provides an interface for tuning the JIT compiler parameters
/// as well as the heuristics for enterring the JIT.

use bincode;
use lir;
use codegen::{CodeGenerator, JitCode};
use std::rc::Rc;
use std::mem;

/// Opaque structure which is used to store the function mapping, and tune
/// the JIT parameters.
pub struct JitContext {
    // TODO: Storage space for all compiled functions. At the moment only hold a
    // single compiled function.
    code: Option<Rc<JitCode>>,
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
                // TODO: Move this as part of the JitContext. In the mean time
                // deserialize the LIR context everytime we start a compilation.
                let (mut ctx, unit) : (lir::context::Context, lir::unit::Unit) =
                    match bincode::deserialize(bytes) {
                        Ok(res) => res,
                        Err(err) => {
                            println!("bincode::ErrorKind = {}", err);
                            return None
                        }
                    };
                let defs : &'static () = unsafe { mem::transmute(defs) };
                ctx.set_static_refs(defs);
                let mut codegen = CodeGenerator::new();
                match codegen.compile(&ctx, &unit) {
                    Ok(jit) => {
                        let jit = Rc::new(jit);
                        // TODO: Store the Jit code on the context.
                        // self.code = Some(jit);
                        Some(jit)
                    }
                    Err(e) => {
                        println!("JIT Codegen Error: {:?}", e);
                        None
                    }
                }
            }
        }
    }
}

impl Default for JitContext {
    fn default() -> JitContext {
        JitContext{
            code: None,
        }
    }
}
