//! Compiles the LIR representation into executable code using the dynasm
//! plugin and library. The use of dynasm is only for having a working proof
//! of concept, but is unlikely to be part of the final solution.

use dynasmrt;
use dynasmrt::x64::Assembler;
// use dynasmrt::{DynasmApi, DynasmLabelApi};

use lir;

/// The compiler state.
pub struct Compiler {
    /// The underlying assembler.
    asm: Assembler,
    /// The offset of the starting instruction.
    start: dynasmrt::AssemblyOffset,
    /// Set of labels for linking blocks with each others.
    bb_labels: Vec<dynasmrt::DynamicLabel>
}

#[derive(Debug)]
pub enum Error {
    FinalizeError,
}

impl Compiler {
    pub fn compile(mut self, cu: &lir::CompilationUnit) -> Result<dynasmrt::ExecutableBuffer, Error> {
        for _ in 0..cu.blocks.len() {
            let label = self.asm.new_dynamic_label();
            self.bb_labels.push(label);
        }

        for block in cu.blocks.iter() {
            self.compile_block(block)?
        }

        self.finalize()
    }

    fn compile_block(&mut self, block: &lir::Block) -> Result<(), Error> {
        for inst in block.insts.iter() {
            self.compile_inst(inst)?
        }

        Ok(())
    }

    fn compile_inst(&mut self, inst: &lir::Inst) -> Result<(), Error> {
        Ok(())
    }

    fn finalize(self) -> Result<dynasmrt::ExecutableBuffer, Error> {
        match self.asm.finalize() {
            // TODO: transmute and return a class which implements Fn types.
            Ok(buf) => Ok(buf),
            Err(_) => Err(Error::FinalizeError),
        }
    }
}
