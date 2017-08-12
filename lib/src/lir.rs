//! Defines the LIR structures and how to represent a graph and its
//! instructions.

/// Prototype of the Mir graph of a single function. This representation is
/// not optimized for graph optimizations, but optimized only for the ease
/// of convertion from the MIR and the ease of naive compilation.
pub struct CompilationUnit {
    /// Size of all local variable of the Mir.
    pub stack_size: usize,

    /// List of basic blocks of a given function.
    pub blocks: Vec<Block>,
}

/// (Prototype) Set of instruction within a block.
pub struct Block {
    pub insts: Vec<Inst>,
}

/// (Prototype) Minimal set of instructions to support the MIR graph of
/// Rust.
pub enum Inst {
}
