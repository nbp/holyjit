//! Defines the LIR structures and how to represent a graph and its
//! instructions.
use std::fmt;

/// Prototype of the Mir graph of a single function. This representation is
/// not optimized for graph optimizations, but optimized only for the ease
/// of convertion from the MIR and the ease of naive compilation.
#[derive(Serialize, Deserialize, Debug)]
pub struct CompilationUnit {
    /// Size of all local variable of the Mir.
    pub stack_size: usize,

    /// Ordered list of arguments, with their associated registers.
    pub args_defs: Vec<RegDef>,

    /// List of basic blocks of a given function.
    pub blocks: Vec<BasicBlockData>,
}

/// (Prototype) Set of instruction within a block.
pub type BasicBlock = usize;

/// Basic block which contains a list of instructions.
#[derive(Serialize, Deserialize, Debug)]
pub struct BasicBlockData {
    /// Ordered list of registers available in this basic block.
    //
    // Note: We should probably look at encoding these lists of registers as
    // spagheti stacks, such that we do not over-consume memory, by storing
    // this information.
    pub input_regs: Vec<RegDef>,
    /// Ordered list of registers available after this basic block.
    pub output_regs: Vec<Reg>,
    /// Ordered list of instructions.
    pub insts: Vec<Inst>,
    /// How the basic block ends.
    pub end: Terminator,
}

/// Basic block terminator instruction, which resume the execution in
/// another basic block.
#[derive(Serialize, Deserialize, Debug)]
pub enum Terminator {
    /// Exit successfully the current function.
    Return {
        value: Option<Reg>,
    },

    /// Unwind the current function.
    Unwind,

    /// Trap.
    Unreachable,

    /// Jump unconditionally to the next basic block.
    Goto {
        target: BasicBlock,
    },

    /// Conditional branches, implemented as a switch case to handle all
    /// forms of conditionals.
    SwitchInt {
        value: Reg,
        range: RangeInclusive,
        targets: Vec<(Imm, BasicBlock)>,
        otherwise: Option<BasicBlock>,
    },

    /// Call a function. (any function, or an assertion, or a drop function)
    Call {
        /// Pointer to a given function.
        function: Reg,

        // TODO: Add a reference of the ABI we are compiling this call with.

        /// Set of argument to be used for calling the function.
        args: Vec<Reg>,

        /// If the function returns, then the following register would be
        /// defined in the block, and listed in the output_regs of the
        /// current block, and the inputs-regs of the listed block.
        return_target: Option<(Option<RegDef>, BasicBlock)>,

        /// If the function unwinds then the unwinding resumes in the
        /// following block.,
        unwind_target: Option<BasicBlock>
    }
}

pub type Sz = usize;
pub type Reg = usize;
pub type Imm = isize;
pub type RegDef = (Reg, Sz);
pub type RangeInclusive = (Imm, Imm);

/// (Prototype) Minimal set of instructions to support the MIR graph of
/// Rust for the examples directory.
#[derive(Serialize, Deserialize, Debug)]
pub enum Inst {
    // Initialize the frame pointer.
    SetFramePtr(Reg, Sz, Sz),

    // Copy the address of a static value in a register.
    Static(Reg, Imm, Sz),

    // Copy a constant into a register.
    CopyImm(Reg, Imm, Sz),

    // Cast operator for dummies.
    Resize(Reg, Reg, Sz),

    // Logical & Math operations.
    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    Mul(Reg, Reg, Reg),
    Div(Reg, Reg, Reg),
    Rem(Reg, Reg, Reg),
    BitXor(Reg, Reg, Reg),
    BitAnd(Reg, Reg, Reg),
    BitOr(Reg, Reg, Reg),
    Shl(Reg, Reg, Reg),
    Shr(Reg, Reg, Reg),
    Eq(Reg, Reg, Reg),
    Lt(Reg, Reg, Reg),
    Le(Reg, Reg, Reg),
    Ne(Reg, Reg, Reg),
    Gt(Reg, Reg, Reg),
    Ge(Reg, Reg, Reg),

    // Convert an operation result into a checked operation result, i-e add
    // a boolean size value to the register size.
    Chk(Reg, Reg),

    // Store at the address, the register value.
    Store(Reg, Reg, Sz),

    // Load into the register, from the address.
    Load(Reg, Reg, Sz),

    // Note: These 2 instructions are based on the infinite size registers,
    // and are made to represent structures which are held in
    // registers. This is a useful trick for generating code from Rust's
    // Mir, but also a useful trick to represent owned memory.

    // Initialize the content of a register with the the content of a
    // smaller one at an offset of the first.
    StoreInto(Reg, Reg, /* offset */ Sz, /* size */ Sz),

    // Load from a register at a given offset, and a given size within the
    // register the value to be output in the first register.
    LoadFrom(Reg, Reg, /* offset */ Sz, /* size */ Sz),

    // Reserve, or kill a register allocation.
    // Note: Live is useless in case of SSA forms.
    Live(Reg), Dead(Reg),
}

/// Display a Compilation unit with some hard-coded indentation level.
impl fmt::Display for CompilationUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CompilationUnit {{\n")?;
        write!(f, "  stack_size: {:?},\n", self.stack_size)?;
        write!(f, "  args_defs: {:?},\n", self.args_defs)?;
        write!(f, "  blocks: [\n")?;
        for (x, b) in self.blocks.iter().enumerate() {
            write!(f, "    [{}] = {},\n", x, b)?;
        }
        write!(f, "  ],\n")?;
        write!(f, "}}")
    }
}

/// Display a BasicBlockData unit with some hard-coded indentation level.
impl fmt::Display for BasicBlockData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BasicBlockData {{\n")?;
        write!(f, "      input_regs: {:?},\n", self.input_regs)?;
        write!(f, "      insts: [\n")?;
        for ins in &self.insts {
            write!(f, "        {:?},\n", ins)?;
        }
        write!(f, "      ],\n")?;
        write!(f, "      end: {:?},\n", self.end)?;
        write!(f, "      output_regs: {:?},\n", self.output_regs)?;
        write!(f, "    }}")
    }
}
