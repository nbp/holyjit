use data_flow::{DataFlow, Value};
use control_flow::ControlFlow;
use types::ComplexTypeId;
use std::fmt;

/// A LIR Unit is a connected set of basic blocks with an entry and exit blocks.
/// This might correspond to a Rust function, a subset of a Rust function which
/// corresponds to an opcode or inline caches, or to an target specific
/// intrinsic abstract code. A Unit contains the set of instructions and blocks
/// which are indexing the instruction in the order in which they are expected
/// to be executed.
#[derive(Serialize, Deserialize, Debug)]
pub struct Unit {
    /// Unique Unit Identifier.
    pub id: UnitId,

    /// Data flow, contains all the instructions and their operands, as well as
    /// the potentially memory dependencies.
    pub dfg: DataFlow,

    /// Control flow, contains all the blocks which are making references to the
    /// data flow instructions, and also the control flow instructions.
    pub cfg: ControlFlow,

    /// Signature of the current unit.
    pub sig: ComplexTypeId,

    /// Value corresponding to the arguments. (Uniquely identified Rehash values)
    pub inputs: Vec<Value>,

    // Set of Value corresponding to the returned value. (Return opcode)
    pub outputs: Vec<Value>,
}

/// Unique Unit identifier of an intrinsic.
type IntrinsicId = u32;
/// Unique unit identifier of a function.
type FunctionId = u32;
/// Unique unit identifier of a sub-set of a function.
type SubSetId = u32;

/// Unique Unit identifier.
#[derive(Serialize, Deserialize, Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum UnitId {
    /// Identifier of a pseudo-code of an intrinsic used to represent the
    /// equivalent LIR of a target specific optimization. Intrisic do not have
    /// support for unwinding.
    Intrinsic(IntrinsicId),

    /// Identifier of a callable function.
    Function(FunctionId),

    /// Identifier of a sub-set of a Rust function.
    SubSet(SubSetId)
}

impl Unit {
    /// Create a new Unit. It is recommended to use the `UnitBuilder` to
    /// construct the data flow and control flow graph of the Unit.
    pub fn new(id: UnitId) -> Unit {
        Unit {
            id,
            dfg: DataFlow::new(),
            cfg: ControlFlow::new(),
            sig: ComplexTypeId(usize::max_value()),
            inputs: vec![],
            outputs: vec![],
        }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use data_flow::VecValue;
        use control_flow::SequenceIndex;
        let width = f.width().unwrap_or(0);
        write!(f, "{e:i$}unit {:?} : {} -> {} {{\n", self.id, VecValue(&self.inputs), VecValue(&self.outputs), e = "", i = width)?;
        for (index, ref seq) in self.cfg.sequences.iter().enumerate() {
            write!(f, "{e:i$}Seq {}:\n", index, e = "", i = width * 2)?;
            for v in seq.sequence.iter() {
                if v.is_dummy() {
                    write!(f, "{e:i$}(dummy instruction)\n", e = "", i = width * 3)?;
                } else {
                    let ins = &self.dfg.instructions[v.index()];
                    write!(f, "{e:i$}{}\n", ins, e = "", i = width * 3)?;
                }
            }
            if seq.control.is_dummy() {
                write!(f, "{e:i$}(dummy control instruction)\n\n", e = "", i = width * 3)?;
            } else {
                let ins = &self.dfg.instructions[seq.control.index()];
                write!(f, "{e:i$}{}\n\n", ins, e = "", i = width * 3)?;
            }
            for &(val, succ) in seq.targets.iter() {
                let SequenceIndex(seq) = seq.successors[succ.0];
                write!(f, "{e:i$}{} -> Seq {}\n", val, seq, e = "", i = width * 3)?;
            }
            if let Some(succ) = seq.default {
                let SequenceIndex(seq) = seq.successors[succ.0];
                write!(f, "{e:i$}default -> Seq {}\n", seq, e = "", i = width * 3)?;
            }
            if let Some(succ) = seq.unwind {
                let SequenceIndex(seq) = seq.successors[succ.0];
                write!(f, "{e:i$}unwind -> Seq {}\n", seq, e = "", i = width * 3)?;
            }
            write!(f, "\n")?;
        }
        write!(f, "{e:i$}}}", e = "", i = width)
    }
}
