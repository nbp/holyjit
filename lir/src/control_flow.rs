/// This module contains the definition of the control flow. The control flow 

use data_flow::Value;

/// A Control flow contains all the sequences and how they flow from one to
/// another. They reference data flow instructions to determine the order in
/// which side-effectful instructions are expected. They are ending with a
/// control flow instruction.
#[derive(Serialize, Deserialize, Debug)]
pub struct ControlFlow {
    /// List of basic sequences.
    pub sequences: Vec<Sequence>,
    /// Index of the entry sequence.
    pub entry: SequenceIndex,
    /// Set of exit sequences indexes.
    pub exit: Vec<SequenceIndex>,
}

/// A LIR Sequence Index is an integer which corresponds to an index within a given Unit.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct SequenceIndex(pub usize);

/// Index within the list of successors.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct SuccessorIndex(pub usize);

/// A LIR Sequence is a sequence of computation of value and a control value,
/// used to decide how the evaluation change sequences. Instead of the common
/// wording of "block", the term sequence is used to express sequences of
/// instructions which are ending with a control instruction to jump to another
/// sequence of code.
#[derive(Serialize, Deserialize, Debug, Hash)]
pub struct Sequence {
    /// Sequence of instructions in the order in which they have to be executed.
    pub sequence: Vec<Value>,
    /// Control instruction.
    pub control: Value,

    /// List of sequences from which the control flow flowed into the current
    /// block. This list is ordered such that it matches the order of Phi
    /// instructions' operands.
    pub predecessors: Vec<(SequenceIndex, SuccessorIndex)>,

    /// List of sequences the control flow instruction can flow into.
    pub successors: Vec<SequenceIndex>,

    /// Goto's branch, Call's return location and Switch default case.
    pub default: Option<SuccessorIndex>,
    /// Error handling sequence, if any error happens during the control flow
    /// instruction.
    pub unwind: Option<SuccessorIndex>,
    /// Switch targets.
    pub targets: Vec<(isize, SuccessorIndex)>,
}

impl ControlFlow {
    pub fn new() -> ControlFlow {
        ControlFlow {
            sequences: vec![],
            entry: SequenceIndex::dummy(),
            exit: vec![],
        }
    }
}

impl SequenceIndex {
    pub fn dummy() -> Self { SequenceIndex(usize::max_value()) }
    pub fn is_dummy(self) -> bool { self == SequenceIndex(usize::max_value()) }
}

impl Sequence {
    pub fn new() -> Self {
        Sequence {
            sequence: vec![],
            control: Value::dummy(),
            predecessors: vec![],
            successors: vec![],
            default: None,
            unwind: None,
            targets: vec![],
        }
    }
}
