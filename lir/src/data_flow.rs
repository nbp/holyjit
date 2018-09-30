/// The data flow is a graph of operation described by their opcodes and memory
/// dependencies. The data flow express how the data are flowing from the Unit
/// inputs to the Unit output. The data flow cannot be evaluated with the
/// addition of a control flow, which describes the interpretation of Phi nodes
/// operands.

/// Automatically derive a hashing function for each type, to make sure that we
/// can apply patches to a subset of instructions.
use std::hash::{Hash, Hasher};

use number;
use unit;
use types::ComplexTypeId;

/// A Data flow contains all the instructions from one Unit, and they describe
/// how data are flowing through instructions.
#[derive(Serialize, Deserialize, Debug)]
pub struct DataFlow {
    /// Set of instruction, where each instruction should be referenced by at
    /// least a Block using their ValueHash.
    pub instructions: Vec<Instruction>,
}

/// A LIR Value corresponds to the computation of either instructions or
/// terminator. As opposed to ordinary SSA notation, we use a hash instead of an
/// instruction index, in order to be able to generate position-independent
/// patches for each Unit.
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct Value {
    pub hash: u64,
    pub index: usize,
}

/// A LIR Instruction is a single operation which aggregates operands and an
/// operation to produce a ValueHash.
#[derive(Serialize, Deserialize, Debug)] /* derive(Hash)-manually */
pub struct Instruction {
    /// Opcode of the instruction.
    pub opcode: Opcode,
    /// Ordered list of operands of the instruction.
    pub operands: Vec<Value>,
    /// Set of previous instructions which might change the memory read by this
    /// instruction.
    pub dependencies: Vec<Value>,
    /// Set if this instruction got replaced by another. This is not taken into
    /// account while computing the hash of an instruction.
    pub replaced_by: Option<Value>,
}

#[derive(Serialize, Deserialize, Debug, Hash, Clone, Copy)]
pub struct SwitchData {
    pub low: i32,
    pub high: i32,
}

/// An Opcode is an instruction which contains basic operations. This list of
/// opcode is minimal and contains all the building blocks to express higher
/// level IR instructions which are platform specific as intrinsic units.
#[derive(Serialize, Deserialize, Debug, Hash, Clone, Copy)]
pub enum Opcode {
    /// Denote the Entry point of the given Unit. This instruction should be
    /// replaced when the data flow graph is inlined in another as the entry
    /// should not alias the entry of the caller.
    Entry(unit::UnitId),

    /// Rehash is a way to wrap a Value with another ValueHash which will be
    /// used either for loop-back or to limit the amount of context referrenced
    /// in a patch.
    /// (1 operand)
    Rehash(usize),

    /// Same as Rehash except that we are wrapping an unknown value which does
    /// not exists yet. This is useful for mapping arguments of a function, or
    /// for extra entry points.
    Newhash(usize),

    /// Phi is an instruction which merges values from different blocks. Note
    /// that this LIR uses Phi instead of extended basic block, in order to
    /// avoid carrying around too many variables to the next block, which would
    /// imply additional rewrite of the graphs on inlining.
    /// (multiple operands)
    Phi,

    /// Encode a constant.
    /// (0 operand)
    Const(number::NumberValue),

    /// Cast is used to change the type interpretation of a Value without any content checks.
    /// (1 operand)
    Cast(ComplexTypeId),

    /// Extract overflow flag from the operation on which this instruction
    /// depends on. (0 operand, 1 dependency)
    OverflowFlag,
    /// Extract carry flag from the operation on which this instruction depends
    /// on. (0 operand, 1 dependency)
    CarryFlag,

    /// Addition. (2 operands)
    Add(number::NumberType),
    /// Substraction. (2 operands: result = lhs - rhs)
    Sub(number::NumberType),
    /// Multiplication. (2 operands)
    Mul(number::NumberType),
    /// Division. (2 operands: result = lhs / rhs)
    Div(number::NumberType),
    /// Remainder. (2 operands: result = lhs % rhs)
    Rem(number::SignedType),
    /// Sign-extend. (1 operand)
    SignExt(number::SignedType),
    /// Zero-extend. (1 operand)
    ZeroExt(number::IntType),

    /// Truncate. (round towards zero) (1 operand)
    Truncate(number::FloatType),
    /// Round. (round towards nearest) (1 operand)
    Round(number::FloatType),
    /// Floor. (round towards -Inf) (1 operand)
    Floor(number::FloatType),
    /// Ceil. (round towards +Inf) (1 operand)
    Ceil(number::FloatType),

    /// Bitwise exclusive or. (2 operands)
    BwXor(number::IntType),
    /// Bitwise And. (2 operands)
    BwAnd(number::IntType),
    /// Bitwise Or. (2 operands)
    BwOr(number::IntType),
    /// Bitwise Not. (2 operands)
    BwNot(number::IntType),
    /// Shift left. (2 operands: result = lhs << rhs)
    ShiftLeft(number::IntType),
    /// Shift right. (2 operands: result = lhs >> rhs)
    ShiftRight(number::SignedType),

    /// Equal. (2 operands)
    Eq(number::NumberType),
    /// Less than. (2 operands: result = lhs < rhs)
    Lt(number::NumberType),
    /// Less than or equal. (2 operands: result = lhs <= rhs)
    Le(number::NumberType),
    /// Not equal. (2 operands)
    Ne(number::NumberType),
    /// Greather than. (2 operands: result = lhs > rhs)
    Gt(number::NumberType),
    // Greather than or equal. (2 operands: result = lhs >= rhs)
    Ge(number::NumberType),

    /// StaticAddress is used to refer to data which is not yet known at compile
    /// time, but known at the execution, such as function pointer addresses.
    /// (0 operand)
    StaticAddress,
    /// CPUAddress is used to refer to internal CPU data, and help the compiler
    /// reason about the aliasing intrinsic using CPU data, such as flags and
    /// cpuid.
    /// (0 operand)
    CPUAddress,
    /// Get the address of where the input operand is stored. At the end of the
    /// pipeline, if any of these instructions remain it enforces the data to
    /// live in memory at least as long as the address exists.
    /// (1 operand)
    Address,

    /// Load content from the address. (1 operand: result = *input)
    Load(ComplexTypeId),
    /// Store content to the address. (2 operands: *lhs = rhs)
    Store(ComplexTypeId),

    // Acquire = LoadFence{Load, Store}
    // Release = {Load, Store}FenceStore

    /// LoadFenceLoad or read barrier implies that all loads must be completed
    /// before proceeding with any loads. (Prevent the compiler from moving load
    /// instructions) (0 operand)
    LoadFenceLoad,
    /// LoadFenceStore implies that all loads must be completed before
    /// proceeding with any stores. (Prevent the compiler from moving load and
    /// store instructions) (0 operand)
    LoadFenceStore,
    /// StoreFenceLoad implies that all stores must be completed before
    /// proceeding with any loads. (Prevent the compiler from moving load and
    /// store instructions) (0 operand)
    StoreFenceLoad,
    /// StoreFenceStore or write barrier implies that all stores must be
    /// completed before proceeding with any stores. (Prevent the compiler from
    /// moving store instructions) (0 operand)
    StoreFenceStore,

    /// Unit is used for non fallible unit. For example, this can be used for
    /// non-inlinable and non-optimizable intrinsics which are expressed in
    /// terms of the minimal set of instructions. This is used to provide target
    /// specific instructions such as SIMD, locked-instructions or cpuid which
    /// are not represented in this LIR. (maybe operands)
    Unit(unit::UnitId),

    //
    // List of Control instructions.
    //

    /// Return the value computed by the instruction behind Value. This
    /// corresponds either to the returned value of a function, or to the result
    /// of an expression for subset of Rust functions. Note, the number of
    /// operands of any return opcode should match the number of outputs
    /// described in the signature of the Unit.
    /// (? operand)
    Return,

    /// Unwind ends the control flow, and unwind everything.
    Unwind,

    /// Unreachable is used either as an assertion / optimization mechanism.
    Unreachable,

    /// Goto is an unconditional jump to another basic block in the same Unit.
    /// (0 operand, default target)
    Goto,

    /// Switch is a conditional branch implemented as a switch case over
    /// variable ranges of integer values. This is used even for simple if/else
    /// branches.
    /// (1 operand, maybe default target, targets)
    Switch(SwitchData),

    /// Call implements a function call, such as any Rust function, an assertion
    /// or a drop function. The argument correspond to the signature of the
    /// function being called. (many operands: function + arguments, maybe
    /// default target, maybe unwind target)
    Call(ComplexTypeId),

    /// CallUnit implements an internal Unit call or inline.
    /// (many operands: arguments, maybe default target, maybe unwind target)
    CallUnit(unit::UnitId),
}

pub enum ValueType {
    Boolean,
    Pointer,
    Number(number::NumberType),
    Complex(ComplexTypeId),
    ResultOfUnit(unit::UnitId),
    ResultOfSig(ComplexTypeId),
    InheritFromOperands,
    None,
}

impl Opcode {
    pub fn is_control(self) -> bool {
        match self {
            Opcode::Return |
            Opcode::Unwind |
            Opcode::Unreachable |
            Opcode::Goto |
            Opcode::Switch(_) |
            Opcode::Call(_) |
            Opcode::CallUnit(_) => true,
            _ => false,
        }
    }

    pub fn is_return(self) -> bool {
        match self {
            Opcode::Return => true,
            _ => false,
        }
    }

    pub fn is_phi(self) -> bool {
        match self {
            Opcode::Phi => true,
            _ => false,
        }
    }

    pub fn is_rehash(self) -> bool {
        match self {
            Opcode::Rehash(_) => true,
            _ => false,
        }
    }

    pub fn result_type(self) -> ValueType {
        use self::Opcode::*;
        match self {
            Entry(_) |
            Newhash(_) => ValueType::None,
            Rehash(_) |
            Phi => ValueType::InheritFromOperands,
            Const(val) => ValueType::Number(val.into()),
            Cast(id) => ValueType::Complex(id),
            OverflowFlag |
            CarryFlag => ValueType::Boolean,
            Add(n) |
            Sub(n) |
            Mul(n) |
            Div(n) => ValueType::Number(n),
            Rem(n) => ValueType::Number(n.into()),
            SignExt(n) => ValueType::Number(n.into()),
            ZeroExt(n) => ValueType::Number(n.into()),
            Truncate(f) |
            Round(f) |
            Floor(f) |
            Ceil(f) => ValueType::Number(f.into()),
            BwXor(b) |
            BwAnd(b) |
            BwOr(b) |
            BwNot(b) => ValueType::Number(b.into()),
            ShiftLeft(i) => ValueType::Number(i.into()),
            ShiftRight(i) => ValueType::Number(i.into()),
            Eq(_) | Lt(_) | Le(_) |
            Ne(_) | Gt(_) | Ge(_) => ValueType::Boolean,
            StaticAddress |
            Address => ValueType::Pointer,
            CPUAddress => ValueType::None,
            Load(_) => ValueType::None,
            Store(ty) => ValueType::Complex(ty),
            LoadFenceLoad |
            LoadFenceStore |
            StoreFenceLoad |
            StoreFenceStore => ValueType::None,
            Unit(id) => ValueType::ResultOfUnit(id),
            Return |
            Unwind |
            Unreachable |
            Goto |
            Switch(_) => ValueType::None,
            Call(id) => ValueType::ResultOfSig(id),
            CallUnit(id) => ValueType::ResultOfUnit(id),
        }
    }
}

impl Instruction {
    pub fn is_control(&self) -> bool { self.opcode.is_control() }
    pub fn is_phi(&self) -> bool { self.opcode.is_phi() }
    pub fn is_rehash(&self) -> bool { self.opcode.is_rehash() }
}

impl Hash for Instruction {
    fn hash<H : Hasher>(&self, state: &mut H) {
        self.opcode.hash(state);
        if !self.is_rehash() {
            // Rehash ignores the hash of its operands in order to properly
            // handle loops without having circulat computation of hashes. Note,
            // we could use the replace_by field to implement Rehash based on
            // Newhash, but we might want to have the ability to remove all
            // replace_by fields when needed.
            self.operands.hash(state);
        }
        self.dependencies.hash(state);
        // Exclude self.replaced_by.
    }
}

impl<'a> From<&'a Instruction> for u64 {
    fn from(ins: &'a Instruction) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        let mut hasher = DefaultHasher::new();
        ins.hash(&mut hasher);
        hasher.finish()
    }
}

impl DataFlow {
    pub fn new() -> DataFlow {
        DataFlow { instructions: vec![] }
    }

    fn get_value(&self, index: usize) -> Value {
        Value { hash: (&self.instructions[index]).into(), index }
    }

    pub fn add_ins(&mut self, ins: Instruction) -> Value {
        // TODO: Ensure that if the instruction already exists, then it is not
        // being added a second time, and the returned Value output correspond
        // to the existing Instruction.
        // TODO: Add consistency checks that all value references are indeed in
        // the current DataFlow structure.
        self.instructions.push(ins);
        self.get_value(self.instructions.len() - 1)
    }
}

impl Value {
    // Create a Dummy value which should fail any validation test.
    pub fn dummy() -> Value {
        Value { hash: 0, index: usize::max_value() }
    }
    // Check if this is the value created as a dummy.
    pub fn is_dummy(self) -> bool {
        self.index == usize::max_value()
    }
    pub fn index(self) -> usize {
        debug_assert!(!self.is_dummy());
        self.index
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rehash() {
        let mut df = DataFlow::new();
        let v0 = df.add_ins(Instruction {
            opcode: Opcode::Const(number::NumberValue::U32(1024)),
            operands: vec![],
            dependencies: vec![],
            replaced_by: None,
        });
        let v1 = df.add_ins(Instruction {
            opcode: Opcode::Rehash(21),
            operands: vec![v0],
            dependencies: vec![],
            replaced_by: None,
        });
        let v2 = df.add_ins(Instruction {
            opcode: Opcode::Rehash(69),
            operands: vec![v0],
            dependencies: vec![],
            replaced_by: None,
        });
        // Rehash opcode compute a different hash value based on the number
        // which is given as argument. This is used to handle loops.
        assert_ne!(v1.hash, v2.hash);
    }

    #[test]
    fn rehash_ignore_operands() {
        let mut df = DataFlow::new();
        let v0 = df.add_ins(Instruction {
            opcode: Opcode::Const(number::NumberValue::U32(1024)),
            operands: vec![],
            dependencies: vec![],
            replaced_by: None,
        });
        let v1 = df.add_ins(Instruction {
            opcode: Opcode::Const(number::NumberValue::U32(512)),
            operands: vec![],
            dependencies: vec![],
            replaced_by: None,
        });
        let v2 = df.add_ins(Instruction {
            opcode: Opcode::Rehash(51),
            operands: vec![v0],
            dependencies: vec![],
            replaced_by: None,
        });
        let v3 = df.add_ins(Instruction {
            opcode: Opcode::Rehash(51),
            operands: vec![v1],
            dependencies: vec![],
            replaced_by: None,
        });
        // Rehash opcode compute a hash which is independent of the operand.
        // This is used to avoid loops in hashes computations.
        assert_eq!(v2.hash, v3.hash);
    }

    #[test]
    fn stable_hash() {
        // TODO: We should add test cases like that for each instruction, to
        // ensure that 2 identical instructions are given the same hash.
        let mut df = DataFlow::new();
        let v0 = df.add_ins(Instruction {
            opcode: Opcode::Const(number::NumberValue::U32(1024)),
            operands: vec![],
            dependencies: vec![],
            replaced_by: None,
        });
        let v1 = df.add_ins(Instruction {
            opcode: Opcode::Const(number::NumberValue::U32(1024)),
            operands: vec![],
            dependencies: vec![],
            replaced_by: None,
        });
        // Rehash opcode compute a different hash value based on the number
        // which is given as argument. This is used to handle loops.
        assert_eq!(v0.hash, v1.hash);
    }

    #[test]
    fn replaced_by() {
        let mut df = DataFlow::new();
        let v0 = df.add_ins(Instruction {
            opcode: Opcode::Const(number::NumberValue::I32(1)),
            operands: vec![],
            dependencies: vec![],
            replaced_by: None,
        });
        let v1 = df.add_ins(Instruction {
            opcode: Opcode::Add(number::NumberType::I32),
            operands: vec![v0, v0],
            dependencies: vec![],
            replaced_by: None,
        });
        let v2 = df.add_ins(Instruction {
            opcode: Opcode::Const(number::NumberValue::I32(2)),
            operands: vec![v0],
            dependencies: vec![],
            replaced_by: None,
        });
        df.instructions[v1.index].replaced_by = Some(v2);
        // When setting the replaced_by field, the hash of an instruction should
        // not change.
        assert_eq!(v1.hash, df.get_value(v1.index).hash);
    }
}
