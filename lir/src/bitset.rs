/// This module contains some data structure used for representing bit sets.

use std::collections::BTreeSet;

/// This implementation implement a set of bits which is encoded with 0 being
/// the most representated value.
// TODO: This implementation is very naive and might be optimized for a
// representation of bit set which is less sparsed.
pub type BitSet = BTreeSet<u32>;
