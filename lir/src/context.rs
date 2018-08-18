use types::{ComplexType, ComplexTypeId};

/// A context is a structure which centralize all the data necessary for the
/// execution of any Unit. It holds the collection of complex types, and any
/// counter related to having unique identifiers.
pub struct Context {
    /// This counter is used for both Rehash instructions and Newhash
    /// instructions. It holds the next value to be allocated if any of these
    /// instruction should be added to the graph.
    wrapper_seed: usize,

    /// This vector holds the list of types references by all Unit associated to
    /// this context. Any ComplexTypeId is an index in this Vector.
    types: Vec<ComplexType>,
}

impl Context {
    /// Create a new Context. This function should not be used externally,
    /// instead use a ContextBuilder to build a context for you.
    pub fn new() -> Context {
        Context {
            wrapper_seed: 0,
            types: vec![],
        }
    }

    /// Create a new hash seed, such that we can avoid aliasing of hash values.
    /// This function is used by the ContextBuilder for creating new Rehash /
    /// Newhash instructions.
    pub fn get_hash_seed(&mut self) -> usize {
        let value = self.wrapper_seed;
        self.wrapper_seed += 1;
        value
    }

    /// Add a new complex type in the list of known types. This function is used
    /// by the ContextBuilder to register types seen while generating Units.
    pub fn add_type(&mut self, ty: ComplexType) -> ComplexTypeId {
        self.types.push(ty);
        ComplexTypeId(self.types.len() - 1)
    }

    /// Given a ComplexTypeId, returns the associated type.
    pub fn get_type(&self, id: ComplexTypeId) -> &ComplexType {
        let ComplexTypeId(index) = id;
        &self.types[index]
    }
}
