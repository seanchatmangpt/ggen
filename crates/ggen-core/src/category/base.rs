/// Base category theory structures
///
/// Defines the fundamental concepts of category theory:
/// - Objects: Elements of a category
/// - Morphisms: Structure-preserving maps between objects
/// - Categories: Collections of objects and morphisms with composition

use std::fmt::Debug;
use std::hash::Hash;

/// An object in a category
///
/// Objects represent the "things" in a category. In our context:
/// - RDF triples, graphs, or ontologies
/// - Code structures (ASTs, types, modules)
/// - Template definitions
/// - SPARQL queries
pub trait Object: Clone + Debug + PartialEq {}

/// A morphism (arrow) in a category
///
/// Morphisms represent structure-preserving transformations between objects.
/// They must satisfy:
/// - **Identity**: For every object A, there exists id_A: A → A
/// - **Composition**: f: A → B and g: B → C gives g ∘ f: A → C
/// - **Associativity**: h ∘ (g ∘ f) = (h ∘ g) ∘ f
pub trait Morphism<A: Object, B: Object>: Clone + Debug {
    /// Apply the morphism to transform an object
    fn apply(&self, obj: &A) -> Result<B, MorphismError>;

    /// Get the source object type (domain)
    fn source(&self) -> &str;

    /// Get the target object type (codomain)
    fn target(&self) -> &str;

    /// Compose this morphism with another
    ///
    /// If self: A → B and other: B → C, returns g ∘ f: A → C
    fn compose<C: Object>(
        &self,
        other: &dyn Morphism<B, C>,
    ) -> Result<Box<dyn Morphism<A, C>>, MorphismError>
    where
        Self: Sized;
}

/// Errors that can occur during morphism operations
#[derive(Debug, Clone, thiserror::Error)]
pub enum MorphismError {
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },

    #[error("Composition failed: {reason}")]
    CompositionFailed { reason: String },

    #[error("Invalid morphism: {reason}")]
    Invalid { reason: String },

    #[error("Identity morphism cannot be composed with non-identity")]
    IdentityComposition,

    #[error("Application failed: {reason}")]
    ApplicationFailed { reason: String },
}

/// A category consists of objects and morphisms
///
/// Laws:
/// 1. **Identity**: ∀A ∈ Obj(C), ∃id_A: A → A such that f ∘ id_A = f and id_B ∘ f = f
/// 2. **Associativity**: (h ∘ g) ∘ f = h ∘ (g ∘ f)
pub trait Category {
    /// The type of objects in this category
    type Obj: Object;

    /// The type of morphisms in this category
    type Mor: Clone + Debug;

    /// Get the identity morphism for an object
    fn identity(obj: &Self::Obj) -> Self::Mor;

    /// Compose two morphisms
    ///
    /// Given f: A → B and g: B → C, returns g ∘ f: A → C
    fn compose(f: &Self::Mor, g: &Self::Mor) -> Result<Self::Mor, MorphismError>;

    /// Verify the category laws
    fn verify_laws(&self) -> Result<(), CategoryError>;
}

/// Errors in category structure
#[derive(Debug, Clone, thiserror::Error)]
pub enum CategoryError {
    #[error("Identity law violated: {reason}")]
    IdentityViolation { reason: String },

    #[error("Associativity law violated: {reason}")]
    AssociativityViolation { reason: String },

    #[error("Composition law violated: {reason}")]
    CompositionViolation { reason: String },
}

/// Identity morphism implementation
///
/// For any object A, id_A: A → A is the morphism that does nothing
#[derive(Debug, Clone, PartialEq)]
pub struct Identity<A: Object> {
    _phantom: std::marker::PhantomData<A>,
}

impl<A: Object> Identity<A> {
    pub fn new() -> Self {
        Self {
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<A: Object> Default for Identity<A> {
    fn default() -> Self {
        Self::new()
    }
}

impl<A: Object> Morphism<A, A> for Identity<A> {
    fn apply(&self, obj: &A) -> Result<A, MorphismError> {
        Ok(obj.clone())
    }

    fn source(&self) -> &str {
        std::any::type_name::<A>()
    }

    fn target(&self) -> &str {
        std::any::type_name::<A>()
    }

    fn compose<C: Object>(
        &self,
        other: &dyn Morphism<A, C>,
    ) -> Result<Box<dyn Morphism<A, C>>, MorphismError> {
        // Identity law: id_A ∘ f = f
        Err(MorphismError::IdentityComposition)
    }
}

/// Composite morphism: g ∘ f
///
/// If f: A → B and g: B → C, then Composite(g, f): A → C
#[derive(Debug, Clone)]
pub struct Composite<A: Object, B: Object, C: Object> {
    /// The first morphism (f: A → B)
    pub f: Box<dyn Morphism<A, B>>,
    /// The second morphism (g: B → C)
    pub g: Box<dyn Morphism<B, C>>,
    _phantom: std::marker::PhantomData<(A, B, C)>,
}

impl<A: Object, B: Object, C: Object> Composite<A, B, C> {
    pub fn new(f: Box<dyn Morphism<A, B>>, g: Box<dyn Morphism<B, C>>) -> Self {
        Self {
            f,
            g,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<A: Object, B: Object, C: Object> Morphism<A, C> for Composite<A, B, C> {
    fn apply(&self, obj: &A) -> Result<C, MorphismError> {
        let intermediate = self.f.apply(obj)?;
        self.g.apply(&intermediate)
    }

    fn source(&self) -> &str {
        self.f.source()
    }

    fn target(&self) -> &str {
        self.g.target()
    }

    fn compose<D: Object>(
        &self,
        other: &dyn Morphism<C, D>,
    ) -> Result<Box<dyn Morphism<A, D>>, MorphismError> {
        // Associativity: (h ∘ g) ∘ f = h ∘ (g ∘ f)
        Err(MorphismError::CompositionFailed {
            reason: "Composite morphism composition not yet implemented".to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Example object types for testing
    #[derive(Debug, Clone, PartialEq)]
    struct TestObject {
        value: i32,
    }

    impl Object for TestObject {}

    #[derive(Debug, Clone)]
    struct IncrementMorphism;

    impl Morphism<TestObject, TestObject> for IncrementMorphism {
        fn apply(&self, obj: &TestObject) -> Result<TestObject, MorphismError> {
            Ok(TestObject {
                value: obj.value + 1,
            })
        }

        fn source(&self) -> &str {
            "TestObject"
        }

        fn target(&self) -> &str {
            "TestObject"
        }

        fn compose<C: Object>(
            &self,
            _other: &dyn Morphism<TestObject, C>,
        ) -> Result<Box<dyn Morphism<TestObject, C>>, MorphismError> {
            Err(MorphismError::CompositionFailed {
                reason: "Not implemented".to_string(),
            })
        }
    }

    #[test]
    fn identity_preserves_object() {
        let obj = TestObject { value: 42 };
        let id = Identity::new();
        let result = id.apply(&obj).unwrap();
        assert_eq!(result, obj);
    }

    #[test]
    fn morphism_application() {
        let obj = TestObject { value: 5 };
        let f = IncrementMorphism;
        let result = f.apply(&obj).unwrap();
        assert_eq!(result.value, 6);
    }

    #[test]
    fn composite_morphism() {
        let obj = TestObject { value: 0 };
        let f = Box::new(IncrementMorphism) as Box<dyn Morphism<TestObject, TestObject>>;
        let g = Box::new(IncrementMorphism) as Box<dyn Morphism<TestObject, TestObject>>;
        let composite = Composite::new(f, g);
        let result = composite.apply(&obj).unwrap();
        assert_eq!(result.value, 2); // 0 + 1 + 1
    }
}
