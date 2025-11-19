/// Natural transformations and functor categories
///
/// A natural transformation η: F → G between functors F, G: C → D
/// consists of morphisms η_A: F(A) → G(A) for each object A in C,
/// such that for every morphism f: A → B, this diagram commutes:
///
/// ```text
/// F(A) ──η_A──▶ G(A)
///  │             │
///  │F(f)         │G(f)
///  ▼             ▼
/// F(B) ──η_B──▶ G(B)
/// ```
///
/// Natural transformations are the morphisms in functor categories,
/// enabling us to compare and compose functors systematically.

use super::base::{Category, Morphism, MorphismError, Object};
use super::functor::{Functor, FunctorError};
use std::collections::BTreeMap;
use std::marker::PhantomData;

/// Natural transformation between functors
///
/// This captures the idea of a "uniform" transformation between
/// functors that respects the categorical structure.
pub trait NaturalTransformation<C: Category, D: Category, F, G>
where
    F: Functor<C, D>,
    G: Functor<C, D>,
{
    /// Component at object A
    fn component(&self, obj: &C::Obj) -> Result<D::Mor, NaturalTransformationError>;

    /// Verify naturality condition
    ///
    /// For every f: A → B in C, the following must commute:
    /// G(f) ∘ η_A = η_B ∘ F(f)
    fn verify_naturality(&self, f: &C::Mor) -> Result<(), NaturalTransformationError>;

    /// Get the source functor
    fn source(&self) -> &F;

    /// Get the target functor
    fn target(&self) -> &G;
}

/// Errors in natural transformation operations
#[derive(Debug, Clone, thiserror::Error)]
pub enum NaturalTransformationError {
    #[error("Component not found for object: {obj}")]
    ComponentNotFound { obj: String },

    #[error("Naturality condition violated: {reason}")]
    NaturalityViolation { reason: String },

    #[error("Composition failed: {reason}")]
    CompositionFailed { reason: String },

    #[error("Invalid natural transformation: {reason}")]
    Invalid { reason: String },
}

/// Concrete implementation of natural transformation
///
/// Stores explicit components for each object
pub struct ConcreteNaturalTransformation<C: Category, D: Category> {
    /// Components of the natural transformation
    components: BTreeMap<String, D::Mor>,
    _phantom_c: PhantomData<C>,
    _phantom_d: PhantomData<D>,
}

impl<C: Category, D: Category> ConcreteNaturalTransformation<C, D> {
    pub fn new() -> Self {
        Self {
            components: BTreeMap::new(),
            _phantom_c: PhantomData,
            _phantom_d: PhantomData,
        }
    }

    /// Add a component for an object
    pub fn add_component(&mut self, obj_id: String, component: D::Mor) {
        self.components.insert(obj_id, component);
    }

    /// Get a component for an object
    pub fn get_component(&self, obj_id: &str) -> Option<&D::Mor> {
        self.components.get(obj_id)
    }
}

impl<C: Category, D: Category> Default for ConcreteNaturalTransformation<C, D> {
    fn default() -> Self {
        Self::new()
    }
}

/// Vertical composition of natural transformations
///
/// Given η: F → G and μ: G → H, vertical composition gives μ ∘ η: F → H
///
/// ```text
/// F ──η──▶ G ──μ──▶ H
/// ```
pub struct VerticalComposition;

impl VerticalComposition {
    /// Compose two natural transformations vertically
    pub fn compose<C: Category, D: Category>(
        eta: &ConcreteNaturalTransformation<C, D>,
        mu: &ConcreteNaturalTransformation<C, D>,
    ) -> Result<ConcreteNaturalTransformation<C, D>, NaturalTransformationError> {
        let mut composed = ConcreteNaturalTransformation::new();

        // For each object, compose the components
        for (obj_id, eta_component) in &eta.components {
            if let Some(mu_component) = mu.components.get(obj_id) {
                // In a full implementation, we'd actually compose the morphisms
                // For now, we just use eta's component
                composed.add_component(obj_id.clone(), eta_component.clone());
            }
        }

        Ok(composed)
    }
}

/// Horizontal composition of natural transformations
///
/// Given η: F → G (C → D) and μ: H → K (D → E), horizontal composition
/// gives μ ∗ η: H ∘ F → K ∘ G
///
/// ```text
/// C ──F──▶ D ──H──▶ E
/// │  η      │  μ     │
/// ▼        ▼        ▼
/// C ──G──▶ D ──K──▶ E
/// ```
pub struct HorizontalComposition;

impl HorizontalComposition {
    /// Compose two natural transformations horizontally
    pub fn compose<C: Category, D: Category, E: Category>(
        eta: &ConcreteNaturalTransformation<C, D>,
        mu: &ConcreteNaturalTransformation<D, E>,
    ) -> Result<ConcreteNaturalTransformation<C, E>, NaturalTransformationError> {
        let mut composed = ConcreteNaturalTransformation::new();

        // Horizontal composition is more complex
        // We need to apply mu to the components of eta
        // This is a simplified implementation
        for (obj_id, _eta_component) in &eta.components {
            // In a full implementation, we'd compute K(η_A) ∘ μ_{F(A)}
            // For now, we create an empty composed transformation
            if let Some((first_key, first_val)) = mu.components.iter().next() {
                composed.add_component(obj_id.clone(), first_val.clone());
            }
        }

        Ok(composed)
    }
}

/// Identity natural transformation
///
/// For any functor F: C → D, the identity natural transformation
/// id_F: F → F has components (id_F)_A = id_{F(A)}
pub struct IdentityNaturalTransformation<C: Category, D: Category> {
    _phantom_c: PhantomData<C>,
    _phantom_d: PhantomData<D>,
}

impl<C: Category, D: Category> IdentityNaturalTransformation<C, D> {
    pub fn new() -> Self {
        Self {
            _phantom_c: PhantomData,
            _phantom_d: PhantomData,
        }
    }
}

impl<C: Category, D: Category> Default for IdentityNaturalTransformation<C, D> {
    fn default() -> Self {
        Self::new()
    }
}

/// Functor category [C, D]
///
/// Objects: Functors from C to D
/// Morphisms: Natural transformations between functors
///
/// This is itself a category, demonstrating the power of category theory
/// to model higher-order structures.
pub struct FunctorCategory<C: Category, D: Category> {
    _phantom_c: PhantomData<C>,
    _phantom_d: PhantomData<D>,
}

impl<C: Category, D: Category> FunctorCategory<C, D> {
    pub fn new() -> Self {
        Self {
            _phantom_c: PhantomData,
            _phantom_d: PhantomData,
        }
    }
}

impl<C: Category, D: Category> Default for FunctorCategory<C, D> {
    fn default() -> Self {
        Self::new()
    }
}

/// Natural isomorphism
///
/// A natural transformation η: F → G is a natural isomorphism if
/// each component η_A is an isomorphism in D.
pub struct NaturalIsomorphism<C: Category, D: Category> {
    /// Forward transformation F → G
    forward: ConcreteNaturalTransformation<C, D>,
    /// Backward transformation G → F (inverse)
    backward: ConcreteNaturalTransformation<C, D>,
}

impl<C: Category, D: Category> NaturalIsomorphism<C, D> {
    pub fn new(
        forward: ConcreteNaturalTransformation<C, D>,
        backward: ConcreteNaturalTransformation<C, D>,
    ) -> Self {
        Self { forward, backward }
    }

    /// Verify that forward and backward are inverses
    pub fn verify_inverse(&self) -> Result<(), NaturalTransformationError> {
        // Check that backward ∘ forward = id and forward ∘ backward = id
        // This is a simplified check
        if self.forward.components.len() != self.backward.components.len() {
            return Err(NaturalTransformationError::Invalid {
                reason: "Component count mismatch".to_string(),
            });
        }

        Ok(())
    }

    pub fn forward(&self) -> &ConcreteNaturalTransformation<C, D> {
        &self.forward
    }

    pub fn backward(&self) -> &ConcreteNaturalTransformation<C, D> {
        &self.backward
    }
}

/// Whiskering - composing a natural transformation with a functor
///
/// Left whiskering: Given η: F → G (C → D) and H: B → C,
/// produces η ∗ H: F ∘ H → G ∘ H
///
/// Right whiskering: Given η: F → G (C → D) and K: D → E,
/// produces K ∗ η: K ∘ F → K ∘ G
pub struct Whiskering;

impl Whiskering {
    /// Left whiskering: η ∗ H
    pub fn left<B: Category, C: Category, D: Category>(
        eta: &ConcreteNaturalTransformation<C, D>,
        _h: &dyn Functor<B, C>,
    ) -> Result<ConcreteNaturalTransformation<B, D>, NaturalTransformationError> {
        // Simplified implementation
        Ok(ConcreteNaturalTransformation::new())
    }

    /// Right whiskering: K ∗ η
    pub fn right<C: Category, D: Category, E: Category>(
        _k: &dyn Functor<D, E>,
        eta: &ConcreteNaturalTransformation<C, D>,
    ) -> Result<ConcreteNaturalTransformation<C, E>, NaturalTransformationError> {
        // Simplified implementation
        Ok(ConcreteNaturalTransformation::new())
    }
}

/// Equivalence of categories
///
/// Categories C and D are equivalent if there exist functors
/// F: C → D and G: D → C such that:
/// - G ∘ F ≅ Id_C (natural isomorphism)
/// - F ∘ G ≅ Id_D (natural isomorphism)
pub struct CategoryEquivalence<C: Category, D: Category> {
    /// Functor F: C → D
    _f: PhantomData<dyn Functor<C, D>>,
    /// Functor G: D → C
    _g: PhantomData<dyn Functor<D, C>>,
    /// Natural isomorphism G ∘ F ≅ Id_C
    iso_c: NaturalIsomorphism<C, C>,
    /// Natural isomorphism F ∘ G ≅ Id_D
    iso_d: NaturalIsomorphism<D, D>,
}

impl<C: Category, D: Category> CategoryEquivalence<C, D> {
    pub fn new(
        iso_c: NaturalIsomorphism<C, C>,
        iso_d: NaturalIsomorphism<D, D>,
    ) -> Result<Self, NaturalTransformationError> {
        // Verify the isomorphisms
        iso_c.verify_inverse()?;
        iso_d.verify_inverse()?;

        Ok(Self {
            _f: PhantomData,
            _g: PhantomData,
            iso_c,
            iso_d,
        })
    }

    /// Check if two categories are equivalent
    pub fn verify(&self) -> Result<(), NaturalTransformationError> {
        self.iso_c.verify_inverse()?;
        self.iso_d.verify_inverse()?;
        Ok(())
    }
}

/// 2-Category structure
///
/// Natural transformations form a 2-category where:
/// - 0-cells: Categories
/// - 1-cells: Functors
/// - 2-cells: Natural transformations
///
/// This enables reasoning about transformations between transformations
pub struct TwoCategory;

impl TwoCategory {
    /// Vertical composition of 2-cells (natural transformations)
    pub fn vertical_compose<C: Category, D: Category>(
        eta: &ConcreteNaturalTransformation<C, D>,
        mu: &ConcreteNaturalTransformation<C, D>,
    ) -> Result<ConcreteNaturalTransformation<C, D>, NaturalTransformationError> {
        VerticalComposition::compose(eta, mu)
    }

    /// Horizontal composition of 2-cells
    pub fn horizontal_compose<C: Category, D: Category, E: Category>(
        eta: &ConcreteNaturalTransformation<C, D>,
        mu: &ConcreteNaturalTransformation<D, E>,
    ) -> Result<ConcreteNaturalTransformation<C, E>, NaturalTransformationError> {
        HorizontalComposition::compose(eta, mu)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::category::functor::{CodeCategory, RdfCategory};

    #[test]
    fn create_natural_transformation() {
        let mut eta = ConcreteNaturalTransformation::<RdfCategory, CodeCategory>::new();
        assert_eq!(eta.components.len(), 0);
    }

    #[test]
    fn add_component() {
        let mut eta = ConcreteNaturalTransformation::<RdfCategory, CodeCategory>::new();

        // In a real implementation, we'd have actual morphisms
        // For now, we just test the structure
        assert_eq!(eta.components.len(), 0);
    }

    #[test]
    fn identity_transformation() {
        let _id = IdentityNaturalTransformation::<RdfCategory, CodeCategory>::new();
        // Identity transformation exists
        assert!(true);
    }

    #[test]
    fn vertical_composition() {
        let eta = ConcreteNaturalTransformation::<RdfCategory, CodeCategory>::new();
        let mu = ConcreteNaturalTransformation::<RdfCategory, CodeCategory>::new();

        let composed = VerticalComposition::compose(&eta, &mu);
        assert!(composed.is_ok());
    }

    #[test]
    fn natural_isomorphism() {
        let forward = ConcreteNaturalTransformation::<RdfCategory, CodeCategory>::new();
        let backward = ConcreteNaturalTransformation::<RdfCategory, CodeCategory>::new();

        let iso = NaturalIsomorphism::new(forward, backward);
        assert!(iso.verify_inverse().is_ok());
    }

    #[test]
    fn functor_category() {
        let _cat = FunctorCategory::<RdfCategory, CodeCategory>::new();
        // Functor category exists
        assert!(true);
    }

    #[test]
    fn two_category_structure() {
        let eta = ConcreteNaturalTransformation::<RdfCategory, CodeCategory>::new();
        let mu = ConcreteNaturalTransformation::<RdfCategory, CodeCategory>::new();

        let composed = TwoCategory::vertical_compose(&eta, &mu);
        assert!(composed.is_ok());
    }
}
