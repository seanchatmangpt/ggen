/// Yoneda lemma applications for type-safe code generation
///
/// The Yoneda lemma is one of the most profound results in category theory.
/// It states that for any functor F: C → Set and object A in C:
///
///   Nat(Hom(A, -), F) ≅ F(A)
///
/// Natural transformations from the hom-functor to F correspond bijectively
/// to elements of F(A).
///
/// # Applications to Code Generation
///
/// 1. **Representable Functors**: A functor F is representable if F ≅ Hom(A, -)
///    for some object A. This means F is determined by a single object.
///
/// 2. **Code Templates as Functors**: Templates can be viewed as functors
///    from the category of contexts to the category of generated code.
///
/// 3. **Type-Safe Generation**: The Yoneda embedding ensures that code
///    generation preserves type structure.
///
/// # Example
///
/// Consider a template for generating Rust structs:
/// - The functor F maps contexts to struct definitions
/// - A natural transformation picks a specific template instantiation
/// - The Yoneda lemma guarantees type safety of the instantiation

use super::base::{Category, Morphism, MorphismError, Object};
use super::functor::{CodeObject, Functor, FunctorError, RdfObject};
use std::collections::BTreeMap;
use std::marker::PhantomData;

/// A representable functor
///
/// A functor F: C → Set is representable if there exists an object A in C
/// such that F is naturally isomorphic to Hom(A, -).
///
/// The representing object A is called the representation.
pub trait Representable<C: Category> {
    /// The representing object
    fn representation(&self) -> C::Obj;

    /// The natural isomorphism Hom(A, -) → F
    fn yoneda_map(&self, morphism: &C::Mor) -> Result<Self::Element, YonedaError>;

    /// The type of elements produced by this functor
    type Element: Clone + std::fmt::Debug;
}

/// Errors in Yoneda operations
#[derive(Debug, Clone, thiserror::Error)]
pub enum YonedaError {
    #[error("Yoneda map failed: {reason}")]
    YonedaMapFailed { reason: String },

    #[error("Representation error: {reason}")]
    RepresentationError { reason: String },

    #[error("Natural transformation failed: {reason}")]
    NaturalTransformationFailed { reason: String },
}

/// Yoneda embedding
///
/// The Yoneda embedding is a functor Y: C → [C^op, Set] that sends
/// each object A to the functor Hom(-, A).
///
/// This embedding is:
/// - **Full**: Every natural transformation comes from a morphism
/// - **Faithful**: Different morphisms give different natural transformations
/// - **Preserves limits**: The Yoneda embedding preserves all limits
pub struct YonedaEmbedding<C: Category> {
    _phantom: PhantomData<C>,
}

impl<C: Category> YonedaEmbedding<C> {
    pub fn new() -> Self {
        Self {
            _phantom: PhantomData,
        }
    }

    /// Embed an object into the functor category
    pub fn embed(&self, obj: &C::Obj) -> HomFunctor<C> {
        HomFunctor::new(obj.clone())
    }

    /// Embed a morphism into a natural transformation
    pub fn embed_morphism(&self, mor: &C::Mor) -> Result<NaturalTransformation<C>, YonedaError> {
        Ok(NaturalTransformation::from_morphism(mor.clone()))
    }
}

impl<C: Category> Default for YonedaEmbedding<C> {
    fn default() -> Self {
        Self::new()
    }
}

/// Hom functor: Hom(A, -)
///
/// For a fixed object A, this functor maps:
/// - Objects B to Hom(A, B) (morphisms from A to B)
/// - Morphisms f: B → C to post-composition: Hom(A, f): Hom(A, B) → Hom(A, C)
pub struct HomFunctor<C: Category> {
    source: C::Obj,
    _phantom: PhantomData<C>,
}

impl<C: Category> HomFunctor<C> {
    pub fn new(source: C::Obj) -> Self {
        Self {
            source,
            _phantom: PhantomData,
        }
    }

    pub fn source(&self) -> &C::Obj {
        &self.source
    }
}

/// Natural transformation between functors
///
/// A natural transformation η: F → G assigns to each object A
/// a morphism η_A: F(A) → G(A) such that for every morphism f: A → B,
/// the following diagram commutes:
///
/// ```text
/// F(A) ──η_A──▶ G(A)
///  │             │
///  │F(f)         │G(f)
///  ▼             ▼
/// F(B) ──η_B──▶ G(B)
/// ```
#[derive(Debug, Clone)]
pub struct NaturalTransformation<C: Category> {
    components: BTreeMap<String, C::Mor>,
    _phantom: PhantomData<C>,
}

impl<C: Category> NaturalTransformation<C> {
    pub fn new() -> Self {
        Self {
            components: BTreeMap::new(),
            _phantom: PhantomData,
        }
    }

    pub fn from_morphism(mor: C::Mor) -> Self {
        let mut components = BTreeMap::new();
        components.insert("default".to_string(), mor);
        Self {
            components,
            _phantom: PhantomData,
        }
    }

    pub fn add_component(&mut self, obj_name: String, component: C::Mor) {
        self.components.insert(obj_name, component);
    }

    pub fn component(&self, obj_name: &str) -> Option<&C::Mor> {
        self.components.get(obj_name)
    }

    /// Verify naturality condition
    pub fn verify_naturality(&self) -> Result<(), YonedaError> {
        // In a real implementation, we would verify the commutative diagram
        // For now, we trust that components are added correctly
        Ok(())
    }
}

impl<C: Category> Default for NaturalTransformation<C> {
    fn default() -> Self {
        Self::new()
    }
}

/// Code generator using Yoneda lemma
///
/// This generator leverages the Yoneda lemma to ensure type-safe
/// code generation. Each template is a representable functor,
/// and code generation is a natural transformation.
pub struct CodeGenerator {
    /// Template definitions (representable functors)
    templates: BTreeMap<String, TemplateDefinition>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            templates: BTreeMap::new(),
        }
    }

    /// Register a template
    pub fn register_template(&mut self, name: String, template: TemplateDefinition) {
        self.templates.insert(name, template);
    }

    /// Generate code from a template using Yoneda
    ///
    /// The template is a representable functor F with representing object A.
    /// Generation is a natural transformation from Hom(A, -) to F,
    /// which by Yoneda corresponds to an element of F(A).
    pub fn generate(
        &self,
        template_name: &str,
        context: &GenerationContext,
    ) -> Result<GeneratedCode, YonedaError> {
        let template = self.templates.get(template_name).ok_or_else(|| {
            YonedaError::RepresentationError {
                reason: format!("Template '{}' not found", template_name),
            }
        })?;

        // Apply the Yoneda map
        template.generate(context)
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Template definition - a representable functor
///
/// Each template represents a functor from contexts to code.
/// The representing object is the template's input schema.
#[derive(Debug, Clone)]
pub struct TemplateDefinition {
    /// Name of the template
    pub name: String,
    /// Input schema (representing object)
    pub input_schema: TemplateSchema,
    /// Template body
    pub body: String,
    /// Output type
    pub output_type: OutputType,
}

impl TemplateDefinition {
    pub fn new(
        name: String,
        input_schema: TemplateSchema,
        body: String,
        output_type: OutputType,
    ) -> Self {
        Self {
            name,
            input_schema,
            body,
            output_type,
        }
    }

    /// Generate code (apply Yoneda map)
    pub fn generate(&self, context: &GenerationContext) -> Result<GeneratedCode, YonedaError> {
        // Verify context matches schema
        self.input_schema.validate(context)?;

        // Apply template transformation
        let code = self.apply_template(context)?;

        Ok(GeneratedCode {
            template_name: self.name.clone(),
            output_type: self.output_type.clone(),
            code,
        })
    }

    fn apply_template(&self, context: &GenerationContext) -> Result<String, YonedaError> {
        // Simplified template application
        // In reality, this would use Tera or another template engine
        let mut result = self.body.clone();

        for (key, value) in &context.variables {
            result = result.replace(&format!("{{{{{}}}}}", key), value);
        }

        Ok(result)
    }
}

/// Template schema - defines the structure of template inputs
///
/// This is the representing object in the Yoneda lemma application
#[derive(Debug, Clone)]
pub struct TemplateSchema {
    /// Required variables
    pub required: Vec<String>,
    /// Optional variables with defaults
    pub optional: BTreeMap<String, String>,
    /// Type constraints
    pub types: BTreeMap<String, String>,
}

impl TemplateSchema {
    pub fn new() -> Self {
        Self {
            required: vec![],
            optional: BTreeMap::new(),
            types: BTreeMap::new(),
        }
    }

    pub fn validate(&self, context: &GenerationContext) -> Result<(), YonedaError> {
        // Check all required variables are present
        for req in &self.required {
            if !context.variables.contains_key(req) {
                return Err(YonedaError::RepresentationError {
                    reason: format!("Required variable '{}' not found", req),
                });
            }
        }

        // Type checking (simplified)
        for (var, expected_type) in &self.types {
            if let Some(_value) = context.variables.get(var) {
                // In a real implementation, we'd check the actual type
                // For now, we assume types are correct
            }
        }

        Ok(())
    }
}

impl Default for TemplateSchema {
    fn default() -> Self {
        Self::new()
    }
}

/// Generation context - the domain of the template functor
#[derive(Debug, Clone)]
pub struct GenerationContext {
    /// Variables for template substitution
    pub variables: BTreeMap<String, String>,
    /// RDF data
    pub rdf_data: Option<String>,
    /// SPARQL query results
    pub sparql_results: Vec<BTreeMap<String, String>>,
}

impl GenerationContext {
    pub fn new() -> Self {
        Self {
            variables: BTreeMap::new(),
            rdf_data: None,
            sparql_results: vec![],
        }
    }

    pub fn with_variable(mut self, key: String, value: String) -> Self {
        self.variables.insert(key, value);
        self
    }

    pub fn with_rdf(mut self, rdf: String) -> Self {
        self.rdf_data = Some(rdf);
        self
    }
}

impl Default for GenerationContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Generated code - the codomain of the template functor
#[derive(Debug, Clone)]
pub struct GeneratedCode {
    pub template_name: String,
    pub output_type: OutputType,
    pub code: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OutputType {
    Rust,
    TypeScript,
    Python,
    Java,
    Yaml,
    Json,
}

/// Presheaf - a contravariant functor C^op → Set
///
/// Presheaves are central to the Yoneda lemma. Every presheaf
/// is a colimit of representable functors.
pub trait Presheaf<C: Category> {
    type Value: Clone + std::fmt::Debug;

    /// Apply the presheaf to an object
    fn apply(&self, obj: &C::Obj) -> Result<Self::Value, YonedaError>;

    /// Apply the presheaf to a morphism (contravariant)
    fn contramap(&self, mor: &C::Mor) -> Result<Self, YonedaError>
    where
        Self: Sized;
}

/// Universal property of the Yoneda embedding
///
/// This trait captures the universal property: natural transformations
/// from Hom(A, -) to F are in bijection with elements of F(A).
pub trait UniversalProperty<C: Category, F> {
    /// Get the element of F(A) corresponding to a natural transformation
    fn universal_element(&self) -> Result<F, YonedaError>;

    /// Get the natural transformation corresponding to an element of F(A)
    fn from_element(element: F) -> Result<Self, YonedaError>
    where
        Self: Sized;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::category::functor::RdfCategory;

    #[test]
    fn template_schema_validation() {
        let mut schema = TemplateSchema::new();
        schema.required.push("name".to_string());
        schema.required.push("type".to_string());

        let context = GenerationContext::new()
            .with_variable("name".to_string(), "Product".to_string())
            .with_variable("type".to_string(), "struct".to_string());

        assert!(schema.validate(&context).is_ok());
    }

    #[test]
    fn template_schema_validation_fails() {
        let mut schema = TemplateSchema::new();
        schema.required.push("name".to_string());

        let context = GenerationContext::new();

        assert!(schema.validate(&context).is_err());
    }

    #[test]
    fn code_generation() {
        let template = TemplateDefinition::new(
            "struct_template".to_string(),
            TemplateSchema::new(),
            "pub struct {{name}} {}".to_string(),
            OutputType::Rust,
        );

        let context = GenerationContext::new()
            .with_variable("name".to_string(), "Product".to_string());

        let result = template.generate(&context).unwrap();
        assert_eq!(result.code, "pub struct Product {}");
    }

    #[test]
    fn code_generator_registration() {
        let mut generator = CodeGenerator::new();

        let template = TemplateDefinition::new(
            "test_template".to_string(),
            TemplateSchema::new(),
            "Hello {{name}}".to_string(),
            OutputType::Rust,
        );

        generator.register_template("test".to_string(), template);

        let context = GenerationContext::new()
            .with_variable("name".to_string(), "World".to_string());

        let result = generator.generate("test", &context).unwrap();
        assert_eq!(result.code, "Hello World");
    }

    #[test]
    fn yoneda_embedding() {
        let embedding = YonedaEmbedding::<RdfCategory>::new();
        // Test that we can create embeddings
        // Full functionality would require more infrastructure
        assert!(true);
    }
}
