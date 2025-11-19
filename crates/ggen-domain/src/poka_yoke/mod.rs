//! # Poka-Yoke Mistake-Proofing for Code Generation
//!
//! This module implements manufacturing-grade mistake-proofing principles
//! for code generation systems. Using compile-time type system guardrails,
//! it makes invalid states unrepresentable and errors impossible.
//!
//! ## Design Principles
//!
//! 1. **Physical Constraints**: Use the type system to prevent mistakes physically
//! 2. **Compile-Time Validation**: Catch errors at compile time, not runtime
//! 3. **Type-State Pattern**: Track state in the type system
//! 4. **Phantom Types**: Encode semantic information at the type level
//! 5. **Sealed Traits**: Prevent unsafe extensions
//!
//! ## Modules
//!
//! - `ontology_guards`: Compile-time type guards for ontology transformations
//! - `template_selection`: Fool-proof template selection with physical constraints
//! - `api_builder`: Error-impossible API design using type-state pattern
//! - `semantic_projection`: Zero-defect semantic projections with validation

pub mod ontology_guards;
pub mod template_selection;
pub mod api_builder;
pub mod semantic_projection;
pub mod examples;

// Re-export key types for convenience
pub use ontology_guards::{
    OntologyTransform, ValidatedOntology, SchemaVersion,
    NamespaceValidator, TransformationGuard,
};
pub use template_selection::{
    TemplateSelector, ValidatedTemplate, TemplateCompatibility,
    TemplateSelectionBuilder, TemplateConstraints,
};
pub use api_builder::{
    GeneratorBuilder, Incomplete, Complete, WithTemplate,
    WithOntology, WithOutput, BuilderState,
};
pub use semantic_projection::{
    SemanticProjection, ValidatedSemantics, ProjectionConstraints,
    TripleValidator, SchemaEnforcer,
};
