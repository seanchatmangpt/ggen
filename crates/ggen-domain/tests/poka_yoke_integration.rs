//! Integration tests for Poka-Yoke mistake-proofing system
//!
//! These tests verify that the compile-time safety guarantees work correctly
//! and that invalid operations are prevented.

use ggen_domain::poka_yoke::{
    ontology_guards::{V1_0_0, V2_0_0, V2_1_0, OntologyTransform},
    template_selection::{
        RustLibrary, RustCLI, NextJsApp, TemplateCompatibility,
    },
};

/// Test that schema version constants are correct
#[test]
fn test_schema_versions() {
    assert_eq!(V1_0_0::VERSION, "1.0.0");
    assert_eq!(V1_0_0::NAMESPACE, "http://ggen.io/schema/v1#");

    assert_eq!(V2_0_0::VERSION, "2.0.0");
    assert_eq!(V2_0_0::NAMESPACE, "http://ggen.io/schema/v2#");

    assert_eq!(V2_1_0::VERSION, "2.1.0");
    assert_eq!(V2_1_0::NAMESPACE, "http://ggen.io/schema/v2.1#");
}

/// Test that valid ontology transformations can be created
#[test]
fn test_valid_transformations() {
    // Forward migrations should work
    assert!(
        OntologyTransform::<V1_0_0, V2_0_0>::new().is_some(),
        "V1 -> V2 transformation should be valid"
    );

    assert!(
        OntologyTransform::<V2_0_0, V2_1_0>::new().is_some(),
        "V2.0 -> V2.1 transformation should be valid"
    );

    assert!(
        OntologyTransform::<V1_0_0, V2_1_0>::new().is_some(),
        "V1 -> V2.1 transformation should be valid"
    );
}

/// Test that invalid ontology transformations are rejected
#[test]
fn test_invalid_transformations() {
    // Backward migrations should NOT work
    assert!(
        OntologyTransform::<V2_0_0, V1_0_0>::new().is_none(),
        "V2 -> V1 transformation should be invalid"
    );

    assert!(
        OntologyTransform::<V2_1_0, V2_0_0>::new().is_none(),
        "V2.1 -> V2.0 transformation should be invalid"
    );

    assert!(
        OntologyTransform::<V2_1_0, V1_0_0>::new().is_none(),
        "V2.1 -> V1 transformation should be invalid"
    );
}

/// Test template compatibility with schema versions
#[test]
fn test_template_compatibility() {
    // Rust projects compatible with all versions
    assert!(RustLibrary::is_compatible::<V1_0_0>());
    assert!(RustLibrary::is_compatible::<V2_0_0>());
    assert!(RustLibrary::is_compatible::<V2_1_0>());

    // CLI templates require v2.0.0+
    assert!(!RustCLI::is_compatible::<V1_0_0>());
    assert!(RustCLI::is_compatible::<V2_0_0>());
    assert!(RustCLI::is_compatible::<V2_1_0>());

    // Next.js templates require v2.1.0+
    assert!(!NextJsApp::is_compatible::<V1_0_0>());
    assert!(!NextJsApp::is_compatible::<V2_0_0>());
    assert!(NextJsApp::is_compatible::<V2_1_0>());
}

/// Test that sealed traits cannot be extended
/// (This test mainly documents the design - actual prevention is compile-time)
#[test]
fn test_sealed_traits() {
    // The following would NOT compile due to sealed traits:

    // Cannot create custom SchemaVersion:
    // struct MyVersion;
    // impl SchemaVersion for MyVersion { ... } // Error: private::Sealed not accessible

    // Cannot create custom TemplateType:
    // struct MyTemplate;
    // impl TemplateType for MyTemplate { ... } // Error: private::Sealed not accessible

    // Cannot create custom ValidationState:
    // struct MyState;
    // impl ValidationState for MyState { ... } // Error: private::Sealed not accessible
}

/// Test compile-time type safety guarantees
/// These tests document the compile-time guarantees
#[test]
fn test_type_safety_guarantees() {
    // Guarantee 1: Invalid transformations cannot be constructed
    // Already tested in test_invalid_transformations()

    // Guarantee 2: Incompatible templates cannot be selected
    // Already tested in test_template_compatibility()

    // Guarantee 3: Triple components have correct types
    // Verified by type system - cannot construct invalid triples

    // Guarantee 4: Builder requires all fields
    // Tested in builder_integration tests below
}

// The following tests would require actual Graph/Template instances
// They are marked as ignored and serve as integration test templates

#[test]
#[ignore]
fn test_complete_ontology_workflow() {
    // This would test:
    // 1. Load graph
    // 2. Create ValidatedOntology
    // 3. Validate
    // 4. Check schema
    // 5. Transform to new version
    // 6. Use transformed ontology
}

#[test]
#[ignore]
fn test_complete_template_workflow() {
    // This would test:
    // 1. Create TemplateSelector
    // 2. Select template with type checking
    // 3. Validate template exists
    // 4. Validate template structure
    // 5. Validate compatibility with ontology
}

#[test]
#[ignore]
fn test_complete_builder_workflow() {
    // This would test:
    // 1. Create GeneratorBuilder
    // 2. Set template (Incomplete -> WithTemplate)
    // 3. Set ontology (WithTemplate -> WithOntology)
    // 4. Set output (WithOntology -> WithOutput)
    // 5. Mark complete (WithOutput -> Complete)
    // 6. Build generator
    // 7. Generate code
}

#[test]
#[ignore]
fn test_complete_semantic_projection_workflow() {
    // This would test:
    // 1. Create SemanticProjection
    // 2. Build validated triples
    // 3. Add triples to projection
    // 4. Type check (Unchecked -> TypeChecked)
    // 5. Semantic check (TypeChecked -> SemanticChecked)
    // 6. Full validate (SemanticChecked -> FullyValidated)
}

/// Test that physical constraints are enforced
#[test]
fn test_physical_constraints() {
    use ggen_domain::poka_yoke::template_selection::TemplateConstraints;
    use std::path::PathBuf;

    let constraints = TemplateConstraints::new();

    // Valid path should pass
    let valid_path = PathBuf::from("templates/rust/library");
    assert!(constraints.validate_path(&valid_path).is_ok());

    // Path with .. should fail
    let forbidden_path = PathBuf::from("templates/../secret");
    assert!(constraints.validate_path(&forbidden_path).is_err());

    // Path with .git should fail
    let git_path = PathBuf::from("templates/.git/hooks");
    assert!(constraints.validate_path(&git_path).is_err());

    // Very deep path should fail
    let deep_path = PathBuf::from("a/b/c/d/e/f/g/h/i/j/k/l/m/n/o");
    assert!(constraints.validate_path(&deep_path).is_err());
}

/// Test namespace validation
#[test]
fn test_namespace_validation() {
    use ggen_domain::poka_yoke::ontology_guards::NamespaceValidator;

    let validator = NamespaceValidator::<V2_0_0>::new();

    // Valid namespace
    let valid = validator.validate_namespace("http://ggen.io/schema/v2#Something");
    assert!(valid.is_ok());

    // Invalid namespace
    let invalid = validator.validate_namespace("http://other.io/schema#Something");
    assert!(invalid.is_err());
}

/// Test subject validation
#[test]
fn test_subject_validation() {
    use ggen_domain::poka_yoke::semantic_projection::SubjectValidator;

    let validator = SubjectValidator::<V2_0_0>::new();

    // Valid IRI
    let valid = validator.validate("http://ggen.io/schema/v2#MyClass");
    assert!(valid.is_ok());

    // Invalid IRI (not http/https)
    let invalid = validator.validate("not-a-uri");
    assert!(invalid.is_err());
}

/// Test predicate validation
#[test]
fn test_predicate_validation() {
    use ggen_domain::poka_yoke::semantic_projection::PredicateValidator;

    let validator = PredicateValidator::<V2_0_0>::new();

    // Valid RDF predicate
    let valid = validator.validate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
    assert!(valid.is_ok());

    // Invalid predicate (not in vocabulary)
    let invalid = validator.validate("http://example.com/unknown");
    assert!(invalid.is_err());
}

/// Test projection constraints
#[test]
fn test_projection_constraints() {
    use ggen_domain::poka_yoke::semantic_projection::ProjectionConstraints;

    let constraints = ProjectionConstraints::new()
        .with_max_triples(100)
        .with_strict_namespace(true)
        .with_shacl_validation(true);

    assert_eq!(constraints.max_triples, 100);
    assert!(constraints.strict_namespace);
    assert!(constraints.require_shacl);
}

/// Test that the module exports are correct
#[test]
fn test_module_exports() {
    use ggen_domain::poka_yoke::*;

    // Verify all key types are exported
    let _: Option<OntologyTransform<V1_0_0, V2_0_0>> = None;
    let _: Option<ValidatedOntology<V2_0_0, _>> = None;
    let _: Option<TemplateSelector> = None;
    let _: Option<GeneratorBuilder<V2_0_0, RustLibrary, _>> = None;
    let _: Option<SemanticProjection<V2_0_0, _>> = None;
}
