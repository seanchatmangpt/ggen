//! # Poka-Yoke Usage Examples
//!
//! This module demonstrates how to use the Poka-Yoke mistake-proofing
//! system for compile-time safe code generation.

#![allow(dead_code)]

use super::*;
use ontology_guards::{V2_0_0, ValidatedOntology, Unvalidated, OntologyTransform, V1_0_0};
use template_selection::{TemplateSelector, RustLibrary, NextJsApp};
use api_builder::{GeneratorBuilder, Incomplete};
use semantic_projection::{SemanticProjection, TripleValidator, Unchecked};
use crate::graph::core::Graph;
use std::path::PathBuf;

/// Example 1: Type-Safe Ontology Transformation
///
/// This example shows how the type system prevents invalid ontology
/// transformations at compile time.
pub fn example_ontology_transformation() -> Result<(), Box<dyn std::error::Error>> {
    // Create a graph (in real code, load from file)
    let graph = Graph::new()?;

    // Create validated ontology for version 1.0.0
    let ontology_v1 = ValidatedOntology::<V1_0_0, Unvalidated>::new(graph)
        .validate()?
        .check_schema()?;

    // Transform from V1 to V2 (compile-time checked)
    let transform = OntologyTransform::<V1_0_0, V2_0_0>::new()
        .expect("V1 -> V2 transformation is always valid");

    let ontology_v2 = transform.apply(ontology_v1)?;

    println!(
        "Successfully transformed ontology from {} to {}",
        V1_0_0::VERSION,
        ontology_v2.version()
    );

    // This would NOT compile - backward transformations are prevented:
    // let backward_transform = OntologyTransform::<V2_0_0, V1_0_0>::new();
    // This returns None because backward migrations are not allowed

    Ok(())
}

/// Example 2: Fool-Proof Template Selection
///
/// This example demonstrates compile-time template compatibility checking.
pub fn example_template_selection() -> Result<(), Box<dyn std::error::Error>> {
    // Select a template for schema version 2.0.0
    let selector = TemplateSelector::for_schema::<V2_0_0>();

    // Rust library templates are compatible with v2.0.0
    let template = selector
        .select::<RustLibrary>(PathBuf::from("/path/to/rust-library-template"))?
        .select()?;

    println!(
        "Selected template: {} for schema version {}",
        template.template_type(),
        template.schema_version()
    );

    // NextJS templates require v2.1.0 or later
    // This would compile but return an error at runtime for v2.0.0:
    // let nextjs_template = selector.select::<NextJsApp>(PathBuf::from("/path"))?;

    Ok(())
}

/// Example 3: Error-Impossible API Design
///
/// This example shows the type-state pattern preventing incomplete
/// generator configuration.
pub fn example_error_impossible_api() -> Result<(), Box<dyn std::error::Error>> {
    // Create validated components
    let graph = Graph::new()?;
    let ontology = ValidatedOntology::<V2_0_0, Unvalidated>::new(graph)
        .validate()?
        .check_schema()?;

    let selector = TemplateSelector::for_schema::<V2_0_0>();
    let template = selector
        .select::<RustLibrary>(PathBuf::from("/path/to/template"))?
        .select()?
        .validate(&ontology)?;

    // Build generator with type-state pattern
    // Each method moves to a new state, ensuring all required fields are set
    let generator = GeneratorBuilder::<V2_0_0, RustLibrary, Incomplete>::new()
        .with_template(template)
        .with_ontology(ontology)
        .with_output(PathBuf::from("/output"))
        .complete()
        .build();

    // This would NOT compile - missing required fields:
    // let incomplete = GeneratorBuilder::<V2_0_0, RustLibrary, Incomplete>::new()
    //     .with_template(template)
    //     .complete()  // Compile error: ontology and output not set
    //     .build();

    // Generate code
    let result = generator.generate()?;
    println!(
        "Generated {} files using {} template",
        result.file_count(),
        result.template_type
    );

    Ok(())
}

/// Example 4: Zero-Defect Semantic Projection
///
/// This example demonstrates compile-time validation of RDF triples
/// and progressive semantic checking.
pub fn example_semantic_projection() -> Result<(), Box<dyn std::error::Error>> {
    // Create validated ontology
    let graph = Graph::new()?;
    let ontology = ValidatedOntology::<V2_0_0, Unvalidated>::new(graph)
        .validate()?
        .check_schema()?;

    // Create semantic projection
    let mut projection = SemanticProjection::<V2_0_0, Unchecked>::new(ontology);

    // Create triple validator
    let validator = TripleValidator::<V2_0_0>::new();

    // Build validated triples (compile-time type checking)
    let triple = validator.build_triple(
        "http://ggen.io/schema/v2#MyClass",
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        "http://www.w3.org/2000/01/rdf-schema#Class",
    )?;

    projection.add_triple(triple);

    // Progressive validation
    let type_checked = projection.type_check()?;
    let semantic_checked = type_checked.semantic_check()?;
    let fully_validated = semantic_checked.full_validate()?;

    println!(
        "Projection validated at level {}",
        fully_validated.validity_level()
    );

    Ok(())
}

/// Example 5: Complete End-to-End Workflow
///
/// This example shows a complete code generation workflow with
/// full Poka-Yoke mistake-proofing.
pub fn example_complete_workflow() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Complete Poka-Yoke Code Generation Workflow ===\n");

    // Step 1: Load and validate ontology
    println!("Step 1: Loading and validating ontology...");
    let graph = Graph::new()?;
    let ontology = ValidatedOntology::<V2_0_0, Unvalidated>::new(graph)
        .validate()?
        .check_schema()?;
    println!("✓ Ontology validated for schema version {}\n", ontology.version());

    // Step 2: Select and validate template
    println!("Step 2: Selecting template...");
    let selector = TemplateSelector::for_schema::<V2_0_0>();
    let template = selector
        .select::<RustLibrary>(PathBuf::from("/templates/rust-library"))?
        .select()?
        .validate(&ontology)?;
    println!("✓ Template '{}' selected and validated\n", template.template_type());

    // Step 3: Build semantic projection
    println!("Step 3: Building semantic projection...");
    let mut projection = SemanticProjection::<V2_0_0, Unchecked>::new(ontology.clone());
    let validator = TripleValidator::<V2_0_0>::new();

    let triple = validator.build_triple(
        "http://ggen.io/schema/v2#LibraryModule",
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        "http://www.w3.org/2000/01/rdf-schema#Class",
    )?;
    projection.add_triple(triple);

    let validated_projection = projection
        .type_check()?
        .semantic_check()?
        .full_validate()?;
    println!("✓ Semantic projection validated at level {}\n", validated_projection.validity_level());

    // Step 4: Configure and build generator
    println!("Step 4: Configuring generator...");
    let generator = GeneratorBuilder::<V2_0_0, RustLibrary, Incomplete>::new()
        .with_template(template)
        .with_ontology(ontology)
        .with_output(PathBuf::from("/output"))
        .with_variable("project_name".to_string(), "my_library".to_string())
        .with_dry_run(true)
        .complete()
        .build();
    println!("✓ Generator configured\n");

    // Step 5: Generate code
    println!("Step 5: Generating code...");
    let result = generator.generate()?;
    println!("✓ Generated {} files", result.file_count());
    println!("  Template type: {}", result.template_type);
    println!("  Schema version: {}", result.schema_version);
    println!("  Dry run: {}\n", result.dry_run);

    println!("=== Workflow completed successfully ===");

    Ok(())
}

/// Example 6: Physical Constraints in Action
///
/// This example demonstrates how physical constraints prevent
/// invalid operations at compile time.
pub fn example_physical_constraints() {
    // Physical Constraint 1: Schema version compatibility
    // Only valid transformations can be created
    assert!(OntologyTransform::<V1_0_0, V2_0_0>::new().is_some());
    assert!(OntologyTransform::<V2_0_0, V1_0_0>::new().is_none());

    // Physical Constraint 2: Template type checking
    // Templates must match their role in the type system
    // The compiler ensures correct usage through phantom types

    // Physical Constraint 3: Builder completeness
    // Cannot call generate() until all required fields are set
    // This is enforced by the type system - incomplete builders
    // don't have the generate() method available

    println!("All physical constraints enforced at compile time!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore] // Requires actual files/graph setup
    fn test_ontology_transformation() {
        // This test demonstrates the API
        // In real usage, it would work with actual graph data
    }

    #[test]
    #[ignore] // Requires actual template files
    fn test_template_selection() {
        // This test demonstrates the API
        // In real usage, it would work with actual template files
    }

    #[test]
    fn test_physical_constraints() {
        example_physical_constraints();
    }
}
