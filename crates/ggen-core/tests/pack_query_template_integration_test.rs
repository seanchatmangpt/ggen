//! Pack Query and Template Integration Tests (μ₂/μ₃)
//!
//! This test verifies that:
//! 1. μ₂ loads and executes pack-contributed SPARQL CONSTRUCT queries
//! 2. μ₃ loads and renders pack-contributed Tera templates
//! 3. Generated files are recorded in the receipt with pack provenance
//! 4. Pack queries execute against merged ontology (project + packs)
//! 5. Templates render with bindings from pack queries
//!
//! See `docs/marketplace/PACK_QUERY_CONTRACT.md` for the pack contract.

use ggen_core::graph::Graph;
use ggen_core::pack_resolver::{PackResolver, ResolvedPacks};
use ggen_core::v6::passes::{EmissionPass, ExtractionPass};
use ggen_core::v6::pipeline::PipelineConfig;
use ggen_core::v6::pipeline::StagedPipeline;
use tempfile::TempDir;

/// Helper: Create a test project with pack integration
fn create_test_project_with_pack() -> tempfile::TempDir {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path();

    // Create .ggen directory
    let ggen_dir = project_dir.join(".ggen");
    std::fs::create_dir_all(&ggen_dir).unwrap();

    // Copy lockfile with test-pack-integration
    let lockfile_content = r#"{
      "packs": {
        "test-pack-integration": {
          "version": "0.1.0",
          "source": {
            "type": "Local",
            "path": "/Users/sac/.ggen/packs/test-pack-integration"
          },
          "installed_at": "2026-04-01T06:00:00.000000Z",
          "dependencies": []
        }
      },
      "updated_at": "2026-04-01T06:00:00.000000Z",
      "ggen_version": "0.2.0"
    }"#;

    std::fs::write(ggen_dir.join("packs.lock"), lockfile_content).unwrap();

    // Create ontology with entities that match the pack query
    let ontology_dir = project_dir.join("ontology");
    std::fs::create_dir_all(&ontology_dir).unwrap();

    // Add a test entity that the pack query will extract
    let ontology_content = r#"
        @prefix pack: <http://ggen.dev/pack/test-pack-integration#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        pack:UserEntity a pack:TestEntity ;
            rdfs:label "User" ;
            pack:testField "username" .

        pack:OrderEntity a pack:TestEntity ;
            rdfs:label "Order" ;
            pack:testField "order_id" .
    "#;

    std::fs::write(ontology_dir.join("domain.ttl"), ontology_content).unwrap();

    temp_dir
}

#[test]
fn test_pack_query_loading() {
    // This test verifies that pack queries are loaded from the pack cache
    let temp_dir = create_test_project_with_pack();

    // Create pack resolver
    let resolver = PackResolver::new(temp_dir.path()).unwrap();

    // Resolve packs
    let resolved = resolver.resolve().unwrap();

    // Verify pack queries were loaded
    assert!(
        !resolved.queries.is_empty(),
        "Pack queries should be loaded"
    );

    // Verify the test-pack-integration query
    let test_query = resolved
        .queries
        .iter()
        .find(|q| q.name.contains("extract-test-entities"));

    assert!(
        test_query.is_some(),
        "Pack should contribute extract-test-entities query"
    );

    let query = test_query.unwrap();
    assert!(
        query.sparql.contains("CONSTRUCT"),
        "Pack query must be CONSTRUCT (see PACK_QUERY_CONTRACT.md)"
    );
}

#[test]
fn test_pack_template_loading() {
    let temp_dir = create_test_project_with_pack();

    // Create pack resolver
    let resolver = PackResolver::new(temp_dir.path()).unwrap();

    // Resolve packs
    let resolved = resolver.resolve().unwrap();

    // Verify pack templates were loaded
    assert!(
        !resolved.templates.is_empty(),
        "Pack templates should be loaded"
    );

    // Verify the test-pack-integration template
    let test_template = resolved
        .templates
        .iter()
        .find(|t| t.path.display().to_string().contains("test_entity.rs.tera"));

    assert!(
        test_template.is_some(),
        "Pack should contribute test_entity.rs.tera template"
    );

    let template = test_template.unwrap();
    assert!(
        !template.content.is_empty(),
        "Template content should not be empty"
    );
    assert!(
        template.content.contains("pub struct"),
        "Template should contain Rust struct definition"
    );
}

#[test]
fn test_extraction_pass_extends_with_pack_queries() {
    let temp_dir = create_test_project_with_pack();

    // Create pack resolver
    let resolver = PackResolver::new(temp_dir.path()).unwrap();
    let resolved = resolver.resolve().unwrap();

    // Create extraction pass with standard rules
    let mut extraction = ExtractionPass::with_standard_rules();

    // Get initial query count
    let initial_count = extraction.tensor_queries.len();

    // Extend with pack queries
    extraction
        .extend_with_pack_construct_queries(&resolved.queries)
        .expect("Pack queries should be valid CONSTRUCT queries");

    // Verify queries were added
    assert!(
        extraction.tensor_queries.len() > initial_count,
        "Pack queries should be added to extraction pass"
    );

    // Verify pack query is present
    let pack_query = extraction
        .tensor_queries
        .iter()
        .find(|q| q.name.contains("pack::") && q.name.contains("extract-test-entities"));

    assert!(
        pack_query.is_some(),
        "Pack query should be added with 'pack::' prefix"
    );
}

#[test]
fn test_emission_pass_extends_with_pack_templates() {
    let temp_dir = create_test_project_with_pack();

    // Create pack resolver
    let resolver = PackResolver::new(temp_dir.path()).unwrap();
    let resolved = resolver.resolve().unwrap();

    // Create emission pass
    let mut emission = EmissionPass::new();

    // Get initial rule count
    let initial_count = emission.rules.len();

    // Extend with pack templates
    emission
        .extend_with_pack_templates(&resolved.templates)
        .expect("Pack templates should be valid");

    // Verify templates were added as rules
    assert!(
        emission.rules.len() > initial_count,
        "Pack templates should be added as emission rules"
    );

    // Verify pack template rule is present
    let pack_rule = emission
        .rules
        .iter()
        .find(|r| r.name.contains("pack:") && r.name.contains("test_entity"));

    assert!(
        pack_rule.is_some(),
        "Pack template should be added as emission rule with 'pack:' prefix"
    );

    let rule = pack_rule.unwrap();
    assert!(
        rule.inline_template.is_some(),
        "Pack template content should be stored inline"
    );
    assert!(
        rule.inline_template
            .as_ref()
            .unwrap()
            .contains("pub struct"),
        "Inline template should contain Rust struct"
    );
}

#[test]
fn test_pipeline_with_pack_queries_and_templates() {
    let temp_dir = create_test_project_with_pack();

    // Create pipeline config
    let config = PipelineConfig::new("test-project", "0.1.0")
        .with_base_path(temp_dir.path())
        .with_ontology("ontology/domain.ttl")
        .with_output_dir("output")
        .with_receipt_path(".ggen/receipt.json");

    // Run pipeline
    let mut pipeline = StagedPipeline::new(config).unwrap();
    let receipt = pipeline.run().unwrap();

    // Verify pipeline completed successfully
    assert!(receipt.is_valid, "Pipeline should complete successfully");

    // Verify pack provenance in receipt
    assert!(
        !receipt.packs.is_empty(),
        "Receipt should contain pack provenance"
    );

    // Find test-pack-integration in receipt
    let pack_provenance = receipt
        .packs
        .iter()
        .find(|p| p.pack_id == "test-pack-integration");

    assert!(
        pack_provenance.is_some(),
        "Receipt should record test-pack-integration pack"
    );

    let provenance = pack_provenance.unwrap();
    assert!(
        !provenance.queries_contributed.is_empty(),
        "Pack should record contributed queries"
    );
    assert!(
        !provenance.templates_contributed.is_empty(),
        "Pack should record contributed templates"
    );
    assert!(
        !provenance.digest.is_empty(),
        "Pack should have digest for queries+templates"
    );

    // Verify generated files exist
    let output_dir = temp_dir.path().join("output");

    // The pack template should generate files for each TestEntity
    // Our ontology has UserEntity and OrderEntity, so we expect 2 files
    let generated_files: Vec<_> = std::fs::read_dir(&output_dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .collect();

    assert!(
        !generated_files.is_empty(),
        "Output directory should contain generated files"
    );

    // Verify at least one file contains pack-generated content
    let has_pack_generated = generated_files.iter().any(|path| {
        let content = std::fs::read_to_string(path).unwrap_or_default();
        content.contains("Auto-generated by test-pack-integration")
    });

    assert!(
        has_pack_generated,
        "At least one file should be generated from pack template"
    );
}

#[test]
fn test_pack_query_rejects_select() {
    let temp_dir = create_test_project_with_pack();

    // Manually create an invalid SELECT query
    let invalid_query = ggen_core::pack_resolver::SparqlQuery {
        name: "test-pack-integration::invalid-select".to_string(),
        sparql: "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
    };

    // Create extraction pass
    let mut extraction = ExtractionPass::new();

    // Try to extend with invalid query
    let result = extraction.extend_with_pack_construct_queries(&[invalid_query]);

    // Should fail with clear error
    assert!(
        result.is_err(),
        "SELECT queries should be rejected in μ₂ extraction"
    );

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("not CONSTRUCT"),
        "Error should reference PACK_QUERY_CONTRACT.md"
    );
}

#[test]
fn test_pack_provenance_digest() {
    let temp_dir = create_test_project_with_pack();

    // Create pack resolver
    let resolver = PackResolver::new(temp_dir.path()).unwrap();
    let resolved = resolver.resolve().unwrap();

    // Get digest for test-pack-integration
    use ggen_marketplace::atomic::AtomicPackId;
    let pack_id = AtomicPackId::from_str("test-pack-integration").unwrap();

    let digest = resolved.digest_for_pack(&pack_id);

    assert!(!digest.is_empty(), "Digest should not be empty");
    assert!(digest.starts_with("sha256:"), "Digest should use SHA-256");
    assert!(digest.len() > "sha256:".len(), "Digest should contain hash");
}

#[test]
fn test_pack_query_and_template_names() {
    let temp_dir = create_test_project_with_pack();

    // Create pack resolver
    let resolver = PackResolver::new(temp_dir.path()).unwrap();
    let resolved = resolver.resolve().unwrap();

    use ggen_marketplace::atomic::AtomicPackId;
    let pack_id = AtomicPackId::from_str("test-pack-integration").unwrap();

    // Get query names
    let query_names = resolved.query_names_for_pack(&pack_id);
    assert!(
        !query_names.is_empty(),
        "Pack should have contributed queries"
    );
    assert!(
        query_names
            .iter()
            .any(|n| n.contains("extract-test-entities")),
        "Query names should include extract-test-entities"
    );

    // Get template paths
    let template_paths = resolved.template_paths_for_pack(&pack_id);
    assert!(
        !template_paths.is_empty(),
        "Pack should have contributed templates"
    );
    assert!(
        template_paths
            .iter()
            .any(|p| p.contains("test_entity.rs.tera")),
        "Template paths should include test_entity.rs.tera"
    );
}
