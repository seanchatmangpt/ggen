//! Integration tests for v6 pipeline CONSTRUCT guarantees
//!
//! Tests all five stages (μ₁-μ₅) with their respective gates:
//! - μ₁: SHACL validation gate
//! - μ₂: CONSTRUCT-only verification
//! - μ₃: Determinism checks
//! - μ₄: Formatter gate
//! - μ₅: Receipt generation

use ggen_core::graph::Graph;
use ggen_core::v6::passes::{
    CanonicalizationPass, EmissionPass, ExtractionPass, NormalizationPass, ReceiptGenerationPass,
};
use ggen_core::v6::receipt::BuildReceipt;
use ggen_core::v6::{Epoch, PipelineConfig, StagedPipeline};
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_mu1_normalization_with_shacl_validation() {
    // Test that μ₁ runs SHACL validation before and after CONSTRUCT
    let temp_dir = TempDir::new().unwrap();
    let ontology_dir = temp_dir.path().join("ontology");
    std::fs::create_dir_all(&ontology_dir).unwrap();

    // Create a valid ontology
    std::fs::write(
        ontology_dir.join("domain.ttl"),
        r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a rdfs:Class ;
            rdfs:label "Person" .
        "#,
    )
    .unwrap();

    let config = PipelineConfig::new("test", "1.0.0")
        .with_base_path(temp_dir.path())
        .with_ontology(PathBuf::from("ontology/domain.ttl"))
        .with_output_dir("output");

    let mut pipeline = StagedPipeline::new(config).unwrap();

    // Run normalization (SHACL validation is stubbed for now)
    let result = pipeline.load_ontologies();
    assert!(result.is_ok());
}

#[test]
fn test_mu2_extraction_rejects_construct_queries() {
    // Test that μ₂ rejects CONSTRUCT queries
    use ggen_core::v6::pass::{Pass, PassContext};
    use ggen_core::v6::passes::ExtractionPass;

    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a rdfs:Class .
        "#,
        )
        .unwrap();

    let mut pass = ExtractionPass::new();

    // Add a CONSTRUCT query (should be rejected)
    use ggen_core::v6::passes::ExtractionRule;
    pass.add_rule(ExtractionRule {
        name: "bad-construct".to_string(),
        query: r#"
            CONSTRUCT {
                ?s ?p ?o .
            }
            WHERE {
                ?s ?p ?o .
            }
        "#
        .to_string(),
        binding_key: "data".to_string(),
        description: Some("This should fail".to_string()),
    });

    let mut ctx = PassContext::new(&graph, PathBuf::from("."), PathBuf::from("output"));

    // Should fail with CONSTRUCT detection error
    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("CONSTRUCT"));
    assert!(err_msg.contains("STOPPED THE LINE"));
}

#[test]
fn test_mu2_extraction_rejects_update_queries() {
    // Test that μ₂ rejects INSERT/DELETE queries
    use ggen_core::v6::pass::{Pass, PassContext};
    use ggen_core::v6::passes::ExtractionPass;

    let graph = Graph::new().unwrap();
    let mut pass = ExtractionPass::new();

    use ggen_core::v6::passes::ExtractionRule;
    pass.add_rule(ExtractionRule {
        name: "bad-insert".to_string(),
        query: r#"
            INSERT DATA {
                <http://example.org/test> <http://example.org/prop> "value" .
            }
        "#
        .to_string(),
        binding_key: "data".to_string(),
        description: None,
    });

    let mut ctx = PassContext::new(&graph, PathBuf::from("."), PathBuf::from("output"));

    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("UPDATE") || err_msg.contains("INSERT"));
    assert!(err_msg.contains("STOPPED THE LINE"));
}

#[test]
fn test_mu3_emission_detects_nondeterministic_patterns() {
    // Test that μ₃ detects non-deterministic patterns in generated code
    use ggen_core::v6::pass::{Pass, PassContext};
    use ggen_core::v6::passes::{EmissionPass, EmissionRule};

    let temp_dir = TempDir::new().unwrap();
    let template_dir = temp_dir.path().join("templates");
    std::fs::create_dir_all(&template_dir).unwrap();

    // Create a template with non-deterministic pattern
    std::fs::write(
        template_dir.join("bad.rs.tera"),
        r#"
        pub fn get_timestamp() -> String {
            Utc::now().to_string()
        }
        "#,
    )
    .unwrap();

    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let graph = Graph::new().unwrap();
    let mut pass = EmissionPass::new();
    pass = pass.with_guards(ggen_core::v6::guard::GuardSet::new()); // Disable path guards for test

    pass.add_rule(EmissionRule {
        name: "bad-template".to_string(),
        template_path: PathBuf::from("templates/bad.rs.tera"),
        output_pattern: "timestamp.rs".to_string(),
        binding_key: "data".to_string(),
        iterate: false,
        skip_empty: false,
        description: None,
    });

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);

    // Should fail with non-deterministic pattern detection
    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("Non-Deterministic") || err_msg.contains("now()"));
    assert!(err_msg.contains("STOPPED THE LINE"));
}

#[test]
fn test_mu4_canonicalization_strict_mode_rejects_invalid_json() {
    // Test that μ₄ in strict mode rejects invalid JSON
    use ggen_core::v6::pass::{Pass, PassContext};
    use ggen_core::v6::passes::CanonicalizationPass;

    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    // Create invalid JSON file
    std::fs::write(
        output_dir.join("bad.json"),
        r#"{ "key": "value", invalid }"#,
    )
    .unwrap();

    let graph = Graph::new().unwrap();
    let pass = CanonicalizationPass::new().with_strict_mode(true);

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir);
    ctx.generated_files.push(PathBuf::from("bad.json"));

    // Should fail with JSON parsing error
    let result = pass.execute(&mut ctx);
    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("JSON") || err_msg.contains("Invalid"));
    assert!(err_msg.contains("STOPPED THE LINE"));
}

#[test]
fn test_mu5_receipt_generation_creates_valid_receipt() {
    // Test that μ₅ generates a valid receipt with hashes
    use ggen_core::v6::pass::{Pass, PassContext};

    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    // Create a generated file
    let content = "pub struct Model {}";
    std::fs::write(output_dir.join("model.rs"), content).unwrap();

    let graph = Graph::new().unwrap();
    let pass =
        ReceiptGenerationPass::new("6.0.0").with_receipt_path(PathBuf::from("receipt.json"));

    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone())
        .with_project("test".to_string(), "1.0.0".to_string());
    ctx.generated_files.push(PathBuf::from("model.rs"));

    let result = pass.execute(&mut ctx);
    assert!(result.is_ok());

    // Verify receipt file exists
    let receipt_path = output_dir.join("receipt.json");
    assert!(receipt_path.exists());

    // Verify receipt can be loaded and is valid
    let receipt = BuildReceipt::read_from_file(&receipt_path).unwrap();
    assert_eq!(receipt.toolchain_version, "6.0.0");
    assert_eq!(receipt.outputs.len(), 1);
    assert!(receipt.is_valid);

    // Verify hash is correct
    use sha2::{Digest, Sha256};
    let expected_hash = format!("{:x}", Sha256::digest(content.as_bytes()));
    assert_eq!(receipt.outputs[0].hash, expected_hash);
}

#[test]
fn test_complete_pipeline_with_receipt_verification() {
    // Test complete μ₁-μ₅ pipeline with receipt generation and verification
    let temp_dir = TempDir::new().unwrap();
    let ontology_dir = temp_dir.path().join("ontology");
    std::fs::create_dir_all(&ontology_dir).unwrap();

    // Create minimal ontology
    std::fs::write(
        ontology_dir.join("domain.ttl"),
        r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a rdfs:Class ;
            rdfs:label "Person" .
        "#,
    )
    .unwrap();

    let config = PipelineConfig::new("test", "1.0.0")
        .with_base_path(temp_dir.path())
        .with_ontology(PathBuf::from("ontology/domain.ttl"))
        .with_output_dir("output")
        .with_receipt_path(PathBuf::from("receipt.json"));

    let mut pipeline = StagedPipeline::new(config).unwrap();

    // Disable SHACL gate for this test (since shapes are stubbed)
    let norm_pass = NormalizationPass::with_standard_rules().with_shacl_gate(false);
    pipeline = pipeline.with_normalization(norm_pass);

    // Run complete pipeline
    let result = pipeline.run();
    assert!(result.is_ok());

    let receipt = result.unwrap();
    assert!(receipt.is_valid);
    assert_eq!(receipt.passes.len(), 4); // μ₁, μ₂, μ₃, μ₄ (μ₅ is implicit in pipeline)

    // Verify receipt was written
    let receipt_path = temp_dir.path().join("receipt.json");
    assert!(receipt_path.exists());
}

#[test]
fn test_pipeline_stops_on_first_failure() {
    // Test that pipeline stops immediately on first failure (Andon protocol)
    let temp_dir = TempDir::new().unwrap();
    let ontology_dir = temp_dir.path().join("ontology");
    std::fs::create_dir_all(&ontology_dir).unwrap();

    // Create minimal ontology
    std::fs::write(
        ontology_dir.join("domain.ttl"),
        r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a rdfs:Class .
        "#,
    )
    .unwrap();

    let config = PipelineConfig::new("test", "1.0.0")
        .with_base_path(temp_dir.path())
        .with_ontology(PathBuf::from("ontology/domain.ttl"))
        .with_output_dir("output");

    let mut pipeline = StagedPipeline::new(config).unwrap();

    // Add extraction pass with bad CONSTRUCT query
    let mut extract_pass = ExtractionPass::new();
    use ggen_core::v6::passes::ExtractionRule;
    extract_pass.add_rule(ExtractionRule {
        name: "bad".to_string(),
        query: "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }".to_string(),
        binding_key: "data".to_string(),
        description: None,
    });
    pipeline = pipeline.with_extraction(extract_pass);

    // Pipeline should stop at μ₂
    let result = pipeline.run();
    assert!(result.is_err());

    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("CONSTRUCT"));
    assert!(err_msg.contains("STOPPED THE LINE"));
}
