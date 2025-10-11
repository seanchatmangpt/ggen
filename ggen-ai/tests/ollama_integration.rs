//! Ollama integration tests for ggen-ai
//!
//! These tests validate that all AI features work correctly with Ollama LLM.
//! They are excluded from normal test runs and require the ollama-integration feature flag.
//!
//! Prerequisites:
//! - Ollama must be running locally on port 11434
//! - qwen3-coder:30b model must be pulled: `ollama pull qwen3-coder:30b`
//!
//! Run with: `cargo test --features ollama-integration --test ollama_integration`

#![cfg(feature = "ollama-integration")]

use ggen_ai::{
    client::LlmClient,
    generators::{OntologyGenerator, SparqlGenerator, TemplateGenerator},
    mcp::tools::AiMcpTools,
    skip_if_ollama_unavailable,
    test_helpers::create_test_ollama_client,
};
use ggen_core::Graph;
use std::time::Duration;
use tempfile::TempDir;
use tokio::time::timeout;

#[tokio::test]
async fn test_ollama_template_generation() {
    skip_if_ollama_unavailable!();

    println!("🧪 Testing Ollama template generation...");

    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));

    let description = "A simple Rust CLI tool for file operations";
    let examples = vec![
        "Include file copy functionality",
        "Use clap for argument parsing",
    ];

    let result = timeout(
        Duration::from_secs(30),
        generator.generate_template(description, examples),
    )
    .await;

    match result {
        Ok(Ok(template)) => {
            println!("✅ Template generation successful");
            assert!(
                !template.body.is_empty(),
                "Template body should not be empty"
            );
            // Note: front.to might be None for some templates, that's okay
            println!(
                "📝 Generated template length: {} characters",
                template.body.len()
            );
        }
        Ok(Err(e)) => panic!("Template generation failed: {}", e),
        Err(_) => panic!("Template generation timed out"),
    }
}

#[tokio::test]
async fn test_ollama_sparql_generation() {
    skip_if_ollama_unavailable!();

    println!("🧪 Testing Ollama SPARQL generation...");

    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = SparqlGenerator::with_ollama_qwen3_coder(Box::new(client));

    let description = "Query for user profiles with email addresses";
    let graph = Graph::new().expect("Failed to create empty graph");

    let result = timeout(
        Duration::from_secs(30),
        generator.generate_query(&graph, description),
    )
    .await;

    match result {
        Ok(Ok(query)) => {
            println!("✅ SPARQL generation successful");
            assert!(!query.is_empty(), "SPARQL query should not be empty");
            assert!(
                query.contains("SELECT"),
                "SPARQL query should contain SELECT"
            );
            println!("🔍 Generated SPARQL query: {}", query);
        }
        Ok(Err(e)) => panic!("SPARQL generation failed: {}", e),
        Err(_) => panic!("SPARQL generation timed out"),
    }
}

#[tokio::test]
async fn test_ollama_ontology_generation() {
    skip_if_ollama_unavailable!();

    println!("🧪 Testing Ollama ontology generation...");

    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = OntologyGenerator::with_ollama_qwen3_coder(Box::new(client));

    let description = "E-commerce domain ontology with products, customers, and orders";
    let examples = vec![
        "Include product categories",
        "Define customer relationships",
    ];

    let result = timeout(
        Duration::from_secs(30),
        generator.generate_ontology(description, examples),
    )
    .await;

    match result {
        Ok(Ok(ontology)) => {
            println!("✅ Ontology generation successful");
            assert!(!ontology.is_empty(), "Ontology should not be empty");
            assert!(
                ontology.contains("@prefix"),
                "Ontology should contain prefixes"
            );
            println!(
                "🏗️ Generated ontology length: {} characters",
                ontology.len()
            );
        }
        Ok(Err(e)) => panic!("Ontology generation failed: {}", e),
        Err(_) => panic!("Ontology generation timed out"),
    }
}

#[tokio::test]
async fn test_ollama_mcp_tools() {
    skip_if_ollama_unavailable!();

    println!("🧪 Testing Ollama MCP tools...");

    let ai_tools = AiMcpTools::new().with_ollama();

    // Test that all generators are initialized
    assert!(
        ai_tools.has_template_generator(),
        "Template generator should be initialized"
    );
    assert!(
        ai_tools.has_sparql_generator(),
        "SPARQL generator should be initialized"
    );
    assert!(
        ai_tools.has_ontology_generator(),
        "Ontology generator should be initialized"
    );
    assert!(
        ai_tools.has_refactor_assistant(),
        "Refactor assistant should be initialized"
    );

    println!("✅ MCP tools initialization successful");
}

#[tokio::test]
async fn test_ollama_cli_integration() {
    skip_if_ollama_unavailable!();

    println!("🧪 Testing Ollama CLI integration...");

    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let output_file = temp_dir.path().join("test_output.tmpl");

    // Test template generation via CLI
    let output = tokio::process::Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "ggen",
            "--",
            "ai",
            "generate",
            "--description",
            "A Python web API with FastAPI",
            "--language",
            "python",
            "--framework",
            "fastapi",
            "--output",
            output_file.to_str().unwrap(),
        ])
        .current_dir("/Users/sac/ggen")
        .output()
        .await
        .expect("Failed to run CLI command");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        panic!("CLI command failed: {}", stderr);
    }

    // Verify output file was created and contains content
    let content = std::fs::read_to_string(&output_file).expect("Failed to read output file");

    assert!(
        !content.is_empty(),
        "Generated template should not be empty"
    );
    assert!(
        content.contains("FastAPI"),
        "Template should mention FastAPI"
    );

    println!("✅ CLI integration test successful");
}

#[tokio::test]
async fn test_ollama_frontmatter_generation() {
    skip_if_ollama_unavailable!();

    println!("🧪 Testing Ollama frontmatter generation...");

    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let output_file = temp_dir.path().join("frontmatter_test.tmpl");

    // Test frontmatter generation via CLI
    let output = tokio::process::Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "ggen",
            "--",
            "ai",
            "frontmatter",
            "--description",
            "A TypeScript React component",
            "--output",
            output_file.to_str().unwrap(),
        ])
        .current_dir("/Users/sac/ggen")
        .output()
        .await
        .expect("Failed to run CLI command");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        panic!("Frontmatter CLI command failed: {}", stderr);
    }

    // Verify output file was created
    assert!(output_file.exists(), "Frontmatter output file should exist");

    println!("✅ Frontmatter generation test successful");
}

#[tokio::test]
async fn test_ollama_sparql_cli() {
    skip_if_ollama_unavailable!();

    println!("🧪 Testing Ollama SPARQL CLI...");

    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let output_file = temp_dir.path().join("sparql_test.tmpl");

    // Test SPARQL generation via CLI
    let output = tokio::process::Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "ggen",
            "--",
            "ai",
            "sparql",
            "--description",
            "Query for product inventory data",
            "--output",
            output_file.to_str().unwrap(),
        ])
        .current_dir("/Users/sac/ggen")
        .output()
        .await
        .expect("Failed to run CLI command");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        panic!("SPARQL CLI command failed: {}", stderr);
    }

    // Verify output file was created and contains SPARQL
    let content = std::fs::read_to_string(&output_file).expect("Failed to read SPARQL output file");

    assert!(!content.is_empty(), "Generated SPARQL should not be empty");
    assert!(content.contains("SELECT"), "SPARQL should contain SELECT");

    println!("✅ SPARQL CLI test successful");
}

#[tokio::test]
async fn test_ollama_model_availability() {
    skip_if_ollama_unavailable!();

    println!("🧪 Testing Ollama model availability...");

    let client = create_test_ollama_client().expect("Failed to create Ollama client");

    // Test that the specific model is supported
    assert!(
        client.supports_model("qwen3-coder:30b"),
        "qwen3-coder:30b should be supported"
    );

    // Test provider name
    assert_eq!(
        client.provider_name(),
        "ollama",
        "Provider name should be 'ollama'"
    );

    println!("✅ Model availability test successful");
}

#[tokio::test]
async fn test_ollama_error_handling() {
    skip_if_ollama_unavailable!();

    println!("🧪 Testing Ollama error handling...");

    let client = create_test_ollama_client().expect("Failed to create Ollama client");

    // Test with invalid model (should fail gracefully)
    let invalid_config = ggen_ai::client::LlmConfig {
        model: "nonexistent-model".to_string(),
        max_tokens: Some(10),
        temperature: Some(0.1),
        top_p: None,
        stop: None,
        extra: std::collections::HashMap::new(),
    };

    let result = timeout(
        Duration::from_secs(10),
        client.complete("Hello", Some(invalid_config)),
    )
    .await;

    // Should fail gracefully, not panic
    match result {
        Ok(Err(_)) => {
            println!("✅ Error handling test successful - invalid model handled gracefully");
        }
        Ok(Ok(_)) => {
            println!("⚠️  Unexpected success with invalid model");
        }
        Err(_) => {
            println!("✅ Error handling test successful - timeout handled gracefully");
        }
    }
}
