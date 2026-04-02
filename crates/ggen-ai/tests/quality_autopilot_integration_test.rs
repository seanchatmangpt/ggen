//! Integration tests for QualityAutopilot agent
//!
//! Tests the autonomous quality fixing functionality including:
//! - Cycle detection and fixing
//! - Quality assessment
//! - Validation context extraction

#![cfg(feature = "swarm")]

use ggen_ai::swarm::agents::quality_autopilot::CycleBreakerAgent;
use ggen_ai::GenAiClient;
use std::fs;
use tempfile::TempDir;

/// Create a test Rust project with circular dependencies
fn create_test_project_with_cycles(dir: &std::path::Path) -> Result<(), Box<dyn std::error::Error>> {
    // Create Cargo.toml
    let cargo_toml = r#"
[package]
name = "test-cycles"
version = "0.1.0"
edition = "2021"

[dependencies]
"#;
    fs::write(dir.join("Cargo.toml"), cargo_toml)?;

    // Create src directory
    let src_dir = dir.join("src");
    fs::create_dir_all(&src_dir)?;

    // Create main.rs
    let main_rs = r#"
mod a;
mod b;

fn main() {
    a::function_a();
}
"#;
    fs::write(src_dir.join("main.rs"), main_rs)?;

    // Create a.rs with cycle
    let a_rs = r#"
use super::b;

pub fn function_a() {
    b::function_b();
}
"#;
    fs::write(src_dir.join("a.rs"), a_rs)?;

    // Create b.rs that creates cycle
    let b_rs = r#"
use super::a;

pub fn function_b() {
    a::function_a();
}
"#;
    fs::write(src_dir.join("b.rs"), b_rs)?;

    Ok(())
}

/// Create a clean test Rust project
fn create_clean_test_project(dir: &std::path::Path) -> Result<(), Box<dyn std::error::Error>> {
    // Create Cargo.toml
    let cargo_toml = r#"
[package]
name = "test-clean"
version = "0.1.0"
edition = "2021"

[dependencies]
"#;
    fs::write(dir.join("Cargo.toml"), cargo_toml)?;

    // Create src directory
    let src_dir = dir.join("src");
    fs::create_dir_all(&src_dir)?;

    // Create main.rs
    let main_rs = r#"
fn main() {
    println!("Hello, world!");
}
"#;
    fs::write(src_dir.join("main.rs"), main_rs)?;

    Ok(())
}

#[test]
fn test_quality_autopilot_detects_cycles() {
    let temp_dir = TempDir::new().unwrap();
    create_test_project_with_cycles(temp_dir.path()).unwrap();

    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    // Test language detection
    let language = agent.detect_language(temp_dir.path()).unwrap();
    assert_eq!(language, "rust");

    // Test import graph extraction
    let graph = agent.extract_import_graph(temp_dir.path(), &language).unwrap();
    assert!(!graph.is_empty());

    // Test cycle detection
    let cycles = agent.detect_cycles(&graph).unwrap();
    // Should detect the circular dependency between a.rs and b.rs
    assert!(!cycles.is_empty(), "Expected to detect cycles in test project");
}

#[test]
fn test_quality_autopilot_no_cycles_in_clean_project() {
    let temp_dir = TempDir::new().unwrap();
    create_clean_test_project(temp_dir.path()).unwrap();

    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    // Test language detection
    let language = agent.detect_language(temp_dir.path()).unwrap();
    assert_eq!(language, "rust");

    // Test import graph extraction
    let graph = agent.extract_import_graph(temp_dir.path(), &language).unwrap();

    // Test cycle detection
    let cycles = agent.detect_cycles(&graph).unwrap();
    assert_eq!(cycles.len(), 0, "Expected no cycles in clean project");
}

#[test]
fn test_quality_score_calculation() {
    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    // Test with no issues
    let context = ggen_ai::swarm::agents::quality_autopilot::QualityContext {
        sparql_valid: true,
        template_valid: true,
        cycles_detected: 0,
        files_generated: 10,
        complexity_score: 0.5,
    };
    let score = agent.calculate_quality_score(&[], &context);
    assert!(score > 0.9, "Expected high score for clean context");

    // Test with issues
    let issues = vec![
        ggen_ai::swarm::agents::quality_autopilot::QualityIssue {
            category: ggen_ai::swarm::agents::quality_autopilot::QualityCategory::Syntax,
            severity: ggen_ai::swarm::agents::quality_autopilot::QualitySeverity::Error,
            description: "Test error".to_string(),
            affected_area: "test".to_string(),
            suggested_fix: None,
            line_number: None,
        },
    ];
    let score_with_issues = agent.calculate_quality_score(&issues, &context);
    assert!(score_with_issues < score, "Expected lower score with issues");
}

#[test]
fn test_quality_report_generation() {
    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    let issues = vec![
        ggen_ai::swarm::agents::quality_autopilot::QualityIssue {
            category: ggen_ai::swarm::agents::quality_autopilot::QualityCategory::Syntax,
            severity: ggen_ai::swarm::agents::quality_autopilot::QualitySeverity::Error,
            description: "Syntax error".to_string(),
            affected_area: "compilation".to_string(),
            suggested_fix: Some("Fix syntax".to_string()),
            line_number: Some(42),
        },
    ];

    let context = ggen_ai::swarm::agents::quality_autopilot::QualityContext {
        sparql_valid: false,
        template_valid: true,
        cycles_detected: 2,
        files_generated: 10,
        complexity_score: 0.7,
    };

    let report = agent.generate_quality_report(&issues, &context);

    assert!(report.overall_score < 1.0, "Expected score less than perfect");
    assert_eq!(report.issues_count, 1);
    assert_eq!(report.issues.len(), 1);
    assert!(!report.recommendations.is_empty(), "Expected recommendations");
    assert!(!report.timestamp.is_empty());
}
