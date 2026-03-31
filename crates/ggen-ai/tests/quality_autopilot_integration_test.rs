//! Integration tests for CycleBreakerAgent (quality_autopilot)
//!
//! Tests the autonomous cycle detection and fixing for generated code.

#![cfg(feature = "swarm")]

use ggen_ai::swarm::agents::quality_autopilot::{CycleBreakerAgent, FixStrategy};
use ggen_ai::GenAiClient;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Create a test Rust project with a circular dependency
fn create_rust_project_with_cycle(dir: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    // Create Cargo.toml
    let cargo_toml = r#"
[package]
name = "cycle-test"
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

fn main() {
    a::hello();
}
"#;
    fs::write(src_dir.join("main.rs"), main_rs)?;

    // Create a.rs
    let a_rs = r#"
mod b;

pub fn hello() {
    b::world();
}
"#;
    fs::write(src_dir.join("a.rs"), a_rs)?;

    // Create b.rs with cycle back to a
    let b_rs = r#"
use super::a;

pub fn world() {
    a::hello();
}
"#;
    fs::write(src_dir.join("b.rs"), b_rs)?;

    Ok(())
}

/// Create a test Go project with a circular dependency
fn create_go_project_with_cycle(dir: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    // Create go.mod
    let go_mod = r#"
module cycle-test

go 1.21
"#;
    fs::write(dir.join("go.mod"), go_mod)?;

    // Create main.go
    let main_go = r#"
package main

import "cycle-test/a"

func main() {
    a.Hello()
}
"#;
    fs::write(dir.join("main.go"), main_go)?;

    // Create a.go
    let a_go = r#"
package a

import "cycle-test/b"

func Hello() {
    b.World()
}
"#;
    fs::write(dir.join("a.go"), a_go)?;

    // Create b.go with cycle
    let b_go = r#"
package b

import "cycle-test/a"

func World() {
    a.Hello()
}
"#;
    fs::write(dir.join("b.go"), b_go)?;

    Ok(())
}

#[test]
fn test_detect_language_rust() {
    let temp_dir = TempDir::new().unwrap();
    let dir = temp_dir.path().to_path_buf();
    create_rust_project_with_cycle(&dir).unwrap();

    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    let language = agent.detect_language(&dir).unwrap();
    assert_eq!(language, "rust");
}

#[test]
fn test_detect_language_go() {
    let temp_dir = TempDir::new().unwrap();
    let dir = temp_dir.path().to_path_buf();
    create_go_project_with_cycle(&dir).unwrap();

    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    let language = agent.detect_language(&dir).unwrap();
    assert_eq!(language, "go");
}

#[test]
fn test_extract_rust_imports() {
    let temp_dir = TempDir::new().unwrap();
    let dir = temp_dir.path().to_path_buf();
    create_rust_project_with_cycle(&dir).unwrap();

    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    let mut graph = std::collections::HashMap::new();
    agent.extract_rust_imports(&dir, &mut graph).unwrap();

    // Should have extracted imports from src/main.rs, src/a.rs, src/b.rs
    assert!(!graph.is_empty());
}

#[test]
fn test_detect_cycles_simple() {
    let mut graph = std::collections::HashMap::new();
    graph.insert("A.rs".to_string(), vec!["B.rs".to_string()]);
    graph.insert("B.rs".to_string(), vec!["C.rs".to_string()]);
    graph.insert("C.rs".to_string(), vec!["A.rs".to_string()]);

    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    let cycles = agent.detect_cycles(&graph).unwrap();
    assert_eq!(cycles.len(), 1);
    assert_eq!(cycles[0].len(), 4); // A → B → C → A
}

#[test]
fn test_detect_cycles_no_cycle() {
    let mut graph = std::collections::HashMap::new();
    graph.insert("A.rs".to_string(), vec!["B.rs".to_string()]);
    graph.insert("B.rs".to_string(), vec!["C.rs".to_string()]);
    graph.insert("C.rs".to_string(), vec![]);

    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    let cycles = agent.detect_cycles(&graph).unwrap();
    assert_eq!(cycles.len(), 0);
}

#[test]
fn test_detect_cycles_multiple() {
    let mut graph = std::collections::HashMap::new();
    graph.insert("A.rs".to_string(), vec!["B.rs".to_string()]);
    graph.insert("B.rs".to_string(), vec!["A.rs".to_string()]);
    graph.insert("C.rs".to_string(), vec!["D.rs".to_string()]);
    graph.insert("D.rs".to_string(), vec!["C.rs".to_string()]);

    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    let cycles = agent.detect_cycles(&graph).unwrap();
    assert_eq!(cycles.len(), 2);
}

#[test]
fn test_build_fix_prompt() {
    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    let cycle = vec!["A.rs".to_string(), "B.rs".to_string(), "C.rs".to_string()];
    let prompt = agent.build_fix_prompt(&cycle, "rust").unwrap();

    assert!(prompt.contains("A.rs → B.rs → C.rs"));
    assert!(prompt.contains("ExtractInterface"));
    assert!(prompt.contains("LazyInitialization"));
    assert!(prompt.contains("DependencyInversion"));
}

#[tokio::test]
async fn test_swarm_agent_interface() {
    let temp_dir = TempDir::new().unwrap();
    let dir = temp_dir.path().to_path_buf();
    create_rust_project_with_cycle(&dir).unwrap();

    let llm_client = GenAiClient::new(ggen_ai::LlmConfig::default()).unwrap();
    let agent = CycleBreakerAgent::new(llm_client);

    // Test agent interface
    assert_eq!(agent.name(), "cycle-breaker-code");
    assert!(agent
        .capabilities()
        .contains(&"cycle_detection".to_string()));

    // Test health check
    let health = agent.health_check().await;
    assert!(format!("{:?}", health.status).contains("Healthy"));
    assert_eq!(health.score, 1.0);

    // Test validate
    let validated = agent.validate().await.unwrap();
    assert!(validated);
}
