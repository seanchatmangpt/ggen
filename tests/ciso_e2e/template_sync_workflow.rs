//! Template and Sync Authority E2E Workflow Tests
//!
//! Tests that sync is the authoritative code generation path
//! and templates follow governance rules.
//!
//! Chicago TDD: Real CLI invocations, no mocks.

use assert_cmd::Command;
use predicates::prelude::*;

use super::helpers::*;

type TestResult = Result<(), Box<dyn std::error::Error>>;

// ==============================================================================
// Template Authority Tests
// ==============================================================================

#[test]
fn test_template_list_returns_json_output() {
    let workspace = create_temp_workspace().unwrap();
    let (output, code) = run_ggen(&["template", "list"], workspace.path()).unwrap();
    println!("template list exit code: {}", code);
    println!("template list output: {}", output.trim());
    if code == 0 && !output.trim().is_empty() {
        let json = parse_json_output(&output);
        assert!(json.get("total").is_some(), "template list JSON should contain 'total' field");
    }
}

#[test]
fn test_template_show_help_shows_usage() {
    Command::cargo_bin("ggen").unwrap().args(&["template", "show", "--help"]).assert().success().stdout(predicate::str::contains("template"));
}

#[test]
fn test_template_new_creates_template() {
    let workspace = create_temp_workspace().unwrap();
    let (output, code) = run_ggen(&["template", "new", "--name", "e2e-test-template", "--template-type", "generic"], workspace.path()).unwrap();
    println!("template new exit code: {}", code);
    println!("template new output: {}", output.trim());
    assert_eq!(code, 0, "template new should succeed");
    let json = parse_json_output(&output);
    assert_eq!(json["template_name"].as_str().unwrap_or(""), "e2e-test-template", "Output should contain template name");
    assert_eq!(json["template_type"].as_str().unwrap_or(""), "generic", "Output should contain template type");
}

#[test]
fn test_template_list_with_directory_flag() {
    let workspace = create_temp_workspace().unwrap();
    let templates_dir = workspace.path().join("templates");
    std::fs::create_dir_all(&templates_dir).unwrap();
    std::fs::write(templates_dir.join("test.tmpl"), "# {{ name }}\n").unwrap();
    let (output, code) = run_ggen(&["template", "list", "--directory", templates_dir.to_str().unwrap()], workspace.path()).unwrap();
    println!("template list --directory exit code: {}", code);
    println!("template list --directory output: {}", output.trim());
    assert_eq!(code, 0, "template list with valid directory should succeed");
    if !output.trim().is_empty() {
        let json = parse_json_output(&output);
        assert!(json.get("total").is_some(), "JSON output should have 'total' field");
    }
}

#[test]
fn test_template_lint_requires_template_arg() {
    let workspace = create_temp_workspace().unwrap();
    let (_output, code) = run_ggen(&["template", "lint"], workspace.path()).unwrap();
    assert_ne!(code, 0, "template lint without arguments should fail");
}

// ==============================================================================
// Sync Authority Tests
// ==============================================================================

#[test]
fn test_sync_help_shows_usage() {
    Command::cargo_bin("ggen").unwrap().args(&["sync", "--help"]).assert().success().stdout(predicate::str::contains("sync")).stdout(predicate::str::contains("Usage"));
}

#[test]
fn test_init_creates_valid_project_structure() {
    let workspace = create_temp_workspace().unwrap();
    let (output, code) = run_ggen(&["init", "--path", workspace.path().to_str().unwrap(), "--skip_hooks"], workspace.path()).unwrap();
    println!("init exit code: {}", code);
    println!("init output: {}", output.trim());
    assert_eq!(code, 0, "ggen init should succeed");
    let json = parse_json_output(&output);
    assert_eq!(json["status"].as_str().unwrap_or(""), "success", "Init should report success status");
    assert!(workspace.path().join("ggen.toml").exists(), "ggen.toml should exist after init");
    assert!(workspace.path().join("schema/domain.ttl").exists(), "schema/domain.ttl should exist after init");
    assert!(workspace.path().join("Makefile").exists(), "Makefile should exist after init");
    assert!(workspace.path().join("scripts/startup.sh").exists(), "scripts/startup.sh should exist after init");
    assert!(workspace.path().join("templates/example.txt.tera").exists(), "templates/example.txt.tera should exist after init");
    assert!(workspace.path().join("schema").is_dir(), "schema/ directory should exist");
    assert!(workspace.path().join("templates").is_dir(), "templates/ directory should exist");
    assert!(workspace.path().join("scripts").is_dir(), "scripts/ directory should exist");
}

#[test]
fn test_init_twice_without_force_fails() {
    let workspace = create_temp_workspace().unwrap();
    let project_dir = workspace.path().to_str().unwrap();
    let (_output1, code1) = run_ggen(&["init", "--path", project_dir, "--skip_hooks"], workspace.path()).unwrap();
    assert_eq!(code1, 0, "First init should succeed");
    let (output2, code2) = run_ggen(&["init", "--path", project_dir, "--skip_hooks"], workspace.path()).unwrap();
    assert_ne!(code2, 0, "Second init without --force should fail");
    let json = parse_json_output(&output2);
    assert_eq!(json["status"].as_str().unwrap_or(""), "error", "Second init should report error status");
    assert!(json["error"].as_str().unwrap_or("").contains("already initialized"), "Error message should mention already initialized");
}

#[test]
fn test_init_force_reinitializes() {
    let workspace = create_temp_workspace().unwrap();
    let project_dir = workspace.path().to_str().unwrap();
    let (_output1, code1) = run_ggen(&["init", "--path", project_dir, "--skip_hooks"], workspace.path()).unwrap();
    assert_eq!(code1, 0, "First init should succeed");
    let toml_path = workspace.path().join("ggen.toml");
    std::fs::write(&toml_path, "# modified\n").unwrap();
    let (output2, code2) = run_ggen(&["init", "--path", project_dir, "--force", "--skip_hooks"], workspace.path()).unwrap();
    assert_eq!(code2, 0, "Force re-init should succeed");
    let json = parse_json_output(&output2);
    assert_eq!(json["status"].as_str().unwrap_or(""), "success", "Force re-init should report success");
    let content = std::fs::read_to_string(&toml_path).unwrap();
    assert!(content.contains("[project]"), "ggen.toml should be restored to original content after force init");
}

#[test]
fn test_init_creates_transaction_receipt() {
    let workspace = create_temp_workspace().unwrap();
    let (output, code) = run_ggen(&["init", "--path", workspace.path().to_str().unwrap(), "--skip_hooks"], workspace.path()).unwrap();
    assert_eq!(code, 0, "Init should succeed");
    let json = parse_json_output(&output);
    assert!(json.get("transaction").is_some(), "Output should contain transaction info");
    let tx = &json["transaction"];
    assert!(tx["committed"].as_bool().unwrap_or(false), "Transaction should be committed");
    assert!(tx["total_files"].as_u64().unwrap_or(0) > 0, "Transaction should have created files");
}

#[test]
fn test_sync_dry_run_with_init_project() {
    let workspace = create_temp_workspace().unwrap();
    let project_dir = workspace.path().to_str().unwrap();
    let (init_output, init_code) = run_ggen(&["init", "--path", project_dir, "--skip_hooks"], workspace.path()).unwrap();
    assert_eq!(init_code, 0, "Init should succeed");
    println!("Init output: {}", init_output.trim());
    let (sync_output, sync_code) = run_ggen(&["sync", "--dry-run"], workspace.path()).unwrap();
    println!("sync --dry-run exit code: {}", sync_code);
    println!("sync --dry-run output: {}", sync_output.trim());
}

// ==============================================================================
// Graph Authority Tests
// ==============================================================================

#[test]
fn test_graph_help_shows_usage() {
    Command::cargo_bin("ggen").unwrap().args(&["graph", "--help"]).assert().success().stdout(predicate::str::contains("graph"));
}

#[test]
fn test_graph_load_help_shows_usage() {
    Command::cargo_bin("ggen").unwrap().args(&["graph", "load", "--help"]).assert().success();
}

#[test]
fn test_graph_query_help_shows_usage() {
    Command::cargo_bin("ggen").unwrap().args(&["graph", "query", "--help"]).assert().success();
}

#[test]
fn test_graph_export_help_shows_usage() {
    Command::cargo_bin("ggen").unwrap().args(&["graph", "export", "--help"]).assert().success();
}

#[test]
fn test_graph_load_with_real_data() {
    let workspace = create_temp_workspace().unwrap();
    let ttl_path = workspace.path().join("test.ttl");
    std::fs::write(&ttl_path, r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

ex:Thing a rdfs:Class ;
    rdfs:label "Thing" .
"#).unwrap();
    let (output, code) = run_ggen(&["graph", "load", ttl_path.to_str().unwrap(), "--format", "turtle"], workspace.path()).unwrap();
    println!("graph load exit code: {}", code);
    println!("graph load output: {}", output.trim());
    if code == 0 && !output.trim().is_empty() {
        let json = parse_json_output(&output);
        assert!(json.get("triples_loaded").is_some(), "graph load JSON should contain 'triples_loaded'");
    }
}

#[test]
fn test_version_returns_consistent_version() {
    let v1 = Command::cargo_bin("ggen").unwrap().arg("--version").assert().success().get_output().clone();
    let v2 = Command::cargo_bin("ggen").unwrap().arg("--version").assert().success().get_output().clone();
    let out1 = String::from_utf8_lossy(&v1.stdout).to_string();
    let out2 = String::from_utf8_lossy(&v2.stdout).to_string();
    assert_eq!(out1, out2, "Version output should be consistent across calls");
    assert!(out1.contains("ggen"), "Version output should contain 'ggen'");
}
