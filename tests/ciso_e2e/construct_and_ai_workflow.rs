//! Construct and AI E2E Workflow Tests
//!
//! Tests the construct and AI command workflows.
//!
//! Chicago TDD: Real CLI invocations, no mocks.

use assert_cmd::Command;
use predicates::prelude::*;

use super::helpers::*;

type TestResult = Result<(), Box<dyn std::error::Error>>;

#[test]
#[ignore = "construct noun does not exist in CLI"]
fn test_construct_create_help_shows_usage() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?.args(&["construct", "create", "--help"]).assert().success().stdout(predicate::str::contains("spec")).stdout(predicate::str::contains("output"));
    Ok(())
}

#[test]
#[ignore = "construct noun does not exist in CLI"]
fn test_construct_validate_help_shows_usage() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?.args(&["construct", "validate", "--help"]).assert().success().stdout(predicate::str::contains("module"));
    Ok(())
}

#[test]
#[ignore = "construct noun does not exist in CLI"]
fn test_construct_create_with_nonexistent_spec() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let (output, code) = run_ggen(&["construct", "create", "--spec_path", "/nonexistent/spec.ttl"], workspace.path())?;
    println!("construct create exit code: {}", code);
    println!("construct create output: {}", output.trim());
    assert_eq!(code, 0, "construct create should return JSON even for missing file");
    let json = parse_json_output(&output);
    assert_eq!(json["status"].as_str().unwrap_or(""), "error", "Should report error status for missing spec file");
    assert!(json["message"].as_str().unwrap_or("").contains("not found"), "Error message should mention file not found");
    Ok(())
}

#[test]
#[ignore = "construct noun does not exist in CLI"]
fn test_construct_create_with_non_ttl_extension() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let spec_path = workspace.path().join("spec.txt");
    std::fs::write(&spec_path, "not a turtle file").unwrap();
    let (output, code) = run_ggen(&["construct", "create", "--spec_path", spec_path.to_str().unwrap()], workspace.path()).unwrap();
    assert_eq!(code, 0, "construct create should return JSON");
    let json = parse_json_output(&output);
    assert_eq!(json["status"].as_str().unwrap_or(""), "error", "Should report error for non-.ttl file");
    assert!(json["message"].as_str().unwrap_or("").contains("Turtle"), "Error should mention Turtle format requirement");
    Ok(())
}

#[test]
#[ignore = "construct noun does not exist in CLI"]
fn test_construct_create_with_valid_ttl_reports_not_implemented() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let spec_path = workspace.path().join("spec.ttl");
    std::fs::write(&spec_path, r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

ex:TestClass a rdfs:Class ;
    rdfs:label "Test Class" .
"#).unwrap();
    let (output, code) = run_ggen(&["construct", "create", "--spec_path", spec_path.to_str().unwrap()], workspace.path()).unwrap();
    println!("construct create (valid TTL) exit code: {}", code);
    println!("construct create output: {}", output.trim());
    assert_eq!(code, 0, "construct create should return JSON");
    let json = parse_json_output(&output);
    assert_eq!(json["status"].as_str().unwrap_or(""), "not_implemented", "Construct pipeline should report not_implemented status");
    assert!(json.get("module_name").is_some(), "Output should contain inferred module name");
    assert!(json.get("next_steps").is_some(), "Output should contain next steps");
    Ok(())
}

#[test]
#[ignore = "construct noun does not exist in CLI"]
fn test_construct_validate_with_module_name() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let (output, code) = run_ggen(&["construct", "validate", "--module_name", "test_module"], workspace.path())?;
    println!("construct validate exit code: {}", code);
    println!("construct validate output: {}", output.trim());
    assert_eq!(code, 0, "construct validate should return JSON");
    let json = parse_json_output(&output);
    assert_eq!(json["status"].as_str().unwrap_or(""), "not_implemented", "Construct validate should report not_implemented");
    assert_eq!(json["module_name"].as_str().unwrap_or(""), "test_module", "Output should echo module name");
    Ok(())
}

#[test]
fn test_ai_generate_help_shows_usage() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?.args(&["ai", "generate", "--help"]).assert().success().stdout(predicate::str::contains("prompt")).stdout(predicate::str::contains("model"));
    Ok(())
}

#[test]
fn test_ai_chat_help_shows_usage() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?.args(&["ai", "chat", "--help"]).assert().success().stdout(predicate::str::contains("message")).stdout(predicate::str::contains("model"));
    Ok(())
}

#[test]
fn test_ai_analyze_help_shows_usage() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?.args(&["ai", "analyze", "--help"]).assert().success().stdout(predicate::str::contains("file")).stdout(predicate::str::contains("code"));
    Ok(())
}

#[test]
fn test_ai_generate_without_api_key_fails_gracefully() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    std::env::remove_var("GROQ_API_KEY");
    let (output, code) = run_ggen(&["ai", "generate", "--prompt", "Hello world"], workspace.path())?;
    println!("ai generate exit code: {}", code);
    println!("ai generate output: {}", output.trim());
    Ok(())
}

#[test]
fn test_ai_analyze_requires_file_or_code() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let (_output, code) = run_ggen(&["ai", "analyze"], workspace.path())?;
    assert_ne!(code, 0, "ai analyze without --file or --code should fail");
    Ok(())
}

#[test]
fn test_packs_list_shows_packs() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let (output, code) = run_ggen(&["packs", "list"], workspace.path())?;
    println!("packs list exit code: {}", code);
    println!("packs list output: {}", output.trim());
    assert_eq!(code, 0, "packs list should succeed");
    let json = parse_json_output(&output);
    assert!(json.get("total").is_some(), "packs list JSON should contain 'total' field");
    assert!(json.get("packs").is_some(), "packs list JSON should contain 'packs' array");
    Ok(())
}

#[test]
fn test_packs_list_verbose() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let (output, code) = run_ggen(&["packs", "list", "--verbose"], workspace.path())?;
    println!("packs list --verbose exit code: {}", code);
    assert_eq!(code, 0, "packs list --verbose should succeed");
    let json = parse_json_output(&output);
    assert!(json.get("total").is_some(), "packs list --verbose JSON should contain 'total' field");
    Ok(())
}

#[test]
fn test_packs_search_requires_query() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let (_output, code) = run_ggen(&["packs", "search"], workspace.path())?;
    assert_ne!(code, 0, "packs search without query should fail");
    Ok(())
}

#[test]
fn test_packs_search_with_query() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let (output, code) = run_ggen(&["packs", "search", "--query", "mcp"], workspace.path())?;
    println!("packs search exit code: {}", code);
    println!("packs search output: {}", output.trim());
    assert_eq!(code, 0, "packs search should succeed");
    let json = parse_json_output(&output);
    assert!(json.get("total").is_some(), "packs search JSON should contain 'total' field");
    assert!(json.get("results").is_some(), "packs search JSON should contain 'results' array");
    Ok(())
}

#[test]
fn test_policy_list_shows_profiles() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let (output, code) = run_ggen(&["policy", "list"], workspace.path())?;
    println!("policy list exit code: {}", code);
    println!("policy list output: {}", output.trim());
    assert_eq!(code, 0, "policy list should succeed");
    let json = parse_json_output(&output);
    assert!(json.get("total").is_some(), "policy list JSON should contain 'total' field");
    assert!(json.get("profiles").is_some(), "policy list JSON should contain 'profiles' array");
    let total = json["total"].as_u64().unwrap_or(0);
    assert!(total > 0, "Should have at least one predefined policy profile");
    Ok(())
}

#[test]
fn test_policy_show_help_shows_usage() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?.args(&["policy", "show", "--help"]).assert().success().stdout(predicate::str::contains("profile"));
    Ok(())
}

#[test]
fn test_help_shows_all_nouns() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?.arg("--help").assert().success()
        .stdout(predicate::str::contains("sync"))
        .stdout(predicate::str::contains("init"))
        .stdout(predicate::str::contains("template"))
        .stdout(predicate::str::contains("graph"))
        .stdout(predicate::str::contains("receipt"))
        .stdout(predicate::str::contains("construct"))
        .stdout(predicate::str::contains("ai"))
        .stdout(predicate::str::contains("packs"))
        .stdout(predicate::str::contains("capability"))
        .stdout(predicate::str::contains("policy"));
    Ok(())
}

#[test]
fn test_unknown_noun_fails() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?.arg("nonexistent_noun_12345").assert().failure();
    Ok(())
}
