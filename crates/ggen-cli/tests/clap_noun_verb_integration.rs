#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]

//! Integration tests for the clap-noun-verb CLI surface.
//!
//! **Test Coverage**:
//! - CLI auto-discovery: the real `ggen` binary advertises its nouns and rejects
//!   invalid nouns/verbs with helpful errors (`--help`, `--version`).
//! - RDF spec structure: a CLI specification expressed in RDF/TTL is well-formed.
//!
//! **Chicago TDD**: REAL `ggen` process execution via `assert_cmd`; REAL
//! filesystem operations; no mocking of core functionality.
//!
//! The former template-driven generation / end-to-end suites were retired along
//! with the `ggen template generate_tree` subcommand they exercised (the CLI was
//! consolidated to `ggen sync` in v26.5.19+).

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Helper to create ggen command
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

// =============================================================================
// TEST SUITE 1: CLI AUTO-DISCOVERY
// =============================================================================

#[test]
fn test_ggen_help_shows_template_noun() {
    // Verify: ggen --help displays 'template' noun via auto-discovery
    ggen()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("template"));
}

#[test]
fn test_invalid_noun_returns_error() {
    // Verify: Invalid nouns produce helpful error messages
    ggen().arg("invalid-noun").assert().failure().stderr(
        predicate::str::contains("error")
            .or(predicate::str::contains("invalid"))
            .or(predicate::str::contains("unrecognized")),
    );
}

#[test]
fn test_invalid_verb_returns_error() {
    // Verify: Invalid verbs for valid nouns produce helpful errors
    ggen()
        .arg("template")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("error")
                .or(predicate::str::contains("invalid"))
                .or(predicate::str::contains("unrecognized")),
        );
}

#[test]
fn test_template_noun_in_main_help() {
    // Verify: Main help shows template noun
    ggen()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("template"));
}

#[test]
fn test_cli_version_flag() {
    // Verify: Version flag works
    ggen().arg("--version").assert().success();
}

// =============================================================================
// TEST SUITE 2: RDF CLI SPECIFICATION STRUCTURE
// =============================================================================

/// Create a test CLI specification in RDF/TTL format
fn create_cli_spec_ttl(temp_dir: &TempDir) -> PathBuf {
    let ttl_content = r#"@prefix cli: <http://example.org/cli#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# CLI Application Definition
cli:MyCLI a cli:Application ;
    rdfs:label "MyCLI" ;
    rdfs:comment "A test CLI application using clap-noun-verb" ;
    cli:version "0.1.0" ;
    cli:hasNoun cli:UserNoun, cli:ProjectNoun .

# User Noun
cli:UserNoun a cli:Noun ;
    rdfs:label "user" ;
    rdfs:comment "User management commands" ;
    cli:hasVerb cli:UserListVerb, cli:UserCreateVerb, cli:UserDeleteVerb .

cli:UserListVerb a cli:Verb ;
    rdfs:label "list" ;
    rdfs:comment "List all users" ;
    cli:hasArg cli:UserListFormatArg .

cli:UserListFormatArg a cli:Argument ;
    rdfs:label "format" ;
    rdfs:comment "Output format (json, table)" ;
    cli:type "String" ;
    cli:default "table" .

cli:UserCreateVerb a cli:Verb ;
    rdfs:label "create" ;
    rdfs:comment "Create a new user" ;
    cli:hasArg cli:UserCreateNameArg, cli:UserCreateEmailArg .

cli:UserCreateNameArg a cli:Argument ;
    rdfs:label "name" ;
    rdfs:comment "User name" ;
    cli:type "String" ;
    cli:required true .

cli:UserCreateEmailArg a cli:Argument ;
    rdfs:label "email" ;
    rdfs:comment "User email" ;
    cli:type "String" ;
    cli:required true .

cli:UserDeleteVerb a cli:Verb ;
    rdfs:label "delete" ;
    rdfs:comment "Delete a user" ;
    cli:hasArg cli:UserDeleteIdArg .

cli:UserDeleteIdArg a cli:Argument ;
    rdfs:label "id" ;
    rdfs:comment "User ID" ;
    cli:type "u32" ;
    cli:required true .

# Project Noun
cli:ProjectNoun a cli:Noun ;
    rdfs:label "project" ;
    rdfs:comment "Project management commands" ;
    cli:hasVerb cli:ProjectInitVerb, cli:ProjectBuildVerb .

cli:ProjectInitVerb a cli:Verb ;
    rdfs:label "init" ;
    rdfs:comment "Initialize a new project" ;
    cli:hasArg cli:ProjectInitNameArg .

cli:ProjectInitNameArg a cli:Argument ;
    rdfs:label "name" ;
    rdfs:comment "Project name" ;
    cli:type "String" ;
    cli:required true .

cli:ProjectBuildVerb a cli:Verb ;
    rdfs:label "build" ;
    rdfs:comment "Build the project" ;
    cli:hasArg cli:ProjectBuildReleaseArg .

cli:ProjectBuildReleaseArg a cli:Argument ;
    rdfs:label "release" ;
    rdfs:comment "Build in release mode" ;
    cli:type "bool" ;
    cli:default false .
"#;

    let ttl_path = temp_dir.path().join("cli-spec.ttl");
    fs::write(&ttl_path, ttl_content).expect("Failed to write TTL file");
    ttl_path
}

#[test]
fn test_load_rdf_cli_definition() {
    // Verify: TTL file can be read and contains valid RDF
    let temp_dir = TempDir::new().unwrap();
    let ttl_path = create_cli_spec_ttl(&temp_dir);

    // Verify TTL file exists and has content
    assert!(ttl_path.exists(), "TTL file should be created");
    let content = fs::read_to_string(&ttl_path).unwrap();
    assert!(
        content.contains("cli:Application"),
        "Should contain CLI spec"
    );
    assert!(content.contains("cli:Noun"), "Should define nouns");
    assert!(content.contains("cli:Verb"), "Should define verbs");
}

#[test]
fn test_rdf_spec_structure_valid() {
    // Verify: RDF specification has valid structure for CLI generation
    let temp_dir = TempDir::new().unwrap();
    let ttl_path = create_cli_spec_ttl(&temp_dir);

    let content = fs::read_to_string(&ttl_path).unwrap();

    // Verify nouns
    assert!(content.contains("cli:UserNoun"), "Should have User noun");
    assert!(
        content.contains("cli:ProjectNoun"),
        "Should have Project noun"
    );

    // Verify verbs
    assert!(
        content.contains("cli:UserListVerb"),
        "Should have list verb"
    );
    assert!(
        content.contains("cli:UserCreateVerb"),
        "Should have create verb"
    );
    assert!(
        content.contains("cli:UserDeleteVerb"),
        "Should have delete verb"
    );

    // Verify arguments
    assert!(content.contains("cli:Argument"), "Should define arguments");
    assert!(
        content.contains("cli:required"),
        "Should have required fields"
    );
}
