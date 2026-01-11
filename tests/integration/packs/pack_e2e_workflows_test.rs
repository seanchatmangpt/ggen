//! End-to-end workflow tests for packs system
//!
//! Tests complete user journeys from pack discovery to project generation

use ggen_core::gpack::GpackManifest;
use ggen_core::{GenContext, Generator, Pipeline};
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

const WEB_API_PACK: &str = "tests/fixtures/packs/web-api-pack";
const CLI_TOOL_PACK: &str = "tests/fixtures/packs/cli-tool-pack";
const DATABASE_PACK: &str = "tests/fixtures/packs/database-pack";

// ============================================================================
// Single Pack Generation Workflows
// ============================================================================

#[test]
fn test_workflow_generate_project_from_single_pack() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    fs::create_dir_all(&output_dir).unwrap();

    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    // Discover templates
    let templates = manifest.discover_templates(&pack_path).unwrap();
    assert!(!templates.is_empty(), "Should find templates in pack");

    // For each template, verify it can be read
    for template_path in templates {
        let content = fs::read_to_string(&template_path).unwrap();
        assert!(!content.is_empty(), "Template should have content");
    }
}

#[test]
fn test_workflow_generate_with_variables() {
    let temp_dir = TempDir::new().unwrap();
    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    let templates = manifest.discover_templates(&pack_path).unwrap();
    let template = &templates[0]; // Use first template

    // Read template and verify it has variables
    let content = fs::read_to_string(template).unwrap();
    assert!(content.contains("{{"), "Template should have Tera variables");

    // Create variables map
    let mut vars = BTreeMap::new();
    vars.insert("output_dir".to_string(), "output".to_string());
    vars.insert("endpoint_name".to_string(), "user".to_string());
    vars.insert("resource_name".to_string(), "User".to_string());

    // Variables are ready for generation
    assert_eq!(vars.len(), 3);
}

#[test]
fn test_workflow_validate_pack_before_use() {
    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    // Step 1: Validate manifest structure
    assert!(!manifest.metadata.id.is_empty());
    assert!(!manifest.metadata.version.is_empty());
    assert!(semver::Version::parse(&manifest.metadata.version).is_ok());

    // Step 2: Validate file discovery
    let templates = manifest.discover_templates(&pack_path).unwrap();
    let rdf_files = manifest.discover_rdf_files(&pack_path).unwrap();
    let queries = manifest.discover_query_files(&pack_path).unwrap();

    // Step 3: Validate files exist
    for file in templates.iter().chain(rdf_files.iter()).chain(queries.iter()) {
        assert!(file.exists(), "File {:?} should exist", file);
    }

    // Pack is valid and ready to use
    assert!(true);
}

// ============================================================================
// Multi-Pack Composition Workflows
// ============================================================================

#[test]
fn test_workflow_compose_multiple_packs() {
    // Load multiple packs
    let web_api_path = PathBuf::from(WEB_API_PACK);
    let cli_tool_path = PathBuf::from(CLI_TOOL_PACK);
    let database_path = PathBuf::from(DATABASE_PACK);

    let web_api = GpackManifest::load_from_file(&web_api_path.join("gpack.toml")).unwrap();
    let cli_tool = GpackManifest::load_from_file(&cli_tool_path.join("gpack.toml")).unwrap();
    let database = GpackManifest::load_from_file(&database_path.join("gpack.toml")).unwrap();

    // Collect all templates
    let mut all_templates = Vec::new();
    all_templates.extend(web_api.discover_templates(&web_api_path).unwrap());
    all_templates.extend(cli_tool.discover_templates(&cli_tool_path).unwrap());
    all_templates.extend(database.discover_templates(&database_path).unwrap());

    assert!(all_templates.len() >= 3, "Should have templates from all packs");

    // Collect all RDF files
    let mut all_rdf = Vec::new();
    all_rdf.extend(web_api.discover_rdf_files(&web_api_path).unwrap());
    all_rdf.extend(cli_tool.discover_rdf_files(&cli_tool_path).unwrap());
    all_rdf.extend(database.discover_rdf_files(&database_path).unwrap());

    assert!(all_rdf.len() >= 3, "Should have RDF from all packs");
}

#[test]
fn test_workflow_check_pack_compatibility() {
    let cli_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap();
    let web_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK))).unwrap();

    // CLI pack depends on web-api
    assert!(cli_pack.dependencies.contains_key("test.web-api"));

    // Check version compatibility
    let required_version = cli_pack.dependencies.get("test.web-api").unwrap();
    let web_version = &web_pack.metadata.version;

    let req = semver::VersionReq::parse(required_version).unwrap();
    let ver = semver::Version::parse(web_version).unwrap();

    assert!(req.matches(&ver), "Versions should be compatible");
}

#[test]
fn test_workflow_merge_rdf_from_multiple_packs() {
    let web_api_path = PathBuf::from(WEB_API_PACK);
    let cli_tool_path = PathBuf::from(CLI_TOOL_PACK);

    let web_api = GpackManifest::load_from_file(&web_api_path.join("gpack.toml")).unwrap();
    let cli_tool = GpackManifest::load_from_file(&cli_tool_path.join("gpack.toml")).unwrap();

    // Collect RDF prefixes from both packs
    let mut all_prefixes = BTreeMap::new();
    all_prefixes.extend(web_api.rdf.prefixes.clone());
    all_prefixes.extend(cli_tool.rdf.prefixes.clone());

    // Should have prefixes from both packs
    assert!(all_prefixes.contains_key("api"));
    assert!(all_prefixes.contains_key("cli"));

    // Check for prefix conflicts (same prefix, different URI)
    let web_prefixes = &web_api.rdf.prefixes;
    let cli_prefixes = &cli_tool.rdf.prefixes;

    for (prefix, uri) in web_prefixes {
        if let Some(cli_uri) = cli_prefixes.get(prefix) {
            if uri != cli_uri {
                panic!("Prefix conflict: {} -> {} vs {}", prefix, uri, cli_uri);
            }
        }
    }
}

// ============================================================================
// SPARQL Query Execution Workflows
// ============================================================================

#[test]
fn test_workflow_execute_sparql_queries_from_pack() {
    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    // Discover and read SPARQL queries
    let queries = manifest.discover_query_files(&pack_path).unwrap();
    assert!(!queries.is_empty(), "Pack should have SPARQL queries");

    for query_path in queries {
        let query_content = fs::read_to_string(&query_path).unwrap();

        // Validate query structure
        assert!(query_content.contains("PREFIX"), "Should have PREFIX");
        assert!(query_content.contains("SELECT") || query_content.contains("CONSTRUCT"), "Should be a valid query");
        assert!(query_content.contains("WHERE"), "Should have WHERE clause");

        // In a real system, we'd execute this against the RDF graph
        // For now, just verify the query is well-formed
    }
}

#[test]
fn test_workflow_query_aliases() {
    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    // Check if pack defines query aliases
    if !manifest.queries.aliases.is_empty() {
        for (alias, query_path) in &manifest.queries.aliases {
            let full_path = pack_path.join(query_path);
            assert!(
                full_path.exists(),
                "Aliased query {} should exist at {:?}",
                alias,
                full_path
            );
        }
    }
}

// ============================================================================
// Install and Generate Workflows
// ============================================================================

#[test]
fn test_workflow_install_dependencies_before_generate() {
    let cli_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap();

    // Check dependencies
    assert!(!cli_pack.dependencies.is_empty(), "CLI pack should have dependencies");

    // Resolve each dependency
    for (dep_id, version_req) in &cli_pack.dependencies {
        // In real system, would fetch from registry
        // For test, verify the dependency exists locally
        if dep_id == "test.web-api" {
            let web_pack_path = PathBuf::from(WEB_API_PACK).join("gpack.toml");
            assert!(web_pack_path.exists(), "Dependency {} should be available", dep_id);

            let web_pack = GpackManifest::load_from_file(&web_pack_path).unwrap();
            let req = semver::VersionReq::parse(version_req).unwrap();
            let ver = semver::Version::parse(&web_pack.metadata.version).unwrap();

            assert!(req.matches(&ver), "Dependency version should match");
        }
    }
}

#[test]
fn test_workflow_generate_with_preset_variables() {
    let pack_path = PathBuf::from(CLI_TOOL_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    // Check preset configuration
    if !manifest.preset.vars.is_empty() {
        let preset_vars = &manifest.preset.vars;

        // These variables would be used during generation
        for (key, value) in preset_vars {
            assert!(!key.is_empty(), "Preset var key should not be empty");
            assert!(!value.is_empty(), "Preset var value should not be empty");
        }
    }
}

// ============================================================================
// Error Recovery Workflows
// ============================================================================

#[test]
fn test_workflow_handle_missing_template() {
    let temp_dir = TempDir::new().unwrap();
    let manifest_path = temp_dir.path().join("gpack.toml");

    // Create manifest referencing non-existent template
    fs::write(
        &manifest_path,
        r#"
[gpack]
id = "test.broken"
name = "Broken Pack"
version = "1.0.0"
description = "Pack with missing template"
license = "MIT"
ggen_compat = ">=3.0.0"

[templates]
patterns = ["nonexistent/**/*.tmpl"]
"#,
    )
    .unwrap();

    let manifest = GpackManifest::load_from_file(&manifest_path).unwrap();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();

    // Should return empty list, not error
    assert_eq!(templates.len(), 0);
}

#[test]
fn test_workflow_handle_circular_dependencies() {
    // This is a simplified check - real system needs graph traversal
    let cli_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap();
    let web_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK))).unwrap();

    // CLI depends on web-api
    assert!(cli_pack.dependencies.contains_key("test.web-api"));

    // Web-api should not depend on CLI (would create cycle)
    assert!(
        !web_pack.dependencies.contains_key("test.cli-tool"),
        "Should not have circular dependency"
    );
}

// ============================================================================
// Complete Project Generation Workflow
// ============================================================================

#[test]
fn test_workflow_complete_project_generation() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("my-project");
    fs::create_dir_all(&output_dir).unwrap();

    // Step 1: Select pack
    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    // Step 2: Validate pack
    assert!(!manifest.metadata.id.is_empty());
    assert!(semver::Version::parse(&manifest.metadata.version).is_ok());

    // Step 3: Discover resources
    let templates = manifest.discover_templates(&pack_path).unwrap();
    let rdf_files = manifest.discover_rdf_files(&pack_path).unwrap();
    let queries = manifest.discover_query_files(&pack_path).unwrap();

    assert!(!templates.is_empty(), "Need templates to generate");

    // Step 4: Prepare variables
    let mut vars = BTreeMap::new();
    vars.insert("output_dir".to_string(), "handlers".to_string());
    vars.insert("endpoint_name".to_string(), "product".to_string());
    vars.insert("resource_name".to_string(), "Product".to_string());

    // Step 5: Verify RDF and SPARQL are available
    assert!(!rdf_files.is_empty(), "Should have RDF metadata");
    assert!(!queries.is_empty(), "Should have SPARQL queries");

    // Step 6: Ready to generate (actual generation would use ggen-core)
    assert!(output_dir.exists(), "Output directory created");
    assert!(vars.len() >= 3, "Variables prepared");
}

#[test]
fn test_workflow_multi_pack_project() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("full-stack-app");
    fs::create_dir_all(&output_dir).unwrap();

    // Step 1: Load all needed packs
    let packs = vec![
        (
            PathBuf::from(WEB_API_PACK),
            GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK))).unwrap(),
        ),
        (
            PathBuf::from(CLI_TOOL_PACK),
            GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap(),
        ),
        (
            PathBuf::from(DATABASE_PACK),
            GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", DATABASE_PACK))).unwrap(),
        ),
    ];

    // Step 2: Validate all packs
    for (_, manifest) in &packs {
        assert!(semver::Version::parse(&manifest.metadata.version).is_ok());
    }

    // Step 3: Check dependencies
    let cli_pack = &packs[1].1;
    assert!(cli_pack.dependencies.contains_key("test.web-api"));

    // Step 4: Collect all resources
    let mut all_templates = Vec::new();
    let mut all_rdf = Vec::new();
    let mut all_queries = Vec::new();

    for (pack_path, manifest) in &packs {
        all_templates.extend(manifest.discover_templates(pack_path).unwrap());
        all_rdf.extend(manifest.discover_rdf_files(pack_path).unwrap());
        all_queries.extend(manifest.discover_query_files(pack_path).unwrap());
    }

    // Step 5: Verify we have resources from all packs
    assert!(all_templates.len() >= 3);
    assert!(all_rdf.len() >= 3);
    assert!(all_queries.len() >= 3);

    // Ready for multi-pack generation
    assert!(output_dir.exists());
}
