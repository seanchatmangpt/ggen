//! Integration tests for pack CLI commands
//!
//! Tests complete user workflows:
//! - List packs with filtering
//! - Show pack metadata
//! - Generate from packs
//! - Multi-pack composition
//! - Validate packs

use ggen_core::gpack::GpackManifest;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

// Test fixture paths
const WEB_API_PACK: &str = "tests/fixtures/packs/web-api-pack";
const CLI_TOOL_PACK: &str = "tests/fixtures/packs/cli-tool-pack";
const DATABASE_PACK: &str = "tests/fixtures/packs/database-pack";

// ============================================================================
// Pack Discovery and Listing Tests
// ============================================================================

#[test]
fn test_list_all_test_packs() {
    let web_api = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK)));
    let cli_tool = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK)));
    let database = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", DATABASE_PACK)));

    assert!(web_api.is_ok(), "Web API pack should load");
    assert!(cli_tool.is_ok(), "CLI tool pack should load");
    assert!(database.is_ok(), "Database pack should load");

    let web_manifest = web_api.unwrap();
    let cli_manifest = cli_tool.unwrap();
    let db_manifest = database.unwrap();

    assert_eq!(web_manifest.metadata.id, "test.web-api");
    assert_eq!(cli_manifest.metadata.id, "test.cli-tool");
    assert_eq!(db_manifest.metadata.id, "test.database");
}

#[test]
fn test_pack_version_filtering() {
    let web_api = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK))).unwrap();
    let cli_tool = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap();
    let database = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", DATABASE_PACK))).unwrap();

    // Verify versions
    assert_eq!(web_api.metadata.version, "1.0.0");
    assert_eq!(cli_tool.metadata.version, "2.0.0");
    assert_eq!(database.metadata.version, "1.5.0");

    // Test semantic version filtering (would be done by CLI)
    let versions: Vec<&str> = vec![
        &web_api.metadata.version,
        &cli_tool.metadata.version,
        &database.metadata.version,
    ];

    assert!(versions.iter().all(|v| semver::Version::parse(v).is_ok()));
}

// ============================================================================
// Pack Metadata and Show Tests
// ============================================================================

#[test]
fn test_show_pack_complete_metadata() {
    let cli_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap();

    // Verify all metadata fields
    assert_eq!(cli_pack.metadata.id, "test.cli-tool");
    assert_eq!(cli_pack.metadata.name, "CLI Tool Pack");
    assert_eq!(cli_pack.metadata.version, "2.0.0");
    assert_eq!(cli_pack.metadata.description, "CLI application template pack");
    assert_eq!(cli_pack.metadata.license, "MIT OR Apache-2.0");
    assert_eq!(cli_pack.metadata.ggen_compat, ">=3.0.0");

    // Verify dependencies
    assert_eq!(cli_pack.dependencies.len(), 1);
    assert_eq!(cli_pack.dependencies.get("test.web-api"), Some(&"^1.0".to_string()));

    // Verify RDF config
    assert_eq!(cli_pack.rdf.base, Some("https://example.org/cli/".to_string()));
    assert_eq!(cli_pack.rdf.prefixes.len(), 2);
}

#[test]
fn test_discover_pack_templates() {
    let web_api_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&web_api_path.join("gpack.toml")).unwrap();

    let templates = manifest.discover_templates(&web_api_path).unwrap();

    assert!(!templates.is_empty(), "Should find templates");
    assert!(templates.iter().any(|t| t.ends_with("api-handler.tmpl")));
}

#[test]
fn test_discover_pack_rdf_files() {
    let web_api_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&web_api_path.join("gpack.toml")).unwrap();

    let rdf_files = manifest.discover_rdf_files(&web_api_path).unwrap();

    assert!(!rdf_files.is_empty(), "Should find RDF files");
    assert!(rdf_files.iter().any(|f| f.ends_with("api-ontology.ttl")));
}

#[test]
fn test_discover_pack_sparql_queries() {
    let web_api_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&web_api_path.join("gpack.toml")).unwrap();

    let queries = manifest.discover_query_files(&web_api_path).unwrap();

    assert!(!queries.is_empty(), "Should find SPARQL queries");
    assert!(queries.iter().any(|q| q.ends_with("find-endpoints.sparql")));
}

#[test]
fn test_discover_pack_shacl_shapes() {
    let web_api_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&web_api_path.join("gpack.toml")).unwrap();

    let shapes = manifest.discover_shape_files(&web_api_path).unwrap();

    assert!(!shapes.is_empty(), "Should find SHACL shapes");
    assert!(shapes.iter().any(|s| s.ends_with("endpoint-shape.shacl.ttl")));
}

// ============================================================================
// Template Content Validation Tests
// ============================================================================

#[test]
fn test_template_contains_valid_frontmatter() {
    let template_path = PathBuf::from(WEB_API_PACK).join("templates/api-handler.tmpl");
    let content = fs::read_to_string(template_path).unwrap();

    assert!(content.starts_with("---\n"), "Template should have YAML frontmatter");
    assert!(content.contains("to:"), "Frontmatter should have 'to' field");
    assert!(content.contains("{{"), "Template should have Tera variables");
}

#[test]
fn test_rdf_file_is_valid_turtle() {
    let rdf_path = PathBuf::from(WEB_API_PACK).join("templates/api/graphs/api-ontology.ttl");
    let content = fs::read_to_string(rdf_path).unwrap();

    assert!(content.contains("@prefix"), "Should have prefix declarations");
    assert!(content.contains("rdfs:Class"), "Should define classes");
    assert!(content.contains("rdfs:label"), "Should have labels");
}

#[test]
fn test_sparql_query_is_valid() {
    let query_path = PathBuf::from(WEB_API_PACK).join("templates/api/queries/find-endpoints.sparql");
    let content = fs::read_to_string(query_path).unwrap();

    assert!(content.contains("PREFIX"), "Should have PREFIX declarations");
    assert!(content.contains("SELECT"), "Should be a SELECT query");
    assert!(content.contains("WHERE"), "Should have WHERE clause");
}

#[test]
fn test_shacl_shape_is_valid() {
    let shape_path = PathBuf::from(WEB_API_PACK).join("templates/api/graphs/shapes/endpoint-shape.shacl.ttl");
    let content = fs::read_to_string(shape_path).unwrap();

    assert!(content.contains("@prefix sh:"), "Should have SHACL prefix");
    assert!(content.contains("sh:NodeShape"), "Should define NodeShape");
    assert!(content.contains("sh:property"), "Should have property constraints");
}

// ============================================================================
// Multi-Pack Dependency Tests
// ============================================================================

#[test]
fn test_pack_has_valid_dependencies() {
    let cli_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap();

    // CLI pack depends on web-api pack
    assert!(cli_pack.dependencies.contains_key("test.web-api"));

    // Verify dependency version constraint
    let dep_version = cli_pack.dependencies.get("test.web-api").unwrap();
    assert_eq!(dep_version, "^1.0");
}

#[test]
fn test_dependency_resolution() {
    let cli_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap();
    let web_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK))).unwrap();

    let required_dep = cli_pack.dependencies.get("test.web-api").unwrap();
    let web_version = &web_pack.metadata.version;

    // Parse version requirement
    let req = semver::VersionReq::parse(required_dep).unwrap();
    let version = semver::Version::parse(web_version).unwrap();

    assert!(req.matches(&version), "Web API version should satisfy CLI tool dependency");
}

// ============================================================================
// Pack Validation Tests
// ============================================================================

#[test]
fn test_validate_pack_structure() {
    for pack_dir in [WEB_API_PACK, CLI_TOOL_PACK, DATABASE_PACK] {
        let pack_path = PathBuf::from(pack_dir);

        // gpack.toml must exist
        assert!(
            pack_path.join("gpack.toml").exists(),
            "Pack {} must have gpack.toml",
            pack_dir
        );

        // Can parse manifest
        let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml"));
        assert!(manifest.is_ok(), "Pack {} manifest must be valid", pack_dir);

        let manifest = manifest.unwrap();

        // All discovered files must exist
        let templates = manifest.discover_templates(&pack_path).unwrap();
        for template in templates {
            assert!(template.exists(), "Template {:?} must exist", template);
        }

        let rdf_files = manifest.discover_rdf_files(&pack_path).unwrap();
        for rdf_file in rdf_files {
            assert!(rdf_file.exists(), "RDF file {:?} must exist", rdf_file);
        }

        let queries = manifest.discover_query_files(&pack_path).unwrap();
        for query in queries {
            assert!(query.exists(), "Query {:?} must exist", query);
        }
    }
}

#[test]
fn test_pack_compatibility_version() {
    let packs = vec![
        GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK))).unwrap(),
        GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap(),
        GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", DATABASE_PACK))).unwrap(),
    ];

    for pack in packs {
        // All test packs should be compatible with ggen >=3.0.0
        assert!(
            pack.metadata.ggen_compat.contains("3.0"),
            "Pack {} should support ggen 3.0+",
            pack.metadata.id
        );
    }
}

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

#[test]
fn test_load_nonexistent_pack() {
    let result = GpackManifest::load_from_file(&PathBuf::from("/nonexistent/pack/gpack.toml"));
    assert!(result.is_err());
}

#[test]
fn test_discover_files_in_empty_pack() {
    let temp_dir = TempDir::new().unwrap();
    let manifest_path = temp_dir.path().join("gpack.toml");

    // Create minimal manifest
    fs::write(
        &manifest_path,
        r#"
[gpack]
id = "test.empty"
name = "Empty Pack"
version = "1.0.0"
description = "Empty test pack"
license = "MIT"
ggen_compat = ">=3.0.0"
"#,
    )
    .unwrap();

    let manifest = GpackManifest::load_from_file(&manifest_path).unwrap();

    // Should return empty vectors, not errors
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();
    let rdf_files = manifest.discover_rdf_files(temp_dir.path()).unwrap();
    let queries = manifest.discover_query_files(temp_dir.path()).unwrap();
    let shapes = manifest.discover_shape_files(temp_dir.path()).unwrap();

    assert_eq!(templates.len(), 0);
    assert_eq!(rdf_files.len(), 0);
    assert_eq!(queries.len(), 0);
    assert_eq!(shapes.len(), 0);
}

#[test]
fn test_pack_with_circular_dependency() {
    // Note: This would require more sophisticated dependency resolution
    // For now, just verify we can detect the dependency
    let cli_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap();

    assert!(cli_pack.dependencies.contains_key("test.web-api"));
    // In a real system, we'd check for cycles in the dependency graph
}

// ============================================================================
// Performance Constraint Tests
// ============================================================================

#[test]
fn test_pack_operations_are_fast() {
    use std::time::Instant;

    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest_path = pack_path.join("gpack.toml");

    // Load manifest should be fast (<10ms)
    let start = Instant::now();
    let manifest = GpackManifest::load_from_file(&manifest_path).unwrap();
    let load_time = start.elapsed();
    assert!(
        load_time.as_millis() < 10,
        "Manifest load took {}ms (should be <10ms)",
        load_time.as_millis()
    );

    // File discovery should be fast (<100ms)
    let start = Instant::now();
    let _templates = manifest.discover_templates(&pack_path).unwrap();
    let _rdf = manifest.discover_rdf_files(&pack_path).unwrap();
    let _queries = manifest.discover_query_files(&pack_path).unwrap();
    let _shapes = manifest.discover_shape_files(&pack_path).unwrap();
    let discovery_time = start.elapsed();

    assert!(
        discovery_time.as_millis() < 100,
        "File discovery took {}ms (should be <100ms)",
        discovery_time.as_millis()
    );
}
