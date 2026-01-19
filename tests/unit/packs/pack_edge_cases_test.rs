//! Edge case tests for pack system
//!
//! Tests unusual, malformed, and boundary conditions

use ggen_core::gpack::{GpackManifest, GpackMetadata};
use std::collections::BTreeMap;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use tempfile::TempDir;

// ============================================================================
// Invalid Pack IDs
// ============================================================================

#[test]
fn test_pack_id_with_spaces() {
    let toml = r#"
[gpack]
id = "test with spaces"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"
"#;

    // Should parse (validation happens separately)
    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert_eq!(manifest.metadata.id, "test with spaces");
}

#[test]
fn test_pack_id_very_long() {
    let long_id = format!("org.{}.pack", "x".repeat(200));
    let toml = format!(
        r#"
[gpack]
id = "{}"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"
"#,
        long_id
    );

    let manifest: GpackManifest = toml::from_str(&toml).unwrap();
    assert_eq!(manifest.metadata.id, long_id);
}

#[test]
fn test_pack_id_with_special_characters() {
    let toml = r#"
[gpack]
id = "test.pack-v2_beta.🚀"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert!(manifest.metadata.id.contains("🚀"));
}

// ============================================================================
// Version Edge Cases
// ============================================================================

#[test]
fn test_version_with_prerelease() {
    let toml = r#"
[gpack]
id = "test.prerelease"
name = "Test"
version = "1.0.0-alpha.1"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert_eq!(manifest.metadata.version, "1.0.0-alpha.1");
    assert!(semver::Version::parse(&manifest.metadata.version).is_ok());
}

#[test]
fn test_version_with_build_metadata() {
    let toml = r#"
[gpack]
id = "test.build"
name = "Test"
version = "1.0.0+build.123"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert!(semver::Version::parse(&manifest.metadata.version).is_ok());
}

#[test]
fn test_version_invalid_semver() {
    let toml = r#"
[gpack]
id = "test.invalid"
name = "Test"
version = "not-a-version"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert!(semver::Version::parse(&manifest.metadata.version).is_err());
}

// ============================================================================
// Circular Dependencies
// ============================================================================

#[test]
fn test_detect_self_dependency() {
    let toml = r#"
[gpack]
id = "test.self-dep"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[dependencies]
"test.self-dep" = "^1.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert!(manifest.dependencies.contains_key("test.self-dep"));

    // Check for self-dependency
    let has_self_dep = manifest.dependencies.contains_key(&manifest.metadata.id);
    assert!(has_self_dep, "Should detect self-dependency");
}

#[test]
fn test_transitive_circular_dependency() {
    // Pack A depends on B, B depends on C, C depends on A (cycle)
    // This requires a dependency resolver to detect
    let pack_a = r#"
[gpack]
id = "test.a"
name = "A"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[dependencies]
"test.b" = "^1.0"
"#;

    let pack_b = r#"
[gpack]
id = "test.b"
name = "B"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[dependencies]
"test.c" = "^1.0"
"#;

    let pack_c = r#"
[gpack]
id = "test.c"
name = "C"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[dependencies]
"test.a" = "^1.0"
"#;

    let manifest_a: GpackManifest = toml::from_str(pack_a).unwrap();
    let manifest_b: GpackManifest = toml::from_str(pack_b).unwrap();
    let manifest_c: GpackManifest = toml::from_str(pack_c).unwrap();

    // Would need graph traversal to detect cycle
    assert!(manifest_a.dependencies.contains_key("test.b"));
    assert!(manifest_b.dependencies.contains_key("test.c"));
    assert!(manifest_c.dependencies.contains_key("test.a"));
}

// ============================================================================
// Missing or Invalid Files
// ============================================================================

#[test]
fn test_missing_template_files() {
    let temp_dir = TempDir::new().unwrap();
    let manifest_path = temp_dir.path().join("gpack.toml");

    fs::write(
        &manifest_path,
        r#"
[gpack]
id = "test.missing"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[templates]
patterns = ["templates/**/*.tmpl"]
"#,
    )
    .unwrap();

    let manifest = GpackManifest::load_from_file(&manifest_path).unwrap();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();

    // Should return empty, not error
    assert_eq!(templates.len(), 0);
}

#[test]
fn test_template_with_no_frontmatter() {
    let temp_dir = TempDir::new().unwrap();
    create_file(temp_dir.path(), "templates/bad.tmpl", "No frontmatter here!");

    let manifest = minimal_manifest();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();

    assert_eq!(templates.len(), 1);

    // File exists but may fail during generation
    let content = fs::read_to_string(&templates[0]).unwrap();
    assert!(!content.starts_with("---"));
}

#[test]
fn test_malformed_rdf_file() {
    let temp_dir = TempDir::new().unwrap();
    create_file(
        temp_dir.path(),
        "templates/api/graphs/bad.ttl",
        "This is not valid Turtle syntax { [ ] }",
    );

    let manifest = minimal_manifest();
    let rdf_files = manifest.discover_rdf_files(temp_dir.path()).unwrap();

    assert_eq!(rdf_files.len(), 1);
    // File is discovered but parsing would fail
}

#[test]
fn test_invalid_sparql_query() {
    let temp_dir = TempDir::new().unwrap();
    create_file(
        temp_dir.path(),
        "templates/api/queries/bad.sparql",
        "NOT A VALID SPARQL QUERY!!!",
    );

    let manifest = minimal_manifest();
    let queries = manifest.discover_query_files(temp_dir.path()).unwrap();

    assert_eq!(queries.len(), 1);
    // File is discovered but execution would fail
}

// ============================================================================
// Conflicting Resources
// ============================================================================

#[test]
fn test_duplicate_template_names() {
    let temp_dir = TempDir::new().unwrap();

    // Create two templates with same name in different dirs
    create_file(temp_dir.path(), "templates/api/handler.tmpl", "API version");
    create_file(temp_dir.path(), "templates/cli/handler.tmpl", "CLI version");

    let manifest = minimal_manifest();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();

    assert_eq!(templates.len(), 2, "Should find both templates");
    // Would need naming resolution during generation
}

#[test]
fn test_conflicting_rdf_prefixes() {
    let pack1_toml = r#"
[gpack]
id = "test.pack1"
name = "Pack 1"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[rdf.prefixes]
ex = "https://example.org/pack1/"
"#;

    let pack2_toml = r#"
[gpack]
id = "test.pack2"
name = "Pack 2"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[rdf.prefixes]
ex = "https://example.org/pack2/"
"#;

    let pack1: GpackManifest = toml::from_str(pack1_toml).unwrap();
    let pack2: GpackManifest = toml::from_str(pack2_toml).unwrap();

    let uri1 = pack1.rdf.prefixes.get("ex").unwrap();
    let uri2 = pack2.rdf.prefixes.get("ex").unwrap();

    assert_ne!(uri1, uri2, "Prefix conflict detected");
}

// ============================================================================
// Empty and Boundary Cases
// ============================================================================

#[test]
fn test_empty_pack() {
    let toml = r#"
[gpack]
id = "test.empty"
name = "Empty"
version = "1.0.0"
description = "Empty pack"
license = "MIT"
ggen_compat = ">=3.0.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    let temp_dir = TempDir::new().unwrap();

    assert_eq!(manifest.discover_templates(temp_dir.path()).unwrap().len(), 0);
    assert_eq!(manifest.discover_rdf_files(temp_dir.path()).unwrap().len(), 0);
    assert_eq!(manifest.discover_query_files(temp_dir.path()).unwrap().len(), 0);
}

#[test]
fn test_pack_with_only_dependencies() {
    let toml = r#"
[gpack]
id = "test.meta"
name = "Meta Pack"
version = "1.0.0"
description = "Only dependencies, no content"
license = "MIT"
ggen_compat = ">=3.0.0"

[dependencies]
"test.pack1" = "^1.0"
"test.pack2" = "^2.0"
"test.pack3" = "^3.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    assert_eq!(manifest.dependencies.len(), 3);

    let temp_dir = TempDir::new().unwrap();
    assert_eq!(manifest.discover_templates(temp_dir.path()).unwrap().len(), 0);
}

#[test]
fn test_zero_length_files() {
    let temp_dir = TempDir::new().unwrap();
    create_file(temp_dir.path(), "templates/empty.tmpl", "");

    let manifest = minimal_manifest();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();

    assert_eq!(templates.len(), 1);
    let content = fs::read_to_string(&templates[0]).unwrap();
    assert_eq!(content.len(), 0);
}

// ============================================================================
// Path and Encoding Edge Cases
// ============================================================================

#[test]
fn test_template_with_unicode_path() {
    let temp_dir = TempDir::new().unwrap();
    create_file(temp_dir.path(), "templates/日本語/test.tmpl", "content");
    create_file(temp_dir.path(), "templates/emoji-🚀/test.tmpl", "content");

    let manifest = minimal_manifest();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();

    assert!(templates.len() >= 2, "Should handle Unicode paths");
}

#[test]
fn test_very_deep_directory_nesting() {
    let temp_dir = TempDir::new().unwrap();
    let deep_path = "templates/a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p/test.tmpl";
    create_file(temp_dir.path(), deep_path, "deep content");

    let manifest = minimal_manifest();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();

    assert_eq!(templates.len(), 1, "Should handle deep nesting");
}

#[test]
fn test_symlink_handling() {
    // Symlinks require special handling on different platforms
    // This test verifies graceful handling
    let temp_dir = TempDir::new().unwrap();
    create_file(temp_dir.path(), "templates/real.tmpl", "content");

    // Try to create symlink (may fail on Windows)
    #[cfg(unix)]
    {
        use std::os::unix::fs as unix_fs;
        let real_path = temp_dir.path().join("templates/real.tmpl");
        let link_path = temp_dir.path().join("templates/link.tmpl");
        let _ = unix_fs::symlink(&real_path, &link_path);
    }

    let manifest = minimal_manifest();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();

    assert!(templates.len() >= 1, "Should handle symlinks gracefully");
}

// ============================================================================
// Performance Edge Cases
// ============================================================================

#[test]
fn test_very_large_manifest() {
    let mut deps = BTreeMap::new();
    for i in 0..1000 {
        deps.insert(format!("test.dep{}", i), "^1.0".to_string());
    }

    let manifest = GpackManifest {
        metadata: GpackMetadata {
            id: "test.large".to_string(),
            name: "Large Pack".to_string(),
            version: "1.0.0".to_string(),
            description: "Pack with many dependencies".to_string(),
            license: "MIT".to_string(),
            ggen_compat: ">=3.0.0".to_string(),
        },
        dependencies: deps,
        templates: Default::default(),
        macros: Default::default(),
        rdf: Default::default(),
        queries: Default::default(),
        shapes: Default::default(),
        preset: Default::default(),
    };

    assert_eq!(manifest.dependencies.len(), 1000);
}

#[test]
fn test_many_small_files() {
    let temp_dir = TempDir::new().unwrap();

    // Create 100 small template files
    for i in 0..100 {
        create_file(temp_dir.path(), &format!("templates/file{}.tmpl", i), "small");
    }

    let manifest = minimal_manifest();
    let start = std::time::Instant::now();
    let templates = manifest.discover_templates(temp_dir.path()).unwrap();
    let duration = start.elapsed();

    assert_eq!(templates.len(), 100);
    assert!(
        duration.as_millis() < 200,
        "Should discover 100 files quickly (took {}ms)",
        duration.as_millis()
    );
}

// ============================================================================
// Helper Functions
// ============================================================================

fn minimal_manifest() -> GpackManifest {
    GpackManifest {
        metadata: GpackMetadata {
            id: "test.minimal".to_string(),
            name: "Test".to_string(),
            version: "1.0.0".to_string(),
            description: "Test".to_string(),
            license: "MIT".to_string(),
            ggen_compat: ">=3.0.0".to_string(),
        },
        dependencies: BTreeMap::new(),
        templates: Default::default(),
        macros: Default::default(),
        rdf: Default::default(),
        queries: Default::default(),
        shapes: Default::default(),
        preset: Default::default(),
    }
}

fn create_file(base: &std::path::Path, relative_path: &str, content: &str) {
    let path = base.join(relative_path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    let mut file = fs::File::create(path).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}
