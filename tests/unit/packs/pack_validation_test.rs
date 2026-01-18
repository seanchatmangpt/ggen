//! Data validation tests for pack metadata
//!
//! Ensures all pack metadata is valid and consistent

use ggen_core::gpack::{GpackManifest, GpackMetadata};
use std::collections::BTreeMap;
use tempfile::TempDir;

// ============================================================================
// Metadata Field Validation
// ============================================================================

#[test]
fn test_validate_pack_id_format() {
    // Valid reverse-domain notation
    let valid_ids = vec![
        "org.example.pack",
        "io.github.user.cli-tool",
        "com.company.product.v2",
        "net.domain.pack-name_123",
    ];

    for id in valid_ids {
        assert!(is_valid_pack_id(id), "ID '{}' should be valid", id);
    }
}

#[test]
fn test_validate_version_semver() {
    let valid_versions = vec!["1.0.0", "2.3.4", "1.0.0-alpha", "1.0.0+build.123"];

    for version in valid_versions {
        assert!(
            semver::Version::parse(version).is_ok(),
            "Version '{}' should be valid semver",
            version
        );
    }
}

#[test]
fn test_validate_license_spdx() {
    let valid_licenses = vec![
        "MIT",
        "Apache-2.0",
        "MIT OR Apache-2.0",
        "GPL-3.0-only",
        "BSD-3-Clause",
    ];

    for license in valid_licenses {
        assert!(is_valid_license(license), "License '{}' should be valid SPDX", license);
    }
}

#[test]
fn test_validate_ggen_compat_version_req() {
    let valid_reqs = vec![">=3.0.0", ">=3.0.0 <4.0.0", "^3.2.0", "~3.2.4", "*"];

    for req in valid_reqs {
        assert!(
            semver::VersionReq::parse(req).is_ok(),
            "Version requirement '{}' should be valid",
            req
        );
    }
}

#[test]
fn test_validate_metadata_required_fields() {
    let metadata = GpackMetadata {
        id: "test.pack".to_string(),
        name: "Test Pack".to_string(),
        version: "1.0.0".to_string(),
        description: "A test pack".to_string(),
        license: "MIT".to_string(),
        ggen_compat: ">=3.0.0".to_string(),
    };

    // All required fields are non-empty
    assert!(!metadata.id.is_empty());
    assert!(!metadata.name.is_empty());
    assert!(!metadata.version.is_empty());
    assert!(!metadata.description.is_empty());
    assert!(!metadata.license.is_empty());
    assert!(!metadata.ggen_compat.is_empty());

    // Fields are valid
    assert!(is_valid_pack_id(&metadata.id));
    assert!(semver::Version::parse(&metadata.version).is_ok());
    assert!(semver::VersionReq::parse(&metadata.ggen_compat).is_ok());
}

// ============================================================================
// Dependency Validation
// ============================================================================

#[test]
fn test_validate_dependency_versions() {
    let toml = r#"
[gpack]
id = "test.deps"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[dependencies]
"org.example.pack1" = "^1.0.0"
"org.example.pack2" = "~2.3.4"
"org.example.pack3" = ">=1.0.0 <2.0.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();

    for (dep_id, version_req) in &manifest.dependencies {
        assert!(is_valid_pack_id(dep_id), "Dependency ID '{}' should be valid", dep_id);
        assert!(
            semver::VersionReq::parse(version_req).is_ok(),
            "Dependency version requirement '{}' should be valid",
            version_req
        );
    }
}

#[test]
fn test_detect_no_self_dependency() {
    let toml = r#"
[gpack]
id = "test.pack"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[dependencies]
"other.pack" = "^1.0.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();

    // Pack should not depend on itself
    assert!(
        !manifest.dependencies.contains_key(&manifest.metadata.id),
        "Pack should not have self-dependency"
    );
}

// ============================================================================
// RDF Configuration Validation
// ============================================================================

#[test]
fn test_validate_rdf_base_uri() {
    let toml = r#"
[gpack]
id = "test.rdf"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[rdf]
base = "https://example.org/ontology/"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();
    let base = manifest.rdf.base.unwrap();

    assert!(base.starts_with("http://") || base.starts_with("https://"));
    assert!(base.ends_with('/'), "Base URI should end with /");
}

#[test]
fn test_validate_rdf_prefixes() {
    let toml = r#"
[gpack]
id = "test.prefixes"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[rdf.prefixes]
ex = "https://example.org/"
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
skos = "http://www.w3.org/2004/02/skos/core#"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();

    for (prefix, uri) in &manifest.rdf.prefixes {
        assert!(!prefix.is_empty(), "Prefix should not be empty");
        assert!(
            uri.starts_with("http://") || uri.starts_with("https://"),
            "Prefix URI should be valid HTTP(S) URL"
        );
    }
}

#[test]
fn test_validate_inline_rdf_syntax() {
    let toml = r#"
[gpack]
id = "test.inline"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[rdf]
inline = [
    "@prefix ex: <https://example.org/> .",
    "ex:Thing a ex:Class ."
]
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();

    for turtle in &manifest.rdf.inline {
        // Basic Turtle syntax check
        assert!(
            turtle.contains("@prefix") || turtle.contains("a ") || turtle.contains(":"),
            "Inline RDF should look like Turtle syntax"
        );
    }
}

// ============================================================================
// Pattern Validation
// ============================================================================

#[test]
fn test_validate_glob_patterns() {
    let toml = r#"
[gpack]
id = "test.patterns"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[templates]
patterns = ["templates/**/*.tmpl", "custom/*.tera"]

[rdf]
patterns = ["data/**/*.ttl"]

[queries]
patterns = ["sparql/*.rq"]
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();

    // Validate template patterns
    for pattern in &manifest.templates.patterns {
        assert!(is_valid_glob_pattern(pattern), "Pattern '{}' should be valid glob", pattern);
    }

    // Validate RDF patterns
    for pattern in &manifest.rdf.patterns {
        assert!(is_valid_glob_pattern(pattern), "Pattern '{}' should be valid glob", pattern);
    }

    // Validate query patterns
    for pattern in &manifest.queries.patterns {
        assert!(is_valid_glob_pattern(pattern), "Pattern '{}' should be valid glob", pattern);
    }
}

// ============================================================================
// Cross-Field Validation
// ============================================================================

#[test]
fn test_validate_dependency_version_constraints() {
    let toml = r#"
[gpack]
id = "test.constraints"
name = "Test"
version = "2.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[dependencies]
"old.pack" = "^1.0.0"
"newer.pack" = "^2.5.0"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();

    for (dep_id, version_req) in &manifest.dependencies {
        let req = semver::VersionReq::parse(version_req).unwrap();

        // Ensure pack doesn't depend on a different version of itself
        if dep_id == &manifest.metadata.id {
            let own_version = semver::Version::parse(&manifest.metadata.version).unwrap();
            assert!(
                req.matches(&own_version),
                "Self-dependency should match own version"
            );
        }
    }
}

#[test]
fn test_validate_query_aliases_reference_patterns() {
    let toml = r#"
[gpack]
id = "test.aliases"
name = "Test"
version = "1.0.0"
description = "Test"
license = "MIT"
ggen_compat = ">=3.0.0"

[queries]
patterns = ["queries/**/*.sparql"]

[queries.aliases]
find_components = "queries/components.sparql"
"#;

    let manifest: GpackManifest = toml::from_str(toml).unwrap();

    // Aliases should point to files matching the patterns
    for (alias, path) in &manifest.queries.aliases {
        assert!(!alias.is_empty(), "Alias should not be empty");
        assert!(!path.is_empty(), "Alias path should not be empty");
        assert!(path.ends_with(".sparql") || path.ends_with(".rq"));
    }
}

// ============================================================================
// Complete Pack Validation
// ============================================================================

#[test]
fn test_validate_complete_pack() {
    let toml = r#"
[gpack]
id = "com.example.complete"
name = "Complete Pack"
version = "1.2.3"
description = "A complete, valid pack"
license = "MIT"
ggen_compat = ">=3.0.0 <4.0.0"

[dependencies]
"org.example.dep1" = "^1.0"
"org.example.dep2" = "~2.3.4"

[templates]
patterns = ["templates/**/*.tmpl"]
includes = ["macros/**/*.tera"]

[rdf]
base = "https://example.com/ontology/"

[rdf.prefixes]
ex = "https://example.com/"
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

[queries]
patterns = ["queries/**/*.sparql"]

[queries.aliases]
find_all = "queries/find_all.sparql"

[preset]

[preset.vars]
author = "Example Corp"
license = "MIT"
"#;

    let result: Result<GpackManifest, _> = toml::from_str(toml);
    assert!(result.is_ok(), "Complete pack should parse successfully");

    let manifest = result.unwrap();

    // Validate all sections
    assert!(is_valid_pack_id(&manifest.metadata.id));
    assert!(semver::Version::parse(&manifest.metadata.version).is_ok());
    assert!(semver::VersionReq::parse(&manifest.metadata.ggen_compat).is_ok());
    assert!(!manifest.metadata.description.is_empty());

    // Validate dependencies
    assert!(!manifest.dependencies.is_empty());
    for (_, version_req) in &manifest.dependencies {
        assert!(semver::VersionReq::parse(version_req).is_ok());
    }

    // Validate RDF config
    assert!(manifest.rdf.base.is_some());
    assert!(!manifest.rdf.prefixes.is_empty());

    // Validate queries
    assert!(!manifest.queries.patterns.is_empty());
    assert!(!manifest.queries.aliases.is_empty());
}

// ============================================================================
// Error Detection Tests
// ============================================================================

#[test]
fn test_detect_invalid_pack_structure() {
    let invalid_cases = vec![
        // Missing required field
        r#"
[gpack]
id = "test.incomplete"
name = "Test"
version = "1.0.0"
# Missing description, license, ggen_compat
"#,
        // Invalid TOML
        "{ not valid toml }",
    ];

    for (i, case) in invalid_cases.iter().enumerate() {
        let result: Result<GpackManifest, _> = toml::from_str(case);
        assert!(result.is_err(), "Case {} should fail parsing", i);
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn is_valid_pack_id(id: &str) -> bool {
    // Basic validation: not empty, reasonable length
    !id.is_empty() && id.len() < 256
}

fn is_valid_license(license: &str) -> bool {
    // Common SPDX identifiers
    let common_licenses = [
        "MIT",
        "Apache-2.0",
        "GPL-3.0-only",
        "BSD-3-Clause",
        "ISC",
        "MPL-2.0",
    ];

    common_licenses.iter().any(|&l| license.contains(l)) || license.contains(" OR ")
}

fn is_valid_glob_pattern(pattern: &str) -> bool {
    // Basic check: not empty, contains valid glob characters
    !pattern.is_empty() && (pattern.contains('*') || pattern.contains('/'))
}
