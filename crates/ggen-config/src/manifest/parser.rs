//! ggen.toml manifest parser
//!
//! Parses TOML manifests into strongly-typed `GgenManifest` structures.
//!
//! Ported from `ggen-core/src/manifest/parser.rs` (specs/014-ggen-core-replacement,
//! docs/jira/v26.7.16/05-MANIFEST-CONFIG-PORT.md). `Result<T>`/`Error` (a plain
//! string-message type, no `thiserror`) rewritten to this crate's `ConfigError`-based
//! `Result`, per that ticket's own note that porting `manifest/*` verbatim alongside
//! `ConfigError` would leave two incompatible error philosophies in one crate.

use crate::manifest::types::GgenManifest;
use crate::manifest::validation::ManifestValidator;
use crate::{ConfigError, Result};
use std::path::Path;

/// Parser for ggen.toml manifest files
pub struct ManifestParser;

impl ManifestParser {
    /// Parse a ggen.toml manifest from the given path
    ///
    /// # Arguments
    /// * `path` - Path to ggen.toml file
    ///
    /// # Errors
    /// Returns [`ConfigError::Io`] if the file cannot be read, or
    /// [`ConfigError::TomlParse`] if its contents are not valid TOML / don't
    /// match the [`GgenManifest`] schema.
    ///
    /// # Example
    /// ```rust,no_run
    /// use ggen_config::manifest::ManifestParser;
    /// use std::path::Path;
    ///
    /// let manifest = ManifestParser::parse(Path::new("ggen.toml"))?;
    /// println!("Project: {}", manifest.project.name);
    /// # Ok::<(), ggen_config::ConfigError>(())
    /// ```
    pub fn parse(path: &Path) -> Result<GgenManifest> {
        let content = std::fs::read_to_string(path)?;
        Self::parse_str(&content)
    }

    /// Parse a ggen.toml manifest and validate it in one step.
    ///
    /// This is the preferred entry point for all production callers. It
    /// combines TOML parsing with semantic validation (file existence, required
    /// fields, dependency checks), emitting a hard error at the first missing
    /// file so that misconfigured manifests are caught at load time rather than
    /// silently producing empty generation output.
    ///
    /// # Arguments
    /// * `path` - Path to ggen.toml file (used as base for relative paths)
    ///
    /// # Errors
    /// Returns a descriptive [`ConfigError::Validation`] if any `imports` path,
    /// query file, or template file does not exist on disk. Example:
    /// `Ontology import not found: /project/domain/base.ttl`
    pub fn parse_and_validate(path: &Path) -> Result<GgenManifest> {
        let manifest = Self::parse(path)?;
        let base_path = path.parent().unwrap_or_else(|| Path::new("."));
        ManifestValidator::new(&manifest, base_path).validate()?;
        Ok(manifest)
    }

    /// Parse a ggen.toml manifest from a string
    ///
    /// # Arguments
    /// * `content` - TOML content string
    ///
    /// # Errors
    /// Returns [`ConfigError::TomlParse`] if `content` is not valid TOML or
    /// doesn't match the [`GgenManifest`] schema.
    pub fn parse_str(content: &str) -> Result<GgenManifest> {
        toml::from_str(content).map_err(ConfigError::TomlParse)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal_manifest() {
        let toml = r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "domain/model.ttl"

[generation]
rules = []
"#;

        let manifest = ManifestParser::parse_str(toml).expect("Should parse");
        assert_eq!(manifest.project.name, "test-project");
        assert_eq!(manifest.project.version, "1.0.0");
    }

    #[test]
    fn test_parse_full_manifest() {
        let toml = r#"
[project]
name = "my-domain"
version = "1.0.0"
description = "My domain model"

[ontology]
source = "domain/model.ttl"
imports = ["domain/base.ttl"]
base_iri = "http://example.org/"

[ontology.prefixes]
code = "http://ggen.dev/code#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"

[[inference.rules]]
name = "auditable_fields"
description = "Add timestamps to auditable entities"
construct = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }"
order = 1

[[generation.rules]]
name = "structs"
query = { file = "queries/structs.sparql" }
template = { file = "templates/struct.tera" }
output_file = "src/models/{{name}}.rs"
skip_empty = true

[generation]
max_sparql_timeout_ms = 10000
require_audit_trail = true
output_dir = "."

[validation]
shacl = ["shapes/domain.ttl"]
validate_syntax = true
no_unsafe = true
"#;

        let manifest = ManifestParser::parse_str(toml).expect("Should parse");
        assert_eq!(manifest.project.name, "my-domain");
        assert_eq!(manifest.inference.rules.len(), 1);
        assert_eq!(manifest.inference.rules[0].name, "auditable_fields");
        assert_eq!(manifest.generation.rules.len(), 1);
        assert!(manifest.validation.validate_syntax);
    }

    #[test]
    fn test_parse_missing_required_field() {
        let toml = r#"
[project]
name = "test"
# missing version

[ontology]
source = "test.ttl"

[generation]
rules = []
"#;

        let result = ManifestParser::parse_str(toml);
        assert!(result.is_err());
    }
}
