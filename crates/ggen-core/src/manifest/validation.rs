//! Manifest validation module
//!
//! Validates parsed manifests for semantic correctness beyond TOML parsing.

use crate::manifest::types::{GgenManifest, QuerySource, TemplateSource};
use ggen_utils::error::{Error, Result};
use std::path::Path;

/// Validator for ggen.toml manifests
pub struct ManifestValidator<'a> {
    manifest: &'a GgenManifest,
    base_path: &'a Path,
}

impl<'a> ManifestValidator<'a> {
    /// Create a new validator for the given manifest
    ///
    /// # Arguments
    /// * `manifest` - The parsed manifest to validate
    /// * `base_path` - Base path for resolving relative file paths
    pub fn new(manifest: &'a GgenManifest, base_path: &'a Path) -> Self {
        Self {
            manifest,
            base_path,
        }
    }

    /// Run all validations
    ///
    /// # Returns
    /// * `Ok(())` - All validations passed
    /// * `Err(Error)` - First validation failure
    pub fn validate(&self) -> Result<()> {
        self.validate_project()?;
        self.validate_ontology()?;
        self.validate_inference_rules()?;
        self.validate_generation_rules()?;
        self.validate_shacl_paths()?;
        Ok(())
    }

    /// Validate project section
    fn validate_project(&self) -> Result<()> {
        if self.manifest.project.name.is_empty() {
            return Err(Error::new("project.name cannot be empty"));
        }
        if self.manifest.project.version.is_empty() {
            return Err(Error::new("project.version cannot be empty"));
        }
        Ok(())
    }

    /// Validate ontology configuration
    fn validate_ontology(&self) -> Result<()> {
        let source_path = self.base_path.join(&self.manifest.ontology.source);
        if !source_path.exists() {
            return Err(Error::new(&format!(
                "Ontology source not found: {}",
                source_path.display()
            )));
        }

        // Validate imports exist
        for import in &self.manifest.ontology.imports {
            let import_path = self.base_path.join(import);
            if !import_path.exists() {
                return Err(Error::new(&format!(
                    "Ontology import not found: {}",
                    import_path.display()
                )));
            }
        }

        Ok(())
    }

    /// Validate inference rules
    fn validate_inference_rules(&self) -> Result<()> {
        let mut seen_orders: Vec<i32> = Vec::new();

        for rule in &self.manifest.inference.rules {
            if rule.name.is_empty() {
                return Err(Error::new("inference.rules[].name cannot be empty"));
            }
            if rule.construct.is_empty() {
                return Err(Error::new(&format!(
                    "inference.rules[{}].construct cannot be empty",
                    rule.name
                )));
            }

            // Check for ORDER BY in CONSTRUCT (determinism warning)
            let construct_upper = rule.construct.to_uppercase();
            if !construct_upper.contains("ORDER BY") {
                log::warn!(
                    "Inference rule '{}' CONSTRUCT query lacks ORDER BY - may produce non-deterministic results",
                    rule.name
                );
            }

            // Check for duplicate order values (not an error, just tracked)
            if seen_orders.contains(&rule.order) {
                log::warn!(
                    "Inference rule '{}' has duplicate order value {}",
                    rule.name,
                    rule.order
                );
            }
            seen_orders.push(rule.order);
        }

        Ok(())
    }

    /// Validate generation rules
    fn validate_generation_rules(&self) -> Result<()> {
        for rule in &self.manifest.generation.rules {
            if rule.name.is_empty() {
                return Err(Error::new("generation.rules[].name cannot be empty"));
            }

            // Validate query source exists
            if let QuerySource::File { file } = &rule.query {
                let query_path = self.base_path.join(file);
                if !query_path.exists() {
                    return Err(Error::new(&format!(
                        "Query file not found for rule '{}': {}",
                        rule.name,
                        query_path.display()
                    )));
                }
            }

            // Validate template source exists
            if let TemplateSource::File { file } = &rule.template {
                let template_path = self.base_path.join(file);
                if !template_path.exists() {
                    return Err(Error::new(&format!(
                        "Template file not found for rule '{}': {}",
                        rule.name,
                        template_path.display()
                    )));
                }
            }

            // Validate output path pattern
            if rule.output_file.is_empty() {
                return Err(Error::new(&format!(
                    "generation.rules[{}].output_file cannot be empty",
                    rule.name
                )));
            }
        }

        Ok(())
    }

    /// Validate SHACL shape file paths
    fn validate_shacl_paths(&self) -> Result<()> {
        for shacl_path in &self.manifest.validation.shacl {
            let full_path = self.base_path.join(shacl_path);
            if !full_path.exists() {
                return Err(Error::new(&format!(
                    "SHACL shape file not found: {}",
                    full_path.display()
                )));
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::manifest::ManifestParser;

    fn create_test_manifest() -> GgenManifest {
        let toml = r#"
[project]
name = "test"
version = "1.0.0"

[ontology]
source = "Cargo.toml"  # Use existing file for test

[generation]
rules = []
"#;
        ManifestParser::parse_str(toml).unwrap()
    }

    #[test]
    fn test_validate_empty_project_name() {
        let toml = r#"
[project]
name = ""
version = "1.0.0"

[ontology]
source = "test.ttl"

[generation]
rules = []
"#;
        let manifest = ManifestParser::parse_str(toml).unwrap();
        let validator = ManifestValidator::new(&manifest, Path::new("."));
        assert!(validator.validate().is_err());
    }

    #[test]
    fn test_validate_missing_ontology() {
        let manifest = create_test_manifest();
        // Use a path where the ontology won't exist
        let validator = ManifestValidator::new(&manifest, Path::new("/nonexistent/path"));
        assert!(validator.validate().is_err());
    }
}
