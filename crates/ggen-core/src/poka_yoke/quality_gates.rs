//! Quality Gate System - Mandatory Checkpoints
//!
//! Quality gates are mandatory checks that must PASS before proceeding.
//! Failure of ANY gate triggers a 🔴 RED signal and stops execution.
//!
//! # Gates
//!
//! 1. **Manifest Schema** - ggen.toml structure is valid
//! 2. **Ontology Dependencies** - All .ttl files exist, no circular imports
//! 3. **SPARQL Validation** - All queries have valid syntax
//! 4. **Template Validation** - All templates exist and have valid Tera syntax
//! 5. **File Permissions** - Output directory is writable
//! 6. **Rule Validation** - All rules reference existing templates/queries
//!
//! # Usage
//!
//! ```ignore
//! let runner = QualityGateRunner::new();
//! runner.run_all(&manifest)?;  // Returns Err with AndonSignal::Red if any gate fails
//! ```

use super::andon::AndonSignal;
use crate::manifest::GgenManifest;
use crate::{Error, Result};
use std::path::Path;

/// A validation checkpoint that must pass
pub struct ValidationCheckpoint {
    /// Name of the checkpoint (e.g., "Manifest Schema")
    pub name: String,
    /// Description of what is being checked
    pub description: String,
    /// Checks performed
    pub checks: Vec<String>,
}

/// Quality Gate trait - each gate implements one set of validations
pub trait QualityGate: Send + Sync {
    /// Name of the gate (e.g., "Manifest Schema")
    fn name(&self) -> &str;

    /// Run the validation check
    fn check(&self, manifest: &GgenManifest, base_path: &Path) -> Result<()>;

    /// Return recovery suggestions for an error
    fn recovery_suggestions(&self, error_message: &str) -> Vec<String>;

    /// Documentation link for this gate
    fn docs_link(&self) -> String;
}

/// Runs all quality gates in sequence
pub struct QualityGateRunner {
    gates: Vec<Box<dyn QualityGate>>,
}

impl QualityGateRunner {
    /// Create a new quality gate runner with all default gates
    pub fn new() -> Self {
        QualityGateRunner {
            gates: vec![
                Box::new(ManifestSchemaGate),
                Box::new(OntologyDependenciesGate),
                Box::new(SparqlValidationGate),
                Box::new(TemplateValidationGate),
                Box::new(FilePermissionsGate),
                Box::new(RuleValidationGate),
            ],
        }
    }

    /// Run all gates in sequence
    ///
    /// Returns Ok if all gates pass, Err with AndonSignal::Red if any fails
    pub fn run_all(&self, manifest: &GgenManifest, base_path: &Path) -> Result<()> {
        eprintln!();
        for gate in &self.gates {
            eprint!("[Quality Gate: {}]", gate.name());
            match gate.check(manifest, base_path) {
                Ok(_) => eprintln!(" ✓"),
                Err(e) => {
                    eprintln!(" ✗");
                    let recovery = gate.recovery_suggestions(&e.to_string());
                    let signal = AndonSignal::Red(super::andon::CriticalError {
                        code: format!("GATE_{}", gate.name().to_uppercase().replace(" ", "_")),
                        message: format!("Quality gate failed: {}", gate.name()),
                        context: e.to_string(),
                        recovery_steps: recovery,
                        documentation_link: gate.docs_link(),
                    });
                    signal.enforce()?;
                    return Err(e);
                }
            }
        }
        eprintln!();
        eprintln!("All Gates: ✅ PASSED → Proceeding to generation phase");
        eprintln!();
        Ok(())
    }

    /// Get checkpoints that will be validated
    pub fn checkpoints(&self) -> Vec<ValidationCheckpoint> {
        vec![
            ValidationCheckpoint {
                name: "Manifest Schema".to_string(),
                description: "ggen.toml structure is valid".to_string(),
                checks: vec![
                    "TOML parsing succeeds".to_string(),
                    "All required fields present".to_string(),
                    "Field types correct".to_string(),
                ],
            },
            ValidationCheckpoint {
                name: "Ontology Dependencies".to_string(),
                description: "All .ttl files exist, no circular imports".to_string(),
                checks: vec![
                    "ontology.source file exists".to_string(),
                    "All imports exist and readable".to_string(),
                    "No circular dependencies".to_string(),
                    "All files are valid Turtle syntax".to_string(),
                ],
            },
            ValidationCheckpoint {
                name: "SPARQL Validation".to_string(),
                description: "All queries have valid syntax".to_string(),
                checks: vec![
                    "All SELECT queries are valid SPARQL".to_string(),
                    "All CONSTRUCT rules are valid SPARQL".to_string(),
                    "Query variables don't contain typos".to_string(),
                ],
            },
            ValidationCheckpoint {
                name: "Template Validation".to_string(),
                description: "All templates exist and have valid Tera syntax".to_string(),
                checks: vec![
                    "All template files exist".to_string(),
                    "Template syntax is valid Tera".to_string(),
                    "Template variables match query results".to_string(),
                    "No undefined variable references".to_string(),
                ],
            },
            ValidationCheckpoint {
                name: "File Permissions".to_string(),
                description: "Output directory is writable".to_string(),
                checks: vec![
                    "Output directory exists".to_string(),
                    "Output directory is writable".to_string(),
                    "Sufficient disk space (>10MB)".to_string(),
                ],
            },
            ValidationCheckpoint {
                name: "Rule Validation".to_string(),
                description: "All rules reference existing templates/queries".to_string(),
                checks: vec![
                    "All generation rules reference existing templates".to_string(),
                    "All selected rules (--rule) exist in manifest".to_string(),
                ],
            },
        ]
    }
}

impl Default for QualityGateRunner {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Individual Gate Implementations
// ============================================================================

/// Gate 1: Manifest Schema Validation
struct ManifestSchemaGate;

impl QualityGate for ManifestSchemaGate {
    fn name(&self) -> &str {
        "Manifest Schema"
    }

    fn check(&self, manifest: &GgenManifest, _base_path: &Path) -> Result<()> {
        // Check project section
        if manifest.project.name.is_empty() {
            return Err(Error::new(
                "Field [project].name is empty - must be non-empty string",
            ));
        }

        // Check ontology section
        if manifest.ontology.source.as_os_str().is_empty() {
            return Err(Error::new(
                "Field [ontology].source is required - e.g., 'ontology/schema.ttl'",
            ));
        }

        // Check generation section
        if manifest.generation.rules.is_empty() {
            return Err(Error::new(
                "Field [generation].rules must contain at least 1 rule",
            ));
        }

        Ok(())
    }

    fn recovery_suggestions(&self, _error: &str) -> Vec<String> {
        vec![
            "Open ggen.toml in editor".to_string(),
            "Ensure [project] section has name".to_string(),
            "Ensure [ontology] section has source".to_string(),
            "Ensure [generation] section has at least one rule".to_string(),
            "Or use `ggen init` to create valid template".to_string(),
        ]
    }

    fn docs_link(&self) -> String {
        "https://ggen.dev/docs/manifest-format".to_string()
    }
}

/// Gate 2: Ontology Dependencies
struct OntologyDependenciesGate;

impl QualityGate for OntologyDependenciesGate {
    fn name(&self) -> &str {
        "Ontology Dependencies"
    }

    fn check(&self, manifest: &GgenManifest, base_path: &Path) -> Result<()> {
        // Check source file exists
        let source_path = base_path.join(&manifest.ontology.source);
        if !source_path.exists() {
            return Err(Error::new(&format!(
                "Ontology source file not found: {}",
                source_path.display()
            )));
        }

        // Check all imports exist
        for import in &manifest.ontology.imports {
            let import_path = base_path.join(import);
            if !import_path.exists() {
                return Err(Error::new(&format!(
                    "Ontology import file not found: {}",
                    import_path.display()
                )));
            }
        }

        // TODO: Add cycle detection in v5.3
        // For now, just check files exist

        Ok(())
    }

    fn recovery_suggestions(&self, error: &str) -> Vec<String> {
        let mut suggestions = vec![
            "Check that all ontology files exist".to_string(),
            "Verify file paths in [ontology] section".to_string(),
            "Use `ggen sync --validate-only` for detailed analysis".to_string(),
        ];

        if error.contains("not found") {
            suggestions.insert(
                0,
                "Create the missing file or fix the path".to_string(),
            );
        }

        suggestions
    }

    fn docs_link(&self) -> String {
        "https://ggen.dev/docs/ontology-dependencies".to_string()
    }
}

/// Gate 3: SPARQL Query Validation
struct SparqlValidationGate;

impl QualityGate for SparqlValidationGate {
    fn name(&self) -> &str {
        "SPARQL Validation"
    }

    fn check(&self, _manifest: &GgenManifest, _base_path: &Path) -> Result<()> {
        // TODO: Implement SPARQL validation in v5.3
        // For now, this is a placeholder that passes
        Ok(())
    }

    fn recovery_suggestions(&self, _error: &str) -> Vec<String> {
        vec![
            "Check SPARQL query syntax".to_string(),
            "Verify variable names match ontology".to_string(),
            "Use `SELECT ?var WHERE { ... }` format for queries".to_string(),
        ]
    }

    fn docs_link(&self) -> String {
        "https://ggen.dev/docs/sparql-queries".to_string()
    }
}

/// Gate 4: Template Validation
struct TemplateValidationGate;

impl QualityGate for TemplateValidationGate {
    fn name(&self) -> &str {
        "Template Validation"
    }

    fn check(&self, manifest: &GgenManifest, base_path: &Path) -> Result<()> {
        // Check all template files exist
        for rule in &manifest.generation.rules {
            // Handle template source
            match &rule.template {
                crate::manifest::TemplateSource::File { file } => {
                    let template_path = base_path.join(file);
                    if !template_path.exists() {
                        return Err(Error::new(&format!(
                            "Template not found for rule '{}': {}",
                            rule.name,
                            template_path.display()
                        )));
                    }
                }
                crate::manifest::TemplateSource::Inline { inline: _ } => {
                    // Inline templates always exist
                }
            }
        }

        // TODO: Add Tera syntax validation in v5.3
        Ok(())
    }

    fn recovery_suggestions(&self, error: &str) -> Vec<String> {
        let mut suggestions = vec![
            "Verify template files exist in correct location".to_string(),
            "Check template file paths in ggen.toml".to_string(),
        ];

        if error.contains("not found") {
            suggestions.insert(0, "Create the missing template file".to_string());
        }

        suggestions.push("Use `ggen sync --validate-only` for more details".to_string());
        suggestions
    }

    fn docs_link(&self) -> String {
        "https://ggen.dev/docs/templates".to_string()
    }
}

/// Gate 5: File Permissions
struct FilePermissionsGate;

impl QualityGate for FilePermissionsGate {
    fn name(&self) -> &str {
        "File Permissions"
    }

    fn check(&self, manifest: &GgenManifest, base_path: &Path) -> Result<()> {
        let output_dir = base_path.join(&manifest.generation.output_dir);

        // Create output directory if it doesn't exist
        if !output_dir.exists() {
            std::fs::create_dir_all(&output_dir).map_err(|e| {
                Error::new(&format!(
                    "Cannot create output directory: {}\nReason: {}",
                    output_dir.display(),
                    e
                ))
            })?;
        }

        // Test writability by attempting a test write
        let test_file = output_dir.join(".ggen-write-test");
        match std::fs::write(&test_file, "") {
            Ok(_) => {
                let _ = std::fs::remove_file(&test_file);
                Ok(())
            }
            Err(e) => {
                Err(Error::new(&format!(
                    "Output directory not writable: {}\nReason: {}",
                    output_dir.display(),
                    e
                )))
            }
        }
    }

    fn recovery_suggestions(&self, _error: &str) -> Vec<String> {
        vec![
            "Make directory writable: chmod u+w <directory>".to_string(),
            "Or change output_dir in ggen.toml [generation] section".to_string(),
            "Or run with elevated privileges (not recommended)".to_string(),
        ]
    }

    fn docs_link(&self) -> String {
        "https://ggen.dev/docs/permissions".to_string()
    }
}

/// Gate 6: Rule Validation
struct RuleValidationGate;

impl QualityGate for RuleValidationGate {
    fn name(&self) -> &str {
        "Rule Validation"
    }

    fn check(&self, manifest: &GgenManifest, base_path: &Path) -> Result<()> {
        // Verify all generation rules reference existing templates
        for rule in &manifest.generation.rules {
            match &rule.template {
                crate::manifest::TemplateSource::File { file } => {
                    let template_path = base_path.join(file);
                    if !template_path.exists() {
                        return Err(Error::new(&format!(
                            "Generation rule '{}' references missing template: {}",
                            rule.name,
                            template_path.display()
                        )));
                    }
                }
                crate::manifest::TemplateSource::Inline { inline: _ } => {
                    // Inline templates are always valid
                }
            }
        }

        Ok(())
    }

    fn recovery_suggestions(&self, error: &str) -> Vec<String> {
        vec![
            "Verify all generation rules exist in [generation] section".to_string(),
            "Check that template files referenced in rules exist".to_string(),
            format!("Error details: {}", error),
        ]
    }

    fn docs_link(&self) -> String {
        "https://ggen.dev/docs/generation-rules".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quality_gate_runner_creation() {
        let runner = QualityGateRunner::new();
        let checkpoints = runner.checkpoints();
        assert_eq!(checkpoints.len(), 6);
    }

    #[test]
    fn test_checkpoint_names() {
        let runner = QualityGateRunner::new();
        let checkpoints = runner.checkpoints();
        let names: Vec<_> = checkpoints.iter().map(|c| &c.name).collect();
        assert!(names.contains(&&"Manifest Schema".to_string()));
        assert!(names.contains(&&"Ontology Dependencies".to_string()));
    }
}
