//! Marketplace Package Validation
//!
//! This module provides validation logic for marketplace packages to determine
//! production readiness based on required files, quality checks, and scoring.

use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

use ggen_core::graph::Graph;
use ggen_utils::error::{Error, Result};

/// Configuration for package validation behavior
#[derive(Debug, Clone)]
pub struct ValidationConfig {
    /// Weight for required checks (default: 0.6)
    pub required_checks_weight: f64,
    /// Weight for quality checks (default: 0.4)
    pub quality_checks_weight: f64,
    /// Minimum score for production readiness (default: 95.0)
    pub production_ready_threshold: f64,
    /// Minimum README length in characters (default: 100)
    pub readme_min_length: usize,
    /// Minimum ontology file lines (default: 200)
    pub ontology_min_lines: usize,
    /// Minimum RDF triples in ontology (default: 1)
    pub ontology_min_triples: usize,
    /// Treat warnings in required checks as failures (default: true)
    pub required_warnings_fail: bool,
}

impl Default for ValidationConfig {
    fn default() -> Self {
        Self {
            required_checks_weight: REQUIRED_CHECKS_WEIGHT,
            quality_checks_weight: QUALITY_CHECKS_WEIGHT,
            production_ready_threshold: PRODUCTION_READY_THRESHOLD,
            readme_min_length: README_MIN_LENGTH,
            ontology_min_lines: ONTOLOGY_MIN_LINES,
            ontology_min_triples: ONTOLOGY_MIN_TRIPLES,
            required_warnings_fail: true,
        }
    }
}

// ============================================================================
// VALIDATION CONSTANTS - Configurable thresholds and weights
// ============================================================================

/// Weight for required checks in final score calculation (must multiply with quality weight = 1.0)
const REQUIRED_CHECKS_WEIGHT: f64 = 0.6;

/// Weight for quality checks in final score calculation
const QUALITY_CHECKS_WEIGHT: f64 = 0.4;

/// Minimum score required for production readiness (as percentage)
const PRODUCTION_READY_THRESHOLD: f64 = 95.0;

/// Minimum length for README.md content in characters
const README_MIN_LENGTH: usize = 100;

/// Minimum number of lines for RDF ontology files
const ONTOLOGY_MIN_LINES: usize = 200;

/// Minimum number of valid RDF triples in ontology
const ONTOLOGY_MIN_TRIPLES: usize = 1;

/// Validation result for a single check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CheckResult {
    /// Check passed with message
    Pass(String),
    /// Check failed with message
    Fail(String),
    /// Warning with message
    Warning(String),
    /// Check not applicable with reason
    NotApplicable(String),
}

impl CheckResult {
    pub fn is_pass(&self) -> bool {
        matches!(self, CheckResult::Pass(_))
    }

    pub fn is_fail(&self) -> bool {
        matches!(self, CheckResult::Fail(_))
    }

    pub fn is_warning(&self) -> bool {
        matches!(self, CheckResult::Warning(_))
    }

    pub fn is_not_applicable(&self) -> bool {
        matches!(self, CheckResult::NotApplicable(_))
    }

    pub fn message(&self) -> &str {
        match self {
            CheckResult::Pass(msg)
            | CheckResult::Fail(msg)
            | CheckResult::Warning(msg)
            | CheckResult::NotApplicable(msg) => msg,
        }
    }
}

/// Required check types (critical - 60% weight)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RequiredCheck {
    PackageToml,
    Readme,
    SourceCode,
    License,
}

/// Quality check types (bonus - 40% weight)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum QualityCheck {
    RdfOntology,
    SparqlQueries,
    Examples,
    Tests,
    Documentation,
}

/// Complete validation result for a package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageValidation {
    pub package_name: String,
    pub package_path: PathBuf,
    pub score: f64,
    pub production_ready: bool,
    pub required_checks: Vec<(RequiredCheck, CheckResult)>,
    pub quality_checks: Vec<(QualityCheck, CheckResult)>,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

impl PackageValidation {
    pub fn new(package_name: String, package_path: PathBuf) -> Self {
        Self {
            package_name,
            package_path,
            score: 0.0,
            production_ready: false,
            required_checks: Vec::new(),
            quality_checks: Vec::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    /// Calculate final score and production readiness with given configuration
    pub fn calculate_score_with_config(&mut self, config: &ValidationConfig) {
        // Required checks: weighted portion, all must pass for production_ready
        let required_score = self.calculate_required_score();

        // Quality checks: weighted portion (bonus points)
        let quality_score = self.calculate_quality_score();

        // Final score: weighted combination
        self.score = (required_score * config.required_checks_weight)
            + (quality_score * config.quality_checks_weight);

        // Check if all required checks pass (warnings can optionally fail)
        let all_required_pass = if config.required_warnings_fail {
            self.required_checks
                .iter()
                .all(|(_, result)| result.is_pass())
        } else {
            self.required_checks
                .iter()
                .all(|(_, result)| !result.is_fail())
        };

        self.production_ready =
            self.score >= config.production_ready_threshold && all_required_pass;
    }

    /// Calculate final score and production readiness with default configuration
    pub fn calculate_score(&mut self) {
        let config = ValidationConfig::default();
        self.calculate_score_with_config(&config);
    }

    /// Generate a human-readable validation summary
    pub fn summary(&self) -> String {
        let required_pass = self
            .required_checks
            .iter()
            .filter(|(_, r)| r.is_pass())
            .count();
        let quality_pass = self
            .quality_checks
            .iter()
            .filter(|(_, r)| r.is_pass())
            .count();

        format!(
            "Package: {} | Score: {:.1}% | Production Ready: {} | Required: {}/{} | Quality: {}/{} | Errors: {}",
            self.package_name,
            self.score,
            if self.production_ready { "✓" } else { "✗" },
            required_pass,
            self.required_checks.len(),
            quality_pass,
            self.quality_checks.len(),
            self.errors.len()
        )
    }

    fn calculate_required_score(&self) -> f64 {
        if self.required_checks.is_empty() {
            return 0.0;
        }

        let passed = self
            .required_checks
            .iter()
            .filter(|(_, result)| result.is_pass())
            .count();

        (passed as f64 / self.required_checks.len() as f64) * 100.0
    }

    fn calculate_quality_score(&self) -> f64 {
        if self.quality_checks.is_empty() {
            return 0.0;
        }

        // Count applicable checks (exclude NotApplicable)
        let applicable: Vec<_> = self
            .quality_checks
            .iter()
            .filter(|(_, result)| !matches!(result, CheckResult::NotApplicable(_)))
            .collect();

        if applicable.is_empty() {
            // If no applicable quality checks, score is neutral (100%)
            // This means packages without optional features aren't penalized
            return 100.0;
        }

        let passed = applicable
            .iter()
            .filter(|(_, result)| result.is_pass())
            .count();

        (passed as f64 / applicable.len() as f64) * 100.0
    }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Validate TOML file has required fields with proper parsing
fn validate_toml_file(path: &Path, required_fields: &[&str]) -> bool {
    if !path.exists() {
        return false;
    }

    match fs::read_to_string(path) {
        Ok(content) => {
            // Try parsing as TOML
            match content.parse::<toml::Value>() {
                Ok(toml_value) => {
                    // Check all required fields exist and are non-empty
                    // Look for fields at both top level and in a [package] section
                    required_fields.iter().all(|field| {
                        // Try top-level first
                        toml_value
                            .get(field)
                            .and_then(|v| v.as_str())
                            .map(|s| !s.trim().is_empty())
                            // Try [package] section fallback
                            .or_else(|| {
                                toml_value
                                    .get("package")
                                    .and_then(|pkg| pkg.get(field))
                                    .and_then(|v| v.as_str())
                                    .map(|s| !s.trim().is_empty())
                            })
                            .unwrap_or(false)
                    })
                }
                Err(_) => {
                    // Fallback: Try simple string matching if TOML parsing fails
                    // This handles edge cases where content might be malformed
                    required_fields.iter().all(|field| {
                        content.contains(&format!("{}\\s*=", field))
                            || content.contains(&format!("{} =", field))
                    })
                }
            }
        }
        Err(_) => false, // Cannot read file
    }
}

/// Find files in directory with specific extensions
fn find_files_with_extensions(dir: &Path, extensions: &[&str]) -> Result<Vec<PathBuf>> {
    if !dir.exists() || !dir.is_dir() {
        return Ok(Vec::new());
    }

    let files: Vec<_> = fs::read_dir(dir)?
        .filter_map(|e| e.ok())
        .filter(|e| {
            let path = e.path();
            path.is_file()
                && path
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| extensions.contains(&ext))
                    .unwrap_or(false)
        })
        .map(|e| e.path())
        .collect();

    Ok(files)
}

/// Validate a single package
pub fn validate_package(package_path: &Path) -> Result<PackageValidation> {
    // Validate path exists and is a directory
    if !package_path.exists() {
        return Err(Error::new(&format!(
            "Package path does not exist: {}",
            package_path.display()
        )));
    }

    if !package_path.is_dir() {
        return Err(Error::new(&format!(
            "Package path is not a directory: {}",
            package_path.display()
        )));
    }

    let package_name = package_path
        .file_name()
        .and_then(|n| n.to_str())
        .ok_or_else(|| Error::new("Invalid package path"))?
        .to_string();

    let mut validation = PackageValidation::new(package_name.clone(), package_path.to_path_buf());

    // Validate required checks
    validate_required_checks(&mut validation, package_path)?;

    // Validate quality checks
    validate_quality_checks(&mut validation, package_path)?;

    // Calculate final score
    validation.calculate_score();

    Ok(validation)
}

/// Validate all required checks (critical - 60% weight)
fn validate_required_checks(validation: &mut PackageValidation, package_path: &Path) -> Result<()> {
    // Check package.toml with proper TOML parsing
    let package_toml = package_path.join("package.toml");
    if package_toml.exists() {
        // Validate using proper TOML parsing with required fields
        if validate_toml_file(&package_toml, &["name", "version", "description"]) {
            validation.required_checks.push((
                RequiredCheck::PackageToml,
                CheckResult::Pass(
                    "package.toml exists with all required fields (name, version, description)"
                        .to_string(),
                ),
            ));
        } else {
            // Determine if it's invalid TOML or missing required fields
            let error_msg = match fs::read_to_string(&package_toml) {
                Ok(content) => {
                    match content.parse::<toml::Value>() {
                        Ok(_) => "package.toml has invalid or missing required fields (name, version, description)".to_string(),
                        Err(e) => format!("package.toml has invalid TOML syntax: {}", e),
                    }
                }
                Err(e) => format!("Cannot read package.toml: {}", e),
            };

            validation.required_checks.push((
                RequiredCheck::PackageToml,
                CheckResult::Fail(error_msg.clone()),
            ));
            validation.errors.push(error_msg);
        }
    } else {
        validation.required_checks.push((
            RequiredCheck::PackageToml,
            CheckResult::Fail("package.toml not found".to_string()),
        ));
        validation.errors.push("package.toml not found".to_string());
    }

    // Check README.md
    let readme = package_path.join("README.md");
    if readme.exists() {
        if let Ok(content) = fs::read_to_string(&readme) {
            if content.len() > README_MIN_LENGTH {
                validation.required_checks.push((
                    RequiredCheck::Readme,
                    CheckResult::Pass(format!(
                        "README.md exists with {} characters (>{} required)",
                        content.len(),
                        README_MIN_LENGTH
                    )),
                ));
            } else {
                validation.required_checks.push((
                    RequiredCheck::Readme,
                    CheckResult::Warning(format!(
                        "README.md exists but only {} characters (<{} recommended)",
                        content.len(),
                        README_MIN_LENGTH
                    )),
                ));
                validation
                    .warnings
                    .push(format!("README.md is very short ({} chars)", content.len()));
            }
        } else {
            validation.required_checks.push((
                RequiredCheck::Readme,
                CheckResult::Fail("Cannot read README.md".to_string()),
            ));
            validation.errors.push("Cannot read README.md".to_string());
        }
    } else {
        validation.required_checks.push((
            RequiredCheck::Readme,
            CheckResult::Fail("README.md not found".to_string()),
        ));
        validation.errors.push("README.md not found".to_string());
    }

    // Check source code (src/main.rs or src/lib.rs)
    let main_rs = package_path.join("src/main.rs");
    let lib_rs = package_path.join("src/lib.rs");
    if main_rs.exists() || lib_rs.exists() {
        validation.required_checks.push((
            RequiredCheck::SourceCode,
            CheckResult::Pass("Source code found (src/main.rs or src/lib.rs)".to_string()),
        ));
    } else {
        // Check if it's a template-only package (has templates/)
        let templates_dir = package_path.join("templates");
        if templates_dir.exists() && templates_dir.is_dir() {
            validation.required_checks.push((
                RequiredCheck::SourceCode,
                CheckResult::NotApplicable(
                    "Template-only package (no source code required)".to_string(),
                ),
            ));
        } else {
            validation.required_checks.push((
                RequiredCheck::SourceCode,
                CheckResult::Fail("No source code found (src/main.rs or src/lib.rs)".to_string()),
            ));
            validation.errors.push("No source code found".to_string());
        }
    }

    // Check license file
    let license = package_path.join("LICENSE");
    let license_mit = package_path.join("LICENSE-MIT");
    let license_apache = package_path.join("LICENSE-APACHE");

    if license.exists() || license_mit.exists() || license_apache.exists() {
        validation.required_checks.push((
            RequiredCheck::License,
            CheckResult::Pass("License file found".to_string()),
        ));
    } else {
        validation.required_checks.push((
            RequiredCheck::License,
            CheckResult::Fail(
                "No license file found (LICENSE, LICENSE-MIT, or LICENSE-APACHE)".to_string(),
            ),
        ));
        validation.errors.push("No license file found".to_string());
    }

    Ok(())
}

// ============================================================================
// QUALITY CHECK HELPER FUNCTIONS - Each check in its own focused function
// ============================================================================

/// Check for RDF ontology in the package
fn check_rdf_ontology(validation: &mut PackageValidation, package_path: &Path) -> Result<()> {
    let rdf_dir = package_path.join("rdf");
    let ontology_ttl = package_path.join("rdf/ontology.ttl");
    let ontology_dir = package_path.join("ontology");

    if ontology_ttl.exists() {
        match Graph::new() {
            Ok(graph) => match graph.load_path(&ontology_ttl) {
                Ok(()) => {
                    let triple_count = graph.len();
                    let lines = fs::read_to_string(&ontology_ttl)
                        .map(|c| c.lines().count())
                        .unwrap_or(0);

                    if lines >= ONTOLOGY_MIN_LINES && triple_count >= ONTOLOGY_MIN_TRIPLES {
                        validation.quality_checks.push((
                                QualityCheck::RdfOntology,
                                CheckResult::Pass(format!(
                                    "RDF ontology validated: {} lines, {} triples (≥{} lines, ≥{} triples required)",
                                    lines, triple_count, ONTOLOGY_MIN_LINES, ONTOLOGY_MIN_TRIPLES
                                )),
                            ));
                    } else if lines >= ONTOLOGY_MIN_LINES {
                        validation.quality_checks.push((
                                QualityCheck::RdfOntology,
                                CheckResult::Warning(format!(
                                    "RDF ontology loaded but only {} triples found (≥{} lines, but {} triples)",
                                    triple_count, ONTOLOGY_MIN_LINES, triple_count
                                )),
                            ));
                    } else {
                        validation.quality_checks.push((
                                QualityCheck::RdfOntology,
                                CheckResult::Warning(format!(
                                    "RDF ontology found but only {} lines (<{} recommended), {} triples",
                                    lines, ONTOLOGY_MIN_LINES, triple_count
                                )),
                            ));
                        validation
                            .warnings
                            .push(format!("RDF ontology has only {} lines", lines));
                    }
                }
                Err(e) => {
                    validation.quality_checks.push((
                        QualityCheck::RdfOntology,
                        CheckResult::Warning(format!(
                            "RDF ontology file exists but cannot be loaded: {}",
                            e
                        )),
                    ));
                    validation
                        .warnings
                        .push(format!("Failed to load RDF ontology: {}", e));
                }
            },
            Err(e) => {
                validation.quality_checks.push((
                    QualityCheck::RdfOntology,
                    CheckResult::Warning(format!(
                        "RDF ontology file exists but graph initialization failed: {}",
                        e
                    )),
                ));
            }
        }
    } else if rdf_dir.exists() || ontology_dir.exists() {
        validation.quality_checks.push((
            QualityCheck::RdfOntology,
            CheckResult::Warning("RDF directory exists but ontology.ttl not found".to_string()),
        ));
    } else {
        validation.quality_checks.push((
            QualityCheck::RdfOntology,
            CheckResult::NotApplicable(
                "No RDF ontology (not required for all packages)".to_string(),
            ),
        ));
    }

    Ok(())
}

/// Check for SPARQL queries in the package
fn check_sparql_queries(validation: &mut PackageValidation, package_path: &Path) -> Result<()> {
    let sparql_dir = package_path.join("sparql");
    let queries_dir = package_path.join("queries");

    // Check sparql/ directory
    match find_files_with_extensions(&sparql_dir, &["rq", "sparql"]) {
        Ok(queries) if !queries.is_empty() => {
            validation.quality_checks.push((
                QualityCheck::SparqlQueries,
                CheckResult::Pass(format!("Found {} SPARQL query file(s)", queries.len())),
            ));
            return Ok(());
        }
        Ok(_) if sparql_dir.exists() => {
            validation.quality_checks.push((
                QualityCheck::SparqlQueries,
                CheckResult::Warning(
                    "sparql/ directory exists but no .rq or .sparql files found".to_string(),
                ),
            ));
            return Ok(());
        }
        _ => {}
    }

    // Check queries/ directory
    match find_files_with_extensions(&queries_dir, &["rq", "sparql"]) {
        Ok(queries) if !queries.is_empty() => {
            validation.quality_checks.push((
                QualityCheck::SparqlQueries,
                CheckResult::Pass(format!(
                    "Found {} SPARQL query file(s) in queries/",
                    queries.len()
                )),
            ));
            return Ok(());
        }
        Ok(_) if queries_dir.exists() => {
            validation.quality_checks.push((
                QualityCheck::SparqlQueries,
                CheckResult::Warning(
                    "queries/ directory exists but no .rq or .sparql files found".to_string(),
                ),
            ));
            return Ok(());
        }
        _ => {}
    }

    validation.quality_checks.push((
        QualityCheck::SparqlQueries,
        CheckResult::NotApplicable("No SPARQL queries (not required for all packages)".to_string()),
    ));

    Ok(())
}

/// Check for examples in the package
fn check_examples(validation: &mut PackageValidation, package_path: &Path) -> Result<()> {
    let examples_dir = package_path.join("examples");

    match find_files_with_extensions(&examples_dir, &["rs", "py", "ts", "js", "go", "java"]) {
        Ok(examples) if !examples.is_empty() => {
            validation.quality_checks.push((
                QualityCheck::Examples,
                CheckResult::Pass(format!("Found {} example file(s)", examples.len())),
            ));
        }
        Ok(_) if examples_dir.exists() => {
            validation.quality_checks.push((
                QualityCheck::Examples,
                CheckResult::Warning("examples/ directory exists but is empty".to_string()),
            ));
        }
        _ => {
            validation.quality_checks.push((
                QualityCheck::Examples,
                CheckResult::Warning("No examples/ directory found".to_string()),
            ));
        }
    }

    Ok(())
}

/// Check for tests in the package
fn check_tests(validation: &mut PackageValidation, package_path: &Path) -> Result<()> {
    let tests_dir = package_path.join("tests");

    match find_files_with_extensions(&tests_dir, &["rs", "py", "ts"]) {
        Ok(tests) if !tests.is_empty() => {
            validation.quality_checks.push((
                QualityCheck::Tests,
                CheckResult::Pass(format!("Found {} test file(s)", tests.len())),
            ));
        }
        Ok(_) if tests_dir.exists() => {
            validation.quality_checks.push((
                QualityCheck::Tests,
                CheckResult::Warning("tests/ directory exists but no test files found".to_string()),
            ));
        }
        _ => {
            validation.quality_checks.push((
                QualityCheck::Tests,
                CheckResult::Warning("No tests/ directory found".to_string()),
            ));
        }
    }

    Ok(())
}

/// Check for documentation in the package
fn check_documentation(validation: &mut PackageValidation, package_path: &Path) -> Result<()> {
    let docs_dir = package_path.join("docs");

    match find_files_with_extensions(&docs_dir, &["md"]) {
        Ok(docs) if !docs.is_empty() => {
            validation.quality_checks.push((
                QualityCheck::Documentation,
                CheckResult::Pass(format!("Found {} documentation file(s)", docs.len())),
            ));
        }
        Ok(_) if docs_dir.exists() => {
            validation.quality_checks.push((
                QualityCheck::Documentation,
                CheckResult::Warning("docs/ directory exists but no .md files found".to_string()),
            ));
        }
        _ => {
            validation.quality_checks.push((
                QualityCheck::Documentation,
                CheckResult::NotApplicable(
                    "No docs/ directory (README.md is sufficient)".to_string(),
                ),
            ));
        }
    }

    Ok(())
}

/// Validate quality checks (bonus - 40% weight)
/// Delegates to specialized helper functions for each quality check
fn validate_quality_checks(validation: &mut PackageValidation, package_path: &Path) -> Result<()> {
    check_rdf_ontology(validation, package_path)?;
    check_sparql_queries(validation, package_path)?;
    check_examples(validation, package_path)?;
    check_tests(validation, package_path)?;
    check_documentation(validation, package_path)?;
    Ok(())
}

/// Validate all packages in a directory
pub fn validate_all_packages(packages_dir: &Path) -> Result<Vec<PackageValidation>> {
    let mut validations = Vec::new();

    if !packages_dir.exists() {
        return Err(Error::new(&format!(
            "Packages directory does not exist: {}",
            packages_dir.display()
        )));
    }

    let entries = fs::read_dir(packages_dir)?;

    for entry in entries {
        let entry = entry?;
        let path = entry.path();

        // Skip if not a directory
        if !path.is_dir() {
            continue;
        }

        // Skip hidden directories
        if path
            .file_name()
            .and_then(|n| n.to_str())
            .map(|s| s.starts_with('.'))
            .unwrap_or(false)
        {
            continue;
        }

        // Validate package
        match validate_package(&path) {
            Ok(validation) => validations.push(validation),
            Err(e) => {
                let msg = format!("Failed to validate package {}: {}", path.display(), e);
                ggen_utils::alert_warning!(&msg);
            }
        }
    }

    Ok(validations)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_validate_package_missing_files() {
        let temp_dir = TempDir::new().unwrap();
        let package_path = temp_dir.path();

        let validation = validate_package(package_path).unwrap();

        assert_eq!(
            validation.package_name,
            package_path.file_name().unwrap().to_str().unwrap()
        );
        assert!(!validation.production_ready);
        assert!(validation.score < 95.0);
    }

    #[test]
    fn test_validate_package_complete() {
        let temp_dir = TempDir::new().unwrap();
        let package_path = temp_dir.path();

        // Create required files
        fs::create_dir_all(package_path.join("src")).unwrap();
        fs::write(
            package_path.join("package.toml"),
            "name = \"test\"\nversion = \"1.0.0\"\ndescription = \"Test package\"",
        )
        .unwrap();
        fs::write(
            package_path.join("README.md"),
            "Test package documentation with enough content to pass validation".repeat(2),
        )
        .unwrap();
        fs::write(package_path.join("LICENSE-MIT"), "MIT License").unwrap();
        fs::write(package_path.join("src/main.rs"), "fn main() {}").unwrap();

        // Create quality files
        fs::create_dir_all(package_path.join("rdf")).unwrap();

        // Create valid Turtle RDF ontology with >200 lines and valid triples
        let mut rdf_content = String::from(
            "@prefix ex: <http://example.org/> .\n\
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\n",
        );

        // Add 100+ valid RDF triples to ensure parsing succeeds
        for i in 0..100 {
            rdf_content.push_str(&format!(
                "ex:Entity{} rdf:type ex:TestType .\n\
                ex:Entity{} rdfs:label \"Test Entity {}\" .\n\
                ex:Entity{} rdfs:comment \"Comment for entity {}\" .\n",
                i, i, i, i, i
            ));
        }

        fs::write(package_path.join("rdf/ontology.ttl"), rdf_content).unwrap();

        fs::create_dir_all(package_path.join("examples")).unwrap();
        fs::write(package_path.join("examples/test.rs"), "// example").unwrap();
        fs::create_dir_all(package_path.join("tests")).unwrap();
        fs::write(package_path.join("tests/test.rs"), "#[test] fn test() {}").unwrap();

        let validation = validate_package(package_path).unwrap();

        assert_eq!(
            validation.package_name,
            package_path.file_name().unwrap().to_str().unwrap()
        );
        assert!(
            validation.score >= 95.0,
            "Expected score >= 95.0 but got {}, errors: {:?}, quality checks: {:?}",
            validation.score,
            validation.errors,
            validation.quality_checks
        );
        assert!(
            validation.production_ready,
            "Package should be production ready"
        );
    }

    #[test]
    fn test_validate_template_only_package() {
        let temp_dir = TempDir::new().unwrap();
        let package_path = temp_dir.path();

        // Create required files but no source code
        fs::write(
            package_path.join("package.toml"),
            "name = \"template\"\nversion = \"1.0.0\"\ndescription = \"Template package\"",
        )
        .unwrap();
        fs::write(
            package_path.join("README.md"),
            "Template package documentation".repeat(5),
        )
        .unwrap();
        fs::write(package_path.join("LICENSE"), "MIT").unwrap();

        // Create templates directory instead of src/
        fs::create_dir_all(package_path.join("templates")).unwrap();
        fs::write(package_path.join("templates/test.txt"), "template content").unwrap();

        let validation = validate_package(package_path).unwrap();

        // Should have NotApplicable for SourceCode check since it's a template package
        let source_code_check = validation
            .required_checks
            .iter()
            .find(|(check, _)| matches!(check, RequiredCheck::SourceCode));

        assert!(source_code_check.is_some());
        match source_code_check {
            Some((_, CheckResult::NotApplicable(_))) => {}
            _ => panic!("Template package should have NotApplicable for SourceCode check"),
        }
    }

    #[test]
    fn test_validate_readme_boundary_short() {
        let temp_dir = TempDir::new().unwrap();
        let package_path = temp_dir.path();

        fs::write(
            package_path.join("package.toml"),
            "name = \"test\"\nversion = \"1.0.0\"\ndescription = \"Test\"",
        )
        .unwrap();
        fs::write(package_path.join("README.md"), "x".repeat(99)).unwrap(); // 99 chars < 100
        fs::write(package_path.join("LICENSE"), "MIT").unwrap();
        fs::create_dir_all(package_path.join("src")).unwrap();
        fs::write(package_path.join("src/main.rs"), "fn main() {}").unwrap();

        let validation = validate_package(package_path).unwrap();

        // Should have Warning for README
        let readme_check = validation
            .required_checks
            .iter()
            .find(|(check, _)| matches!(check, RequiredCheck::Readme));

        assert!(readme_check.is_some());
        match readme_check {
            Some((_, CheckResult::Warning(_))) => {}
            _ => panic!("Short README should trigger Warning"),
        }
    }

    #[test]
    fn test_validate_readme_boundary_pass() {
        let temp_dir = TempDir::new().unwrap();
        let package_path = temp_dir.path();

        fs::write(
            package_path.join("package.toml"),
            "name = \"test\"\nversion = \"1.0.0\"\ndescription = \"Test\"",
        )
        .unwrap();
        fs::write(package_path.join("README.md"), "x".repeat(101)).unwrap(); // 101 chars > 100
        fs::write(package_path.join("LICENSE"), "MIT").unwrap();
        fs::create_dir_all(package_path.join("src")).unwrap();
        fs::write(package_path.join("src/main.rs"), "fn main() {}").unwrap();

        let validation = validate_package(package_path).unwrap();

        // Should have Pass for README
        let readme_check = validation
            .required_checks
            .iter()
            .find(|(check, _)| matches!(check, RequiredCheck::Readme));

        assert!(readme_check.is_some());
        match readme_check {
            Some((_, CheckResult::Pass(_))) => {}
            _ => panic!("Sufficient README should trigger Pass"),
        }
    }

    #[test]
    fn test_validate_alternative_license_files() {
        // Test with LICENSE file
        let temp_dir = TempDir::new().unwrap();
        let package_path = temp_dir.path();

        fs::write(
            package_path.join("package.toml"),
            "name = \"test\"\nversion = \"1.0.0\"\ndescription = \"Test\"",
        )
        .unwrap();
        fs::write(package_path.join("README.md"), "content".repeat(50)).unwrap();
        fs::write(package_path.join("LICENSE"), "MIT License").unwrap(); // Generic LICENSE file
        fs::create_dir_all(package_path.join("src")).unwrap();
        fs::write(package_path.join("src/main.rs"), "fn main() {}").unwrap();

        let validation = validate_package(package_path).unwrap();

        let license_check = validation
            .required_checks
            .iter()
            .find(|(check, _)| matches!(check, RequiredCheck::License));

        assert!(license_check.is_some());
        match license_check {
            Some((_, CheckResult::Pass(_))) => {}
            _ => panic!("LICENSE file should be accepted"),
        }
    }

    #[test]
    fn test_validate_lib_rs_source() {
        let temp_dir = TempDir::new().unwrap();
        let package_path = temp_dir.path();

        fs::write(
            package_path.join("package.toml"),
            "name = \"test\"\nversion = \"1.0.0\"\ndescription = \"Test\"",
        )
        .unwrap();
        fs::write(package_path.join("README.md"), "content".repeat(50)).unwrap();
        fs::write(package_path.join("LICENSE"), "MIT").unwrap();
        fs::create_dir_all(package_path.join("src")).unwrap();
        fs::write(package_path.join("src/lib.rs"), "pub fn lib_fn() {}").unwrap(); // Use lib.rs instead of main.rs

        let validation = validate_package(package_path).unwrap();

        let source_check = validation
            .required_checks
            .iter()
            .find(|(check, _)| matches!(check, RequiredCheck::SourceCode));

        assert!(source_check.is_some());
        match source_check {
            Some((_, CheckResult::Pass(_))) => {}
            _ => panic!("lib.rs should be accepted as source code"),
        }
    }

    #[test]
    fn test_validate_nonexistent_path() {
        let nonexistent_path = std::path::PathBuf::from("/nonexistent/package/path");

        let result = validate_package(&nonexistent_path);

        assert!(result.is_err(), "Should return error for nonexistent path");
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("does not exist"),
            "Error should mention path doesn't exist"
        );
    }

    #[test]
    fn test_validate_file_instead_of_directory() {
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("file.txt");
        fs::write(&file_path, "content").unwrap();

        let result = validate_package(&file_path);

        assert!(
            result.is_err(),
            "Should return error for file instead of directory"
        );
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("not a directory"),
            "Error should mention it's not a directory"
        );
    }

    #[test]
    fn test_score_with_all_required_no_quality() {
        let temp_dir = TempDir::new().unwrap();
        let package_path = temp_dir.path();

        // Create only required files, no quality files
        fs::write(
            package_path.join("package.toml"),
            "name = \"test\"\nversion = \"1.0.0\"\ndescription = \"Test\"",
        )
        .unwrap();
        fs::write(package_path.join("README.md"), "content".repeat(50)).unwrap();
        fs::write(package_path.join("LICENSE"), "MIT").unwrap();
        fs::create_dir_all(package_path.join("src")).unwrap();
        fs::write(package_path.join("src/main.rs"), "fn main() {}").unwrap();

        let validation = validate_package(package_path).unwrap();

        // Score should be 60% (all required pass) + 0% (no quality) = 60%
        assert_eq!(validation.score, 60.0);
        assert!(!validation.production_ready); // < 95%
    }

    #[test]
    fn test_check_result_helpers() {
        let pass = CheckResult::Pass("test".to_string());
        assert!(pass.is_pass());
        assert!(!pass.is_fail());
        assert!(!pass.is_warning());
        assert!(!pass.is_not_applicable());

        let fail = CheckResult::Fail("test".to_string());
        assert!(!fail.is_pass());
        assert!(fail.is_fail());
        assert!(!fail.is_warning());
        assert!(!fail.is_not_applicable());

        let warning = CheckResult::Warning("test".to_string());
        assert!(!warning.is_pass());
        assert!(!warning.is_fail());
        assert!(warning.is_warning());
        assert!(!warning.is_not_applicable());

        let na = CheckResult::NotApplicable("test".to_string());
        assert!(!na.is_pass());
        assert!(!na.is_fail());
        assert!(!na.is_warning());
        assert!(na.is_not_applicable());
    }
}
