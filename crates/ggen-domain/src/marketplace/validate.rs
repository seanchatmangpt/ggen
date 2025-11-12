//! Marketplace Package Validation
//!
//! This module provides validation logic for marketplace packages to determine
//! production readiness based on required files, quality checks, and scoring.

use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

use ggen_utils::error::{Error, Result};

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

    /// Calculate final score and production readiness
    pub fn calculate_score(&mut self) {
        // Required checks: 60% weight, all must pass for production_ready
        let required_score = self.calculate_required_score();

        // Quality checks: 40% weight (bonus points)
        let quality_score = self.calculate_quality_score();

        // Final score: weighted combination
        self.score = (required_score * 0.6) + (quality_score * 0.4);

        // Production ready if score >= 95% AND all required checks pass
        let all_required_pass = self
            .required_checks
            .iter()
            .all(|(_, result)| result.is_pass());

        self.production_ready = self.score >= 95.0 && all_required_pass;
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

        let applicable = self
            .quality_checks
            .iter()
            .filter(|(_, result)| !matches!(result, CheckResult::NotApplicable(_)))
            .count();

        if applicable == 0 {
            return 0.0;
        }

        let passed = self
            .quality_checks
            .iter()
            .filter(|(_, result)| result.is_pass())
            .count();

        (passed as f64 / applicable as f64) * 100.0
    }
}

/// Validate a single package
pub fn validate_package(package_path: &Path) -> Result<PackageValidation> {
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
    // Check package.toml
    let package_toml = package_path.join("package.toml");
    if package_toml.exists() {
        // Validate it's valid TOML and has required fields
        if let Ok(content) = fs::read_to_string(&package_toml) {
            if content.contains("name =")
                && content.contains("version =")
                && content.contains("description =")
            {
                validation.required_checks.push((
                    RequiredCheck::PackageToml,
                    CheckResult::Pass("package.toml exists with required fields".to_string()),
                ));
            } else {
                validation.required_checks.push((
                    RequiredCheck::PackageToml,
                    CheckResult::Fail(
                        "package.toml missing required fields (name, version, description)"
                            .to_string(),
                    ),
                ));
                validation
                    .errors
                    .push("package.toml missing required fields".to_string());
            }
        } else {
            validation.required_checks.push((
                RequiredCheck::PackageToml,
                CheckResult::Fail("Cannot read package.toml".to_string()),
            ));
            validation
                .errors
                .push("Cannot read package.toml".to_string());
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
            if content.len() > 100 {
                validation.required_checks.push((
                    RequiredCheck::Readme,
                    CheckResult::Pass("README.md exists with content".to_string()),
                ));
            } else {
                validation.required_checks.push((
                    RequiredCheck::Readme,
                    CheckResult::Warning("README.md exists but is very short".to_string()),
                ));
                validation
                    .warnings
                    .push("README.md is very short".to_string());
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

/// Validate quality checks (bonus - 40% weight)
fn validate_quality_checks(validation: &mut PackageValidation, package_path: &Path) -> Result<()> {
    // Check RDF ontology
    let rdf_dir = package_path.join("rdf");
    let ontology_ttl = package_path.join("rdf/ontology.ttl");
    let ontology_dir = package_path.join("ontology");

    if ontology_ttl.exists() {
        if let Ok(content) = fs::read_to_string(&ontology_ttl) {
            let lines = content.lines().count();
            if lines >= 200 {
                validation.quality_checks.push((
                    QualityCheck::RdfOntology,
                    CheckResult::Pass(format!(
                        "RDF ontology found with {} lines (≥200 required)",
                        lines
                    )),
                ));
            } else {
                validation.quality_checks.push((
                    QualityCheck::RdfOntology,
                    CheckResult::Warning(format!(
                        "RDF ontology found but only {} lines (<200 recommended)",
                        lines
                    )),
                ));
                validation
                    .warnings
                    .push(format!("RDF ontology has only {} lines", lines));
            }
        } else {
            validation.quality_checks.push((
                QualityCheck::RdfOntology,
                CheckResult::Warning("RDF ontology file exists but cannot be read".to_string()),
            ));
        }
    } else if rdf_dir.exists() || ontology_dir.exists() {
        // RDF directory exists but no ontology.ttl
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

    // Check SPARQL queries
    let sparql_dir = package_path.join("sparql");
    let queries_dir = package_path.join("queries");

    if sparql_dir.exists() && sparql_dir.is_dir() {
        let queries: Vec<_> = fs::read_dir(&sparql_dir)?
            .filter_map(|e| e.ok())
            .filter(|e| {
                let path = e.path();
                path.extension()
                    .map(|ext| ext == "rq" || ext == "sparql")
                    .unwrap_or(false)
            })
            .collect();

        if !queries.is_empty() {
            validation.quality_checks.push((
                QualityCheck::SparqlQueries,
                CheckResult::Pass(format!("Found {} SPARQL query file(s)", queries.len())),
            ));
        } else {
            validation.quality_checks.push((
                QualityCheck::SparqlQueries,
                CheckResult::Warning(
                    "sparql/ directory exists but no .rq or .sparql files found".to_string(),
                ),
            ));
        }
    } else if queries_dir.exists() && queries_dir.is_dir() {
        let queries: Vec<_> = fs::read_dir(&queries_dir)?
            .filter_map(|e| e.ok())
            .filter(|e| {
                let path = e.path();
                path.extension()
                    .map(|ext| ext == "rq" || ext == "sparql")
                    .unwrap_or(false)
            })
            .collect();

        if !queries.is_empty() {
            validation.quality_checks.push((
                QualityCheck::SparqlQueries,
                CheckResult::Pass(format!(
                    "Found {} SPARQL query file(s) in queries/",
                    queries.len()
                )),
            ));
        } else {
            validation.quality_checks.push((
                QualityCheck::SparqlQueries,
                CheckResult::Warning(
                    "queries/ directory exists but no .rq or .sparql files found".to_string(),
                ),
            ));
        }
    } else {
        validation.quality_checks.push((
            QualityCheck::SparqlQueries,
            CheckResult::NotApplicable(
                "No SPARQL queries (not required for all packages)".to_string(),
            ),
        ));
    }

    // Check examples
    let examples_dir = package_path.join("examples");
    if examples_dir.exists() && examples_dir.is_dir() {
        let examples: Vec<_> = fs::read_dir(&examples_dir)?
            .filter_map(|e| e.ok())
            .filter(|e| {
                let path = e.path();
                path.is_file() && path.extension().is_some()
            })
            .collect();

        if !examples.is_empty() {
            validation.quality_checks.push((
                QualityCheck::Examples,
                CheckResult::Pass(format!("Found {} example file(s)", examples.len())),
            ));
        } else {
            validation.quality_checks.push((
                QualityCheck::Examples,
                CheckResult::Warning("examples/ directory exists but is empty".to_string()),
            ));
        }
    } else {
        validation.quality_checks.push((
            QualityCheck::Examples,
            CheckResult::Warning("No examples/ directory found".to_string()),
        ));
    }

    // Check tests
    let tests_dir = package_path.join("tests");
    if tests_dir.exists() && tests_dir.is_dir() {
        let tests: Vec<_> = fs::read_dir(&tests_dir)?
            .filter_map(|e| e.ok())
            .filter(|e| {
                let path = e.path();
                path.is_file()
                    && (path
                        .extension()
                        .map(|ext| ext == "rs" || ext == "py" || ext == "ts")
                        .unwrap_or(false))
            })
            .collect();

        if !tests.is_empty() {
            validation.quality_checks.push((
                QualityCheck::Tests,
                CheckResult::Pass(format!("Found {} test file(s)", tests.len())),
            ));
        } else {
            validation.quality_checks.push((
                QualityCheck::Tests,
                CheckResult::Warning("tests/ directory exists but no test files found".to_string()),
            ));
        }
    } else {
        validation.quality_checks.push((
            QualityCheck::Tests,
            CheckResult::Warning("No tests/ directory found".to_string()),
        ));
    }

    // Check documentation
    let docs_dir = package_path.join("docs");
    if docs_dir.exists() && docs_dir.is_dir() {
        let docs: Vec<_> = fs::read_dir(&docs_dir)?
            .filter_map(|e| e.ok())
            .filter(|e| {
                let path = e.path();
                path.is_file() && path.extension().map(|ext| ext == "md").unwrap_or(false)
            })
            .collect();

        if !docs.is_empty() {
            validation.quality_checks.push((
                QualityCheck::Documentation,
                CheckResult::Pass(format!("Found {} documentation file(s)", docs.len())),
            ));
        } else {
            validation.quality_checks.push((
                QualityCheck::Documentation,
                CheckResult::Warning("docs/ directory exists but no .md files found".to_string()),
            ));
        }
    } else {
        validation.quality_checks.push((
            QualityCheck::Documentation,
            CheckResult::NotApplicable("No docs/ directory (README.md is sufficient)".to_string()),
        ));
    }

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
        fs::write(
            package_path.join("rdf/ontology.ttl"),
            "ontology content\n".repeat(250),
        )
        .unwrap();
        fs::create_dir_all(package_path.join("examples")).unwrap();
        fs::write(package_path.join("examples/test.rs"), "// example").unwrap();
        fs::create_dir_all(package_path.join("tests")).unwrap();
        fs::write(package_path.join("tests/test.rs"), "#[test] fn test() {}").unwrap();

        let validation = validate_package(package_path).unwrap();

        assert_eq!(
            validation.package_name,
            package_path.file_name().unwrap().to_str().unwrap()
        );
        assert!(validation.score >= 95.0);
        assert!(validation.production_ready);
    }
}
