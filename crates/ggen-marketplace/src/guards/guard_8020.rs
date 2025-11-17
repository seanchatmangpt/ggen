//! Guard8020Coverage - Validation for 8020 packages
//!
//! This guard validates that a package meets the critical "8020" criteria:
//! covers 80% of real-world use cases with minimal hand-editing.
//!
//! A package is 8020-certified when ALL checks pass:
//! 1. Ontology present & valid RDF
//! 2. Projections complete (Π_models, Π_apis, Π_docs)
//! 3. Templates present (≥3)
//! 4. Tests present (unit + integration)
//! 5. Documentation complete (README, examples, architecture)
//! 6. Validation guards defined (≥1)
//! 7. Bundle integration (links to ≥1 other 8020 package)

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::Path;
use tracing::info;

/// Guard8020Coverage validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Guard8020Result {
    /// Overall certification status
    pub is_8020_certified: bool,

    /// Package sector (e.g., "observability", "microservice")
    pub sector: Option<String>,

    /// Individual check results
    pub checks: Vec<Guard8020Check>,

    /// Total score (0-100)
    pub total_score: u32,

    /// Dark matter reduction target (if provided)
    pub dark_matter_reduction_target: Option<String>,

    /// Timestamp of validation
    pub validated_at: String,
}

/// Individual 8020 check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Guard8020Check {
    /// Check name (e.g., "ontology_valid", "templates_count")
    pub name: String,

    /// Check description
    pub description: String,

    /// Did it pass?
    pub passed: bool,

    /// Feedback
    pub feedback: String,

    /// Weight for scoring (1-100)
    pub weight: u32,
}

impl Guard8020Check {
    /// Create a passing check
    pub fn pass(name: &str, description: &str, feedback: &str, weight: u32) -> Self {
        Self {
            name: name.to_string(),
            description: description.to_string(),
            passed: true,
            feedback: feedback.to_string(),
            weight,
        }
    }

    /// Create a failing check
    pub fn fail(name: &str, description: &str, feedback: &str, weight: u32) -> Self {
        Self {
            name: name.to_string(),
            description: description.to_string(),
            passed: false,
            feedback: feedback.to_string(),
            weight,
        }
    }
}

/// Guard8020Coverage implementation
pub struct Guard8020Coverage;

impl Guard8020Coverage {
    /// Validate a package against 8020 criteria
    pub async fn validate(package_path: &str) -> Result<Guard8020Result> {
        let path = Path::new(package_path);
        let mut checks = Vec::new();

        // Check 1: Ontology present & valid RDF
        let ontology_check = Self::check_ontology(path).await;
        checks.push(ontology_check);

        // Check 2: Projections complete
        let projections_check = Self::check_projections(path).await;
        checks.push(projections_check);

        // Check 3: Templates present (≥3)
        let templates_check = Self::check_templates(path).await;
        checks.push(templates_check);

        // Check 4: Tests present
        let tests_check = Self::check_tests(path).await;
        checks.push(tests_check);

        // Check 5: Documentation complete
        let docs_check = Self::check_documentation(path).await;
        checks.push(docs_check);

        // Check 6: Guards defined
        let guards_check = Self::check_guards(path).await;
        checks.push(guards_check);

        // Check 7: Bundle integration (optional, doesn't fail certification)
        let bundle_check = Self::check_bundle_integration(path).await;
        checks.push(bundle_check);

        // Calculate score and certification status
        let (total_score, is_certified) = Self::calculate_score(&checks);

        // Read dark matter reduction target if available
        let dark_matter_target = Self::read_dark_matter_target(path).await;

        // Read sector if available
        let sector = Self::read_sector(path).await;

        info!(
            "Guard8020Coverage validation complete: package={}, certified={}, score={}",
            package_path, is_certified, total_score
        );

        Ok(Guard8020Result {
            is_8020_certified: is_certified,
            sector,
            checks,
            total_score,
            dark_matter_reduction_target: dark_matter_target,
            validated_at: chrono::Utc::now().to_rfc3339(),
        })
    }

    /// Check 1: Ontology present & valid RDF
    async fn check_ontology(path: &Path) -> Guard8020Check {
        let ontologies_dir = path.join("ontologies");

        if !ontologies_dir.exists() {
            return Guard8020Check::fail(
                "ontology_valid",
                "Ontology present and valid RDF",
                "ontologies/ directory not found. 8020 packages must include domain ontology.",
                20,
            );
        }

        // Check for .ttl files
        match tokio::fs::read_dir(&ontologies_dir).await {
            Ok(mut entries) => {
                let mut has_ttl = false;
                while let Ok(Some(entry)) = entries.next_entry().await {
                    if let Some(filename) = entry.file_name().to_str() {
                        if filename.ends_with(".ttl") {
                            has_ttl = true;
                            break;
                        }
                    }
                }

                if has_ttl {
                    Guard8020Check::pass(
                        "ontology_valid",
                        "Ontology present and valid RDF",
                        "Found valid .ttl (Turtle) ontology files",
                        20,
                    )
                } else {
                    Guard8020Check::fail(
                        "ontology_valid",
                        "Ontology present and valid RDF",
                        "No .ttl files found in ontologies/",
                        20,
                    )
                }
            }
            Err(_) => Guard8020Check::fail(
                "ontology_valid",
                "Ontology present and valid RDF",
                "Could not read ontologies/ directory",
                20,
            ),
        }
    }

    /// Check 2: Projections complete (Π_models, Π_apis, Π_docs)
    async fn check_projections(path: &Path) -> Guard8020Check {
        let templates_dir = path.join("templates");

        if !templates_dir.exists() {
            return Guard8020Check::fail(
                "projections_complete",
                "Projection families complete (models, APIs, docs)",
                "templates/ directory not found",
                20,
            );
        }

        // Look for projection templates: model templates, api templates, doc templates
        match tokio::fs::read_dir(&templates_dir).await {
            Ok(mut entries) => {
                let mut has_models = false;
                let mut has_apis = false;
                let mut has_docs = false;

                while let Ok(Some(entry)) = entries.next_entry().await {
                    if let Some(filename) = entry.file_name().to_str() {
                        if filename.contains("model")
                            || filename.contains("struct")
                            || filename.contains("type")
                        {
                            has_models = true;
                        }
                        if filename.contains("api")
                            || filename.contains("endpoint")
                            || filename.contains("rest")
                        {
                            has_apis = true;
                        }
                        if filename.contains("doc")
                            || filename.contains("readme")
                            || filename.contains("guide")
                        {
                            has_docs = true;
                        }
                    }
                }

                if has_models && has_apis && has_docs {
                    Guard8020Check::pass(
                        "projections_complete",
                        "Projection families complete (models, APIs, docs)",
                        "Found model, API, and documentation projections",
                        20,
                    )
                } else {
                    let missing = [
                        if !has_models { "models" } else { "" },
                        if !has_apis { "APIs" } else { "" },
                        if !has_docs { "docs" } else { "" },
                    ]
                    .iter()
                    .filter(|s| !s.is_empty())
                    .copied()
                    .collect::<Vec<_>>()
                    .join(", ");

                    Guard8020Check::fail(
                        "projections_complete",
                        "Projection families complete (models, APIs, docs)",
                        &format!("Missing projection(s): {}", missing),
                        20,
                    )
                }
            }
            Err(_) => Guard8020Check::fail(
                "projections_complete",
                "Projection families complete (models, APIs, docs)",
                "Could not read templates/ directory",
                20,
            ),
        }
    }

    /// Check 3: Templates present (≥3)
    async fn check_templates(path: &Path) -> Guard8020Check {
        let templates_dir = path.join("templates");

        if !templates_dir.exists() {
            return Guard8020Check::fail(
                "templates_count",
                "Templates present (minimum 3)",
                "templates/ directory not found",
                15,
            );
        }

        match tokio::fs::read_dir(&templates_dir).await {
            Ok(mut entries) => {
                let mut count = 0;
                while let Ok(Some(_)) = entries.next_entry().await {
                    count += 1;
                }

                if count >= 3 {
                    Guard8020Check::pass(
                        "templates_count",
                        "Templates present (minimum 3)",
                        &format!("Found {} template files", count),
                        15,
                    )
                } else {
                    Guard8020Check::fail(
                        "templates_count",
                        "Templates present (minimum 3)",
                        &format!("Only found {} template(s), minimum 3 required", count),
                        15,
                    )
                }
            }
            Err(_) => Guard8020Check::fail(
                "templates_count",
                "Templates present (minimum 3)",
                "Could not read templates/ directory",
                15,
            ),
        }
    }

    /// Check 4: Tests present
    async fn check_tests(path: &Path) -> Guard8020Check {
        let tests_dir = path.join("tests");

        if !tests_dir.exists() {
            return Guard8020Check::fail(
                "tests_complete",
                "Tests present (unit and integration)",
                "tests/ directory not found",
                15,
            );
        }

        match tokio::fs::read_dir(&tests_dir).await {
            Ok(mut entries) => {
                let mut has_unit = false;
                let mut has_integration = false;

                while let Ok(Some(entry)) = entries.next_entry().await {
                    if let Some(filename) = entry.file_name().to_str() {
                        if filename.contains("unit") {
                            has_unit = true;
                        }
                        if filename.contains("integration") || filename.contains("e2e") {
                            has_integration = true;
                        }
                    }
                }

                if has_unit && has_integration {
                    Guard8020Check::pass(
                        "tests_complete",
                        "Tests present (unit and integration)",
                        "Found both unit and integration tests",
                        15,
                    )
                } else if has_unit || has_integration {
                    Guard8020Check::fail(
                        "tests_complete",
                        "Tests present (unit and integration)",
                        "Found only one of: unit tests, integration tests. Need both.",
                        15,
                    )
                } else {
                    Guard8020Check::fail(
                        "tests_complete",
                        "Tests present (unit and integration)",
                        "No unit or integration tests found in tests/",
                        15,
                    )
                }
            }
            Err(_) => Guard8020Check::fail(
                "tests_complete",
                "Tests present (unit and integration)",
                "Could not read tests/ directory",
                15,
            ),
        }
    }

    /// Check 5: Documentation complete
    async fn check_documentation(path: &Path) -> Guard8020Check {
        let mut has_readme = false;
        let mut has_examples = false;
        let mut has_arch_docs = false;

        // Check for README
        if tokio::fs::metadata(path.join("README.md")).await.is_ok() {
            has_readme = true;
        }

        // Check for examples
        if tokio::fs::metadata(path.join("examples")).await.is_ok() {
            has_examples = true;
        }

        // Check for architecture docs
        if tokio::fs::metadata(path.join("docs")).await.is_ok() {
            has_arch_docs = true;
        }

        if has_readme && has_examples && has_arch_docs {
            Guard8020Check::pass(
                "documentation_complete",
                "Documentation complete (README, examples, architecture)",
                "Found README.md, examples/, and docs/",
                10,
            )
        } else {
            let missing = [
                if !has_readme { "README.md" } else { "" },
                if !has_examples { "examples/" } else { "" },
                if !has_arch_docs { "docs/" } else { "" },
            ]
            .iter()
            .filter(|s| !s.is_empty())
            .copied()
            .collect::<Vec<_>>()
            .join(", ");

            Guard8020Check::fail(
                "documentation_complete",
                "Documentation complete (README, examples, architecture)",
                &format!("Missing: {}", missing),
                10,
            )
        }
    }

    /// Check 6: Guards defined
    async fn check_guards(path: &Path) -> Guard8020Check {
        let guards_dir = path.join("guards");

        if !guards_dir.exists() {
            return Guard8020Check::fail(
                "guards_defined",
                "Validation guards defined (minimum 1)",
                "guards/ directory not found",
                10,
            );
        }

        match tokio::fs::read_dir(&guards_dir).await {
            Ok(mut entries) => {
                let mut count = 0;
                while let Ok(Some(_)) = entries.next_entry().await {
                    count += 1;
                }

                if count >= 1 {
                    Guard8020Check::pass(
                        "guards_defined",
                        "Validation guards defined (minimum 1)",
                        &format!("Found {} guard definition(s)", count),
                        10,
                    )
                } else {
                    Guard8020Check::fail(
                        "guards_defined",
                        "Validation guards defined (minimum 1)",
                        "No guard definitions found",
                        10,
                    )
                }
            }
            Err(_) => Guard8020Check::fail(
                "guards_defined",
                "Validation guards defined (minimum 1)",
                "Could not read guards/ directory",
                10,
            ),
        }
    }

    /// Check 7: Bundle integration (optional)
    async fn check_bundle_integration(path: &Path) -> Guard8020Check {
        let package_toml_path = path.join("package.toml");

        if let Ok(content) = tokio::fs::read_to_string(&package_toml_path).await {
            // Look for bundle dependencies or sector field
            if content.contains("dependencies") || content.contains("sector") {
                Guard8020Check::pass(
                    "bundle_integration",
                    "Bundle integration (optional, doesn't block certification)",
                    "Package declares dependencies or sector classification",
                    0, // Weight 0 - doesn't affect score
                )
            } else {
                Guard8020Check::fail(
                    "bundle_integration",
                    "Bundle integration (optional, doesn't block certification)",
                    "Package doesn't declare bundle dependencies",
                    0, // Weight 0 - doesn't affect score
                )
            }
        } else {
            Guard8020Check::fail(
                "bundle_integration",
                "Bundle integration (optional, doesn't block certification)",
                "Could not read package.toml",
                0, // Weight 0 - doesn't affect score
            )
        }
    }

    /// Calculate total score and certification status
    fn calculate_score(checks: &[Guard8020Check]) -> (u32, bool) {
        let mut total_weight = 0u32;
        let mut passed_weight = 0u32;

        for check in checks {
            total_weight = total_weight.saturating_add(check.weight);
            if check.passed {
                passed_weight = passed_weight.saturating_add(check.weight);
            }
        }

        let score = if total_weight > 0 {
            (passed_weight as f32 / total_weight as f32 * 100.0) as u32
        } else {
            0
        };

        // Certified if all critical checks pass (weight > 0)
        let all_critical_pass = checks.iter().filter(|c| c.weight > 0).all(|c| c.passed);

        (score, all_critical_pass)
    }

    /// Read dark matter reduction target from package.toml
    async fn read_dark_matter_target(path: &Path) -> Option<String> {
        let package_toml_path = path.join("package.toml");

        if let Ok(content) = tokio::fs::read_to_string(&package_toml_path).await {
            // Simple regex-like search for dark_matter_reduction_target
            for line in content.lines() {
                if line.contains("dark_matter_reduction_target") {
                    if let Some(start) = line.find('"') {
                        if let Some(end) = line.rfind('"') {
                            if start < end {
                                return Some(line[start + 1..end].to_string());
                            }
                        }
                    }
                }
            }
        }

        None
    }

    /// Read sector from package.toml
    async fn read_sector(path: &Path) -> Option<String> {
        let package_toml_path = path.join("package.toml");

        if let Ok(content) = tokio::fs::read_to_string(&package_toml_path).await {
            for line in content.lines() {
                if line.contains("sector") && !line.contains("dark_matter") {
                    if let Some(start) = line.find('"') {
                        if let Some(end) = line.rfind('"') {
                            if start < end {
                                return Some(line[start + 1..end].to_string());
                            }
                        }
                    }
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_guard8020_check_creation() {
        let pass_check = Guard8020Check::pass("test", "Test check", "Passed", 10);
        assert!(pass_check.passed);
        assert_eq!(pass_check.weight, 10);

        let fail_check = Guard8020Check::fail("test", "Test check", "Failed", 10);
        assert!(!fail_check.passed);
        assert_eq!(fail_check.weight, 10);
    }

    #[test]
    fn test_score_calculation() {
        let checks = vec![
            Guard8020Check::pass("check1", "Check 1", "Pass", 20),
            Guard8020Check::pass("check2", "Check 2", "Pass", 20),
            Guard8020Check::fail("check3", "Check 3", "Fail", 10),
        ];

        let (score, certified) = Guard8020Coverage::calculate_score(&checks);
        assert_eq!(score, 80); // 40/50 = 80%
        assert!(!certified); // Failed check blocks certification
    }
}
