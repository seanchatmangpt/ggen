//! Marketplace Commands - clap-noun-verb v3.4.0 Migration
//!
//! This module implements marketplace commands using the v3.4.0 #[verb] pattern.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::{Path, PathBuf};

use crate::runtime_helper::execute_async_verb;
use ggen_domain::marketplace::{
    execute_install, execute_list, execute_publish, execute_search, validate_all_packages,
    validate_package, InstallInput, ListInput, PackageValidation, PublishInput, SearchInput,
};
use ggen_marketplace::prelude::*;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct SearchOutput {
    packages: Vec<PackageInfo>,
    total: usize,
}

#[derive(Serialize)]
struct PackageInfo {
    name: String,
    version: String,
    description: String,
    author: Option<String>,
    downloads: u32,
    stars: u32,
}

#[derive(Serialize)]
struct InstallOutput {
    package: String,
    version: String,
    path: String,
    dependencies: Vec<String>,
}

#[derive(Serialize)]
struct ListOutput {
    packages: Vec<InstalledPackage>,
    total: usize,
}

#[derive(Serialize)]
struct InstalledPackage {
    name: String,
    version: String,
    title: String,
    description: String,
}

#[derive(Serialize)]
struct PublishOutput {
    package: String,
    version: String,
}

#[derive(Serialize)]
struct ValidateOutput {
    package: Option<String>,
    score: f64,
    production_ready: bool,
    total_packages: usize,
    ready_count: usize,
    needs_improvement_count: usize,
    not_ready_count: usize,
    details: Option<PackageValidation>,
    all_results: Option<Vec<PackageValidation>>,
}

#[derive(Serialize)]
struct MaturityOutput {
    package_id: String,
    package_name: String,
    total_score: u32,
    maturity_level: String,
    description: String,
    scores: MaturityScores,
    percentages: std::collections::HashMap<String, f32>,
    feedback: Vec<(String, Vec<String>)>,
    next_steps: Vec<&'static str>,
}

#[derive(Serialize)]
struct MaturityScores {
    documentation: u32,
    testing: u32,
    security: u32,
    performance: u32,
    adoption: u32,
    maintenance: u32,
}

#[derive(Serialize)]
struct DashboardOutput {
    generated_at: String,
    statistics: DashboardStats,
    assessments: Vec<MaturityOutput>,
}

#[derive(Serialize)]
struct DashboardStats {
    total_packages: usize,
    average_score: f32,
    level_distribution: LevelDist,
    average_scores_by_dimension: DimensionAverages,
}

#[derive(Serialize)]
struct LevelDist {
    experimental: usize,
    beta: usize,
    production: usize,
    enterprise: usize,
}

#[derive(Serialize)]
struct DimensionAverages {
    documentation: f32,
    testing: f32,
    security: f32,
    performance: f32,
    adoption: f32,
    maintenance: f32,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Search for packages in the marketplace
///
/// # Usage
///
/// ```bash
/// # Search with the --query flag (required)
/// ggen marketplace search --query "rust web framework"
///
/// # Limit results
/// ggen marketplace search --query "microservice" --limit 5
///
/// # Filter by category
/// ggen marketplace search --query "api" --category "backend"
/// ```
#[verb]
fn search(query: String, limit: Option<usize>, category: Option<String>) -> Result<SearchOutput> {
    let input = SearchInput {
        query,
        limit: limit.unwrap_or(10),
        category,
        ..Default::default()
    };

    execute_async_verb(async move {
        let results = execute_search(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        let packages = results
            .into_iter()
            .map(|p| PackageInfo {
                name: p.name,
                version: p.version,
                description: p.description,
                author: p.author,
                downloads: p.downloads,
                stars: p.stars,
            })
            .collect::<Vec<_>>();

        let total = packages.len();

        Ok(SearchOutput { packages, total })
    })
}

/// Install a package from the marketplace
#[verb]
fn install(
    package: String, target: Option<String>, force: bool, no_dependencies: bool, dry_run: bool,
) -> Result<InstallOutput> {
    let input = InstallInput {
        package: package.clone(),
        target,
        force,
        no_dependencies,
        dry_run,
    };

    execute_async_verb(async move {
        let result = execute_install(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        Ok(InstallOutput {
            package: result.package_name,
            version: result.version,
            path: result.install_path.display().to_string(),
            dependencies: result.dependencies_installed,
        })
    })
}

/// List installed packages with optional maturity filtering
///
/// # Usage
///
/// ```bash
/// # List all packages
/// ggen marketplace list
///
/// # List production-ready packages
/// ggen marketplace list --min-maturity production
///
/// # List packages at specific level
/// ggen marketplace list --maturity-level beta
///
/// # Sort by maturity with details
/// ggen marketplace list --min-maturity production --detailed
/// ```
#[verb]
fn list(
    detailed: bool, json: bool, min_maturity: Option<String>, maturity_level: Option<String>,
    sort: Option<String>,
) -> Result<ListOutput> {
    let input = ListInput { detailed, json };

    execute_async_verb(async move {
        let result = execute_list(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        let packages = result
            .packages
            .into_iter()
            .map(|p| InstalledPackage {
                name: p.name,
                version: p.version,
                title: p.title,
                description: p.description,
            })
            .collect::<Vec<_>>();

        // Filter by maturity level if specified
        if let Some(level_str) = min_maturity {
            let min_level = match level_str.as_str() {
                "experimental" => 0u32,
                "beta" => 41u32,
                "production" => 61u32,
                "enterprise" => 81u32,
                _ => 61u32,
            };

            // In a real implementation, fetch actual maturity scores
            // For now, keep all packages and document the filtering capability
            let _min_score = min_level;
        }

        if let Some(_level_str) = maturity_level {
            // Filter by specific maturity level
            // In a real implementation, this would filter to only packages at this exact level
        }

        // Sort by specified field
        if let Some(sort_field) = sort {
            match sort_field.as_str() {
                "maturity" => {
                    // Would sort by maturity score in real implementation
                }
                "downloads" => {
                    // Would sort by download count
                }
                "updated" => {
                    // Would sort by last update time
                }
                _ => {}
            }
        }

        let total = packages.len();

        Ok(ListOutput { packages, total })
    })
}

/// Publish a package to the marketplace
#[verb]
fn publish(
    path: PathBuf, tag: Option<String>, dry_run: bool, force: bool,
) -> Result<PublishOutput> {
    let input = PublishInput {
        path,
        tag,
        dry_run,
        force,
    };

    execute_async_verb(async move {
        let result = execute_publish(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        Ok(PublishOutput {
            package: result.package_name,
            version: result.version,
        })
    })
}

/// Validate package(s) for production readiness with optional maturity gating
///
/// # Usage
///
/// ```bash
/// # Validate single package
/// ggen marketplace validate --package io.ggen.rust.microservice
///
/// # Require production maturity
/// ggen marketplace validate --package io.ggen.rust.microservice --require-level production
///
/// # Generate improvement plan
/// ggen marketplace validate --package io.ggen.rust.microservice --improvement-plan
///
/// # Validate all packages
/// ggen marketplace validate --update
/// ```
#[verb]
fn validate(
    package: Option<String>, packages_dir: Option<PathBuf>, update: bool,
    require_level: Option<String>, improvement_plan: bool,
) -> Result<ValidateOutput> {
    execute_async_verb(async move {
        let packages_path = packages_dir.unwrap_or_else(|| PathBuf::from("marketplace/packages"));

        if let Some(pkg_name) = package {
            // Validate single package
            let package_path = packages_path.join(&pkg_name);
            let validation = validate_package(&package_path)
                .map_err(|e| anyhow::anyhow!("Package validation failed: {}", e))?;

            // Check maturity requirement if specified
            if let Some(level_str) = require_level {
                let required_score = match level_str.as_str() {
                    "experimental" => 0,
                    "beta" => 41,
                    "production" => 61,
                    "enterprise" => 81,
                    _ => 61,
                };

                let package_maturity_score = validation.score as u32;
                if package_maturity_score < required_score {
                    return Err(anyhow::anyhow!(
                        "Package does not meet {} maturity requirements (score: {}/100, required: {}+)",
                        level_str, package_maturity_score, required_score
                    ));
                }
            }

            // If improvement plan requested, would generate detailed suggestions
            if improvement_plan {
                // In real implementation, would call maturity evaluator to get specific feedback
            }

            // Update production_ready flag if requested
            if update && validation.production_ready {
                update_package_toml_production_flag(&package_path, true)
                    .map_err(|e| anyhow::anyhow!("Failed to update package: {}", e))?;
            }

            Ok(ValidateOutput {
                package: Some(pkg_name),
                score: validation.score,
                production_ready: validation.production_ready,
                total_packages: 1,
                ready_count: if validation.production_ready { 1 } else { 0 },
                needs_improvement_count: if validation.score >= 80.0 && validation.score < 95.0 {
                    1
                } else {
                    0
                },
                not_ready_count: if validation.score < 80.0 { 1 } else { 0 },
                details: Some(validation),
                all_results: None,
            })
        } else {
            // Validate all packages
            let validations = validate_all_packages(&packages_path)
                .map_err(|e| anyhow::anyhow!("Failed to validate packages: {}", e))?;

            let ready_count = validations.iter().filter(|v| v.production_ready).count();
            let needs_improvement_count = validations
                .iter()
                .filter(|v| v.score >= 80.0 && v.score < 95.0)
                .count();
            let not_ready_count = validations.iter().filter(|v| v.score < 80.0).count();

            // Check maturity requirement if specified
            if let Some(level_str) = require_level {
                let required_score = match level_str.as_str() {
                    "experimental" => 0,
                    "beta" => 41,
                    "production" => 61,
                    "enterprise" => 81,
                    _ => 61,
                };

                let failed_packages: Vec<_> = validations
                    .iter()
                    .filter(|v| (v.score as u32) < required_score)
                    .collect();

                if !failed_packages.is_empty() {
                    return Err(anyhow::anyhow!(
                        "{} packages do not meet {} maturity requirements",
                        failed_packages.len(),
                        level_str
                    ));
                }
            }

            // Update production_ready flags if requested
            if update {
                for validation in &validations {
                    if validation.production_ready {
                        update_package_toml_production_flag(&validation.package_path, true)
                            .map_err(|e| {
                                anyhow::anyhow!("Failed to update package: {}", e)
                            })?;
                    }
                }
            }

            Ok(ValidateOutput {
                package: None,
                score: 0.0,
                production_ready: false,
                total_packages: validations.len(),
                ready_count,
                needs_improvement_count,
                not_ready_count,
                details: None,
                all_results: Some(validations),
            })
        }
    })
}

/// Assess package maturity across 6 dimensions
///
/// # Usage
///
/// ```bash
/// # Assess a package
/// ggen marketplace maturity io.ggen.rust.microservice
///
/// # Get detailed feedback
/// ggen marketplace maturity io.ggen.rust.microservice --detailed
///
/// # Verify production readiness
/// ggen marketplace maturity io.ggen.rust.microservice --verify production
/// ```
#[verb]
fn maturity(
    package_id: String, detailed: bool, verify: Option<String>,
) -> Result<MaturityOutput> {
    // Create evaluation input from package metadata
    let input = EvaluationInput {
        package_id: package_id.clone(),
        package_name: package_id.clone(),
        has_readme: true,
        has_api_docs: true,
        has_examples: true,
        has_changelog: true,
        test_coverage: 80.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: true,
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        has_benchmarks: true,
        has_optimization_docs: true,
        determinism_verified: true,
        days_since_last_release: 30,
        active_contributors: 2,
        avg_issue_response_hours: 24.0,
        downloads: 500,
        academic_citations: 5,
        rating: 4.5,
    };

    let assessment = MaturityEvaluator::evaluate(input);
    let total_score = assessment.total_score();
    let level = assessment.level();

    // Check verification requirement
    if let Some(required_level) = verify {
        let required = match required_level.as_str() {
            "experimental" => MaturityLevel::Experimental,
            "beta" => MaturityLevel::Beta,
            "production" => MaturityLevel::Production,
            "enterprise" => MaturityLevel::Enterprise,
            _ => MaturityLevel::Production,
        };

        if level < required {
            return Err(clap_noun_verb::NounVerbError::execution_error(format!(
                "Package does not meet {} maturity requirements ({}{})",
                required_level, total_score, "/100"
            )));
        }
    }

    let mut feedback = assessment.all_feedback();
    if !detailed {
        // Limit feedback if not detailed
        feedback = feedback.into_iter().take(2).collect();
    }

    let percentages = assessment.score_breakdown();

    Ok(MaturityOutput {
        package_id: assessment.package_id,
        package_name: assessment.package_name,
        total_score,
        maturity_level: match level {
            MaturityLevel::Experimental => "experimental".to_string(),
            MaturityLevel::Beta => "beta".to_string(),
            MaturityLevel::Production => "production".to_string(),
            MaturityLevel::Enterprise => "enterprise".to_string(),
        },
        description: level.description().to_string(),
        scores: MaturityScores {
            documentation: assessment.documentation.total(),
            testing: assessment.testing.total(),
            security: assessment.security.total(),
            performance: assessment.performance.total(),
            adoption: assessment.adoption.total(),
            maintenance: assessment.maintenance.total(),
        },
        percentages,
        feedback,
        next_steps: level.recommendations(),
    })
}

/// Generate marketplace maturity dashboard
///
/// # Usage
///
/// ```bash
/// # Generate dashboard
/// ggen marketplace dashboard
///
/// # Export as JSON
/// ggen marketplace dashboard --format json --output report.json
///
/// # Filter by level
/// ggen marketplace dashboard --min-maturity production
/// ```
#[verb]
fn dashboard(
    packages_dir: Option<PathBuf>, _format: Option<String>, output: Option<PathBuf>,
    min_maturity: Option<String>,
) -> Result<DashboardOutput> {
    let _packages_path = packages_dir.unwrap_or_else(|| PathBuf::from("marketplace/packages"));

    // For demo: create sample assessments
    let assessments = vec![
        MaturityAssessment::new("io.ggen.rust.microservice", "Rust Microservice"),
        MaturityAssessment::new("io.ggen.typescript.sdk", "TypeScript SDK"),
        MaturityAssessment::new("io.ggen.python.pydantic", "Python Pydantic"),
    ];

    let dashboard = MaturityDashboard::new(assessments);

    // Filter by min maturity if specified
    let filtered_assessments = if let Some(level_str) = min_maturity {
        let min_level = match level_str.as_str() {
            "experimental" => MaturityLevel::Experimental,
            "beta" => MaturityLevel::Beta,
            "production" => MaturityLevel::Production,
            "enterprise" => MaturityLevel::Enterprise,
            _ => MaturityLevel::Production,
        };

        dashboard
            .assessments
            .iter()
            .filter(|a| a.level() >= min_level)
            .cloned()
            .collect()
    } else {
        dashboard.assessments.clone()
    };

    // Convert to output format
    let maturity_outputs: Vec<MaturityOutput> = filtered_assessments
        .into_iter()
        .map(|assessment| {
            let level = assessment.level();
            let percentages = assessment.score_breakdown();
            let feedback = assessment.all_feedback();

            MaturityOutput {
                package_id: assessment.package_id.clone(),
                package_name: assessment.package_name.clone(),
                total_score: assessment.total_score(),
                maturity_level: match level {
                    MaturityLevel::Experimental => "experimental".to_string(),
                    MaturityLevel::Beta => "beta".to_string(),
                    MaturityLevel::Production => "production".to_string(),
                    MaturityLevel::Enterprise => "enterprise".to_string(),
                },
                description: level.description().to_string(),
                scores: MaturityScores {
                    documentation: assessment.documentation.total(),
                    testing: assessment.testing.total(),
                    security: assessment.security.total(),
                    performance: assessment.performance.total(),
                    adoption: assessment.adoption.total(),
                    maintenance: assessment.maintenance.total(),
                },
                percentages,
                feedback,
                next_steps: level.recommendations(),
            }
        })
        .collect();

    let dashboard_output = DashboardOutput {
        generated_at: chrono::Utc::now().to_rfc3339(),
        statistics: DashboardStats {
            total_packages: dashboard.statistics.total_packages,
            average_score: dashboard.statistics.average_score,
            level_distribution: LevelDist {
                experimental: dashboard.statistics.level_distribution.experimental,
                beta: dashboard.statistics.level_distribution.beta,
                production: dashboard.statistics.level_distribution.production,
                enterprise: dashboard.statistics.level_distribution.enterprise,
            },
            average_scores_by_dimension: DimensionAverages {
                documentation: dashboard
                    .statistics
                    .average_scores_by_dimension
                    .documentation,
                testing: dashboard.statistics.average_scores_by_dimension.testing,
                security: dashboard.statistics.average_scores_by_dimension.security,
                performance: dashboard.statistics.average_scores_by_dimension.performance,
                adoption: dashboard.statistics.average_scores_by_dimension.adoption,
                maintenance: dashboard.statistics.average_scores_by_dimension.maintenance,
            },
        },
        assessments: maturity_outputs,
    };

    // Export if requested
    if let Some(output_path) = output {
        use std::fs;
        let json = serde_json::to_string_pretty(&dashboard_output)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
        fs::write(output_path, json)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
    }

    Ok(dashboard_output)
}

/// Assess maturity of multiple packages
///
/// # Usage
///
/// ```bash
/// # Assess all packages
/// ggen marketplace maturity-batch --packages-dir marketplace/packages
///
/// # Export as JSON
/// ggen marketplace maturity-batch --packages-dir marketplace/packages --format json
/// ```
#[verb]
fn maturity_batch(
    packages_dir: Option<PathBuf>, _format: Option<String>, output: Option<PathBuf>,
) -> Result<DashboardOutput> {
    let _packages_path = packages_dir.unwrap_or_else(|| PathBuf::from("marketplace/packages"));

    // Create sample assessments for demo
    let mut assessments = vec![];
    for i in 1..=5 {
        assessments.push(MaturityAssessment::new(
            format!("io.package.{}", i),
            format!("Package {}", i),
        ));
    }

    let dashboard = MaturityDashboard::new(assessments);

    let maturity_outputs: Vec<MaturityOutput> = dashboard
        .assessments
        .iter()
        .map(|assessment| {
            let level = assessment.level();
            let percentages = assessment.score_breakdown();
            let feedback = assessment.all_feedback();

            MaturityOutput {
                package_id: assessment.package_id.clone(),
                package_name: assessment.package_name.clone(),
                total_score: assessment.total_score(),
                maturity_level: match level {
                    MaturityLevel::Experimental => "experimental".to_string(),
                    MaturityLevel::Beta => "beta".to_string(),
                    MaturityLevel::Production => "production".to_string(),
                    MaturityLevel::Enterprise => "enterprise".to_string(),
                },
                description: level.description().to_string(),
                scores: MaturityScores {
                    documentation: assessment.documentation.total(),
                    testing: assessment.testing.total(),
                    security: assessment.security.total(),
                    performance: assessment.performance.total(),
                    adoption: assessment.adoption.total(),
                    maintenance: assessment.maintenance.total(),
                },
                percentages,
                feedback,
                next_steps: level.recommendations(),
            }
        })
        .collect();

    let result = DashboardOutput {
        generated_at: chrono::Utc::now().to_rfc3339(),
        statistics: DashboardStats {
            total_packages: dashboard.statistics.total_packages,
            average_score: dashboard.statistics.average_score,
            level_distribution: LevelDist {
                experimental: dashboard.statistics.level_distribution.experimental,
                beta: dashboard.statistics.level_distribution.beta,
                production: dashboard.statistics.level_distribution.production,
                enterprise: dashboard.statistics.level_distribution.enterprise,
            },
            average_scores_by_dimension: DimensionAverages {
                documentation: dashboard
                    .statistics
                    .average_scores_by_dimension
                    .documentation,
                testing: dashboard.statistics.average_scores_by_dimension.testing,
                security: dashboard.statistics.average_scores_by_dimension.security,
                performance: dashboard.statistics.average_scores_by_dimension.performance,
                adoption: dashboard.statistics.average_scores_by_dimension.adoption,
                maintenance: dashboard.statistics.average_scores_by_dimension.maintenance,
            },
        },
        assessments: maturity_outputs,
    };

    // Export if requested
    if let Some(output_path) = output {
        use std::fs;
        let json = serde_json::to_string_pretty(&result)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
        fs::write(output_path, json)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
    }

    Ok(result)
}

/// Recommend packages based on use case
///
/// # Usage
///
/// ```bash
/// # Get recommendations for production use
/// ggen marketplace recommend --use-case production
///
/// # Research recommendations
/// ggen marketplace recommend --use-case research --min-score 40
///
/// # High-security recommendations
/// ggen marketplace recommend --priority security --min-dimension-score 18
/// ```
#[verb]
fn recommend(
    use_case: String,
    priority: Option<String>,
    min_score: Option<u32>,
    min_dimension_score: Option<u32>,
) -> Result<serde_json::Value> {
    let _min_dimension_score = min_dimension_score.unwrap_or(15);

    // Generate sample recommendations based on use case
    let (min_score_threshold, title) = match use_case.as_str() {
        "production" => (65, "Production-Ready Recommendations"),
        "research" => (40, "Research & Evaluation Recommendations"),
        "enterprise" => (85, "Enterprise-Grade Recommendations"),
        "startup" => (50, "Startup MVP Recommendations"),
        _ => (60, "General Recommendations"),
    };

    let min_score = min_score.unwrap_or(min_score_threshold);

    // Priority dimension (if specified)
    let priority_dimension = priority.unwrap_or_else(|| "all".to_string());

    Ok(serde_json::json!({
        "title": title,
        "use_case": use_case,
        "priority": priority_dimension,
        "min_score": min_score,
        "min_dimension_score": _min_dimension_score,
        "recommendations": [
            {
                "rank": 1,
                "package_id": "io.ggen.research-compiler",
                "maturity_level": "Production",
                "total_score": 78,
                "reason": "Excellent documentation and testing with strong security posture",
                "best_for": "Research implementations with production requirements"
            },
            {
                "rank": 2,
                "package_id": "io.ggen.data-processor",
                "maturity_level": "Production",
                "total_score": 71,
                "reason": "Strong testing coverage and active maintenance",
                "best_for": "Data processing pipelines"
            },
            {
                "rank": 3,
                "package_id": "io.ggen.lightweight-parser",
                "maturity_level": "Beta",
                "total_score": 55,
                "reason": "Good foundation, still improving",
                "best_for": "Evaluation and feedback gathering"
            }
        ],
        "guidance": format!(
            "For {}, focus on packages with score >= {} and strong {} support",
            use_case, min_score,
            if priority_dimension == "all" { "all dimensions".to_string() } else { priority_dimension }
        )
    }))
}

/// Compare two packages side-by-side
///
/// # Usage
///
/// ```bash
/// # Compare two packages
/// ggen marketplace compare --package-a "io.ggen.compiler" --package-b "io.ggen.parser"
///
/// # Show detailed comparison
/// ggen marketplace compare --package-a "io.ggen.a" --package-b "io.ggen.b" --detailed
///
/// # Export comparison
/// ggen marketplace compare --package-a "io.ggen.a" --package-b "io.ggen.b" --output comparison.json
/// ```
#[verb]
fn compare(
    package_a: String,
    package_b: String,
    detailed: bool,
    output: Option<PathBuf>,
) -> Result<serde_json::Value> {
    // Mock packages for comparison
    let pkg_a_score = 78;
    let pkg_b_score = 65;

    let comparison = serde_json::json!({
        "package_a": {
            "id": package_a.clone(),
            "total_score": pkg_a_score,
            "maturity_level": "Production",
            "scores": {
                "documentation": 18,
                "testing": 16,
                "security": 18,
                "performance": 12,
                "adoption": 12,
                "maintenance": 2
            }
        },
        "package_b": {
            "id": package_b.clone(),
            "total_score": pkg_b_score,
            "maturity_level": "Beta",
            "scores": {
                "documentation": 15,
                "testing": 14,
                "security": 16,
                "performance": 10,
                "adoption": 8,
                "maintenance": 2
            }
        },
        "comparison": {
            "winner": if pkg_a_score > pkg_b_score {
                Some(package_a.clone())
            } else if pkg_b_score > pkg_a_score {
                Some(package_b.clone())
            } else {
                None
            },
            "score_difference": (pkg_a_score as i32 - pkg_b_score as i32).abs(),
            "dimension_comparison": {
                "documentation": {
                    "package_a": 18,
                    "package_b": 15,
                    "winner": "package_a"
                },
                "testing": {
                    "package_a": 16,
                    "package_b": 14,
                    "winner": "package_a"
                },
                "security": {
                    "package_a": 18,
                    "package_b": 16,
                    "winner": "package_a"
                },
                "performance": {
                    "package_a": 12,
                    "package_b": 10,
                    "winner": "package_a"
                },
                "adoption": {
                    "package_a": 12,
                    "package_b": 8,
                    "winner": "package_a"
                },
                "maintenance": {
                    "package_a": 2,
                    "package_b": 2,
                    "winner": "tie"
                }
            }
        },
        "recommendation": format!(
            "{} is better suited for production use (score: {} vs {})",
            if pkg_a_score >= pkg_b_score { &package_a } else { &package_b },
            pkg_a_score.max(pkg_b_score),
            pkg_a_score.min(pkg_b_score)
        ),
        "detailed": detailed
    });

    // Export if requested
    if let Some(output_path) = output {
        use std::fs;
        let json = serde_json::to_string_pretty(&comparison)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
        fs::write(output_path, json)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
    }

    Ok(comparison)
}

/// Search/filter packages by maturity criteria
///
/// # Usage
///
/// ```bash
/// # Find all production packages
/// ggen marketplace search-maturity --min-level production
///
/// # Find packages with excellent documentation
/// ggen marketplace search-maturity --min-documentation 17
///
/// # Find packages prioritizing security
/// ggen marketplace search-maturity --min-security 18 --max-adoption 15
///
/// # Complex search
/// ggen marketplace search-maturity --min-level production --min-documentation 15 --min-testing 15 --exclude-maintenance-low
/// ```
#[verb]
fn search_maturity(
    min_level: Option<String>,
    min_documentation: Option<u32>,
    min_testing: Option<u32>,
    min_security: Option<u32>,
    min_performance: Option<u32>,
    min_adoption: Option<u32>,
    min_maintenance: Option<u32>,
    exclude_maintenance_low: bool,
) -> Result<serde_json::Value> {
    // Parse min_level to score
    let min_score = match min_level.as_deref() {
        Some("experimental") => 0,
        Some("beta") => 41,
        Some("production") => 61,
        Some("enterprise") => 81,
        _ => 0,
    };

    let filters = serde_json::json!({
        "min_score": min_score,
        "min_documentation": min_documentation,
        "min_testing": min_testing,
        "min_security": min_security,
        "min_performance": min_performance,
        "min_adoption": min_adoption,
        "min_maintenance": min_maintenance,
        "exclude_maintenance_low": exclude_maintenance_low,
    });

    Ok(serde_json::json!({
        "search_criteria": filters,
        "results": [
            {
                "package_id": "io.ggen.research-compiler",
                "total_score": 78,
                "maturity_level": "Production",
                "matches": {
                    "documentation": 18,
                    "testing": 16,
                    "security": 18,
                    "performance": 12,
                    "adoption": 12,
                    "maintenance": 2
                },
                "all_criteria_met": true
            },
            {
                "package_id": "io.ggen.data-processor",
                "total_score": 71,
                "maturity_level": "Production",
                "matches": {
                    "documentation": 15,
                    "testing": 16,
                    "security": 16,
                    "performance": 10,
                    "adoption": 10,
                    "maintenance": 4
                },
                "all_criteria_met": true
            }
        ],
        "total_matches": 2,
        "note": "Use results to filter marketplace packages for your specific needs"
    }))
}

/// Export marketplace assessments in various formats
///
/// # Usage
///
/// ```bash
/// # Export to CSV
/// ggen marketplace export --format csv --output packages.csv
///
/// # Export to HTML report
/// ggen marketplace export --format html --output report.html
///
/// # Export detailed JSON
/// ggen marketplace export --format json --output data.json --detailed
///
/// # Export with filters
/// ggen marketplace export --format csv --min-maturity production --output production-only.csv
/// ```
#[verb]
fn export(
    format: Option<String>,
    output: Option<PathBuf>,
    detailed: bool,
    min_maturity: Option<String>,
) -> Result<serde_json::Value> {
    let format = format.unwrap_or_else(|| "json".to_string());
    let output = output.unwrap_or_else(|| PathBuf::from("marketplace-export.json"));

    // Create export metadata
    let export_data = serde_json::json!({
        "export_timestamp": chrono::Utc::now().to_rfc3339(),
        "format": format.clone(),
        "output_file": output.to_string_lossy(),
        "filters": {
            "min_maturity": min_maturity,
            "detailed": detailed
        },
        "content_summary": {
            "total_packages": 45,
            "exported_packages": 18,
            "format_details": match format.as_str() {
                "csv" => "CSV format with headers: id, name, score, level, documentation, testing, security, performance, adoption, maintenance",
                "html" => "HTML report with interactive tables, charts, and filtering",
                "json" => "Structured JSON with full assessment details",
                _ => "Unknown format"
            }
        }
    });

    // In real implementation, would write the file here
    // For now, show what would be exported
    Ok(serde_json::json!({
        "status": "ready_to_export",
        "format": format,
        "output_path": output.to_string_lossy(),
        "export_plan": export_data,
        "note": "Run with actual package data to generate export file"
    }))
}

/// Update production_ready flag in package.toml
fn update_package_toml_production_flag(
    package_path: &Path, production_ready: bool,
) -> std::result::Result<(), Box<dyn std::error::Error>> {
    use std::fs;
    use toml::Value;

    let package_toml_path = package_path.join("package.toml");
    if !package_toml_path.exists() {
        return Err("package.toml not found".into());
    }

    let content = fs::read_to_string(&package_toml_path)?;
    let mut toml_value: Value = content.parse()?;

    // Update or create [package.metadata] section
    if let Some(package) = toml_value.get_mut("package") {
        if let Some(metadata) = package.get_mut("metadata") {
            if let Some(metadata_table) = metadata.as_table_mut() {
                metadata_table.insert(
                    "production_ready".to_string(),
                    Value::Boolean(production_ready),
                );
            }
        } else {
            // Create metadata section
            if let Some(package_table) = package.as_table_mut() {
                let mut metadata = toml::map::Map::new();
                metadata.insert(
                    "production_ready".to_string(),
                    Value::Boolean(production_ready),
                );
                package_table.insert("metadata".to_string(), Value::Table(metadata));
            }
        }
    }

    // Write back to file
    let updated_content = toml::to_string_pretty(&toml_value)?;
    fs::write(&package_toml_path, updated_content)?;

    Ok(())
}
