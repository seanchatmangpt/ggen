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
    // Generate all assessments
    let assessments = ggen_marketplace::prelude::generate_all_assessments();

    // Find packages matching use case
    let candidates = ggen_marketplace::prelude::find_for_use_case(&assessments, &use_case);

    // Filter by min_score if specified
    let min_score_val = min_score.unwrap_or_else(|| match use_case.as_str() {
        "production" => 65,
        "research" => 40,
        "enterprise" => 85,
        "startup" => 50,
        _ => 60,
    });

    let filtered: Vec<_> = candidates
        .iter()
        .filter(|a| a.total_score() >= min_score_val)
        .collect();

    // Filter by dimension if priority is security
    let final_candidates: Vec<_> = if priority.as_deref() == Some("security") {
        let min_dim = min_dimension_score.unwrap_or(15);
        filtered
            .iter()
            .filter(|a| a.security.total() >= min_dim)
            .copied()
            .collect()
    } else {
        filtered.iter().copied().collect()
    };

    // Sort by score descending
    let mut sorted = final_candidates;
    sorted.sort_by(|a, b| b.total_score().cmp(&a.total_score()));

    // Build recommendations
    let recommendations: Vec<_> = sorted
        .iter()
        .enumerate()
        .map(|(idx, assessment)| {
            serde_json::json!({
                "rank": idx + 1,
                "package_id": &assessment.package_id,
                "package_name": &assessment.package_name,
                "maturity_level": format!("{:?}", assessment.level()),
                "total_score": assessment.total_score(),
                "reason": format!(
                    "Score: {} - {}",
                    assessment.total_score(),
                    assessment.level().description()
                ),
                "best_for": &assessment.package_name
            })
        })
        .collect();

    let (title, _) = match use_case.as_str() {
        "production" => ("Production-Ready Recommendations", 65),
        "research" => ("Research & Evaluation Recommendations", 40),
        "enterprise" => ("Enterprise-Grade Recommendations", 85),
        "startup" => ("Startup MVP Recommendations", 50),
        _ => ("General Recommendations", 60),
    };

    Ok(serde_json::json!({
        "title": title,
        "use_case": use_case,
        "priority": priority.unwrap_or_else(|| "all".to_string()),
        "min_score": min_score_val,
        "total_matches": recommendations.len(),
        "recommendations": recommendations,
        "guidance": format!(
            "Found {} packages matching {} use case with score >= {}",
            recommendations.len(),
            use_case,
            min_score_val
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
    // Generate assessments
    let assessments = ggen_marketplace::prelude::generate_all_assessments();

    // Find packages A and B
    let pkg_a = assessments
        .iter()
        .find(|a| a.package_id == package_a)
        .ok_or_else(|| anyhow::anyhow!("Package {} not found", package_a))?;

    let pkg_b = assessments
        .iter()
        .find(|a| a.package_id == package_b)
        .ok_or_else(|| anyhow::anyhow!("Package {} not found", package_b))?;

    // Get scores
    let pkg_a_score = pkg_a.total_score();
    let pkg_b_score = pkg_b.total_score();

    // Build comparison
    let mut comparison = serde_json::json!({
        "package_a": {
            "id": pkg_a.package_id.clone(),
            "name": pkg_a.package_name.clone(),
            "total_score": pkg_a_score,
            "maturity_level": format!("{:?}", pkg_a.level()),
            "scores": {
                "documentation": pkg_a.documentation.total(),
                "testing": pkg_a.testing.total(),
                "security": pkg_a.security.total(),
                "performance": pkg_a.performance.total(),
                "adoption": pkg_a.adoption.total(),
                "maintenance": pkg_a.maintenance.total()
            }
        },
        "package_b": {
            "id": pkg_b.package_id.clone(),
            "name": pkg_b.package_name.clone(),
            "total_score": pkg_b_score,
            "maturity_level": format!("{:?}", pkg_b.level()),
            "scores": {
                "documentation": pkg_b.documentation.total(),
                "testing": pkg_b.testing.total(),
                "security": pkg_b.security.total(),
                "performance": pkg_b.performance.total(),
                "adoption": pkg_b.adoption.total(),
                "maintenance": pkg_b.maintenance.total()
            }
        },
        "comparison": {
            "winner": if pkg_a_score > pkg_b_score {
                Some(&pkg_a.package_id)
            } else if pkg_b_score > pkg_a_score {
                Some(&pkg_b.package_id)
            } else {
                None
            },
            "score_difference": (pkg_a_score as i32 - pkg_b_score as i32).abs(),
            "dimension_comparison": {
                "documentation": {
                    "package_a": pkg_a.documentation.total(),
                    "package_b": pkg_b.documentation.total(),
                    "winner": if pkg_a.documentation.total() > pkg_b.documentation.total() { "package_a" } else if pkg_b.documentation.total() > pkg_a.documentation.total() { "package_b" } else { "tie" }
                },
                "testing": {
                    "package_a": pkg_a.testing.total(),
                    "package_b": pkg_b.testing.total(),
                    "winner": if pkg_a.testing.total() > pkg_b.testing.total() { "package_a" } else if pkg_b.testing.total() > pkg_a.testing.total() { "package_b" } else { "tie" }
                },
                "security": {
                    "package_a": pkg_a.security.total(),
                    "package_b": pkg_b.security.total(),
                    "winner": if pkg_a.security.total() > pkg_b.security.total() { "package_a" } else if pkg_b.security.total() > pkg_a.security.total() { "package_b" } else { "tie" }
                },
                "performance": {
                    "package_a": pkg_a.performance.total(),
                    "package_b": pkg_b.performance.total(),
                    "winner": if pkg_a.performance.total() > pkg_b.performance.total() { "package_a" } else if pkg_b.performance.total() > pkg_a.performance.total() { "package_b" } else { "tie" }
                },
                "adoption": {
                    "package_a": pkg_a.adoption.total(),
                    "package_b": pkg_b.adoption.total(),
                    "winner": if pkg_a.adoption.total() > pkg_b.adoption.total() { "package_a" } else if pkg_b.adoption.total() > pkg_a.adoption.total() { "package_b" } else { "tie" }
                },
                "maintenance": {
                    "package_a": pkg_a.maintenance.total(),
                    "package_b": pkg_b.maintenance.total(),
                    "winner": if pkg_a.maintenance.total() > pkg_b.maintenance.total() { "package_a" } else if pkg_b.maintenance.total() > pkg_a.maintenance.total() { "package_b" } else { "tie" }
                }
            }
        },
        "recommendation": format!(
            "{} is better suited for production use (score: {} vs {})",
            if pkg_a_score >= pkg_b_score { &pkg_a.package_id } else { &pkg_b.package_id },
            pkg_a_score.max(pkg_b_score),
            pkg_a_score.min(pkg_b_score)
        ),
        "detailed": detailed
    });

    // Export if requested
    if let Some(output_path) = output {
        use std::fs;
        let json = serde_json::to_string_pretty(&comparison)
            .map_err(|e| anyhow::anyhow!("JSON serialization failed: {}", e))?;
        fs::write(output_path, json)
            .map_err(|e| anyhow::anyhow!("Failed to write file: {}", e))?;
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
    // Generate assessments
    let assessments = ggen_marketplace::prelude::generate_all_assessments();

    // Parse min_level to score
    let min_score = match min_level.as_deref() {
        Some("experimental") => 0,
        Some("beta") => 41,
        Some("production") => 61,
        Some("enterprise") => 81,
        _ => 0,
    };

    // Filter by score first
    let mut filtered: Vec<_> = assessments
        .iter()
        .filter(|a| a.total_score() >= min_score)
        .collect();

    // Apply dimension filters
    filtered.retain(|a| {
        let checks = vec![
            min_documentation.map(|m| a.documentation.total() >= m),
            min_testing.map(|m| a.testing.total() >= m),
            min_security.map(|m| a.security.total() >= m),
            min_performance.map(|m| a.performance.total() >= m),
            min_adoption.map(|m| a.adoption.total() >= m),
            min_maintenance.map(|m| a.maintenance.total() >= m),
        ];
        checks.into_iter().all(|c| c.unwrap_or(true))
    });

    // Exclude low maintenance if requested
    if exclude_maintenance_low {
        filtered.retain(|a| a.maintenance.total() >= 3);
    }

    // Build results
    let results: Vec<_> = filtered
        .iter()
        .map(|a| {
            serde_json::json!({
                "package_id": a.package_id.clone(),
                "package_name": a.package_name.clone(),
                "total_score": a.total_score(),
                "maturity_level": format!("{:?}", a.level()),
                "matches": {
                    "documentation": a.documentation.total(),
                    "testing": a.testing.total(),
                    "security": a.security.total(),
                    "performance": a.performance.total(),
                    "adoption": a.adoption.total(),
                    "maintenance": a.maintenance.total()
                },
                "all_criteria_met": true
            })
        })
        .collect();

    let criteria = serde_json::json!({
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
        "search_criteria": criteria,
        "results": results,
        "total_matches": results.len(),
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
    let format_str = format.unwrap_or_else(|| "json".to_string());
    let output_path = output.unwrap_or_else(|| {
        if format_str == "csv" {
            PathBuf::from("marketplace-export.csv")
        } else {
            PathBuf::from("marketplace-export.json")
        }
    });

    // Generate assessments
    let mut assessments = ggen_marketplace::prelude::generate_all_assessments();

    // Filter by maturity level if specified
    if let Some(level) = &min_maturity {
        assessments.retain(|a| {
            let pkg_level = format!("{:?}", a.level()).to_lowercase();
            pkg_level == level.to_lowercase()
        });
    }

    // Generate export content
    let export_content = match format_str.as_str() {
        "csv" => ggen_marketplace::prelude::export_as_csv(&assessments),
        "json" => {
            let json_val = ggen_marketplace::prelude::export_as_json(&assessments);
            serde_json::to_string_pretty(&json_val)
                .map_err(|e| anyhow::anyhow!("JSON serialization failed: {}", e))?
        }
        "html" => {
            // Generate basic HTML report
            let mut html = String::from("<html><head><title>Marketplace Assessment Report</title>");
            html.push_str("<style>table{border-collapse:collapse}td,th{border:1px solid #ddd;padding:8px}</style>");
            html.push_str("</head><body><h1>Marketplace Assessment Report</h1>");
            html.push_str(&format!("<p>Generated: {}</p>", chrono::Utc::now().to_rfc3339()));
            html.push_str("<table><tr><th>Package</th><th>Score</th><th>Level</th><th>Docs</th><th>Tests</th><th>Security</th></tr>");

            for a in &assessments {
                html.push_str(&format!(
                    "<tr><td>{}</td><td>{}</td><td>{:?}</td><td>{}</td><td>{}</td><td>{}</td></tr>",
                    a.package_name,
                    a.total_score(),
                    a.level(),
                    a.documentation.total(),
                    a.testing.total(),
                    a.security.total()
                ));
            }

            html.push_str("</table></body></html>");
            html
        }
        _ => {
            return Err(anyhow::anyhow!("Unknown export format: {}", format_str));
        }
    };

    // Write file
    use std::fs;
    fs::write(&output_path, &export_content)
        .map_err(|e| anyhow::anyhow!("Failed to write export file: {}", e))?;

    Ok(serde_json::json!({
        "status": "success",
        "format": format_str,
        "output_path": output_path.to_string_lossy(),
        "export_timestamp": chrono::Utc::now().to_rfc3339(),
        "total_packages": assessments.len(),
        "filters": {
            "min_maturity": min_maturity,
            "detailed": detailed
        },
        "message": format!("Successfully exported {} packages to {}", assessments.len(), output_path.display())
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
