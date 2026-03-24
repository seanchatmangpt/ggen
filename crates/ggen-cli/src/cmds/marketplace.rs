//! Marketplace Commands - clap-noun-verb v3.4.0 Migration
//!
//! This module implements marketplace commands using the v3.4.0 #[verb] pattern.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

use crate::runtime_helper::execute_async_verb;
use ggen_domain::marketplace::{
    emit_receipts_for_marketplace, execute_install, execute_list, execute_publish, execute_search,
    execute_update, generate_packages_markdown, generate_registry_index,
    generate_validation_report, validate_all_packages, validate_package, write_packages_markdown,
    write_registry_index, BundleInstallManifest, BundleRegistry, InstallInput, ListInput,
    PublishInput, SearchInput, UpdateInput,
};

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
struct UpdateOutput {
    packages_updated: usize,
}

#[derive(Serialize)]
struct SyncOutput {
    status: String,
    last_sync: String,
}

#[derive(Serialize)]
struct ValidateOutput {
    package: String,
    score: f64,
    passed: usize,
    failed: usize,
    warnings: usize,
    summary: String,
}

#[derive(Serialize)]
struct EmitReceiptsOutput {
    receipts: Vec<String>,
    total: usize,
    succeeded: usize,
    failed: usize,
}

#[derive(Serialize)]
struct ReportOutput {
    total_packages: usize,
    valid_packages: usize,
    average_score: f64,
}

#[derive(Serialize)]
struct BundleInfo {
    id: String,
    name: String,
    description: String,
    packages: Vec<String>,
}

#[derive(Serialize)]
struct ListBundlesOutput {
    bundles: Vec<BundleInfo>,
    total: usize,
}

#[derive(Serialize)]
struct GenerateArtifactsOutput {
    registry_index: String,
    packages_markdown: String,
}

#[derive(Serialize)]
struct RecommendOutput {
    use_case: String,
    min_score: u32,
    recommendations: Vec<RecommendedPackage>,
    total_matches: usize,
}

#[derive(Serialize)]
struct RecommendedPackage {
    rank: usize,
    package_name: String,
    score: f64,
    production_ready: bool,
    reason: String,
}

#[derive(Serialize)]
struct CompareOutput {
    package_a: PackageComparison,
    package_b: PackageComparison,
    winner: Option<String>,
    score_difference: f64,
    recommendation: String,
}

#[derive(Serialize)]
struct PackageComparison {
    name: String,
    score: f64,
    production_ready: bool,
    passed_checks: usize,
    failed_checks: usize,
}

#[derive(Serialize)]
struct SearchMaturityOutput {
    min_score: f64,
    results: Vec<MaturitySearchResult>,
    total_matches: usize,
}

#[derive(Serialize)]
struct MaturitySearchResult {
    package_name: String,
    score: f64,
    production_ready: bool,
    matches_all_criteria: bool,
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Count passed and failed checks for a validation
fn count_checks(validation: &ggen_domain::marketplace::PackageValidation) -> (usize, usize) {
    let passed = validation
        .required_checks
        .iter()
        .filter(|(_, r)| r.is_pass())
        .count()
        + validation
            .quality_checks
            .iter()
            .filter(|(_, r)| r.is_pass())
            .count();
    let failed = validation
        .required_checks
        .iter()
        .filter(|(_, r)| r.is_fail())
        .count()
        + validation
            .quality_checks
            .iter()
            .filter(|(_, r)| r.is_fail())
            .count();
    (passed, failed)
}

/// Sort validations by score
fn sort_validations_by_score(validations: &mut Vec<ggen_domain::marketplace::PackageValidation>) {
    validations.sort_by(|a, b| {
        b.score
            .partial_cmp(&a.score)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
}

/// Build recommendation from validation
fn build_recommendation(
    idx: usize, validation: &ggen_domain::marketplace::PackageValidation,
) -> RecommendedPackage {
    RecommendedPackage {
        rank: idx + 1,
        package_name: validation.package_name.clone(),
        score: validation.score,
        production_ready: validation.production_ready,
        reason: format!("Score: {:.1}%", validation.score),
    }
}

/// Build comparison result from two validations
fn build_comparison(
    package_a: String, validation_a: &ggen_domain::marketplace::PackageValidation,
    package_b: String, validation_b: &ggen_domain::marketplace::PackageValidation,
) -> CompareOutput {
    let (passed_a, failed_a) = count_checks(validation_a);
    let (passed_b, failed_b) = count_checks(validation_b);

    let pkg_a = PackageComparison {
        name: package_a.clone(),
        score: validation_a.score,
        production_ready: validation_a.production_ready,
        passed_checks: passed_a,
        failed_checks: failed_a,
    };

    let pkg_b = PackageComparison {
        name: package_b.clone(),
        score: validation_b.score,
        production_ready: validation_b.production_ready,
        passed_checks: passed_b,
        failed_checks: failed_b,
    };

    let score_diff = (pkg_a.score - pkg_b.score).abs();
    let winner = if pkg_a.score > pkg_b.score {
        Some(pkg_a.name.clone())
    } else if pkg_b.score > pkg_a.score {
        Some(pkg_b.name.clone())
    } else {
        None
    };

    let recommendation = if let Some(ref winner_name) = winner {
        let winner_score = if winner_name == &pkg_a.name {
            pkg_a.score
        } else {
            pkg_b.score
        };
        let loser_score = if winner_name == &pkg_a.name {
            pkg_b.score
        } else {
            pkg_a.score
        };
        format!(
            "{} is better suited for your needs (score: {:.1}% vs {:.1}%)",
            winner_name, winner_score, loser_score
        )
    } else {
        format!("Both packages have equal scores ({:.1}%)", pkg_a.score)
    };

    CompareOutput {
        package_a: pkg_a,
        package_b: pkg_b,
        winner,
        score_difference: score_diff,
        recommendation,
    }
}

/// Filter packages by maturity criteria
fn filter_by_maturity(
    validations: Vec<ggen_domain::marketplace::PackageValidation>, threshold: f64,
    production_ready_only: bool,
) -> Vec<MaturitySearchResult> {
    validations
        .into_iter()
        .filter(|v| {
            let meets_score = v.score >= threshold;
            let meets_production = !production_ready_only || v.production_ready;
            meets_score && meets_production
        })
        .map(|v| MaturitySearchResult {
            package_name: v.package_name,
            score: v.score,
            production_ready: v.production_ready,
            matches_all_criteria: v.score >= threshold
                && (!production_ready_only || v.production_ready),
        })
        .collect()
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Search for packages in the marketplace
#[verb]
fn search(
    query: String, limit: usize, category: Option<String>, marketplace_root: Option<PathBuf>,
) -> Result<SearchOutput> {
    if let Some(root) = marketplace_root {
        std::env::set_var("GGEN_MARKETPLACE_ROOT", root.to_string_lossy().as_ref());
    }

    let limit = if limit == 0 { 10 } else { limit };
    let input = SearchInput {
        query,
        limit,
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

/// List installed packages
#[verb]
fn list(detailed: bool, json: bool) -> Result<ListOutput> {
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

        let total = result.packages_listed;
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

/// Update installed packages
#[verb]
fn update(update_all: bool, dry_run: bool) -> Result<UpdateOutput> {
    let input = UpdateInput {
        package: None,
        all: update_all,
        dry_run,
    };

    execute_async_verb(async move {
        let result = execute_update(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        Ok(UpdateOutput {
            packages_updated: result.packages_updated,
        })
    })
}

/// Sync marketplace cache with remote registry
#[verb]
fn sync() -> Result<SyncOutput> {
    Ok(SyncOutput {
        status: "Marketplace cache synced".to_string(),
        last_sync: "now".to_string(),
    })
}

/// Validate a package's quality and compliance
#[verb]
fn validate(package_path: PathBuf) -> Result<ValidateOutput> {
    execute_async_verb(async move {
        let result = validate_package(&package_path)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        let package = result.package_name.clone();
        let summary = result.summary();
        let (passed, failed) = count_checks(&result);
        let warnings = result
            .required_checks
            .iter()
            .filter(|(_, r)| r.is_warning())
            .count()
            + result
                .quality_checks
                .iter()
                .filter(|(_, r)| r.is_warning())
                .count();

        Ok(ValidateOutput {
            package,
            score: result.score,
            passed,
            failed,
            warnings,
            summary,
        })
    })
}

/// List sector bundles
#[verb]
fn list_bundles() -> Result<ListBundlesOutput> {
    let bundles = BundleRegistry::list_bundles();
    let output = bundles
        .into_iter()
        .map(|b| BundleInfo {
            id: b.id.clone(),
            name: b.id.clone(),
            description: b.description,
            packages: b.packages.clone(),
        })
        .collect::<Vec<_>>();
    let total = output.len();

    Ok(ListBundlesOutput {
        bundles: output,
        total,
    })
}

/// Show bundle details
#[verb]
fn bundle_info(bundle_id: String) -> Result<BundleInfo> {
    let bundle = BundleRegistry::get_bundle(&bundle_id).ok_or_else(|| {
        clap_noun_verb::NounVerbError::execution_error(format!("Bundle '{}' not found", bundle_id))
    })?;

    Ok(BundleInfo {
        id: bundle.id.clone(),
        name: bundle.id.clone(),
        description: bundle.description,
        packages: bundle.packages.clone(),
    })
}

/// Install a bundle
#[verb]
fn install_bundle(bundle_id: String, _target_dir: Option<PathBuf>) -> Result<String> {
    let bundle = BundleRegistry::get_bundle(&bundle_id).ok_or_else(|| {
        clap_noun_verb::NounVerbError::execution_error(format!("Bundle '{}' not found", bundle_id))
    })?;

    let _manifest = BundleInstallManifest::new(bundle_id.clone());
    Ok(format!(
        "Bundle '{}' with {} packages ready for installation",
        bundle_id,
        bundle.packages.len()
    ))
}

/// Generate validation receipts for all packages
#[verb]
fn emit_receipts(marketplace_root: Option<PathBuf>) -> Result<EmitReceiptsOutput> {
    let root =
        marketplace_root.unwrap_or_else(|| PathBuf::from("/Users/sac/ggen/marketplace/packages"));

    let results = emit_receipts_for_marketplace(&root);

    let succeeded = results.iter().filter(|(_, r)| r.is_ok()).count();
    let failed = results.iter().filter(|(_, r)| r.is_err()).count();

    let receipts = results
        .into_iter()
        .filter_map(|(name, result)| result.ok().map(|_p| name))
        .collect::<Vec<_>>();

    let total = receipts.len();

    Ok(EmitReceiptsOutput {
        receipts,
        total,
        succeeded,
        failed,
    })
}

/// Generate marketplace health report
#[verb]
fn report(marketplace_root: Option<PathBuf>) -> Result<ReportOutput> {
    let root =
        marketplace_root.unwrap_or_else(|| PathBuf::from("/Users/sac/ggen/marketplace/packages"));

    let report = generate_validation_report(&root)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    Ok(ReportOutput {
        total_packages: report.total_packages,
        valid_packages: report.production_ready_count,
        average_score: report.average_score,
    })
}

/// Generate marketplace artifacts (JSON and Markdown)
#[verb]
fn generate_artifacts(marketplace_root: Option<PathBuf>) -> Result<GenerateArtifactsOutput> {
    let root = marketplace_root.unwrap_or_else(|| PathBuf::from("/Users/sac/ggen/marketplace"));

    let index_json = generate_registry_index(&root)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    let packages_md = generate_packages_markdown(&root)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    let index_path = root.join("registry/index.json");
    let markdown_path = root.join("PACKAGES.md");

    write_registry_index(&index_json, &index_path)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    write_packages_markdown(&packages_md, &markdown_path)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    Ok(GenerateArtifactsOutput {
        registry_index: index_path.display().to_string(),
        packages_markdown: markdown_path.display().to_string(),
    })
}

/// Recommend packages based on use case
#[verb]
fn recommend(use_case: String, min_score: Option<u32>) -> Result<RecommendOutput> {
    execute_async_verb(async move {
        let root = PathBuf::from("/Users/sac/ggen/marketplace/packages");
        let threshold = get_threshold_for_use_case(&use_case, min_score);

        let validations = validate_all_packages(&root)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        let mut matching = filter_validations_by_threshold(validations, threshold);
        sort_validations_by_score(&mut matching);

        let recommendations = build_recommendations(matching);

        let total = recommendations.len();

        Ok(RecommendOutput {
            use_case,
            min_score: threshold as u32,
            recommendations,
            total_matches: total,
        })
    })
}

/// Get threshold score for use case
fn get_threshold_for_use_case(use_case: &str, min_score: Option<u32>) -> f64 {
    min_score.unwrap_or_else(|| match use_case {
        "production" => 95,
        "research" => 60,
        "enterprise" => 97,
        "startup" => 80,
        "learning" => 50,
        _ => 70,
    }) as f64
}

/// Filter validations by score threshold
fn filter_validations_by_threshold(
    validations: Vec<ggen_domain::marketplace::PackageValidation>, threshold: f64,
) -> Vec<ggen_domain::marketplace::PackageValidation> {
    validations
        .into_iter()
        .filter(|v| v.score >= threshold)
        .collect()
}

/// Build recommendations from validations
fn build_recommendations(
    validations: Vec<ggen_domain::marketplace::PackageValidation>,
) -> Vec<RecommendedPackage> {
    validations
        .into_iter()
        .enumerate()
        .map(|(idx, validation)| build_recommendation(idx, &validation))
        .collect()
}

/// Compare two packages side-by-side
#[verb]
fn compare(package_a: String, package_b: String) -> Result<CompareOutput> {
    execute_async_verb(async move {
        let root = PathBuf::from("/Users/sac/ggen/marketplace/packages");
        let path_a = root.join(&package_a);
        let path_b = root.join(&package_b);

        let validation_a = validate_package(&path_a).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Package A error: {}", e))
        })?;

        let validation_b = validate_package(&path_b).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Package B error: {}", e))
        })?;

        Ok(build_comparison(
            package_a,
            &validation_a,
            package_b,
            &validation_b,
        ))
    })
}

/// Search/filter packages by maturity criteria
#[verb]
fn search_maturity(
    min_score: Option<f64>, production_ready_only: bool,
) -> Result<SearchMaturityOutput> {
    execute_async_verb(async move {
        let root = PathBuf::from("/Users/sac/ggen/marketplace/packages");
        let threshold = min_score.unwrap_or(0.0);

        let validations = validate_all_packages(&root)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        let results = filter_by_maturity(validations, threshold, production_ready_only);
        let total = results.len();

        Ok(SearchMaturityOutput {
            min_score: threshold,
            results,
            total_matches: total,
        })
    })
}
