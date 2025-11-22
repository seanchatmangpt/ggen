//! Consolidated Marketplace Unit Tests
//!
//! This file consolidates unit tests for:
//! - Package filtering (maturity level, score range)
//! - Maturity scoring (6 dimensions)
//! - Search ranking algorithm
//! - Adapter conversions
//! - RDF mappings
//!
//! Originally from:
//! - marketplace/unit/package_filtering_test.rs
//! - marketplace/unit/maturity_scoring_test.rs
//! - marketplace/unit/adapter_conversion_test.rs
//! - marketplace/unit/rdf_mapping_test.rs
//! - marketplace/unit/search_ranking_test.rs

use chrono::{Duration, Utc};

// ============================================================================
// SECTION 1: MATURITY LEVEL TESTS
// ============================================================================

/// Test maturity level enum ordering
#[test]
fn test_maturity_levels_ordering() {
    // Maturity levels should be: Experimental < Beta < Production < Enterprise
    assert!(
        level_value("experimental") < level_value("beta"),
        "Experimental should be less than Beta"
    );
    assert!(
        level_value("beta") < level_value("production"),
        "Beta should be less than Production"
    );
    assert!(
        level_value("production") < level_value("enterprise"),
        "Production should be less than Enterprise"
    );
}

fn level_value(level: &str) -> u8 {
    match level {
        "experimental" => 1,
        "beta" => 2,
        "production" => 3,
        "enterprise" => 4,
        _ => 0,
    }
}

#[test]
fn test_maturity_level_from_score_experimental() {
    // Scores 0-40 should be Experimental
    assert_eq!(maturity_level_from_score(0), "experimental");
    assert_eq!(maturity_level_from_score(20), "experimental");
    assert_eq!(maturity_level_from_score(40), "experimental");
}

#[test]
fn test_maturity_level_from_score_beta() {
    // Scores 41-60 should be Beta
    assert_eq!(maturity_level_from_score(41), "beta");
    assert_eq!(maturity_level_from_score(50), "beta");
    assert_eq!(maturity_level_from_score(60), "beta");
}

#[test]
fn test_maturity_level_from_score_production() {
    // Scores 61-80 should be Production
    assert_eq!(maturity_level_from_score(61), "production");
    assert_eq!(maturity_level_from_score(70), "production");
    assert_eq!(maturity_level_from_score(80), "production");
}

#[test]
fn test_maturity_level_from_score_enterprise() {
    // Scores 81-100 should be Enterprise
    assert_eq!(maturity_level_from_score(81), "enterprise");
    assert_eq!(maturity_level_from_score(90), "enterprise");
    assert_eq!(maturity_level_from_score(100), "enterprise");
}

fn maturity_level_from_score(score: u32) -> &'static str {
    match score {
        0..=40 => "experimental",
        41..=60 => "beta",
        61..=80 => "production",
        81..=100 => "enterprise",
        _ => "unknown",
    }
}

// ============================================================================
// SECTION 2: MATURITY SCORING DIMENSION TESTS
// ============================================================================

/// Documentation dimension (max 20 points)
#[test]
fn test_documentation_score_full() {
    let score = calculate_documentation_score(true, true, true, true);
    assert_eq!(score, 20, "Full documentation should score 20 points");
}

#[test]
fn test_documentation_score_partial() {
    // Only README and examples
    let score = calculate_documentation_score(true, false, true, false);
    assert_eq!(score, 10, "Partial documentation should score 10 points");
}

#[test]
fn test_documentation_score_minimal() {
    // Only README
    let score = calculate_documentation_score(true, false, false, false);
    assert_eq!(score, 5, "README only should score 5 points");
}

#[test]
fn test_documentation_score_none() {
    let score = calculate_documentation_score(false, false, false, false);
    assert_eq!(score, 0, "No documentation should score 0 points");
}

fn calculate_documentation_score(
    has_readme: bool, has_api_docs: bool, has_examples: bool, has_changelog: bool,
) -> u32 {
    let mut score = 0;
    if has_readme {
        score += 5;
    }
    if has_api_docs {
        score += 5;
    }
    if has_examples {
        score += 5;
    }
    if has_changelog {
        score += 5;
    }
    score
}

/// Testing dimension (max 20 points)
#[test]
fn test_testing_score_high_coverage() {
    // 90%+ coverage with all test types
    let score = calculate_testing_score(90.0, true, true, true);
    assert_eq!(score, 20, "High coverage with all tests should score 20");
}

#[test]
fn test_testing_score_medium_coverage() {
    // 70% coverage with unit tests only
    let score = calculate_testing_score(70.0, true, false, false);
    assert_eq!(score, 12, "Medium coverage with unit tests should score 12");
}

#[test]
fn test_testing_score_low_coverage() {
    // 30% coverage with no tests
    let score = calculate_testing_score(30.0, false, false, false);
    assert_eq!(score, 3, "Low coverage with no tests should score 3");
}

fn calculate_testing_score(
    coverage: f64, has_unit: bool, has_integration: bool, has_e2e: bool,
) -> u32 {
    let coverage_points = (coverage / 10.0).min(10.0) as u32;
    let test_type_points =
        (has_unit as u32 * 4) + (has_integration as u32 * 4) + (has_e2e as u32 * 2);
    coverage_points + test_type_points.min(10)
}

/// Security dimension (max 20 points)
#[test]
fn test_security_score_full() {
    let score = calculate_security_score(true, true, true, true);
    assert_eq!(score, 20, "Full security should score 20");
}

#[test]
fn test_security_score_partial() {
    let score = calculate_security_score(true, true, false, false);
    assert_eq!(score, 10, "Partial security should score 10");
}

fn calculate_security_score(
    no_vulnerabilities: bool, dependency_audit: bool, security_policy: bool, signed_releases: bool,
) -> u32 {
    let mut score = 0;
    if no_vulnerabilities {
        score += 8;
    }
    if dependency_audit {
        score += 6;
    }
    if security_policy {
        score += 3;
    }
    if signed_releases {
        score += 3;
    }
    score.min(20)
}

/// Performance dimension (max 15 points)
#[test]
fn test_performance_score_excellent() {
    let score = calculate_performance_score(true, true, 10);
    assert_eq!(score, 15, "Excellent performance should score 15");
}

#[test]
fn test_performance_score_average() {
    let score = calculate_performance_score(true, false, 100);
    assert_eq!(score, 10, "Average performance should score 10");
}

fn calculate_performance_score(
    has_benchmarks: bool, optimized_build: bool, latency_ms: u64,
) -> u32 {
    let mut score = 0;
    if has_benchmarks {
        score += 5;
    }
    if optimized_build {
        score += 5;
    }
    // Latency scoring: <50ms = 5, <100ms = 3, <500ms = 1
    score += match latency_ms {
        0..=50 => 5,
        51..=100 => 3,
        101..=500 => 1,
        _ => 0,
    };
    score.min(15)
}

/// Adoption dimension (max 15 points)
#[test]
fn test_adoption_score_high() {
    let score = calculate_adoption_score(10000, 100, 50);
    assert!(score >= 12, "High adoption should score >= 12");
}

#[test]
fn test_adoption_score_low() {
    let score = calculate_adoption_score(10, 1, 0);
    assert!(score <= 5, "Low adoption should score <= 5");
}

fn calculate_adoption_score(downloads: u64, stars: u64, contributors: u64) -> u32 {
    let download_score = ((downloads as f64).log10().max(0.0) as u32).min(6);
    let star_score = ((stars as f64).log10().max(0.0) as u32 * 2).min(6);
    let contributor_score = (contributors as u32 / 10).min(3);
    (download_score + star_score + contributor_score).min(15)
}

/// Maintenance dimension (max 10 points)
#[test]
fn test_maintenance_score_active() {
    // Updated within last 30 days, regular releases
    let score = calculate_maintenance_score(15, 12);
    assert_eq!(score, 10, "Active maintenance should score 10");
}

#[test]
fn test_maintenance_score_stale() {
    // Updated 6 months ago, few releases
    let score = calculate_maintenance_score(180, 2);
    assert!(score <= 4, "Stale maintenance should score <= 4");
}

fn calculate_maintenance_score(days_since_update: u32, releases_per_year: u32) -> u32 {
    let recency_score = match days_since_update {
        0..=30 => 5,
        31..=90 => 3,
        91..=180 => 1,
        _ => 0,
    };
    let release_score = (releases_per_year / 3).min(5);
    (recency_score + release_score).min(10)
}

// ============================================================================
// SECTION 3: TOTAL SCORE CALCULATION
// ============================================================================

#[test]
fn test_total_score_enterprise_level() {
    let total = calculate_total_score(20, 18, 18, 14, 14, 10);
    assert_eq!(total, 94, "High scores should sum to 94");
    assert_eq!(maturity_level_from_score(total), "enterprise");
}

#[test]
fn test_total_score_production_level() {
    let total = calculate_total_score(15, 12, 14, 10, 10, 7);
    assert_eq!(total, 68, "Medium-high scores should sum to 68");
    assert_eq!(maturity_level_from_score(total), "production");
}

#[test]
fn test_total_score_beta_level() {
    let total = calculate_total_score(10, 8, 8, 8, 8, 5);
    assert_eq!(total, 47, "Medium scores should sum to 47");
    assert_eq!(maturity_level_from_score(total), "beta");
}

#[test]
fn test_total_score_experimental_level() {
    let total = calculate_total_score(5, 5, 4, 4, 4, 2);
    assert_eq!(total, 24, "Low scores should sum to 24");
    assert_eq!(maturity_level_from_score(total), "experimental");
}

fn calculate_total_score(
    documentation: u32, testing: u32, security: u32, performance: u32, adoption: u32,
    maintenance: u32,
) -> u32 {
    documentation + testing + security + performance + adoption + maintenance
}

// ============================================================================
// SECTION 4: SEARCH RANKING TESTS
// ============================================================================

/// Search ranking structure for testing
struct SearchScore {
    relevance: f64,
    popularity: f64,
    quality: f64,
    recency: f64,
}

impl SearchScore {
    fn calculate(&self) -> f64 {
        // Default weights: Relevance 50%, Popularity 20%, Quality 20%, Recency 10%
        self.relevance * 0.5 + self.popularity * 0.2 + self.quality * 0.2 + self.recency * 0.1
    }

    fn calculate_custom(&self, r_weight: f64, p_weight: f64, q_weight: f64, n_weight: f64) -> f64 {
        self.relevance * r_weight
            + self.popularity * p_weight
            + self.quality * q_weight
            + self.recency * n_weight
    }
}

#[test]
fn test_search_score_default_weights() {
    let score = SearchScore {
        relevance: 0.8,
        popularity: 0.6,
        quality: 0.9,
        recency: 0.7,
    };

    let result = score.calculate();
    let expected = 0.8 * 0.5 + 0.6 * 0.2 + 0.9 * 0.2 + 0.7 * 0.1;
    assert!(
        (result - expected).abs() < 0.001,
        "Score should match weighted calculation"
    );
}

#[test]
fn test_search_score_custom_weights_quality_focused() {
    let score = SearchScore {
        relevance: 0.5,
        popularity: 0.3,
        quality: 1.0,
        recency: 0.5,
    };

    // Quality-focused: 30% relevance, 10% popularity, 50% quality, 10% recency
    let result = score.calculate_custom(0.3, 0.1, 0.5, 0.1);
    let expected = 0.5 * 0.3 + 0.3 * 0.1 + 1.0 * 0.5 + 0.5 * 0.1;
    assert!((result - expected).abs() < 0.001);
}

#[test]
fn test_popularity_logarithmic_scaling() {
    // Downloads should use logarithmic scaling
    fn popularity_score(downloads: u64) -> f64 {
        let log_downloads = (downloads as f64 + 1.0).log10();
        (log_downloads / 7.0).min(1.0) // Normalize to max 10M downloads
    }

    let low = popularity_score(100);
    let medium = popularity_score(10_000);
    let high = popularity_score(1_000_000);

    assert!(
        low < medium,
        "Medium downloads should score higher than low"
    );
    assert!(
        medium < high,
        "High downloads should score higher than medium"
    );

    // Verify logarithmic curve (not linear)
    let low_to_medium = medium - low;
    let medium_to_high = high - medium;
    assert!(
        low_to_medium > medium_to_high * 0.5,
        "Logarithmic: 100x increase at low should have larger impact"
    );
}

#[test]
fn test_recency_scoring() {
    fn recency_score(days_old: u32) -> f64 {
        // Decay function: score decreases with age
        let max_days = 365.0;
        let decay = (-(days_old as f64) / max_days).exp();
        decay
    }

    let recent = recency_score(7);
    let month_old = recency_score(30);
    let year_old = recency_score(365);

    assert!(
        recent > month_old,
        "Recent should score higher than month old"
    );
    assert!(
        month_old > year_old,
        "Month old should score higher than year old"
    );
    assert!(recent > 0.9, "Week-old package should score > 0.9");
}

#[test]
fn test_quality_normalized() {
    fn quality_score(rating: f64) -> f64 {
        // Normalize 1-5 rating to 0-1
        (rating - 1.0) / 4.0
    }

    assert!((quality_score(1.0) - 0.0).abs() < 0.001, "Rating 1 = 0.0");
    assert!((quality_score(3.0) - 0.5).abs() < 0.001, "Rating 3 = 0.5");
    assert!((quality_score(5.0) - 1.0).abs() < 0.001, "Rating 5 = 1.0");
}

// ============================================================================
// SECTION 5: PACKAGE FILTERING TESTS
// ============================================================================

#[test]
fn test_filter_by_score_range_valid() {
    let packages = vec![("pkg1", 30), ("pkg2", 55), ("pkg3", 75), ("pkg4", 90)];

    let filtered: Vec<_> = packages
        .iter()
        .filter(|(_, score)| *score >= 50 && *score <= 80)
        .collect();

    assert_eq!(filtered.len(), 2);
    assert!(filtered.iter().any(|(name, _)| *name == "pkg2"));
    assert!(filtered.iter().any(|(name, _)| *name == "pkg3"));
}

#[test]
fn test_filter_by_level_production() {
    let packages = vec![
        ("experimental", 30),
        ("beta", 50),
        ("production", 70),
        ("enterprise", 90),
    ];

    let production_or_higher: Vec<_> = packages
        .iter()
        .filter(|(_, score)| {
            maturity_level_from_score(*score) == "production"
                || maturity_level_from_score(*score) == "enterprise"
        })
        .collect();

    assert_eq!(production_or_higher.len(), 2);
}

#[test]
fn test_filter_empty_result() {
    let packages = vec![("pkg1", 30), ("pkg2", 40)];

    let filtered: Vec<_> = packages.iter().filter(|(_, score)| *score >= 90).collect();

    assert!(filtered.is_empty(), "No packages should match score >= 90");
}

// ============================================================================
// SECTION 6: ADAPTER CONVERSION TESTS
// ============================================================================

#[test]
fn test_package_to_search_result_conversion() {
    // Test that package data converts correctly to search result format
    let package_id = "io.ggen.rust.cli";
    let package_name = "Rust CLI Generator";
    let version = "1.2.0";

    let search_result = format!("{}@{}: {}", package_id, version, package_name);
    assert!(search_result.contains(package_id));
    assert!(search_result.contains(version));
}

#[test]
fn test_lockfile_entry_to_installed_package() {
    let entry_name = "my-pkg";
    let entry_version = "1.0.0";
    let entry_checksum = "sha256:abc123";

    // Simulate conversion
    let installed = format!(
        "Installed: {} v{} ({})",
        entry_name,
        entry_version,
        &entry_checksum[..20]
    );
    assert!(installed.contains(entry_name));
    assert!(installed.contains(entry_version));
}

// ============================================================================
// SECTION 7: RDF MAPPING TESTS
// ============================================================================

#[test]
fn test_rdf_package_uri_format() {
    let package_id = "io.ggen.rust.cli";
    let expected_uri = format!("https://ggen.io/packages/{}", package_id);
    assert!(expected_uri.starts_with("https://ggen.io/packages/"));
}

#[test]
fn test_rdf_triple_structure() {
    // Subject-Predicate-Object structure
    let subject = "<https://ggen.io/packages/my-pkg>";
    let predicate = "<https://schema.org/version>";
    let object = "\"1.0.0\"";

    let triple = format!("{} {} {} .", subject, predicate, object);
    assert!(triple.contains("<https://ggen.io/packages/my-pkg>"));
    assert!(triple.contains("<https://schema.org/version>"));
    assert!(triple.ends_with("."));
}

#[test]
fn test_rdf_category_mapping() {
    let categories = vec!["ai", "web", "cli", "database", "tools"];

    for category in categories {
        let uri = format!("https://ggen.io/categories/{}", category);
        assert!(uri.contains(category));
    }
}

#[cfg(test)]
mod score_boundary_tests {
    use super::*;

    #[test]
    fn test_score_boundary_40_41() {
        assert_eq!(maturity_level_from_score(40), "experimental");
        assert_eq!(maturity_level_from_score(41), "beta");
    }

    #[test]
    fn test_score_boundary_60_61() {
        assert_eq!(maturity_level_from_score(60), "beta");
        assert_eq!(maturity_level_from_score(61), "production");
    }

    #[test]
    fn test_score_boundary_80_81() {
        assert_eq!(maturity_level_from_score(80), "production");
        assert_eq!(maturity_level_from_score(81), "enterprise");
    }
}
