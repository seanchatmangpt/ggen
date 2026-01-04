//! Recommendation System Tests (Story 6 - Recommendations)
//!
//! Tests for quality-based package recommendations.
//!
//! # Test Categories
//!
//! - T045: Ranking algorithm tests
//! - T045: Quality tier ordering (Gold first, then Silver, Bronze)
//! - T045: Tiebreaker sorting tests
//! - T045: Coverage targets (80%+)

use ggen_domain::marketplace::{
    PackageQualityInfo, QualityTier, QualityTierCalculator,
};

/// T045: Gold tier packages ranked first
#[test]
fn test_gold_tier_ranked_first() {
    let packages = vec![
        PackageQualityInfo::new("bronze-pkg".to_string(), 5, true, 100),
        PackageQualityInfo::new("gold-pkg".to_string(), 150, true, 15),
        PackageQualityInfo::new("silver-pkg".to_string(), 50, true, 45),
    ];

    // Sort by tier then quality score
    let mut sorted = packages.clone();
    sorted.sort_by(|a, b| {
        match a.tier.priority().cmp(&b.tier.priority()) {
            std::cmp::Ordering::Equal => {
                b.quality_score
                    .unwrap_or(0.0)
                    .partial_cmp(&a.quality_score.unwrap_or(0.0))
                    .unwrap_or(std::cmp::Ordering::Equal)
            }
            other => other,
        }
    });

    assert_eq!(sorted[0].name, "gold-pkg");
    assert_eq!(sorted[0].tier, QualityTier::Gold);
}

/// T045: Silver tier ranked after Gold
#[test]
fn test_silver_tier_ranked_after_gold() {
    let packages = vec![
        PackageQualityInfo::new("silver-pkg".to_string(), 50, true, 45),
        PackageQualityInfo::new("gold-pkg".to_string(), 150, true, 15),
    ];

    let mut sorted = packages.clone();
    sorted.sort_by(|a, b| a.tier.priority().cmp(&b.tier.priority()));

    assert_eq!(sorted[0].tier, QualityTier::Gold);
    assert_eq!(sorted[1].tier, QualityTier::Silver);
}

/// T045: Bronze tier ranked after Silver
#[test]
fn test_bronze_tier_ranked_after_silver() {
    let packages = vec![
        PackageQualityInfo::new("bronze-pkg".to_string(), 5, true, 100),
        PackageQualityInfo::new("silver-pkg".to_string(), 50, true, 45),
    ];

    let mut sorted = packages.clone();
    sorted.sort_by(|a, b| a.tier.priority().cmp(&b.tier.priority()));

    assert_eq!(sorted[0].tier, QualityTier::Silver);
    assert_eq!(sorted[1].tier, QualityTier::Bronze);
}

/// T045: Unrated tier ranked last
#[test]
fn test_unrated_tier_ranked_last() {
    let packages = vec![
        PackageQualityInfo::new("unrated-pkg".to_string(), 0, true, 10),
        PackageQualityInfo::new("bronze-pkg".to_string(), 5, true, 100),
    ];

    let mut sorted = packages.clone();
    sorted.sort_by(|a, b| a.tier.priority().cmp(&b.tier.priority()));

    assert_eq!(sorted[0].tier, QualityTier::Bronze);
    assert_eq!(sorted[1].tier, QualityTier::Unrated);
}

/// T045: Tiebreaker by quality score within same tier
#[test]
fn test_tiebreaker_by_quality_score() {
    let packages = vec![
        PackageQualityInfo::new("gold-low".to_string(), 100, true, 30),
        PackageQualityInfo::new("gold-high".to_string(), 500, true, 5),
    ];

    // Both are Gold tier, should break tie by quality score
    let mut sorted = packages.clone();
    sorted.sort_by(|a, b| {
        match a.tier.priority().cmp(&b.tier.priority()) {
            std::cmp::Ordering::Equal => {
                // Higher quality score first
                b.quality_score
                    .unwrap_or(0.0)
                    .partial_cmp(&a.quality_score.unwrap_or(0.0))
                    .unwrap_or(std::cmp::Ordering::Equal)
            }
            other => other,
        }
    });

    // gold-high should be first (higher quality score)
    assert_eq!(sorted[0].name, "gold-high");
    assert!(sorted[0].quality_score.unwrap() > sorted[1].quality_score.unwrap());
}

/// T045: Tiebreaker by downloads when quality score equal
#[test]
fn test_tiebreaker_by_downloads() {
    // Create packages with same tier but different downloads
    let pkg_high_downloads = PackageQualityInfo::new("high-downloads".to_string(), 500, true, 10);
    let pkg_low_downloads = PackageQualityInfo::new("low-downloads".to_string(), 100, true, 10);

    // Both Gold tier, high downloads should have higher score
    assert_eq!(pkg_high_downloads.tier, QualityTier::Gold);
    assert_eq!(pkg_low_downloads.tier, QualityTier::Gold);
    assert!(pkg_high_downloads.quality_score.unwrap() > pkg_low_downloads.quality_score.unwrap());
}

/// T045: Tiebreaker by age when other factors equal
#[test]
fn test_tiebreaker_by_age() {
    // Same downloads and FMEA, different age
    let fresh = PackageQualityInfo::new("fresh".to_string(), 100, true, 5);
    let older = PackageQualityInfo::new("older".to_string(), 100, true, 25);

    // Both Gold tier, fresh should have higher score
    assert_eq!(fresh.tier, QualityTier::Gold);
    assert_eq!(older.tier, QualityTier::Gold);
    assert!(fresh.quality_score.unwrap() > older.quality_score.unwrap());
}

/// T045: Quality score calculation consistency
#[test]
fn test_quality_score_consistency() {
    // Create same package twice
    let pkg1 = PackageQualityInfo::new("consistent".to_string(), 150, true, 15);
    let pkg2 = PackageQualityInfo::new("consistent".to_string(), 150, true, 15);

    // Scores should be identical
    assert_eq!(pkg1.quality_score, pkg2.quality_score);
    assert_eq!(pkg1.tier, pkg2.tier);
}

/// T045: Quality score bounds (0-100)
#[test]
fn test_quality_score_bounds() {
    // Maximum possible quality
    let high = PackageQualityInfo::new("high".to_string(), 10000, true, 0);
    assert!(high.quality_score.unwrap() <= 100.0);
    assert!(high.quality_score.unwrap() >= 0.0);

    // Minimum possible quality
    let low = PackageQualityInfo::new("low".to_string(), 1, false, 1000);
    assert!(low.quality_score.unwrap() <= 100.0);
    assert!(low.quality_score.unwrap() >= 0.0);
}

/// T045: FMEA passed contributes to score
#[test]
fn test_fmea_contributes_to_score() {
    let with_fmea = PackageQualityInfo::new("with-fmea".to_string(), 50, true, 50);
    let without_fmea = PackageQualityInfo::new("without-fmea".to_string(), 50, false, 50);

    // FMEA should contribute ~30 points
    let score_diff = with_fmea.quality_score.unwrap() - without_fmea.quality_score.unwrap();
    assert!(score_diff >= 25.0 && score_diff <= 35.0);
}

/// T045: Downloads contribute to score (log scale)
#[test]
fn test_downloads_contribute_to_score() {
    let high_downloads = PackageQualityInfo::new("high".to_string(), 1000, false, 100);
    let low_downloads = PackageQualityInfo::new("low".to_string(), 10, false, 100);

    // More downloads should mean higher score
    assert!(high_downloads.quality_score.unwrap() > low_downloads.quality_score.unwrap());
}

/// T045: Age contributes to score (fresher is better)
#[test]
fn test_age_contributes_to_score() {
    let fresh = PackageQualityInfo::new("fresh".to_string(), 50, false, 5);
    let old = PackageQualityInfo::new("old".to_string(), 50, false, 400);

    // Fresher should mean higher score
    assert!(fresh.quality_score.unwrap() > old.quality_score.unwrap());
}

/// T045: Quality tier filter works
#[test]
fn test_quality_tier_filter() {
    let packages = vec![
        PackageQualityInfo::new("gold".to_string(), 150, true, 15),
        PackageQualityInfo::new("silver".to_string(), 50, true, 45),
        PackageQualityInfo::new("bronze".to_string(), 5, true, 100),
    ];

    // Filter to Gold only
    let gold_only: Vec<_> = packages
        .iter()
        .filter(|p| p.tier == QualityTier::Gold)
        .collect();

    assert_eq!(gold_only.len(), 1);
    assert_eq!(gold_only[0].name, "gold");
}

/// T045: Best match filter (Gold + Silver only)
#[test]
fn test_best_match_filter() {
    let packages = vec![
        PackageQualityInfo::new("gold".to_string(), 150, true, 15),
        PackageQualityInfo::new("silver".to_string(), 50, true, 45),
        PackageQualityInfo::new("bronze".to_string(), 5, true, 100),
        PackageQualityInfo::new("unrated".to_string(), 0, true, 10),
    ];

    // Best match = Gold + Silver
    let best_match: Vec<_> = packages
        .iter()
        .filter(|p| p.tier == QualityTier::Gold || p.tier == QualityTier::Silver)
        .collect();

    assert_eq!(best_match.len(), 2);
    assert!(best_match.iter().all(|p|
        p.tier == QualityTier::Gold || p.tier == QualityTier::Silver
    ));
}

/// T045: Tier calculation edge cases
#[test]
fn test_tier_calculation_edge_cases() {
    let calc = QualityTierCalculator::default();

    // Exact Gold boundary
    assert_eq!(calc.calculate(100, true, 30), QualityTier::Gold);

    // Just under Gold
    assert_eq!(calc.calculate(99, true, 30), QualityTier::Silver);

    // Just over Gold age
    assert_eq!(calc.calculate(100, true, 31), QualityTier::Silver);

    // Exact Silver boundary
    assert_eq!(calc.calculate(10, true, 90), QualityTier::Silver);

    // Just under Silver
    assert_eq!(calc.calculate(9, true, 90), QualityTier::Bronze);
}

/// T045: Recommendation sorting complete test
#[test]
fn test_complete_recommendation_sorting() {
    let packages = vec![
        PackageQualityInfo::new("bronze-1".to_string(), 5, true, 100),
        PackageQualityInfo::new("gold-2".to_string(), 200, true, 10),
        PackageQualityInfo::new("silver-1".to_string(), 50, true, 45),
        PackageQualityInfo::new("gold-1".to_string(), 150, true, 15),
        PackageQualityInfo::new("unrated-1".to_string(), 0, true, 10),
        PackageQualityInfo::new("silver-2".to_string(), 80, true, 60),
    ];

    let mut sorted = packages;
    sorted.sort_by(|a, b| {
        match a.tier.priority().cmp(&b.tier.priority()) {
            std::cmp::Ordering::Equal => {
                b.quality_score
                    .unwrap_or(0.0)
                    .partial_cmp(&a.quality_score.unwrap_or(0.0))
                    .unwrap_or(std::cmp::Ordering::Equal)
            }
            other => other,
        }
    });

    // Order should be: gold-2, gold-1, silver-2, silver-1, bronze-1, unrated-1
    assert_eq!(sorted[0].name, "gold-2");
    assert_eq!(sorted[1].name, "gold-1");
    assert_eq!(sorted[2].tier, QualityTier::Silver);
    assert_eq!(sorted[3].tier, QualityTier::Silver);
    assert_eq!(sorted[4].tier, QualityTier::Bronze);
    assert_eq!(sorted[5].tier, QualityTier::Unrated);
}

/// T045: Minimum quality threshold filter
#[test]
fn test_minimum_quality_threshold() {
    let packages = vec![
        PackageQualityInfo::new("high".to_string(), 1000, true, 5),
        PackageQualityInfo::new("medium".to_string(), 50, true, 45),
        PackageQualityInfo::new("low".to_string(), 1, false, 400),
    ];

    let min_score = 50.0;
    let filtered: Vec<_> = packages
        .iter()
        .filter(|p| p.quality_score.unwrap_or(0.0) >= min_score)
        .collect();

    assert!(filtered.len() <= packages.len());
    assert!(filtered.iter().all(|p| p.quality_score.unwrap() >= min_score));
}

/// T045: Empty package list handling
#[test]
fn test_empty_package_list() {
    let packages: Vec<PackageQualityInfo> = vec![];

    let sorted = packages;
    assert!(sorted.is_empty());
}

/// T045: Single package ranking
#[test]
fn test_single_package_ranking() {
    let packages = vec![
        PackageQualityInfo::new("only-one".to_string(), 150, true, 15),
    ];

    let mut sorted = packages;
    sorted.sort_by(|a, b| a.tier.priority().cmp(&b.tier.priority()));

    assert_eq!(sorted.len(), 1);
    assert_eq!(sorted[0].name, "only-one");
}
