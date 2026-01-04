//! Quality Tiers Tests for Marketplace (Story 3 - Search & Discover)
//!
//! Tests for Gold/Silver/Bronze quality tier classification.

use ggen_domain::marketplace::{
    PackageQualityInfo, QualityThresholds, QualityTier, QualityTierCalculator,
};

/// T030: Gold tier requirements - FMEA passed + >100 downloads + <30 days old
#[test]
fn test_gold_tier_requirements() {
    let calc = QualityTierCalculator::default();

    // Gold: FMEA passed + >100 downloads + <30 days
    assert_eq!(calc.calculate(150, true, 15), QualityTier::Gold);
    assert_eq!(calc.calculate(100, true, 30), QualityTier::Gold);
    assert_eq!(calc.calculate(500, true, 0), QualityTier::Gold);
    assert_eq!(calc.calculate(1000, true, 29), QualityTier::Gold);
}

/// T030: Silver tier requirements - FMEA passed + 10-100 downloads OR 30-90 days old
#[test]
fn test_silver_tier_requirements() {
    let calc = QualityTierCalculator::default();

    // Silver: FMEA passed + 10-100 downloads OR 30-90 days
    assert_eq!(calc.calculate(50, true, 45), QualityTier::Silver);
    assert_eq!(calc.calculate(10, true, 90), QualityTier::Silver);
    assert_eq!(calc.calculate(20, true, 60), QualityTier::Silver);
    assert_eq!(calc.calculate(99, true, 31), QualityTier::Silver);
}

/// T030: Bronze tier - basic validation
#[test]
fn test_bronze_tier_requirements() {
    let calc = QualityTierCalculator::default();

    // Bronze: Any downloads but doesn't meet Silver/Gold
    assert_eq!(calc.calculate(5, true, 100), QualityTier::Bronze);
    assert_eq!(calc.calculate(1, false, 365), QualityTier::Bronze);
    assert_eq!(calc.calculate(50, false, 45), QualityTier::Bronze); // No FMEA
}

/// T030: Unrated tier - no downloads
#[test]
fn test_unrated_tier() {
    let calc = QualityTierCalculator::default();

    // Unrated: No downloads
    assert_eq!(calc.calculate(0, true, 10), QualityTier::Unrated);
    assert_eq!(calc.calculate(0, false, 0), QualityTier::Unrated);
}

/// T031: Quality tier ordering for sorting
#[test]
fn test_quality_tier_ordering() {
    // Gold < Silver < Bronze < Unrated (for sorting)
    assert!(QualityTier::Gold < QualityTier::Silver);
    assert!(QualityTier::Silver < QualityTier::Bronze);
    assert!(QualityTier::Bronze < QualityTier::Unrated);
}

/// T031: Quality tier priority values
#[test]
fn test_quality_tier_priority() {
    assert_eq!(QualityTier::Gold.priority(), 0);
    assert_eq!(QualityTier::Silver.priority(), 1);
    assert_eq!(QualityTier::Bronze.priority(), 2);
    assert_eq!(QualityTier::Unrated.priority(), 3);
}

/// T031: is_at_least comparison
#[test]
fn test_is_at_least() {
    assert!(QualityTier::Gold.is_at_least(QualityTier::Gold));
    assert!(QualityTier::Gold.is_at_least(QualityTier::Silver));
    assert!(QualityTier::Gold.is_at_least(QualityTier::Bronze));
    assert!(QualityTier::Gold.is_at_least(QualityTier::Unrated));

    assert!(!QualityTier::Silver.is_at_least(QualityTier::Gold));
    assert!(QualityTier::Silver.is_at_least(QualityTier::Silver));

    assert!(!QualityTier::Bronze.is_at_least(QualityTier::Gold));
    assert!(!QualityTier::Bronze.is_at_least(QualityTier::Silver));
}

/// T031: Quality tier display symbols
#[test]
fn test_tier_symbols() {
    assert_eq!(QualityTier::Gold.symbol(), "🥇");
    assert_eq!(QualityTier::Silver.symbol(), "🥈");
    assert_eq!(QualityTier::Bronze.symbol(), "🥉");
    assert_eq!(QualityTier::Unrated.symbol(), "○");
}

/// T031: String parsing (case-insensitive)
#[test]
fn test_from_str_loose() {
    assert_eq!(QualityTier::from_str_loose("gold"), QualityTier::Gold);
    assert_eq!(QualityTier::from_str_loose("GOLD"), QualityTier::Gold);
    assert_eq!(QualityTier::from_str_loose("Silver"), QualityTier::Silver);
    assert_eq!(QualityTier::from_str_loose("BRONZE"), QualityTier::Bronze);
    assert_eq!(QualityTier::from_str_loose("unknown"), QualityTier::Unrated);
    assert_eq!(QualityTier::from_str_loose(""), QualityTier::Unrated);
}

/// T031: FMEA requirement for Gold/Silver
#[test]
fn test_fmea_requirement() {
    let calc = QualityTierCalculator::default();

    // Without FMEA, can't get Gold or Silver (with default thresholds)
    assert_eq!(calc.calculate(200, false, 10), QualityTier::Bronze);
    assert_eq!(calc.calculate(50, false, 45), QualityTier::Bronze);

    // With FMEA, can get Gold/Silver
    assert_eq!(calc.calculate(200, true, 10), QualityTier::Gold);
    assert_eq!(calc.calculate(50, true, 45), QualityTier::Silver);
}

/// T031: Timestamp-based calculation
#[test]
fn test_calculate_with_timestamp() {
    let calc = QualityTierCalculator::default();

    // Recent timestamp (today) should qualify for Gold with high downloads
    let recent = chrono::Utc::now().to_rfc3339();
    assert_eq!(
        calc.calculate_with_timestamp(150, true, Some(&recent)),
        QualityTier::Gold
    );

    // Old timestamp (2020) should be Bronze at best
    assert_eq!(
        calc.calculate_with_timestamp(150, true, Some("2020-01-01T00:00:00Z")),
        QualityTier::Bronze
    );

    // No timestamp defaults to very old = Bronze
    assert_eq!(
        calc.calculate_with_timestamp(150, true, None),
        QualityTier::Bronze
    );
}

/// T031: Custom thresholds
#[test]
fn test_custom_thresholds() {
    let thresholds = QualityThresholds {
        gold_min_downloads: 50, // Lower threshold
        gold_max_age_days: 60,  // More lenient
        silver_min_downloads: 5,
        silver_max_age_days: 180,
        gold_requires_fmea: false, // Don't require FMEA
        silver_requires_fmea: false,
    };
    let calc = QualityTierCalculator::with_thresholds(thresholds);

    // With relaxed thresholds, this should be Gold
    assert_eq!(calc.calculate(50, false, 60), QualityTier::Gold);

    // Low downloads can still get Silver
    assert_eq!(calc.calculate(5, false, 100), QualityTier::Silver);
}

/// T031: PackageQualityInfo creation
#[test]
fn test_package_quality_info() {
    let info = PackageQualityInfo::new("test-package".to_string(), 150, true, 10);

    assert_eq!(info.name, "test-package");
    assert_eq!(info.tier, QualityTier::Gold);
    assert_eq!(info.downloads, 150);
    assert!(info.fmea_passed);
    assert_eq!(info.age_days, 10);
    assert!(info.quality_score.is_some());
    assert!(info.quality_score.unwrap() > 50.0);
}

/// T031: Quality score computation
#[test]
fn test_quality_score_ranges() {
    // High quality package (many downloads, FMEA, fresh)
    let high = PackageQualityInfo::new("high".to_string(), 1000, true, 5);
    let high_score = high.quality_score.unwrap();
    assert!(high_score > 80.0, "High quality should score >80, got {}", high_score);

    // Medium quality package
    let medium = PackageQualityInfo::new("medium".to_string(), 50, true, 45);
    let medium_score = medium.quality_score.unwrap();
    assert!(
        medium_score > 40.0 && medium_score < 80.0,
        "Medium quality should score 40-80, got {}",
        medium_score
    );

    // Low quality package (few downloads, no FMEA, old)
    let low = PackageQualityInfo::new("low".to_string(), 1, false, 400);
    let low_score = low.quality_score.unwrap();
    assert!(low_score < 30.0, "Low quality should score <30, got {}", low_score);
}

/// T031: Serialization/deserialization
#[test]
fn test_quality_tier_serialization() {
    let tier = QualityTier::Gold;
    let json = serde_json::to_string(&tier).unwrap();
    assert_eq!(json, r#""gold""#);

    let deserialized: QualityTier = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized, QualityTier::Gold);

    // Test all tiers
    for tier in [
        QualityTier::Gold,
        QualityTier::Silver,
        QualityTier::Bronze,
        QualityTier::Unrated,
    ] {
        let json = serde_json::to_string(&tier).unwrap();
        let back: QualityTier = serde_json::from_str(&json).unwrap();
        assert_eq!(tier, back);
    }
}

/// T031: Display trait
#[test]
fn test_display_trait() {
    assert_eq!(format!("{}", QualityTier::Gold), "Gold");
    assert_eq!(format!("{}", QualityTier::Silver), "Silver");
    assert_eq!(format!("{}", QualityTier::Bronze), "Bronze");
    assert_eq!(format!("{}", QualityTier::Unrated), "Unrated");
}

/// T031: Default trait
#[test]
fn test_default_trait() {
    let tier: QualityTier = Default::default();
    assert_eq!(tier, QualityTier::Unrated);
}

/// T031: Edge cases for tier boundaries
#[test]
fn test_tier_boundary_conditions() {
    let calc = QualityTierCalculator::default();

    // Exact Gold boundary
    assert_eq!(calc.calculate(100, true, 30), QualityTier::Gold);
    assert_eq!(calc.calculate(99, true, 30), QualityTier::Silver); // Just under
    assert_eq!(calc.calculate(100, true, 31), QualityTier::Silver); // Just over

    // Exact Silver boundary
    assert_eq!(calc.calculate(10, true, 90), QualityTier::Silver);
    assert_eq!(calc.calculate(9, true, 90), QualityTier::Bronze); // Just under
    assert_eq!(calc.calculate(10, true, 91), QualityTier::Bronze); // Just over

    // Bronze boundary
    assert_eq!(calc.calculate(1, true, 365), QualityTier::Bronze);
    assert_eq!(calc.calculate(0, true, 365), QualityTier::Unrated);
}
