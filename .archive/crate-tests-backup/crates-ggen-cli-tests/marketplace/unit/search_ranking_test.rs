//! Unit tests for marketplace search ranking algorithm
//!
//! Tests verify the multi-factor scoring system:
//! - Relevance (TF-IDF, 50%)
//! - Popularity (downloads, 20%)
//! - Quality (ratings, 20%)
//! - Recency (update time, 10%)

use chrono::{Duration, Utc};
use ggen_marketplace::search::scoring::CustomScorer;
use ggen_marketplace::types::Package;

fn create_test_package(id: &str, downloads: u64, rating: f32, days_old: i64) -> Package {
    Package {
        id: id.to_string(),
        name: format!("{}-package", id),
        description: "Test package".to_string(),
        version: "1.0.0".to_string(),
        category: "tools".to_string(),
        language: "rust".to_string(),
        license: "MIT".to_string(),
        tags: vec![],
        downloads,
        rating,
        created_at: Utc::now(),
        updated_at: Utc::now() - Duration::days(days_old),
        author: "test".to_string(),
        repository_url: None,
    }
}

#[test]
fn test_default_scorer_weights() {
    let scorer = CustomScorer::default();
    let package = create_test_package("test", 1000, 4.5, 30);

    let score = scorer.score(&package, 1.0);

    // Score should be in valid range
    assert!(
        score > 0.0 && score <= 1.0,
        "Score {} should be in [0,1]",
        score
    );
}

#[test]
fn test_custom_scorer_weights() {
    // Arrange: Custom weights favoring quality
    let scorer = CustomScorer::new(0.3, 0.1, 0.5, 0.1);
    let package = create_test_package("test", 1000, 5.0, 30);

    // Act
    let score = scorer.score(&package, 0.8);

    // Assert: Score should reflect high quality weight
    assert!(score > 0.0);
    assert!(score <= 1.0);
}

#[test]
fn test_popularity_scoring_logarithmic_scale() {
    let scorer = CustomScorer::default();

    // Arrange: Packages with different download counts
    let low_downloads = create_test_package("low", 100, 3.0, 30);
    let medium_downloads = create_test_package("medium", 10_000, 3.0, 30);
    let high_downloads = create_test_package("high", 1_000_000, 3.0, 30);

    // Act
    let low_score = scorer.score(&low_downloads, 0.5);
    let medium_score = scorer.score(&medium_downloads, 0.5);
    let high_score = scorer.score(&high_downloads, 0.5);

    // Assert: More downloads should increase score (logarithmically)
    assert!(
        medium_score > low_score,
        "Medium downloads should score higher than low"
    );
    assert!(
        high_score > medium_score,
        "High downloads should score higher than medium"
    );

    // Difference should be logarithmic (not linear)
    let low_to_medium_diff = medium_score - low_score;
    let medium_to_high_diff = high_score - medium_score;
    assert!(
        low_to_medium_diff > medium_to_high_diff,
        "Logarithmic scaling: 100x increase should have bigger impact than 100x at higher scale"
    );
}

#[test]
fn test_quality_scoring_normalized() {
    let scorer = CustomScorer::default();

    // Arrange: Packages with different ratings
    let low_rating = create_test_package("low", 1000, 1.0, 30);
    let medium_rating = create_test_package("medium", 1000, 3.0, 30);
    let high_rating = create_test_package("high", 1000, 5.0, 30);

    // Act
    let low_score = scorer.score(&low_rating, 0.5);
    let medium_score = scorer.score(&medium_rating, 0.5);
    let high_score = scorer.score(&high_rating, 0.5);

    // Assert: Higher ratings should increase score
    assert!(medium_score > low_score);
    assert!(high_score > medium_score);
}

#[test]
fn test_recency_scoring_time_decay() {
    let scorer = CustomScorer::default();

    // Arrange: Packages with different update times
    let recent = create_test_package("recent", 1000, 3.0, 1);
    let moderate = create_test_package("moderate", 1000, 3.0, 180); // 6 months
    let old = create_test_package("old", 1000, 3.0, 400); // Over 1 year

    // Act
    let recent_score = scorer.score(&recent, 0.5);
    let moderate_score = scorer.score(&moderate, 0.5);
    let old_score = scorer.score(&old, 0.5);

    // Assert: More recent updates should score higher
    assert!(
        recent_score > moderate_score,
        "Recent package should score higher"
    );
    assert!(
        moderate_score > old_score,
        "Moderate recency should score higher than old"
    );
}

#[test]
fn test_relevance_weight_dominates() {
    // Arrange: High relevance weight (default 50%)
    let scorer = CustomScorer::default();
    let package = create_test_package("test", 1000, 3.0, 30);

    // Act: Compare high vs low base relevance
    let high_relevance = scorer.score(&package, 1.0);
    let low_relevance = scorer.score(&package, 0.2);

    // Assert: Relevance should have significant impact
    let diff = high_relevance - low_relevance;
    assert!(
        diff > 0.3,
        "Relevance difference should be significant (>0.3), got {}",
        diff
    );
}

#[test]
fn test_zero_downloads_handling() {
    let scorer = CustomScorer::default();
    let package = create_test_package("test", 0, 4.0, 30);

    // Should not panic or produce invalid score
    let score = scorer.score(&package, 0.5);
    assert!(score >= 0.0 && score <= 1.0);
}

#[test]
fn test_perfect_package_score() {
    // Arrange: Perfect package (high relevance, downloads, rating, recent)
    let scorer = CustomScorer::default();
    let perfect = create_test_package("perfect", 1_000_000, 5.0, 1);

    // Act
    let score = scorer.score(&perfect, 1.0);

    // Assert: Should score very high
    assert!(
        score > 0.9,
        "Perfect package should score >0.9, got {}",
        score
    );
}

#[test]
fn test_poor_package_score() {
    // Arrange: Poor package (low relevance, downloads, rating, old)
    let scorer = CustomScorer::default();
    let poor = create_test_package("poor", 10, 1.0, 400);

    // Act
    let score = scorer.score(&poor, 0.1);

    // Assert: Should score low
    assert!(score < 0.3, "Poor package should score <0.3, got {}", score);
}

#[test]
fn test_score_consistency() {
    // Arrange: Same package scored multiple times
    let scorer = CustomScorer::default();
    let package = create_test_package("test", 1000, 4.0, 30);

    // Act
    let score1 = scorer.score(&package, 0.8);
    let score2 = scorer.score(&package, 0.8);
    let score3 = scorer.score(&package, 0.8);

    // Assert: Should produce consistent results
    assert_eq!(score1, score2);
    assert_eq!(score2, score3);
}

#[test]
fn test_weight_sum_validation() {
    // Weights should sum to approximately 1.0
    let scorer = CustomScorer::default();

    // With perfect inputs, max score should be close to 1.0
    let package = create_test_package("test", 1_000_000, 5.0, 1);
    let max_score = scorer.score(&package, 1.0);

    assert!(
        max_score <= 1.1,
        "Max score should not significantly exceed 1.0, got {}",
        max_score
    );
}

#[test]
fn test_extreme_downloads() {
    let scorer = CustomScorer::default();

    // Test very large download count doesn't cause overflow
    let mega_popular = create_test_package("mega", u64::MAX, 5.0, 1);
    let score = scorer.score(&mega_popular, 1.0);

    assert!(score.is_finite(), "Score should be finite");
    assert!(score >= 0.0 && score <= 2.0, "Score should be reasonable");
}

#[test]
fn test_negative_days_handling() {
    // Edge case: Package updated after current time (test robustness)
    let scorer = CustomScorer::default();
    let mut package = create_test_package("test", 1000, 4.0, 30);
    package.updated_at = Utc::now() + Duration::days(30);

    let score = scorer.score(&package, 0.5);
    assert!(score >= 0.0 && score <= 1.0);
}

#[test]
fn test_ranking_order() {
    // Arrange: Multiple packages with varying attributes
    let scorer = CustomScorer::default();
    let packages = vec![
        (
            "excellent",
            create_test_package("excellent", 100_000, 5.0, 10),
        ),
        ("good", create_test_package("good", 50_000, 4.0, 30)),
        ("average", create_test_package("average", 10_000, 3.0, 90)),
        ("poor", create_test_package("poor", 1000, 2.0, 200)),
    ];

    // Act: Score all with same base relevance
    let mut scores: Vec<_> = packages
        .iter()
        .map(|(name, pkg)| (*name, scorer.score(pkg, 0.5)))
        .collect();

    // Sort by score descending
    scores.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

    // Assert: Should rank in expected order
    assert_eq!(scores[0].0, "excellent");
    assert_eq!(scores[1].0, "good");
    assert_eq!(scores[2].0, "average");
    assert_eq!(scores[3].0, "poor");
}
