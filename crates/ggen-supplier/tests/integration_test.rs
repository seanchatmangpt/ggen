use ggen_supplier::{
    QualityMetrics, Supplier,
    history::{HistoryTracker, TrendDirection},
    limiter::{RateLimiter, RateLimitConfig},
    scorer::{QualityScorer, RiskLevel},
};
use chrono::Duration;

#[test]
fn test_end_to_end_high_quality_supplier() {
    // Arrange
    let supplier = Supplier::new("s1", "High Quality Corp");
    let metrics = QualityMetrics::new(2, 1, 1, 0, 100).unwrap();

    // Act - Score
    let assessment = QualityScorer::assess_supplier(&metrics).unwrap();

    // Assert
    assert_eq!(assessment.risk_level, RiskLevel::Low);
    assert!(assessment.score.is_high_quality);
    assert!(!assessment.score.should_rate_limit());

    // Act - Rate Limit Check
    let mut limiter = RateLimiter::with_default_config();
    let status = limiter.check_supplier(&supplier, &metrics).unwrap();

    // Assert
    assert!(!status.is_limited);
    assert_eq!(status.violations, 0);
}

#[test]
fn test_end_to_end_poor_quality_supplier() {
    // Arrange
    let supplier = Supplier::new("s2", "Poor Quality Inc");
    let bad_metrics = QualityMetrics::new(25, 10, 5, 5, 100).unwrap();

    // Act - Score
    let assessment = QualityScorer::assess_supplier(&bad_metrics).unwrap();

    // Assert
    assert_eq!(assessment.risk_level, RiskLevel::Critical);
    assert!(!assessment.score.is_high_quality);
    assert!(assessment.score.should_rate_limit());
    assert!(!assessment.recommendations.is_empty());

    // Act - Rate Limit Check (multiple violations)
    let mut limiter = RateLimiter::with_default_config();

    limiter.check_supplier(&supplier, &bad_metrics).unwrap();
    limiter.check_supplier(&supplier, &bad_metrics).unwrap();
    let status = limiter.check_supplier(&supplier, &bad_metrics).unwrap();

    // Assert
    assert!(status.is_limited);
    assert_eq!(status.violations, 3);
    assert!(status.limited_until.is_some());

    // Act - Enforce should fail
    let result = limiter.enforce(&supplier, &bad_metrics);
    assert!(result.is_err());
}

#[test]
fn test_end_to_end_history_tracking() {
    // Arrange
    let mut tracker = HistoryTracker::new();
    let supplier = Supplier::new("s3", "Improving Corp");

    // Act - Record improving trend
    for defects in (5..=15).rev() {
        let metrics = QualityMetrics::new(defects, 0, 0, 0, 100).unwrap();
        tracker.record(&supplier, metrics).unwrap();
    }

    // Assert
    let history = tracker.get_history("s3").unwrap();
    assert_eq!(history.entries.len(), 11);

    let trend = history.trend_analysis(4).unwrap();
    assert_eq!(trend.direction, TrendDirection::Improving);

    let avg = tracker.get_average_defect_rate("s3").unwrap();
    assert_eq!(avg, 10.0);
}

#[test]
fn test_end_to_end_supplier_comparison() {
    // Arrange
    let mut tracker = HistoryTracker::new();

    let s1 = Supplier::new("s1", "Good Supplier");
    let good_metrics = QualityMetrics::new(5, 2, 1, 0, 100).unwrap();
    tracker.record(&s1, good_metrics).unwrap();

    let s2 = Supplier::new("s2", "Average Supplier");
    let avg_metrics = QualityMetrics::new(10, 5, 3, 2, 100).unwrap();
    tracker.record(&s2, avg_metrics).unwrap();

    let s3 = Supplier::new("s3", "Poor Supplier");
    let poor_metrics = QualityMetrics::new(25, 10, 5, 5, 100).unwrap();
    tracker.record(&s3, poor_metrics).unwrap();

    // Act
    let comparisons = tracker.compare_suppliers(&["s1", "s2", "s3"]);

    // Assert
    assert_eq!(comparisons.len(), 3);
    assert!(comparisons[0].average_defect_rate < comparisons[1].average_defect_rate);
    assert!(comparisons[1].average_defect_rate < comparisons[2].average_defect_rate);
}

#[test]
fn test_end_to_end_custom_rate_limit_config() {
    // Arrange
    let config = RateLimitConfig {
        defect_threshold: 15.0,
        cooldown_duration: Duration::hours(48),
        max_violations: 2,
    };

    let mut limiter = RateLimiter::new(config);
    let supplier = Supplier::new("s4", "Moderate Supplier");
    let metrics = QualityMetrics::new(20, 5, 5, 0, 100).unwrap();

    // Act - First violation
    let status1 = limiter.check_supplier(&supplier, &metrics).unwrap();
    assert!(!status1.is_limited);
    assert_eq!(status1.violations, 1);

    // Act - Second violation triggers limit
    let status2 = limiter.check_supplier(&supplier, &metrics).unwrap();
    assert!(status2.is_limited);
    assert_eq!(status2.violations, 2);
}

#[test]
fn test_end_to_end_workflow() {
    // Arrange - Full workflow simulation
    let mut tracker = HistoryTracker::new();
    let mut limiter = RateLimiter::with_default_config();
    let supplier = Supplier::new("workflow", "Workflow Test Supplier");

    // Act - Week 1: Good performance
    let week1_metrics = QualityMetrics::new(5, 2, 1, 0, 100).unwrap();
    tracker.record(&supplier, week1_metrics.clone()).unwrap();
    let status1 = limiter.check_supplier(&supplier, &week1_metrics).unwrap();
    assert!(!status1.is_limited);

    // Act - Week 2: Quality degrades
    let week2_metrics = QualityMetrics::new(15, 8, 5, 2, 100).unwrap();
    tracker.record(&supplier, week2_metrics.clone()).unwrap();
    let status2 = limiter.check_supplier(&supplier, &week2_metrics).unwrap();
    assert!(!status2.is_limited);

    // Act - Week 3: Critical quality issues
    let week3_metrics = QualityMetrics::new(25, 10, 8, 5, 100).unwrap();
    tracker.record(&supplier, week3_metrics.clone()).unwrap();

    // Trigger violations
    limiter.check_supplier(&supplier, &week3_metrics).unwrap();
    limiter.check_supplier(&supplier, &week3_metrics).unwrap();
    let status3 = limiter.check_supplier(&supplier, &week3_metrics).unwrap();

    // Assert - Supplier is now rate limited
    assert!(status3.is_limited);

    // Assert - History shows worsening trend
    let history = tracker.get_history("workflow").unwrap();
    let trend = history.trend_analysis(2).unwrap();
    assert_eq!(trend.direction, TrendDirection::Worsening);

    // Assert - Latest assessment shows critical risk
    let assessment = QualityScorer::assess_supplier(&week3_metrics).unwrap();
    assert_eq!(assessment.risk_level, RiskLevel::Critical);
}
