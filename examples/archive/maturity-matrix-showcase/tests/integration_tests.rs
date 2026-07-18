use maturity_matrix_showcase::*;
use agents::*;
use std::collections::HashMap;

#[test]
fn test_full_assessment_cycle() {
    // Setup: Create realistic metrics
    let mut metrics = SystemMetrics {
        test_coverage: 65,
        test_count: 250,
        build_time_seconds: 12,
        memory_usage_mb: 450,
        uptime_percentage: 96.5,
        mttr_minutes: 45,
        alert_count: 12,
        log_levels: 4,
        vulnerability_count: 3,
        audit_count: 2,
    };

    // Act: Assess system
    let assessment = Assessor::assess_system(&metrics);

    // Assert: Assessment is complete
    assert_eq!(assessment.dimensions.len(), 5);
    assert!(assessment.overall_score > 0.0);
    assert!(assessment.overall_score <= 5.0);
}

#[test]
fn test_gap_analysis_and_recommendations() {
    // Setup: Create assessment with multiple dimensions
    let mut assessment = Assessment::new("full-test".to_string());

    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "10% coverage".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Repeatable,
        "60s builds".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Reliability,
        MaturityLevel::Defined,
        "92% uptime".to_string(),
    ));

    // Act: Analyze and recommend
    let gaps = Analyzer::analyze(&assessment, MaturityLevel::Managed);
    let mut plan = Recommender::recommend(&assessment, MaturityLevel::Managed);

    // Assert: Recommendations are prioritized
    assert!(!plan.recommendations.is_empty());
    assert_eq!(gaps.len(), assessment.dimensions.len());

    // Verify sorting by priority
    for i in 0..plan.recommendations.len() - 1 {
        assert!(
            plan.recommendations[i].priority >= plan.recommendations[i + 1].priority,
            "Recommendations should be sorted by priority (descending)"
        );
    }
}

#[test]
fn test_self_improvement_cycle() {
    // Setup: Initialize tracking
    let mut tracker = ProgressTracker::new();

    // Cycle 1: Initial assessment
    let mut assessment1 = Assessment::new("cycle-1".to_string());
    assessment1.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "evidence".to_string(),
    ));
    assessment1.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Initial,
        "evidence".to_string(),
    ));

    let report1 = Tracker::track_progress(&mut tracker, assessment1.clone());

    // Act & Assert: First cycle
    assert_eq!(report1.total_assessments, 1);
    assert_eq!(report1.improvement, 1.0); // Initial: average of two InitialLevels = 1.0

    // Cycle 2: Improved assessment
    let mut assessment2 = Assessment::new("cycle-2".to_string());
    assessment2.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Defined,
        "evidence".to_string(),
    ));
    assessment2.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Managed,
        "evidence".to_string(),
    ));

    let report2 = Tracker::track_progress(&mut tracker, assessment2.clone());

    // Assert: Improvement is positive
    assert_eq!(report2.total_assessments, 2);
    assert!(report2.improvement > 0.0);

    // Verify improvement trend
    let summary = Tracker::generate_summary(&tracker);
    assert_eq!(summary.total_assessments, 2);
    assert!(summary.average_improvement_per_cycle >= 0.0);
}

#[test]
fn test_critical_dimension_identification() {
    // Setup: Create assessment with varied maturity levels
    let mut assessment = Assessment::new("varied-test".to_string());

    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Optimizing,
        "95% coverage".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Security,
        MaturityLevel::Initial,
        "0 audits".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Repeatable,
        "45s builds".to_string(),
    ));

    // Act: Find critical gaps
    let gaps = Analyzer::analyze(&assessment, MaturityLevel::Managed);
    let critical = Analyzer::find_critical_gaps(&gaps);

    // Assert: Security should be in critical gaps
    let has_security = critical.iter().any(|g| g.dimension == Dimension::Security);
    assert!(has_security);
}

#[test]
fn test_improvement_plan_effort_estimation() {
    // Setup: Create improvement plan
    let mut plan = ImprovementPlan::new("effort-test".to_string());

    let gap1 = Gap::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        MaturityLevel::Managed,
    );
    plan.add_recommendation(Recommendation::new(
        Dimension::CodeQuality,
        gap1,
        "Add tests".to_string(),
        0.4,
        3.0,
    ));

    let gap2 = Gap::new(
        Dimension::Performance,
        MaturityLevel::Repeatable,
        MaturityLevel::Managed,
    );
    plan.add_recommendation(Recommendation::new(
        Dimension::Performance,
        gap2,
        "Optimize".to_string(),
        0.6,
        2.0,
    ));

    // Assert: Effort aggregation
    assert_eq!(plan.total_effort, 1.0);
    assert!(plan.estimated_gain > 0.0);
}

#[test]
fn test_dimension_tracking_over_time() {
    // Setup: Multiple assessments tracking same dimension
    let mut tracker = ProgressTracker::new();

    // Assessment 1
    let mut a1 = Assessment::new("a1".to_string());
    a1.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "evidence".to_string(),
    ));
    tracker.add_assessment(a1);

    // Assessment 2
    let mut a2 = Assessment::new("a2".to_string());
    a2.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Repeatable,
        "evidence".to_string(),
    ));
    tracker.add_assessment(a2);

    // Assessment 3
    let mut a3 = Assessment::new("a3".to_string());
    a3.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Defined,
        "evidence".to_string(),
    ));
    tracker.add_assessment(a3);

    // Assert: Consistent improvement
    let trends = Analyzer::analyze_trends(&tracker);
    assert!(trends.get(&Dimension::CodeQuality).copied().unwrap_or(0.0) > 0.0);
}

#[test]
fn test_learning_state_generation() {
    // Setup: Tracker with multiple cycles
    let mut tracker = ProgressTracker::new();

    for i in 0..5 {
        let mut assessment = Assessment::new(format!("cycle-{}", i));
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            match i {
                0 => MaturityLevel::Initial,
                1 => MaturityLevel::Repeatable,
                2 => MaturityLevel::Defined,
                3 => MaturityLevel::Managed,
                _ => MaturityLevel::Optimizing,
            },
            "evidence".to_string(),
        ));
        tracker.add_assessment(assessment);
    }

    // Act: Generate summary
    let summary = Tracker::generate_summary(&tracker);

    // Assert: Learning state captures progression
    assert_eq!(summary.total_assessments, 5);
    assert!(summary.average_improvement_per_cycle > 0.0);
}

#[test]
fn test_multi_dimension_improvement_coordination() {
    // Setup: Assessment with multiple low dimensions
    let mut assessment = Assessment::new("multi-dim-test".to_string());

    for dim in &[
        Dimension::CodeQuality,
        Dimension::Performance,
        Dimension::Security,
    ] {
        assessment.add_dimension(DimensionAssessment::new(
            *dim,
            MaturityLevel::Initial,
            "needs improvement".to_string(),
        ));
    }

    // Act: Generate coordinated plan
    let mut plan = Recommender::recommend(&assessment, MaturityLevel::Optimizing);

    // Assert: All dimensions addressed
    assert_eq!(plan.recommendations.len(), 3);
    assert!(plan.estimated_gain >= 12.0); // 4 levels per dimension * 3 dimensions

    // Verify priority-based sorting
    plan.sort_by_priority();
    let first_priority = plan.recommendations[0].priority;
    for rec in &plan.recommendations[1..] {
        assert!(first_priority >= rec.priority);
    }
}

#[test]
fn test_metrics_to_assessment_conversion() {
    // Setup: Represent different system states
    let states = vec![
        ("poor", SystemMetrics {
            test_coverage: 10,
            build_time_seconds: 120,
            vulnerability_count: 15,
            uptime_percentage: 75.0,
            alert_count: 100,
            ..Default::default()
        }),
        ("average", SystemMetrics {
            test_coverage: 50,
            build_time_seconds: 20,
            vulnerability_count: 3,
            uptime_percentage: 95.0,
            alert_count: 20,
            ..Default::default()
        }),
        ("good", SystemMetrics {
            test_coverage: 85,
            build_time_seconds: 5,
            vulnerability_count: 0,
            uptime_percentage: 99.5,
            alert_count: 5,
            ..Default::default()
        }),
    ];

    // Act & Assert: Verify relative scores
    let mut scores = vec![];
    for (_, metrics) in states {
        let assessment = Assessor::assess_system(&metrics);
        scores.push(assessment.overall_score);
    }

    assert!(scores[0] < scores[1]);
    assert!(scores[1] < scores[2]);
}

#[test]
fn test_recommendation_action_specificity() {
    // Setup: Different gaps at different targets
    let gap1 = Gap::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        MaturityLevel::Managed,
    );
    let gap2 = Gap::new(
        Dimension::CodeQuality,
        MaturityLevel::Managed,
        MaturityLevel::Optimizing,
    );

    let rec1 = Recommendation::new(
        Dimension::CodeQuality,
        gap1,
        "Test action 1".to_string(),
        0.5,
        2.0,
    );
    let rec2 = Recommendation::new(
        Dimension::CodeQuality,
        gap2,
        "Test action 2".to_string(),
        0.8,
        3.0,
    );

    // Assert: Different recommendations for different gaps
    assert_ne!(rec1.action, rec2.action);
    assert!(rec1.effort < rec2.effort); // Moving from Managed to Optimizing requires more effort
}

#[test]
fn test_progress_tracker_multiple_improvement_cycles() {
    // Setup: Tracker with multiple plans
    let mut tracker = ProgressTracker::new();

    // Cycle 1: Initial assessment + plan
    let mut a1 = Assessment::new("a1".to_string());
    a1.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "ev".to_string(),
    ));
    tracker.add_assessment(a1);

    let mut plan1 = ImprovementPlan::new("plan1".to_string());
    let gap = Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Managed);
    plan1.add_recommendation(Recommendation::new(
        Dimension::CodeQuality,
        gap,
        "action".to_string(),
        0.5,
        2.0,
    ));
    tracker.add_improvement(plan1);

    // Cycle 2: New assessment after improvement
    let mut a2 = Assessment::new("a2".to_string());
    a2.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Managed,
        "ev".to_string(),
    ));
    tracker.add_assessment(a2);

    let mut plan2 = ImprovementPlan::new("plan2".to_string());
    let gap2 = Gap::new(Dimension::CodeQuality, MaturityLevel::Managed, MaturityLevel::Optimizing);
    plan2.add_recommendation(Recommendation::new(
        Dimension::CodeQuality,
        gap2,
        "action2".to_string(),
        0.8,
        3.0,
    ));
    tracker.add_improvement(plan2);

    // Assert: Full cycle tracking
    assert_eq!(tracker.assessments.len(), 2);
    assert_eq!(tracker.improvement_history.len(), 2);
}
