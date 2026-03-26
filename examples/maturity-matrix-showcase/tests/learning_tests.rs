use maturity_matrix_showcase::*;
use agents::*;
use learning::*;
use assessment_engine::*;
use performance_metrics::*;

#[test]
fn test_learning_agent_patterns_from_single_cycle() {
    let mut tracker = ProgressTracker::new();

    let mut a1 = Assessment::new("a1".to_string());
    a1.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "Low coverage".to_string(),
    ));
    a1.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Repeatable,
        "Slow builds".to_string(),
    ));
    tracker.add_assessment(a1);

    let gap1 = Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Managed);
    let mut plan1 = ImprovementPlan::new("plan1".to_string());
    plan1.add_recommendation(Recommendation::new(
        Dimension::CodeQuality,
        gap1,
        "Increase test coverage to 70%".to_string(),
        0.6,
        3.0,
    ));
    tracker.add_improvement(plan1);

    let mut a2 = Assessment::new("a2".to_string());
    a2.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Managed,
        "High coverage achieved".to_string(),
    ));
    a2.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Managed,
        "Optimized builds".to_string(),
    ));
    tracker.add_assessment(a2);

    // Add second improvement for learning
    let gap2 = Gap::new(Dimension::Performance, MaturityLevel::Managed, MaturityLevel::Optimizing);
    let mut plan2 = ImprovementPlan::new("plan2".to_string());
    plan2.add_recommendation(Recommendation::new(
        Dimension::Performance,
        gap2,
        "Further optimize".to_string(),
        0.5,
        2.5,
    ));
    tracker.add_improvement(plan2);

    let pattern = LearningAgent::learn_from_improvements(&tracker);
    assert!(pattern.is_some());
    let p = pattern.unwrap();
    assert!(!p.patterns.is_empty());
    assert!(p.best_improving_dimension().is_some());
}

#[test]
fn test_learning_agent_multi_cycle_learning() {
    let mut tracker = ProgressTracker::new();

    // Three improvement cycles
    for cycle in 0..3 {
        let mut a = Assessment::new(format!("assessment-{}", cycle));
        let level = match cycle {
            0 => MaturityLevel::Initial,
            1 => MaturityLevel::Repeatable,
            _ => MaturityLevel::Defined,
        };
        a.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            level,
            format!("Cycle {} evidence", cycle),
        ));
        tracker.add_assessment(a);

        let target = match cycle {
            0 => MaturityLevel::Repeatable,
            1 => MaturityLevel::Defined,
            _ => MaturityLevel::Managed,
        };
        let gap = Gap::new(Dimension::CodeQuality, level, target);
        let mut plan = ImprovementPlan::new(format!("plan-{}", cycle));
        plan.add_recommendation(Recommendation::new(
            Dimension::CodeQuality,
            gap,
            format!("Action {}", cycle),
            0.5,
            2.0,
        ));
        tracker.add_improvement(plan);
    }

    let pattern = LearningAgent::learn_from_improvements(&tracker);
    assert!(pattern.is_some());
    let p = pattern.unwrap();
    assert_eq!(p.patterns.len(), 1);
    assert!(p.patterns[0].total_cycles >= 2);
}

#[test]
fn test_predict_next_level_gradual_improvement() {
    let mut assessment = Assessment::new("test".to_string());
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Repeatable,
        "Getting better".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Repeatable,
        "Stable".to_string(),
    ));

    let predictions = LearningAgent::predict_next_level(&assessment, 0.5);
    assert!(predictions.contains_key(&Dimension::CodeQuality));
    assert!(predictions.contains_key(&Dimension::Performance));

    // Both should improve slightly
    let cq_pred = predictions[&Dimension::CodeQuality];
    assert!(cq_pred >= MaturityLevel::Repeatable);
}

#[test]
fn test_recommendation_score_perfect_accuracy() {
    let score = RecommendationScore {
        action: "Test".to_string(),
        dimension: Dimension::Performance,
        expected_impact: 2.0,
        actual_impact: 2.0,
        execution_count: 5,
    };

    assert_eq!(score.accuracy(), 1.0);
}

#[test]
fn test_recommendation_score_clamped_accuracy() {
    let score = RecommendationScore {
        action: "Test".to_string(),
        dimension: Dimension::Security,
        expected_impact: 1.0,
        actual_impact: 2.0,
        execution_count: 1,
    };

    assert_eq!(score.accuracy(), 1.0); // Clamped to 1.0
}

#[test]
fn test_strategy_adaptor_focus_areas() {
    let mut assessment = Assessment::new("test".to_string());
    for (i, dim) in [
        Dimension::CodeQuality,
        Dimension::Performance,
        Dimension::Reliability,
        Dimension::Operations,
        Dimension::Security,
    ]
    .iter()
    .enumerate()
    {
        let level = match i {
            0 => MaturityLevel::Optimizing,
            1 => MaturityLevel::Managed,
            2 => MaturityLevel::Defined,
            3 => MaturityLevel::Repeatable,
            _ => MaturityLevel::Initial,
        };
        assessment.add_dimension(DimensionAssessment::new(*dim, level, "ev".to_string()));
    }

    let tracker = ProgressTracker::new();
    let strategy = StrategyAdaptor::select_strategy(&assessment, &tracker);

    assert!(!strategy.recommended_focus.is_empty());
    // Should focus on lowest maturity dimensions first
    assert_eq!(strategy.recommended_focus[0], Dimension::Security);
}

#[test]
fn test_assessment_engine_weighted_empty_metrics() {
    let metrics = std::collections::HashMap::new();
    let weights = std::collections::HashMap::new();
    let result = AssessmentEngine::weighted_assessment(&metrics, &weights);
    assert_eq!(result, 0.0);
}

#[test]
fn test_anomaly_detection_no_previous() {
    let mut assessment = Assessment::new("test".to_string());
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Managed,
        "ev".to_string(),
    ));

    let anomalies = AssessmentEngine::detect_anomalies(&assessment, None);
    // Should detect missing dimensions
    assert!(!anomalies.is_empty());
}

#[test]
fn test_assessment_comparison_mixed_changes() {
    let mut a1 = Assessment::new("a1".to_string());
    a1.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "ev".to_string(),
    ));
    a1.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Managed,
        "ev".to_string(),
    ));

    let mut a2 = Assessment::new("a2".to_string());
    a2.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Defined,
        "ev".to_string(),
    ));
    a2.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Repeatable,
        "ev".to_string(),
    ));

    let comparison = AssessmentEngine::compare_assessments(&a1, &a2);
    assert_eq!(comparison.improved_dimensions, 1);
    assert_eq!(comparison.degraded_dimensions, 1);
}

#[test]
fn test_confidence_multiple_dimensions() {
    let mut assessment = Assessment::new("test".to_string());
    for (i, dim) in [
        Dimension::CodeQuality,
        Dimension::Performance,
        Dimension::Reliability,
    ]
    .iter()
    .enumerate()
    {
        let evidence = if i == 0 {
            "a".repeat(150) // Detailed
        } else if i == 1 {
            "medium evidence".to_string() // Adequate
        } else {
            "short".to_string() // Vague
        };
        assessment.add_dimension(DimensionAssessment::new(*dim, MaturityLevel::Managed, evidence));
    }

    let confidence = AssessmentEngine::assess_confidence(&assessment);
    assert!(confidence.overall_confidence > 0.0);
    assert!(confidence.overall_confidence < 1.0);
    assert_eq!(confidence.dimension_count, 3);
}

#[test]
fn test_performance_velocity_single_assessment() {
    let mut tracker = ProgressTracker::new();
    let mut a = Assessment::new("a1".to_string());
    a.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "ev".to_string(),
    ));
    tracker.add_assessment(a);

    let velocity = PerformanceAnalyzer::calculate_velocity(&tracker);
    assert_eq!(velocity, 0.0); // No velocity with single assessment
}

#[test]
fn test_estimate_time_with_current_at_target() {
    let mut assessment = Assessment::new("test".to_string());
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Optimizing,
        "ev".to_string(),
    ));

    let result = PerformanceAnalyzer::estimate_time_to_target(&assessment, MaturityLevel::Optimizing, 1.0);
    assert_eq!(result, 0.0);
}

#[test]
fn test_acceleration_in_consistent_improvement() {
    let mut tracker = ProgressTracker::new();

    for i in 0..5 {
        let mut a = Assessment::new(format!("a{}", i));
        let score = i as f32;
        let level = match i {
            0 => MaturityLevel::Initial,
            1 => MaturityLevel::Repeatable,
            2 => MaturityLevel::Defined,
            3 => MaturityLevel::Managed,
            _ => MaturityLevel::Optimizing,
        };
        a.add_dimension(DimensionAssessment::new(Dimension::CodeQuality, level, "ev".to_string()));
        tracker.add_assessment(a);
    }

    let accel = PerformanceAnalyzer::calculate_acceleration(&tracker);
    assert!(accel >= 0.0); // Consistent pace should show zero or positive
}

#[test]
fn test_capacity_metrics_headroom() {
    let mut assessment = Assessment::new("test".to_string());
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Managed,
        "ev".to_string(),
    ));

    let tracker = ProgressTracker::new();
    let capacity = PerformanceAnalyzer::calculate_capacity(&tracker, &assessment);

    assert_eq!(capacity.headroom, 1.0); // 5.0 - 4.0
    assert!(capacity.runway_cycles >= 0.0);
}

#[test]
fn test_bottleneck_identification() {
    let mut assessment = Assessment::new("test".to_string());
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Security,
        MaturityLevel::Initial,
        "No security measures".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Optimizing,
        "Excellent testing".to_string(),
    ));

    let bottlenecks = PerformanceAnalyzer::identify_bottlenecks(&assessment);
    assert!(!bottlenecks.is_empty());
    assert!(bottlenecks[0].dimension == Dimension::Security);
}

#[test]
fn test_slo_calculation_progression() {
    let mut assessment = Assessment::new("test".to_string());

    // Initial state
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "ev".to_string(),
    ));

    let slos = PerformanceAnalyzer::calculate_dimension_slos(&assessment);
    assert_eq!(slos[0].target_level, MaturityLevel::Repeatable);
    assert_eq!(slos[0].gap, 1.0);
}

#[test]
fn test_momentum_detection_insufficient_data() {
    let mut tracker = ProgressTracker::new();

    for i in 0..3 {
        let mut a = Assessment::new(format!("a{}", i));
        a.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));
        tracker.add_assessment(a);
    }

    let momentum = PerformanceAnalyzer::detect_momentum_change(&tracker);
    assert!(momentum.is_none()); // Less than 4 assessments
}

#[test]
fn test_learning_pattern_worst_dimension() {
    let pattern = LearningPattern {
        patterns: vec![
            DimensionPattern {
                dimension: Dimension::Performance,
                average_improvement: 1.5,
                total_cycles: 2,
            },
            DimensionPattern {
                dimension: Dimension::Security,
                average_improvement: 0.1,
                total_cycles: 2,
            },
        ],
    };

    assert_eq!(pattern.worst_improving_dimension(), Some(Dimension::Security));
}

#[test]
fn test_weighted_assessment_single_dimension() {
    let mut metrics = std::collections::HashMap::new();
    metrics.insert(Dimension::CodeQuality, 3.5);

    let mut weights = std::collections::HashMap::new();
    weights.insert(Dimension::CodeQuality, 2.0);

    let result = AssessmentEngine::weighted_assessment(&metrics, &weights);
    assert_eq!(result, 3.5);
}

#[test]
fn test_confidence_quality_evidence_detection() {
    let mut assessment = Assessment::new("test".to_string());

    // Add dimensions with various evidence qualities
    let evidence_samples = vec![
        ("a".repeat(200), EvidenceQuality::Detailed),
        ("medium".to_string(), EvidenceQuality::Adequate),
        ("x".repeat(200), EvidenceQuality::Detailed),
    ];

    for (i, (evidence, _)) in evidence_samples.iter().enumerate() {
        let dim = match i {
            0 => Dimension::CodeQuality,
            1 => Dimension::Performance,
            _ => Dimension::Reliability,
        };
        assessment.add_dimension(DimensionAssessment::new(dim, MaturityLevel::Managed, evidence.clone()));
    }

    let confidence = AssessmentEngine::assess_confidence(&assessment);
    assert_eq!(confidence.evidence_quality, EvidenceQuality::Detailed);
}
