use maturity_matrix_showcase::*;
use agents::*;
use mcp_tools::*;

#[test]
fn test_assessment_tool_complete_system_metrics() {
    let metrics = r#"{
        "test_coverage": 85,
        "test_count": 500,
        "build_time_seconds": 8,
        "memory_usage_mb": 350,
        "uptime_percentage": 99.5,
        "mttr_minutes": 15,
        "alert_count": 3,
        "log_levels": 5,
        "vulnerability_count": 0,
        "audit_count": 12
    }"#;

    let response = AssessmentTool::execute(metrics);
    assert!(response.success);
    assert_eq!(response.tool, "/assess-system");

    let data = response.data;
    assert!(data.get("overall_score").is_some());
    assert!(data.get("dimensions").is_some());

    let score = data.get("overall_score").unwrap().as_f64().unwrap();
    assert!(score >= 4.0); // Should be high quality system
}

#[test]
fn test_gap_analysis_tool_comprehensive() {
    let mut assessment = Assessment::new("comprehensive".to_string());

    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Repeatable,
        "Basic testing".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Initial,
        "No optimization".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Security,
        MaturityLevel::Defined,
        "Security reviews in place".to_string(),
    ));

    let assessment_json = serde_json::to_string(&assessment).unwrap();
    let response = GapAnalysisTool::execute(&assessment_json, "optimizing");

    assert!(response.success);
    let data = response.data;
    assert!(data.get("total_gaps").is_some());
    assert!(data.get("critical_count").is_some());
    assert!(data.get("gaps").is_some());
}

#[test]
fn test_recommendation_tool_prioritization() {
    let mut assessment = Assessment::new("test".to_string());

    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "No testing".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Performance,
        MaturityLevel::Repeatable,
        "Slow builds".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Reliability,
        MaturityLevel::Managed,
        "Good uptime".to_string(),
    ));

    let assessment_json = serde_json::to_string(&assessment).unwrap();
    let response = RecommendationTool::execute(&assessment_json, "managed");

    assert!(response.success);
    let data = response.data;
    let recs = data.get("recommendations").unwrap().as_array().unwrap();

    // Verify recommendations are prioritized
    for i in 0..recs.len() - 1 {
        let priority1 = recs[i].get("priority").unwrap().as_f64().unwrap();
        let priority2 = recs[i + 1].get("priority").unwrap().as_f64().unwrap();
        assert!(priority1 >= priority2);
    }
}

#[test]
fn test_progress_tracking_tool_multi_cycle() {
    let mut tracker = ProgressTracker::new();

    // First assessment
    let mut a1 = Assessment::new("a1".to_string());
    a1.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "evidence1".to_string(),
    ));
    tracker.add_assessment(a1);

    let tracker_json = serde_json::to_string(&tracker).unwrap();

    // Second assessment
    let mut a2 = Assessment::new("a2".to_string());
    a2.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Repeatable,
        "evidence2".to_string(),
    ));
    let a2_json = serde_json::to_string(&a2).unwrap();

    let response = ProgressTrackingTool::execute_update(&tracker_json, &a2_json);
    assert!(response.success);

    let data = response.data;
    assert_eq!(data.get("action").unwrap().as_str().unwrap(), "tracked");
    assert_eq!(data.get("total_assessments").unwrap().as_u64().unwrap(), 2);
    assert!(data.get("improvement").unwrap().as_f64().unwrap() > 0.0);
}

#[test]
fn test_assessor_boundary_scores() {
    let metrics = SystemMetrics {
        test_coverage: 100,
        test_count: 1000,
        build_time_seconds: 2,
        memory_usage_mb: 100,
        uptime_percentage: 99.99,
        mttr_minutes: 5,
        alert_count: 0,
        log_levels: 5,
        vulnerability_count: 0,
        audit_count: 50,
    };

    let assessment = Assessor::assess_system(&metrics);

    // All dimensions should be Optimizing
    for (_, dim_assess) in &assessment.dimensions {
        assert_eq!(dim_assess.level, MaturityLevel::Optimizing);
    }

    assert_eq!(assessment.overall_score, 5.0);
}

#[test]
fn test_assessor_worst_case_scores() {
    let metrics = SystemMetrics {
        test_coverage: 0,
        test_count: 0,
        build_time_seconds: 300,
        memory_usage_mb: 2000,
        uptime_percentage: 50.0,
        mttr_minutes: 480,
        alert_count: 200,
        log_levels: 0,
        vulnerability_count: 100,
        audit_count: 0,
    };

    let assessment = Assessor::assess_system(&metrics);

    // All dimensions should be Initial
    for (_, dim_assess) in &assessment.dimensions {
        assert_eq!(dim_assess.level, MaturityLevel::Initial);
    }

    assert_eq!(assessment.overall_score, 1.0);
}

#[test]
fn test_analyzer_find_critical_gaps_ranking() {
    let gaps = vec![
        Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Optimizing),
        Gap::new(Dimension::Performance, MaturityLevel::Repeatable, MaturityLevel::Optimizing),
        Gap::new(Dimension::Security, MaturityLevel::Managed, MaturityLevel::Optimizing),
        Gap::new(Dimension::Reliability, MaturityLevel::Defined, MaturityLevel::Optimizing),
        Gap::new(Dimension::Operations, MaturityLevel::Initial, MaturityLevel::Optimizing),
    ];

    let critical = Analyzer::find_critical_gaps(&gaps);
    assert_eq!(critical.len(), 3);

    // Verify sorted by size (descending)
    for i in 0..critical.len() - 1 {
        assert!(critical[i].gap_size >= critical[i + 1].gap_size);
    }
}

#[test]
fn test_recommender_dimension_specific_actions() {
    let mut assessment = Assessment::new("test".to_string());

    assessment.add_dimension(DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Initial,
        "ev".to_string(),
    ));
    assessment.add_dimension(DimensionAssessment::new(
        Dimension::Security,
        MaturityLevel::Initial,
        "ev".to_string(),
    ));

    let plan = Recommender::recommend(&assessment, MaturityLevel::Managed);

    // Each dimension should have specific action
    let cq_rec = plan.recommendations.iter().find(|r| r.dimension == Dimension::CodeQuality);
    let sec_rec = plan.recommendations.iter().find(|r| r.dimension == Dimension::Security);

    assert!(cq_rec.is_some());
    assert!(sec_rec.is_some());

    let cq_action = &cq_rec.unwrap().action;
    let sec_action = &sec_rec.unwrap().action;

    assert_ne!(cq_action, sec_action); // Different dimensions = different actions
}

#[test]
fn test_tracker_improvement_trend_detection() {
    let mut tracker = ProgressTracker::new();

    // Simulate consistent improvement
    for i in 0..5 {
        let mut assessment = Assessment::new(format!("a{}", i));
        let level = match i {
            0 => MaturityLevel::Initial,
            1 => MaturityLevel::Repeatable,
            2 => MaturityLevel::Defined,
            3 => MaturityLevel::Managed,
            _ => MaturityLevel::Optimizing,
        };
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            level,
            format!("Step {}", i),
        ));
        Tracker::track_progress(&mut tracker, assessment);
    }

    let summary = Tracker::generate_summary(&tracker);
    assert!(summary.average_improvement_per_cycle > 0.0);
    assert_eq!(summary.best_performing_dimension, Some(Dimension::CodeQuality));
}

#[test]
fn test_metrics_serialization_roundtrip() {
    let original = SystemMetrics {
        test_coverage: 75,
        test_count: 300,
        build_time_seconds: 15,
        memory_usage_mb: 400,
        uptime_percentage: 97.5,
        mttr_minutes: 30,
        alert_count: 10,
        log_levels: 4,
        vulnerability_count: 2,
        audit_count: 5,
    };

    let json = serde_json::to_string(&original).unwrap();
    let deserialized: SystemMetrics = serde_json::from_str(&json).unwrap();

    assert_eq!(original.test_coverage, deserialized.test_coverage);
    assert_eq!(original.vulnerability_count, deserialized.vulnerability_count);
}

#[test]
fn test_improvement_plan_effort_calculation() {
    let mut plan = ImprovementPlan::new("test".to_string());

    for i in 0..5 {
        let gap = Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Managed);
        let effort = (i as f32 + 1.0) * 0.1;
        plan.add_recommendation(Recommendation::new(
            Dimension::CodeQuality,
            gap,
            format!("Action {}", i),
            effort,
            2.0,
        ));
    }

    assert_eq!(plan.total_effort, 1.5); // 0.1 + 0.2 + 0.3 + 0.4 + 0.5
    assert!(plan.estimated_gain > 0.0);
}

#[test]
fn test_assessment_id_uniqueness() {
    let a1 = Assessment::new("assessment-1".to_string());
    let a2 = Assessment::new("assessment-2".to_string());

    assert_ne!(a1.id, a2.id);
}

#[test]
fn test_dimension_assessment_timestamp() {
    let dim_assess = DimensionAssessment::new(
        Dimension::CodeQuality,
        MaturityLevel::Managed,
        "evidence".to_string(),
    );

    assert!(dim_assess.timestamp > 0);
}

#[test]
fn test_recommendation_effort_impact_ratio() {
    let gap = Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Managed);

    let low_effort = Recommendation::new(
        Dimension::CodeQuality,
        gap.clone(),
        "action".to_string(),
        0.2,
        3.0,
    );

    let high_effort = Recommendation::new(
        Dimension::CodeQuality,
        gap,
        "action".to_string(),
        0.8,
        3.0,
    );

    assert!(low_effort.priority > high_effort.priority);
}

#[test]
fn test_gap_analysis_tool_empty_assessment() {
    let assessment = Assessment::new("empty".to_string());
    let assessment_json = serde_json::to_string(&assessment).unwrap();

    let response = GapAnalysisTool::execute(&assessment_json, "managed");
    assert!(response.success);

    let data = response.data;
    assert_eq!(data.get("total_gaps").unwrap().as_u64().unwrap(), 0);
}

#[test]
fn test_multiple_assessment_cycles_tracking() {
    let mut tracker = ProgressTracker::new();

    for cycle in 0..10 {
        let mut assessment = Assessment::new(format!("cycle-{}", cycle));
        let level = match cycle {
            0..=2 => MaturityLevel::Initial,
            3..=5 => MaturityLevel::Repeatable,
            6..=8 => MaturityLevel::Defined,
            _ => MaturityLevel::Managed,
        };
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            level,
            "evidence".to_string(),
        ));

        Tracker::track_progress(&mut tracker, assessment);
    }

    assert_eq!(tracker.assessments.len(), 10);

    let summary = Tracker::generate_summary(&tracker);
    assert!(summary.average_improvement_per_cycle >= 0.0);
}
