use crate::{Assessment, ImprovementPlan, ProgressTracker, agents::*};
use serde_json::{json, Value};

/// MCP tool responses
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ToolResponse {
    pub tool: String,
    pub success: bool,
    pub data: Value,
    pub error: Option<String>,
}

impl ToolResponse {
    pub fn success(tool: &str, data: Value) -> Self {
        Self {
            tool: tool.to_string(),
            success: true,
            data,
            error: None,
        }
    }

    pub fn error(tool: &str, error: String) -> Self {
        Self {
            tool: tool.to_string(),
            success: false,
            data: json!({}),
            error: Some(error),
        }
    }
}

/// Assessment MCP tool
pub struct AssessmentTool;

impl AssessmentTool {
    pub const NAME: &'static str = "/assess-system";
    pub const DESCRIPTION: &'static str =
        "Assess system maturity across all dimensions (code quality, performance, reliability, operations, security)";

    pub fn execute(metrics_json: &str) -> ToolResponse {
        match serde_json::from_str::<SystemMetrics>(metrics_json) {
            Ok(metrics) => {
                let assessment = Assessor::assess_system(&metrics);
                let dimensions: serde_json::Map<String, Value> = assessment
                    .dimensions
                    .iter()
                    .map(|(dim, dim_assess)| {
                        (
                            dim.as_str().to_string(),
                            json!({
                                "level": dim_assess.level.as_str(),
                                "score": dim_assess.score,
                                "evidence": dim_assess.evidence,
                            }),
                        )
                    })
                    .collect();

                ToolResponse::success(
                    Self::NAME,
                    json!({
                        "id": assessment.id,
                        "timestamp": assessment.timestamp,
                        "overall_score": assessment.overall_score,
                        "dimensions": dimensions,
                    }),
                )
            }
            Err(e) => ToolResponse::error(Self::NAME, format!("Invalid metrics: {}", e)),
        }
    }
}

/// Gap analysis MCP tool
pub struct GapAnalysisTool;

impl GapAnalysisTool {
    pub const NAME: &'static str = "/analyze-gaps";
    pub const DESCRIPTION: &'static str =
        "Analyze gaps between current maturity and target level, identify critical improvements";

    pub fn execute(assessment_json: &str, target_level: &str) -> ToolResponse {
        let target = parse_maturity_level(target_level);
        match serde_json::from_str::<Assessment>(assessment_json) {
            Ok(assessment) => {
                let gaps = Analyzer::analyze(&assessment, target);
                let critical = Analyzer::find_critical_gaps(&gaps);

                ToolResponse::success(
                    Self::NAME,
                    json!({
                        "assessment_id": assessment.id,
                        "target_level": target.as_str(),
                        "total_gaps": gaps.len(),
                        "critical_count": critical.len(),
                        "gaps": gaps
                            .iter()
                            .map(|g| json!({
                                "dimension": g.dimension.as_str(),
                                "current": g.current.as_str(),
                                "target": g.target.as_str(),
                                "gap_size": g.gap_size,
                                "is_critical": critical.iter().any(|c| c.dimension == g.dimension),
                            }))
                            .collect::<Vec<_>>(),
                    }),
                )
            }
            Err(e) => ToolResponse::error(Self::NAME, format!("Invalid assessment: {}", e)),
        }
    }
}

/// Recommendation MCP tool
pub struct RecommendationTool;

impl RecommendationTool {
    pub const NAME: &'static str = "/recommend-improvements";
    pub const DESCRIPTION: &'static str =
        "Generate prioritized improvement recommendations based on gap analysis";

    pub fn execute(assessment_json: &str, target_level: &str) -> ToolResponse {
        let target = parse_maturity_level(target_level);
        match serde_json::from_str::<Assessment>(assessment_json) {
            Ok(assessment) => {
                let plan = Recommender::recommend(&assessment, target);

                ToolResponse::success(
                    Self::NAME,
                    json!({
                        "assessment_id": plan.assessment_id,
                        "total_recommendations": plan.recommendations.len(),
                        "total_effort": plan.total_effort,
                        "estimated_gain": plan.estimated_gain,
                        "recommendations": plan
                            .recommendations
                            .iter()
                            .map(|r| json!({
                                "dimension": r.dimension.as_str(),
                                "action": r.action,
                                "effort": r.effort,
                                "impact": r.impact,
                                "priority": r.priority,
                                "gap": {
                                    "current": r.gap.current.as_str(),
                                    "target": r.gap.target.as_str(),
                                    "size": r.gap.gap_size,
                                }
                            }))
                            .collect::<Vec<_>>(),
                    }),
                )
            }
            Err(e) => ToolResponse::error(Self::NAME, format!("Invalid assessment: {}", e)),
        }
    }
}

/// Progress tracking MCP tool
pub struct ProgressTrackingTool;

impl ProgressTrackingTool {
    pub const NAME: &'static str = "/track-progress";
    pub const DESCRIPTION: &'static str =
        "Track maturity improvements over time and generate learning summaries";

    pub fn execute_update(tracker_json: &str, assessment_json: &str) -> ToolResponse {
        match (
            serde_json::from_str::<ProgressTracker>(tracker_json),
            serde_json::from_str::<Assessment>(assessment_json),
        ) {
            (Ok(mut tracker), Ok(assessment)) => {
                let report = Tracker::track_progress(&mut tracker, assessment);

                let trends: serde_json::Map<String, Value> = report
                    .trend
                    .iter()
                    .map(|(dim, score)| (dim.as_str().to_string(), json!(score)))
                    .collect();

                ToolResponse::success(
                    Self::NAME,
                    json!({
                        "action": "tracked",
                        "total_assessments": report.total_assessments,
                        "improvement": report.improvement,
                        "trends": trends,
                    }),
                )
            }
            _ => ToolResponse::error(
                Self::NAME,
                "Invalid tracker or assessment JSON".to_string(),
            ),
        }
    }

    pub fn execute_summary(tracker_json: &str) -> ToolResponse {
        match serde_json::from_str::<ProgressTracker>(tracker_json) {
            Ok(tracker) => {
                let summary = Tracker::generate_summary(&tracker);

                ToolResponse::success(
                    Self::NAME,
                    json!({
                        "action": "summary",
                        "total_assessments": summary.total_assessments,
                        "total_improvements_executed": summary.total_improvements_executed,
                        "average_improvement_per_cycle": summary.average_improvement_per_cycle,
                        "best_performing": summary.best_performing_dimension.map(|d| d.as_str()),
                        "worst_performing": summary.worst_performing_dimension.map(|d| d.as_str()),
                    }),
                )
            }
            Err(e) => ToolResponse::error(Self::NAME, format!("Invalid tracker: {}", e)),
        }
    }
}

fn parse_maturity_level(s: &str) -> crate::MaturityLevel {
    match s.to_lowercase().as_str() {
        "initial" => crate::MaturityLevel::Initial,
        "repeatable" => crate::MaturityLevel::Repeatable,
        "defined" => crate::MaturityLevel::Defined,
        "managed" => crate::MaturityLevel::Managed,
        "optimizing" => crate::MaturityLevel::Optimizing,
        _ => crate::MaturityLevel::Defined, // Default
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assessment_tool_response() {
        let metrics = r#"{
            "test_coverage": 75,
            "test_count": 250,
            "build_time_seconds": 12,
            "memory_usage_mb": 450,
            "uptime_percentage": 96.5,
            "mttr_minutes": 45,
            "alert_count": 12,
            "log_levels": 4,
            "vulnerability_count": 3,
            "audit_count": 2
        }"#;

        let response = AssessmentTool::execute(metrics);
        assert!(response.success);
        assert_eq!(response.tool, "/assess-system");
        assert!(response.data.get("overall_score").is_some());
    }

    #[test]
    fn test_assessment_tool_invalid_input() {
        let invalid = "not json";
        let response = AssessmentTool::execute(invalid);
        assert!(!response.success);
        assert!(response.error.is_some());
    }

    #[test]
    fn test_gap_analysis_tool() {
        let assessment = crate::Assessment::new("test".to_string());
        let assessment_json = serde_json::to_string(&assessment).unwrap();

        let response = GapAnalysisTool::execute(&assessment_json, "managed");
        assert!(response.success);
        assert_eq!(response.tool, "/analyze-gaps");
    }

    #[test]
    fn test_recommendation_tool() {
        let mut assessment = crate::Assessment::new("test".to_string());
        assessment.add_dimension(crate::DimensionAssessment::new(
            crate::Dimension::CodeQuality,
            crate::MaturityLevel::Initial,
            "test".to_string(),
        ));
        let assessment_json = serde_json::to_string(&assessment).unwrap();

        let response = RecommendationTool::execute(&assessment_json, "managed");
        assert!(response.success);
        assert_eq!(response.tool, "/recommend-improvements");
        assert!(response.data.get("recommendations").is_some());
    }

    #[test]
    fn test_progress_tracking_tool_summary() {
        let tracker = crate::ProgressTracker::new();
        let tracker_json = serde_json::to_string(&tracker).unwrap();

        let response = ProgressTrackingTool::execute_summary(&tracker_json);
        assert!(response.success);
        assert_eq!(response.tool, "/track-progress");
    }

    #[test]
    fn test_tool_response_success() {
        let resp = ToolResponse::success("test-tool", json!({"key": "value"}));
        assert!(resp.success);
        assert_eq!(resp.tool, "test-tool");
        assert!(resp.error.is_none());
    }

    #[test]
    fn test_tool_response_error() {
        let resp = ToolResponse::error("test-tool", "error message".to_string());
        assert!(!resp.success);
        assert_eq!(resp.error, Some("error message".to_string()));
    }

    #[test]
    fn test_parse_maturity_levels() {
        assert_eq!(parse_maturity_level("initial"), crate::MaturityLevel::Initial);
        assert_eq!(parse_maturity_level("managed"), crate::MaturityLevel::Managed);
        assert_eq!(parse_maturity_level("OPTIMIZING"), crate::MaturityLevel::Optimizing);
        assert_eq!(parse_maturity_level("invalid"), crate::MaturityLevel::Defined);
    }
}
