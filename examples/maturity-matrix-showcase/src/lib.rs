use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

pub mod agents;
pub mod mcp_tools;
pub mod learning;
pub mod assessment_engine;
pub mod performance_metrics;

/// Maturity levels following CMM structure
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum MaturityLevel {
    Initial = 1,
    Repeatable = 2,
    Defined = 3,
    Managed = 4,
    Optimizing = 5,
}

impl MaturityLevel {
    pub fn score(&self) -> f32 {
        *self as u8 as f32
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Initial => "Initial",
            Self::Repeatable => "Repeatable",
            Self::Defined => "Defined",
            Self::Managed => "Managed",
            Self::Optimizing => "Optimizing",
        }
    }
}

impl fmt::Display for MaturityLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Assessment dimension (e.g., code quality, performance, reliability)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Dimension {
    CodeQuality,
    Performance,
    Reliability,
    Operations,
    Security,
}

impl Dimension {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::CodeQuality => "Code Quality",
            Self::Performance => "Performance",
            Self::Reliability => "Reliability",
            Self::Operations => "Operations",
            Self::Security => "Security",
        }
    }
}

impl fmt::Display for Dimension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Single assessment result for a dimension
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DimensionAssessment {
    pub dimension: Dimension,
    pub level: MaturityLevel,
    pub score: f32,
    pub evidence: String,
    pub timestamp: u64,
}

impl DimensionAssessment {
    pub fn new(
        dimension: Dimension,
        level: MaturityLevel,
        evidence: String,
    ) -> Self {
        Self {
            dimension,
            level,
            score: level.score(),
            evidence,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0),
        }
    }
}

/// Complete assessment across all dimensions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assessment {
    pub id: String,
    pub timestamp: u64,
    pub dimensions: HashMap<Dimension, DimensionAssessment>,
    pub overall_score: f32,
}

impl Assessment {
    pub fn new(id: String) -> Self {
        Self {
            id,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0),
            dimensions: HashMap::new(),
            overall_score: 0.0,
        }
    }

    pub fn add_dimension(&mut self, assessment: DimensionAssessment) {
        self.dimensions.insert(assessment.dimension, assessment);
        self.compute_overall_score();
    }

    fn compute_overall_score(&mut self) {
        if self.dimensions.is_empty() {
            self.overall_score = 0.0;
        } else {
            let sum: f32 = self.dimensions.values().map(|d| d.score).sum();
            self.overall_score = sum / self.dimensions.len() as f32;
        }
    }

    pub fn get_dimension(&self, dimension: Dimension) -> Option<&DimensionAssessment> {
        self.dimensions.get(&dimension)
    }
}

/// Gap between current and target maturity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gap {
    pub dimension: Dimension,
    pub current: MaturityLevel,
    pub target: MaturityLevel,
    pub gap_size: f32,
}

impl Gap {
    pub fn new(dimension: Dimension, current: MaturityLevel, target: MaturityLevel) -> Self {
        let gap_size = target.score() - current.score();
        Self {
            dimension,
            current,
            target,
            gap_size,
        }
    }
}

/// Improvement recommendation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Recommendation {
    pub dimension: Dimension,
    pub gap: Gap,
    pub action: String,
    pub effort: f32, // 0.0 to 1.0 (1.0 = maximum effort)
    pub impact: f32, // 0.0 to 1.0 (1.0 = maximum impact)
    pub priority: f32, // impact / effort
}

impl Recommendation {
    pub fn new(
        dimension: Dimension,
        gap: Gap,
        action: String,
        effort: f32,
        impact: f32,
    ) -> Self {
        let priority = if effort > 0.0 { impact / effort } else { 0.0 };
        Self {
            dimension,
            gap,
            action,
            effort,
            impact,
            priority,
        }
    }
}

/// Improvement plan generated from analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementPlan {
    pub assessment_id: String,
    pub recommendations: Vec<Recommendation>,
    pub total_effort: f32,
    pub estimated_gain: f32,
}

impl ImprovementPlan {
    pub fn new(assessment_id: String) -> Self {
        Self {
            assessment_id,
            recommendations: Vec::new(),
            total_effort: 0.0,
            estimated_gain: 0.0,
        }
    }

    pub fn add_recommendation(&mut self, rec: Recommendation) {
        self.estimated_gain += rec.gap.gap_size;
        self.total_effort += rec.effort;
        self.recommendations.push(rec);
    }

    pub fn sort_by_priority(&mut self) {
        self.recommendations.sort_by(|a, b| {
            b.priority.partial_cmp(&a.priority).unwrap_or(std::cmp::Ordering::Equal)
        });
    }
}

/// Progress tracker for learning over time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgressTracker {
    pub assessments: Vec<Assessment>,
    pub improvement_history: Vec<ImprovementPlan>,
}

impl ProgressTracker {
    pub fn new() -> Self {
        Self {
            assessments: Vec::new(),
            improvement_history: Vec::new(),
        }
    }

    pub fn add_assessment(&mut self, assessment: Assessment) {
        self.assessments.push(assessment);
    }

    pub fn add_improvement(&mut self, plan: ImprovementPlan) {
        self.improvement_history.push(plan);
    }

    pub fn latest_assessment(&self) -> Option<&Assessment> {
        self.assessments.last()
    }

    pub fn score_improvement(&self) -> f32 {
        if self.assessments.len() < 2 {
            return 0.0;
        }
        let latest = &self.assessments[self.assessments.len() - 1];
        let previous = &self.assessments[self.assessments.len() - 2];
        latest.overall_score - previous.overall_score
    }
}

impl Default for ProgressTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_maturity_level_score() {
        assert_eq!(MaturityLevel::Initial.score(), 1.0);
        assert_eq!(MaturityLevel::Repeatable.score(), 2.0);
        assert_eq!(MaturityLevel::Defined.score(), 3.0);
        assert_eq!(MaturityLevel::Managed.score(), 4.0);
        assert_eq!(MaturityLevel::Optimizing.score(), 5.0);
    }

    #[test]
    fn test_dimension_display() {
        assert_eq!(Dimension::CodeQuality.as_str(), "Code Quality");
        assert_eq!(Dimension::Performance.as_str(), "Performance");
        assert_eq!(Dimension::Reliability.as_str(), "Reliability");
        assert_eq!(Dimension::Operations.as_str(), "Operations");
        assert_eq!(Dimension::Security.as_str(), "Security");
    }

    #[test]
    fn test_dimension_assessment_creation() {
        let assessment = DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Managed,
            "90% test coverage".to_string(),
        );
        assert_eq!(assessment.dimension, Dimension::CodeQuality);
        assert_eq!(assessment.level, MaturityLevel::Managed);
        assert_eq!(assessment.score, 4.0);
        assert_eq!(assessment.evidence, "90% test coverage");
    }

    #[test]
    fn test_assessment_overall_score() {
        let mut assessment = Assessment::new("test-1".to_string());

        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Managed,
            "evidence".to_string(),
        ));
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::Performance,
            MaturityLevel::Defined,
            "evidence".to_string(),
        ));

        assert_eq!(assessment.overall_score, 3.5); // (4.0 + 3.0) / 2
    }

    #[test]
    fn test_gap_calculation() {
        let gap = Gap::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            MaturityLevel::Managed,
        );
        assert_eq!(gap.gap_size, 3.0);
    }

    #[test]
    fn test_recommendation_priority() {
        let gap = Gap::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            MaturityLevel::Managed,
        );
        let rec = Recommendation::new(
            Dimension::CodeQuality,
            gap,
            "Improve testing".to_string(),
            0.5,
            4.0,
        );
        assert_eq!(rec.priority, 8.0); // impact / effort = 4.0 / 0.5
    }

    #[test]
    fn test_improvement_plan_sorting() {
        let mut plan = ImprovementPlan::new("test-plan".to_string());

        let gap1 = Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Managed);
        plan.add_recommendation(Recommendation::new(
            Dimension::CodeQuality,
            gap1,
            "Add tests".to_string(),
            0.5,
            2.0,
        ));

        let gap2 = Gap::new(Dimension::Performance, MaturityLevel::Repeatable, MaturityLevel::Optimizing);
        plan.add_recommendation(Recommendation::new(
            Dimension::Performance,
            gap2,
            "Optimize".to_string(),
            1.0,
            1.0,
        ));

        plan.sort_by_priority();
        assert!(plan.recommendations[0].priority > plan.recommendations[1].priority);
    }

    #[test]
    fn test_progress_tracker() {
        let mut tracker = ProgressTracker::new();

        let mut assessment1 = Assessment::new("test-1".to_string());
        assessment1.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "evidence".to_string(),
        ));
        tracker.add_assessment(assessment1);

        let mut assessment2 = Assessment::new("test-2".to_string());
        assessment2.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Managed,
            "evidence".to_string(),
        ));
        tracker.add_assessment(assessment2);

        assert_eq!(tracker.assessments.len(), 2);
        assert_eq!(tracker.score_improvement(), 3.0);
    }

    #[test]
    fn test_maturity_level_ordering() {
        assert!(MaturityLevel::Initial < MaturityLevel::Managed);
        assert!(MaturityLevel::Defined < MaturityLevel::Optimizing);
    }

    #[test]
    fn test_assessment_get_dimension() {
        let mut assessment = Assessment::new("test-1".to_string());
        let dim_assessment = DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Managed,
            "evidence".to_string(),
        );
        assessment.add_dimension(dim_assessment);

        assert!(assessment.get_dimension(Dimension::CodeQuality).is_some());
        assert!(assessment.get_dimension(Dimension::Performance).is_none());
    }

    #[test]
    fn test_gap_zero_effort_priority() {
        let gap = Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Managed);
        let rec = Recommendation::new(
            Dimension::CodeQuality,
            gap,
            "action".to_string(),
            0.0,
            4.0,
        );
        assert_eq!(rec.priority, 0.0);
    }

    #[test]
    fn test_all_dimensions() {
        let dims = vec![
            Dimension::CodeQuality,
            Dimension::Performance,
            Dimension::Reliability,
            Dimension::Operations,
            Dimension::Security,
        ];
        assert_eq!(dims.len(), 5);

        for dim in dims {
            let assessment = DimensionAssessment::new(
                dim,
                MaturityLevel::Managed,
                "test".to_string(),
            );
            assert_eq!(assessment.dimension, dim);
        }
    }
}
