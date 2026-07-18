use crate::{
    Assessment, DimensionAssessment, Dimension, Gap, ImprovementPlan, MaturityLevel, Recommendation,
    ProgressTracker,
};
use std::collections::HashMap;

/// Assessor agent evaluates system maturity
pub struct Assessor;

impl Assessor {
    /// Assess current system across all dimensions
    pub fn assess_system(metrics: &SystemMetrics) -> Assessment {
        let mut assessment = Assessment::new(format!("assessment-{}", uuid()));

        // Assess code quality
        let cq_level = Self::assess_code_quality(metrics);
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            cq_level,
            format!("Coverage: {}%, Tests: {}", metrics.test_coverage, metrics.test_count),
        ));

        // Assess performance
        let perf_level = Self::assess_performance(metrics);
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::Performance,
            perf_level,
            format!("Build time: {}s, Memory: {}MB", metrics.build_time_seconds, metrics.memory_usage_mb),
        ));

        // Assess reliability
        let rel_level = Self::assess_reliability(metrics);
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::Reliability,
            rel_level,
            format!("Uptime: {}%, MTTR: {}min", metrics.uptime_percentage, metrics.mttr_minutes),
        ));

        // Assess operations
        let ops_level = Self::assess_operations(metrics);
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::Operations,
            ops_level,
            format!("Alerts: {}, Logs: {} levels", metrics.alert_count, metrics.log_levels),
        ));

        // Assess security
        let sec_level = Self::assess_security(metrics);
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::Security,
            sec_level,
            format!("Vulnerabilities: {}, Audits: {}", metrics.vulnerability_count, metrics.audit_count),
        ));

        assessment
    }

    fn assess_code_quality(metrics: &SystemMetrics) -> MaturityLevel {
        match metrics.test_coverage {
            0..=20 => MaturityLevel::Initial,
            21..=40 => MaturityLevel::Repeatable,
            41..=60 => MaturityLevel::Defined,
            61..=80 => MaturityLevel::Managed,
            81..=100 => MaturityLevel::Optimizing,
            _ => MaturityLevel::Optimizing, // >100 treated as Optimizing
        }
    }

    fn assess_performance(metrics: &SystemMetrics) -> MaturityLevel {
        match metrics.build_time_seconds {
            0..=5 => MaturityLevel::Optimizing,
            6..=15 => MaturityLevel::Managed,
            16..=30 => MaturityLevel::Defined,
            31..=60 => MaturityLevel::Repeatable,
            _ => MaturityLevel::Initial,
        }
    }

    fn assess_reliability(metrics: &SystemMetrics) -> MaturityLevel {
        match metrics.uptime_percentage as u32 {
            99..=100 => MaturityLevel::Optimizing,
            95..=98 => MaturityLevel::Managed,
            90..=94 => MaturityLevel::Defined,
            80..=89 => MaturityLevel::Repeatable,
            _ => MaturityLevel::Initial,
        }
    }

    fn assess_operations(metrics: &SystemMetrics) -> MaturityLevel {
        match metrics.alert_count {
            0..=5 => MaturityLevel::Optimizing,
            6..=15 => MaturityLevel::Managed,
            16..=30 => MaturityLevel::Defined,
            31..=60 => MaturityLevel::Repeatable,
            _ => MaturityLevel::Initial,
        }
    }

    fn assess_security(metrics: &SystemMetrics) -> MaturityLevel {
        match metrics.vulnerability_count {
            0 => MaturityLevel::Optimizing,
            1..=2 => MaturityLevel::Managed,
            3..=5 => MaturityLevel::Defined,
            6..=10 => MaturityLevel::Repeatable,
            _ => MaturityLevel::Initial,
        }
    }
}

/// Analyzer agent identifies improvement gaps
pub struct Analyzer;

impl Analyzer {
    /// Analyze assessment and identify gaps
    pub fn analyze(assessment: &Assessment, target_level: MaturityLevel) -> Vec<Gap> {
        assessment
            .dimensions
            .values()
            .map(|d| Gap::new(d.dimension, d.level, target_level))
            .collect()
    }

    /// Find critical gaps (largest improvement needed)
    pub fn find_critical_gaps(gaps: &[Gap]) -> Vec<Gap> {
        let mut sorted = gaps.to_vec();
        sorted.sort_by(|a, b| b.gap_size.partial_cmp(&a.gap_size).unwrap());
        sorted.into_iter().take(3).collect()
    }

    /// Analyze improvement trends
    pub fn analyze_trends(tracker: &ProgressTracker) -> HashMap<Dimension, f32> {
        let mut trends = HashMap::new();

        if tracker.assessments.len() < 2 {
            return trends;
        }

        let latest = tracker.assessments.last().unwrap();
        let previous = &tracker.assessments[tracker.assessments.len() - 2];

        for (&dim, latest_dim) in &latest.dimensions {
            if let Some(prev_dim) = previous.dimensions.get(&dim) {
                let improvement = latest_dim.score - prev_dim.score;
                trends.insert(dim, improvement);
            }
        }

        trends
    }
}

/// Recommender agent suggests improvements
pub struct Recommender;

impl Recommender {
    /// Generate improvement recommendations
    pub fn recommend(assessment: &Assessment, target_level: MaturityLevel) -> ImprovementPlan {
        let mut plan = ImprovementPlan::new(assessment.id.clone());
        let gaps = Analyzer::analyze(assessment, target_level);

        for gap in gaps {
            if gap.gap_size > 0.0 {
                let (action, effort, impact) = Self::suggest_action(&gap);
                let rec = Recommendation::new(gap.dimension, gap, action, effort, impact);
                plan.add_recommendation(rec);
            }
        }

        plan.sort_by_priority();
        plan
    }

    fn suggest_action(gap: &Gap) -> (String, f32, f32) {
        match (gap.dimension, gap.current, gap.target) {
            // Code Quality improvements
            (Dimension::CodeQuality, _, MaturityLevel::Managed) => (
                "Increase test coverage to 70%+".to_string(),
                0.6,
                3.0,
            ),
            (Dimension::CodeQuality, _, MaturityLevel::Optimizing) => (
                "Achieve 90%+ test coverage and implement mutation testing".to_string(),
                0.8,
                4.0,
            ),

            // Performance improvements
            (Dimension::Performance, _, MaturityLevel::Managed) => (
                "Implement performance benchmarks and SLOs".to_string(),
                0.5,
                2.5,
            ),
            (Dimension::Performance, _, MaturityLevel::Optimizing) => (
                "Optimize hot paths and achieve sub-5s builds".to_string(),
                0.9,
                4.5,
            ),

            // Reliability improvements
            (Dimension::Reliability, _, MaturityLevel::Managed) => (
                "Implement health checks and automated failover".to_string(),
                0.7,
                3.5,
            ),
            (Dimension::Reliability, _, MaturityLevel::Optimizing) => (
                "Achieve 99.99% uptime with distributed redundancy".to_string(),
                0.95,
                5.0,
            ),

            // Operations improvements
            (Dimension::Operations, _, MaturityLevel::Managed) => (
                "Implement comprehensive logging and alerting".to_string(),
                0.6,
                3.0,
            ),
            (Dimension::Operations, _, MaturityLevel::Optimizing) => (
                "Full observability with distributed tracing".to_string(),
                0.85,
                4.5,
            ),

            // Security improvements
            (Dimension::Security, _, MaturityLevel::Managed) => (
                "Implement security scanning in CI/CD".to_string(),
                0.5,
                3.5,
            ),
            (Dimension::Security, _, MaturityLevel::Optimizing) => (
                "Zero-vulnerability policy with continuous audits".to_string(),
                0.9,
                4.8,
            ),

            _ => ("General improvement".to_string(), 0.5, 2.0),
        }
    }
}

/// Tracker agent monitors progress
pub struct Tracker;

impl Tracker {
    /// Track progress and update learning state
    pub fn track_progress(tracker: &mut ProgressTracker, assessment: Assessment) -> ProgressReport {
        let previous_score = tracker.latest_assessment().map(|a| a.overall_score).unwrap_or(0.0);
        let improvement = assessment.overall_score - previous_score;

        tracker.add_assessment(assessment);

        ProgressReport {
            improvement,
            total_assessments: tracker.assessments.len(),
            trend: Analyzer::analyze_trends(tracker),
        }
    }

    /// Generate learning summary
    pub fn generate_summary(tracker: &ProgressTracker) -> LearningState {
        let total_assessments = tracker.assessments.len();
        let total_improvements_executed = tracker.improvement_history.len();
        let avg_improvement = if total_assessments > 1 {
            (tracker.assessments.last().unwrap().overall_score
                - tracker.assessments.first().unwrap().overall_score)
                / (total_assessments - 1) as f32
        } else {
            0.0
        };

        LearningState {
            total_assessments,
            total_improvements_executed,
            average_improvement_per_cycle: avg_improvement,
            best_performing_dimension: Self::find_best_dimension(tracker),
            worst_performing_dimension: Self::find_worst_dimension(tracker),
        }
    }

    fn find_best_dimension(tracker: &ProgressTracker) -> Option<Dimension> {
        tracker
            .latest_assessment()
            .and_then(|a| {
                a.dimensions
                    .iter()
                    .max_by(|(_, a), (_, b)| a.score.partial_cmp(&b.score).unwrap_or(std::cmp::Ordering::Equal))
                    .map(|(d, _)| *d)
            })
    }

    fn find_worst_dimension(tracker: &ProgressTracker) -> Option<Dimension> {
        tracker
            .latest_assessment()
            .and_then(|a| {
                a.dimensions
                    .iter()
                    .min_by(|(_, a), (_, b)| a.score.partial_cmp(&b.score).unwrap_or(std::cmp::Ordering::Equal))
                    .map(|(d, _)| *d)
            })
    }
}

/// System metrics for assessment
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SystemMetrics {
    pub test_coverage: u32,
    pub test_count: u32,
    pub build_time_seconds: u32,
    pub memory_usage_mb: u32,
    pub uptime_percentage: f32,
    pub mttr_minutes: u32,
    pub alert_count: u32,
    pub log_levels: u32,
    pub vulnerability_count: u32,
    pub audit_count: u32,
}

impl SystemMetrics {
    pub fn new() -> Self {
        Self {
            test_coverage: 0,
            test_count: 0,
            build_time_seconds: 0,
            memory_usage_mb: 0,
            uptime_percentage: 0.0,
            mttr_minutes: 0,
            alert_count: 0,
            log_levels: 0,
            vulnerability_count: 0,
            audit_count: 0,
        }
    }
}

impl Default for SystemMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// Progress report from tracking
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ProgressReport {
    pub improvement: f32,
    pub total_assessments: usize,
    pub trend: HashMap<Dimension, f32>,
}

/// Learning state summary
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LearningState {
    pub total_assessments: usize,
    pub total_improvements_executed: usize,
    pub average_improvement_per_cycle: f32,
    pub best_performing_dimension: Option<Dimension>,
    pub worst_performing_dimension: Option<Dimension>,
}

fn uuid() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos().to_string())
        .unwrap_or_else(|_| "unknown".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assessor_code_quality() {
        let mut metrics = SystemMetrics::new();
        metrics.test_coverage = 75;
        let assessment = Assessor::assess_system(&metrics);
        let cq = assessment.get_dimension(Dimension::CodeQuality).unwrap();
        assert_eq!(cq.level, MaturityLevel::Managed);
    }

    #[test]
    fn test_assessor_performance() {
        let mut metrics = SystemMetrics::new();
        metrics.build_time_seconds = 3;
        let assessment = Assessor::assess_system(&metrics);
        let perf = assessment.get_dimension(Dimension::Performance).unwrap();
        assert_eq!(perf.level, MaturityLevel::Optimizing);
    }

    #[test]
    fn test_analyzer_gaps() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "test".to_string(),
        ));

        let gaps = Analyzer::analyze(&assessment, MaturityLevel::Managed);
        assert_eq!(gaps.len(), 1);
        assert_eq!(gaps[0].gap_size, 3.0);
    }

    #[test]
    fn test_analyzer_critical_gaps() {
        let gaps = vec![
            Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Managed),
            Gap::new(Dimension::Performance, MaturityLevel::Repeatable, MaturityLevel::Managed),
            Gap::new(Dimension::Reliability, MaturityLevel::Initial, MaturityLevel::Managed),
        ];

        let critical = Analyzer::find_critical_gaps(&gaps);
        assert!(critical.len() <= 3);
    }

    #[test]
    fn test_recommender_generates_plan() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "test".to_string(),
        ));

        let plan = Recommender::recommend(&assessment, MaturityLevel::Managed);
        assert!(!plan.recommendations.is_empty());
        assert!(plan.recommendations[0].priority >= plan.recommendations.last().unwrap().priority);
    }

    #[test]
    fn test_tracker_progress_report() {
        let mut tracker = ProgressTracker::new();
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "test".to_string(),
        ));

        let report = Tracker::track_progress(&mut tracker, assessment);
        assert_eq!(report.total_assessments, 1);
    }

    #[test]
    fn test_tracker_generates_summary() {
        let mut tracker = ProgressTracker::new();
        let mut assessment = Assessment::new("test1".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "test".to_string(),
        ));
        tracker.add_assessment(assessment);

        let summary = Tracker::generate_summary(&tracker);
        assert_eq!(summary.total_assessments, 1);
    }

    #[test]
    fn test_system_metrics_default() {
        let metrics = SystemMetrics::default();
        assert_eq!(metrics.test_coverage, 0);
        assert_eq!(metrics.vulnerability_count, 0);
    }
}
