//! Bottleneck Detection and Analysis
//!
//! Identifies constraints limiting overall value stream throughput and provides
//! recommendations for improvement.

use serde::{Deserialize, Serialize};

use super::stage::StageType;

/// Severity level of a bottleneck
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BottleneckSeverity {
    /// Critical - immediate action required
    Critical,
    /// High - significant impact on throughput
    High,
    /// Medium - moderate impact
    Medium,
    /// Low - minor impact
    Low,
}

impl BottleneckSeverity {
    /// Get severity score (0-4)
    pub fn score(&self) -> u8 {
        match self {
            BottleneckSeverity::Critical => 4,
            BottleneckSeverity::High => 3,
            BottleneckSeverity::Medium => 2,
            BottleneckSeverity::Low => 1,
        }
    }

    /// Get color for visualization
    pub fn color(&self) -> &'static str {
        match self {
            BottleneckSeverity::Critical => "#c0392b", // Dark red
            BottleneckSeverity::High => "#e74c3c",     // Red
            BottleneckSeverity::Medium => "#f39c12",   // Orange
            BottleneckSeverity::Low => "#f1c40f",      // Yellow
        }
    }
}

impl std::fmt::Display for BottleneckSeverity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BottleneckSeverity::Critical => write!(f, "Critical"),
            BottleneckSeverity::High => write!(f, "High"),
            BottleneckSeverity::Medium => write!(f, "Medium"),
            BottleneckSeverity::Low => write!(f, "Low"),
        }
    }
}

/// Type of bottleneck constraint
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BottleneckType {
    /// Limited processing capacity
    Capacity,
    /// Quality issues causing rework
    Quality,
    /// Slow approval process
    Approval,
    /// Waiting on dependencies
    Dependency,
    /// Resource constraints
    Resource,
    /// Process inefficiency
    ProcessInefficiency,
}

impl BottleneckType {
    /// Get human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            BottleneckType::Capacity => "Capacity Constraint",
            BottleneckType::Quality => "Quality Issue",
            BottleneckType::Approval => "Approval Bottleneck",
            BottleneckType::Dependency => "Dependency Wait",
            BottleneckType::Resource => "Resource Constraint",
            BottleneckType::ProcessInefficiency => "Process Inefficiency",
        }
    }

    /// Get RDF URI suffix
    pub fn rdf_uri(&self) -> &'static str {
        match self {
            BottleneckType::Capacity => "Capacity",
            BottleneckType::Quality => "Quality",
            BottleneckType::Approval => "Approval",
            BottleneckType::Dependency => "Dependency",
            BottleneckType::Resource => "Resource",
            BottleneckType::ProcessInefficiency => "ProcessInefficiency",
        }
    }
}

impl std::fmt::Display for BottleneckType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// A detected bottleneck in the value stream
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bottleneck {
    /// Unique identifier
    pub id: String,
    /// Stage where bottleneck occurs
    pub stage: StageType,
    /// Type of bottleneck
    pub bottleneck_type: BottleneckType,
    /// Severity level
    pub severity: BottleneckSeverity,
    /// Impact on total lead time (ms)
    pub impact_ms: f64,
    /// Current performance metric value
    pub current_value: f64,
    /// Expected or target value
    pub target_value: f64,
    /// Percentage of total value stream delay caused by this bottleneck
    pub delay_percentage: f64,
    /// Description of the issue
    pub description: String,
    /// Recommended improvement actions
    pub recommendations: Vec<String>,
    /// Estimated improvement potential (ms saved if addressed)
    pub improvement_potential_ms: f64,
}

impl Bottleneck {
    /// Create a new bottleneck
    pub fn new(
        stage: StageType,
        bottleneck_type: BottleneckType,
        severity: BottleneckSeverity,
        impact_ms: f64,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            stage,
            bottleneck_type,
            severity,
            impact_ms,
            current_value: 0.0,
            target_value: 0.0,
            delay_percentage: 0.0,
            description: String::new(),
            recommendations: Vec::new(),
            improvement_potential_ms: 0.0,
        }
    }

    /// Set description
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }

    /// Add recommendation
    pub fn add_recommendation(&mut self, recommendation: impl Into<String>) {
        self.recommendations.push(recommendation.into());
    }

    /// Set improvement potential
    pub fn with_improvement_potential(mut self, potential_ms: f64) -> Self {
        self.improvement_potential_ms = potential_ms;
        self
    }

    /// Set performance values
    pub fn with_values(mut self, current: f64, target: f64) -> Self {
        self.current_value = current;
        self.target_value = target;
        self
    }

    /// Calculate gap between current and target
    pub fn performance_gap(&self) -> f64 {
        if self.target_value > 0.0 {
            (self.current_value - self.target_value).abs() / self.target_value
        } else {
            0.0
        }
    }

    /// Get priority score for addressing this bottleneck (higher is more urgent)
    pub fn priority_score(&self) -> f64 {
        let severity_weight = 0.4;
        let impact_weight = 0.4;
        let potential_weight = 0.2;

        let severity_score = self.severity.score() as f64 / 4.0;
        let impact_score = (self.delay_percentage / 100.0).min(1.0);
        let potential_score = if self.improvement_potential_ms > 0.0 {
            (self.improvement_potential_ms / 10000.0).min(1.0)
        } else {
            0.0
        };

        severity_weight * severity_score
            + impact_weight * impact_score
            + potential_weight * potential_score
    }

    /// Get formatted summary
    pub fn summary(&self) -> String {
        format!(
            "{} in {} stage: {} severity, {:.2}ms impact ({:.1}% of total delay)",
            self.bottleneck_type,
            self.stage,
            self.severity,
            self.impact_ms,
            self.delay_percentage
        )
    }
}

/// Collection of bottlenecks with analysis capabilities
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BottleneckAnalysis {
    /// All detected bottlenecks
    pub bottlenecks: Vec<Bottleneck>,
    /// Total delay caused by bottlenecks (ms)
    pub total_delay_ms: f64,
    /// Total improvement potential (ms)
    pub total_improvement_potential_ms: f64,
}

impl BottleneckAnalysis {
    /// Create new bottleneck analysis
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a bottleneck
    pub fn add(&mut self, bottleneck: Bottleneck) {
        self.total_delay_ms += bottleneck.impact_ms;
        self.total_improvement_potential_ms += bottleneck.improvement_potential_ms;
        self.bottlenecks.push(bottleneck);
    }

    /// Get bottlenecks by severity
    pub fn by_severity(&self, severity: BottleneckSeverity) -> Vec<&Bottleneck> {
        self.bottlenecks
            .iter()
            .filter(|b| b.severity == severity)
            .collect()
    }

    /// Get critical bottlenecks
    pub fn critical(&self) -> Vec<&Bottleneck> {
        self.by_severity(BottleneckSeverity::Critical)
    }

    /// Get high severity bottlenecks
    pub fn high_severity(&self) -> Vec<&Bottleneck> {
        self.by_severity(BottleneckSeverity::High)
    }

    /// Get bottlenecks sorted by priority
    pub fn by_priority(&self) -> Vec<&Bottleneck> {
        let mut sorted: Vec<&Bottleneck> = self.bottlenecks.iter().collect();
        sorted.sort_by(|a, b| {
            b.priority_score()
                .partial_cmp(&a.priority_score())
                .unwrap()
        });
        sorted
    }

    /// Get top N bottlenecks by priority
    pub fn top_bottlenecks(&self, n: usize) -> Vec<&Bottleneck> {
        self.by_priority().into_iter().take(n).collect()
    }

    /// Get bottlenecks for a specific stage
    pub fn by_stage(&self, stage: StageType) -> Vec<&Bottleneck> {
        self.bottlenecks.iter().filter(|b| b.stage == stage).collect()
    }

    /// Calculate potential ROI if top N bottlenecks are addressed
    pub fn potential_roi(&self, n: usize) -> f64 {
        let top = self.top_bottlenecks(n);
        let potential: f64 = top.iter().map(|b| b.improvement_potential_ms).sum();

        if self.total_delay_ms > 0.0 {
            potential / self.total_delay_ms
        } else {
            0.0
        }
    }

    /// Get recommended actions (sorted by priority)
    pub fn recommended_actions(&self) -> Vec<String> {
        let mut actions = Vec::new();
        for bottleneck in self.by_priority() {
            actions.push(format!(
                "[{}] {}: {}",
                bottleneck.severity, bottleneck.stage, bottleneck.description
            ));
            for rec in &bottleneck.recommendations {
                actions.push(format!("  → {}", rec));
            }
        }
        actions
    }

    /// Get summary statistics
    pub fn summary(&self) -> String {
        format!(
            "{} bottlenecks detected ({} critical, {} high), total delay: {:.2}ms, improvement potential: {:.2}ms ({:.1}%)",
            self.bottlenecks.len(),
            self.critical().len(),
            self.high_severity().len(),
            self.total_delay_ms,
            self.total_improvement_potential_ms,
            if self.total_delay_ms > 0.0 {
                (self.total_improvement_potential_ms / self.total_delay_ms) * 100.0
            } else {
                0.0
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bottleneck_creation() {
        let mut bottleneck = Bottleneck::new(
            StageType::Validation,
            BottleneckType::Approval,
            BottleneckSeverity::High,
            5000.0,
        )
        .with_description("Slow governance approval process")
        .with_improvement_potential(4000.0);

        bottleneck.add_recommendation("Implement automated approval for low-risk changes");
        bottleneck.add_recommendation("Set up approval timeouts");

        assert_eq!(bottleneck.stage, StageType::Validation);
        assert_eq!(bottleneck.severity, BottleneckSeverity::High);
        assert_eq!(bottleneck.impact_ms, 5000.0);
        assert_eq!(bottleneck.recommendations.len(), 2);
    }

    #[test]
    fn test_bottleneck_priority() {
        let high_impact = Bottleneck::new(
            StageType::CodeGeneration,
            BottleneckType::Capacity,
            BottleneckSeverity::Critical,
            10000.0,
        )
        .with_improvement_potential(8000.0);

        let low_impact = Bottleneck::new(
            StageType::Testing,
            BottleneckType::Quality,
            BottleneckSeverity::Low,
            500.0,
        )
        .with_improvement_potential(300.0);

        assert!(high_impact.priority_score() > low_impact.priority_score());
    }

    #[test]
    fn test_bottleneck_analysis() {
        let mut analysis = BottleneckAnalysis::new();

        analysis.add(
            Bottleneck::new(
                StageType::Validation,
                BottleneckType::Approval,
                BottleneckSeverity::Critical,
                5000.0,
            )
            .with_improvement_potential(4000.0),
        );

        analysis.add(
            Bottleneck::new(
                StageType::Testing,
                BottleneckType::Quality,
                BottleneckSeverity::Medium,
                2000.0,
            )
            .with_improvement_potential(1500.0),
        );

        assert_eq!(analysis.bottlenecks.len(), 2);
        assert_eq!(analysis.total_delay_ms, 7000.0);
        assert_eq!(analysis.total_improvement_potential_ms, 5500.0);
        assert_eq!(analysis.critical().len(), 1);

        let top = analysis.top_bottlenecks(1);
        assert_eq!(top.len(), 1);
        assert_eq!(top[0].severity, BottleneckSeverity::Critical);

        let roi = analysis.potential_roi(2);
        assert!(roi > 0.7); // >70% improvement potential
    }
}
