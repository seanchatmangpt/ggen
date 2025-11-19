//! Future State Mapping with Moonshot 2028 Targets
//!
//! Defines aspirational future states and transformational improvement goals.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::stage::StageType;

/// A moonshot target for breakthrough improvement by 2028
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MoonshotTarget {
    /// Unique identifier
    pub id: String,
    /// Target name
    pub name: String,
    /// Metric being targeted
    pub metric_name: String,
    /// Current baseline value
    pub current_baseline: f64,
    /// Ambitious 2028 target value
    pub target_value: f64,
    /// Unit of measurement
    pub unit: String,
    /// Improvement multiplier (e.g., 10x)
    pub improvement_factor: f64,
    /// Strategic initiatives to achieve target
    pub initiatives: Vec<String>,
    /// Milestone dates and values
    pub milestones: Vec<Milestone>,
    /// Whether target is feasible with current technology
    pub is_feasible: bool,
    /// Risk assessment
    pub risk_level: RiskLevel,
    /// Description
    pub description: String,
}

impl MoonshotTarget {
    /// Create a new moonshot target
    pub fn new(
        name: impl Into<String>,
        metric_name: impl Into<String>,
        current_baseline: f64,
        target_value: f64,
        unit: impl Into<String>,
    ) -> Self {
        let current = current_baseline;
        let target = target_value;

        // Calculate improvement factor
        let improvement_factor = if current > 0.0 {
            if target < current {
                // For metrics where lower is better (e.g., lead time)
                current / target
            } else {
                // For metrics where higher is better (e.g., throughput)
                target / current
            }
        } else {
            1.0
        };

        Self {
            id: uuid::Uuid::new_v4().to_string(),
            name: name.into(),
            metric_name: metric_name.into(),
            current_baseline,
            target_value,
            unit: unit.into(),
            improvement_factor,
            initiatives: Vec::new(),
            milestones: Vec::new(),
            is_feasible: true,
            risk_level: RiskLevel::Medium,
            description: String::new(),
        }
    }

    /// Add initiative
    pub fn add_initiative(&mut self, initiative: impl Into<String>) {
        self.initiatives.push(initiative.into());
    }

    /// Add milestone
    pub fn add_milestone(&mut self, milestone: Milestone) {
        self.milestones.push(milestone);
    }

    /// Set description
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }

    /// Set risk level
    pub fn with_risk_level(mut self, risk_level: RiskLevel) -> Self {
        self.risk_level = risk_level;
        self
    }

    /// Set feasibility
    pub fn with_feasibility(mut self, is_feasible: bool) -> Self {
        self.is_feasible = is_feasible;
        self
    }

    /// Calculate gap to target
    pub fn gap_percentage(&self) -> f64 {
        if self.current_baseline > 0.0 {
            ((self.target_value - self.current_baseline).abs() / self.current_baseline) * 100.0
        } else {
            0.0
        }
    }

    /// Calculate progress towards target (0.0 to 1.0)
    pub fn progress(&self, current_value: f64) -> f64 {
        if self.current_baseline == self.target_value {
            return 1.0;
        }

        let total_improvement = (self.target_value - self.current_baseline).abs();
        let achieved_improvement = (current_value - self.current_baseline).abs();

        (achieved_improvement / total_improvement).clamp(0.0, 1.0)
    }

    /// Get next milestone
    pub fn next_milestone(&self) -> Option<&Milestone> {
        let now = Utc::now();
        self.milestones
            .iter()
            .find(|m| m.target_date > now && !m.achieved)
    }

    /// Get summary
    pub fn summary(&self) -> String {
        format!(
            "{}: {:.2}{} → {:.2}{} ({:.1}x improvement)",
            self.name,
            self.current_baseline,
            self.unit,
            self.target_value,
            self.unit,
            self.improvement_factor
        )
    }
}

/// A milestone on the path to moonshot target
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Milestone {
    /// Milestone name
    pub name: String,
    /// Target date
    pub target_date: DateTime<Utc>,
    /// Target value at this milestone
    pub target_value: f64,
    /// Whether achieved
    pub achieved: bool,
    /// When achieved
    pub achieved_date: Option<DateTime<Utc>>,
}

impl Milestone {
    /// Create a new milestone
    pub fn new(name: impl Into<String>, target_date: DateTime<Utc>, target_value: f64) -> Self {
        Self {
            name: name.into(),
            target_date,
            target_value,
            achieved: false,
            achieved_date: None,
        }
    }

    /// Mark milestone as achieved
    pub fn achieve(&mut self) {
        self.achieved = true;
        self.achieved_date = Some(Utc::now());
    }
}

/// Risk level for moonshot target
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RiskLevel {
    Low,
    Medium,
    High,
    VeryHigh,
}

impl std::fmt::Display for RiskLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RiskLevel::Low => write!(f, "Low"),
            RiskLevel::Medium => write!(f, "Medium"),
            RiskLevel::High => write!(f, "High"),
            RiskLevel::VeryHigh => write!(f, "Very High"),
        }
    }
}

/// Future state of the value stream with improvement targets
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FutureState {
    /// Unique identifier
    pub id: String,
    /// Future state name
    pub name: String,
    /// Target date for achieving this state
    pub target_date: DateTime<Utc>,
    /// Target lead time (ms)
    pub target_lead_time_ms: f64,
    /// Target process efficiency (0.0 to 1.0)
    pub target_process_efficiency: f64,
    /// Target throughput (items/hour)
    pub target_throughput: f64,
    /// Target first-pass yield (0.0 to 1.0)
    pub target_first_pass_yield: f64,
    /// Moonshot targets
    pub moonshot_targets: Vec<MoonshotTarget>,
    /// Strategic initiatives
    pub initiatives: Vec<Initiative>,
    /// Stage-specific improvements
    pub stage_improvements: HashMap<StageType, StageImprovement>,
    /// Description
    pub description: String,
}

impl FutureState {
    /// Create a new future state
    pub fn new(name: impl Into<String>, target_date: DateTime<Utc>) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            name: name.into(),
            target_date,
            target_lead_time_ms: 0.0,
            target_process_efficiency: 0.9, // 90% default target
            target_throughput: 0.0,
            target_first_pass_yield: 0.95, // 95% default target
            moonshot_targets: Vec::new(),
            initiatives: Vec::new(),
            stage_improvements: HashMap::new(),
            description: String::new(),
        }
    }

    /// Create moonshot 2028 future state
    pub fn moonshot_2028() -> Self {
        let target_date = DateTime::parse_from_rfc3339("2028-12-31T23:59:59Z")
            .unwrap()
            .with_timezone(&Utc);

        let mut state = Self::new("Moonshot 2028", target_date);
        state.description =
            "Transformational AI-accelerated semantic code generation".to_string();

        // 10x lead time reduction
        let mut lead_time_target = MoonshotTarget::new(
            "10x Lead Time Reduction",
            "End-to-end lead time",
            3600000.0, // 1 hour baseline
            360000.0,  // 6 minutes target
            "ms",
        )
        .with_description("Reduce total pipeline time from 1 hour to 6 minutes")
        .with_risk_level(RiskLevel::High);

        lead_time_target.add_initiative("Implement parallel stage execution");
        lead_time_target.add_initiative("AI-powered automated approvals");
        lead_time_target.add_initiative("Real-time SPARQL optimization");

        state.add_moonshot_target(lead_time_target);

        // 100% automation
        let mut automation_target = MoonshotTarget::new(
            "100% Automation Rate",
            "Manual touchpoint percentage",
            30.0, // 30% manual baseline
            0.0,  // 0% manual target
            "%",
        )
        .with_description("Eliminate all manual handoffs and approvals")
        .with_risk_level(RiskLevel::Medium);

        automation_target.add_initiative("Implement autonomous governance agents");
        automation_target.add_initiative("Self-healing pipeline recovery");
        automation_target.add_initiative("Predictive quality gates");

        state.add_moonshot_target(automation_target);

        // 99.9% first-pass yield
        let mut quality_target = MoonshotTarget::new(
            "99.9% Quality Target",
            "First-pass yield",
            85.0,  // 85% baseline
            99.9,  // 99.9% target
            "%",
        )
        .with_description("Near-perfect quality with minimal rework")
        .with_risk_level(RiskLevel::Medium);

        quality_target.add_initiative("AI-powered code validation");
        quality_target.add_initiative("Continuous learning from defects");
        quality_target.add_initiative("Proactive anomaly detection");

        state.add_moonshot_target(quality_target);

        state
    }

    /// Add moonshot target
    pub fn add_moonshot_target(&mut self, target: MoonshotTarget) {
        self.moonshot_targets.push(target);
    }

    /// Add strategic initiative
    pub fn add_initiative(&mut self, initiative: Initiative) {
        self.initiatives.push(initiative);
    }

    /// Add stage improvement
    pub fn add_stage_improvement(&mut self, stage: StageType, improvement: StageImprovement) {
        self.stage_improvements.insert(stage, improvement);
    }

    /// Calculate overall progress towards future state
    pub fn overall_progress(&self, current_metrics: &HashMap<String, f64>) -> f64 {
        if self.moonshot_targets.is_empty() {
            return 0.0;
        }

        let total_progress: f64 = self
            .moonshot_targets
            .iter()
            .filter_map(|target| {
                current_metrics
                    .get(&target.metric_name)
                    .map(|&current| target.progress(current))
            })
            .sum();

        total_progress / self.moonshot_targets.len() as f64
    }

    /// Get summary
    pub fn summary(&self) -> String {
        format!(
            "Future State '{}' (target: {}): {} moonshot targets, {} initiatives",
            self.name,
            self.target_date.format("%Y-%m-%d"),
            self.moonshot_targets.len(),
            self.initiatives.len()
        )
    }
}

/// Strategic initiative for achieving future state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Initiative {
    /// Initiative name
    pub name: String,
    /// Description
    pub description: String,
    /// Priority (1-5, higher is more important)
    pub priority: u8,
    /// Expected impact on lead time (ms saved)
    pub expected_impact_ms: f64,
    /// Investment required
    pub investment_level: InvestmentLevel,
    /// Dependencies
    pub dependencies: Vec<String>,
}

/// Investment level required for initiative
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InvestmentLevel {
    Low,
    Medium,
    High,
    VeryHigh,
}

impl std::fmt::Display for InvestmentLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvestmentLevel::Low => write!(f, "Low"),
            InvestmentLevel::Medium => write!(f, "Medium"),
            InvestmentLevel::High => write!(f, "High"),
            InvestmentLevel::VeryHigh => write!(f, "Very High"),
        }
    }
}

/// Improvement plan for a specific stage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StageImprovement {
    /// Target lead time for this stage (ms)
    pub target_lead_time_ms: f64,
    /// Target process efficiency (0.0 to 1.0)
    pub target_efficiency: f64,
    /// Planned improvements
    pub improvements: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_moonshot_target() {
        let mut target = MoonshotTarget::new(
            "10x Lead Time",
            "lead_time",
            1000.0, // 1 second
            100.0,  // 100ms
            "ms",
        );

        assert_eq!(target.improvement_factor, 10.0);
        assert!(target.gap_percentage() > 0.0);

        target.add_initiative("Optimize SPARQL queries");
        assert_eq!(target.initiatives.len(), 1);

        // Test progress calculation
        assert_eq!(target.progress(1000.0), 0.0); // No progress
        assert_eq!(target.progress(100.0), 1.0); // Full progress
        assert!((target.progress(550.0) - 0.5).abs() < 0.01); // 50% progress
    }

    #[test]
    fn test_milestone() {
        let target_date = Utc::now();
        let mut milestone = Milestone::new("Q1 2025", target_date, 500.0);

        assert!(!milestone.achieved);
        assert!(milestone.achieved_date.is_none());

        milestone.achieve();
        assert!(milestone.achieved);
        assert!(milestone.achieved_date.is_some());
    }

    #[test]
    fn test_future_state() {
        let target_date = DateTime::parse_from_rfc3339("2028-12-31T23:59:59Z")
            .unwrap()
            .with_timezone(&Utc);

        let mut state = FutureState::new("2028 Vision", target_date);

        let target = MoonshotTarget::new("Test Target", "metric1", 100.0, 10.0, "ms");
        state.add_moonshot_target(target);

        assert_eq!(state.moonshot_targets.len(), 1);

        let mut current_metrics = HashMap::new();
        current_metrics.insert("metric1".to_string(), 55.0); // Halfway

        let progress = state.overall_progress(&current_metrics);
        assert!((progress - 0.5).abs() < 0.01);
    }

    #[test]
    fn test_moonshot_2028() {
        let state = FutureState::moonshot_2028();

        assert_eq!(state.name, "Moonshot 2028");
        assert!(state.moonshot_targets.len() >= 3);

        // Check for key targets
        let has_lead_time = state
            .moonshot_targets
            .iter()
            .any(|t| t.name.contains("Lead Time"));
        let has_automation = state
            .moonshot_targets
            .iter()
            .any(|t| t.name.contains("Automation"));
        let has_quality = state
            .moonshot_targets
            .iter()
            .any(|t| t.name.contains("Quality"));

        assert!(has_lead_time);
        assert!(has_automation);
        assert!(has_quality);
    }
}
