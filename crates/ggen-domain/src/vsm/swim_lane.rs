//! Swim Lane Analysis for Stakeholder Touchpoints
//!
//! Tracks stakeholder roles and their interactions throughout the value stream.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Stakeholder roles in the semantic generation pipeline
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StakeholderRole {
    /// Designs semantic models and RDF schemas
    OntologyArchitect,
    /// Creates and maintains generation templates
    TemplateAuthor,
    /// Manages RDF data and SPARQL queries
    DataEngineer,
    /// Consumes generated code and provides feedback
    Developer,
    /// Validates outputs and enforces quality gates
    QualityAssurance,
    /// Handles deployment and operations
    DevOps,
    /// Automated AI-driven generation and governance
    AISystem,
    /// Approval authority for critical decisions
    GovernanceBoard,
}

impl StakeholderRole {
    /// Get human-readable name for stakeholder role
    pub fn name(&self) -> &'static str {
        match self {
            StakeholderRole::OntologyArchitect => "Ontology Architect",
            StakeholderRole::TemplateAuthor => "Template Author",
            StakeholderRole::DataEngineer => "Data Engineer",
            StakeholderRole::Developer => "Developer",
            StakeholderRole::QualityAssurance => "Quality Assurance",
            StakeholderRole::DevOps => "DevOps Engineer",
            StakeholderRole::AISystem => "AI System",
            StakeholderRole::GovernanceBoard => "Governance Board",
        }
    }

    /// Get RDF URI suffix for this role
    pub fn rdf_uri(&self) -> &'static str {
        match self {
            StakeholderRole::OntologyArchitect => "OntologyArchitect",
            StakeholderRole::TemplateAuthor => "TemplateAuthor",
            StakeholderRole::DataEngineer => "DataEngineer",
            StakeholderRole::Developer => "Developer",
            StakeholderRole::QualityAssurance => "QualityAssurance",
            StakeholderRole::DevOps => "DevOps",
            StakeholderRole::AISystem => "AISystem",
            StakeholderRole::GovernanceBoard => "GovernanceBoard",
        }
    }

    /// Get color for visualization (hex code)
    pub fn color(&self) -> &'static str {
        match self {
            StakeholderRole::OntologyArchitect => "#3498db", // Blue
            StakeholderRole::TemplateAuthor => "#9b59b6",    // Purple
            StakeholderRole::DataEngineer => "#1abc9c",      // Teal
            StakeholderRole::Developer => "#2ecc71",         // Green
            StakeholderRole::QualityAssurance => "#e74c3c",  // Red
            StakeholderRole::DevOps => "#e67e22",            // Orange
            StakeholderRole::AISystem => "#34495e",          // Dark gray
            StakeholderRole::GovernanceBoard => "#f39c12",   // Yellow
        }
    }

    /// Get all stakeholder roles
    pub fn all() -> Vec<StakeholderRole> {
        vec![
            StakeholderRole::OntologyArchitect,
            StakeholderRole::TemplateAuthor,
            StakeholderRole::DataEngineer,
            StakeholderRole::Developer,
            StakeholderRole::QualityAssurance,
            StakeholderRole::DevOps,
            StakeholderRole::AISystem,
            StakeholderRole::GovernanceBoard,
        ]
    }
}

impl std::fmt::Display for StakeholderRole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// A swim lane representing a stakeholder's involvement in the value stream
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwimLane {
    /// Stakeholder role
    pub role: StakeholderRole,
    /// Total time spent in this swim lane across all stages (ms)
    pub total_time_ms: f64,
    /// Number of stages owned by this stakeholder
    pub stage_count: usize,
    /// Number of touchpoints involving this stakeholder
    pub touchpoint_count: usize,
    /// Average cycle time for this stakeholder (ms)
    pub average_cycle_time_ms: f64,
    /// Workload metrics
    pub workload: WorkloadMetrics,
}

impl SwimLane {
    /// Create a new swim lane for a stakeholder role
    pub fn new(role: StakeholderRole) -> Self {
        Self {
            role,
            total_time_ms: 0.0,
            stage_count: 0,
            touchpoint_count: 0,
            average_cycle_time_ms: 0.0,
            workload: WorkloadMetrics::default(),
        }
    }

    /// Add stage time to this swim lane
    pub fn add_stage_time(&mut self, time_ms: f64) {
        self.total_time_ms += time_ms;
        self.stage_count += 1;
        self.average_cycle_time_ms = self.total_time_ms / self.stage_count as f64;
    }

    /// Add touchpoint to this swim lane
    pub fn add_touchpoint(&mut self, duration_ms: f64) {
        self.touchpoint_count += 1;
        self.workload.collaboration_time_ms += duration_ms;
    }

    /// Get utilization ratio (0.0 to 1.0)
    pub fn utilization(&self) -> f64 {
        self.workload.utilization()
    }

    /// Check if swim lane is overloaded
    pub fn is_overloaded(&self) -> bool {
        self.utilization() > 0.85
    }

    /// Check if swim lane is underutilized
    pub fn is_underutilized(&self) -> bool {
        self.utilization() < 0.3
    }
}

/// Workload metrics for a stakeholder
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WorkloadMetrics {
    /// Active work time (ms)
    pub active_time_ms: f64,
    /// Idle time (ms)
    pub idle_time_ms: f64,
    /// Time spent in collaboration/handoffs (ms)
    pub collaboration_time_ms: f64,
    /// Time spent waiting for approvals (ms)
    pub approval_waiting_time_ms: f64,
}

impl WorkloadMetrics {
    /// Calculate utilization ratio
    pub fn utilization(&self) -> f64 {
        let total = self.active_time_ms
            + self.idle_time_ms
            + self.collaboration_time_ms
            + self.approval_waiting_time_ms;
        if total > 0.0 {
            (self.active_time_ms + self.collaboration_time_ms) / total
        } else {
            0.0
        }
    }

    /// Get total time
    pub fn total_time(&self) -> f64 {
        self.active_time_ms
            + self.idle_time_ms
            + self.collaboration_time_ms
            + self.approval_waiting_time_ms
    }
}

/// Analysis of swim lanes across the value stream
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwimLaneAnalysis {
    /// Swim lanes by stakeholder role
    pub lanes: HashMap<StakeholderRole, SwimLane>,
    /// Total number of handoffs between swim lanes
    pub total_handoffs: usize,
    /// Average handoff time (ms)
    pub average_handoff_time_ms: f64,
}

impl SwimLaneAnalysis {
    /// Create new swim lane analysis
    pub fn new() -> Self {
        Self {
            lanes: HashMap::new(),
            total_handoffs: 0,
            average_handoff_time_ms: 0.0,
        }
    }

    /// Get or create swim lane for a role
    pub fn get_or_create_lane(&mut self, role: StakeholderRole) -> &mut SwimLane {
        self.lanes.entry(role).or_insert_with(|| SwimLane::new(role))
    }

    /// Record a handoff between swim lanes
    pub fn record_handoff(&mut self, _from: StakeholderRole, _to: StakeholderRole, duration_ms: f64) {
        self.total_handoffs += 1;
        let total_time = self.average_handoff_time_ms * (self.total_handoffs - 1) as f64;
        self.average_handoff_time_ms = (total_time + duration_ms) / self.total_handoffs as f64;
    }

    /// Get most loaded stakeholder
    pub fn most_loaded_stakeholder(&self) -> Option<(StakeholderRole, f64)> {
        self.lanes
            .iter()
            .max_by(|a, b| {
                a.1.utilization()
                    .partial_cmp(&b.1.utilization())
                    .unwrap()
            })
            .map(|(role, lane)| (*role, lane.utilization()))
    }

    /// Get least loaded stakeholder
    pub fn least_loaded_stakeholder(&self) -> Option<(StakeholderRole, f64)> {
        self.lanes
            .iter()
            .min_by(|a, b| {
                a.1.utilization()
                    .partial_cmp(&b.1.utilization())
                    .unwrap()
            })
            .map(|(role, lane)| (*role, lane.utilization()))
    }

    /// Get stakeholders that are overloaded
    pub fn overloaded_stakeholders(&self) -> Vec<StakeholderRole> {
        self.lanes
            .iter()
            .filter(|(_, lane)| lane.is_overloaded())
            .map(|(role, _)| *role)
            .collect()
    }

    /// Get stakeholders that are underutilized
    pub fn underutilized_stakeholders(&self) -> Vec<StakeholderRole> {
        self.lanes
            .iter()
            .filter(|(_, lane)| lane.is_underutilized())
            .map(|(role, _)| *role)
            .collect()
    }

    /// Get summary statistics
    pub fn summary(&self) -> String {
        let avg_util = if !self.lanes.is_empty() {
            self.lanes.values().map(|l| l.utilization()).sum::<f64>() / self.lanes.len() as f64
        } else {
            0.0
        };

        format!(
            "{} swim lanes, {} handoffs, avg handoff time: {:.2}ms, avg utilization: {:.1}%",
            self.lanes.len(),
            self.total_handoffs,
            self.average_handoff_time_ms,
            avg_util * 100.0
        )
    }
}

impl Default for SwimLaneAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stakeholder_roles() {
        assert_eq!(
            StakeholderRole::OntologyArchitect.name(),
            "Ontology Architect"
        );
        assert_eq!(StakeholderRole::AISystem.color(), "#34495e");
        assert_eq!(StakeholderRole::all().len(), 8);
    }

    #[test]
    fn test_swim_lane() {
        let mut lane = SwimLane::new(StakeholderRole::Developer);

        lane.add_stage_time(1000.0);
        lane.add_stage_time(500.0);

        assert_eq!(lane.stage_count, 2);
        assert_eq!(lane.total_time_ms, 1500.0);
        assert_eq!(lane.average_cycle_time_ms, 750.0);
    }

    #[test]
    fn test_workload_metrics() {
        let mut workload = WorkloadMetrics::default();
        workload.active_time_ms = 800.0;
        workload.idle_time_ms = 200.0;

        assert_eq!(workload.total_time(), 1000.0);
        assert_eq!(workload.utilization(), 0.8);
    }

    #[test]
    fn test_swim_lane_analysis() {
        let mut analysis = SwimLaneAnalysis::new();

        let lane = analysis.get_or_create_lane(StakeholderRole::Developer);
        lane.workload.active_time_ms = 900.0;
        lane.workload.idle_time_ms = 100.0;

        let lane2 = analysis.get_or_create_lane(StakeholderRole::AISystem);
        lane2.workload.active_time_ms = 200.0;
        lane2.workload.idle_time_ms = 800.0;

        analysis.record_handoff(
            StakeholderRole::Developer,
            StakeholderRole::AISystem,
            50.0,
        );

        assert_eq!(analysis.total_handoffs, 1);
        assert_eq!(analysis.average_handoff_time_ms, 50.0);

        let (role, util) = analysis.most_loaded_stakeholder().unwrap();
        assert_eq!(role, StakeholderRole::Developer);
        assert!(util > 0.8);

        let overloaded = analysis.overloaded_stakeholders();
        assert_eq!(overloaded.len(), 1);
        assert_eq!(overloaded[0], StakeholderRole::Developer);
    }
}
