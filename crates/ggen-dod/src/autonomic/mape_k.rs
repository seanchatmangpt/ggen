//! MAPE-K Autonomic Loop: Monitor-Analyze-Plan-Execute-Knowledge
//!
//! The autonomic loop enables ggen to self-improve without human intervention.
//! Each cycle:
//! 1. Monitor: Collect observations (O)
//! 2. Analyze: Detect anomalies and drift
//! 3. Plan: Propose schema changes (ΔΣ)
//! 4. Execute: Apply changes with proofs and rollback capability
//! 5. Knowledge: Record findings and decisions in Γ

use crate::error::DoDResult;
use crate::observation::Observation;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;

/// Phase 1: Monitor - Collect observations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObservationPhase {
    /// Collected observations
    observations: Vec<Observation>,
    /// Monitoring started at
    started_at: DateTime<Utc>,
}

impl ObservationPhase {
    /// Create a new monitoring phase
    pub fn new() -> Self {
        Self {
            observations: Vec::new(),
            started_at: Utc::now(),
        }
    }

    /// Add an observation
    pub fn observe(mut self, obs: Observation) -> Self {
        self.observations.push(obs);
        self
    }

    /// Get observations
    pub fn observations(&self) -> &[Observation] {
        &self.observations
    }

    /// Transition to analysis
    pub fn analyze(self) -> DoDResult<AnalysisPhase> {
        if self.observations.is_empty() {
            return Err(crate::error::DoDError::MAPEKPhase(
                "no observations collected".to_string(),
            ));
        }

        Ok(AnalysisPhase::from_observations(self.observations))
    }
}

impl Default for ObservationPhase {
    fn default() -> Self {
        Self::new()
    }
}

/// Finding from analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Finding {
    /// Finding type
    finding_type: String,
    /// Severity (info, warning, error)
    severity: String,
    /// Description
    description: String,
    /// Affected subsystems
    affected_subsystems: Vec<String>,
}

impl Finding {
    /// Create a new finding
    pub fn new(
        finding_type: impl Into<String>, severity: impl Into<String>,
        description: impl Into<String>,
    ) -> Self {
        Self {
            finding_type: finding_type.into(),
            severity: severity.into(),
            description: description.into(),
            affected_subsystems: Vec::new(),
        }
    }

    /// Add affected subsystem
    pub fn with_subsystem(mut self, subsystem: impl Into<String>) -> Self {
        self.affected_subsystems.push(subsystem.into());
        self
    }

    /// Get finding type
    pub fn finding_type(&self) -> &str {
        &self.finding_type
    }

    /// Get severity
    pub fn severity(&self) -> &str {
        &self.severity
    }

    /// Is this error-level or higher?
    pub fn is_critical(&self) -> bool {
        self.severity == "error" || self.severity == "critical"
    }
}

/// Phase 2: Analyze - Detect anomalies and drift
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisPhase {
    /// Input observations
    observations: Vec<Observation>,
    /// Detected findings
    findings: Vec<Finding>,
    /// Analysis started at
    started_at: DateTime<Utc>,
}

impl AnalysisPhase {
    /// Create analysis phase from observations
    fn from_observations(observations: Vec<Observation>) -> Self {
        Self {
            observations,
            findings: Vec::new(),
            started_at: Utc::now(),
        }
    }

    /// Record a finding
    pub fn with_finding(mut self, finding: Finding) -> Self {
        self.findings.push(finding);
        self
    }

    /// Get findings
    pub fn findings(&self) -> &[Finding] {
        &self.findings
    }

    /// Transition to planning
    pub fn plan(self) -> DoDResult<PlanningPhase> {
        Ok(PlanningPhase::from_findings(self.findings))
    }
}

/// Proposed schema change (ΔΣ)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SchemaProposal {
    /// Proposal ID
    id: String,
    /// Description
    description: String,
    /// Proposed changes
    changes: Vec<String>,
    /// Risk level (low, medium, high)
    risk_level: String,
    /// Expected benefits
    expected_benefits: Vec<String>,
}

impl SchemaProposal {
    /// Create a new schema proposal
    pub fn new(description: impl Into<String>) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            description: description.into(),
            changes: Vec::new(),
            risk_level: "low".to_string(),
            expected_benefits: Vec::new(),
        }
    }

    /// Get proposal ID
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Add a change
    pub fn with_change(mut self, change: impl Into<String>) -> Self {
        self.changes.push(change.into());
        self
    }

    /// Add benefit
    pub fn with_benefit(mut self, benefit: impl Into<String>) -> Self {
        self.expected_benefits.push(benefit.into());
        self
    }

    /// Set risk level
    pub fn with_risk_level(mut self, risk: impl Into<String>) -> Self {
        self.risk_level = risk.into();
        self
    }
}

/// Phase 3: Plan - Propose schema changes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlanningPhase {
    /// Detected findings
    findings: Vec<Finding>,
    /// Proposed changes
    proposals: Vec<SchemaProposal>,
    /// Planning started at
    started_at: DateTime<Utc>,
}

impl PlanningPhase {
    /// Create planning phase from findings
    fn from_findings(findings: Vec<Finding>) -> Self {
        Self {
            findings,
            proposals: Vec::new(),
            started_at: Utc::now(),
        }
    }

    /// Add a proposal
    pub fn with_proposal(mut self, proposal: SchemaProposal) -> Self {
        self.proposals.push(proposal);
        self
    }

    /// Get proposals
    pub fn proposals(&self) -> &[SchemaProposal] {
        &self.proposals
    }

    /// Transition to execution
    pub fn execute(self) -> DoDResult<ExecutionPhase> {
        Ok(ExecutionPhase::from_proposals(self.proposals))
    }
}

/// Execution result
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExecutionResult {
    /// Executed successfully
    Success,
    /// Rolled back due to failure
    RolledBack,
    /// Rejected (didn't meet requirements)
    Rejected,
}

/// Phase 4: Execute - Apply changes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionPhase {
    /// Proposed changes
    proposals: Vec<SchemaProposal>,
    /// Execution results
    results: Vec<(String, ExecutionResult)>,
    /// Execution started at
    started_at: DateTime<Utc>,
}

impl ExecutionPhase {
    /// Create execution phase from proposals
    fn from_proposals(proposals: Vec<SchemaProposal>) -> Self {
        Self {
            proposals,
            results: Vec::new(),
            started_at: Utc::now(),
        }
    }

    /// Record an execution result
    pub fn record_result(
        mut self, proposal_id: impl Into<String>, result: ExecutionResult,
    ) -> Self {
        self.results.push((proposal_id.into(), result));
        self
    }

    /// Get results
    pub fn results(&self) -> &[(String, ExecutionResult)] {
        &self.results
    }

    /// Transition to knowledge update
    pub fn learn(self) -> KnowledgePhase {
        KnowledgePhase::from_execution(self.results)
    }
}

/// Phase 5: Knowledge - Record findings and decisions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KnowledgePhase {
    /// Execution results
    results: Vec<(String, ExecutionResult)>,
    /// Knowledge updated at
    updated_at: DateTime<Utc>,
}

impl KnowledgePhase {
    /// Create knowledge phase from execution
    fn from_execution(results: Vec<(String, ExecutionResult)>) -> Self {
        Self {
            results,
            updated_at: Utc::now(),
        }
    }

    /// Get results
    pub fn results(&self) -> &[(String, ExecutionResult)] {
        &self.results
    }
}

/// The MAPE-K loop: cycles through all phases
#[derive(Debug)]
pub struct MAPEKLoop {
    /// History of completed cycles
    cycles: VecDeque<MAPEKCycle>,
    /// Current cycle (if in progress)
    current_cycle: Option<MAPEKCycleState>,
}

/// A completed MAPE-K cycle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MAPEKCycle {
    /// Cycle number
    cycle_num: u64,
    /// Observations collected
    observation_count: usize,
    /// Findings detected
    finding_count: usize,
    /// Proposals made
    proposal_count: usize,
    /// Successful executions
    successful_executions: usize,
    /// Failed executions
    failed_executions: usize,
    /// Cycle completed at
    completed_at: DateTime<Utc>,
}

/// Current state of MAPE-K cycle
#[derive(Debug, Clone)]
pub enum MAPEKCycleState {
    Monitoring(ObservationPhase),
    Analyzing(AnalysisPhase),
    Planning(PlanningPhase),
    Executing(ExecutionPhase),
    Learning(KnowledgePhase),
}

impl MAPEKLoop {
    /// Create a new MAPE-K loop
    pub fn new() -> Self {
        Self {
            cycles: VecDeque::new(),
            current_cycle: None,
        }
    }

    /// Start a new monitoring phase
    pub fn start_monitor(&mut self) {
        self.current_cycle = Some(MAPEKCycleState::Monitoring(ObservationPhase::new()));
    }

    /// Get the number of completed cycles
    pub fn completed_cycles(&self) -> usize {
        self.cycles.len()
    }

    /// Get the last N cycles
    pub fn recent_cycles(&self, n: usize) -> Vec<&MAPEKCycle> {
        self.cycles.iter().rev().take(n).collect()
    }

    /// Current phase name
    pub fn current_phase(&self) -> Option<&'static str> {
        match &self.current_cycle {
            Some(MAPEKCycleState::Monitoring(_)) => Some("Monitoring"),
            Some(MAPEKCycleState::Analyzing(_)) => Some("Analyzing"),
            Some(MAPEKCycleState::Planning(_)) => Some("Planning"),
            Some(MAPEKCycleState::Executing(_)) => Some("Executing"),
            Some(MAPEKCycleState::Learning(_)) => Some("Learning"),
            None => None,
        }
    }
}

impl Default for MAPEKLoop {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_observation_phase() {
        let phase = ObservationPhase::new();
        assert_eq!(phase.observations().len(), 0);
    }

    #[test]
    fn test_finding() {
        let finding = Finding::new("drift", "warning", "detected drift in metric");
        assert_eq!(finding.finding_type(), "drift");
        assert!(!finding.is_critical());
    }

    #[test]
    fn test_schema_proposal() {
        let proposal = SchemaProposal::new("add new field")
            .with_change("fields += 1")
            .with_benefit("better observability");

        assert_eq!(proposal.changes.len(), 1);
        assert_eq!(proposal.expected_benefits.len(), 1);
    }

    #[test]
    fn test_mape_k_loop() {
        let loop_ = MAPEKLoop::new();
        assert_eq!(loop_.completed_cycles(), 0);
    }
}
