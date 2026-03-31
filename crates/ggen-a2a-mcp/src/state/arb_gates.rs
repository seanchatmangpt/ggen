//! ARB Gate System - Architecture Review Board approval workflow.
//!
//! ARB gates sit at the end of each TOGAF phase and require approval
//! from designated stakeholder roles before the protocol can advance.
//!
//! Gate definitions follow the ontology in `turn_protocol.ttl`:
//! - ChiefArchitect: approves at turns 8, 22, 40, 54, 70
//! - ComplianceOfficer: approves at turns 22, 40, 62
//! - ExecutiveSponsor: approves at turns 10, 25, 45, 65
//! - BusinessOwner: approves at turns 10, 25, 45

use std::collections::HashMap;
use std::sync::Arc;

use chrono::Utc;
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;
use tracing::{info, instrument};

use super::error::{StateError, StateResult};
use super::togaf_state::TogafPhase;

// ---------------------------------------------------------------------------
// StakeholderRole
// ---------------------------------------------------------------------------

/// A stakeholder role that can approve ARB gates.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum StakeholderRole {
    /// Chief Architect -- reviews technical coherence and architectural integrity.
    ChiefArchitect,
    /// Executive Sponsor -- reviews business value alignment and strategic fit.
    ExecutiveSponsor,
    /// Compliance Officer -- reviews FIBO regulatory compliance and risk.
    ComplianceOfficer,
    /// Business Owner -- reviews business capability alignment.
    BusinessOwner,
}

impl StakeholderRole {
    /// All known stakeholder roles.
    pub fn all() -> &'static [StakeholderRole] {
        &[
            StakeholderRole::ChiefArchitect,
            StakeholderRole::ExecutiveSponsor,
            StakeholderRole::ComplianceOfficer,
            StakeholderRole::BusinessOwner,
        ]
    }

    /// Display name for the role.
    pub fn display_name(&self) -> &'static str {
        match self {
            StakeholderRole::ChiefArchitect => "Chief Architect",
            StakeholderRole::ExecutiveSponsor => "Executive Sponsor",
            StakeholderRole::ComplianceOfficer => "Compliance Officer",
            StakeholderRole::BusinessOwner => "Business Owner",
        }
    }
}

impl std::fmt::Display for StakeholderRole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

// ---------------------------------------------------------------------------
// ApprovalCriterion
// ---------------------------------------------------------------------------

/// A criterion that must be satisfied for ARB gate approval.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApprovalCriterion {
    /// Human-readable name of the criterion.
    pub name: String,
    /// Description of what must be satisfied.
    pub description: String,
    /// Whether this criterion is mandatory (vs. recommended).
    pub mandatory: bool,
}

// ---------------------------------------------------------------------------
// ApprovalDecision
// ---------------------------------------------------------------------------

/// A single reviewer's decision on an ARB gate.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ApprovalDecision {
    /// The reviewer approves the gate.
    Approved,
    /// The reviewer rejects the gate with a reason.
    Rejected(String),
    /// The reviewer requests revisions with specific feedback.
    NeedsRevision(String),
}

impl ApprovalDecision {
    /// Returns true if the decision is an approval.
    pub fn is_approved(&self) -> bool {
        matches!(self, ApprovalDecision::Approved)
    }
}

// ---------------------------------------------------------------------------
// ApprovalResponse
// ---------------------------------------------------------------------------

/// A reviewer's response for an ARB gate.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApprovalResponse {
    /// The stakeholder role of the reviewer.
    pub reviewer: StakeholderRole,
    /// The reviewer's decision.
    pub decision: ApprovalDecision,
    /// Free-text comments from the reviewer.
    pub comments: String,
    /// Timestamp of the response.
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl ApprovalResponse {
    /// Create a new approval response.
    pub fn new(
        reviewer: StakeholderRole, decision: ApprovalDecision, comments: impl Into<String>,
    ) -> Self {
        Self {
            reviewer,
            decision,
            comments: comments.into(),
            timestamp: Utc::now(),
        }
    }
}

// ---------------------------------------------------------------------------
// ApprovalStatus
// ---------------------------------------------------------------------------

/// Overall status of an ARB gate approval.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ApprovalStatus {
    /// Approval is pending; not all reviewers have responded.
    Pending,
    /// All required reviewers have approved.
    Approved,
    /// One or more reviewers rejected the gate.
    Rejected(String),
    /// The approval window has expired.
    Expired,
}

impl std::fmt::Display for ApprovalStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ApprovalStatus::Pending => write!(f, "Pending"),
            ApprovalStatus::Approved => write!(f, "Approved"),
            ApprovalStatus::Rejected(reason) => write!(f, "Rejected({})", reason),
            ApprovalStatus::Expired => write!(f, "Expired"),
        }
    }
}

// ---------------------------------------------------------------------------
// ArbGate
// ---------------------------------------------------------------------------

/// An ARB gate at a specific turn in the protocol.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArbGate {
    /// The turn number where this gate sits.
    pub turn: usize,
    /// The phase this gate belongs to.
    pub phase: TogafPhase,
    /// Stakeholder roles required to approve this gate.
    pub required_reviewers: Vec<StakeholderRole>,
    /// Criteria that must be satisfied for approval.
    pub criteria: Vec<ApprovalCriterion>,
}

impl ArbGate {
    /// Create a new ARB gate.
    pub fn new(
        turn: usize, phase: TogafPhase, required_reviewers: Vec<StakeholderRole>,
        criteria: Vec<ApprovalCriterion>,
    ) -> Self {
        Self {
            turn,
            phase,
            required_reviewers,
            criteria,
        }
    }
}

// ---------------------------------------------------------------------------
// ArbApproval
// ---------------------------------------------------------------------------

/// Tracks the approval state of a single ARB gate.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArbApproval {
    /// The gate being approved.
    pub gate: ArbGate,
    /// Responses received from reviewers.
    pub responses: HashMap<StakeholderRole, ApprovalResponse>,
    /// Current approval status.
    pub status: ApprovalStatus,
    /// When the approval was created.
    pub created_at: chrono::DateTime<chrono::Utc>,
    /// When the approval was resolved (approved/rejected).
    pub resolved_at: Option<chrono::DateTime<chrono::Utc>>,
}

impl ArbApproval {
    /// Create a new pending approval for a gate.
    pub fn new(gate: ArbGate) -> Self {
        Self {
            gate,
            responses: HashMap::new(),
            status: ApprovalStatus::Pending,
            created_at: Utc::now(),
            resolved_at: None,
        }
    }

    /// Returns true if all required reviewers have responded.
    pub fn all_responded(&self) -> bool {
        self.gate
            .required_reviewers
            .iter()
            .all(|role| self.responses.contains_key(role))
    }

    /// Returns the set of required reviewers who have not yet responded.
    pub fn missing_reviewers(&self) -> Vec<StakeholderRole> {
        self.gate
            .required_reviewers
            .iter()
            .filter(|role| !self.responses.contains_key(*role))
            .cloned()
            .collect()
    }

    /// Compute the current status based on responses received.
    fn recompute_status(&mut self) {
        if !self.all_responded() {
            self.status = ApprovalStatus::Pending;
            return;
        }

        // Check for any rejections.
        for response in self.responses.values() {
            match &response.decision {
                ApprovalDecision::Rejected(reason) => {
                    self.status = ApprovalStatus::Rejected(reason.clone());
                    self.resolved_at = Some(Utc::now());
                    return;
                }
                ApprovalDecision::NeedsRevision(reason) => {
                    self.status = ApprovalStatus::Rejected(format!("Needs revision: {}", reason));
                    self.resolved_at = Some(Utc::now());
                    return;
                }
                ApprovalDecision::Approved => {}
            }
        }

        // All reviewers approved.
        self.status = ApprovalStatus::Approved;
        self.resolved_at = Some(Utc::now());
    }
}

// ---------------------------------------------------------------------------
// ArbGateSummary
// ---------------------------------------------------------------------------

/// Read-only summary of an ARB gate's state.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArbGateSummary {
    /// Turn number of the gate.
    pub turn: usize,
    /// Phase of the gate.
    pub phase: TogafPhase,
    /// Current approval status.
    pub status: ApprovalStatus,
    /// Number of required reviewers.
    pub required_count: usize,
    /// Number of reviewers who have responded.
    pub responded_count: usize,
    /// Reviewers who have not yet responded.
    pub missing_reviewers: Vec<String>,
}

// ---------------------------------------------------------------------------
// ArbApprovalManager
// ---------------------------------------------------------------------------

/// Manages ARB gate definitions and approval workflows.
///
/// Thread-safe via `Arc<RwLock<>>` for interior mutability.
#[derive(Clone)]
pub struct ArbApprovalManager {
    gates: Arc<RwLock<HashMap<usize, ArbGate>>>,
    approvals: Arc<RwLock<HashMap<usize, ArbApproval>>>,
}

impl ArbApprovalManager {
    /// Create an empty manager with no gates.
    pub fn new() -> Self {
        Self {
            gates: Arc::new(RwLock::new(HashMap::new())),
            approvals: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Create a manager pre-configured with the standard TOGAF ARB gates
    /// as defined in `turn_protocol.ttl`.
    pub fn with_standard_gates() -> Self {
        let manager = Self::new();

        // Phase A gate at turn 8: ChiefArchitect
        manager.define_gate(ArbGate::new(
            8,
            TogafPhase::A,
            vec![StakeholderRole::ChiefArchitect],
            vec![ApprovalCriterion {
                name: "Architecture Vision Complete".to_string(),
                description: "Architecture vision and stakeholder requirements documented"
                    .to_string(),
                mandatory: true,
            }],
        ));

        // Phase B gate at turn 22: ChiefArchitect + ComplianceOfficer
        manager.define_gate(ArbGate::new(
            22,
            TogafPhase::B,
            vec![
                StakeholderRole::ChiefArchitect,
                StakeholderRole::ComplianceOfficer,
            ],
            vec![
                ApprovalCriterion {
                    name: "Business Architecture Complete".to_string(),
                    description: "Business capability map and organization structure finalized"
                        .to_string(),
                    mandatory: true,
                },
                ApprovalCriterion {
                    name: "FIBO Compliance".to_string(),
                    description: "FIBO semantic consistency validated across business artifacts"
                        .to_string(),
                    mandatory: true,
                },
            ],
        ));

        // Phase C gate at turn 40: ChiefArchitect + ComplianceOfficer
        manager.define_gate(ArbGate::new(
            40,
            TogafPhase::C,
            vec![
                StakeholderRole::ChiefArchitect,
                StakeholderRole::ComplianceOfficer,
            ],
            vec![ApprovalCriterion {
                name: "Information Systems Architecture Complete".to_string(),
                description: "Data and application architectures aligned with FIBO".to_string(),
                mandatory: true,
            }],
        ));

        // Phase D gate at turn 54: ChiefArchitect
        manager.define_gate(ArbGate::new(
            54,
            TogafPhase::D,
            vec![StakeholderRole::ChiefArchitect],
            vec![ApprovalCriterion {
                name: "Technology Architecture Complete".to_string(),
                description: "Technology platforms and infrastructure defined".to_string(),
                mandatory: true,
            }],
        ));

        // Phase E gate at turn 62: ComplianceOfficer
        manager.define_gate(ArbGate::new(
            62,
            TogafPhase::E,
            vec![StakeholderRole::ComplianceOfficer],
            vec![ApprovalCriterion {
                name: "Implementation Strategy Compliant".to_string(),
                description: "Solution meets FIBO regulatory requirements".to_string(),
                mandatory: true,
            }],
        ));

        // Phase F gate at turn 70: ChiefArchitect
        manager.define_gate(ArbGate::new(
            70,
            TogafPhase::F,
            vec![StakeholderRole::ChiefArchitect],
            vec![ApprovalCriterion {
                name: "Migration Plan Complete".to_string(),
                description: "Migration plan maintains FIBO semantic continuity".to_string(),
                mandatory: true,
            }],
        ));

        // Additional stakeholder review checkpoints.
        // Turn 10: ExecutiveSponsor + BusinessOwner
        manager.define_gate(ArbGate::new(
            10,
            TogafPhase::B,
            vec![
                StakeholderRole::ExecutiveSponsor,
                StakeholderRole::BusinessOwner,
            ],
            vec![ApprovalCriterion {
                name: "Target Business Capabilities Defined".to_string(),
                description: "FIBO-based target capabilities reviewed by business".to_string(),
                mandatory: true,
            }],
        ));

        // Turn 25: ExecutiveSponsor + BusinessOwner
        manager.define_gate(ArbGate::new(
            25,
            TogafPhase::C,
            vec![
                StakeholderRole::ExecutiveSponsor,
                StakeholderRole::BusinessOwner,
            ],
            vec![ApprovalCriterion {
                name: "Gap Analysis Approved".to_string(),
                description: "Business-technology gap analysis reviewed".to_string(),
                mandatory: true,
            }],
        ));

        // Turn 45: ExecutiveSponsor + BusinessOwner
        manager.define_gate(ArbGate::new(
            45,
            TogafPhase::D,
            vec![
                StakeholderRole::ExecutiveSponsor,
                StakeholderRole::BusinessOwner,
            ],
            vec![ApprovalCriterion {
                name: "Technology Alignment".to_string(),
                description: "Technology architecture aligns with business strategy".to_string(),
                mandatory: true,
            }],
        ));

        // Turn 65: ExecutiveSponsor
        manager.define_gate(ArbGate::new(
            65,
            TogafPhase::F,
            vec![StakeholderRole::ExecutiveSponsor],
            vec![ApprovalCriterion {
                name: "Migration Strategy Business Review".to_string(),
                description: "Migration plan reviewed for business continuity".to_string(),
                mandatory: true,
            }],
        ));

        manager
    }

    /// Define (or replace) a gate at the given turn.
    ///
    /// Returns `false` if the lock could not be acquired (caller should retry).
    pub fn define_gate(&self, gate: ArbGate) -> bool {
        // This is a synchronous initialization method; we use try_write
        // to avoid deadlocks during setup.
        if let Ok(mut gates) = self.gates.try_write() {
            gates.insert(gate.turn, gate);
            true
        } else {
            tracing::warn!(
                turn = gate.turn,
                "define_gate: failed to acquire lock, gate not stored"
            );
            false
        }
    }

    /// Request approval for the gate at the given turn.
    ///
    /// Creates a pending `ArbApproval` if one does not already exist.
    #[instrument(skip(self), fields(turn))]
    pub async fn request_approval(&self, turn: usize) -> StateResult<()> {
        let gates_guard = self.gates.read().await;

        let gate = gates_guard
            .get(&turn)
            .cloned()
            .ok_or(StateError::GateNotFound(turn))?;

        drop(gates_guard);

        let mut approvals_guard = self.approvals.write().await;

        if !approvals_guard.contains_key(&turn) {
            approvals_guard.insert(turn, ArbApproval::new(gate));
            info!(turn = turn, "ARB approval requested");
        }

        Ok(())
    }

    /// Submit a reviewer's response for the gate at the given turn.
    #[instrument(skip(self, response), fields(turn, reviewer = %response.reviewer))]
    pub async fn submit_response(
        &self, turn: usize, response: ApprovalResponse,
    ) -> StateResult<()> {
        let gates_guard = self.gates.read().await;

        let gate = gates_guard
            .get(&turn)
            .ok_or(StateError::GateNotFound(turn))?;

        // Validate the reviewer is in the required list.
        if !gate.required_reviewers.contains(&response.reviewer) {
            return Err(StateError::UnexpectedReviewer(
                turn,
                response.reviewer.to_string(),
            ));
        }

        drop(gates_guard);

        let mut approvals_guard = self.approvals.write().await;

        let approval = approvals_guard
            .get_mut(&turn)
            .ok_or(StateError::GateNotFound(turn))?;

        // Check for duplicate responses.
        if approval.responses.contains_key(&response.reviewer) {
            return Err(StateError::DuplicateResponse(
                response.reviewer.to_string(),
                turn,
            ));
        }

        approval
            .responses
            .insert(response.reviewer.clone(), response);
        approval.recompute_status();

        info!(
            turn = turn,
            status = %approval.status,
            "ARB response submitted"
        );

        Ok(())
    }

    /// Check whether the gate at the given turn is approved.
    pub async fn is_approved(&self, turn: usize) -> bool {
        let approvals_guard = self.approvals.read().await;
        approvals_guard
            .get(&turn)
            .map(|a| a.status == ApprovalStatus::Approved)
            .unwrap_or(false)
    }

    /// Check whether a gate exists at the given turn.
    pub async fn check_gate(&self, turn: usize) -> StateResult<bool> {
        let gates_guard = self.gates.read().await;
        Ok(gates_guard.contains_key(&turn))
    }

    /// Get the gate definition at the given turn.
    pub async fn get_gate(&self, turn: usize) -> StateResult<ArbGate> {
        let gates_guard = self.gates.read().await;
        gates_guard
            .get(&turn)
            .cloned()
            .ok_or(StateError::GateNotFound(turn))
    }

    /// Get the approval state at the given turn.
    pub async fn get_approval(&self, turn: usize) -> StateResult<ArbApproval> {
        let approvals_guard = self.approvals.read().await;
        approvals_guard
            .get(&turn)
            .cloned()
            .ok_or(StateError::GateNotFound(turn))
    }

    /// Produce a summary of all defined gates and their approval states.
    pub async fn summary(&self) -> Vec<ArbGateSummary> {
        let gates_guard = self.gates.read().await;
        let approvals_guard = self.approvals.read().await;

        let mut summaries = Vec::new();

        for (&turn, gate) in gates_guard.iter() {
            let approval = approvals_guard.get(&turn);

            let status = approval
                .map(|a| a.status.clone())
                .unwrap_or(ApprovalStatus::Pending);

            let responded_count = approval.map(|a| a.responses.len()).unwrap_or(0);

            let missing: Vec<String> = if let Some(a) = approval {
                a.missing_reviewers()
                    .iter()
                    .map(|r| r.to_string())
                    .collect()
            } else {
                gate.required_reviewers
                    .iter()
                    .map(|r| r.to_string())
                    .collect()
            };

            summaries.push(ArbGateSummary {
                turn,
                phase: gate.phase,
                status,
                required_count: gate.required_reviewers.len(),
                responded_count,
                missing_reviewers: missing,
            });
        }

        summaries.sort_by_key(|s| s.turn);
        summaries
    }

    /// Returns all turn numbers that have defined gates.
    pub async fn gate_turns(&self) -> Vec<usize> {
        let gates_guard = self.gates.read().await;
        let mut turns: Vec<usize> = gates_guard.keys().copied().collect();
        turns.sort();
        turns
    }
}

impl Default for ArbApprovalManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn standard_gates_exist() {
        let mgr = ArbApprovalManager::with_standard_gates();
        let turns = mgr.gate_turns().await;

        // Should have gates at the end of each phase plus stakeholder reviews.
        assert!(turns.contains(&8)); // Phase A end
        assert!(turns.contains(&22)); // Phase B end
        assert!(turns.contains(&40)); // Phase C end
        assert!(turns.contains(&54)); // Phase D end
        assert!(turns.contains(&62)); // Phase E end
        assert!(turns.contains(&70)); // Phase F end
    }

    #[tokio::test]
    async fn request_and_approve_single_reviewer() {
        let mgr = ArbApprovalManager::with_standard_gates();

        // Phase A gate at turn 8 requires only ChiefArchitect.
        mgr.request_approval(8).await.unwrap();

        let response = ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::Approved,
            "Architecture vision looks good",
        );
        mgr.submit_response(8, response).await.unwrap();

        assert!(mgr.is_approved(8).await);
    }

    #[tokio::test]
    async fn reject_on_single_rejection() {
        let mgr = ArbApprovalManager::with_standard_gates();
        mgr.request_approval(8).await.unwrap(); // Phase A: ChiefArchitect only

        let response = ApprovalResponse::new(
            StakeholderRole::ChiefArchitect,
            ApprovalDecision::Rejected("Missing FIBO mappings".to_string()),
            "Need more FIBO concept coverage",
        );
        mgr.submit_response(8, response).await.unwrap();

        assert!(!mgr.is_approved(8).await);

        let approval = mgr.get_approval(8).await.unwrap();
        assert_eq!(
            approval.status,
            ApprovalStatus::Rejected("Missing FIBO mappings".to_string())
        );
    }

    #[tokio::test]
    async fn approve_multi_reviewer_gate() {
        let mgr = ArbApprovalManager::with_standard_gates();
        mgr.request_approval(22).await.unwrap(); // Phase B: ChiefArchitect + ComplianceOfficer

        mgr.submit_response(
            22,
            ApprovalResponse::new(
                StakeholderRole::ChiefArchitect,
                ApprovalDecision::Approved,
                "Approved",
            ),
        )
        .await
        .unwrap();

        // Not yet approved -- ComplianceOfficer hasn't responded.
        assert!(!mgr.is_approved(22).await);

        mgr.submit_response(
            22,
            ApprovalResponse::new(
                StakeholderRole::ComplianceOfficer,
                ApprovalDecision::Approved,
                "FIBO compliance verified",
            ),
        )
        .await
        .unwrap();

        assert!(mgr.is_approved(22).await);
    }

    #[tokio::test]
    async fn unexpected_reviewer_rejected() {
        let mgr = ArbApprovalManager::with_standard_gates();
        mgr.request_approval(8).await.unwrap(); // Phase A: ChiefArchitect only

        let response = ApprovalResponse::new(
            StakeholderRole::ComplianceOfficer,
            ApprovalDecision::Approved,
            "Not my gate",
        );
        let result = mgr.submit_response(8, response).await;
        assert!(result.is_err());
        match result.unwrap_err() {
            StateError::UnexpectedReviewer(8, role) => {
                assert!(role.contains("Compliance"));
            }
            other => panic!("Expected UnexpectedReviewer, got: {:?}", other),
        }
    }

    #[tokio::test]
    async fn duplicate_response_rejected() {
        let mgr = ArbApprovalManager::with_standard_gates();
        mgr.request_approval(8).await.unwrap();

        mgr.submit_response(
            8,
            ApprovalResponse::new(
                StakeholderRole::ChiefArchitect,
                ApprovalDecision::Approved,
                "First response",
            ),
        )
        .await
        .unwrap();

        let result = mgr
            .submit_response(
                8,
                ApprovalResponse::new(
                    StakeholderRole::ChiefArchitect,
                    ApprovalDecision::Approved,
                    "Duplicate",
                ),
            )
            .await;
        assert!(result.is_err());
        match result.unwrap_err() {
            StateError::DuplicateResponse(_, 8) => {} // expected
            other => panic!("Expected DuplicateResponse, got: {:?}", other),
        }
    }

    #[tokio::test]
    async fn gate_not_found() {
        let mgr = ArbApprovalManager::new(); // Empty manager
        let result = mgr.request_approval(99).await;
        assert!(result.is_err());
        match result.unwrap_err() {
            StateError::GateNotFound(99) => {} // expected
            other => panic!("Expected GateNotFound, got: {:?}", other),
        }
    }

    #[tokio::test]
    async fn summary_lists_all_gates() {
        let mgr = ArbApprovalManager::with_standard_gates();
        mgr.request_approval(8).await.unwrap();
        mgr.submit_response(
            8,
            ApprovalResponse::new(
                StakeholderRole::ChiefArchitect,
                ApprovalDecision::Approved,
                "OK",
            ),
        )
        .await
        .unwrap();

        let summaries = mgr.summary().await;
        assert!(!summaries.is_empty());

        let turn8 = summaries.iter().find(|s| s.turn == 8).unwrap();
        assert_eq!(turn8.phase, TogafPhase::A);
        assert_eq!(turn8.status, ApprovalStatus::Approved);
        assert_eq!(turn8.required_count, 1);
        assert_eq!(turn8.responded_count, 1);
        assert!(turn8.missing_reviewers.is_empty());
    }

    #[tokio::test]
    async fn needs_revision_treated_as_rejection() {
        let mgr = ArbApprovalManager::with_standard_gates();
        mgr.request_approval(8).await.unwrap();

        mgr.submit_response(
            8,
            ApprovalResponse::new(
                StakeholderRole::ChiefArchitect,
                ApprovalDecision::NeedsRevision("Add more metrics".to_string()),
                "Needs work",
            ),
        )
        .await
        .unwrap();

        assert!(!mgr.is_approved(8).await);
    }
}
