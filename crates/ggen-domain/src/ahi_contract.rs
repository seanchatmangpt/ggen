//! AHI Contract - Formal Interface for Autonomic Hyper-Intelligence in ggen
//!
//! Defines the closed-world governance model where AHI operates on Σ (ontologies),
//! Π (projections), Q (invariants), ΔΣ (overlays), Λ (priorities), and Γ (observations)
//! to continuously refine the marketplace without human code editing.
//!
//! AHI operates under three fundamental constraints:
//! 1. Closed-World Governance: All change flows through documented, auditable gates
//! 2. Deterministic Projections: Given Σ*, Q, Λ, input → identical output always
//! 3. Doctrine Alignment: Every action respects DOCTRINE_2027 constraints

use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

/// Kind of ontology element (for meta-ontology Σ² validation)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ElementKind {
    Concept,
    Relation,
    Pattern,
    Guard,
    Workflow,
    Service,
}

/// Input requirements for AHI decision cycle
#[derive(Debug, Clone)]
pub struct AHIInput<O, G, L, D>
where
    O: Debug + Clone,
    G: Debug + Clone,
    L: Debug + Clone,
    D: Debug + Clone,
{
    /// Σ* - Current active ontology snapshot (immutable)
    pub ontology_snapshot: O,

    /// Γ - Observations from KNHK (receipts, metrics, anomalies)
    pub observations: G,

    /// Λ - Priority order, risk appetite, sector-specific doctrine
    pub lambda_config: L,

    /// Historical doctrine decisions (for audit chain)
    pub doctrine_history: D,
}

/// Output decisions from AHI cycle
#[derive(Debug, Clone)]
pub struct AHIOutput<DS, DPI, DQ, MA>
where
    DS: Debug + Clone,
    DPI: Debug + Clone,
    DQ: Debug + Clone,
    MA: Debug + Clone,
{
    /// ΔΣ - Proposed ontology refinements
    pub delta_sigma: Option<DS>,

    /// ΔΠ - Proposed projection changes
    pub delta_pi: Option<DPI>,

    /// ΔQ - Proposed invariant adjustments
    pub delta_q: Option<DQ>,

    /// Marketplace actions (promote, deprecate, reroute)
    pub marketplace_actions: Vec<MA>,
}

/// Invariants that AHI must preserve on every cycle
#[derive(Debug, Clone)]
pub struct AHIInvariants {
    /// Every ΔΣ must be traceable to observations in Γ
    pub delta_sigma_justified: bool,

    /// Every ΔΠ must derive from valid ΔΣ or ΔQ
    pub delta_pi_grounded: bool,

    /// All changes respect Doctrine constraints
    pub doctrine_aligned: bool,

    /// Σ* remains immutable; all changes create new snapshots
    pub immutability_preserved: bool,

    /// No ad-hoc mutations to Π or Q
    pub no_hidden_changes: bool,
}

/// AHI Decision Cycle Trait - the core contract
///
/// Implementers (e.g., AutonomicIntelligence) must satisfy:
/// - Inputs come from O, Γ, Λ, doctrine_history
/// - Outputs are ΔΣ, ΔΠ, ΔQ, marketplace_actions
/// - All changes are justified and auditable
/// - Every cycle preserves AHIInvariants
pub trait AHIDecisionCycle: Send + Sync + Debug {
    /// Observation type (receipts, metrics, anomalies from KNHK)
    type Observation: Debug + Clone + Send + Sync;

    /// Ontology snapshot type
    type OntologySnapshot: Debug + Clone + Send + Sync;

    /// Configuration/priority type
    type Config: Debug + Clone + Send + Sync;

    /// Doctrine history type
    type DoctrineHistory: Debug + Clone + Send + Sync;

    /// Proposed ontology change
    type DeltaSigma: Debug + Clone + Send + Sync;

    /// Proposed projection change
    type DeltaPi: Debug + Clone + Send + Sync;

    /// Proposed invariant change
    type DeltaQ: Debug + Clone + Send + Sync;

    /// Marketplace action (promote, deprecate, etc.)
    type MarketplaceAction: Debug + Clone + Send + Sync;

    /// Proposal with justification
    type Justification: Debug + Clone + Send + Sync;

    /// Execute one full MAPE-K + governance cycle
    fn cycle(
        &self,
        input: AHIInput<
            Self::OntologySnapshot,
            Vec<Self::Observation>,
            Self::Config,
            Self::DoctrineHistory,
        >,
    ) -> Result<
        AHIOutput<Self::DeltaSigma, Self::DeltaPi, Self::DeltaQ, Self::MarketplaceAction>,
        AHIError,
    >;

    /// Justify a ΔΣ proposal - must cite observations from Γ
    fn justify_delta_sigma(
        &self,
        proposal: &Self::DeltaSigma,
        observations: &[Self::Observation],
    ) -> Result<Self::Justification, AHIError>;

    /// Validate that output preserves invariants
    fn validate_invariants(
        &self,
        input: &AHIInput<
            Self::OntologySnapshot,
            Vec<Self::Observation>,
            Self::Config,
            Self::DoctrineHistory,
        >,
        output: &AHIOutput<Self::DeltaSigma, Self::DeltaPi, Self::DeltaQ, Self::MarketplaceAction>,
    ) -> Result<AHIInvariants, AHIError>;

    /// Audit trail: show the decision path that led to this output
    fn audit_path(
        &self,
        output: &AHIOutput<Self::DeltaSigma, Self::DeltaPi, Self::DeltaQ, Self::MarketplaceAction>,
    ) -> Vec<String>;
}

/// Error type for AHI operations
#[derive(Debug, Clone)]
pub enum AHIError {
    /// Doctrine constraint violated
    DoctrineViolation(String),

    /// Observation insufficient to justify proposal
    InsufficientJustification(String),

    /// Invariant would be broken
    InvariantBreach(String),

    /// Configuration invalid
    InvalidConfig(String),

    /// Ontology snapshot invalid
    InvalidSnapshot(String),

    /// Projection cannot be generated from ontology
    ProjectionError(String),

    /// Marketplace action invalid
    InvalidMarketplaceAction(String),
}

impl std::fmt::Display for AHIError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AHIError::DoctrineViolation(msg) => write!(f, "Doctrine violation: {}", msg),
            AHIError::InsufficientJustification(msg) => write!(f, "Insufficient justification: {}", msg),
            AHIError::InvariantBreach(msg) => write!(f, "Invariant breach: {}", msg),
            AHIError::InvalidConfig(msg) => write!(f, "Invalid config: {}", msg),
            AHIError::InvalidSnapshot(msg) => write!(f, "Invalid snapshot: {}", msg),
            AHIError::ProjectionError(msg) => write!(f, "Projection error: {}", msg),
            AHIError::InvalidMarketplaceAction(msg) => write!(f, "Invalid marketplace action: {}", msg),
        }
    }
}

impl std::error::Error for AHIError {}

/// Sealed trait for observable measurement (enables closed-world control)
pub trait Measurable: Debug + Clone + Send + Sync {
    /// Extract numeric metric from observation
    fn metric(&self) -> f64;

    /// Get measurement category
    fn category(&self) -> &str;
}

/// Doctrine constraint - executable rule on Σ, Π, ΔΣ
///
/// Doctrine_2027 is encoded as a set of DoctrineConstraints that AHI
/// must satisfy. Examples:
/// - "Closed world": ΔΣ must be justified by Γ observations
/// - "Chatman constant": Π projections must respect 8-tick budget
/// - "Sector isolation": ΔΣ/ΔΠ changes don't leak across sectors
pub trait DoctrineConstraint: Send + Sync + Debug {
    /// Apply constraint - returns error if violated
    fn check(&self, subject: &str, kind: ElementKind, context: &str) -> Result<(), AHIError>;

    /// Human-readable constraint name
    fn name(&self) -> &str;

    /// Doctrine that this constraint enforces
    fn doctrine(&self) -> &str;
}

/// Proposal trait - ΔΣ, ΔΠ, or ΔQ candidate with justification
///
/// Every proposal must carry:
/// - What is changing (element, kind, current value, new value)
/// - Why (observations that justify it)
/// - Risk/benefit estimates
/// - Doctrine alignment check
pub trait Proposal: Send + Sync + Debug + Clone {
    /// ID of the proposal (for audit chain)
    fn id(&self) -> &str;

    /// What is proposed
    fn what(&self) -> String;

    /// Why (cite observations)
    fn why(&self) -> Vec<String>;

    /// Expected benefit (0-100 score)
    fn expected_benefit(&self) -> f64;

    /// Risk level (0-100 score, 0=safe, 100=dangerous)
    fn risk_level(&self) -> f64;

    /// Is this doctrine-aligned?
    fn doctrine_aligned(&self) -> bool;

    /// Can be converted to action (approved for promotion)
    fn is_actionable(&self) -> bool;
}

/// Sector context - allows multi-tenant, vertical-specific optimization
///
/// AHI can specialize its behavior per sector (finance, healthcare, logistics, etc.)
/// while maintaining global invariants
#[derive(Debug, Clone)]
pub struct SectorContext {
    /// Sector ID (e.g., "finance", "healthcare", "logistics")
    pub sector_id: String,

    /// Tenant ID (account/org that owns this sector slice)
    pub tenant_id: String,

    /// Regulatory strictness (0-100, higher = more strict)
    pub regulatory_strictness: u32,

    /// Risk appetite (0-100, higher = more willing to experiment)
    pub risk_appetite: u32,

    /// Performance vs robustness bias (0=robustness, 100=performance)
    pub performance_bias: u32,
}

/// WIP: State machine for proposal lifecycle
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProposalState {
    /// Proposed but not yet validated
    Candidate,

    /// Passed all doctrine checks
    Validated,

    /// Approved for promotion
    Approved,

    /// Executed (Σ*, Π*, Q* updated)
    Promoted,

    /// Rejected (violated doctrine or risk threshold)
    Rejected,

    /// Reverted after execution (rollback due to incident)
    Reverted,
}

/// WIP: Generic state machine for proposal transitions
pub struct ProposalTransition<P: Proposal> {
    proposal: P,
    current_state: ProposalState,
    _phantom: PhantomData<P>,
}

impl<P: Proposal> ProposalTransition<P> {
    pub fn new(proposal: P) -> Self {
        Self {
            proposal,
            current_state: ProposalState::Candidate,
            _phantom: PhantomData,
        }
    }

    /// Transition from Candidate → Validated (if doctrine check passes)
    pub fn validate(mut self, doctrine_ok: bool) -> Result<Self, AHIError> {
        if self.current_state != ProposalState::Candidate {
            return Err(AHIError::InvalidConfig(
                "Can only validate from Candidate state".to_string(),
            ));
        }

        if !doctrine_ok || !self.proposal.doctrine_aligned() {
            self.current_state = ProposalState::Rejected;
            return Err(AHIError::DoctrineViolation(
                "Proposal fails doctrine alignment".to_string(),
            ));
        }

        self.current_state = ProposalState::Validated;
        Ok(self)
    }

    /// Transition from Validated → Approved (if risk/benefit acceptable)
    pub fn approve(mut self, risk_acceptable: bool) -> Result<Self, AHIError> {
        if self.current_state != ProposalState::Validated {
            return Err(AHIError::InvalidConfig(
                "Can only approve from Validated state".to_string(),
            ));
        }

        if !risk_acceptable || self.proposal.risk_level() > 75.0 {
            self.current_state = ProposalState::Rejected;
            return Err(AHIError::InvalidConfig(
                "Risk level unacceptable".to_string(),
            ));
        }

        self.current_state = ProposalState::Approved;
        Ok(self)
    }

    /// Transition from Approved → Promoted (execute the change)
    pub fn promote(mut self) -> Result<Self, AHIError> {
        if self.current_state != ProposalState::Approved {
            return Err(AHIError::InvalidConfig(
                "Can only promote from Approved state".to_string(),
            ));
        }

        self.current_state = ProposalState::Promoted;
        Ok(self)
    }

    /// Rollback from any state back to Candidate
    pub fn revert(mut self) -> Self {
        self.current_state = ProposalState::Reverted;
        self
    }

    pub fn state(&self) -> ProposalState {
        self.current_state
    }

    pub fn into_proposal(self) -> P {
        self.proposal
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_proposal_state_machine() {
        #[derive(Debug, Clone)]
        struct MockProposal {
            id: String,
        }

        impl Proposal for MockProposal {
            fn id(&self) -> &str {
                &self.id
            }

            fn what(&self) -> String {
                "test".to_string()
            }

            fn why(&self) -> Vec<String> {
                vec!["because".to_string()]
            }

            fn expected_benefit(&self) -> f64 {
                50.0
            }

            fn risk_level(&self) -> f64 {
                25.0
            }

            fn doctrine_aligned(&self) -> bool {
                true
            }

            fn is_actionable(&self) -> bool {
                true
            }
        }

        let proposal = MockProposal {
            id: "prop-1".to_string(),
        };

        let transition = ProposalTransition::new(proposal);
        assert_eq!(transition.state(), ProposalState::Candidate);

        let transition = transition.validate(true).unwrap();
        assert_eq!(transition.state(), ProposalState::Validated);

        let transition = transition.approve(true).unwrap();
        assert_eq!(transition.state(), ProposalState::Approved);

        let transition = transition.promote().unwrap();
        assert_eq!(transition.state(), ProposalState::Promoted);
    }

    #[test]
    fn test_proposal_state_revert() {
        #[derive(Debug, Clone)]
        struct MockProposal {
            id: String,
        }

        impl Proposal for MockProposal {
            fn id(&self) -> &str {
                &self.id
            }

            fn what(&self) -> String {
                "test".to_string()
            }

            fn why(&self) -> Vec<String> {
                vec![]
            }

            fn expected_benefit(&self) -> f64 {
                50.0
            }

            fn risk_level(&self) -> f64 {
                25.0
            }

            fn doctrine_aligned(&self) -> bool {
                true
            }

            fn is_actionable(&self) -> bool {
                true
            }
        }

        let proposal = MockProposal {
            id: "prop-2".to_string(),
        };

        let transition = ProposalTransition::new(proposal)
            .validate(true)
            .unwrap()
            .approve(true)
            .unwrap()
            .promote()
            .unwrap();

        let reverted = transition.revert();
        assert_eq!(reverted.state(), ProposalState::Reverted);
    }
}
