//! Core types for MAPE-K autonomic control system
//!
//! Defines the fundamental data structures for observations, findings, overlays, and policies.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Observation plane: telemetry, metrics, and events
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Observation {
    /// Observation ID (content-addressed hash)
    pub id: String,

    /// Observation type (metric, event, receipt, etc.)
    pub obs_type: ObservationType,

    /// Timestamp (unix milliseconds)
    pub timestamp: u64,

    /// Observation value/data
    pub data: serde_json::Value,

    /// Source component
    pub source: String,
}

/// Types of observations in the system
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ObservationType {
    /// Performance metric (latency, throughput, etc.)
    Metric,
    /// System event (snapshot change, guard failure, etc.)
    Event,
    /// Validation receipt from guard execution
    Receipt,
    /// SLO breach or threshold crossing
    Anomaly,
}

/// Finding: result of analysis on observations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Finding {
    /// Finding ID
    pub id: String,

    /// Type of finding
    pub kind: FindingKind,

    /// Severity (Critical, High, Medium, Low)
    pub severity: String,

    /// Human-readable description
    pub description: String,

    /// Affected component/pattern/guard
    pub component: String,

    /// Supporting evidence (observation IDs)
    pub evidence: Vec<String>,

    /// Suggested action
    pub suggested_action: Option<String>,

    /// When finding was generated
    pub timestamp: u64,

    /// Metadata for further analysis
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Categories of findings
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum FindingKind {
    /// Pattern (workflow structure) exceeds tick budget
    TickBudgetViolation,
    /// Guard has high failure rate
    GuardFailureRate,
    /// Observed behavior diverges from ontology assumptions
    DriftDetected,
    /// SLO threshold breached or near-miss
    SLOBreach,
    /// New behavioral pattern detected by mining
    PatternDiscovered,
    /// Opportunity to optimize (reduce ticks, improve score)
    OptimizationOpportunity,
}

/// Ontology overlay: proposed changes to Σ
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyOverlay {
    /// Overlay ID
    pub id: String,

    /// Base snapshot this overlays
    pub base_snapshot_id: String,

    /// Proposed ontology changes (Turtle RDF diff)
    pub rdf_patch: String,

    /// Type of change
    pub overlay_kind: OverlayKind,

    /// Guard adjustments (ΔQ)
    pub guard_changes: Vec<GuardChange>,

    /// Configuration changes
    pub config_changes: HashMap<String, serde_json::Value>,

    /// Proposer (policy, LLM, user, etc.)
    pub proposer: OverlayProposer,

    /// Related finding ID (if any)
    pub related_finding: Option<String>,

    /// Created timestamp
    pub created_at: u64,

    /// Validation status
    pub validation_status: ValidationStatus,

    /// Validation results from execute phase
    pub validation_results: Vec<ValidationResult>,
}

/// Kind of overlay
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum OverlayKind {
    /// Add new pattern/structure
    Addition,
    /// Modify existing structure
    Modification,
    /// Remove structure
    Removal,
    /// Reorder or restructure
    Refactoring,
}

impl std::fmt::Display for OverlayKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OverlayKind::Addition => write!(f, "Addition"),
            OverlayKind::Modification => write!(f, "Modification"),
            OverlayKind::Removal => write!(f, "Removal"),
            OverlayKind::Refactoring => write!(f, "Refactoring"),
        }
    }
}

/// Who/what proposed an overlay
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum OverlayProposer {
    /// Policy-driven (autonomic rule)
    Policy,
    /// LLM-suggested
    LLM,
    /// User/manual submission
    User,
    /// Automated optimization
    Optimizer,
}

/// Change to a guard
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GuardChange {
    /// Guard ID
    pub guard_id: String,

    /// Change type (relax, tighten, remove)
    pub change_type: String,

    /// New parameters
    pub new_params: HashMap<String, serde_json::Value>,

    /// Reason
    pub reason: String,
}

/// Validation status of overlay
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ValidationStatus {
    /// Pending validation
    Pending,
    /// Currently validating
    InProgress,
    /// Passed all checks
    Valid,
    /// Failed validation
    Invalid,
    /// Approved for promotion
    Approved,
    /// Review needed before promotion
    ReviewNeeded,
    /// Rejected - cannot promote
    Rejected,
    /// Promoted to active (Σ*)
    Promoted,
}

/// Validation stage in the pipeline
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ValidationStage {
    /// SHACL shape validation
    SHACL,
    /// Test-driven deployment
    TDD,
    /// Performance validation
    Performance,
    /// Security checks
    Security,
}

impl ToString for ValidationStage {
    fn to_string(&self) -> String {
        match self {
            ValidationStage::SHACL => "SHACL".to_string(),
            ValidationStage::TDD => "TDD".to_string(),
            ValidationStage::Performance => "Performance".to_string(),
            ValidationStage::Security => "Security".to_string(),
        }
    }
}

/// Result of a single validation check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Validation stage
    pub stage: ValidationStage,

    /// Pass/fail
    pub passed: bool,

    /// Details
    pub details: String,

    /// Duration in ms
    pub execution_time_ms: u64,

    /// Warnings (non-blocking issues)
    pub warnings: Vec<String>,
}

/// Ontology proposal (user-facing)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OverlayProposal {
    /// Human-readable title
    pub title: String,

    /// Description
    pub description: String,

    /// Proposed overlay
    pub overlay: OntologyOverlay,

    /// Cost estimate (effort hours)
    pub estimated_effort: f64,

    /// Expected improvement (score delta)
    pub expected_improvement: f64,

    /// Risk level (Low, Medium, High, Critical)
    pub risk_level: String,
}

/// Policy rule for autonomic decision-making
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyRule {
    /// Rule ID
    pub id: String,

    /// Rule name
    pub name: String,

    /// Condition (finding kind or pattern)
    pub trigger_condition: String,

    /// Action to take
    pub action: PolicyAction,

    /// Priority (higher = evaluated first)
    pub priority: i32,

    /// Is rule enabled?
    pub enabled: bool,
}

/// Action a policy can take
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PolicyAction {
    /// Generate overlay proposal
    ProposeOverlay {
        /// Overlay kind to propose
        overlay_kind: String,
        /// Parameters for proposal
        parameters: HashMap<String, serde_json::Value>,
    },
    /// Open alert/ticket
    Alert {
        /// Alert severity
        severity: String,
        /// Message
        message: String,
    },
    /// Adjust monitoring/sampling
    AdjustMonitoring {
        /// New sampling rate
        sampling_rate: f64,
    },
}

/// Promotion gate for moving Σ to Σ*
#[derive(Debug, Clone)]
pub struct PromotionGate {
    /// Is automatic promotion enabled?
    pub auto_promote: bool,

    /// Minimum validation pass rate (0-100)
    pub validation_threshold: f64,

    /// Require manual approval?
    pub require_approval: bool,

    /// Risk level at which manual approval kicks in
    pub approval_risk_threshold: String,
}

impl Default for PromotionGate {
    fn default() -> Self {
        Self {
            auto_promote: true,
            validation_threshold: 95.0,
            require_approval: false,
            approval_risk_threshold: "Critical".to_string(),
        }
    }
}

/// Knowledge store: persistent memory of system state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KnowledgeStore {
    /// All snapshots (Σ history)
    pub snapshots: Vec<SnapshotMetadata>,

    /// All observations (O)
    pub observations: Vec<Observation>,

    /// All findings (analysis results)
    pub findings: Vec<Finding>,

    /// All overlays (proposed and promoted)
    pub overlays: Vec<OntologyOverlay>,

    /// All validation results
    pub validations: Vec<ValidationResult>,

    /// Currently active snapshot (Σ*)
    pub active_snapshot_id: String,

    /// Policy rules in effect
    pub policies: Vec<PolicyRule>,

    /// MAPE-K execution metrics
    pub mape_metrics: MAPEMetrics,
}

/// Metadata about a snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotMetadata {
    /// Snapshot ID (content hash)
    pub id: String,

    /// Description of snapshot (what overlay created it, etc.)
    pub description: String,

    /// When created
    pub created_at: u64,

    /// Promotion timestamp (None if not promoted)
    pub promoted_at: Option<u64>,

    /// Number of overlays in this snapshot
    pub overlay_count: usize,

    /// Is this the currently active snapshot (Σ*)?
    pub is_active: bool,
}

/// MAPE-K execution metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MAPEMetrics {
    /// Total observations ingested
    pub observations_ingested: usize,

    /// Total findings generated
    pub findings_generated: usize,

    /// Total overlays proposed
    pub overlays_proposed: usize,

    /// Total overlays promoted to active
    pub overlays_promoted: usize,

    /// Cumulative execution time (ms)
    pub total_execution_time_ms: u64,

    /// Last execution timestamp
    pub last_execution_timestamp: u64,
}

impl Default for MAPEMetrics {
    fn default() -> Self {
        Self {
            observations_ingested: 0,
            findings_generated: 0,
            overlays_proposed: 0,
            overlays_promoted: 0,
            total_execution_time_ms: 0,
            last_execution_timestamp: 0,
        }
    }
}
