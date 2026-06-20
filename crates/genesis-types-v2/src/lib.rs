//! KNHK V2 Type System - Foundational data structures for the workflow engine
//!
//! This crate defines all core types used throughout KNHK V2, including:
//! - Workflow and pattern definitions
//! - Execution state and events
//! - Error types
//! - Configuration types

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// A workflow pattern identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PatternId(pub u32);

impl PatternId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

/// A step in a workflow execution
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct StepId(pub String);

impl StepId {
    pub fn generate() -> Self {
        Self(Uuid::new_v4().to_string())
    }
}

/// Unique workflow execution identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExecutionId(pub Uuid);

impl ExecutionId {
    pub fn generate() -> Self {
        Self(Uuid::new_v4())
    }
}

/// Workflow definition - declarative specification of pattern flow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowDef {
    pub id: String,
    pub name: String,
    pub version: String,
    pub patterns: Vec<PatternDef>,
    pub steps: Vec<WorkflowStep>,
}

/// Pattern definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternDef {
    pub id: PatternId,
    pub name: String,
    pub category: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
}

/// A single step in a workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowStep {
    pub id: StepId,
    pub pattern_id: PatternId,
    pub inputs: HashMap<String, serde_json::Value>,
    pub outputs: Vec<String>,
}

/// Execution state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExecutionState {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// Workflow execution context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionContext {
    pub id: ExecutionId,
    pub workflow_id: String,
    pub state: ExecutionState,
    pub current_step: Option<StepId>,
    pub data: HashMap<String, serde_json::Value>,
}

impl ExecutionContext {
    pub fn new(workflow_id: String) -> Self {
        Self {
            id: ExecutionId::generate(),
            workflow_id,
            state: ExecutionState::Pending,
            current_step: None,
            data: HashMap::new(),
        }
    }
}

/// Execution event - immutable record of what happened
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Event {
    pub id: String,
    pub execution_id: ExecutionId,
    pub step_id: Option<StepId>,
    pub event_type: String,
    pub data: serde_json::Value,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl Event {
    pub fn new(execution_id: ExecutionId, event_type: String, data: serde_json::Value) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            execution_id,
            step_id: None,
            event_type,
            data,
            timestamp: chrono::Utc::now(),
        }
    }
}

/// Error type for KNHK operations
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Pattern not found: {0}")]
    PatternNotFound(String),

    #[error("Execution failed: {0}")]
    ExecutionFailed(String),

    #[error("Invalid input: {0}")]
    InvalidInput(String),

    #[error("State machine error: {0}")]
    StateError(String),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("Other error: {0}")]
    Other(String),

    #[error("POWL validation error: {0}")]
    PowlValidation(String),

    #[error("Process admission refused: {0}")]
    AdmissionRefused(String),
}

pub type Result<T> = std::result::Result<T, Error>;

// ──────────────────────────────────────────────────────────────────────────────
// POWL Process Types
// ──────────────────────────────────────────────────────────────────────────────

/// A single node in a POWL process law graph.
/// Corresponds to one activity/operator in the process.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PowlNode {
    /// Unique node identifier within this graph.
    pub id: String,
    /// Human-readable activity name (e.g. "GenerateGeometry", "RenderProjection").
    pub activity: String,
    /// Object type references (e.g. artifact IDs, receipt IDs) this node acts on.
    pub object_refs: Vec<String>,
    /// Optional guard condition expression.
    pub guard: Option<String>,
}

/// A directed edge between two POWL nodes.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PowlEdge {
    pub from: String,
    pub to: String,
    /// Optional Boolean condition (e.g. "residual > threshold").
    pub condition: Option<String>,
}

/// A complete POWL process law graph.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowlGraph {
    pub id: String,
    pub name: String,
    pub nodes: Vec<PowlNode>,
    pub edges: Vec<PowlEdge>,
    /// ID of the root/start node.
    pub root: String,
    /// IDs of terminal/sink nodes.
    pub sinks: Vec<String>,
}

impl PowlGraph {
    /// Validate structural integrity: root must exist in nodes, all edges reference
    /// valid nodes, and no duplicate node IDs exist.
    /// Returns a list of violation messages (empty = valid).
    pub fn validate(&self) -> Vec<String> {
        let mut violations: Vec<String> = Vec::new();

        // Build a set of all node IDs for O(1) lookups.
        let mut seen_ids: std::collections::HashSet<&str> =
            std::collections::HashSet::with_capacity(self.nodes.len());
        let mut node_ids: std::collections::HashSet<&str> =
            std::collections::HashSet::with_capacity(self.nodes.len());

        for node in &self.nodes {
            if !seen_ids.insert(node.id.as_str()) {
                violations.push(format!("duplicate node id '{}'", node.id));
            }
            node_ids.insert(node.id.as_str());
        }

        // Root must exist in nodes.
        if !node_ids.contains(self.root.as_str()) {
            violations.push(format!(
                "root node '{}' not found in nodes",
                self.root
            ));
        }

        // Every edge endpoint must exist in nodes.
        for edge in &self.edges {
            if !node_ids.contains(edge.from.as_str()) {
                violations.push(format!(
                    "edge 'from' node '{}' not found in nodes",
                    edge.from
                ));
            }
            if !node_ids.contains(edge.to.as_str()) {
                violations.push(format!(
                    "edge 'to' node '{}' not found in nodes",
                    edge.to
                ));
            }
        }

        violations
    }

    /// Serialize to a JSON string.
    pub fn to_json(&self) -> Result<String> {
        Ok(serde_json::to_string(self)?)
    }

    /// Deserialize from a JSON string.
    pub fn from_json(json: &str) -> Result<Self> {
        Ok(serde_json::from_str(json)?)
    }

    /// Write JSON representation to a file at `path`.
    pub fn to_json_file(&self, path: &std::path::Path) -> Result<()> {
        let json = self.to_json()?;
        std::fs::write(path, json).map_err(|e| Error::Other(e.to_string()))
    }

    /// Read and deserialize from a JSON file at `path`.
    pub fn from_json_file(path: &std::path::Path) -> Result<Self> {
        let content = std::fs::read_to_string(path).map_err(|e| Error::Other(e.to_string()))?;
        Self::from_json(&content)
    }
}

// ──────────────────────────────────────────────────────────────────────────────
// Process Admission Types
// ──────────────────────────────────────────────────────────────────────────────

/// Admission status for a process law graph against wasm4pm evidence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AdmissionStatus {
    /// All gates pass; process evidence is complete and lawful.
    Alive,
    /// Some gates pass; repair continues; no standing claimed.
    PartialAlive,
    /// A blocker gate explicitly refuses admission.
    Refused,
    /// Evidence is missing; cannot determine status.
    Unknown,
}

impl AdmissionStatus {
    /// Returns true only for `Alive`.
    #[must_use]
    pub fn is_admitted(self) -> bool {
        matches!(self, Self::Alive)
    }

    /// Returns true when the process should continue repair (`PartialAlive` or `Unknown`).
    #[must_use]
    pub fn should_continue_repair(self) -> bool {
        matches!(self, Self::PartialAlive | Self::Unknown)
    }
}

/// Result of a single gate check within process admission.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GateResult {
    pub gate_id: String,
    pub gate_name: String,
    pub status: AdmissionStatus,
    pub detail: String,
    /// Optional BLAKE3 hash of the evidence that was checked.
    pub evidence_hash: Option<String>,
}

/// Full process admission report for a POWL graph.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessAdmissionReport {
    /// UUID v4 for this admission run.
    pub operation_id: String,
    /// ID of the `PowlGraph` that was evaluated.
    pub graph_id: String,
    /// Overall admission status (computed from gate results).
    pub status: AdmissionStatus,
    pub gates: Vec<GateResult>,
    /// BLAKE3 hash of the full report (self-referential: computed over gates, not this field).
    pub receipt_hash: Option<String>,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl ProcessAdmissionReport {
    /// Compute overall status from gate results.
    ///
    /// Logic:
    /// - Any `Refused` → `Refused`
    /// - All `Alive` → `Alive`
    /// - Any `Unknown` → `Unknown`
    /// - Otherwise → `PartialAlive`
    #[must_use]
    pub fn compute_status(gates: &[GateResult]) -> AdmissionStatus {
        if gates.iter().any(|g| g.status == AdmissionStatus::Refused) {
            return AdmissionStatus::Refused;
        }
        if gates.iter().all(|g| g.status == AdmissionStatus::Alive) {
            return AdmissionStatus::Alive;
        }
        if gates.iter().any(|g| g.status == AdmissionStatus::Unknown) {
            return AdmissionStatus::Unknown;
        }
        AdmissionStatus::PartialAlive
    }

    /// Compute a BLAKE3 hex receipt hash over gate results, operation_id, graph_id, and timestamp.
    /// Does NOT include the `receipt_hash` field itself (avoids self-reference).
    pub fn compute_receipt_hash(&self) -> String {
        let json = serde_json::json!({
            "operation_id": self.operation_id,
            "graph_id": self.graph_id,
            "status": self.status,
            "gates": self.gates,
            "timestamp": self.timestamp.to_rfc3339(),
        })
        .to_string();
        let mut h = blake3::Hasher::new();
        h.update(json.as_bytes());
        h.finalize().to_hex().to_string()
    }

    /// Return a clone of `self` with `receipt_hash` populated from [`compute_receipt_hash`].
    #[must_use]
    pub fn with_receipt_hash(mut self) -> Self {
        let hash = self.compute_receipt_hash();
        self.receipt_hash = Some(hash);
        self
    }

    /// Serialize to a JSON string.
    pub fn to_json(&self) -> Result<String> {
        Ok(serde_json::to_string(self)?)
    }

    /// Deserialize from a JSON string.
    pub fn from_json(json: &str) -> Result<Self> {
        Ok(serde_json::from_str(json)?)
    }

    /// Write JSON representation to a file at `path`.
    pub fn to_json_file(&self, path: &std::path::Path) -> Result<()> {
        let json = self.to_json()?;
        std::fs::write(path, json).map_err(|e| Error::Other(e.to_string()))
    }

    /// Read and deserialize from a JSON file at `path`.
    pub fn from_json_file(path: &std::path::Path) -> Result<Self> {
        let content = std::fs::read_to_string(path).map_err(|e| Error::Other(e.to_string()))?;
        Self::from_json(&content)
    }
}

#[cfg(test)]
// Tests use unwrap() for clear failure messages; panics are intentional in test context.
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_execution_id_generate() {
        let id1 = ExecutionId::generate();
        let id2 = ExecutionId::generate();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_execution_context_new() {
        let ctx = ExecutionContext::new("test-workflow".to_string());
        assert_eq!(ctx.state, ExecutionState::Pending);
        assert!(ctx.current_step.is_none());
    }

    #[test]
    fn test_event_serialization() {
        let exec_id = ExecutionId::generate();
        let event = Event::new(
            exec_id,
            "test_event".to_string(),
            serde_json::json!({"key": "value"}),
        );

        let json = serde_json::to_string(&event).unwrap();
        let deserialized: Event = serde_json::from_str(&json).unwrap();

        assert_eq!(event.event_type, deserialized.event_type);
    }

    // ── POWL tests ────────────────────────────────────────────────────────────

    #[test]
    fn test_powl_graph_validate_valid() {
        let graph = PowlGraph {
            id: "test-graph".to_string(),
            name: "VisionSnapLoop".to_string(),
            nodes: vec![
                PowlNode {
                    id: "gen".to_string(),
                    activity: "GenerateGeometry".to_string(),
                    object_refs: vec![],
                    guard: None,
                },
                PowlNode {
                    id: "render".to_string(),
                    activity: "RenderProjection".to_string(),
                    object_refs: vec![],
                    guard: None,
                },
            ],
            edges: vec![PowlEdge {
                from: "gen".to_string(),
                to: "render".to_string(),
                condition: None,
            }],
            root: "gen".to_string(),
            sinks: vec!["render".to_string()],
        };
        assert!(graph.validate().is_empty());
    }

    #[test]
    fn test_powl_graph_validate_bad_root() {
        let graph = PowlGraph {
            id: "g".to_string(),
            name: "g".to_string(),
            nodes: vec![PowlNode {
                id: "a".to_string(),
                activity: "A".to_string(),
                object_refs: vec![],
                guard: None,
            }],
            edges: vec![],
            root: "nonexistent".to_string(),
            sinks: vec![],
        };
        let violations = graph.validate();
        assert!(!violations.is_empty());
        assert!(violations[0].contains("nonexistent"));
    }

    #[test]
    fn test_powl_graph_validate_bad_edge() {
        let graph = PowlGraph {
            id: "g".to_string(),
            name: "g".to_string(),
            nodes: vec![PowlNode {
                id: "a".to_string(),
                activity: "A".to_string(),
                object_refs: vec![],
                guard: None,
            }],
            edges: vec![PowlEdge {
                from: "a".to_string(),
                to: "missing".to_string(),
                condition: None,
            }],
            root: "a".to_string(),
            sinks: vec![],
        };
        let violations = graph.validate();
        assert!(!violations.is_empty());
        assert!(violations.iter().any(|v| v.contains("missing")));
    }

    #[test]
    fn test_powl_graph_validate_duplicate_node_ids() {
        let graph = PowlGraph {
            id: "g".to_string(),
            name: "g".to_string(),
            nodes: vec![
                PowlNode {
                    id: "dup".to_string(),
                    activity: "A".to_string(),
                    object_refs: vec![],
                    guard: None,
                },
                PowlNode {
                    id: "dup".to_string(),
                    activity: "B".to_string(),
                    object_refs: vec![],
                    guard: None,
                },
            ],
            edges: vec![],
            root: "dup".to_string(),
            sinks: vec![],
        };
        let violations = graph.validate();
        assert!(violations.iter().any(|v| v.contains("duplicate")));
    }

    #[test]
    fn test_admission_status_compute() {
        let all_alive = vec![GateResult {
            gate_id: "g1".to_string(),
            gate_name: "G1".to_string(),
            status: AdmissionStatus::Alive,
            detail: "ok".to_string(),
            evidence_hash: None,
        }];
        assert_eq!(
            ProcessAdmissionReport::compute_status(&all_alive),
            AdmissionStatus::Alive
        );

        let mixed = vec![
            GateResult {
                gate_id: "g1".to_string(),
                gate_name: "G1".to_string(),
                status: AdmissionStatus::Alive,
                detail: "ok".to_string(),
                evidence_hash: None,
            },
            GateResult {
                gate_id: "g2".to_string(),
                gate_name: "G2".to_string(),
                status: AdmissionStatus::PartialAlive,
                detail: "partial".to_string(),
                evidence_hash: None,
            },
        ];
        assert_eq!(
            ProcessAdmissionReport::compute_status(&mixed),
            AdmissionStatus::PartialAlive
        );
    }

    #[test]
    fn test_admission_status_compute_refused_dominates() {
        let gates = vec![
            GateResult {
                gate_id: "g1".to_string(),
                gate_name: "G1".to_string(),
                status: AdmissionStatus::Alive,
                detail: "ok".to_string(),
                evidence_hash: None,
            },
            GateResult {
                gate_id: "g2".to_string(),
                gate_name: "G2".to_string(),
                status: AdmissionStatus::Refused,
                detail: "blocked".to_string(),
                evidence_hash: None,
            },
        ];
        assert_eq!(
            ProcessAdmissionReport::compute_status(&gates),
            AdmissionStatus::Refused
        );
    }

    #[test]
    fn test_admission_status_compute_unknown() {
        let gates = vec![
            GateResult {
                gate_id: "g1".to_string(),
                gate_name: "G1".to_string(),
                status: AdmissionStatus::PartialAlive,
                detail: "partial".to_string(),
                evidence_hash: None,
            },
            GateResult {
                gate_id: "g2".to_string(),
                gate_name: "G2".to_string(),
                status: AdmissionStatus::Unknown,
                detail: "no evidence".to_string(),
                evidence_hash: None,
            },
        ];
        assert_eq!(
            ProcessAdmissionReport::compute_status(&gates),
            AdmissionStatus::Unknown
        );
    }

    #[test]
    fn test_admission_status_methods() {
        assert!(AdmissionStatus::Alive.is_admitted());
        assert!(!AdmissionStatus::PartialAlive.is_admitted());
        assert!(!AdmissionStatus::Refused.is_admitted());
        assert!(!AdmissionStatus::Unknown.is_admitted());

        assert!(AdmissionStatus::PartialAlive.should_continue_repair());
        assert!(AdmissionStatus::Unknown.should_continue_repair());
        assert!(!AdmissionStatus::Alive.should_continue_repair());
        assert!(!AdmissionStatus::Refused.should_continue_repair());
    }

    #[test]
    fn test_powl_graph_roundtrip_json() {
        let graph = PowlGraph {
            id: "loop-1".to_string(),
            name: "VisionSnapLoop".to_string(),
            nodes: vec![
                PowlNode {
                    id: "gen".to_string(),
                    activity: "GenerateGeometry".to_string(),
                    object_refs: vec!["artifact-42".to_string()],
                    guard: None,
                },
                PowlNode {
                    id: "measure".to_string(),
                    activity: "MeasureResidual".to_string(),
                    object_refs: vec![],
                    guard: Some("residual > 0.1".to_string()),
                },
            ],
            edges: vec![PowlEdge {
                from: "gen".to_string(),
                to: "measure".to_string(),
                condition: None,
            }],
            root: "gen".to_string(),
            sinks: vec!["measure".to_string()],
        };

        let json = serde_json::to_string(&graph).unwrap();
        let restored: PowlGraph = serde_json::from_str(&json).unwrap();

        assert_eq!(graph.id, restored.id);
        assert_eq!(graph.nodes.len(), restored.nodes.len());
        assert_eq!(graph.edges.len(), restored.edges.len());
        assert!(restored.validate().is_empty());
    }

    #[test]
    fn test_gate_result_roundtrip_json() {
        let gate = GateResult {
            gate_id: "admission-gate-1".to_string(),
            gate_name: "ConformanceCheck".to_string(),
            status: AdmissionStatus::PartialAlive,
            detail: "2 of 3 patterns matched".to_string(),
            evidence_hash: Some("abc123def456".to_string()),
        };

        let json = serde_json::to_string(&gate).unwrap();
        let restored: GateResult = serde_json::from_str(&json).unwrap();

        assert_eq!(gate.gate_id, restored.gate_id);
        assert_eq!(gate.status, restored.status);
        assert_eq!(gate.evidence_hash, restored.evidence_hash);
    }

    #[test]
    fn test_error_variants_powl() {
        let e1 = Error::PowlValidation("root not found".to_string());
        let e2 = Error::AdmissionRefused("gate G3 blocked".to_string());

        assert!(e1.to_string().contains("POWL validation error"));
        assert!(e1.to_string().contains("root not found"));
        assert!(e2.to_string().contains("Process admission refused"));
        assert!(e2.to_string().contains("gate G3 blocked"));
    }
}

// ── Residual-Vector Repair Types (Post-Chatman Feature 4) ─────────────────

/// Classification of how well a dimensional value is known.
/// Used to bound repair operators: only operators consistent with the tier are admissible.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EvidenceTier {
    /// Value is directly measured from a primary source (e.g., physical scan, spec sheet).
    Known,
    /// Value is derived from known values via geometry or physics.
    Inferred,
    /// Value is approximated from similar classes (e.g., similar mech archetypes).
    Estimated,
    /// Value must never be used — violates IP, safety, or identity constraints.
    Forbidden,
    /// Value is explicitly outside normal bands for a documented reason.
    ExceptionClass,
}

/// Bounds for a single measurable dimension.
/// Repair operators must keep the repaired value within `preferred_band`
/// and must never produce a value in `forbidden_band`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepairBand {
    /// (min, max) acceptable range.
    pub default_band: (f64, f64),
    /// (min, max) target range for optimal quality.
    pub preferred_band: (f64, f64),
    /// (min, max) range that must never be produced.
    pub forbidden_band: (f64, f64),
    /// How well this band is evidenced.
    pub tier: EvidenceTier,
    /// Unit label (e.g., "meters", "ratio", "count").
    pub unit: String,
}

impl RepairBand {
    /// Returns true if `value` is within the forbidden band (exclusive bounds).
    pub fn is_forbidden(&self, value: f64) -> bool {
        value > self.forbidden_band.0 && value < self.forbidden_band.1
    }

    /// Returns true if `value` is within the preferred band (inclusive bounds).
    pub fn is_preferred(&self, value: f64) -> bool {
        value >= self.preferred_band.0 && value <= self.preferred_band.1
    }
}

/// Measured gap for a single dimension between current state and target.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResidualDimension {
    /// Dimension name (e.g., "foreground_component_count", "silhouette_iou").
    pub name: String,
    /// Measured value from the most recent fresh render/report.
    pub measured: f64,
    /// Target range (min, max). Residual is 0 when measured falls within this range.
    pub target: (f64, f64),
    /// Signed residual: measured - midpoint(target). Negative = below target, positive = above.
    pub residual: f64,
}

impl ResidualDimension {
    /// Construct a ResidualDimension, computing residual automatically.
    pub fn new(name: impl Into<String>, measured: f64, target: (f64, f64)) -> Self {
        let midpoint = (target.0 + target.1) / 2.0;
        let residual = measured - midpoint;
        Self {
            name: name.into(),
            measured,
            target,
            residual,
        }
    }

    /// Returns true when measured is within the target range (passing).
    pub fn is_passing(&self) -> bool {
        self.measured >= self.target.0 && self.measured <= self.target.1
    }
}

/// Collection of residual dimensions for a single render evaluation.
/// The dominant dimension is the one with the largest absolute residual.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResidualVector {
    pub dimensions: Vec<ResidualDimension>,
    /// Name of the dimension with the largest |residual|. None if dimensions is empty.
    pub dominant: Option<String>,
}

impl ResidualVector {
    /// Construct from dimensions, computing the dominant dimension automatically.
    pub fn new(dimensions: Vec<ResidualDimension>) -> Self {
        let dominant = dimensions
            .iter()
            .max_by(|a, b| {
                a.residual
                    .abs()
                    .partial_cmp(&b.residual.abs())
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
            .map(|d| d.name.clone());
        Self {
            dimensions,
            dominant,
        }
    }

    /// Returns true when all dimensions are passing.
    pub fn all_passing(&self) -> bool {
        self.dimensions.iter().all(|d| d.is_passing())
    }
}

/// A repair operator that is bounded by prior knowledge.
/// An operator is only admissible when the target dimension's residual exceeds a threshold
/// AND the proposed repair keeps the result within the `band`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BoundedRepairOperator {
    /// Unique operator identifier.
    pub id: String,
    /// The dimension this operator targets.
    pub targets_dimension: String,
    /// Bounds within which the operator must keep the result.
    pub band: RepairBand,
    /// Human-readable description of what the operator does.
    pub description: String,
    /// Whether this operator modifies source law (true) or only geometry heuristics (false).
    pub modifies_source_law: bool,
}

/// Result of a single fresh-render visual comparison run.
/// Must be derived from a fresh render (stale PNGs are invalid input).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisualGapReport {
    /// BLAKE3 hash of the render image that was scored.
    pub render_hash: String,
    /// RFC-3339 timestamp of when the render was produced.
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// All residuals computed from this render.
    pub residuals: ResidualVector,
    /// Whether this report comes from a fresh render (set by the pipeline, not the scorer).
    pub is_fresh_render: bool,
}

impl VisualGapReport {
    /// Returns Err if `is_fresh_render` is false — stale reports must be rejected.
    pub fn assert_fresh(&self) -> Result<()> {
        if self.is_fresh_render {
            Ok(())
        } else {
            Err(Error::InvalidInput(
                "VisualGapReport was produced from a stale render; delete and re-render"
                    .to_string(),
            ))
        }
    }

    /// Serialize to a JSON string.
    pub fn to_json(&self) -> Result<String> {
        Ok(serde_json::to_string(self)?)
    }

    /// Deserialize from a JSON string.
    pub fn from_json(json: &str) -> Result<Self> {
        Ok(serde_json::from_str(json)?)
    }

    /// Write JSON representation to a file at `path`.
    pub fn to_json_file(&self, path: &std::path::Path) -> Result<()> {
        let json = self.to_json()?;
        std::fs::write(path, json).map_err(|e| Error::Other(e.to_string()))
    }

    /// Read and deserialize from a JSON file at `path`.
    pub fn from_json_file(path: &std::path::Path) -> Result<Self> {
        let content = std::fs::read_to_string(path).map_err(|e| Error::Other(e.to_string()))?;
        Self::from_json(&content)
    }
}

/// Evidence that a repair operator was applied and produced measurable improvement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepairAdmissionReport {
    /// ID of the operator that was applied.
    pub operator_id: String,
    /// Residual state before the repair.
    pub before: ResidualVector,
    /// Residual state after the repair (from a fresh render).
    pub after: ResidualVector,
    /// True only when at least one dimension improved and none worsened beyond band.
    pub admitted: bool,
    /// Explanation of admission decision.
    pub detail: String,
}

impl RepairAdmissionReport {
    /// Compute admission: admitted if after has fewer failing dimensions than before,
    /// or if all dimensions are now passing.
    pub fn compute(
        operator_id: impl Into<String>,
        before: ResidualVector,
        after: ResidualVector,
    ) -> Self {
        let before_failing = before.dimensions.iter().filter(|d| !d.is_passing()).count();
        let after_failing = after.dimensions.iter().filter(|d| !d.is_passing()).count();

        let admitted = after.all_passing() || after_failing < before_failing;
        let detail = if after.all_passing() {
            "All dimensions are now passing after repair.".to_string()
        } else if after_failing < before_failing {
            format!(
                "Repair reduced failing dimensions from {} to {}.",
                before_failing, after_failing
            )
        } else {
            format!(
                "Repair did not improve: {} failing before, {} failing after.",
                before_failing, after_failing
            )
        };

        Self {
            operator_id: operator_id.into(),
            before,
            after,
            admitted,
            detail,
        }
    }
}

#[cfg(test)]
// Tests use unwrap() for clear failure messages; panics are intentional in test context.
#[allow(clippy::unwrap_used)]
mod residual_vector_tests {
    use super::*;

    #[test]
    fn test_residual_dimension_passing() {
        let d = ResidualDimension::new("silhouette_iou", 0.30, (0.25, 1.0));
        assert!(d.is_passing());
        assert!(d.residual < 0.5); // measured is below midpoint 0.625
    }

    #[test]
    fn test_residual_dimension_failing() {
        let d = ResidualDimension::new("foreground_component_count", 22.0, (1.0, 5.0));
        assert!(!d.is_passing());
        // residual = 22 - 3 = +19
        assert!(d.residual > 0.0);
    }

    #[test]
    fn test_residual_vector_dominant() {
        let dims = vec![
            ResidualDimension::new("a", 10.0, (5.0, 15.0)), // residual = 0
            ResidualDimension::new("b", 22.0, (1.0, 5.0)),  // residual = +19
        ];
        let vec = ResidualVector::new(dims);
        assert_eq!(vec.dominant.as_deref(), Some("b"));
    }

    #[test]
    fn test_repair_band_forbidden() {
        let band = RepairBand {
            default_band: (1.0, 20.0),
            preferred_band: (3.0, 10.0),
            forbidden_band: (0.0, 0.5),
            tier: EvidenceTier::Known,
            unit: "count".to_string(),
        };
        assert!(band.is_forbidden(0.3));
        assert!(!band.is_forbidden(5.0));
    }

    #[test]
    fn test_visual_gap_report_stale_rejected() {
        let report = VisualGapReport {
            render_hash: "abc123".to_string(),
            timestamp: chrono::Utc::now(),
            residuals: ResidualVector::new(vec![]),
            is_fresh_render: false,
        };
        assert!(report.assert_fresh().is_err());
    }

    #[test]
    fn test_repair_admission_admitted() {
        let before = ResidualVector::new(vec![ResidualDimension::new(
            "count",
            22.0,
            (1.0, 5.0),
        )]);
        let after = ResidualVector::new(vec![ResidualDimension::new(
            "count",
            4.0,
            (1.0, 5.0),
        )]);
        let report = RepairAdmissionReport::compute("op-001", before, after);
        assert!(report.admitted);
    }

    #[test]
    fn test_repair_admission_not_admitted_when_no_improvement() {
        let before = ResidualVector::new(vec![ResidualDimension::new(
            "count",
            22.0,
            (1.0, 5.0),
        )]);
        let after = ResidualVector::new(vec![ResidualDimension::new(
            "count",
            20.0,
            (1.0, 5.0),
        )]);
        // Both still failing, same count of failures
        let report = RepairAdmissionReport::compute("op-002", before, after);
        assert!(!report.admitted);
    }

    #[test]
    fn test_residual_vector_all_passing() {
        let dims = vec![
            ResidualDimension::new("a", 5.0, (1.0, 10.0)),
            ResidualDimension::new("b", 0.5, (0.0, 1.0)),
        ];
        let vec = ResidualVector::new(dims);
        assert!(vec.all_passing());
    }

    #[test]
    fn test_residual_vector_empty_has_no_dominant() {
        let vec = ResidualVector::new(vec![]);
        assert!(vec.dominant.is_none());
    }

    #[test]
    fn test_evidence_tier_serialization() {
        let tier = EvidenceTier::Known;
        let json = serde_json::to_string(&tier).unwrap();
        let back: EvidenceTier = serde_json::from_str(&json).unwrap();
        assert_eq!(tier, back);
    }

    #[test]
    fn test_repair_band_preferred() {
        let band = RepairBand {
            default_band: (1.0, 20.0),
            preferred_band: (3.0, 10.0),
            forbidden_band: (0.0, 0.5),
            tier: EvidenceTier::Inferred,
            unit: "ratio".to_string(),
        };
        assert!(band.is_preferred(5.0));
        assert!(!band.is_preferred(0.3));
        assert!(!band.is_preferred(15.0));
    }

    #[test]
    fn test_visual_gap_report_fresh_accepted() {
        let report = VisualGapReport {
            render_hash: "deadbeef".to_string(),
            timestamp: chrono::Utc::now(),
            residuals: ResidualVector::new(vec![]),
            is_fresh_render: true,
        };
        assert!(report.assert_fresh().is_ok());
    }
}
