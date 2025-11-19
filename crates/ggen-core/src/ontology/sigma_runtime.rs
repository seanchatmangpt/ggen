use ggen_utils::error::{Error, Result};
use oxigraph::model::{Dataset, GraphName, NamedNode, Quad};
/// Σ (Sigma) Runtime: Autonomous ontology versioning and snapshot management
///
/// This module implements the immutable snapshot system that allows ontologies to change
/// at hardware speed through atomic pointer swaps, while maintaining hard invariants (Q).
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::SystemTime;

// In oxigraph 0.5.1, Statement is replaced with Quad
// We'll define a custom serializable wrapper around Quad
//
// **Limitation**: Statement only handles NamedNode objects, not Literals or BlankNodes.
// The `object` field is a String that must be a valid IRI when converting back to Quad.
// RDF objects that are Literals or BlankNodes will cause `as_dataset()` to fail with
// an "Invalid object IRI" error. This is intentional for the ontology use case where
// all objects are expected to be NamedNodes (class/property IRIs).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Statement {
    pub subject: String,
    pub predicate: String,
    pub object: String,
    pub graph: Option<String>,
}

impl From<oxigraph::model::Quad> for Statement {
    fn from(quad: oxigraph::model::Quad) -> Self {
        // Strip angle brackets from IRIs for cleaner storage
        fn strip_brackets(s: &str) -> String {
            if s.starts_with('<') && s.ends_with('>') {
                s[1..s.len()-1].to_string()
            } else {
                s.to_string()
            }
        }

        let graph = match &quad.graph_name {
            oxigraph::model::GraphName::DefaultGraph => None,
            oxigraph::model::GraphName::NamedNode(nn) => Some(strip_brackets(&nn.to_string())),
            oxigraph::model::GraphName::BlankNode(bn) => Some(bn.to_string()),
        };
        Self {
            subject: strip_brackets(&quad.subject.to_string()),
            predicate: strip_brackets(&quad.predicate.to_string()),
            object: strip_brackets(&quad.object.to_string()),
            graph,
        }
    }
}

// Custom serialization for Arc<Vec<Statement>>
// Arc doesn't implement Serialize/Deserialize by default, so we serialize the inner Vec
mod arc_vec_serde {
    use super::*;
    use serde::{Deserializer, Serializer};

    pub fn serialize<S>(
        arc: &Arc<Vec<Statement>>, serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::Serialize;
        (*arc).serialize(serializer)
    }

    pub fn deserialize<'de, D>(
        deserializer: D,
    ) -> std::result::Result<Arc<Vec<Statement>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde::Deserialize;
        Vec::<Statement>::deserialize(deserializer).map(Arc::new)
    }
}

/// 128-bit hash-based identifier for a Σ snapshot
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord, Default)]
pub struct SigmaSnapshotId(String);

impl SigmaSnapshotId {
    /// Create a new snapshot ID from a digest of the ontology triples
    pub fn from_digest(data: &[u8]) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(data);
        let result = hasher.finalize();
        Self(format!("{:x}", result))
    }

    /// Get the string representation of the snapshot ID
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for SigmaSnapshotId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Immutable snapshot of the ontology at a point in time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SigmaSnapshot {
    /// Unique identifier (SHA-256 hash of triples)
    pub id: SigmaSnapshotId,

    /// Parent snapshot ID (enables snapshot chains)
    pub parent_id: Option<SigmaSnapshotId>,

    /// RDF triples (immutable store)
    #[serde(with = "arc_vec_serde")]
    pub triples: Arc<Vec<Statement>>,

    /// Semantic version of this snapshot
    pub version: String,

    /// Creation timestamp
    pub timestamp: SystemTime,

    /// Cryptographic signature (ML-DSA or similar)
    pub signature: String,

    /// Metadata about the snapshot
    pub metadata: SnapshotMetadata,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SnapshotMetadata {
    /// Backward compatibility flag
    pub backward_compatible: bool,

    /// Description of changes in this snapshot
    pub description: String,

    /// Sectors affected by this snapshot
    pub sectors: Vec<String>,

    /// Custom metadata tags
    pub tags: BTreeMap<String, String>,
}

impl SigmaSnapshot {
    /// Create a new snapshot
    pub fn new(
        parent_id: Option<SigmaSnapshotId>, triples: Vec<Statement>, version: String,
        signature: String, metadata: SnapshotMetadata,
    ) -> Self {
        let serialized = serde_json::to_vec(&triples).unwrap_or_default();
        let id = SigmaSnapshotId::from_digest(&serialized);

        Self {
            id,
            parent_id,
            triples: Arc::new(triples),
            version,
            timestamp: SystemTime::now(),
            signature,
            metadata,
        }
    }

    /// Get the snapshot as an RDF dataset
    pub fn as_dataset(&self) -> Result<Dataset> {
        let mut dataset = Dataset::default();
        for stmt in self.triples.iter() {
            let subject = NamedNode::new(&stmt.subject)
                .map_err(|e| Error::new(&format!("Invalid subject IRI: {}", e)))?;
            let predicate = NamedNode::new(&stmt.predicate)
                .map_err(|e| Error::new(&format!("Invalid predicate IRI: {}", e)))?;
            let object = NamedNode::new(&stmt.object)
                .map_err(|e| Error::new(&format!("Invalid object IRI: {}", e)))?;
            let graph_name = if let Some(graph) = &stmt.graph {
                GraphName::NamedNode(
                    NamedNode::new(graph)
                        .map_err(|e| Error::new(&format!("Invalid graph IRI: {}", e)))?,
                )
            } else {
                GraphName::DefaultGraph
            };
            let quad = Quad::new(subject, predicate, object, graph_name);
            dataset.insert(&quad);
        }
        Ok(dataset)
    }
}

/// A diff/overlay atop a base snapshot (used for experiments and staged rollouts)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SigmaOverlay {
    /// Base snapshot this overlay is built on
    pub base_id: SigmaSnapshotId,

    /// Triples to add
    pub additions: Vec<Statement>,

    /// Triples to remove (by RDF triple pattern)
    pub removals: Vec<(Option<String>, Option<String>, Option<String>)>,

    /// Overlay ID
    pub id: String,

    /// Description
    pub description: String,
}

impl SigmaOverlay {
    /// Create a new overlay
    pub fn new(
        base_id: SigmaSnapshotId, additions: Vec<Statement>,
        removals: Vec<(Option<String>, Option<String>, Option<String>)>, description: String,
    ) -> Self {
        let id = format!("{}_overlay_{}", base_id, uuid::Uuid::new_v4());
        Self {
            base_id,
            additions,
            removals,
            id,
            description,
        }
    }

    /// Apply this overlay to a snapshot, creating a new composite snapshot
    pub fn apply_to(&self, base: &SigmaSnapshot) -> SigmaSnapshot {
        let mut new_triples = (*base.triples).clone();

        // Add new triples
        new_triples.extend(self.additions.clone());

        // Remove triples (simple pattern matching)
        new_triples.retain(|stmt| {
            for (s, p, o) in &self.removals {
                let subj_match = s.is_none() || Some(stmt.subject.to_string()) == *s;
                let pred_match = p.is_none() || Some(stmt.predicate.to_string()) == *p;
                let obj_match = o.is_none() || Some(stmt.object.to_string()) == *o;

                if subj_match && pred_match && obj_match {
                    return false; // Remove this triple
                }
            }
            true // Keep this triple
        });

        let serialized = serde_json::to_vec(&new_triples).unwrap_or_default();
        let new_id = SigmaSnapshotId::from_digest(&serialized);

        SigmaSnapshot {
            id: new_id,
            parent_id: Some(base.id.clone()),
            triples: Arc::new(new_triples),
            version: format!("{}_overlay", base.version),
            timestamp: SystemTime::now(),
            signature: format!("{}_unsigned", base.signature),
            metadata: base.metadata.clone(),
        }
    }
}

/// Validation proof and audit trail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SigmaReceipt {
    /// ID of the snapshot being validated
    pub snapshot_id: SigmaSnapshotId,

    /// ID of the parent snapshot
    pub parent_snapshot_id: Option<SigmaSnapshotId>,

    /// Description of changes
    pub delta_description: String,

    /// Validation result
    pub result: ValidationResult,

    /// Whether hard invariants (Q) are preserved
    pub invariants_preserved: bool,

    /// Which invariants were checked
    pub invariants_checked: Vec<String>,

    /// Which tests passed/failed
    pub test_results: BTreeMap<String, TestResult>,

    /// Performance metrics
    pub performance_metrics: PerformanceMetrics,

    /// Cryptographic signature
    pub signature: String,

    /// Timestamp of validation
    pub timestamp: SystemTime,

    /// Optional error message if validation failed
    pub error_message: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ValidationResult {
    Valid,
    Invalid,
    Pending,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestResult {
    pub name: String,
    pub passed: bool,
    pub duration_ms: u64,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    /// Memory footprint in bytes
    pub memory_bytes: u64,

    /// Operator path latency in microseconds
    pub operator_latency_us: u64,

    /// Whether SLOs are met
    pub slos_met: bool,

    /// Custom metrics
    pub custom: BTreeMap<String, f64>,
}

impl SigmaReceipt {
    /// Create a new receipt
    pub fn new(
        snapshot_id: SigmaSnapshotId, parent_snapshot_id: Option<SigmaSnapshotId>,
        delta_description: String,
    ) -> Self {
        Self {
            snapshot_id,
            parent_snapshot_id,
            delta_description,
            result: ValidationResult::Pending,
            invariants_preserved: false,
            invariants_checked: Vec::new(),
            test_results: BTreeMap::new(),
            performance_metrics: PerformanceMetrics::default(),
            signature: String::new(),
            timestamp: SystemTime::now(),
            error_message: None,
        }
    }

    /// Mark this receipt as valid with all checks passed
    pub fn mark_valid(mut self) -> Self {
        self.result = ValidationResult::Valid;
        self.invariants_preserved = true;
        self
    }

    /// Mark this receipt as invalid
    pub fn mark_invalid(mut self, reason: String) -> Self {
        self.result = ValidationResult::Invalid;
        self.error_message = Some(reason);
        self
    }

    /// Sign the receipt with a cryptographic signature
    pub fn sign(&mut self, signature: String) {
        self.signature = signature;
    }
}

/// Σ Runtime: Manages snapshots, overlays, and promotion
pub struct SigmaRuntime {
    /// All known snapshots (BTreeMap for deterministic iteration order)
    snapshots: BTreeMap<SigmaSnapshotId, Arc<SigmaSnapshot>>,

    /// Currently active snapshot for projections
    current_snapshot: Arc<SigmaSnapshotId>,

    /// All overlays (BTreeMap for deterministic iteration order)
    #[allow(dead_code)] // Reserved for overlay functionality (not yet implemented)
    overlays: BTreeMap<String, SigmaOverlay>,

    /// Append-only receipt log
    receipt_log: Vec<Arc<SigmaReceipt>>,
}

impl SigmaRuntime {
    /// Create a new runtime with an initial snapshot
    pub fn new(initial_snapshot: SigmaSnapshot) -> Self {
        let snapshot_id = initial_snapshot.id.clone();
        let mut snapshots = BTreeMap::new();
        snapshots.insert(snapshot_id.clone(), Arc::new(initial_snapshot));

        Self {
            snapshots,
            current_snapshot: Arc::new(snapshot_id),
            overlays: BTreeMap::new(),
            receipt_log: Vec::new(),
        }
    }

    /// Get the current active snapshot
    pub fn current_snapshot(&self) -> Option<Arc<SigmaSnapshot>> {
        self.snapshots.get(self.current_snapshot.as_ref()).cloned()
    }

    /// Retrieve a snapshot by ID
    pub fn get_snapshot(&self, id: &SigmaSnapshotId) -> Option<Arc<SigmaSnapshot>> {
        self.snapshots.get(id).cloned()
    }

    /// Store a new snapshot
    pub fn store_snapshot(&mut self, snapshot: SigmaSnapshot) {
        self.snapshots
            .insert(snapshot.id.clone(), Arc::new(snapshot));
    }

    /// Apply an overlay to a snapshot, creating a new composite
    pub fn apply_overlay(
        &mut self, base_id: &SigmaSnapshotId, overlay: SigmaOverlay,
    ) -> Result<SigmaSnapshot> {
        let base = self
            .get_snapshot(base_id)
            .ok_or_else(|| Error::new("Base snapshot not found"))?;

        let new_snapshot = overlay.apply_to(&base);
        Ok(new_snapshot)
    }

    /// Validate and promote a snapshot to be the current one (atomic operation)
    /// In a real system, this would be synchronized with a lock and use atomic ptr swap
    pub fn promote_snapshot(&mut self, id: &SigmaSnapshotId) -> Result<()> {
        if !self.snapshots.contains_key(id) {
            return Err(Error::new(&format!("Snapshot {} not found", id)));
        }

        // Atomic pointer swap (in real system: ATOMIC_PTR.store(...))
        self.current_snapshot = Arc::new(id.clone());
        Ok(())
    }

    /// Record a validation receipt
    pub fn record_receipt(&mut self, receipt: SigmaReceipt) {
        self.receipt_log.push(Arc::new(receipt));
    }

    /// Get all receipts for a snapshot
    pub fn get_receipts_for(&self, snapshot_id: &SigmaSnapshotId) -> Vec<Arc<SigmaReceipt>> {
        self.receipt_log
            .iter()
            .filter(|r| r.snapshot_id == *snapshot_id)
            .cloned()
            .collect()
    }

    /// Get the entire receipt log
    pub fn receipt_log(&self) -> &[Arc<SigmaReceipt>] {
        &self.receipt_log
    }

    /// Get snapshot count
    pub fn snapshot_count(&self) -> usize {
        self.snapshots.len()
    }

    /// List all snapshot IDs
    pub fn list_snapshots(&self) -> Vec<SigmaSnapshotId> {
        self.snapshots.keys().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_snapshot() -> SigmaSnapshot {
        let metadata = SnapshotMetadata {
            backward_compatible: true,
            description: "Test snapshot".to_string(),
            sectors: vec!["test".to_string()],
            tags: BTreeMap::new(),
        };

        SigmaSnapshot::new(
            None,
            vec![],
            "0.1.0".to_string(),
            "test_signature".to_string(),
            metadata,
        )
    }

    #[test]
    fn test_snapshot_creation() {
        let snap = create_test_snapshot();
        assert_eq!(snap.version, "0.1.0");
        assert!(snap.parent_id.is_none());
        assert!(!snap.id.0.is_empty());
    }

    #[test]
    fn test_snapshot_id_determinism() {
        let snap1 = create_test_snapshot();
        let snap2 = create_test_snapshot();

        // Same data should produce same ID
        assert_eq!(snap1.id, snap2.id);
    }

    #[test]
    fn test_runtime_creation() {
        let snap = create_test_snapshot();
        let runtime = SigmaRuntime::new(snap.clone());

        assert_eq!(runtime.snapshot_count(), 1);
        assert!(runtime.current_snapshot().is_some());
        assert_eq!(runtime.current_snapshot().unwrap().id, snap.id);
    }

    #[test]
    fn test_snapshot_promotion() {
        let snap1 = create_test_snapshot();
        let snap1_id = snap1.id.clone();

        let mut metadata = snap1.metadata.clone();
        metadata.description = "Snapshot 2".to_string();
        let snap2 = SigmaSnapshot::new(
            Some(snap1_id.clone()),
            vec![],
            "0.2.0".to_string(),
            "signature2".to_string(),
            metadata,
        );
        let snap2_id = snap2.id.clone();

        let mut runtime = SigmaRuntime::new(snap1.clone());
        runtime.store_snapshot(snap2);

        assert_eq!(runtime.current_snapshot().unwrap().id, snap1_id);

        runtime.promote_snapshot(&snap2_id).unwrap();
        assert_eq!(runtime.current_snapshot().unwrap().id, snap2_id);
    }

    #[test]
    fn test_receipt_signing() {
        let snap_id = SigmaSnapshotId::from_digest(b"test");
        let mut receipt = SigmaReceipt::new(snap_id, None, "Test change".to_string());

        receipt.sign("ml_dsa_signature_here".to_string());
        assert_eq!(receipt.signature, "ml_dsa_signature_here");
    }

    #[test]
    fn test_receipt_log() {
        let snap = create_test_snapshot();
        let mut runtime = SigmaRuntime::new(snap.clone());

        let receipt = SigmaReceipt::new(snap.id.clone(), None, "Change".to_string());
        let receipt = receipt.mark_valid();

        runtime.record_receipt(receipt);
        assert_eq!(runtime.receipt_log().len(), 1);
    }

    #[test]
    fn test_as_dataset_empty() {
        let snap = create_test_snapshot();
        let dataset = snap.as_dataset().unwrap();
        assert_eq!(dataset.len(), 0);
    }

    #[test]
    fn test_as_dataset_with_triples() {
        use oxigraph::model::{GraphName, NamedNode, Quad};

        // Create a quad and convert to Statement
        let subject = NamedNode::new("http://example.org/subject").unwrap();
        let predicate = NamedNode::new("http://example.org/predicate").unwrap();
        let object = NamedNode::new("http://example.org/object").unwrap();
        let quad = Quad::new(subject, predicate, object, GraphName::DefaultGraph);
        let statement = Statement::from(quad.clone());

        let metadata = SnapshotMetadata {
            backward_compatible: true,
            description: "Test snapshot with triples".to_string(),
            sectors: vec!["test".to_string()],
            tags: BTreeMap::new(),
        };

        let snap = SigmaSnapshot::new(
            None,
            vec![statement],
            "0.1.0".to_string(),
            "test_signature".to_string(),
            metadata,
        );

        let dataset = snap.as_dataset().unwrap();
        assert_eq!(dataset.len(), 1);
    }

    #[test]
    fn test_statement_round_trip_default_graph() {
        use oxigraph::model::{GraphName, NamedNode, Quad};

        // Create quad with DefaultGraph
        let subject = NamedNode::new("http://example.org/subject").unwrap();
        let predicate = NamedNode::new("http://example.org/predicate").unwrap();
        let object = NamedNode::new("http://example.org/object").unwrap();
        let original_quad = Quad::new(
            subject.clone(),
            predicate.clone(),
            object.clone(),
            GraphName::DefaultGraph,
        );

        // Convert to Statement
        let statement = Statement::from(original_quad.clone());

        // Verify graph is None for DefaultGraph
        assert!(statement.graph.is_none());

        // Convert back via as_dataset
        let metadata = SnapshotMetadata {
            backward_compatible: true,
            description: "Test".to_string(),
            sectors: vec!["test".to_string()],
            tags: BTreeMap::new(),
        };
        let snap = SigmaSnapshot::new(
            None,
            vec![statement],
            "0.1.0".to_string(),
            "test_signature".to_string(),
            metadata,
        );

        let dataset = snap.as_dataset().unwrap();
        assert_eq!(dataset.len(), 1);
    }

    #[test]
    fn test_statement_round_trip_named_graph() {
        use oxigraph::model::{GraphName, NamedNode, Quad};

        // Create quad with NamedGraph
        let subject = NamedNode::new("http://example.org/subject").unwrap();
        let predicate = NamedNode::new("http://example.org/predicate").unwrap();
        let object = NamedNode::new("http://example.org/object").unwrap();
        let graph_name = NamedNode::new("http://example.org/graph").unwrap();
        let original_quad = Quad::new(
            subject.clone(),
            predicate.clone(),
            object.clone(),
            GraphName::NamedNode(graph_name.clone()),
        );

        // Convert to Statement
        let statement = Statement::from(original_quad.clone());

        // Verify graph is Some for NamedGraph
        assert!(statement.graph.is_some());
        assert_eq!(
            statement.graph.as_ref().unwrap(),
            "http://example.org/graph"
        );

        // Convert back via as_dataset
        let metadata = SnapshotMetadata {
            backward_compatible: true,
            description: "Test".to_string(),
            sectors: vec!["test".to_string()],
            tags: BTreeMap::new(),
        };
        let snap = SigmaSnapshot::new(
            None,
            vec![statement],
            "0.1.0".to_string(),
            "test_signature".to_string(),
            metadata,
        );

        let dataset = snap.as_dataset().unwrap();
        assert_eq!(dataset.len(), 1);
    }

    #[test]
    fn test_as_dataset_invalid_iri() {
        // Create Statement with invalid IRI
        let invalid_statement = Statement {
            subject: "not a valid IRI".to_string(),
            predicate: "http://example.org/predicate".to_string(),
            object: "http://example.org/object".to_string(),
            graph: None,
        };

        let metadata = SnapshotMetadata {
            backward_compatible: true,
            description: "Test".to_string(),
            sectors: vec!["test".to_string()],
            tags: BTreeMap::new(),
        };

        let snap = SigmaSnapshot::new(
            None,
            vec![invalid_statement],
            "0.1.0".to_string(),
            "test_signature".to_string(),
            metadata,
        );

        let result = snap.as_dataset();
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("Invalid subject IRI"));
    }
}
