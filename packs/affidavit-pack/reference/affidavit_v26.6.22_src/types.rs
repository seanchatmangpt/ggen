//! Shared interface contract for the Provenance Layer.
//!
//! Every other module codes against the types declared here. They encode the
//! doctrine: a receipt is an append-only, content-addressed chain of
//! operation-events, and the verifier checks a witness against a format
//! standard rather than deciding whether code is honest.
//!
//! Receipts flow through the type system as Evidence<Receipt, State, AffidavitReceiptChain>
//! per ARDPRD §4 (the court/producer seam with wasm4pm-compat).

use serde::{Deserialize, Deserializer, Serialize};
use wasm4pm_compat::evidence::Evidence;
use wasm4pm_compat::state::Admitted;

/// A BLAKE3 digest rendered as a lowercase hex string.
///
/// Stored as hex so receipts serialize to canonical, human-diffable JSON.
///
/// # Examples
///
/// ```rust
/// use affidavit::Blake3Hash;
/// let hash = Blake3Hash::from_bytes(b"hello world");
/// assert_eq!(hash.as_hex().len(), 64);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Blake3Hash(pub String);

impl Blake3Hash {
    /// Construct a hash from raw bytes by computing their BLAKE3 digest.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use affidavit::Blake3Hash;
    /// let hash = Blake3Hash::from_bytes(b"data");
    /// ```
    pub fn from_bytes(bytes: &[u8]) -> Self {
        Blake3Hash(blake3::hash(bytes).to_hex().to_string())
    }

    /// Construct a hash from an already-computed lowercase hex string.
    ///
    /// Used during deserialization round-trips where the digest is known.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use affidavit::Blake3Hash;
    /// let hash = Blake3Hash::from_hex("d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592");
    /// ```
    pub fn from_hex(hex: impl Into<String>) -> Self {
        Blake3Hash(hex.into())
    }

    /// Borrow the hex representation of this hash.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use affidavit::Blake3Hash;
    /// let hash = Blake3Hash::from_bytes(b"data");
    /// let hex = hash.as_hex();
    /// ```
    pub fn as_hex(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for Blake3Hash {
    /// Render the hash as its lowercase hex string.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// Witness marker for Affidavit receipts: structurally lawful (OCEL) + chain-sealed (BLAKE3).
/// Per ARDPR ADR-4: `Evidence<_, Admitted, AffidavitReceiptChain>` means the receipt passed
/// both wasm4pm-compat's OCEL admission and Affidavit's BLAKE3 chain-seal atomically (Layer 2 seam).
///
/// This is a zero-sized witness type used at the type level; it carries no runtime value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct AffidavitReceiptChain;

/// Zero-cost type alias for the admitted receipt carrier.
/// This is the type that exits Affidavit's output functions (Layer 3 gate per ARDPRD §4).
pub type AdmittedReceipt = Evidence<Receipt, Admitted, AffidavitReceiptChain>;

impl Receipt {
    /// Construct a Receipt with the canonical sealing. Used internally by
    /// [`crate::chain::ChainAssembler::finalize`]. External code cannot call
    /// this because `_seal` is a private field.
    pub(crate) fn sealed(
        format_version: String,
        events: Vec<OperationEvent>,
        chain_hash: Blake3Hash,
    ) -> Self {
        Receipt {
            format_version,
            events,
            chain_hash,
            _seal: (),
        }
    }
}

/// Custom deserialization that re-verifies the chain hash (ADR-3: non-forgeable carrier).
/// A Receipt deserialized from JSON is only valid if its chain_hash recomputes correctly
/// from the events. This closes the deserialization forgery door.
impl<'de> Deserialize<'de> for Receipt {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde::de::Error;

        #[derive(Deserialize)]
        struct RawReceipt {
            format_version: String,
            events: Vec<OperationEvent>,
            chain_hash: Blake3Hash,
        }

        let raw = RawReceipt::deserialize(deserializer)?;

        // Re-verify the chain hash. If it doesn't match, the receipt was forged or corrupted.
        let recomputed = crate::chain::recompute_chain(&raw.events)
            .map_err(|e| D::Error::custom(format!("chain recomputation failed: {e}")))?;

        if recomputed != raw.chain_hash {
            return Err(D::Error::custom(format!(
                "chain hash mismatch: receipt claims {}, recomputed {}",
                raw.chain_hash, recomputed
            )));
        }

        Ok(Receipt {
            format_version: raw.format_version,
            events: raw.events,
            chain_hash: raw.chain_hash,
            _seal: (),
        })
    }
}

/// A qualified reference from an operation-event to an OCEL object.
///
/// # Examples
///
/// ```rust
/// use affidavit::ObjectRef;
/// let obj = ObjectRef {
///     id: "artifact-1".to_string(),
///     obj_type: "artifact".to_string(),
///     qualifier: Some("input".to_string()),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ObjectRef {
    /// Stable identifier of the referenced object.
    pub id: String,
    /// OCEL object type (the class of the object).
    pub obj_type: String,
    /// Optional qualifier describing the role of the object in the event.
    pub qualifier: Option<String>,
}

/// A single append-only operation-event in a receipt chain.
///
/// Carries a logical sequence number (never wall-clock) and a commitment to
/// its payload bytes — the verifier checks commitments without seeing payloads.
///
/// # Examples
///
/// ```rust
/// use affidavit::{OperationEvent, Blake3Hash};
/// let event = OperationEvent {
///     id: "evt-0".to_string(),
///     seq: 0,
///     event_type: "compile".to_string(),
///     objects: vec![],
///     payload_commitment: Blake3Hash::from_bytes(b"some payload"),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OperationEvent {
    /// Identifier of this event, unique within the receipt.
    pub id: String,
    /// Monotonic logical sequence number (deterministic ordering, not time).
    pub seq: u64,
    /// The kind of operation this event records.
    pub event_type: String,
    /// Qualified object references this event relates to.
    pub objects: Vec<ObjectRef>,
    /// BLAKE3 commitment to the event's payload bytes.
    pub payload_commitment: Blake3Hash,
}

/// An immutable, content-addressed chain of operation-events.
///
/// Field order here is the canonical order used for hashing and serialization.
/// The private `_seal` field prevents struct-literal construction from external
/// code, enforcing that Receipts are built only through the canonically-sealed
/// seam [`crate::chain::ChainAssembler::finalize`]
/// (ADR-2: the seal is value-level; ADR-3: the carrier is non-forgeable).
///
/// Deserialization re-verifies the chain hash to block forged receipts from JSON.
///
/// # Panics
///
/// This struct does not panic during normal operation. Deserialization
/// errors are returned as `Result::Err`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Receipt {
    /// Format version string used by the verifier's format check.
    pub format_version: String,
    /// Ordered, append-only operation-events.
    pub events: Vec<OperationEvent>,
    /// Rolling BLAKE3 hash computed over the events in order.
    pub chain_hash: Blake3Hash,
    /// Private seal field — struct-literal construction is unconstructable (E0451).
    #[serde(skip)]
    _seal: (),
}

/// The conformance profile a verdict was evaluated under.
///
/// # Examples
///
/// ```rust
/// use affidavit::ProfileId;
/// let profile = ProfileId::CoreV1;
/// assert_eq!(profile.as_str(), "core/v1");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProfileId {
    /// Core v1: every event has a commitment and a non-empty event_type.
    CoreV1,
}

impl ProfileId {
    /// Stable string identifier for this profile.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use affidavit::ProfileId;
    /// assert_eq!(ProfileId::CoreV1.as_str(), "core/v1");
    /// ```
    pub fn as_str(&self) -> &'static str {
        match self {
            ProfileId::CoreV1 => "core/v1",
        }
    }
}

/// The result of a single decidable pipeline stage.
///
/// # Examples
///
/// ```rust
/// use affidavit::CheckOutcome;
/// let outcome = CheckOutcome {
///     stage: "decode".to_string(),
///     passed: true,
///     detail: "Receipt decoded successfully".to_string(),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CheckOutcome {
    /// Name of the pipeline stage that produced this outcome.
    pub stage: String,
    /// Whether the stage's decidable check passed.
    pub passed: bool,
    /// Human-readable explanation of the outcome.
    pub detail: String,
}

/// The final verdict of the certify pipeline over a receipt.
///
/// # Examples
///
/// ```rust
/// use affidavit::{Verdict, ProfileId};
/// let verdict = Verdict {
///     accepted: true,
///     profile: ProfileId::CoreV1,
///     outcomes: vec![],
///     reason: "All checks passed".to_string(),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Verdict {
    /// True when every required stage passed (ACCEPT), false otherwise (REJECT).
    pub accepted: bool,
    /// The conformance profile under which the receipt was evaluated.
    pub profile: ProfileId,
    /// Per-stage outcomes, in pipeline order.
    pub outcomes: Vec<CheckOutcome>,
    /// Summary reason for the final verdict.
    pub reason: String,
}

/// Detailed structural analysis of a receipt (inspect --format=json).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct InspectionReport {
    /// Total number of events in the receipt.
    pub event_count: usize,
    /// Format version of the receipt.
    pub format_version: String,
    /// Hex-encoded chain hash.
    pub chain_hash: String,
    /// Whether the chain hash correctly recomputes from events.
    pub chain_integrity_valid: bool,
    /// Histogram of event types.
    pub event_types: std::collections::BTreeMap<String, usize>,
    /// Histogram of object types.
    pub object_types: std::collections::BTreeMap<String, usize>,
    /// Summaries of individual events.
    pub events: Vec<EventSummary>,
}

/// A summary row for an event in an inspection report.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EventSummary {
    /// Logical sequence number.
    pub seq: u64,
    /// Event identifier.
    pub id: String,
    /// Type of operation.
    pub event_type: String,
    /// Number of objects referenced by this event.
    pub object_count: usize,
    /// Hex-encoded payload commitment.
    pub commitment: String,
}

/// Output of a successful emit operation (emit --format=json).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EmitOutput {
    /// Unique identifier of the emitted event.
    pub event_id: String,
    /// Logical sequence number assigned to the event.
    pub seq: u64,
    /// Operation type recorded in the event.
    pub event_type: String,
    /// Hex-encoded commitment to the payload.
    pub commitment: String,
}

/// Output of a successful assemble operation (assemble --format=json).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AssembleOutput {
    /// Path to the finalized receipt file.
    pub receipt_path: String,
    /// Content-addressed BLAKE3 hash of the entire receipt.
    pub content_address: String,
    /// Number of events included in the receipt.
    pub event_count: usize,
}

/// Aggregate metrics for a receipt (stats --format=json).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StatsOutput {
    /// Total number of events.
    pub event_count: usize,
    /// Total depth of the hash chain.
    pub chain_depth: usize,
    /// Final rolling chain hash.
    pub chain_hash: String,
    /// Histogram of event types.
    pub event_type_histogram: std::collections::BTreeMap<String, usize>,
    /// Histogram of object types.
    pub object_type_histogram: std::collections::BTreeMap<String, usize>,
}

/// A single quality metric value with descriptive context.
///
/// Represents a numeric measurement of a code quality dimension, paired with
/// a human-readable description explaining what the metric signifies.
///
/// # Examples
///
/// ```rust
/// use affidavit::QualityMetricValue;
/// let metric = QualityMetricValue {
///     value: 0.92,
///     description: "Proportion of code covered by tests".to_string(),
/// };
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMetricValue {
    /// The numeric value of this metric.
    pub value: f64,
    /// Human-readable description of what this metric measures.
    pub description: String,
}

/// A code quality snapshot emitted as an operation-event.
///
/// Represents a comprehensive set of quality measurements taken at a particular
/// point in the receipt chain. Each metric is a `QualityMetricValue` capturing
/// both the numeric value and its interpretation.
///
/// # Examples
///
/// ```rust
/// use affidavit::{QualityMeasurement, QualityMetricValue};
/// let measurement = QualityMeasurement {
///     timestamp: 1718641799,
///     stubs: QualityMetricValue {
///         value: 5.0,
///         description: "Number of stub functions".to_string(),
///     },
///     types: QualityMetricValue {
///         value: 0.0,
///         description: "Number of untyped bindings".to_string(),
///     },
///     churn: QualityMetricValue {
///         value: 0.15,
///         description: "Code churn ratio".to_string(),
///     },
///     comments: QualityMetricValue {
///         value: 0.85,
///         description: "Comment coverage ratio".to_string(),
///     },
///     complexity: QualityMetricValue {
///         value: 4.2,
///         description: "Average cyclomatic complexity".to_string(),
///     },
///     clippy_warnings: QualityMetricValue {
///         value: 3.0,
///         description: "Number of active Clippy warnings".to_string(),
///     },
///     rustfmt_violations: QualityMetricValue {
///         value: 0.0,
///         description: "Number of formatting violations".to_string(),
///     },
///     cargo_deny_issues: QualityMetricValue {
///         value: 0.0,
///         description: "Number of dependency audit issues".to_string(),
///     },
///     cargo_audit_vulnerabilities: QualityMetricValue {
///         value: 0.0,
///         description: "Number of known vulnerabilities in dependencies".to_string(),
///     },
///     test_coverage: QualityMetricValue {
///         value: 0.92,
///         description: "Proportion of code covered by tests".to_string(),
///     },
///     doc_coverage: QualityMetricValue {
///         value: 0.88,
///         description: "Proportion of public items with documentation".to_string(),
///     },
/// };
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMeasurement {
    /// Unix timestamp (seconds since epoch) when this snapshot was taken.
    pub timestamp: u64,
    /// Count and description of stub functions in the codebase.
    pub stubs: QualityMetricValue,
    /// Count and description of untyped bindings.
    pub types: QualityMetricValue,
    /// Ratio and description of code churn (changed lines / total lines).
    pub churn: QualityMetricValue,
    /// Ratio and description of comment coverage.
    pub comments: QualityMetricValue,
    /// Average cyclomatic complexity and description.
    pub complexity: QualityMetricValue,
    /// Count and description of active Clippy linter warnings.
    pub clippy_warnings: QualityMetricValue,
    /// Count and description of code formatting violations.
    pub rustfmt_violations: QualityMetricValue,
    /// Count and description of dependency audit issues detected by cargo-deny.
    pub cargo_deny_issues: QualityMetricValue,
    /// Count and description of known security vulnerabilities in dependencies.
    pub cargo_audit_vulnerabilities: QualityMetricValue,
    /// Ratio and description of test coverage (lines covered / total lines).
    pub test_coverage: QualityMetricValue,
    /// Ratio and description of documentation coverage for public items.
    pub doc_coverage: QualityMetricValue,
}

/// A violation of Western Electric control chart rules detected in quality metrics.
///
/// Records instances where a quality metric violates statistical control rules
/// (e.g., one or more standard deviations beyond a control limit, or sustained
/// trends). Used to flag anomalies that may indicate quality degradation.
///
/// # Examples
///
/// ```rust
/// use affidavit::QualityViolationEvent;
/// let violation = QualityViolationEvent {
///     rule: "Rule 1: beyond 1-sigma".to_string(),
///     metric: "test_coverage".to_string(),
///     value: 0.45,
///     threshold: 0.88,
///     z_score: 2.1,
///     severity: "warning".to_string(),
///     description: "Test coverage dropped below expected control limit".to_string(),
/// };
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityViolationEvent {
    /// Name of the Western Electric rule that was violated (e.g., "Rule 1: beyond 1-sigma").
    pub rule: String,
    /// Name of the quality metric affected (e.g., "test_coverage", "clippy_warnings").
    pub metric: String,
    /// The current numeric value of the metric that triggered the violation.
    pub value: f64,
    /// The control threshold that was exceeded.
    pub threshold: f64,
    /// The standardized z-score (number of standard deviations from mean).
    pub z_score: f64,
    /// Severity level of the violation ("info", "warning", "error").
    pub severity: String,
    /// Human-readable description of the violation and its implications.
    pub description: String,
}

/// Produce deterministic, sorted-key JSON bytes for any serializable value.
///
/// This is the canonical byte form used for content addressing and hashing:
/// objects have their keys recursively sorted so the same logical value always
/// yields identical bytes regardless of in-memory field order.
///
/// # Errors
///
/// Returns `serde_json::Error` if serialization fails.
///
/// # Examples
///
/// ```rust
/// use affidavit::canonical_bytes;
/// use serde::Serialize;
///
/// #[derive(Serialize)]
/// struct Data { b: i32, a: i32 }
/// let data = Data { b: 2, a: 1 };
/// let bytes = canonical_bytes(&data).unwrap();
/// // bytes is canonical {"a":1,"b":2}
/// ```
pub fn canonical_bytes<T: Serialize>(value: &T) -> Result<Vec<u8>, serde_json::Error> {
    let v = serde_json::to_value(value)?;
    let sorted = sort_value(v);
    serde_json::to_vec(&sorted)
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deserialize_rejects_forged_receipt() {
        // Create a valid receipt
        let mut asm = crate::chain::ChainAssembler::new();
        let mut counter = crate::ocel::SeqCounter::new();
        let event = crate::ocel::build_event(
            "test",
            vec![crate::ocel::object_ref("obj", "artifact")],
            b"payload",
            &mut counter,
        )
        .expect("build event");
        asm.append(event).expect("append");
        let honest = asm.finalize();

        // Serialize it
        let honest_json = serde_json::to_string(&honest).expect("serialize");

        // Now forge: change an event_type in the JSON without re-chaining
        let forged_json = honest_json.replace("\"test\"", "\"forged\"");

        // Try to deserialize the forged receipt - should fail
        let result: Result<Receipt, _> = serde_json::from_str(&forged_json);
        assert!(
            result.is_err(),
            "forged receipt should fail deserialization (chain mismatch)"
        );
        assert!(
            result
                .as_ref()
                .unwrap_err()
                .to_string()
                .contains("chain hash mismatch"),
            "error should mention chain hash mismatch"
        );
    }

    #[test]
    fn deserialize_accepts_honest_receipt() {
        let mut asm = crate::chain::ChainAssembler::new();
        let mut counter = crate::ocel::SeqCounter::new();
        let event = crate::ocel::build_event(
            "test",
            vec![crate::ocel::object_ref("obj", "artifact")],
            b"payload",
            &mut counter,
        )
        .expect("build event");
        asm.append(event).expect("append");
        let honest = asm.finalize();

        let honest_json = serde_json::to_string(&honest).expect("serialize");
        let deserialized: Receipt = serde_json::from_str(&honest_json).expect("deserialize honest");
        assert_eq!(deserialized, honest);
    }
}

/// Recursively sort the keys of all JSON objects within a value.
fn sort_value(value: serde_json::Value) -> serde_json::Value {
    use serde_json::Value;
    match value {
        Value::Object(map) => {
            // serde_json::Map preserves insertion order; collect into a BTreeMap
            // to impose deterministic key ordering, then rebuild.
            let sorted: std::collections::BTreeMap<String, Value> =
                map.into_iter().map(|(k, v)| (k, sort_value(v))).collect();
            let mut out = serde_json::Map::new();
            for (k, v) in sorted {
                out.insert(k, v);
            }
            Value::Object(out)
        }
        Value::Array(arr) => Value::Array(arr.into_iter().map(sort_value).collect()),
        other => other,
    }
}
