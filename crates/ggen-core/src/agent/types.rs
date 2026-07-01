//! Structured request/response types for the agent-facing packs + marketplace
//! facade.
//!
//! Every type here is `serde`-serializable so that an autonomous agent (over
//! MCP, A2A, or a direct library call) receives a stable, machine-readable
//! contract instead of human-oriented prose. The design goal is *evidence-
//! bearing* output: each mutating outcome carries the durable artifacts it
//! produced (digests, lockfile path, receipt path) so an agent can verify the
//! state transition actually happened rather than trusting a status string.

use serde::{Deserialize, Serialize};

/// Machine-readable error kind returned by every facade operation.
///
/// Agents branch on `kind` (the serde tag) rather than parsing a message, so
/// the failure mode is part of the contract. Each variant is fail-closed:
/// a refusal is surfaced as a typed error, never swallowed into a success.
#[derive(Debug, Clone, thiserror::Error, Serialize, Deserialize)]
#[serde(tag = "kind", content = "detail", rename_all = "snake_case")]
pub enum AgentError {
    /// The requested pack does not exist in the local registry.
    #[error("pack not found: {0}")]
    PackNotFound(String),

    /// The request was malformed (empty/invalid pack name, bad capability, …).
    #[error("invalid request: {0}")]
    InvalidRequest(String),

    /// A pack was requested for removal but is not present in the lockfile.
    #[error("pack not installed: {0}")]
    NotInstalled(String),

    /// The underlying install pipeline failed.
    #[error("install failed: {0}")]
    InstallFailed(String),

    /// Receipt emission or verification failed.
    #[error("receipt error: {0}")]
    Receipt(String),

    /// Capability resolution failed.
    #[error("resolve failed: {0}")]
    ResolveFailed(String),

    /// A filesystem / IO boundary error.
    #[error("io error: {0}")]
    Io(String),

    /// An internal error that does not map to a more specific kind.
    #[error("internal error: {0}")]
    Internal(String),
}

/// Convenience result alias for facade operations.
pub type AgentResult<T> = std::result::Result<T, AgentError>;

/// A compact reference to a pack, used in lists and search hits.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PackRef {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub category: String,
    /// Origin registry class (`ggen`, `crates.io`, `npm`, `local`, …).
    pub registry_type: String,
    pub production_ready: bool,
}

/// A search result: a pack plus its relevance score in `[0.0, 1.0]`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SearchHit {
    pub pack: PackRef,
    pub score: f64,
}

/// A declared dependency edge of a pack.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DependencyRef {
    pub pack_id: String,
    pub version: String,
    pub optional: bool,
}

/// Validation summary for a pack (quality gate result).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PackValidation {
    pub valid: bool,
    pub score: f64,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

/// Full detail view of a single pack, including validation and dependency edges.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PackDetail {
    pub pack: PackRef,
    pub packages: Vec<String>,
    pub templates: Vec<String>,
    pub dependencies: Vec<DependencyRef>,
    pub sparql_query_count: usize,
    pub validation: PackValidation,
}

/// Outcome of resolving a high-level capability (surface/projection/runtime)
/// to concrete pack IDs. `missing` packs are reported honestly so an agent can
/// decide to install them before proceeding.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResolveOutcome {
    pub surface: String,
    pub projection: Option<String>,
    pub runtime: Option<String>,
    /// Pack IDs that resolved and exist in the local registry.
    pub resolved: Vec<String>,
    /// Pack IDs that resolved by name but are not installed/available locally.
    pub missing: Vec<String>,
    /// `ggen pack add <id>` hints for each missing pack.
    pub install_hints: Vec<String>,
}

/// Outcome of checking whether a set of packs can be composed together.
///
/// Reports conflicts (the same package supplied by more than one pack, or a
/// pack that cannot be loaded) so an agent can decide whether the composition is
/// safe *before* installing anything. Fail-closed: a pack that cannot be loaded
/// makes the set `compatible == false` rather than being silently ignored.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompatibilityOutcome {
    pub pack_ids: Vec<String>,
    pub compatible: bool,
    /// Hard conflicts (overlapping packages, load failures) that block composition.
    pub conflicts: Vec<String>,
    /// Non-blocking advisories.
    pub warnings: Vec<String>,
    /// Human-readable summary.
    pub message: String,
}

/// A pointer to a signed provenance receipt produced by an install.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReceiptRef {
    pub receipt_path: String,
    pub operation_id: String,
    /// Whether the receipt carries a non-empty Ed25519 signature.
    pub signature_present: bool,
}

/// Evidence-bearing outcome of a pack install.
///
/// On a real (non-dry-run) install, `digest` is a non-empty SHA-256 and both
/// `lockfile_path` and `receipt` are `Some`, proving the durable state
/// transition (lockfile written, provenance signed). A dry run carries an empty
/// digest and no durable artifacts.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InstallOutcome {
    pub pack_id: String,
    pub pack_name: String,
    pub pack_version: String,
    pub packages_installed: Vec<String>,
    pub templates_available: Vec<String>,
    /// SHA-256 hex digest pinned in the lockfile (empty only for `dry_run`).
    pub digest: String,
    pub install_path: String,
    pub lockfile_path: Option<String>,
    pub receipt: Option<ReceiptRef>,
    pub dry_run: bool,
}

/// Outcome of removing a pack from the project lockfile.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RemoveOutcome {
    pub pack_id: String,
    pub removed: bool,
    pub lockfile_path: String,
    /// Pack IDs still present in the lockfile after removal.
    pub remaining: Vec<String>,
}

/// Outcome of verifying a provenance receipt against its signing key.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VerifyOutcome {
    pub receipt_path: String,
    pub is_valid: bool,
    pub operation_id: Option<String>,
    /// Reason the receipt is invalid, when `is_valid` is false.
    pub reason: Option<String>,
}

/// A pack currently recorded in the project lockfile (durable installed state).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InstalledPackRef {
    pub pack_id: String,
    pub version: String,
    pub integrity: Option<String>,
    pub installed_at: String,
}

/// Snapshot of the project's installed-pack state, read from `.ggen/packs.lock`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AgentStatus {
    pub lockfile_present: bool,
    pub lockfile_path: String,
    pub ggen_version: Option<String>,
    pub installed: Vec<InstalledPackRef>,
}

/// A capability surface an agent can resolve into packs.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CapabilityRef {
    pub id: String,
    pub name: String,
    pub description: String,
    pub category: String,
    pub atomic_packs: Vec<String>,
}

/// Description of a single operation the facade exposes.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OperationRef {
    pub name: String,
    pub description: String,
    /// Whether the operation mutates durable project state (lockfile/receipts).
    pub mutating: bool,
}

/// The agent's self-description: the operations it offers and the capability
/// surfaces it knows about.
///
/// This is the discovery entry point — an agent calls `capabilities()` first to
/// learn what it can do without out-of-band docs.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Capabilities {
    pub operations: Vec<OperationRef>,
    pub surfaces: Vec<CapabilityRef>,
}

/// Request to install a pack.
///
/// Mirrors the durable-state contract of the underlying pipeline: `dry_run`
/// previews without writing the lockfile or a receipt; `force` permits
/// reinstallation over an existing install path.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InstallRequest {
    pub pack_id: String,
    #[serde(default)]
    pub force: bool,
    #[serde(default)]
    pub dry_run: bool,
    /// Emit a signed provenance receipt on success (ignored for `dry_run`).
    #[serde(default = "default_true")]
    pub emit_receipt: bool,
}

fn default_true() -> bool {
    true
}

impl InstallRequest {
    /// Construct a default install request for `pack_id` (real install, receipt
    /// emitted, no force).
    pub fn new(pack_id: impl Into<String>) -> Self {
        Self {
            pack_id: pack_id.into(),
            force: false,
            dry_run: false,
            emit_receipt: true,
        }
    }
}
