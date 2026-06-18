//! Neutral, OCEL-shaped events emitted by the reverse pipeline.
//!
//! `ggen-core` cannot depend on `ggen-lsp` (where `IntelLog` lives), so the
//! reverse engine returns these neutral events. The CLI layer maps each
//! [`ReverseEvent`] to the OCEL log (`.ggen/ocel/agent-edit-events.ocel.jsonl`)
//! 1:1 — under `--features lsp` via `ggen_lsp::intel::IntelLog`, otherwise via a
//! direct line append. This keeps OCEL logging on the authoritative path
//! regardless of feature flags.

use std::collections::BTreeMap;

/// A single object-centric event: an activity acting on one object, with
/// deterministic (sorted) attributes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReverseEvent {
    /// OCEL activity name (see [`activity`]).
    pub activity: String,
    /// Identifier of the object this event acts on.
    pub object_id: String,
    /// Object type (see [`obj_type`]).
    pub object_type: String,
    /// Sorted event attributes.
    pub attributes: BTreeMap<String, String>,
}

impl ReverseEvent {
    /// Create an event with no attributes.
    pub fn new(activity: &str, object_id: &str, object_type: &str) -> Self {
        Self {
            activity: activity.to_string(),
            object_id: object_id.to_string(),
            object_type: object_type.to_string(),
            attributes: BTreeMap::new(),
        }
    }

    /// Attach an attribute (builder style).
    #[must_use]
    pub fn with_attr(mut self, key: &str, value: impl Into<String>) -> Self {
        self.attributes.insert(key.to_string(), value.into());
        self
    }
}

/// OCEL activity names for reverse-pipeline events.
pub mod activity {
    /// A `ggen reverse scan` run started.
    pub const SCAN_STARTED: &str = "ReverseScanStarted";
    /// A source file contributed services to the discovered graph.
    pub const SCAN_FILE: &str = "ReverseScanFile";
    /// A `ggen reverse scan` run completed.
    pub const SCAN_COMPLETED: &str = "ReverseScanCompleted";
    /// A `ggen reverse cheats` run started.
    pub const CHEATS_STARTED: &str = "ReverseCheatsStarted";
    /// A defect node was emitted.
    pub const CHEATS_DEFECT: &str = "ReverseCheatsDefect";
    /// A `ggen reverse cheats` run completed.
    pub const CHEATS_COMPLETED: &str = "ReverseCheatsCompleted";
    /// A `ggen reverse templates` run started.
    pub const TEMPLATES_STARTED: &str = "ReverseTemplatesStarted";
    /// A template candidate was emitted.
    pub const TEMPLATE_CANDIDATE: &str = "ReverseTemplateCandidate";
    /// A `ggen reverse templates` run completed.
    pub const TEMPLATES_COMPLETED: &str = "ReverseTemplatesCompleted";
    /// A provenance receipt was written.
    pub const RECEIPT_EMITTED: &str = "ReceiptEmitted";
}

/// OCEL object types for reverse-pipeline events.
pub mod obj_type {
    /// The discovered authority graph (`.specify/discovered/<name>.ttl`).
    pub const DISCOVERED_GRAPH: &str = "discovered_graph";
    /// A scanned source file.
    pub const SOURCE_FILE: &str = "source_file";
    /// A defect node.
    pub const DEFECT: &str = "defect";
    /// A Tera template candidate.
    pub const TEMPLATE_CANDIDATE: &str = "template_candidate";
    /// A provenance receipt.
    pub const RECEIPT: &str = "receipt";
}
