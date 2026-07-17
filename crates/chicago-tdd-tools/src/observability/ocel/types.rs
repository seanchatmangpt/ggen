#![allow(missing_docs)]
#![allow(dead_code)]
use crate::core::governance::{DiagnosticCategory, RunId, Severity};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// The OCEL 2.0 activity vocabulary for test execution.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestActivity {
    // ── Test lifecycle ────────────────────────────────────────────────────────
    TestStarted,
    TestCompleted {
        passed: bool,
    },
    TestSkipped {
        reason: String,
    },

    // ── Fixture lifecycle ─────────────────────────────────────────────────────
    FixtureSetup,
    FixtureTeardown,
    FixtureComposed,

    // ── Assertion events ──────────────────────────────────────────────────────
    AssertionPassed {
        macro_name: String,
    },
    AssertionFailed {
        macro_name: String,
    },

    // ── Domain law events ────────────────────────────────────────────────────
    ArtifactAdmitted,
    ArtifactRefused,
    ReceiptGenerated,
    ReceiptMissing,
    DriftZero,
    DriftDetected,
    BypassDetected,
    ConformanceChecked {
        fitness: f64,
    },
    ConformanceFailed {
        fitness: f64,
        first_deviation_step: usize,
    },
    SubstrateGrew {
        rank: u8,
    },
    SubstrateLabor,
    OntologyConsistent,
    OntologyViolation,

    // ── Diagnostic events ────────────────────────────────────────────────────
    DiagnosticEmitted {
        category: DiagnosticCategory,
        severity: Severity,
    },

    // ── Wave orchestration (CA-8) ─────────────────────────────────────────────
    WavePhaseStarted {
        phase: u8,
        task_count: usize,
    },
    WavePhaseCompleted {
        phase: u8,
        admitted: usize,
        refused: usize,
    },
    WaveTaskStarted {
        phase: u8,
        task_index: usize,
    },
    WaveTaskCompleted {
        phase: u8,
        task_index: usize,
        receipt_present: bool,
    },
    WaveConflictDetected,

    // ── Run lifecycle ─────────────────────────────────────────────────────────
    RunStarted {
        registered_domain: String,
    },
    RunCompleted {
        p_admitted: f64,
        andon_count: usize,
    },
}

/// OCEL 2.0 object types for the testing domain.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum TestObjectType {
    Artifact,
    Schema,
    ProcessModel,
    Receipt,
    Fixture,
    AgentContext,
    WaveInstance,
    SectorStack,
}

/// The complete OCEL event record.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestOcelEvent {
    pub event_id: String,
    pub case_id: RunId,
    pub activity: TestActivity,
    pub timestamp_ns: u64,
    pub objects: Vec<(String, TestObjectType)>,
    pub attributes: HashMap<String, serde_json::Value>,
}

/// Represents the complete OCEL 2.0 log.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OcelLog {
    #[serde(rename = "ocel:global-log")]
    pub global_log: HashMap<String, serde_json::Value>,
    #[serde(rename = "ocel:events")]
    pub events: HashMap<String, TestOcelEvent>,
    #[serde(rename = "ocel:objects")]
    pub objects: HashMap<String, TestObject>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestObject {
    #[serde(rename = "ocel:type")]
    pub object_type: TestObjectType,
    #[serde(rename = "ocel:ovmap")]
    pub attributes: HashMap<String, serde_json::Value>,
}

impl OcelLog {
    #[must_use]
    pub fn new() -> Self {
        let mut global_log = HashMap::new();
        let _ = global_log.insert("ocel:version".to_string(), serde_json::json!("2.0"));
        Self {
            global_log,
            events: HashMap::new(),
            objects: HashMap::new(),
        }
    }
}
