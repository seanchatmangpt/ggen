//! OCEL process-model store and graduation helpers.
// Mutex poison recovery via `unwrap_or_else(|e| e.into_inner())` is intentional.
#![allow(clippy::unwrap_used)]
use crate::observability::ocel::types::OcelLog;
use crate::observability::ocel::wasm4pm::TestSuiteWitness;
use std::collections::HashMap;
use std::sync::Mutex;
use wasm4pm_compat::{Evidence, Receipted};

#[cfg(feature = "ocel-generation-discovery")]
use wasm4pm_compat::engine_bridge::{GraduationCandidate, GraduationReason};

pub struct ProcessModelStore {
    models: Mutex<HashMap<String, Vec<u8>>>,
}

impl ProcessModelStore {
    #[must_use]
    pub fn new() -> Self {
        Self { models: Mutex::new(HashMap::new()) }
    }

    pub fn store(&self, name: &str, model: Vec<u8>) {
        let mut models = self.models.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
        let _ = models.insert(name.to_string(), model);
    }
}

impl Default for ProcessModelStore {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "ocel-generation-discovery")]
#[must_use]
pub fn graduate_for_discovery(
    receipted_log: &Evidence<OcelLog, Receipted, TestSuiteWitness>,
) -> GraduationCandidate {
    let evidence_ref = receipted_log.inner().events.len().to_string();
    GraduationCandidate::new(
        GraduationReason::NeedsDiscovery,
        "test-execution-process-model",
        evidence_ref,
    )
}
