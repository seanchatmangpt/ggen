#![allow(unused_imports)]
#![allow(dead_code)]
use crate::core::governance::{
    ContributionKind, Diagnostic, DiagnosticSink, RunId, RunSummary, SubstrateDelta,
};
use crate::observability::ocel::types::{TestActivity, TestObjectType, TestOcelEvent};
use crate::observability::ocel::wasm4pm::{seal_run, TestSuiteWitness};
use dashmap::{DashMap, DashSet};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Mutex;
use wasm4pm_compat::{Admitted, Evidence, Raw};

/// Implements [`DiagnosticSink`] + collects OCEL events automatically.
pub struct OcelCollector {
    pub(crate) events: Mutex<Vec<Evidence<TestOcelEvent, Admitted, TestSuiteWitness>>>,
    pub(crate) known_objects: DashSet<String>,
    pub(crate) last_timestamps: DashMap<RunId, u64>,
    pub(crate) ocel_output_path: Option<PathBuf>,
    pub(crate) auto_discover: bool,
    pub(crate) discovery_threshold: usize,
    pub(crate) discovery_triggered: Mutex<bool>,
}

impl OcelCollector {
    #[must_use]
    pub fn new(output_path: Option<PathBuf>) -> Self {
        Self {
            events: Mutex::new(Vec::new()),
            known_objects: DashSet::new(),
            last_timestamps: DashMap::new(),
            ocel_output_path: output_path,
            auto_discover: true,
            discovery_threshold: 10,
            discovery_triggered: Mutex::new(false),
        }
    }
}

impl DiagnosticSink for OcelCollector {
    fn emit(&self, diagnostic: Diagnostic) -> Result<(), String> {
        let mut objects = Vec::new();
        let mut attributes = HashMap::new();

        for (k, v) in &diagnostic.context {
            let _ = attributes.insert((*k).to_string(), v.clone());
            if k.ends_with("_id") {
                if let Some(s) = v.as_str() {
                    let obj_type = match *k {
                        "schema_id" => TestObjectType::Schema,
                        "fixture_id" => TestObjectType::Fixture,
                        _ => TestObjectType::Artifact,
                    };
                    objects.push((s.to_string(), obj_type));
                    self.known_objects.insert(s.to_string());
                }
            }
        }

        let event = TestOcelEvent {
            event_id: uuid::Uuid::new_v4().to_string(),
            case_id: diagnostic.run_id.clone(),
            activity: TestActivity::DiagnosticEmitted {
                category: diagnostic.code.category,
                severity: diagnostic.severity,
            },
            timestamp_ns: diagnostic.elapsed_ns,
            objects,
            attributes,
        };

        let raw = Evidence::<TestOcelEvent, Raw, TestSuiteWitness>::from_boundary(event);
        match self.admit_event(&raw) {
            Ok(admitted) => {
                let mut events = self.events.lock().map_err(|e| e.to_string())?;
                events.push(admitted);
            }
            Err(refusal) => {
                tracing::warn!("OCEL event refused: {:?} - not added to log", refusal);
            }
        }
        Ok(())
    }

    fn close(&self, summary: RunSummary) -> Result<(), String> {
        let (receipted_log, digest_hex) = seal_run(self, summary.run_id.clone())?;

        if let Some(path) = &self.ocel_output_path {
            let log_json =
                serde_json::to_string_pretty(receipted_log.inner()).map_err(|e| e.to_string())?;
            std::fs::write(path, log_json).map_err(|e| e.to_string())?;
        }

        let _delta = SubstrateDelta {
            contributions: vec![
                (ContributionKind::ProcessPattern, summary.run_id),
                (ContributionKind::Receipt, digest_hex),
            ],
        };

        Ok(())
    }
}
