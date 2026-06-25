//! Process discovery — delegates to `wasm4pm_compat` native algorithms.
//!
//! The former Python pm4py subprocess bridge is replaced by the pure-Rust
//! `wasm4pm_compat::dfg` functions, which are the authoritative process-
//! intelligence entry point for non-WASM consumers.
//!
//! `Pm4pyBridge` converts ggen-graph's `OcelLog` to `wasm4pm_compat::ocel::OCEL`,
//! then calls `discover_ocel_dfg` and `extract_ocel_variants` from compat.

use std::collections::HashMap;

use chrono::FixedOffset;
use serde::{Deserialize, Serialize};
use wasm4pm_compat::dfg::{
    dfg_fitness, dfg_precision, discover_ocel_dfg, extract_ocel_variants,
};
use wasm4pm_compat::ocel::{OCELEvent, OCELObject, OCELRelationship, OCELType, OCEL};

use crate::ocel::OcelLog;
use crate::GraphError;

/// Statistics from process discovery.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Pm4pyStats {
    /// Number of distinct execution variants detected in the log.
    pub variant_count: usize,
    /// The most frequent sequence of activities (canonical path).
    pub canonical_path: Vec<String>,
    /// Conformance fitness score (0.0 = no conformance, 1.0 = perfect).
    pub fitness: f64,
    /// Model precision score (0.0 = excessive behavior, 1.0 = exact match).
    pub precision: f64,
    /// Discovered process model as a directly-follows summary.
    pub discovered_model: String,
}

impl Default for Pm4pyStats {
    fn default() -> Self {
        Self {
            variant_count: 1,
            canonical_path: vec![],
            fitness: 1.0,
            precision: 1.0,
            discovered_model: String::new(),
        }
    }
}

/// Process discovery bridge — delegates to `wasm4pm_compat` native algorithms.
pub struct Pm4pyBridge;

impl Pm4pyBridge {
    pub fn new() -> Self {
        Self
    }

    pub fn discover(&mut self, log: &OcelLog) -> Result<Pm4pyStats, GraphError> {
        let ocel = to_compat_ocel(log);

        let dfg = discover_ocel_dfg(&ocel);
        let traces = extract_ocel_variants(&ocel);

        if traces.is_empty() {
            return Ok(Pm4pyStats::default());
        }

        // Variant count = distinct trace sequences.
        let mut variant_freq: HashMap<Vec<String>, usize> = HashMap::new();
        for trace in &traces {
            *variant_freq.entry(trace.clone()).or_insert(0) += 1;
        }
        let variant_count = variant_freq.len();

        // Canonical path = most frequent variant.
        let canonical_path = variant_freq
            .iter()
            .max_by_key(|(_, freq)| *freq)
            .map(|(trace, _)| trace.clone())
            .unwrap_or_default();

        // Normative arcs from canonical path.
        let normative_arcs: Vec<(String, String)> = canonical_path
            .windows(2)
            .map(|w| (w[0].clone(), w[1].clone()))
            .collect();

        let fitness = dfg_fitness(&dfg, &normative_arcs);
        let precision = dfg_precision(&dfg, &normative_arcs);

        let discovered_model = dfg
            .edges
            .iter()
            .map(|e| format!("{} → {} (×{})", e.source, e.target, e.frequency))
            .collect::<Vec<_>>()
            .join(", ");

        tracing::debug!(
            ocel.variant_count = variant_count,
            ocel.fitness = fitness,
            "process discovery complete"
        );

        Ok(Pm4pyStats {
            variant_count,
            canonical_path,
            fitness,
            precision,
            discovered_model,
        })
    }
}

impl Default for Pm4pyBridge {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert ggen-graph's `OcelLog` to `wasm4pm_compat::ocel::OCEL`.
fn to_compat_ocel(log: &OcelLog) -> OCEL {
    use std::collections::HashSet;

    // Collect unique event types and object types from the log.
    let event_type_names: HashSet<&str> =
        log.events.iter().map(|e| e.activity.as_str()).collect();
    let object_type_names: HashSet<&str> =
        log.objects.iter().map(|o| o.object_type.as_str()).collect();

    let event_types: Vec<OCELType> = event_type_names
        .into_iter()
        .map(|name| OCELType { name: name.to_string(), attributes: vec![] })
        .collect();

    let object_types: Vec<OCELType> = object_type_names
        .into_iter()
        .map(|name| OCELType { name: name.to_string(), attributes: vec![] })
        .collect();

    // Convert objects.
    let objects: Vec<OCELObject> = log
        .objects
        .iter()
        .map(|o| OCELObject {
            id: o.id.clone(),
            object_type: o.object_type.clone(),
            attributes: vec![],
            relationships: vec![],
        })
        .collect();

    // Convert events. Use UTC offset 0 since ggen OcelLog uses chrono::DateTime<Utc>.
    let utc: FixedOffset = FixedOffset::east_opt(0).unwrap_or_default();
    let events: Vec<OCELEvent> = log
        .events
        .iter()
        .map(|e| {
            let time = e.timestamp.with_timezone(&utc);
            let relationships: Vec<OCELRelationship> = e
                .objects
                .iter()
                .map(|r| OCELRelationship {
                    object_id: r.id.clone(),
                    qualifier: r.qualifier.clone().unwrap_or_else(|| r.r#type.clone()),
                })
                .collect();
            OCELEvent {
                id: e.id.clone(),
                event_type: e.activity.clone(),
                time,
                attributes: vec![],
                relationships,
            }
        })
        .collect();

    OCEL { event_types, object_types, events, objects }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ocel::{OcelEvent, OcelLog, OcelObject, OcelObjectRef};
    use chrono::{TimeZone, Utc};

    fn make_log() -> OcelLog {
        OcelLog {
            objects: vec![OcelObject {
                id: "case1".into(),
                object_type: "case".into(),
                attributes: HashMap::new(),
            }],
            events: vec![
                OcelEvent {
                    id: "e1".into(),
                    activity: "A".into(),
                    timestamp: Utc.timestamp_opt(10, 0).single().unwrap(),
                    objects: vec![OcelObjectRef {
                        id: "case1".into(),
                        r#type: "case".into(),
                        qualifier: None,
                    }],
                    attributes: HashMap::new(),
                },
                OcelEvent {
                    id: "e2".into(),
                    activity: "B".into(),
                    timestamp: Utc.timestamp_opt(20, 0).single().unwrap(),
                    objects: vec![OcelObjectRef {
                        id: "case1".into(),
                        r#type: "case".into(),
                        qualifier: None,
                    }],
                    attributes: HashMap::new(),
                },
            ],
        }
    }

    #[test]
    fn discovers_single_variant() {
        let log = make_log();
        let mut bridge = Pm4pyBridge::new();
        let stats = bridge.discover(&log).unwrap();
        assert_eq!(stats.variant_count, 1);
        assert_eq!(stats.canonical_path, vec!["A", "B"]);
        assert!((stats.fitness - 1.0).abs() < 1e-9);
    }

    #[test]
    fn empty_log_returns_default() {
        let log = OcelLog::new();
        let mut bridge = Pm4pyBridge::new();
        let stats = bridge.discover(&log).unwrap();
        assert_eq!(stats.variant_count, 1);
    }
}
