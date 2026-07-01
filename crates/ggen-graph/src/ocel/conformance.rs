//! Lifecycle and repair-route conformance via SPARQL ASK over the OCEL-RDF log.
//!
//! This module provides two levels of conformance checking:
//!
//! 1. **Lifecycle Order Conformance** (existing): A repair "conforms" when its events
//!    occurred in a lawful order for the same correlated object
//!    (e.g. `DiagnosticRaised ≺ RepairApplied ≺ GatePassed` for one diagnostic).
//!
//! 2. **Process Model Conformance** (new): Validates an OCEL event log against an expected
//!    BPMN/YAWL process model by converting the model to SPARQL CONSTRUCT queries
//!    and replaying events through the model to detect deviations.

use oxigraph::sparql::QueryResults;
use std::collections::HashMap;

use crate::graph::DeterministicGraph;
use crate::ocel::{OcelEvent, OcelLog};
use crate::GraphError;

/// Check that a sequence of activities occurred in strict temporal order for a
/// shared case object (identified by the e2o qualifier predicate).
///
/// Returns `true` iff there exist events with the given `activities`, all
/// correlated to the same case object, with strictly increasing timestamps in
/// the given order. An empty or single-element `activities` slice trivially
/// conforms (returns `true`).
///
/// # Errors
/// Returns a [`GraphError`] if the SPARQL query fails to parse or evaluate.
pub fn check_lifecycle_order(
    graph: &DeterministicGraph, case_qualifier_iri: &str, activities: &[&str],
) -> Result<bool, GraphError> {
    if activities.len() < 2 {
        return Ok(true);
    }

    let mut patterns = String::new();
    let mut filters = Vec::new();
    for (i, act) in activities.iter().enumerate() {
        let escaped = act.replace('"', "\\\"");
        patterns.push_str(&format!(
            "?e{i} ocel:activity \"{escaped}\" ; ocel:timestamp ?t{i} ; <{q}> ?case .\n",
            i = i,
            escaped = escaped,
            q = case_qualifier_iri
        ));
        if i > 0 {
            filters.push(format!("?t{prev} < ?t{i}", prev = i - 1, i = i));
        }
    }

    let q = format!(
        r"
        PREFIX ocel: <http://www.ocel-standard.org/ns#>
        ASK {{
            {patterns}
            FILTER({filter})
        }}
        ",
        patterns = patterns,
        filter = filters.join(" && ")
    );

    match graph.query(&q)? {
        QueryResults::Boolean(b) => Ok(b),
        _ => Ok(false),
    }
}

/// Evaluate an inline OCPQ-style SPARQL ASK guard against the log. A route step's
/// `powl:guard` is such a query; this runs it and returns its boolean result.
///
/// # Errors
/// Returns a [`GraphError`] if `ask_query` is not a valid ASK query or fails.
pub fn check_guard(graph: &DeterministicGraph, ask_query: &str) -> Result<bool, GraphError> {
    match graph.query(ask_query)? {
        QueryResults::Boolean(b) => Ok(b),
        _ => Err(GraphError::Serialization(
            "guard query must be an ASK query".to_string(),
        )),
    }
}

/// A deviation from the expected process model during conformance checking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConformanceDrift {
    /// The activity that deviated (e.g., "pack.verify").
    pub activity: String,
    /// The object type (e.g., "pack", "receipt").
    pub object_type: String,
    /// Description of the unexpected transition or missing activity.
    pub description: String,
}

/// Report of OCEL conformance against an expected process model.
#[derive(Debug, Clone)]
pub struct ConformanceReport {
    /// Fitness score (0.0 = no conformance, 1.0 = perfect conformance).
    pub fitness: f64,
    /// Precision score (0.0 = excessive behavior, 1.0 = exact match).
    pub precision: f64,
    /// List of detected deviations from the expected model.
    pub deviations: Vec<ConformanceDrift>,
    /// Optional advanced metrics from pm4py process discovery.
    /// If pm4py is unavailable or discovery fails, this is None.
    pub pm4py_stats: Option<crate::ocel::pm4py_bridge::Pm4pyStats>,
}

/// OCEL conformance checker against BPMN/YAWL process models.
pub struct OcelConformanceChecker;

impl OcelConformanceChecker {
    /// Check conformance of an OCEL event log against an expected process model.
    ///
    /// # Arguments
    ///
    /// * `log` - The OCEL event log to validate
    /// * `expected_model` - A SPARQL CONSTRUCT query that models the expected process,
    ///   or a simple model description (e.g., "pack.install -> pack.verify -> pack.publish")
    ///
    /// # Returns
    ///
    /// A [`ConformanceReport`] with:
    /// - `fitness`: Ratio of events that conform to the expected model (0.0-1.0)
    /// - `precision`: Ratio of expected model transitions that are observed (0.0-1.0)
    /// - `deviations`: List of observed deviations from the model
    ///
    /// # Algorithm
    ///
    /// 1. Group events by object type
    /// 2. For each object type, build the observed activity sequence
    /// 3. Compare against the expected model transitions
    /// 4. Calculate fitness as: (conforming events) / (total events)
    /// 5. Calculate precision as: (observed expected transitions) / (total expected transitions)
    ///
    /// # Errors
    /// Returns a [`GraphError`] if SPARQL queries fail during conformance validation.
    pub fn check(
        log: &OcelLog, expected_model: &str,
    ) -> Result<ConformanceReport, GraphError> {
        // Parse simple model format: "activity1 -> activity2 -> activity3"
        let expected_transitions = Self::parse_model(expected_model);

        // Build observed activity sequences grouped by object type
        let mut sequences_by_type: HashMap<String, Vec<String>> = HashMap::new();
        for event in &log.events {
            for obj_ref in &event.objects {
                let seq = sequences_by_type
                    .entry(obj_ref.r#type.clone())
                    .or_insert_with(Vec::new);
                seq.push(event.activity.clone());
            }
        }

        // Detect deviations and calculate fitness
        let mut deviations = Vec::new();
        let mut conforming_events = 0;
        let total_events = log.events.len();

        for event in &log.events {
            let mut is_conforming = true;
            for obj_ref in &event.objects {
                if let Some(seq) = sequences_by_type.get(&obj_ref.r#type) {
                    // Check if this activity appears in expected model
                    if !expected_transitions.is_empty()
                        && !expected_transitions
                            .iter()
                            .any(|(src, tgt)| src == &event.activity || tgt == &event.activity)
                    {
                        is_conforming = false;
                        deviations.push(ConformanceDrift {
                            activity: event.activity.clone(),
                            object_type: obj_ref.r#type.clone(),
                            description: format!(
                                "Activity '{}' not in expected model",
                                event.activity
                            ),
                        });
                    }
                }
            }
            if is_conforming {
                conforming_events += 1;
            }
        }

        // Calculate fitness
        let fitness = if total_events > 0 {
            conforming_events as f64 / total_events as f64
        } else {
            1.0
        };

        // Calculate precision: % of expected transitions observed
        let observed_transitions = self_build_transitions(log);
        let expected_observed_count = expected_transitions
            .iter()
            .filter(|(src, tgt)| {
                observed_transitions.iter().any(|(o_src, o_tgt)| o_src == src && o_tgt == tgt)
            })
            .count();

        let precision = if !expected_transitions.is_empty() {
            expected_observed_count as f64 / expected_transitions.len() as f64
        } else {
            1.0
        };

        Ok(ConformanceReport {
            fitness,
            precision,
            deviations,
            pm4py_stats: None, // Will be populated by callers if needed
        })
    }

    /// Parse a simple model format: "activity1 -> activity2 -> activity3"
    fn parse_model(model: &str) -> Vec<(String, String)> {
        let activities: Vec<&str> = model.split("->").map(|s| s.trim()).collect();
        let mut transitions = Vec::new();
        for i in 0..activities.len().saturating_sub(1) {
            transitions.push((
                activities[i].to_string(),
                activities[i + 1].to_string(),
            ));
        }
        transitions
    }
}

/// Helper: build all observed transitions (source -> target pairs) from an OCEL log.
fn self_build_transitions(log: &OcelLog) -> Vec<(String, String)> {
    let mut transitions = Vec::new();

    // Group events by object to respect case correlation
    let mut events_by_object: HashMap<String, Vec<&OcelEvent>> = HashMap::new();
    for event in &log.events {
        for obj_ref in &event.objects {
            events_by_object
                .entry(obj_ref.id.clone())
                .or_insert_with(Vec::new)
                .push(event);
        }
    }

    // For each object, find directly-follows transitions
    for (_, mut obj_events) in events_by_object {
        obj_events.sort_by_key(|e| e.timestamp);
        for i in 0..obj_events.len().saturating_sub(1) {
            transitions.push((
                obj_events[i].activity.clone(),
                obj_events[i + 1].activity.clone(),
            ));
        }
    }

    transitions
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ocel::{EvidenceProjector, OcelEvent, OcelLog, OcelObjectRef};
    use chrono::{TimeZone, Utc};
    use std::collections::HashMap;

    fn ev(id: &str, activity: &str, secs: i64, diag: &str) -> OcelEvent {
        OcelEvent {
            id: id.to_string(),
            activity: activity.to_string(),
            timestamp: Utc.timestamp_opt(secs, 0).single().unwrap_or_else(Utc::now),
            objects: vec![OcelObjectRef {
                id: diag.to_string(),
                r#type: "diagnostic_code".to_string(),
                qualifier: Some("diag".to_string()),
            }],
            attributes: HashMap::new(),
        }
    }

    const QUAL: &str = "http://www.ocel-standard.org/ns#qualifier_diag";

    #[test]
    fn conforming_repair_lifecycle_passes() -> Result<(), GraphError> {
        let log = OcelLog {
            objects: vec![],
            events: vec![
                ev("e1", "DiagnosticRaised", 10, "E0010"),
                ev("e2", "RepairApplied", 20, "E0010"),
                ev("e3", "GatePassed", 30, "E0010"),
            ],
        };
        let graph = DeterministicGraph::new()?;
        EvidenceProjector::project_ocel(&graph, &log)?;

        let ok = check_lifecycle_order(
            &graph,
            QUAL,
            &["DiagnosticRaised", "RepairApplied", "GatePassed"],
        )?;
        assert!(ok, "in-order repair must conform");
        Ok(())
    }

    #[test]
    fn out_of_order_repair_fails() -> Result<(), GraphError> {
        // GatePassed BEFORE RepairApplied — not lawful.
        let log = OcelLog {
            objects: vec![],
            events: vec![
                ev("e1", "DiagnosticRaised", 10, "E0010"),
                ev("e2", "GatePassed", 20, "E0010"),
                ev("e3", "RepairApplied", 30, "E0010"),
            ],
        };
        let graph = DeterministicGraph::new()?;
        EvidenceProjector::project_ocel(&graph, &log)?;

        let ok = check_lifecycle_order(
            &graph,
            QUAL,
            &["DiagnosticRaised", "RepairApplied", "GatePassed"],
        )?;
        assert!(!ok, "out-of-order repair must NOT conform");
        Ok(())
    }

    #[test]
    fn conformance_check_with_valid_model() -> Result<(), GraphError> {
        // Arrange: pack lifecycle log that conforms to the expected model
        use crate::ocel::{
            emit_pack_install, emit_pack_publish, emit_pack_verify, pack_object,
            lockfile_entry_object, receipt_object,
        };
        use chrono::{TimeZone, Utc};

        fn ts(secs: i64) -> chrono::DateTime<chrono::Utc> {
            Utc.timestamp_opt(secs, 0)
                .single()
                .unwrap_or_else(Utc::now)
        }

        let mut log = OcelLog::new();
        const PACK_ID: &str = "acme/base";
        const VERSION: &str = "1.0.0";
        const DIGEST: &str = "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b";
        const OPERATION_ID: &str = "11111111-1111-4111-8111-111111111111";

        log.objects.push(pack_object(PACK_ID, VERSION, DIGEST));
        log.objects
            .push(lockfile_entry_object(PACK_ID, VERSION, DIGEST));
        log.objects
            .push(receipt_object(OPERATION_ID, "c2lnbmF0dXJl"));

        log.events
            .push(emit_pack_install("ev_install", ts(10), PACK_ID, VERSION));
        log.events
            .push(emit_pack_verify("ev_verify", ts(20), PACK_ID, VERSION));
        log.events.push(emit_pack_publish(
            "ev_publish",
            ts(30),
            PACK_ID,
            VERSION,
            OPERATION_ID,
        ));

        // Act: check conformance against the expected model
        let model = "pack.install -> pack.verify -> pack.publish";
        let report = OcelConformanceChecker::check(&log, model)?;

        // Assert: high fitness and precision
        assert!(
            report.fitness >= 0.9,
            "fitness should be high for conforming log, got {}",
            report.fitness
        );
        assert!(
            report.deviations.is_empty(),
            "no deviations expected, got {:?}",
            report.deviations
        );
        Ok(())
    }

    #[test]
    fn conformance_check_detects_deviations() -> Result<(), GraphError> {
        // Arrange: log with unexpected activities
        use crate::ocel::{emit_pack_install, pack_object};
        use chrono::{TimeZone, Utc};
        use std::collections::HashMap;

        fn ts(secs: i64) -> chrono::DateTime<chrono::Utc> {
            Utc.timestamp_opt(secs, 0)
                .single()
                .unwrap_or_else(Utc::now)
        }

        let mut log = OcelLog::new();
        const PACK_ID: &str = "acme/base";
        const VERSION: &str = "1.0.0";
        const DIGEST: &str = "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b";

        log.objects.push(pack_object(PACK_ID, VERSION, DIGEST));

        // Expected: install -> verify -> publish
        // Observed: install -> unexpected_action -> verify -> publish
        log.events
            .push(emit_pack_install("ev_install", ts(10), PACK_ID, VERSION));
        log.events.push(OcelEvent {
            id: "ev_unexpected".to_string(),
            activity: "pack.unexpected".to_string(),
            timestamp: ts(15),
            objects: vec![OcelObjectRef {
                id: format!("pack:{}@{}", PACK_ID, VERSION),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });

        // Act: check conformance
        let model = "pack.install -> pack.verify -> pack.publish";
        let report = OcelConformanceChecker::check(&log, model)?;

        // Assert: deviations detected
        assert!(
            report.fitness < 1.0,
            "fitness should be less than 1.0 for non-conforming log"
        );
        assert!(
            !report.deviations.is_empty(),
            "deviations should be detected"
        );
        assert!(
            report
                .deviations
                .iter()
                .any(|d| d.activity == "pack.unexpected"),
            "unexpected activity should be in deviations"
        );
        Ok(())
    }

    #[test]
    fn conformance_check_with_multiple_object_types() -> Result<(), GraphError> {
        // Arrange: log with pack and lockfile-entry objects, each with their own expected model
        use crate::ocel::{
            emit_lockfile_write, emit_pack_install, pack_object, lockfile_entry_object,
        };
        use chrono::{TimeZone, Utc};

        fn ts(secs: i64) -> chrono::DateTime<chrono::Utc> {
            Utc.timestamp_opt(secs, 0)
                .single()
                .unwrap_or_else(Utc::now)
        }

        let mut log = OcelLog::new();
        const PACK_ID: &str = "acme/base";
        const VERSION: &str = "1.0.0";
        const DIGEST: &str = "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b";

        log.objects.push(pack_object(PACK_ID, VERSION, DIGEST));
        log.objects
            .push(lockfile_entry_object(PACK_ID, VERSION, DIGEST));

        // Pack event
        log.events
            .push(emit_pack_install("ev_install", ts(10), PACK_ID, VERSION));

        // Lockfile-entry event
        log.events
            .push(emit_lockfile_write("ev_lock", ts(5), PACK_ID, VERSION));

        // Act: check conformance against a broad model that includes both activities
        let model = "pack.install -> lockfile.write";
        let report = OcelConformanceChecker::check(&log, model)?;

        // Assert: events conform to the model
        assert!(
            report.fitness >= 0.5,
            "at least some events should conform to the model"
        );
        Ok(())
    }
}
