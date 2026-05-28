//! Lifecycle and repair-route conformance via SPARQL ASK over the OCEL-RDF log.
//!
//! A repair "conforms" when its events occurred in a lawful order for the same
//! correlated object (e.g. `DiagnosticRaised ≺ RepairApplied ≺ GatePassed` for
//! one diagnostic). This is an OCPQ-style temporal constraint expressed as a
//! single SPARQL ASK — the LSP's "did the agent return to lawful motion?" check.

use oxigraph::sparql::QueryResults;

use crate::graph::DeterministicGraph;
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
    graph: &DeterministicGraph,
    case_qualifier_iri: &str,
    activities: &[&str],
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
        r#"
        PREFIX ocel: <http://www.ocel-standard.org/ns#>
        ASK {{
            {patterns}
            FILTER({filter})
        }}
        "#,
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
}
