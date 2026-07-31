//! Directly-Follows Graph (DFG) retrieval — thin call-through to `wasm4pm-compat`.
//!
//! Per `CLAUDE.md`'s Process Intelligence Boundary: ggen EMITS process evidence, it does
//! NOT analyse it. All DFG *discovery* (pairing events, counting directly-follows
//! transitions, frequency aggregation) is owned by
//! `wasm4pm_compat::dfg::discover_ocel_dfg` — the authorized native miner. This module's
//! only job is retrieval: pull the raw event tuples (activity, timestamp, case) out of the
//! OCEL-RDF triplestore via a flat SPARQL `SELECT` (no `COUNT`, no `GROUP BY`, no
//! `FILTER NOT EXISTS` adjacency logic), assemble them into a `wasm4pm_compat::ocel::OCEL`
//! value, and hand that off. The discovery algorithm itself never runs inside `ggen-graph`.

use std::collections::BTreeSet;

use chrono::{DateTime, FixedOffset};
use oxigraph::model::Term;
use oxigraph::sparql::QueryResults;
use wasm4pm_compat::dfg::discover_ocel_dfg;
use wasm4pm_compat::ocel::{OCELEvent, OCELRelationship, OCELType, OCEL};

use crate::graph::DeterministicGraph;
use crate::GraphError;

/// A single directly-follows edge between two activities, with observed count.
///
/// Mirrors `wasm4pm_compat::models::DFGEdge` field-for-field; kept as a local type so
/// existing callers are unaffected by the retrieval-vs-discovery split above.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DfgEdge {
    /// Source activity (the predecessor).
    pub source: String,
    /// Target activity (the immediate successor).
    pub target: String,
    /// Number of times `source` was directly followed by `target`.
    pub frequency: u64,
}

/// Retrieve the raw (event, activity, timestamp, case) tuples for the given case
/// qualifier IRI and discover the directly-follows graph over them.
///
/// Retrieval (this function, SPARQL `SELECT`) and discovery
/// (`wasm4pm_compat::dfg::discover_ocel_dfg`) are deliberately separate: this function
/// performs no pairing, counting, or adjacency computation — see the module doc.
///
/// # Errors
/// Returns a [`GraphError`] if the SPARQL query fails to parse, evaluate, or if an event's
/// timestamp is not valid RFC-3339.
pub fn discover_dfg(
    graph: &DeterministicGraph, case_qualifier_iri: &str,
) -> Result<Vec<DfgEdge>, GraphError> {
    let q = format!(
        r"
        PREFIX ocel: <http://www.ocel-standard.org/ns#>
        SELECT ?e ?a ?t ?case WHERE {{
            ?e ocel:activity ?a ;
               ocel:timestamp ?t ;
               <{q}> ?case .
        }}
        ",
        q = case_qualifier_iri
    );

    let results = graph.query(&q)?;

    let mut events = Vec::new();
    let mut activities = BTreeSet::new();
    if let QueryResults::Solutions(solutions) = results {
        for sol in solutions {
            let sol = sol.map_err(|e| GraphError::Serialization(e.to_string()))?;
            let id = format!("e{}", events.len());
            let activity = literal_value(sol.get("a"));
            let timestamp = literal_value(sol.get("t"));
            let case = literal_value(sol.get("case"));
            if let (Some(activity), Some(timestamp), Some(case)) = (activity, timestamp, case) {
                let time: DateTime<FixedOffset> = DateTime::parse_from_rfc3339(&timestamp)
                    .map_err(|e| GraphError::Serialization(e.to_string()))?;
                activities.insert(activity.clone());
                events.push(OCELEvent {
                    id,
                    event_type: activity,
                    time,
                    attributes: Vec::new(),
                    relationships: vec![OCELRelationship {
                        object_id: case,
                        qualifier: "case".to_string(),
                    }],
                });
            }
        }
    }

    let ocel = OCEL {
        event_types: activities
            .into_iter()
            .map(|name| OCELType {
                name,
                attributes: Vec::new(),
            })
            .collect(),
        object_types: Vec::new(),
        events,
        objects: Vec::new(),
    };

    let dfg = discover_ocel_dfg(&ocel);
    Ok(dfg
        .edges
        .into_iter()
        .map(|e| DfgEdge {
            source: e.source,
            target: e.target,
            frequency: e.frequency as u64,
        })
        .collect())
}

fn literal_value(term: Option<&Term>) -> Option<String> {
    match term {
        Some(Term::Literal(l)) => Some(l.value().to_string()),
        Some(Term::NamedNode(n)) => Some(n.as_str().to_string()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ocel::{EvidenceProjector, OcelEvent, OcelLog, OcelObjectRef};
    use chrono::{TimeZone, Utc};
    use std::collections::HashMap;

    fn ev(id: &str, activity: &str, secs: i64, case: &str) -> OcelEvent {
        OcelEvent {
            id: id.to_string(),
            activity: activity.to_string(),
            timestamp: Utc.timestamp_opt(secs, 0).single().unwrap_or_else(Utc::now),
            objects: vec![OcelObjectRef {
                id: case.to_string(),
                r#type: "case".to_string(),
                qualifier: Some("case".to_string()),
            }],
            attributes: HashMap::new(),
        }
    }

    #[test]
    fn discovers_directly_follows_edges() -> Result<(), GraphError> {
        // Two cases, same A→B→C pattern → DFG edges A→B (2), B→C (2).
        let log = OcelLog {
            objects: vec![],
            events: vec![
                ev("e1", "A", 10, "c1"),
                ev("e2", "B", 20, "c1"),
                ev("e3", "C", 30, "c1"),
                ev("e4", "A", 10, "c2"),
                ev("e5", "B", 20, "c2"),
                ev("e6", "C", 30, "c2"),
            ],
        };
        let graph = DeterministicGraph::new()?;
        EvidenceProjector::project_ocel(&graph, &log)?;

        let qual = "http://www.ocel-standard.org/ns#qualifier_case";
        let edges = discover_dfg(&graph, qual)?;

        let ab = edges.iter().find(|e| e.source == "A" && e.target == "B");
        let bc = edges.iter().find(|e| e.source == "B" && e.target == "C");
        assert!(ab.is_some(), "expected A->B edge, got {edges:?}");
        assert!(bc.is_some(), "expected B->C edge, got {edges:?}");
        assert_eq!(ab.map(|e| e.frequency), Some(2));
        assert_eq!(bc.map(|e| e.frequency), Some(2));
        // No spurious A->C (C does not directly follow A).
        assert!(!edges.iter().any(|e| e.source == "A" && e.target == "C"));
        Ok(())
    }
}
