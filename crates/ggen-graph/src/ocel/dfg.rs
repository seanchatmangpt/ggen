//! Directly-Follows Graph (DFG) discovery via SPARQL over the OCEL-RDF event log.
//!
//! The whole process-mining hot loop for ggen lives in the triplestore: a DFG is
//! a SPARQL query that pairs each event with its immediate successor *within the
//! same case object* and counts the activity transitions. No external
//! process-mining engine is required.
//!
//! Timestamps are projected as RFC-3339 string literals (see
//! [`crate::ocel::projection`]); RFC-3339 is lexicographically ordered for a
//! fixed offset, so `?t1 < ?t2` string comparison is chronological for the
//! UTC-stamped events ggen emits.

use oxigraph::model::Term;
use oxigraph::sparql::QueryResults;

use crate::graph::DeterministicGraph;
use crate::GraphError;

/// A single directly-follows edge between two activities, with observed count.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DfgEdge {
    /// Source activity (the predecessor).
    pub source: String,
    /// Target activity (the immediate successor).
    pub target: String,
    /// Number of times `source` was directly followed by `target`.
    pub frequency: u64,
}

/// Discover the directly-follows graph over events correlated by a shared case
/// object, identified by the e2o qualifier predicate `qualifier`.
///
/// "Directly follows" = `?e1` and `?e2` are related to the same case object,
/// `t1 < t2`, and no third event of that case falls strictly between them.
///
/// # Errors
/// Returns a [`GraphError`] if the SPARQL query fails to parse or evaluate.
pub fn discover_dfg(
    graph: &DeterministicGraph,
    case_qualifier_iri: &str,
) -> Result<Vec<DfgEdge>, GraphError> {
    let q = format!(
        r#"
        PREFIX ocel: <http://www.ocel-standard.org/ns#>
        SELECT ?a1 ?a2 (COUNT(*) AS ?freq) WHERE {{
            ?e1 ocel:activity ?a1 ;
                ocel:timestamp ?t1 ;
                <{q}> ?case .
            ?e2 ocel:activity ?a2 ;
                ocel:timestamp ?t2 ;
                <{q}> ?case .
            FILTER(?t1 < ?t2)
            FILTER NOT EXISTS {{
                ?e3 ocel:timestamp ?t3 ; <{q}> ?case .
                FILTER(?t1 < ?t3 && ?t3 < ?t2)
            }}
        }}
        GROUP BY ?a1 ?a2
        ORDER BY DESC(?freq)
        "#,
        q = case_qualifier_iri
    );

    let results = graph.query(&q)?;

    let mut edges = Vec::new();
    if let QueryResults::Solutions(solutions) = results {
        for sol in solutions {
            let sol = sol.map_err(|e| GraphError::Serialization(e.to_string()))?;
            let source = literal_value(sol.get("a1"));
            let target = literal_value(sol.get("a2"));
            let frequency = sol
                .get("freq")
                .and_then(term_to_u64)
                .unwrap_or(0);
            if let (Some(source), Some(target)) = (source, target) {
                edges.push(DfgEdge {
                    source,
                    target,
                    frequency,
                });
            }
        }
    }
    Ok(edges)
}

fn literal_value(term: Option<&Term>) -> Option<String> {
    match term {
        Some(Term::Literal(l)) => Some(l.value().to_string()),
        Some(Term::NamedNode(n)) => Some(n.as_str().to_string()),
        _ => None,
    }
}

fn term_to_u64(term: &Term) -> Option<u64> {
    match term {
        Term::Literal(l) => l.value().parse::<u64>().ok(),
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
