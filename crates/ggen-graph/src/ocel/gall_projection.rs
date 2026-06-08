//! Projecting self-audit logs to and from the deterministic graph.
//!
//! Provides functions to project an `OCEL` with compliant qualifiers to a graph,
//! extract it back, and query relationship qualifiers.

use crate::ocel::{EvidenceProjector, OCEL};
use crate::{DeterministicGraph, GraphError};

/// Encodes unsafe qualifiers to IRI-compatible percent-encoded strings.
pub fn to_safe_qualifier(q: &str) -> String {
    q.replace('<', "%3C").replace('>', "%3E")
}

/// Decodes percent-encoded qualifiers back to their original forms.
pub fn to_orig_qualifier(q: &str) -> String {
    q.replace("%3C", "<").replace("%3E", ">")
}

/// Projects a self-audit `OCEL` into the `DeterministicGraph` after mapping qualifiers to IRI-safe strings.
pub fn project_self_audit(graph: &DeterministicGraph, log: &OCEL) -> Result<(), GraphError> {
    let mut safe_log = log.clone();
    for ev in &mut safe_log.events {
        for obj_ref in &mut ev.objects {
            if let Some(ref qual) = obj_ref.qualifier {
                obj_ref.qualifier = Some(to_safe_qualifier(qual));
            }
        }
    }
    EvidenceProjector::project_ocel(graph, &safe_log)
}

/// Extracts a self-audit `OCEL` from the `DeterministicGraph` and maps IRI-safe qualifiers back to their original forms.
pub fn extract_self_audit(graph: &DeterministicGraph) -> Result<OCEL, GraphError> {
    let mut log = EvidenceProjector::extract_ocel(graph)?;
    for ev in &mut log.events {
        for obj_ref in &mut ev.objects {
            if let Some(ref qual) = obj_ref.qualifier {
                obj_ref.qualifier = Some(to_orig_qualifier(qual));
            }
        }
    }
    Ok(log)
}

/// Queries the `DeterministicGraph` to find event-object relationship pairs connected by a specific qualifier.
pub fn query_relationship(
    graph: &DeterministicGraph, qualifier: &str,
) -> Result<Vec<(String, String)>, GraphError> {
    let safe_qual = to_safe_qualifier(qualifier);
    let query_str = format!(
        r"
        SELECT ?ev ?obj
        WHERE {{
            ?ev <http://www.ocel-standard.org/ns#qualifier_{}> ?obj .
        }}
        ",
        safe_qual
    );

    let mut results = Vec::new();
    if let oxigraph::sparql::QueryResults::Solutions(solutions) = graph.query(&query_str)? {
        for sol_res in solutions {
            let sol = sol_res?;
            if let (
                Some(oxigraph::model::Term::NamedNode(ev_node)),
                Some(oxigraph::model::Term::NamedNode(obj_node)),
            ) = (sol.get("ev"), sol.get("obj"))
            {
                let ev_uri = ev_node.as_str();
                let obj_uri = obj_node.as_str();
                let ev_id = ev_uri.split('/').next_back().unwrap_or(ev_uri).to_string();
                let obj_id = obj_uri
                    .split('/')
                    .next_back()
                    .unwrap_or(obj_uri)
                    .to_string();
                results.push((ev_id, obj_id));
            }
        }
    }
    results.sort();
    Ok(results)
}
