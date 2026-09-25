//! Semantic Procedural Graph (SPG) validation, semantic diff, and projection envelopes.
//!
//! SPG is an interchange boundary. It does not replace HDDL, FOND, TLA+,
//! OCEL 2.0, SA2A, or BRCE, and structural validation never grants runtime
//! authority or execution standing.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};

/// SPG validation/compilation error.
#[derive(Debug, thiserror::Error)]
pub enum SpgError {
    /// Input JSON could not be decoded.
    #[error("REFUSED:SPG_JSON:{0}")]
    Json(#[from] serde_json::Error),
    /// Structural law was violated.
    #[error("{0}")]
    Refused(String),
}

/// One semantic procedure node.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SpgNode {
    /// Stable node identity.
    pub id: String,
    /// OBSERVE, SELECT, CONSTRUCT, or DO.
    pub class: String,
    /// Capability semantic identity.
    pub capability: String,
}

/// One typed semantic transition.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SpgEdge {
    /// Stable edge identity.
    pub id: String,
    /// Source node identity.
    pub from: String,
    /// Target node identity.
    pub to: String,
    /// LEADS_TO, TRIGGERS, PROVIDES_INPUT_FOR, or CONVERGES_TO.
    pub relation: String,
    /// Guard/precondition expression.
    pub guard: String,
    /// Evidence required before transition.
    pub evidence_required: Vec<String>,
    /// Named authority requirement; NONE for powerless edges.
    pub authority_required: String,
    /// observational, constructive, or consequential.
    pub consequence: String,
    /// Whether the transition requires a consequence receipt.
    pub receipt_required: bool,
    /// Falsifier for the edge, mandatory for consequential transitions.
    #[serde(default)]
    pub falsifier: Option<String>,
}

/// Canonical SPG JSON envelope.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SpgGraph {
    /// Schema identity.
    pub schema: String,
    /// Stable graph identity.
    pub id: String,
    /// Graph version.
    pub version: String,
    /// CANDIDATE, ADMITTED, or REFUSED.
    pub state: String,
    /// Evidence standing. Structural compiler refuses self-ALIVE graphs.
    pub standing: String,
    /// Semantic nodes.
    pub nodes: Vec<SpgNode>,
    /// Typed transitions.
    pub edges: Vec<SpgEdge>,
    /// Formalism/runtime projection bindings by family.
    pub projections: BTreeMap<String, BTreeMap<String, String>>,
    /// Prior-art admission records. Their deeper semantics are owned by the
    /// prior-art court.
    #[serde(default)]
    pub prior_art: Vec<Value>,
}

/// Behaviorally relevant semantic delta.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SpgDiff {
    /// Added node identities.
    pub added_nodes: Vec<String>,
    /// Removed node identities.
    pub removed_nodes: Vec<String>,
    /// Added edge identities.
    pub added_edges: Vec<String>,
    /// Removed edge identities.
    pub removed_edges: Vec<String>,
    /// Node fields whose meaning changed.
    pub changed_nodes: BTreeMap<String, Vec<String>>,
    /// Edge fields whose meaning changed.
    pub changed_edges: BTreeMap<String, Vec<String>>,
}

/// Deterministic projection envelope emitted by ggen.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProjectionEnvelope {
    /// Projection schema identity.
    pub schema: String,
    /// Exact SPG semantic identity.
    pub source_graph: String,
    /// Exact SPG version.
    pub source_version: String,
    /// Target formalism/runtime family.
    pub family: String,
    /// Node identity -> target identity bindings.
    pub bindings: BTreeMap<String, String>,
    /// Construction state; never execution standing.
    pub state: String,
    /// Explicit standing ceiling.
    pub standing: String,
    /// Binding adjacency is not equivalence by itself.
    pub semantic_equivalence: String,
}

fn refused(code: &str) -> SpgError {
    SpgError::Refused(format!("REFUSED:{code}"))
}

/// Decode an SPG from JSON bytes.
pub fn from_json(bytes: &[u8]) -> Result<SpgGraph, SpgError> {
    Ok(serde_json::from_slice(bytes)?)
}

/// Validate structural SPG law.
///
/// Success means only ADMITTED_STRUCTURE; it does not grant DO authority,
/// runtime execution, projection equivalence, or production standing.
pub fn validate(graph: &SpgGraph) -> Result<(), SpgError> {
    if graph.schema != "chatman.spg.v1" {
        return Err(refused("SPG_SCHEMA"));
    }
    if graph.id.trim().is_empty() {
        return Err(refused("SPG_ID"));
    }
    if graph.version.trim().is_empty() {
        return Err(refused("SPG_VERSION"));
    }
    if !matches!(graph.state.as_str(), "CANDIDATE" | "ADMITTED" | "REFUSED") {
        return Err(refused("SPG_STATE"));
    }
    if graph.standing == "ALIVE" {
        return Err(refused("SPG_SELF_STANDING"));
    }
    if graph.nodes.is_empty() {
        return Err(refused("SPG_NODES"));
    }
    if graph.edges.is_empty() {
        return Err(refused("SPG_EDGES"));
    }
    if graph.prior_art.is_empty() {
        return Err(refused("SPG_PRIOR_ART_MISSING"));
    }

    let mut node_ids = BTreeSet::new();
    for node in &graph.nodes {
        if node.id.trim().is_empty() || !node_ids.insert(node.id.clone()) {
            return Err(refused("SPG_NODE_ID"));
        }
        if !matches!(
            node.class.as_str(),
            "OBSERVE" | "SELECT" | "CONSTRUCT" | "DO"
        ) {
            return Err(SpgError::Refused(format!(
                "REFUSED:SPG_NODE_CLASS:{}",
                node.id
            )));
        }
        if node.capability.trim().is_empty() {
            return Err(SpgError::Refused(format!(
                "REFUSED:SPG_NODE_CAPABILITY:{}",
                node.id
            )));
        }
    }

    let mut edge_ids = BTreeSet::new();
    for edge in &graph.edges {
        if edge.id.trim().is_empty() || !edge_ids.insert(edge.id.clone()) {
            return Err(refused("SPG_EDGE_ID"));
        }
        if !node_ids.contains(&edge.from) {
            return Err(SpgError::Refused(format!(
                "REFUSED:SPG_EDGE_FROM:{}",
                edge.id
            )));
        }
        if !node_ids.contains(&edge.to) {
            return Err(SpgError::Refused(format!(
                "REFUSED:SPG_EDGE_TO:{}",
                edge.id
            )));
        }
        if !matches!(
            edge.relation.as_str(),
            "LEADS_TO" | "TRIGGERS" | "PROVIDES_INPUT_FOR" | "CONVERGES_TO"
        ) {
            return Err(SpgError::Refused(format!(
                "REFUSED:SPG_RELATION:{}",
                edge.id
            )));
        }
        if edge.guard.trim().is_empty() {
            return Err(SpgError::Refused(format!("REFUSED:SPG_GUARD:{}", edge.id)));
        }
        if edge.evidence_required.is_empty() {
            return Err(SpgError::Refused(format!(
                "REFUSED:SPG_EVIDENCE:{}",
                edge.id
            )));
        }
        if edge.authority_required.trim().is_empty() {
            return Err(SpgError::Refused(format!(
                "REFUSED:SPG_AUTHORITY:{}",
                edge.id
            )));
        }
        if !matches!(
            edge.consequence.as_str(),
            "observational" | "constructive" | "consequential"
        ) {
            return Err(SpgError::Refused(format!(
                "REFUSED:SPG_CONSEQUENCE:{}",
                edge.id
            )));
        }
        if edge.consequence == "consequential" {
            if edge.authority_required == "NONE" {
                return Err(SpgError::Refused(format!(
                    "REFUSED:CONSEQUENCE_WITHOUT_AUTHORITY:{}",
                    edge.id
                )));
            }
            if !edge.receipt_required {
                return Err(SpgError::Refused(format!(
                    "REFUSED:CONSEQUENCE_WITHOUT_RECEIPT:{}",
                    edge.id
                )));
            }
            if edge
                .falsifier
                .as_deref()
                .map_or(true, |value| value.trim().is_empty())
            {
                return Err(SpgError::Refused(format!(
                    "REFUSED:CONSEQUENCE_WITHOUT_FALSIFIER:{}",
                    edge.id
                )));
            }
        }
    }

    for (family, bindings) in &graph.projections {
        if family.trim().is_empty() || bindings.is_empty() {
            return Err(SpgError::Refused(format!(
                "REFUSED:SPG_PROJECTION_EMPTY:{family}"
            )));
        }
        for node_id in bindings.keys() {
            if !node_ids.contains(node_id) {
                return Err(SpgError::Refused(format!(
                    "REFUSED:SPG_PROJECTION_DANGLING:{family}:{node_id}"
                )));
            }
        }
    }
    Ok(())
}

/// Compute a deterministic semantic diff by stable node/edge identity.
pub fn semantic_diff(old: &SpgGraph, new: &SpgGraph) -> SpgDiff {
    let old_nodes: BTreeMap<_, _> = old.nodes.iter().map(|item| (&item.id, item)).collect();
    let new_nodes: BTreeMap<_, _> = new.nodes.iter().map(|item| (&item.id, item)).collect();
    let old_edges: BTreeMap<_, _> = old.edges.iter().map(|item| (&item.id, item)).collect();
    let new_edges: BTreeMap<_, _> = new.edges.iter().map(|item| (&item.id, item)).collect();

    let added_nodes = new_nodes
        .keys()
        .filter(|id| !old_nodes.contains_key(*id))
        .map(|id| (*id).clone())
        .collect();
    let removed_nodes = old_nodes
        .keys()
        .filter(|id| !new_nodes.contains_key(*id))
        .map(|id| (*id).clone())
        .collect();
    let added_edges = new_edges
        .keys()
        .filter(|id| !old_edges.contains_key(*id))
        .map(|id| (*id).clone())
        .collect();
    let removed_edges = old_edges
        .keys()
        .filter(|id| !new_edges.contains_key(*id))
        .map(|id| (*id).clone())
        .collect();

    let mut changed_nodes = BTreeMap::new();
    for (id, old_node) in &old_nodes {
        let Some(new_node) = new_nodes.get(id) else {
            continue;
        };
        let mut fields = Vec::new();
        if old_node.class != new_node.class {
            fields.push("class".to_owned());
        }
        if old_node.capability != new_node.capability {
            fields.push("capability".to_owned());
        }
        if !fields.is_empty() {
            changed_nodes.insert((*id).clone(), fields);
        }
    }

    let mut changed_edges = BTreeMap::new();
    for (id, old_edge) in &old_edges {
        let Some(new_edge) = new_edges.get(id) else {
            continue;
        };
        let mut fields = Vec::new();
        if old_edge.from != new_edge.from {
            fields.push("from".to_owned());
        }
        if old_edge.to != new_edge.to {
            fields.push("to".to_owned());
        }
        if old_edge.relation != new_edge.relation {
            fields.push("relation".to_owned());
        }
        if old_edge.guard != new_edge.guard {
            fields.push("guard".to_owned());
        }
        if old_edge.evidence_required != new_edge.evidence_required {
            fields.push("evidence_required".to_owned());
        }
        if old_edge.authority_required != new_edge.authority_required {
            fields.push("authority_required".to_owned());
        }
        if old_edge.consequence != new_edge.consequence {
            fields.push("consequence".to_owned());
        }
        if old_edge.receipt_required != new_edge.receipt_required {
            fields.push("receipt_required".to_owned());
        }
        if old_edge.falsifier != new_edge.falsifier {
            fields.push("falsifier".to_owned());
        }
        if !fields.is_empty() {
            changed_edges.insert((*id).clone(), fields);
        }
    }

    SpgDiff {
        added_nodes,
        removed_nodes,
        added_edges,
        removed_edges,
        changed_nodes,
        changed_edges,
    }
}

/// Compile one declared projection family into a deterministic envelope.
///
/// The output explicitly carries semantic_equivalence = UNCLAIMED; a binding
/// proves only correspondence declared by the source SPG.
pub fn compile_projection(graph: &SpgGraph, family: &str) -> Result<ProjectionEnvelope, SpgError> {
    validate(graph)?;
    let bindings = graph
        .projections
        .get(family)
        .ok_or_else(|| SpgError::Refused(format!("REFUSED:SPG_PROJECTION_UNSUPPORTED:{family}")))?
        .clone();

    Ok(ProjectionEnvelope {
        schema: "chatman.spg-projection.v1".to_owned(),
        source_graph: graph.id.clone(),
        source_version: graph.version.clone(),
        family: family.to_owned(),
        bindings,
        state: "CONSTRUCTED".to_owned(),
        standing: "NONE".to_owned(),
        semantic_equivalence: "UNCLAIMED".to_owned(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture() -> Result<SpgGraph, SpgError> {
        from_json(
            br#"{
              "schema":"chatman.spg.v1",
              "id":"spg:test",
              "version":"1",
              "state":"CANDIDATE",
              "standing":"NONE",
              "nodes":[
                {"id":"select","class":"SELECT","capability":"capability.select"},
                {"id":"do","class":"DO","capability":"brce.do"}
              ],
              "edges":[{
                "id":"e1","from":"select","to":"do","relation":"TRIGGERS",
                "guard":"admitted","evidence_required":["grant"],
                "authority_required":"BRCE_GRANT","consequence":"consequential",
                "receipt_required":true,"falsifier":"unreceipted consequence"
              }],
              "projections":{"brce":{"select":"command.admit","do":"command.do"}},
              "prior_art":[{"disposition":"REUSE"}]
            }"#,
        )
    }

    #[test]
    fn validates_bounded_graph() -> Result<(), SpgError> {
        let graph = fixture()?;
        validate(&graph)
    }

    #[test]
    fn refuses_unreceipted_consequence() -> Result<(), SpgError> {
        let mut graph = fixture()?;
        graph.edges[0].receipt_required = false;
        let result = validate(&graph);
        assert!(matches!(
            result,
            Err(SpgError::Refused(message)) if message.contains("CONSEQUENCE_WITHOUT_RECEIPT")
        ));
        Ok(())
    }

    #[test]
    fn projection_does_not_claim_equivalence() -> Result<(), SpgError> {
        let graph = fixture()?;
        let projection = compile_projection(&graph, "brce")?;
        assert_eq!(projection.semantic_equivalence, "UNCLAIMED");
        assert_eq!(projection.standing, "NONE");
        Ok(())
    }

    #[test]
    fn diff_surfaces_authority_change() -> Result<(), SpgError> {
        let old = fixture()?;
        let mut new = old.clone();
        new.edges[0].authority_required = "OTHER_GRANT".to_owned();
        let delta = semantic_diff(&old, &new);
        assert_eq!(
            delta.changed_edges.get("e1"),
            Some(&vec!["authority_required".to_owned()])
        );
        Ok(())
    }
}
