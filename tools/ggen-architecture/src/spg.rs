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


/// Immutable Git + graph subject for rewrite manufacture.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SpgExactSubject {
    /// Repository identity in owner/name form.
    pub repository: String,
    /// Exact lowercase 40-hex Git commit.
    pub commit: String,
    /// Canonical source graph digest.
    pub graph_digest: String,
}

/// One deterministic graph rewrite operation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum SpgRewriteOperation {
    /// Remove an edge before removing any node it references.
    RemoveEdge { id: String },
    /// Remove a node after incident edges are removed.
    RemoveNode { id: String },
    /// Replace a node with the same stable identity.
    ReplaceNode { node: SpgNode },
    /// Add a new node.
    AddNode { node: SpgNode },
    /// Replace an edge with the same stable identity.
    ReplaceEdge { edge: SpgEdge },
    /// Add a new edge.
    AddEdge { edge: SpgEdge },
    /// Replace or remove one projection family.
    SetProjection {
        family: String,
        bindings: Option<BTreeMap<String, String>>,
    },
    /// Replace the prior-art evidence sequence.
    SetPriorArt { prior_art: Vec<Value> },
    /// Set the target semantic version after payload rewrites.
    SetVersion { version: String },
}

/// Exact-subject deterministic graph rewrite plan.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SpgRewritePlan {
    /// Rewrite-plan schema.
    pub schema: String,
    /// Immutable source repository/commit/graph binding.
    pub source_subject: SpgExactSubject,
    /// Stable graph identity.
    pub source_graph: String,
    /// Source graph version.
    pub source_version: String,
    /// Target graph version.
    pub target_version: String,
    /// Expected canonical target graph digest.
    pub target_graph_digest: String,
    /// Semantic delta retained for review/qualification.
    pub semantic_diff: SpgDiff,
    /// Deterministic ordered rewrite operations.
    pub operations: Vec<SpgRewriteOperation>,
    /// Any manufactured semantic rewrite requires external requalification.
    pub requires_requalification: bool,
    /// Rewrite manufacture never grants DO authority.
    pub grants_do_authority: bool,
    /// A rewrite plan has no runtime standing.
    pub standing: String,
}

/// Replay evidence for a graph rewrite.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SpgReplayReceipt {
    /// Replay receipt schema.
    pub schema: String,
    /// Immutable source subject.
    pub source_subject: SpgExactSubject,
    /// Expected target graph digest.
    pub target_graph_digest: String,
    /// Digest of the exact rewrite plan.
    pub plan_digest: String,
    /// Digest observed on first replay.
    pub first_replay_digest: String,
    /// Digest observed on second replay from the same immutable source.
    pub second_replay_digest: String,
    /// True only when both replays produce byte-identical canonical target bytes.
    pub second_run_byte_identical: bool,
    /// Semantic changes remain candidates until requalified externally.
    pub requires_requalification: bool,
    /// Replay never grants actuation authority.
    pub authority: String,
    /// Replay evidence alone is not production standing.
    pub standing: String,
}

fn valid_git_commit(value: &str) -> bool {
    value.len() == 40
        && value
            .chars()
            .all(|ch| ch.is_ascii_hexdigit() && !ch.is_ascii_uppercase())
}

fn valid_blake3_digest(value: &str) -> bool {
    value
        .strip_prefix("blake3:")
        .is_some_and(|hex| {
            hex.len() == 64
                && hex
                    .chars()
                    .all(|ch| ch.is_ascii_hexdigit() && !ch.is_ascii_uppercase())
        })
}

fn validate_exact_subject(subject: &SpgExactSubject) -> Result<(), SpgError> {
    if subject.repository.trim().is_empty() {
        return Err(refused("SPG_SUBJECT_REPOSITORY"));
    }
    if !valid_git_commit(&subject.commit) {
        return Err(refused("SPG_SUBJECT_IMMUTABLE_COMMIT"));
    }
    if !valid_blake3_digest(&subject.graph_digest) {
        return Err(refused("SPG_SUBJECT_GRAPH_DIGEST"));
    }
    Ok(())
}

fn normalized_graph(graph: &SpgGraph) -> SpgGraph {
    let mut normalized = graph.clone();
    normalized.nodes.sort_by(|left, right| left.id.cmp(&right.id));
    normalized.edges.sort_by(|left, right| left.id.cmp(&right.id));
    normalized
}

/// Canonical bytes for an admitted SPG structure.
///
/// Stable node/edge identities, rather than input vector ordering, determine
/// the byte representation used by rewrite receipts.
pub fn canonical_graph_bytes(graph: &SpgGraph) -> Result<Vec<u8>, SpgError> {
    validate(graph)?;
    Ok(serde_json::to_vec(&normalized_graph(graph))?)
}

/// Canonical BLAKE3 digest for an SPG graph.
pub fn graph_digest(graph: &SpgGraph) -> Result<String, SpgError> {
    let bytes = canonical_graph_bytes(graph)?;
    Ok(format!("blake3:{}", blake3::hash(&bytes).to_hex()))
}

/// Canonical BLAKE3 digest for a deterministic rewrite plan.
pub fn rewrite_plan_digest(plan: &SpgRewritePlan) -> Result<String, SpgError> {
    let bytes = serde_json::to_vec(plan)?;
    Ok(format!("blake3:{}", blake3::hash(&bytes).to_hex()))
}

fn operation_changes_semantics(operation: &SpgRewriteOperation) -> bool {
    !matches!(operation, SpgRewriteOperation::SetVersion { .. })
}

/// Manufacture an exact-subject deterministic rewrite plan.
///
/// Graph identity, admission state, and evidence standing cannot be changed by
/// rewrite manufacture. Those transitions belong to external admission
/// machinery. Any semantic operation is marked as requiring requalification.
pub fn plan_rewrite(
    old: &SpgGraph,
    new: &SpgGraph,
    source_subject: SpgExactSubject,
) -> Result<SpgRewritePlan, SpgError> {
    validate(old)?;
    validate(new)?;
    validate_exact_subject(&source_subject)?;

    let observed_source_digest = graph_digest(old)?;
    if observed_source_digest != source_subject.graph_digest {
        return Err(refused("SPG_REWRITE_SOURCE_DIGEST_MISMATCH"));
    }
    if old.schema != new.schema {
        return Err(refused("SPG_REWRITE_SCHEMA_CHANGE"));
    }
    if old.id != new.id {
        return Err(refused("SPG_REWRITE_IDENTITY_CHANGE"));
    }
    if old.state != new.state {
        return Err(refused("SPG_REWRITE_ADMISSION_STATE_CHANGE"));
    }
    if old.standing != new.standing {
        return Err(refused("SPG_REWRITE_STANDING_CHANGE"));
    }

    let diff = semantic_diff(old, new);
    let old_nodes: BTreeMap<_, _> = old.nodes.iter().map(|item| (&item.id, item)).collect();
    let new_nodes: BTreeMap<_, _> = new.nodes.iter().map(|item| (&item.id, item)).collect();
    let old_edges: BTreeMap<_, _> = old.edges.iter().map(|item| (&item.id, item)).collect();
    let new_edges: BTreeMap<_, _> = new.edges.iter().map(|item| (&item.id, item)).collect();

    let mut operations = Vec::new();

    for id in old_edges.keys().filter(|id| !new_edges.contains_key(*id)) {
        operations.push(SpgRewriteOperation::RemoveEdge {
            id: (*id).clone(),
        });
    }
    for id in old_nodes.keys().filter(|id| !new_nodes.contains_key(*id)) {
        operations.push(SpgRewriteOperation::RemoveNode {
            id: (*id).clone(),
        });
    }
    for (id, old_node) in &old_nodes {
        if let Some(new_node) = new_nodes.get(id) {
            if *old_node != *new_node {
                operations.push(SpgRewriteOperation::ReplaceNode {
                    node: (*new_node).clone(),
                });
            }
        }
    }
    for id in new_nodes.keys().filter(|id| !old_nodes.contains_key(*id)) {
        operations.push(SpgRewriteOperation::AddNode {
            node: (*new_nodes[id]).clone(),
        });
    }
    for (id, old_edge) in &old_edges {
        if let Some(new_edge) = new_edges.get(id) {
            if *old_edge != *new_edge {
                operations.push(SpgRewriteOperation::ReplaceEdge {
                    edge: (*new_edge).clone(),
                });
            }
        }
    }
    for id in new_edges.keys().filter(|id| !old_edges.contains_key(*id)) {
        operations.push(SpgRewriteOperation::AddEdge {
            edge: (*new_edges[id]).clone(),
        });
    }

    let projection_families: BTreeSet<_> = old
        .projections
        .keys()
        .chain(new.projections.keys())
        .cloned()
        .collect();
    for family in projection_families {
        if old.projections.get(&family) != new.projections.get(&family) {
            operations.push(SpgRewriteOperation::SetProjection {
                family: family.clone(),
                bindings: new.projections.get(&family).cloned(),
            });
        }
    }

    if old.prior_art != new.prior_art {
        operations.push(SpgRewriteOperation::SetPriorArt {
            prior_art: new.prior_art.clone(),
        });
    }

    let has_semantic_change = operations.iter().any(operation_changes_semantics);
    if has_semantic_change && old.version == new.version {
        return Err(refused("SPG_REWRITE_VERSION_NOT_BUMPED"));
    }
    if old.version != new.version {
        operations.push(SpgRewriteOperation::SetVersion {
            version: new.version.clone(),
        });
    }

    let requires_requalification = operations.iter().any(operation_changes_semantics);

    Ok(SpgRewritePlan {
        schema: "chatman.spg-rewrite-plan.v1".to_owned(),
        source_subject,
        source_graph: old.id.clone(),
        source_version: old.version.clone(),
        target_version: new.version.clone(),
        target_graph_digest: graph_digest(new)?,
        semantic_diff: diff,
        operations,
        requires_requalification,
        grants_do_authority: false,
        standing: "NONE".to_owned(),
    })
}

fn find_node(graph: &SpgGraph, id: &str) -> Option<usize> {
    graph.nodes.iter().position(|node| node.id == id)
}

fn find_edge(graph: &SpgGraph, id: &str) -> Option<usize> {
    graph.edges.iter().position(|edge| edge.id == id)
}

/// Apply a rewrite plan only to its exact immutable source.
///
/// This function constructs a candidate target graph. It never changes graph
/// admission state or standing and never actuates the resulting procedure.
pub fn apply_rewrite(
    source: &SpgGraph,
    plan: &SpgRewritePlan,
) -> Result<SpgGraph, SpgError> {
    validate(source)?;
    validate_exact_subject(&plan.source_subject)?;
    if plan.schema != "chatman.spg-rewrite-plan.v1" {
        return Err(refused("SPG_REWRITE_PLAN_SCHEMA"));
    }
    if plan.grants_do_authority || plan.standing != "NONE" {
        return Err(refused("SPG_REWRITE_AMBIENT_AUTHORITY"));
    }
    if source.id != plan.source_graph || source.version != plan.source_version {
        return Err(refused("SPG_REWRITE_SOURCE_IDENTITY_MISMATCH"));
    }
    if graph_digest(source)? != plan.source_subject.graph_digest {
        return Err(refused("SPG_REWRITE_SOURCE_DIGEST_MISMATCH"));
    }

    let mut target = source.clone();
    for operation in &plan.operations {
        match operation {
            SpgRewriteOperation::RemoveEdge { id } => {
                let Some(index) = find_edge(&target, id) else {
                    return Err(SpgError::Refused(format!(
                        "REFUSED:SPG_REWRITE_EDGE_NOT_FOUND:{id}"
                    )));
                };
                target.edges.remove(index);
            }
            SpgRewriteOperation::RemoveNode { id } => {
                if target
                    .edges
                    .iter()
                    .any(|edge| edge.from == *id || edge.to == *id)
                {
                    return Err(SpgError::Refused(format!(
                        "REFUSED:SPG_REWRITE_NODE_STILL_REFERENCED:{id}"
                    )));
                }
                let Some(index) = find_node(&target, id) else {
                    return Err(SpgError::Refused(format!(
                        "REFUSED:SPG_REWRITE_NODE_NOT_FOUND:{id}"
                    )));
                };
                target.nodes.remove(index);
            }
            SpgRewriteOperation::ReplaceNode { node } => {
                let Some(index) = find_node(&target, &node.id) else {
                    return Err(SpgError::Refused(format!(
                        "REFUSED:SPG_REWRITE_NODE_NOT_FOUND:{}",
                        node.id
                    )));
                };
                target.nodes[index] = node.clone();
            }
            SpgRewriteOperation::AddNode { node } => {
                if find_node(&target, &node.id).is_some() {
                    return Err(SpgError::Refused(format!(
                        "REFUSED:SPG_REWRITE_NODE_ALREADY_EXISTS:{}",
                        node.id
                    )));
                }
                target.nodes.push(node.clone());
            }
            SpgRewriteOperation::ReplaceEdge { edge } => {
                let Some(index) = find_edge(&target, &edge.id) else {
                    return Err(SpgError::Refused(format!(
                        "REFUSED:SPG_REWRITE_EDGE_NOT_FOUND:{}",
                        edge.id
                    )));
                };
                target.edges[index] = edge.clone();
            }
            SpgRewriteOperation::AddEdge { edge } => {
                if find_edge(&target, &edge.id).is_some() {
                    return Err(SpgError::Refused(format!(
                        "REFUSED:SPG_REWRITE_EDGE_ALREADY_EXISTS:{}",
                        edge.id
                    )));
                }
                target.edges.push(edge.clone());
            }
            SpgRewriteOperation::SetProjection { family, bindings } => {
                if let Some(bindings) = bindings {
                    target.projections.insert(family.clone(), bindings.clone());
                } else {
                    target.projections.remove(family);
                }
            }
            SpgRewriteOperation::SetPriorArt { prior_art } => {
                target.prior_art.clone_from(prior_art);
            }
            SpgRewriteOperation::SetVersion { version } => {
                target.version.clone_from(version);
            }
        }
    }

    target.nodes.sort_by(|left, right| left.id.cmp(&right.id));
    target.edges.sort_by(|left, right| left.id.cmp(&right.id));
    validate(&target)?;

    if target.version != plan.target_version {
        return Err(refused("SPG_REWRITE_TARGET_VERSION_MISMATCH"));
    }
    if graph_digest(&target)? != plan.target_graph_digest {
        return Err(refused("SPG_REWRITE_TARGET_DIGEST_MISMATCH"));
    }
    Ok(target)
}

/// Replay a rewrite twice from the same exact source and manufacture evidence.
pub fn replay_rewrite(
    source: &SpgGraph,
    plan: &SpgRewritePlan,
) -> Result<(SpgGraph, SpgReplayReceipt), SpgError> {
    let first = apply_rewrite(source, plan)?;
    let second = apply_rewrite(source, plan)?;
    let first_bytes = canonical_graph_bytes(&first)?;
    let second_bytes = canonical_graph_bytes(&second)?;
    let first_digest = graph_digest(&first)?;
    let second_digest = graph_digest(&second)?;
    let identical = first_bytes == second_bytes && first_digest == second_digest;
    if !identical {
        return Err(refused("SPG_REWRITE_NONDETERMINISTIC_REPLAY"));
    }

    Ok((
        first,
        SpgReplayReceipt {
            schema: "chatman.spg-rewrite-replay.v1".to_owned(),
            source_subject: plan.source_subject.clone(),
            target_graph_digest: plan.target_graph_digest.clone(),
            plan_digest: rewrite_plan_digest(plan)?,
            first_replay_digest: first_digest,
            second_replay_digest: second_digest,
            second_run_byte_identical: true,
            requires_requalification: plan.requires_requalification,
            authority: "NONE".to_owned(),
            standing: "NONE".to_owned(),
        },
    ))
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
