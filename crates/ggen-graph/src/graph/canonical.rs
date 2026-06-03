#![allow(clippy::unwrap_used, clippy::match_wildcard_for_single_variants)]
//! Canonical sorting and formatting of RDF quads and datasets.

use oxigraph::model::{BlankNode, GraphName, NamedOrBlankNode, Quad, Term};
use std::collections::{HashMap, HashSet};

/// Sorts a slice of quads in place using their canonical string representation.
pub fn sort_quads_canonically(quads: &mut [Quad]) {
    let canonical = canonicalize_quads(quads);
    quads.clone_from_slice(&canonical);
}

/// Converts a slice of quads into a canonical, alphabetically sorted N-Quads string.
pub fn to_canonical_nquads_string(quads: &[Quad]) -> String {
    let canonical = canonicalize_quads(quads);
    let mut result = String::new();
    for q in canonical {
        result.push_str(&q.to_string());
        result.push('\n');
    }
    result
}

/// Deterministically canonicalizes a slice of quads, renaming blank nodes stably
/// based on their incoming/outgoing edges (color refinement/neighborhood signing)
/// and sorting them lexicographically.
pub fn canonicalize_quads(quads: &[Quad]) -> Vec<Quad> {
    // 1. Identify all blank nodes in the input
    let mut blank_nodes = HashSet::new();
    for q in quads {
        if let NamedOrBlankNode::BlankNode(b) = &q.subject {
            blank_nodes.insert(b.clone());
        }
        if let Term::BlankNode(b) = &q.object {
            blank_nodes.insert(b.clone());
        }
        if let GraphName::BlankNode(b) = &q.graph_name {
            blank_nodes.insert(b.clone());
        }
    }

    if blank_nodes.is_empty() {
        let mut sorted = quads.to_vec();
        sorted.sort_by_key(|q| q.to_string());
        return sorted;
    }

    // 2. Deterministic blank node renaming using neighborhood signature hashing
    let mut bnode_labels: HashMap<BlankNode, String> = blank_nodes
        .iter()
        .map(|b| (b.clone(), "bnode".to_string()))
        .collect();

    // 5 iterations is typically sufficient for small graphs
    for _ in 0..5 {
        let mut next_labels = HashMap::new();
        for bnode in &blank_nodes {
            let mut neighborhood = Vec::new();
            for q in quads {
                let contains_bnode = (match &q.subject {
                    NamedOrBlankNode::BlankNode(b) => b == bnode,
                    _ => false,
                }) || (match &q.object {
                    Term::BlankNode(b) => b == bnode,
                    _ => false,
                }) || (match &q.graph_name {
                    GraphName::BlankNode(b) => b == bnode,
                    _ => false,
                });

                if contains_bnode {
                    let s_term = match &q.subject {
                        NamedOrBlankNode::BlankNode(b) => {
                            if b == bnode {
                                "_:self".to_string()
                            } else {
                                format!("_:{}", bnode_labels.get(b).unwrap())
                            }
                        }
                        NamedOrBlankNode::NamedNode(n) => n.to_string(),
                    };
                    let p_term = q.predicate.to_string();
                    let o_term = match &q.object {
                        Term::BlankNode(b) => {
                            if b == bnode {
                                "_:self".to_string()
                            } else {
                                format!("_:{}", bnode_labels.get(b).unwrap())
                            }
                        }
                        Term::NamedNode(n) => n.to_string(),
                        Term::Literal(l) => l.to_string(),
                        other => other.to_string(),
                    };
                    let g_term = match &q.graph_name {
                        GraphName::DefaultGraph => "".to_string(),
                        GraphName::NamedNode(n) => n.to_string(),
                        GraphName::BlankNode(b) => {
                            if b == bnode {
                                "_:self".to_string()
                            } else {
                                format!("_:{}", bnode_labels.get(b).unwrap())
                            }
                        }
                    };
                    neighborhood.push(format!("{} {} {} {}", s_term, p_term, o_term, g_term));
                }
            }
            neighborhood.sort();
            let signature = blake3::hash(neighborhood.join("\n").as_bytes())
                .to_hex()
                .to_string();
            next_labels.insert(bnode.clone(), signature);
        }
        bnode_labels = next_labels;
    }

    let mut bnode_list: Vec<BlankNode> = blank_nodes.into_iter().collect();
    bnode_list.sort_by(|a, b| {
        let label_a = bnode_labels.get(a).unwrap();
        let label_b = bnode_labels.get(b).unwrap();
        if label_a == label_b {
            a.to_string().cmp(&b.to_string())
        } else {
            label_a.cmp(label_b)
        }
    });

    let canonical_map: HashMap<BlankNode, BlankNode> = bnode_list
        .into_iter()
        .enumerate()
        .map(|(idx, b)| (b, BlankNode::new(format!("c14n{}", idx)).unwrap()))
        .collect();

    let mut canon_quads: Vec<Quad> = quads
        .iter()
        .map(|q| {
            let s = match &q.subject {
                NamedOrBlankNode::BlankNode(b) => {
                    NamedOrBlankNode::BlankNode(canonical_map.get(b).unwrap().clone())
                }
                other => other.clone(),
            };
            let p = q.predicate.clone();
            let o = match &q.object {
                Term::BlankNode(b) => Term::BlankNode(canonical_map.get(b).unwrap().clone()),
                other => other.clone(),
            };
            let g = match &q.graph_name {
                GraphName::BlankNode(b) => {
                    GraphName::BlankNode(canonical_map.get(b).unwrap().clone())
                }
                other => other.clone(),
            };
            Quad::new(s, p, o, g)
        })
        .collect();

    canon_quads.sort_by_key(|q| q.to_string());
    canon_quads
}
