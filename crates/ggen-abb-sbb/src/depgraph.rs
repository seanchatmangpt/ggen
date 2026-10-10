//! Cross-pack Datalog resolver: dependency facts extracted from
//! `ggen.toml`/`pack.toml`, forward-chained through the transitive-dependency
//! rules, and refused on cycles, unbound ports, and artifact collisions —
//! with a topological sync order as the result.
//!
//! Datalog rules (forward chaining over pack facts):
//!
//! ```text
//! dep(P, Q)        <- pack.toml [graph] depends_on / ggen.toml consumer edges.
//! transitive_dep(P, R) :- dep(P, Q), transitive_dep(Q, R).
//! transitive_dep(P, Q) :- dep(P, Q).
//! cyclic(P)        :- transitive_dep(P, P).
//! unbound_port(P, T) :- requires(P, T), not provided_by_closure(P, T).
//! ```
//!
//! The resolver is IO-free: extraction is from in-memory TOML text, the gate
//! takes values and returns values (`SyncPlan` / [`Refusal`]). Chicago tests
//! below exercise real TOML documents end to end.

use crate::Refusal;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

/// One pack's dependency-graph facts, extracted from its `pack.toml`
/// (`[pack] name` plus an optional `[graph]` table).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PackManifest {
    pub name: String,
    #[serde(default)]
    pub depends_on: BTreeSet<String>,
    /// Ports this pack provides to consumers.
    #[serde(default)]
    pub provides: BTreeSet<String>,
    /// Ports this pack requires from its (transitive) dependencies.
    #[serde(default)]
    pub requires: BTreeSet<String>,
    /// Artifact paths this pack writes.
    #[serde(default)]
    pub artifacts: BTreeSet<String>,
}

/// The consumer's own `ggen.toml` edges: a name plus the packs it wires.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ConsumerEdges {
    pub name: String,
    #[serde(default)]
    pub depends_on: BTreeSet<String>,
}

/// Result of a clean resolution: a topological sync order plus the computed
/// transitive-dependency closure (the `transitive_dep` relation).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SyncPlan {
    /// Consumer first (if any), then packs in dependency order.
    pub order: Vec<String>,
    /// `transitive_dep(P) = { Q, ... }` for every pack `P`.
    pub transitive_deps: BTreeMap<String, BTreeSet<String>>,
}

/// Extract [`PackManifest`] facts from a `pack.toml` document.
///
/// `name` is read from `[pack] name` (the existing pack format); dependency
/// facts are read from an optional `[graph]` table (`depends_on`, `provides`,
/// `requires`, `artifacts`). Packs without a `[graph]` table are
/// dependency-free facts (`dep(P, nothing)`) — every existing pack parses
/// clean under this law.
pub fn extract_pack_manifest(pack_toml: &str) -> Result<PackManifest, Refusal> {
    let value: toml::Value =
        star_toml::from_str(pack_toml).map_err(|e| Refusal::MalformedGraph {
        reason: format!("pack.toml is not valid TOML: {e}"),
    })?;
    let name = value
        .get("pack")
        .and_then(|p| p.get("name"))
        .and_then(|n| n.as_str())
        .ok_or_else(|| Refusal::MalformedGraph {
            reason: "pack.toml has no [pack] name".to_string(),
        })?
        .to_string();
    let graph = value.get("graph");
    let set_of = |key: &str| -> BTreeSet<String> {
        graph
            .and_then(|g| g.get(key))
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|x| x.as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default()
    };
    Ok(PackManifest {
        name,
        depends_on: set_of("depends_on"),
        provides: set_of("provides"),
        requires: set_of("requires"),
        artifacts: set_of("artifacts"),
    })
}

/// Extract [`ConsumerEdges`] from a consumer `ggen.toml` document (optional
/// `[graph]` table with `name` and `depends_on`).
pub fn extract_consumer_edges(ggen_toml: &str) -> Result<Option<ConsumerEdges>, Refusal> {
    let value: toml::Value =
        star_toml::from_str(ggen_toml).map_err(|e| Refusal::MalformedGraph {
        reason: format!("ggen.toml is not valid TOML: {e}"),
    })?;
    let Some(graph) = value.get("graph") else {
        return Ok(None);
    };
    let name = graph
        .get("name")
        .and_then(|n| n.as_str())
        .ok_or_else(|| Refusal::MalformedGraph {
            reason: "ggen.toml [graph] table has no name".to_string(),
        })?
        .to_string();
    let depends_on = graph
        .get("depends_on")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|x| x.as_str().map(str::to_string))
                .collect()
        })
        .unwrap_or_default();
    Ok(Some(ConsumerEdges { name, depends_on }))
}

/// Resolve the full cross-pack graph: gates, transitive closure, sync order.
pub fn resolve_sync_order(
    packs: &[PackManifest], consumer: Option<&ConsumerEdges>,
) -> Result<SyncPlan, Refusal> {
    let mut by_name: BTreeMap<&str, &PackManifest> = BTreeMap::new();
    for p in packs {
        if by_name.insert(p.name.as_str(), p).is_some() {
            return Err(Refusal::DuplicateElement { id: p.name.clone() });
        }
    }

    // Gate 1: dangling dependency references.
    for p in packs {
        for d in &p.depends_on {
            if !by_name.contains_key(d.as_str()) {
                return Err(Refusal::DanglingReference {
                    from: p.name.clone(),
                    to: d.clone(),
                });
            }
        }
    }
    if let Some(c) = consumer {
        for d in &c.depends_on {
            if !by_name.contains_key(d.as_str()) {
                return Err(Refusal::DanglingReference {
                    from: c.name.clone(),
                    to: d.clone(),
                });
            }
        }
    }

    // Gate 2: artifact collisions (two packs writing one path).
    let mut artifact_owner: BTreeMap<&str, &str> = BTreeMap::new();
    for p in packs {
        for a in &p.artifacts {
            match artifact_owner.insert(a.as_str(), p.name.as_str()) {
                Some(prev) if prev != p.name.as_str() => {
                    return Err(Refusal::DuplicateArtifactPath { path: a.clone() });
                }
                _ => {}
            }
        }
    }

    // Edges including the consumer node.
    let mut edges: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for p in packs {
        edges.insert(p.name.clone(), p.depends_on.clone());
    }
    if let Some(c) = consumer {
        edges.insert(c.name.clone(), c.depends_on.clone());
    }

    // transitive_dep forward-chaining fixpoint.
    let mut transitive: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for name in edges.keys() {
        transitive.insert(name.clone(), BTreeSet::new());
    }
    loop {
        let mut changed = false;
        for (p, deps) in &edges {
            let mut next = transitive[p].clone();
            for q in deps {
                next.insert(q.clone());
                next.extend(transitive[q].iter().cloned());
            }
            if next != transitive[p] {
                transitive.insert(p.clone(), next);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    // Gate 3: cycles — transitive_dep(P, P).
    for (p, ts) in &transitive {
        if ts.contains(p) {
            return Err(Refusal::CyclicPackDependency {
                cycle: find_cycle(&edges, p),
            });
        }
    }

    // Gate 4: port completeness — requires(P, T) must be provided by P's
    // transitive deps (or P itself).
    let provided: BTreeMap<&str, &BTreeSet<String>> = packs
        .iter()
        .map(|p| (p.name.as_str(), &p.provides))
        .collect();
    for p in packs {
        let closure = &transitive[&p.name];
        for port in &p.requires {
            let bound = p.provides.contains(port)
                || closure.iter().any(|q| {
                    provided
                        .get(q.as_str())
                        .is_some_and(|ports| ports.contains(port))
                });
            if !bound {
                return Err(Refusal::UnboundPort {
                    pack: p.name.clone(),
                    port: port.clone(),
                });
            }
        }
    }

    // Kahn topological order (consumer, if any, sorts with the packs).
    let mut order = Vec::new();
    let mut remaining: BTreeMap<String, BTreeSet<String>> = edges.clone();
    while !remaining.is_empty() {
        let ready: Vec<String> = remaining
            .iter()
            .filter(|(_, deps)| deps.is_empty())
            .map(|(n, _)| n.clone())
            .collect();
        if ready.is_empty() {
            // Unreachable given the cycle gate above; kept as a loud
            // invariant violation rather than a silent partial order.
            return Err(Refusal::CyclicPackDependency {
                cycle: remaining.keys().cloned().collect(),
            });
        }
        for n in ready {
            remaining.remove(&n);
            for deps in remaining.values_mut() {
                deps.remove(&n);
            }
            order.push(n);
        }
    }
    // Consumer last: it consumes every pack, so it sorts after its deps.
    let order = match consumer {
        Some(c) => {
            let mut o: Vec<String> = order.into_iter().filter(|n| n != &c.name).collect();
            o.push(c.name.clone());
            o
        }
        None => order,
    };

    Ok(SyncPlan {
        order,
        transitive_deps: transitive
            .into_iter()
            .filter(|(n, _)| Some(n.as_str()) != consumer.map(|c| c.name.as_str()))
            .collect(),
    })
}

/// Reconstruct an actual cycle path ending at `start` (DFS with visited set).
fn find_cycle(edges: &BTreeMap<String, BTreeSet<String>>, start: &str) -> Vec<String> {
    fn dfs(
        edges: &BTreeMap<String, BTreeSet<String>>, node: &str, start: &str,
        path: &mut Vec<String>, visited: &mut BTreeSet<String>,
    ) -> Option<Vec<String>> {
        for next in &edges[node] {
            if next.as_str() == start {
                path.push(next.clone());
                return Some(path.clone());
            }
            if !visited.insert(next.clone()) {
                continue;
            }
            path.push(next.clone());
            if let Some(c) = dfs(edges, next, start, path, visited) {
                return Some(c);
            }
            path.pop();
        }
        None
    }
    let mut path = vec![start.to_string()];
    let mut visited: BTreeSet<String> = BTreeSet::new();
    visited.insert(start.to_string());
    dfs(edges, start, start, &mut path, &mut visited).unwrap_or_else(|| vec![start.to_string()])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn manifest(name: &str) -> PackManifest {
        PackManifest {
            name: name.into(),
            depends_on: BTreeSet::new(),
            provides: BTreeSet::new(),
            requires: BTreeSet::new(),
            artifacts: BTreeSet::new(),
        }
    }

    #[test]
    fn extracts_real_pack_toml_shape() {
        let src = r#"
[pack]
name = "affidavit-pack"
version = "0.1.0"

[graph]
depends_on = ["base-pack"]
provides = ["receipt"]
requires = ["hashing"]
artifacts = ["src/affidavit_catalog.rs"]
"#;
        let m = extract_pack_manifest(src).unwrap();
        assert_eq!(m.name, "affidavit-pack");
        assert!(m.depends_on.contains("base-pack"));
        assert!(m.provides.contains("receipt"));
        assert!(m.requires.contains("hashing"));
        assert!(m.artifacts.contains("src/affidavit_catalog.rs"));
    }

    #[test]
    fn extraction_refuses_missing_name_and_bad_toml() {
        let err = extract_pack_manifest("[pack]\nversion = \"1\"").unwrap_err();
        assert!(err.to_string().contains("MALFORMED_GRAPH"));
        assert!(extract_pack_manifest("not toml @@@").is_err());
    }

    #[test]
    fn extracts_existing_packs_without_graph_table_as_dependency_free() {
        let src = "[pack]\nname = \"legacy-pack\"\n";
        let m = extract_pack_manifest(src).unwrap();
        assert_eq!(m.name, "legacy-pack");
        assert!(m.depends_on.is_empty());
    }

    #[test]
    fn resolves_topological_order_through_transitive_chain() {
        let consumer = ConsumerEdges {
            name: "consumer:app".into(),
            depends_on: ["a".to_string()].into_iter().collect(),
        };
        let packs = vec![
            manifest("a"),
            PackManifest {
                depends_on: ["a".to_string()].into_iter().collect(),
                ..manifest("b")
            },
            PackManifest {
                depends_on: ["b".to_string()].into_iter().collect(),
                ..manifest("c")
            },
        ];
        let plan = resolve_sync_order(&packs, Some(&consumer)).unwrap();
        assert_eq!(plan.order, vec!["a", "b", "c", "consumer:app"]);
        assert!(plan.transitive_deps["c"].contains("a"));
        assert_eq!(plan.order.first(), Some(&"a".to_string()));
    }

    #[test]
    fn refuses_cycle_with_real_path() {
        let packs = vec![
            PackManifest {
                depends_on: ["b".to_string()].into_iter().collect(),
                ..manifest("a")
            },
            PackManifest {
                depends_on: ["c".to_string()].into_iter().collect(),
                ..manifest("b")
            },
            PackManifest {
                depends_on: ["a".to_string()].into_iter().collect(),
                ..manifest("c")
            },
        ];
        let err = resolve_sync_order(&packs, None).unwrap_err();
        assert!(
            err.to_string().starts_with("CYCLIC_PACK_DEPENDENCY"),
            "got: {err}"
        );
        match err {
            Refusal::CyclicPackDependency { cycle } => {
                assert_eq!(cycle.first(), cycle.last());
                assert!(cycle.len() >= 2);
            }
            other => panic!("wrong refusal: {other:?}"),
        }
    }

    #[test]
    fn refuses_dangling_dependency() {
        let packs = vec![PackManifest {
            depends_on: ["ghost".to_string()].into_iter().collect(),
            ..manifest("a")
        }];
        let err = resolve_sync_order(&packs, None).unwrap_err();
        assert!(err.to_string().contains("DANGLING_REFERENCE"), "got: {err}");
    }

    #[test]
    fn refuses_duplicate_artifact_path_across_packs() {
        let packs = vec![
            PackManifest {
                artifacts: ["src/shared.rs".to_string()].into_iter().collect(),
                ..manifest("a")
            },
            PackManifest {
                artifacts: ["src/shared.rs".to_string()].into_iter().collect(),
                ..manifest("b")
            },
        ];
        let err = resolve_sync_order(&packs, None).unwrap_err();
        assert!(
            err.to_string().starts_with("DUPLICATE_ARTIFACT_PATH"),
            "got: {err}"
        );
    }

    #[test]
    fn refuses_unbound_port() {
        let packs = vec![
            PackManifest {
                provides: ["receipt".to_string()].into_iter().collect(),
                ..manifest("a")
            },
            PackManifest {
                depends_on: ["a".to_string()].into_iter().collect(),
                requires: ["hashing".to_string()].into_iter().collect(),
                ..manifest("b")
            },
        ];
        let err = resolve_sync_order(&packs, None).unwrap_err();
        assert!(err.to_string().starts_with("UNBOUND_PORT"), "got: {err}");
    }

    #[test]
    fn bound_port_through_transitive_dep_passes() {
        let packs = vec![
            PackManifest {
                provides: ["hashing".to_string()].into_iter().collect(),
                ..manifest("base")
            },
            PackManifest {
                depends_on: ["base".to_string()].into_iter().collect(),
                ..manifest("mid")
            },
            PackManifest {
                depends_on: ["mid".to_string()].into_iter().collect(),
                requires: ["hashing".to_string()].into_iter().collect(),
                ..manifest("top")
            },
        ];
        let plan = resolve_sync_order(&packs, None).unwrap();
        assert_eq!(plan.order, vec!["base", "mid", "top"]);
    }

    #[test]
    fn consumer_dangling_reference_is_refused() {
        let packs = vec![manifest("a")];
        let consumer = ConsumerEdges {
            name: "app".into(),
            depends_on: ["ghost".to_string()].into_iter().collect(),
        };
        let err = resolve_sync_order(&packs, Some(&consumer)).unwrap_err();
        assert!(err.to_string().contains("DANGLING_REFERENCE"));
    }

    #[test]
    fn duplicate_pack_name_is_refused() {
        let packs = vec![manifest("a"), manifest("a")];
        let err = resolve_sync_order(&packs, None).unwrap_err();
        assert!(err.to_string().contains("DUPLICATE_ELEMENT"));
    }
}
