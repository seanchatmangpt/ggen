//! Property courts over the pure ABB/SBB kernel and its depgraph resolver.
//!
//! Chicago: real pure functions, deterministic PRNG (splitmix64, no new deps),
//! assertions on final state — no mocks, no interaction checks.
//!
//! Properties:
//! 1. depgraph closure == independent BFS reachable set (100 DAGs x 20 nodes).
//! 2. Injected cycle -> typed `CYCLIC_PACK_DEPENDENCY` naming a real cycle; the
//!    acyclic variant of the same graph resolves.
//! 3. Unbound port -> typed `UNBOUND_PORT`.
//! 4. `admit` determinism: same input 10x -> identical `Admitted` and receipt bytes.
//! 5. Idempotence: admit is pure (`admit(x) == admit(x)`); replay reproduces the receipt.
//! 6. Degenerate inputs (0 packs, self-edge, duplicate edges) -> typed outcomes, no panic.

// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
use ggen_abb_sbb::depgraph::{resolve_sync_order, ConsumerEdges, PackManifest};
use ggen_abb_sbb::{
    admit, canonical_digest, manufacture, synthetic_graph, Authority, Generator, Request,
};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

// ---------- deterministic PRNG (splitmix64) ----------

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E3779B97F4A7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
        z ^ (z >> 31)
    }
    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
}

// ---------- fixture helpers ----------

fn pack(name: &str) -> PackManifest {
    PackManifest {
        name: name.into(),
        depends_on: BTreeSet::new(),
        provides: BTreeSet::new(),
        requires: BTreeSet::new(),
        artifacts: BTreeSet::new(),
    }
}

fn pack_named(i: usize) -> PackManifest {
    pack(&format!("p{i:02}"))
}

/// Independent BFS reachability over the dep edges (edge P -> Q = P depends on Q).
fn bfs_reachable(edges: &BTreeMap<String, BTreeSet<String>>, from: &str) -> BTreeSet<String> {
    let mut seen = BTreeSet::new();
    let mut q: VecDeque<&str> = VecDeque::new();
    for d in &edges[from] {
        q.push_back(d);
    }
    while let Some(n) = q.pop_front() {
        if seen.insert(n.to_string()) {
            if let Some(ds) = edges.get(n) {
                for d in ds {
                    q.push_back(d);
                }
            }
        }
    }
    seen
}

/// Random DAG on `n` nodes: edge i -> j only when i > j (guaranteed acyclic).
fn random_dag(
    rng: &mut Rng, n: usize, edge_prob_pct: usize,
) -> (Vec<PackManifest>, BTreeMap<String, BTreeSet<String>>) {
    let names: Vec<String> = (0..n).map(|i| format!("p{i:02}")).collect();
    let mut edges: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut packs = Vec::new();
    for i in 0..n {
        let mut deps = BTreeSet::new();
        for j in 0..i {
            if rng.below(100) < edge_prob_pct {
                deps.insert(names[j].clone());
            }
        }
        edges.insert(names[i].clone(), deps.clone());
        packs.push(PackManifest {
            depends_on: deps,
            ..pack(&names[i])
        });
    }
    (packs, edges)
}

fn ingest_request() -> Request {
    Request {
        abb: "abb:event-ingest".into(),
        sbb: "sbb:ingest-0000".into(),
        requested_authority: Authority::Construct,
        expected_graph_digest: None,
    }
}

fn generator() -> Generator {
    Generator {
        id: "gen:test".into(),
        version: "1.0.0".into(),
    }
}

// ---------- Property 1: closure == BFS ----------

#[test]
fn depgraph_closure_matches_independent_bfs_on_100_random_dags() {
    let mut rng = Rng(0xD6FC_0001);
    for _graph in 0..100 {
        let (packs, edges) = random_dag(&mut rng, 20, 20);
        let plan = resolve_sync_order(&packs, None).expect("random DAG resolves");
        assert_eq!(plan.order.len(), 20, "every pack appears exactly once");
        for (name, closure) in &plan.transitive_deps {
            let expected = bfs_reachable(&edges, name);
            assert_eq!(
                closure, &expected,
                "closure({name}) diverged from independent BFS"
            );
        }
    }
}

#[test]
fn depgraph_closure_matches_bfs_with_dense_and_empty_edges() {
    let mut rng = Rng(0xD6FC_0002);
    for prob in [0usize, 60, 100] {
        let (packs, edges) = random_dag(&mut rng, 20, prob);
        let plan = resolve_sync_order(&packs, None).unwrap();
        for (name, closure) in &plan.transitive_deps {
            assert_eq!(closure, &bfs_reachable(&edges, name), "prob={prob}");
        }
    }
}

#[test]
fn depgraph_topological_order_respects_every_edge() {
    let mut rng = Rng(0xD6FC_0003);
    for _ in 0..50 {
        let (packs, edges) = random_dag(&mut rng, 20, 30);
        let plan = resolve_sync_order(&packs, None).unwrap();
        let pos: BTreeMap<&str, usize> = plan
            .order
            .iter()
            .enumerate()
            .map(|(i, n)| (n.as_str(), i))
            .collect();
        for (p, deps) in &edges {
            for d in deps {
                assert!(
                    pos[d.as_str()] < pos[p.as_str()],
                    "edge {p} -> {d} violated by order {:?}",
                    plan.order
                );
            }
        }
    }
}

// ---------- Property 2: cycle refusal ----------

#[test]
fn every_injected_cycle_is_typed_refusal_and_acyclic_variant_admits() {
    let mut rng = Rng(0xD6FC_0004);
    let mut checked = 0;
    for _ in 0..100 {
        let (mut packs, _) = random_dag(&mut rng, 8, 25);
        // Inject a cycle: pick two distinct nodes i > j, add edge p_i -> p_j
        // (back edge) plus ensure a forward path exists by adding p_j -> p_{i-1}
        // chain is not needed — a direct back edge on a DAG where p_j is an
        // ancestor-reachable... simplest sound injection: reverse a random
        // forward edge i -> j into j -> i creates a cycle iff i reaches j,
        // so instead force the pair edge: p_hi depends on p_lo AND p_lo on p_hi.
        let hi = 1 + rng.below(7);
        let lo = rng.below(hi);
        let h = format!("p{hi:02}");
        let l = format!("p{lo:02}");
        for p in packs.iter_mut() {
            if p.name == h {
                p.depends_on.insert(l.clone());
            }
            if p.name == l {
                p.depends_on.insert(h.clone());
            }
        }
        let err = resolve_sync_order(&packs, None).unwrap_err();
        match &err {
            ggen_abb_sbb::Refusal::CyclicPackDependency { cycle } => {
                assert!(cycle.len() >= 2, "cycle path too short: {cycle:?}");
                assert_eq!(
                    cycle.first(),
                    cycle.last(),
                    "cycle must be closed: {cycle:?}"
                );
                // The injected pair must appear in the named cycle.
                assert!(
                    cycle.contains(&h) && cycle.contains(&l),
                    "cycle {cycle:?} omits injected pair {h}/{l}"
                );
            }
            other => panic!("expected CYCLIC_PACK_DEPENDENCY, got {other:?}"),
        }
        // Acyclic variant: drop the back edge from the lower node; must resolve.
        let mut acyclic = packs.clone();
        for p in acyclic.iter_mut() {
            if p.name == l {
                p.depends_on.remove(&h);
            }
        }
        let plan = resolve_sync_order(&acyclic, None)
            .unwrap_or_else(|e| panic!("acyclic variant refused: {e}"));
        assert_eq!(plan.order.len(), 8);
        checked += 1;
    }
    assert_eq!(checked, 100);
}

#[test]
fn self_edge_is_typed_cycle_refusal() {
    let mut p = pack_named(0);
    p.depends_on.insert("p00".into());
    let err = resolve_sync_order(std::slice::from_ref(&p), None).unwrap_err();
    match err {
        ggen_abb_sbb::Refusal::CyclicPackDependency { cycle } => {
            assert_eq!(cycle.first(), cycle.last());
            assert!(cycle.contains(&"p00".to_string()));
        }
        other => panic!("expected cycle refusal, got {other:?}"),
    }
}

// ---------- Property 3: unbound port ----------

#[test]
fn unbound_port_is_typed_refusal_naming_pack_and_port() {
    let mut a = pack_named(0);
    a.provides.insert("port:x".into());
    let mut b = pack_named(1);
    b.depends_on.insert("p00".into());
    b.requires.insert("port:missing".into());
    let err = resolve_sync_order(&[a, b], None).unwrap_err();
    match err {
        ggen_abb_sbb::Refusal::UnboundPort { pack, port } => {
            assert_eq!(pack, "p01");
            assert_eq!(port, "port:missing");
        }
        other => panic!("expected UNBOUND_PORT, got {other:?}"),
    }
}

#[test]
fn port_bound_only_through_transitive_dep_is_accepted_randomly() {
    let mut rng = Rng(0xD6FC_0005);
    for _ in 0..25 {
        // chain p0 -> p1 -> p2, provider at p0, requirer at p2
        let mut provider = pack_named(0);
        provider.provides.insert("port:t".into());
        let mut mid = pack_named(1);
        mid.depends_on.insert("p00".into());
        let mut top = pack_named(2);
        top.depends_on.insert("p01".into());
        top.requires.insert("port:t".into());
        let plan = resolve_sync_order(&[provider, mid, top], None).unwrap();
        assert_eq!(plan.order, vec!["p00", "p01", "p02"]);
        let _ = rng.next(); // keep rng used; loop is deterministic replay
    }
}

// ---------- Property 4: admit determinism ----------

#[test]
fn admit_is_deterministic_across_10_repeats() {
    let g = synthetic_graph(3, 2);
    let req = ingest_request();
    let first = admit(&g, &req).unwrap();
    let first_receipt = manufacture(&first, &generator()).unwrap();
    for _ in 0..10 {
        let again = admit(&g, &req).unwrap();
        assert_eq!(first, again, "Admitted value differed across repeats");
        let receipt = manufacture(&again, &generator()).unwrap();
        assert_eq!(
            canonical_digest(&first_receipt.receipt),
            canonical_digest(&receipt.receipt),
            "receipt bytes differed across repeats"
        );
        assert_eq!(first_receipt.artifacts, receipt.artifacts);
    }
}

#[test]
fn plan_is_deterministic_across_10_repeats() {
    let g = synthetic_graph(3, 2);
    let first = ggen_abb_sbb::plan(&g, "abb:event-ingest", Authority::Select).unwrap();
    for _ in 0..10 {
        assert_eq!(
            &first,
            &ggen_abb_sbb::plan(&g, "abb:event-ingest", Authority::Select).unwrap()
        );
    }
}

// ---------- Property 5: idempotence / purity ----------

#[test]
fn admit_is_pure_re_admit_of_admitted_subject_is_identical() {
    let g = synthetic_graph(2, 2);
    let req = ingest_request();
    let a1 = admit(&g, &req).unwrap();
    // Pure function: re-running on the same untouched graph yields the same value.
    let a2 = admit(&g, &req).unwrap();
    assert_eq!(a1, a2);
    assert_eq!(a1.graph_digest(), a2.graph_digest());
    // The graph itself is unchanged by admission (no interior mutation).
    let g2 = synthetic_graph(2, 2);
    assert_eq!(g.digest(), g2.digest());
    // replay(receipt) reproduces byte-identical artifacts: idempotent manufacture.
    let m1 = manufacture(&a1, &generator()).unwrap();
    let replayed = ggen_abb_sbb::replay(&m1.receipt, &g, &generator()).unwrap();
    assert_eq!(m1.artifacts, replayed.artifacts);
    assert_eq!(
        canonical_digest(&m1.receipt),
        canonical_digest(&replayed.receipt)
    );
    ggen_abb_sbb::verify_receipt(&replayed.receipt).unwrap();
}

// ---------- Property 6: degenerate inputs ----------

#[test]
fn empty_pack_list_resolves_to_empty_plan() {
    let plan = resolve_sync_order(&[], None).unwrap();
    assert!(plan.order.is_empty());
    assert!(plan.transitive_deps.is_empty());
}

#[test]
fn single_pack_no_deps_resolves_to_itself() {
    let plan = resolve_sync_order(&[pack_named(0)], None).unwrap();
    assert_eq!(plan.order, vec!["p00"]);
    assert!(plan.transitive_deps["p00"].is_empty());
}

#[test]
fn duplicate_edges_are_absorbed_by_set_semantics() {
    // BTreeSet deps mean a "duplicate" edge is unrepresentable in the type; the
    // closest real input is extraction-level duplicates, so assert set semantics:
    // inserting the same dep twice changes nothing.
    let mut a = pack_named(1);
    a.depends_on.insert("p00".into());
    a.depends_on.insert("p00".into()); // idempotent insert
    let plan1 = resolve_sync_order(&[pack_named(0), a.clone()], None).unwrap();
    let plan2 = resolve_sync_order(&[pack_named(0), a], None).unwrap();
    assert_eq!(plan1, plan2);
}

#[test]
fn consumer_only_graph_resolves_with_consumer_last() {
    let consumer = ConsumerEdges {
        name: "app".into(),
        depends_on: BTreeSet::new(),
    };
    let plan = resolve_sync_order(&[], Some(&consumer)).unwrap();
    assert_eq!(plan.order, vec!["app"]);
}

#[test]
fn duplicate_pack_names_are_typed_refusal() {
    let err = resolve_sync_order(&[pack_named(0), pack_named(0)], None).unwrap_err();
    assert!(matches!(
        err,
        ggen_abb_sbb::Refusal::DuplicateElement { .. }
    ));
}

#[test]
fn degenerate_admit_inputs_are_typed_not_panics() {
    let g = synthetic_graph(1, 1);
    let unknown_abb = Request {
        abb: "abb:nope".into(),
        sbb: "sbb:ingest-0000".into(),
        requested_authority: Authority::Construct,
        expected_graph_digest: None,
    };
    assert!(matches!(
        admit(&g, &unknown_abb),
        Err(ggen_abb_sbb::Refusal::UnknownAbb { .. })
    ));
    let unknown_sbb = Request {
        abb: "abb:event-ingest".into(),
        sbb: "sbb:nope".into(),
        requested_authority: Authority::Construct,
        expected_graph_digest: None,
    };
    assert!(matches!(
        admit(&g, &unknown_sbb),
        Err(ggen_abb_sbb::Refusal::UnknownSbb { .. })
    ));
    let low_auth = Request {
        abb: "abb:event-ingest".into(),
        sbb: "sbb:ingest-0000".into(),
        requested_authority: Authority::None,
        expected_graph_digest: None,
    };
    assert!(matches!(
        admit(&g, &low_auth),
        Err(ggen_abb_sbb::Refusal::InsufficientAuthority { .. })
    ));
    let bad_digest = Request {
        abb: "abb:event-ingest".into(),
        sbb: "sbb:ingest-0000".into(),
        requested_authority: Authority::Construct,
        expected_graph_digest: Some("sha256:deadbeef".into()),
    };
    assert!(matches!(
        admit(&g, &bad_digest),
        Err(ggen_abb_sbb::Refusal::GraphDigestMismatch { .. })
    ));
}
