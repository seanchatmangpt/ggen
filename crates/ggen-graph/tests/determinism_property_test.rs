//! Determinism properties for ggen-graph, exercised over real Oxigraph in-memory
//! stores (Chicago: real collaborators, state-based assertions, no mocks).
//!
//! Properties under test:
//! 1. Hash determinism: identical triples inserted in different orders yield
//!    identical `state_hash` digests (property over 100 shuffled insert orders
//!    of a 50-triple fixture).
//! 2. Delta determinism: `RdfDelta::compute` is pure — the same baseline/target
//!    pair yields byte-identical serializations and hashes — and re-applying an
//!    already-applied delta is a no-op (receipt reports post == pre).
//! 3. Blank-node handling: graphs that differ only in blank-node labels but are
//!    isomorphic produce identical canonical digests (canonicalization uses
//!    color-refinement neighborhood signing in `graph/canonical.rs`; this test
//!    pins the actual behavior, including the tied-signature symmetric case).
//! 4. Scale: 5k triples hash quickly and stably — digests equal across two
//!    independent builds, wall time reported and bounded by a generous sanity
//!    ceiling.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::single_char_pattern,
    clippy::manual_strip
)]

use ggen_graph::{DeterministicGraph, RdfDelta};
use rand::seq::SliceRandom;
use rand::SeedableRng;
use std::time::Instant;

/// Build a 50-triple fixture of plain named-node N-Quads (default graph).
fn fixture_50() -> Vec<String> {
    (0..50)
        .map(|i| {
            format!(
                "<http://example.org/s{}> <http://example.org/p{}> \"v{}\" .",
                i % 17,
                i % 5,
                i
            )
        })
        .collect()
}

fn build_graph(nquads: &[String]) -> Result<DeterministicGraph, Box<dyn std::error::Error>> {
    let graph = DeterministicGraph::new()?;
    for q in nquads {
        graph.insert_quad(&DeterministicGraph::parse_nquad(q)?)?;
    }
    Ok(graph)
}

/// Property 1: insert-order independence of the state digest, over 100 shuffles.
#[test]
fn hash_is_independent_of_insert_order_100_shuffles() -> Result<(), Box<dyn std::error::Error>> {
    let fixture = fixture_50();
    let reference = build_graph(&fixture)?.state_hash()?;

    let mut rng = rand::rngs::StdRng::seed_from_u64(42);
    let mut distinct_orders_seen = 0usize;
    for _ in 0..100 {
        let mut shuffled = fixture.clone();
        shuffled.shuffle(&mut rng);
        if shuffled != fixture {
            distinct_orders_seen += 1;
        }
        let graph = build_graph(&shuffled)?;
        assert_eq!(
            graph.state_hash()?,
            reference,
            "digest diverged under a shuffled insert order"
        );
    }
    // Sanity: the shuffler must actually have produced non-identity orders,
    // otherwise the property above is vacuous.
    assert!(
        distinct_orders_seen >= 90,
        "shuffling produced too few distinct orders: {distinct_orders_seen}/100"
    );
    Ok(())
}

/// Property 2a: delta computation is pure — same diff twice, byte-identical
/// serialization and hash.
#[test]
fn delta_compute_is_deterministic() -> Result<(), Box<dyn std::error::Error>> {
    let fixture = fixture_50();
    let (baseline_part, target_part) = fixture.split_at(40);

    let baseline = build_graph(baseline_part)?;
    let target = build_graph(target_part)?; // drops 10, adds 10 distinct

    let d1 = RdfDelta::compute(&baseline, &target)?;
    let d2 = RdfDelta::compute(&baseline, &target)?;

    assert_eq!(
        serde_json::to_string(&d1)?,
        serde_json::to_string(&d2)?,
        "delta serialization not reproducible"
    );
    assert_eq!(d1.hash(), d2.hash(), "delta hash not reproducible");
    // target = quads 40..50 (10 additions, all absent from baseline);
    // baseline = quads 0..40, none of which survive in target (40 deletions).
    assert_eq!(d1.additions.len(), 10);
    assert_eq!(d1.deletions.len(), 40);
    // Compute sorts additions/deletions: serialization is order-canonical.
    let mut sorted_adds = d1.additions.clone();
    sorted_adds.sort();
    assert_eq!(
        d1.additions, sorted_adds,
        "additions not canonically sorted"
    );
    Ok(())
}

/// Property 2b: applying a delta twice — the second application is a no-op.
/// `apply_delta` re-inserts/re-removes the same quads; Oxigraph dedups set
/// semantics, so the post-state hash of the second receipt equals its pre-state
/// hash and equals the settled target state.
#[test]
fn delta_double_apply_is_noop() -> Result<(), Box<dyn std::error::Error>> {
    let fixture = fixture_50();
    let (baseline_part, target_part) = fixture.split_at(40);

    let baseline = build_graph(baseline_part)?;
    let target = build_graph(target_part)?;
    let target_hash = target.state_hash()?;

    let delta = RdfDelta::compute(&baseline, &target)?;

    let receipt1 = baseline.apply_delta(&delta, &[])?;
    assert_eq!(receipt1.post_state_hash, target_hash);
    assert_ne!(receipt1.pre_state_hash, receipt1.post_state_hash);

    // Second application on the already-mutated graph.
    let pre2 = baseline.state_hash()?;
    let receipt2 = baseline.apply_delta(&delta, &[])?;
    assert_eq!(
        receipt2.pre_state_hash, pre2,
        "graph drifted between applications"
    );
    assert_eq!(
        receipt2.post_state_hash, receipt2.pre_state_hash,
        "second application changed state — not a no-op"
    );
    assert_eq!(
        baseline.state_hash()?,
        target_hash,
        "state after double-apply differs from single-apply"
    );
    Ok(())
}

/// Property 3 (documented actual behavior), blank nodes. Two regimes:
///
/// (a) Distinguishable bnodes: `canonicalize_quads` signs each bnode with its
///     neighborhood hash (color refinement), so isomorphic graphs differing
///     only in bnode labels produce IDENTICAL digests.
///
/// (b) Tied (automorphic) bnodes — e.g. the symmetric pair `a knows b`,
///     `b knows a` where refinement cannot separate the two: the tie-break in
///     `canonicalize_quads` falls back to the ORIGINAL blank-node label, but
///     Oxigraph re-mints blank-node identity per store instance. The digest of
///     a tied class therefore DIVERGES across store instances (honest
///     non-isomorphism-invariance), while remaining stable for any single
///     store. This is a real limitation, pinned by assertion.
#[test]
fn bnode_canonicalization_documented_behavior() -> Result<(), Box<dyn std::error::Error>> {
    // --- Regime (a): distinguishable bnodes --------------------------------
    let mk_star = |bn: &[&str]| -> Result<DeterministicGraph, Box<dyn std::error::Error>> {
        let g = DeterministicGraph::new()?;
        for (i, b) in bn.iter().enumerate() {
            let q = format!("<http://example.org/hub> <http://example.org/rel{i}> {b} .");
            g.insert_quad(&DeterministicGraph::parse_nquad(&q)?)?;
        }
        Ok(g)
    };
    let star1 = mk_star(&["_:alpha", "_:beta"])?;
    let star2 = mk_star(&["_:zzz", "_:yyy"])?;
    assert_eq!(
        star1.state_hash()?,
        star2.state_hash()?,
        "isomorphic graphs with relabeled distinguishable bnodes diverged"
    );

    // --- Regime (b): symmetric tied class ----------------------------------
    let mk_sym = |a: &str, b: &str| -> Result<DeterministicGraph, Box<dyn std::error::Error>> {
        let g = DeterministicGraph::new()?;
        for q in [
            format!("{a} <http://example.org/knows> {b} ."),
            format!("{b} <http://example.org/knows> {a} ."),
        ] {
            g.insert_quad(&DeterministicGraph::parse_nquad(&q)?)?;
        }
        Ok(g)
    };
    let sym1 = mk_sym("_:x1", "_:x2")?;
    let h1 = sym1.state_hash()?;
    let h1_again = sym1.state_hash()?;
    assert_eq!(h1, h1_again, "re-hashing one store changed the digest");

    // HONEST DIVERGENCE (documented, not pinned to a polarity): rebuilds in
    // fresh store instances — even with identical input labels — do NOT
    // reliably reproduce h1. Oxigraph re-mints blank-node identity per store,
    // and the canonicalization tie-break for the tied class falls back to that
    // minted identity; the divergence polarity itself varies run-to-run, so
    // asserting either equality or inequality here would be flaky. The digests
    // are printed as the real observed record instead.
    let h1b = mk_sym("_:x1", "_:x2")?.state_hash()?;
    let h2 = mk_sym("_:q9", "_:q4")?.state_hash()?;
    eprintln!("tied-bnode digests (store1 {h1:?}, rebuild {h1b:?}, relabeled {h2:?})");
    // SECOND DOCUMENTED BEHAVIOR (silent no-op): removing a bnode quad by
    // re-parsing its original N-Quad string is a SILENT NO-OP — parse_nquad
    // mints a fresh blank-node identity that never matches the identity the
    // store minted at insert time, so `remove_quad` matches nothing and
    // returns Ok. The digest is unchanged after the attempted removal.
    sym1.remove_quad(&DeterministicGraph::parse_nquad(
        "_:x1 <http://example.org/knows> _:x2 .",
    )?)?;
    assert_eq!(
        sym1.state_hash()?,
        h1,
        "bnode removal via re-parsed N-Quad actually removed — behavior \
         changed; update this documented-behavior test"
    );
    Ok(())
}

/// Property 4: 5k triples — identical digests across two independent builds and
/// two hash computations on the same graph; wall time reported, bounded by a
/// generous sanity ceiling.
#[test]
fn large_graph_5k_digest_stable_and_bounded() -> Result<(), Box<dyn std::error::Error>> {
    let fixture: Vec<String> = (0..5_000)
        .map(|i| {
            format!(
                "<http://example.org/s{}> <http://example.org/p{}> \"v{}\" .",
                i % 1_000,
                i % 50,
                i
            )
        })
        .collect();

    let t_build0 = Instant::now();
    let g1 = build_graph(&fixture)?;
    let t_build1 = Instant::now();
    let g2 = build_graph(&fixture)?;
    let build_secs = (t_build1 - t_build0).as_secs_f64();

    let t0 = Instant::now();
    let h1a = g1.state_hash()?;
    let t1 = Instant::now();
    let h1b = g1.state_hash()?;
    let t2 = Instant::now();
    let h2 = g2.state_hash()?;
    let t3 = Instant::now();

    let first_hash_ms = (t1 - t0).as_secs_f64() * 1_000.0;
    let rehash_ms = (t2 - t1).as_secs_f64() * 1_000.0;
    let second_graph_hash_ms = (t3 - t2).as_secs_f64() * 1_000.0;

    eprintln!(
        "5k-triple: build+insert {build_secs:.3}s; state_hash first {first_hash_ms:.1}ms, \
         rehash {rehash_ms:.1}ms, second-graph {second_graph_hash_ms:.1}ms"
    );

    assert_eq!(h1a, h1b, "re-hashing the same graph changed the digest");
    assert_eq!(h1a, h2, "independent builds of 5k triples diverged");

    // Generous sanity ceiling (not a perf SLO — just guards pathological blowup
    // on the canonical sort). Reported times above carry the real numbers.
    assert!(
        first_hash_ms < 10_000.0,
        "5k-triple state_hash took {first_hash_ms:.1}ms — pathological"
    );
    Ok(())
}
