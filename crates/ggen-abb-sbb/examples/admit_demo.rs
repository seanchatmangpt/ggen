//! Runnable tour of the IO-free ABB/SBB admission kernel.
//!
//! Run: `cargo run -p ggen-abb-sbb --example admit_demo`
//!
//! Shows the whole contract in one pass:
//! 1. `admit`  — admission gate over an EA graph, returning a sealed `Admitted`.
//! 2. `manufacture` — compile the admitted SBB into artifacts + a receipt.
//! 3. `plan`   — SELECT existing SBB vs MANUFACTURE missing realization.
//! 4. `depgraph::resolve_sync_order` — topological pack sync order.
//! 5. Refusal paths — cycle, unbound port, and a tampered SBB digest, as typed errors.

// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
use ggen_abb_sbb::depgraph::{resolve_sync_order, ConsumerEdges, PackManifest};
use ggen_abb_sbb::{admit, manufacture, plan, synthetic_graph, Authority, Generator, Request};
use std::collections::BTreeSet;

fn main() {
    // ---------- 1. admit: the gate ----------
    // `synthetic_graph` is the kernel's deterministic fixture: one strategy/
    // capability/ABB, every candidate qualified against the live contract.
    let g = synthetic_graph(3, 2);
    let req = Request {
        abb: "abb:event-ingest".into(),
        sbb: "sbb:ingest-0000".into(),
        requested_authority: Authority::Construct,
        expected_graph_digest: None,
    };
    let ad = admit(&g, &req).expect("fixture admits");
    println!("== admit ==");
    println!("  graph_digest  = {}", ad.graph_digest());
    println!(
        "  qualification = {} (sbb {} @ {})",
        ad.qualification(),
        ad.sbb().id,
        ad.sbb_digest()
    );
    println!(
        "  contract      = {}@{} ceiling {:?}",
        ad.contract().id,
        ad.contract().version,
        ad.contract().authority_ceiling
    );

    // ---------- 2. manufacture: admitted input -> deterministic artifacts ----------
    let gen = Generator {
        id: "gen:demo".into(),
        version: "1.0.0".into(),
    };
    let m = manufacture(&ad, &gen).expect("admitted input manufactures");
    println!("\n== manufacture ==");
    println!("  receipt_digest = {}", m.receipt.receipt_digest);
    for a in &m.artifacts {
        println!("  artifact {} ({} bytes)", a.path, a.bytes.len());
    }

    // ---------- 3. plan: SELECT || MANUFACTURE, a value never an action ----------
    println!("\n== plan ==");
    let d = plan(&g, "abb:event-ingest", Authority::Select).expect("plan at SELECT");
    println!("  {d:#?}");

    // A plan against an ABB whose candidates all fail (unknown SBB) falls to MANUFACTURE.
    let empty = synthetic_graph(0, 1);
    let d2 = plan(&empty, "abb:event-ingest", Authority::Select).expect("plan");
    println!("  no-candidates plan: {:?}", d2);

    // ---------- 4. depgraph: topological sync order ----------
    let manifest = |name: &str, deps: &[&str], provides: &[&str], requires: &[&str]| PackManifest {
        name: name.into(),
        depends_on: deps.iter().map(|s| s.to_string()).collect(),
        provides: provides.iter().map(|s| s.to_string()).collect(),
        requires: requires.iter().map(|s| s.to_string()).collect(),
        artifacts: BTreeSet::new(),
    };
    let packs = vec![
        manifest("base", &[], &["hashing"], &[]),
        manifest("mid", &["base"], &[], &[]),
        manifest("top", &["mid"], &["receipt"], &["hashing"]),
    ];
    let consumer = ConsumerEdges {
        name: "app".into(),
        depends_on: std::iter::once("top".to_string()).collect(),
    };
    let sp = resolve_sync_order(&packs, Some(&consumer)).expect("clean graph resolves");
    println!("\n== depgraph resolve_sync_order ==");
    println!("  order: {}", sp.order.join(" -> "));
    println!("  transitive_deps[top] = {:?}", sp.transitive_deps["top"]);

    // ---------- 5. Refusal paths (typed, not panics) ----------
    println!("\n== refusals ==");
    let cyclic = vec![
        manifest("a", &["b"], &[], &[]),
        manifest("b", &["c"], &[], &[]),
        manifest("c", &["a"], &[], &[]),
    ];
    match resolve_sync_order(&cyclic, None) {
        Err(e) => println!("  cycle:       {e}"),
        Ok(_) => unreachable!(),
    }
    let unbound = vec![
        manifest("base", &[], &["receipt"], &[]),
        manifest("consumer", &["base"], &[], &["hashing"]),
    ];
    match resolve_sync_order(&unbound, None) {
        Err(e) => println!("  unbound port: {e}"),
        Ok(_) => unreachable!(),
    }

    // A stale SBB digest: tamper the realization, the gate refuses with a typed mismatch.
    let mut tampered = synthetic_graph(1, 1);
    tampered.candidate_sbbs[0]
        .provides_ports
        .insert("port:extra".into());
    match admit(&tampered, &req) {
        Err(e) => println!("  tamper:      {e}"),
        Ok(_) => unreachable!(),
    }

    println!("\ndemo complete: admit -> manufacture -> plan -> depgraph -> refusals");
}
