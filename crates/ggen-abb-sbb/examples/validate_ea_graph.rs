//! Validate a real `ea.graph.json` from disk against the ggen-abb-sbb kernel.
//!
//! Usage: `cargo run -p ggen-abb-sbb --example validate_ea_graph -- <path>`
//!
//! Read-only usage of the crate: `parse_graph` → `validate` → per-SBB
//! `admit_with_caps` probe. Prints the kernel-computed `sbb_content_digest`
//! for every candidate SBB and `contract_digest` for every contract, so a
//! graph author can pin qualifications to real kernel digests. Never writes.
//!
//! Chicago discipline: real file input, assertions on kernel output, no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
use ggen_abb_sbb::{sbb_content_digest, contract_digest, Authority, Request};
use std::collections::BTreeSet;

fn main() {
    let path = std::env::args().nth(1).expect("usage: validate_ea_graph <ea.graph.json path>");
    let json = std::fs::read_to_string(&path).expect("graph file readable");
    let g = match ggen_abb_sbb::parse_graph(&json) {
        Ok(g) => g,
        Err(r) => {
            eprintln!("PARSE REFUSED: {r}");
            std::process::exit(2);
        }
    };
    println!("schema ok: {}", g.schema);
    if let Err(r) = g.validate() {
        eprintln!("VALIDATE REFUSED: {r}");
        std::process::exit(2);
    }
    println!("validate: ok");
    println!("graph_digest: {}", g.digest());

    println!("\n-- contract digests --");
    for k in &g.contracts {
        println!("  {} = {}", k.id, contract_digest(k));
    }

    println!("\n-- sbb content digests --");
    for s in &g.candidate_sbbs {
        println!("  {} = {}", s.id, sbb_content_digest(s));
    }

    // Per-SBB admission probe at CONSTRUCT, mirroring the engine's Stage 2c loop
    // (crates/ggen-engine/src/sync.rs Stage 2c, expected_graph_digest pinning).
    println!("\n-- admission probe (CONSTRUCT) --");
    for s in &g.candidate_sbbs {
        let req = Request {
            abb: s.abb.clone(),
            sbb: s.id.clone(),
            requested_authority: Authority::Construct,
            expected_graph_digest: Some(g.digest()),
            receipt_generation: 0,
            artifact_baseline_generation: 0,
        };
        match ggen_abb_sbb::admit_with_caps(&g, &req, None) {
            Ok(ad) => println!(
                "  ADMITTED {} (qualification {})",
                s.id,
                ad.qualification()
            ),
            Err(r) => println!("  REFUSED  {} :: {}", s.id, r),
        }
    }

    // plan() at SELECT for every ABB: the SELECT||MANUFACTURE decision table.
    println!("\n-- plan decision table (SELECT) --");
    for a in &g.abbs {
        match ggen_abb_sbb::plan(&g, &a.id, Authority::Select) {
            Ok(d) => println!("  {d:?}"),
            Err(r) => println!("  plan refused {r}"),
        }
    }
    let _ = BTreeSet::<String>::new(); // keep import honest if unused above
}
