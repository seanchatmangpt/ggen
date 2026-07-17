//! Blue River Dam control benchmark (divan) — GraphLaw materialization.
//!
//! Measures `TripleStore::materialize()` over a small delta applied to a
//! preloaded, already-materialized graph (the incremental law-state step).
//! Runs alongside the existing bencher-based benches (`bench.rs`,
//! `hierarchies.rs`, `dialects.rs`) — it does not replace them.

#![allow(missing_docs)]

use praxis_graphlaw::parser::Parser;
use praxis_graphlaw::TripleStore;

fn main() {
    divan::main();
}

/// Base graph: a 32-fact `links` chain plus transitive `reach` rules.
fn base_n3() -> String {
    let mut doc = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..32 {
        doc.push_str(&format!("ex:n{} ex:links ex:n{} .\n", i, i + 1));
    }
    doc.push_str("{ ?a ex:links ?b } => { ?a ex:reach ?b } .\n");
    doc.push_str("{ ?a ex:reach ?b . ?b ex:links ?c } => { ?a ex:reach ?c } .\n");
    doc
}

/// The small delta: one new edge extending the chain.
const DELTA: &str = "@prefix ex: <http://example.org/> .\nex:n32 ex:links ex:n33 .\n";

/// `materialize()` over a small delta on a preloaded graph: the store is
/// loaded and materialized to fixpoint in setup (untimed); the timed body
/// adds the delta triples and re-materializes.
#[divan::bench]
fn graphlaw_materialize_delta(bencher: divan::Bencher) {
    let base = base_n3();
    bencher
        .with_inputs(|| {
            let mut store = TripleStore::from(&base);
            store.materialize().unwrap();
            let (delta_triples, _rules) =
                Parser::parse_n3_document(DELTA).expect("delta parses as N3");
            (store, delta_triples)
        })
        .bench_local_values(|(mut store, delta_triples)| {
            for t in delta_triples {
                store.add(t);
            }
            store.materialize().unwrap().len()
        });
}
