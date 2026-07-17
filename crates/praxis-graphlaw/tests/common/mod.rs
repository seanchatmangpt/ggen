//! Shared test utilities for praxis-graphlaw integration tests.
#![allow(dead_code)]

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::tripleindex::TripleIndex;
use praxis_graphlaw::triples::Triple;
use praxis_graphlaw::TripleStore;

// Expected public HookReceipt structure for hook receipt testing
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HookReceipt {
    pub hook_name: String,
    pub delta_hash: String,
    pub idempotency_key: String,
    pub delta_quads: String,
}

/// Decode an encoded term ID or fail the test with an attributable panic.
///
/// A decode failure inside an assertion helper must be a test failure, never a
/// silent empty-string mismatch (which could false-pass negative assertions).
fn decode_or_panic(id: &usize, role: &str) -> String {
    praxis_graphlaw::encoding::Encoder::decode(id)
        .unwrap_or_else(|| panic!("term failed to decode in assertion helper: {role} id {id}"))
}

/// Assert that a triple exists in the store based on structural co-occurrence
/// (Fix #8: checks decoded triples, not substring containment)
pub fn assert_contains_triple(store: &TripleStore, s: &str, p: &str, o: &str) {
    let found = store.triple_index.triples.iter().any(|t| {
        let s_decoded = decode_or_panic(&t.s.to_encoded(), "subject");
        let p_decoded = decode_or_panic(&t.p.to_encoded(), "predicate");
        let o_decoded = decode_or_panic(&t.o.to_encoded(), "object");
        s_decoded.contains(s) && p_decoded.contains(p) && o_decoded.contains(o)
    });
    assert!(
        found,
        "Expected triple <{} {} {}> not found in store\n{}",
        s,
        p,
        o,
        store.content_to_string()
    );
}

/// Assert that a triple does not exist in the store
/// (Fix #8: checks decoded triples for structural co-occurrence)
pub fn assert_not_contains_triple(store: &TripleStore, s: &str, p: &str, o: &str) {
    let found = store.triple_index.triples.iter().any(|t| {
        let s_decoded = decode_or_panic(&t.s.to_encoded(), "subject");
        let p_decoded = decode_or_panic(&t.p.to_encoded(), "predicate");
        let o_decoded = decode_or_panic(&t.o.to_encoded(), "object");
        s_decoded.contains(s) && p_decoded.contains(p) && o_decoded.contains(o)
    });
    assert!(
        !found,
        "Triple <{} {} {}> was found in store but was expected to be absent:\n{}",
        s,
        p,
        o,
        store.content_to_string()
    );
}

/// Decode all triples to string representation
pub fn decode_all(triples: &[Triple]) -> Vec<String> {
    triples.iter().map(TripleStore::decode_triple).collect()
}

/// Helper to create a predicate IRI for testing
pub fn pred(name: &str) -> String {
    format!("http://example.org/{}", name)
}

/// Build a TripleIndex from Turtle data string
pub fn build_data_index(data_str: &str) -> TripleIndex {
    let triples = Parser::parse_triples(data_str, Syntax::Turtle).unwrap_or_else(|e| {
        let snippet: String = data_str.chars().take(200).collect();
        panic!("test fixture parse failed: {e:?}\ninput (first 200 chars): {snippet}")
    });
    let mut index = TripleIndex::new();
    for t in triples {
        index.add(t);
    }
    index
}

/// Materialize all inferred triples from a data string
pub fn materialize(data: &str) -> Vec<String> {
    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap_or_else(|e| {
        let snippet: String = data.chars().take(200).collect();
        panic!("test fixture materialization failed: {e:?}\ninput (first 200 chars): {snippet}")
    });
    decode_all(&inferred)
}
