//! Generator for `fixtures/ea-graph.json` (never hand-edit the fixture; re-run this):
//! `cargo run --example emit_fixture > fixtures/ea-graph.json`

// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
fn main() {
    let g = ggen_abb_sbb::synthetic_graph(2, 3);
    println!(
        "{}",
        serde_json::to_string_pretty(&g).expect("graph serializes")
    );
}
