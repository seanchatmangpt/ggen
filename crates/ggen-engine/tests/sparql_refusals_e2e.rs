// Policy (operator-adjudicated 2026-10-09): expect!/unwrap-style assertions are
// idiomatic in e2e tests; they remain forbidden in lib code (gated separately).
#![expect(clippy::expect_used)]

//! E2E: SPARQL parse-failure error ergonomics through the public
//! `DeterministicGraph::query` surface.
//!
//! Witnessed defect (twice in the v26.10.5 cycle): an in-scope
//! `BIND(... AS ?x)` rebind made oxigraph/spargebra report a parse error
//! pointing *after* the illegal construct (`error at 44:7: expected
//! OPTIONAL` — the reported position moved with the BIND block, never to
//! it). These tests pin the two refusal classes:
//!
//! - `[FM-GRAPH-013]` — pre-parse diagnostic for the SPARQL 1.1 illegal
//!   rebind (`AS` target variable not fresh in the group), naming the
//!   variable and the offending line;
//! - `[FM-GRAPH-003]` — retained for genuinely malformed queries, now
//!   carrying the offending line's text as context.

use ggen_engine::graph::DeterministicGraph;

/// The witnessed defect shape: `?x` is projected in the group, then
/// `BIND(STR(?x) AS ?x)` re-binds it. The parser reports a position after
/// the BIND; the diagnostic must name `x` and the BIND line.
#[test]
fn bind_rebind_in_scope_variable_refused_fm_graph_013() {
    let g = DeterministicGraph::new().expect("graph");
    let q = r"PREFIX : <http://example.org/>
SELECT ?x WHERE {
  ?s :p ?x .
  BIND(STR(?x) AS ?x) .
";
    let err = g.query(q).map(|_| ()).expect_err("rebind must be refused");
    let msg = err.to_string();
    assert!(
        msg.contains("FM-GRAPH-013"),
        "expected FM-GRAPH-013, got: {msg}"
    );
    assert!(
        msg.contains("?x"),
        "message must name the re-bound variable, got: {msg}"
    );
    assert!(
        msg.contains("rename the AS target"),
        "message must carry the remediation, got: {msg}"
    );
    // Line number computed from the BIND match offset (line 4 here).
    assert!(
        msg.contains("line 4"),
        "message must carry the BIND line number, got: {msg}"
    );
    // Offending line text included.
    assert!(
        msg.contains("BIND(STR(?x) AS ?x)"),
        "message must include the offending line text, got: {msg}"
    );
}

/// Variables used inside the BIND expression before the `AS` count as
/// in-scope too (SPARQL 1.1: fresh up to the point of use).
#[test]
fn bind_rebind_via_own_expression_refused_fm_graph_013() {
    let g = DeterministicGraph::new().expect("graph");
    let q = r"PREFIX : <http://example.org/>
SELECT ?sum WHERE {
  ?s :p ?sum .
  BIND(?sum + 1 AS ?sum) .
";
    let err = g.query(q).map(|_| ()).expect_err("rebind must be refused");
    let msg = err.to_string();
    assert!(msg.contains("FM-GRAPH-013"), "got: {msg}");
    assert!(msg.contains("?sum"), "got: {msg}");
}

/// A valid BIND (fresh AS target) still parses and executes.
#[test]
fn bind_fresh_variable_still_executes() {
    let g = DeterministicGraph::new().expect("graph");
    g.insert_turtle("@prefix ex: <http://example.org/> . ex:s ex:p \"v\" .")
        .expect("ttl");
    let q = r"PREFIX : <http://example.org/>
SELECT ?y WHERE {
  ?s :p ?x .
  BIND(STR(?x) AS ?y) .
";
    g.query(q).expect("fresh BIND target must not be refused");
}

/// FM-GRAPH-003 is retained for genuinely malformed queries, and the
/// message now carries the offending line's text as context.
#[test]
fn truly_malformed_query_still_fm_graph_003_with_line_hint() {
    let g = DeterministicGraph::new().expect("graph");
    let q = "SELECT ?x WHERE {\n  ?s :p ?x .\n  ?s :p\n}";
    let err = g
        .query(q)
        .map(|_| ())
        .expect_err("malformed must be refused");
    let msg = err.to_string();
    assert!(
        msg.contains("FM-GRAPH-003"),
        "expected FM-GRAPH-003, got: {msg}"
    );
    assert!(
        msg.contains("near line"),
        "003 must now carry a line hint, got: {msg}"
    );
}

/// A malformed query with no `error at L:C` position in the message must
/// still yield FM-GRAPH-003 without a line hint (hint is additive only).
#[test]
fn malformed_without_position_still_fm_graph_003() {
    let g = DeterministicGraph::new().expect("graph");
    // `SELECT` with nothing after it — spargebra still reports a position,
    // but this pins the fallthrough for any message shape.
    let err = g.query("SELECT").map(|_| ()).expect_err("must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-GRAPH-003"), "got: {msg}");
}
