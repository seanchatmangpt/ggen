#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes
)]
//! G5 evidence: (1) graph hashing determinism through the SPARQL query
//! surface (complementing `hash_stability.rs`'s direct insert/remove-based
//! coverage with a query-derived-state angle), and (2) a real invocation of
//! `scripts/ci/guard-process-intelligence-boundary.sh` -- the actual
//! mechanism `just pre-commit` runs to enforce the Process Intelligence
//! Boundary (root `CLAUDE.md`) -- proving it both passes on the real
//! workspace and genuinely fires on a fabricated violation, rather than
//! reimplementing a parallel (and possibly divergent) detector.
//!
//! Chicago TDD: real oxigraph-backed `DeterministicGraph`, a real `bash`
//! subprocess running the real guard script against a real temp
//! filesystem. No mocks.

use std::path::Path;
use std::process::Command;

use ggen_graph::DeterministicGraph;

/// Positive witness: the same RDF content, loaded via two different
/// SPARQL-query-observed paths (direct quad insertion vs. inserting the
/// same triples through a second graph and comparing derived state), hashes
/// identically -- and a real SPARQL CONSTRUCT-visible semantic change
/// (adding a triple that the query surface can observe) changes the hash.
#[test]
fn state_hash_is_stable_under_query_observation_and_sensitive_to_content() {
    let g1 = DeterministicGraph::new().expect("graph 1");
    let g2 = DeterministicGraph::new().expect("graph 2");

    let triples = [
        "<http://example.org/order/1> <http://example.org/status> \"open\" .",
        "<http://example.org/order/2> <http://example.org/status> \"closed\" .",
        "<http://example.org/order/3> <http://example.org/status> \"open\" .",
    ];
    for t in &triples {
        let q = DeterministicGraph::parse_nquad(t).expect("parse");
        g1.insert_quad(&q).expect("insert g1");
    }
    // Same content, reverse insertion order, into an independently
    // constructed graph.
    for t in triples.iter().rev() {
        let q = DeterministicGraph::parse_nquad(t).expect("parse");
        g2.insert_quad(&q).expect("insert g2");
    }

    let h1 = g1.state_hash().expect("hash g1");
    let h2 = g2.state_hash().expect("hash g2");
    assert_eq!(
        h1, h2,
        "identical content must hash identically regardless of insertion order"
    );

    // Confirm both graphs actually observe the same content via SPARQL
    // (grounds the hash equality in real, queryable state -- not a
    // coincidence of two hash values).
    let count_query = "SELECT (COUNT(*) AS ?n) WHERE { ?s <http://example.org/status> \"open\" }";
    let open_count = |g: &DeterministicGraph| -> u64 {
        let results = g.query(count_query).expect("query");
        if let oxigraph::sparql::QueryResults::Solutions(mut sols) = results {
            let sol = sols.next().expect("one row").expect("solution");
            match sol.get("n") {
                Some(oxigraph::model::Term::Literal(l)) => l.value().parse().unwrap_or(0),
                _ => 0,
            }
        } else {
            0
        }
    };
    assert_eq!(open_count(&g1), 2);
    assert_eq!(open_count(&g2), 2);

    // Negative falsifier: a real, queryable semantic change (adding a
    // fourth order) must change the hash -- proving the hash is not a
    // constant or a function of graph-construction alone.
    let extra = DeterministicGraph::parse_nquad(
        "<http://example.org/order/4> <http://example.org/status> \"open\" .",
    )
    .expect("parse extra");
    g1.insert_quad(&extra).expect("insert extra");
    let h1_after = g1.state_hash().expect("hash g1 after change");
    assert_ne!(h1, h1_after, "adding a real triple must change state_hash");
    assert_eq!(
        open_count(&g1),
        3,
        "the added triple must be observable via SPARQL"
    );
}

fn workspace_root() -> std::path::PathBuf {
    // CARGO_MANIFEST_DIR = <root>/crates/ggen-graph
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

/// Positive witness: the real Process Intelligence Boundary guard
/// (`scripts/ci/guard-process-intelligence-boundary.sh`, wired into `just
/// pre-commit`) currently passes when run against the live workspace --
/// i.e. it is not merely present but wired to a real, currently-green
/// check.
#[test]
fn process_intelligence_boundary_guard_passes_on_live_workspace() {
    let root = workspace_root();
    let script = root.join("scripts/ci/guard-process-intelligence-boundary.sh");
    assert!(
        script.exists(),
        "guard script must exist at {}",
        script.display()
    );

    let output = Command::new("bash")
        .arg(&script)
        .current_dir(&root)
        .output()
        .expect("run guard script");

    assert!(
        output.status.success(),
        "guard script must currently pass on the live workspace.\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("OK:"),
        "guard script must emit a real OK evidence line, got: {stdout}"
    );
}

/// Negative falsifier: the guard script must genuinely detect a forbidden
/// praxis-graphlaw "chatman" module reference (see the guard script's own
/// FAIL message), proving it is a real check and not a no-op that always
/// exits 0. Run against a fabricated `crates/`/`src/` tree in a TempDir
/// (never touching the real repo) so this test cannot itself introduce a
/// boundary violation, and the forbidden path string itself is built at
/// runtime (not written as a literal below) so the real guard scanning
/// this workspace does not flag this very file.
#[test]
fn process_intelligence_boundary_guard_detects_fabricated_violation() {
    let root = workspace_root();
    let script_src = root.join("scripts/ci/guard-process-intelligence-boundary.sh");
    let script_body = std::fs::read_to_string(&script_src).expect("read guard script");

    let tmp = tempfile::TempDir::new().expect("tempdir");
    std::fs::create_dir_all(tmp.path().join("crates/fake-crate/src")).expect("mkdir");
    std::fs::create_dir_all(tmp.path().join("scripts/ci")).expect("mkdir");
    std::fs::write(
        tmp.path()
            .join("scripts/ci/guard-process-intelligence-boundary.sh"),
        &script_body,
    )
    .expect("copy guard script");
    // Built at runtime (not a literal in this file) so the real guard script
    // -- which scans this whole workspace, including this test file itself
    // -- does not flag this crate's own test suite as the violation it's
    // deliberately fabricating inside an isolated TempDir.
    let forbidden_path = ["praxis_graphlaw", "chatman", "Conformance"].join("::");
    let fabricated_violation = format!("fn analyze() {{ let _ = {forbidden_path}::check(); }}\n");
    std::fs::write(
        tmp.path().join("crates/fake-crate/src/bad.rs"),
        fabricated_violation,
    )
    .expect("write fabricated violation");

    let output = Command::new("bash")
        .arg("scripts/ci/guard-process-intelligence-boundary.sh")
        .current_dir(tmp.path())
        .output()
        .expect("run guard script against fabricated tree");

    assert!(
        !output.status.success(),
        "guard script must fail (non-zero exit) when a fabricated forbidden-module \
         reference ({forbidden_path}) is present outside the excluded dirs"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("FAIL"),
        "guard script must emit a real FAIL evidence line naming the violation, got: {stderr}"
    );
}
