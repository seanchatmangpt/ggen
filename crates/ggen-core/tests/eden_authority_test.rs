//! Phase 3: the Eden byte-class authority data model as an admitted graph (O*).
//!
//! Chicago TDD over the real repo artifacts. Proves the Eden data model:
//! - parses as real RDF (oxigraph);
//! - composes ONLY real public vocabulary (the Phase-2 alignment gate) —
//!   nothing fabricated;
//! - CONFORMS to its own SHACL shapes (the shapes are live, not decorative);
//! - and that those shapes have teeth (a malformed AuthorityDelta is rejected).
//!
//! Note on scope: this delivers the admitted authority graph O* (the contract a
//! future server implements). Forward codegen of the *custom* AuthorityDelta
//! Rust type requires the manifest/StagedPipeline custom-template path — the
//! low-level `--queries` pipeline only emits generic structs — and is a
//! documented follow-up.

use std::path::PathBuf;

use ggen_core::graph::Graph;
use ggen_core::reverse::check_alignment;
use ggen_core::validation::SparqlValidator;

/// Resolve a repo-root-relative path from this crate's manifest dir.
fn repo_path(rel: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(rel)
}

#[test]
fn eden_authority_parses_and_composes_real_public_terms() {
    let onto = repo_path(".specify/ontology/eden-authority.ttl");
    let ttl = std::fs::read_to_string(&onto).expect("read eden-authority.ttl");

    // Real RDF.
    let _graph = Graph::load_from_string(&ttl).expect("eden-authority.ttl must parse as RDF");
    // Core terms present.
    assert!(ttl.contains("eden:AuthorityDelta"));
    assert!(ttl.contains("eden:classByte"));
    assert!(ttl.contains("eden:Assembly"));

    // Composes ONLY real public terms (Phase 2 gate), and actually uses some.
    let vendored = repo_path(".specify/ontology/vendored");
    let report = check_alignment(&onto, &vendored).expect("alignment runs");
    assert!(
        report.is_aligned(),
        "eden model must compose only real public terms; unaligned={:?}",
        report.unaligned
    );
    assert!(
        !report.admitted.is_empty(),
        "eden model must genuinely compose public vocabulary (PROV/QUDT), got none"
    );
}

#[test]
fn eden_authority_conforms_to_its_shapes() {
    let onto_ttl =
        std::fs::read_to_string(repo_path(".specify/ontology/eden-authority.ttl")).expect("onto");
    let shapes_ttl =
        std::fs::read_to_string(repo_path(".specify/shapes/eden-authority.shacl.ttl")).expect("shapes");

    let ontology = Graph::load_from_string(&onto_ttl).expect("parse ontology");
    let shapes = Graph::load_from_string(&shapes_ttl).expect("parse shapes");

    let result = SparqlValidator::new()
        .validate(&ontology, &shapes)
        .expect("validation runs");
    assert!(
        result.passed,
        "the eden authority graph must conform to its shapes; violations={:?}",
        result.violations
    );
}

#[test]
fn shapes_reject_invalid_authority_delta() {
    // Teeth: an AuthorityDelta missing required dimension/classByte/tick must be
    // rejected by the shapes (impossible-state injection).
    let shapes_ttl =
        std::fs::read_to_string(repo_path(".specify/shapes/eden-authority.shacl.ttl")).expect("shapes");
    let shapes = Graph::load_from_string(&shapes_ttl).expect("parse shapes");

    let bad = "@prefix eden: <https://ggen.io/eden#> .\n\
               eden:BadDelta a eden:AuthorityDelta ; eden:objectId \"x\" .\n";
    let bad_graph = Graph::load_from_string(bad).expect("parse bad delta");

    let result = SparqlValidator::new()
        .validate(&bad_graph, &shapes)
        .expect("validation runs");
    assert!(
        !result.passed,
        "an AuthorityDelta missing required fields must be rejected"
    );
    assert!(result.violation_count > 0, "violations must be reported");
}
