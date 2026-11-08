//! G12 — sync-hardening plan item 1, flag-off path: corpus-freshness gate
//! on the `ggen check` surface (`ggen graph validate` project mode).
//!
//! Golden tests over a real on-disk project through the public
//! `handle_graph_validate` entry point (Chicago: real collaborators, real
//! files, no mocks):
//!
//! 1. fresh corpus (bytes == sorted `ontology/*.ttl` concat) → check passes;
//! 2. stale corpus (constituent edited after regen) → typed refusal
//!    `[FM-ONTOLOGY-CHK-001]` naming the stale constituent;
//! 3. corpus pattern undetectable (source not named
//!    `ontology-corpus.ttl`) → gate silent, legacy behavior preserved;
//! 4. `[ontology.aggregate]` present → gate is a no-op (flag-on path is
//!    sync.rs's / G2's).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::verbs::handlers::handle_graph_validate;

const CONSTITUENT_A: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
"#;

const CONSTITUENT_B: &str = r#"
@prefix ex: <http://example.org/> .
ex:bob ex:name "bob" .
"#;

/// Deterministic concat law (matches
/// `sync::enforce_ontology_aggregate`): sorted rel-path order, `\n`-joined,
/// trailing newline ensured.
fn concat(parts: &[&str]) -> String {
    let mut concat = parts.join("\n");
    if !concat.ends_with('\n') {
        concat.push('\n');
    }
    concat
}

fn write(root: &Path, rel: &str, content: &str) {
    let path = root.join(rel);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdir");
    }
    std::fs::write(path, content).expect("write");
}

fn read(root: &Path, rel: &str) -> String {
    std::fs::read_to_string(root.join(rel)).expect(rel)
}

/// Scaffold a detectable corpus project: `ontology-corpus.ttl` at the
/// workspace root, constituents under `ontology/`.
fn scaffold(root: &Path, corpus_body: &str) {
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    write(
        root,
        "ggen.toml",
        "[project]\nname = \"chk\"\n\n[ontology]\nsource = \"ontology-corpus.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    );
    write(root, "ontology/a.ttl", CONSTITUENT_A);
    write(root, "ontology/b.ttl", CONSTITUENT_B);
    write(root, "ontology-corpus.ttl", corpus_body);
}

/// Run the check surface with the process cwd pinned to `root`
/// (`handle_graph_validate` project mode resolves the root as the cwd).
fn check(root: &Path) -> Result<serde_json::Value, String> {
    std::env::set_current_dir(root).expect("set cwd");
    match handle_graph_validate(Vec::new(), Vec::new()) {
        Ok(v) => Ok(v),
        Err(e) => Err(e.to_string()),
    }
}

/// One serialized test: the check surface pins the process cwd, so the
/// scenarios must not run concurrently against each other.
#[test]
fn corpus_freshness_flagoff_golden() {
    // (corpus body, expect_ok, message fragment expected on failure,
    //  stale-constituent name expected on failure — None = divergence-type
    //  failure where no per-constituent name is asserted)
    let scenarios = [
        // Fresh: exact sorted concat.
        (concat(&[CONSTITUENT_A, CONSTITUENT_B]), true, "", None),
        // Unsorted concat: NOT the lawful aggregate (sorted rel-path order
        // is the law) — typed stale refusal, whole-corpus divergence class.
        (
            concat(&[CONSTITUENT_B, CONSTITUENT_A]),
            false,
            "[FM-ONTOLOGY-CHK-001]",
            None,
        ),
        // Stale: b.ttl edited after the last regen — refusal must name it.
        (
            concat(&[
                CONSTITUENT_A,
                r#"
@prefix ex: <http://example.org/> .
ex:bob ex:name "bob-old" .
"#,
            ]),
            false,
            "[FM-ONTOLOGY-CHK-001]",
            Some("ontology/b.ttl"),
        ),
    ];

    for (i, (corpus_body, expect_ok, err_fragment, stale_name)) in scenarios.iter().enumerate() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();
        scaffold(root, corpus_body);
        let result = check(root);
        assert_eq!(result.is_ok(), *expect_ok, "scenario {i}: got {result:?}");
        if let Err(msg) = result {
            assert!(msg.contains(err_fragment), "scenario {i}: {msg}");
            if let Some(name) = stale_name {
                assert!(
                    msg.contains(name),
                    "refusal must name the stale constituent: {msg}"
                );
            }
        }
    }

    // Undetectable pattern: source not corpus-named → gate silent even on
    // divergent content (legacy behavior for hand-authored ontologies).
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    write(
        root,
        "ggen.toml",
        "[project]\nname = \"chk\"\n\n[ontology]\nsource = \"hand.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    );
    write(root, "ontology/a.ttl", CONSTITUENT_A);
    write(root, "ontology/b.ttl", CONSTITUENT_B);
    write(root, "hand.ttl", CONSTITUENT_B);
    check(root).expect("undetectable pattern: gate must stay silent");

    // Flag-on no-op: `[ontology.aggregate]` present → the gate defers to
    // sync.rs (G2) and the check surface stays silent even on a stale
    // corpus (sync refuses there, typed `[FM-ONTOLOGY-AGG-001]`).
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    write(
        root,
        "ggen.toml",
        "[project]\nname = \"chk\"\n\n[ontology]\nsource = \"ontology-corpus.ttl\"\n\n[templates]\ndir = \"templates\"\n\n[ontology.aggregate]\noutput = \"ontology-corpus.ttl\"\nconstituents = [\"ontology/a.ttl\", \"ontology/b.ttl\"]\n",
    );
    write(root, "ontology/a.ttl", CONSTITUENT_A);
    write(root, "ontology/b.ttl", CONSTITUENT_B);
    // Stale w.r.t. the sorted-concat law, but parseable: the flag-on
    // deferral (not the parse) is what this scenario witnesses.
    let stale_but_parseable = concat(&[CONSTITUENT_B, CONSTITUENT_A]);
    write(root, "ontology-corpus.ttl", &stale_but_parseable);
    check(root).expect("flag-on path: check defers to sync.rs");
    assert_eq!(read(root, "ontology-corpus.ttl"), stale_but_parseable);

    // Non-vacuity witness: the SAME stale corpus under the flag-off
    // detection law IS refused — the deferral above is a real branch, not a
    // vacuous pass.
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    scaffold(root, "stale bytes");
    let err = check(root).expect_err("flag-off + stale must refuse");
    assert!(err.contains("[FM-ONTOLOGY-CHK-001]"), "{err}");
}
