//! Courts: `[FM-GEN-009]` (existing generation-rule target unreadable as
//! UTF-8) and `[FM-GEN-010]` (merge markers out of order, fail closed).
//!
//! Chicago: real temp-dir projects with `[[generation.rules]]`, real
//! ontology, real oxigraph store, real Tera render — driven through the real
//! `ggen_engine::sync::sync` entry (the generation-rules module is private;
//! `sync::sync` is its only external door). No mocks.
//! Emission sites: `crates/ggen-engine/src/generation_rules.rs`
//! `decide_and_maybe_apply` (`fm_gen(9, ...)`) and
//! `merge::merge_sections` (`fm_gen(10, ...)`).
//! Closes GAP-3 for FM-GEN-009/010 (CARRY-09, zd-48).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn write_project(root: &std::path::Path, rule: &str) {
    std::fs::write(
        root.join("ggen.toml"),
        format!(
            "[project]\nname = \"genrefusal\"\nversion = \"0.1.0\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n{rule}"
        ),
    )
    .expect("write ggen.toml");
    std::fs::write(
        root.join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:proj ex:version \"v1\" .\n",
    )
    .expect("write ontology.ttl");
}

const MERGE_RULE: &str = "[[generation.rules]]\nname = \"versioned\"\nmode = \"Merge\"\nquery = { inline = \"SELECT ?v WHERE { ?s <http://example.org/version> ?v } ORDER BY ?v\" }\ntemplate = { inline = \"fn version() -> &'static str { \\\"{{ row.v }}\\\" }\" }\noutput_file = \"src/generated.rs\"\n";

fn opts() -> SyncOptions {
    SyncOptions {
        skip_ea_admission: true,
        ..Default::default()
    }
}

/// `[FM-GEN-009]`: an existing target that is not valid UTF-8 is a typed
/// refusal, not a silent overwrite.
#[test]
fn non_utf8_existing_target_refuses_merge_sync_with_fm_gen_009() {
    let dir = TempDir::new().expect("tempdir");
    write_project(dir.path(), MERGE_RULE);
    std::fs::create_dir_all(dir.path().join("src")).expect("mkdir src");
    std::fs::write(
        dir.path().join("src/generated.rs"),
        [0xFFu8, 0xFE, 0x00, 0x9F],
    )
    .expect("write binary target");

    let err = sync(dir.path(), opts()).expect_err("non-UTF-8 existing target must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-GEN-009"), "{msg}");
    assert!(msg.contains("UTF-8"), "{msg}");
    assert!(
        msg.contains("src/generated.rs"),
        "refusal must name the target: {msg}"
    );
    // Real file on disk must be untouched (fail closed, no corruption spread).
    let bytes = std::fs::read(dir.path().join("src/generated.rs")).expect("read target back");
    assert_eq!(
        bytes,
        [0xFFu8, 0xFE, 0x00, 0x9F],
        "target bytes must be unchanged"
    );
}

/// `[FM-GEN-010]`: merge markers present but out of order (`=======` before
/// `<<<<<<< GENERATED`) fail closed instead of guessing at intent.
#[test]
fn out_of_order_merge_markers_refuse_sync_with_fm_gen_010() {
    let dir = TempDir::new().expect("tempdir");
    write_project(dir.path(), MERGE_RULE);
    std::fs::create_dir_all(dir.path().join("src")).expect("mkdir src");
    std::fs::write(
        dir.path().join("src/generated.rs"),
        "=======\n<<<<<<< GENERATED\nstale body\n>>>>>>> MANUAL\n",
    )
    .expect("write out-of-order target");

    let err = sync(dir.path(), opts()).expect_err("out-of-order merge markers must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-GEN-010"), "{msg}");
    assert!(
        msg.contains("marker order"),
        "refusal must name the disorder: {msg}"
    );
    let on_disk =
        std::fs::read_to_string(dir.path().join("src/generated.rs")).expect("read target back");
    assert!(
        on_disk.contains("stale body"),
        "fail-closed: the ambiguous file must not be rewritten:\n{on_disk}"
    );
}
