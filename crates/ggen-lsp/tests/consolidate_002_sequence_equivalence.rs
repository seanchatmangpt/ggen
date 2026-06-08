#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! CONSOLIDATE-002 — multi-species OCEL SEQUENCE-EQUIVALENCE proof.
//!
//! CONSOLIDATE-002 merges the three order-sensitive Phase-A publish branches in
//! [`ServerState::analyze_and_observe`](ggen_lsp::ServerState::analyze_and_observe)
//! (the TPL / HARNESS / OUT group loops) into ONE species-driven loop. Those
//! publishes write the editor-flow OCEL chain to the EXTERNAL on-disk log
//! (`.ggen/ocel/agent-edit-events.ocel.jsonl`) in CALL ORDER. Reordering the
//! publishes would change the emitted event SEQUENCE — a FAKE-LIVE defect that
//! pass-counts cannot detect.
//!
//! This test pins the emitted sequence with a *committed golden*. The golden is
//! the ordered list of normalized `(activity, root_relative_file, diagnostic_code)`
//! tuples, read in on-disk (= call/write) order from the real OCEL log produced by
//! the REAL orchestration over a real fixture project. Normalization drops only the
//! nondeterministic fields (event `id`/blake3 receipt id, `timestamp`, `run_id` /
//! `session_id`, and the TempDir-absolute path prefix). The ACTIVITY ORDER, the
//! per-event `file` object id (made root-relative), and the `diagnostic_code` object
//! id are KEPT — they ARE the sequence key.
//!
//! The canonical multi-species single-pass scenario: editing `ggen.toml` is a
//! `tpl_is_trigger` surface, so ONE `analyze_and_observe` pass fires BOTH the TPL
//! detector (anchored on the `.tera`, which consumes an unbound projection var) AND
//! the OUT detector (anchored on the `ggen.toml`, whose `output_file` consumes an
//! unbound var). Phase order TPL → OUT must be byte/sequence-identical pre- and
//! post-merge.
//!
//! Chicago TDD: the golden was captured from REAL execution of production code over
//! a real on-disk fixture (see [`generate_golden_sequence`], an `#[ignore]`-gated
//! generator). No fabricated events, no hand-built JSONL, no mocks.

use std::path::{Path, PathBuf};

use ggen_lsp::ServerState;
use tower_lsp_max::lsp_types::Url;

/// Absolute path to the committed multi-species fixture project root.
fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("consolidate_002_multispecies")
}

/// Absolute path to the committed golden normalized-sequence file.
fn golden_path() -> PathBuf {
    fixture_root().join("golden-sequence.jsonl")
}

/// Recursively copy a directory tree using only std (+ tempfile). Real I/O.
fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
}

/// Read every line of the EXTERNAL on-disk OCEL log under `<root>` (the same
/// unforgeable surface `oracle_tape_generator.rs::read_log_lines` reads).
fn read_log_lines(root: &Path) -> Vec<String> {
    let path = root
        .join(".ggen")
        .join("ocel")
        .join("agent-edit-events.ocel.jsonl");
    std::fs::read_to_string(path)
        .unwrap_or_default()
        .lines()
        .map(str::to_string)
        .collect()
}

/// One normalized OCEL event: the sequence-key tuple, in on-disk (call) order.
/// `file` is made TempDir-prefix-independent by keeping only the path SUFFIX from
/// the fixture-project root (e.g. `/templates/item.tera`, `/ggen.toml`).
#[derive(Debug, Clone, PartialEq, Eq)]
struct NormEvent {
    activity: String,
    file: String,
    code: String,
}

/// Extract the object id of the first object whose `type` matches `obj_type`.
fn object_id(v: &serde_json::Value, obj_type: &str) -> Option<String> {
    v.get("objects")?.as_array()?.iter().find_map(|o| {
        if o.get("type").and_then(|t| t.as_str()) == Some(obj_type) {
            o.get("id").and_then(|i| i.as_str()).map(str::to_string)
        } else {
            None
        }
    })
}

/// Make a `file` object id (an absolute path under the hermetic project root)
/// independent of the TempDir prefix by keeping the suffix from the project root
/// directory name onward. The fixture root dir is named `consolidate_002_multispecies`
/// (real run) or `project` (the copied-tree TempDir). We normalize by cutting at the
/// LAST occurrence of either anchor segment so only the in-project relative path
/// remains — the load-bearing identity for sequence equivalence.
fn root_relative(file: &str) -> String {
    for anchor in ["/project/", "/consolidate_002_multispecies/"] {
        if let Some(idx) = file.rfind(anchor) {
            return file[idx + anchor.len() - 1..].to_string();
        }
    }
    file.to_string()
}

/// Parse the on-disk OCEL log into the ordered normalized sequence. The order is
/// the file's line order, which IS the `observe_diagnostics` call order (each call
/// appends its event batch via `IntelLog::append`). We keep ONLY the events that
/// carry a `diagnostic_code` object (every agent-edit event does) and normalize
/// away `id`/`timestamp`/`run_id`/`session_id`/`receipt_id`/TempDir-path.
fn normalized_sequence(root: &Path) -> Vec<NormEvent> {
    read_log_lines(root)
        .iter()
        .filter_map(|line| {
            let v: serde_json::Value = serde_json::from_str(line).ok()?;
            let activity = v.get("activity")?.as_str()?.to_string();
            let file = object_id(&v, "file")?;
            let code = object_id(&v, "diagnostic_code")?;
            Some(NormEvent {
                activity,
                file: root_relative(&file),
                code,
            })
        })
        .collect()
}

/// Serialize a normalized sequence to NDJSON (one tuple per line, trailing
/// newline) — the committed golden's on-disk shape.
fn sequence_to_ndjson(seq: &[NormEvent]) -> String {
    let mut out = String::new();
    for e in seq {
        out.push_str(&format!(
            "{{\"activity\":{},\"file\":{},\"code\":{}}}\n",
            serde_json::to_string(&e.activity).expect("activity str"),
            serde_json::to_string(&e.file).expect("file str"),
            serde_json::to_string(&e.code).expect("code str"),
        ));
    }
    out
}

/// Parse a committed golden NDJSON back into the normalized sequence.
fn ndjson_to_sequence(ndjson: &str) -> Vec<NormEvent> {
    ndjson
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            let v: serde_json::Value = serde_json::from_str(l).expect("golden line is JSON");
            NormEvent {
                activity: v["activity"].as_str().expect("activity").to_string(),
                file: v["file"].as_str().expect("file").to_string(),
                code: v["code"].as_str().expect("code").to_string(),
            }
        })
        .collect()
}

/// Drive the REAL multi-species single-pass scenario in a hermetic TempDir and
/// return the emitted normalized OCEL sequence. Editing `ggen.toml` fires BOTH the
/// TPL detector (`.tera` anchor) AND the OUT detector (`ggen.toml` anchor) in ONE
/// `analyze_and_observe` pass — Phase A TPL → OUT, no HARNESS (basenames disjoint).
async fn capture_multispecies_sequence(tmp_root: &Path) -> Vec<NormEvent> {
    let dst = tmp_root.join("project");
    copy_tree(&fixture_root(), &dst).expect("copy fixture tree");
    let state = ServerState::with_root(&dst);

    let manifest_path = dst.join("ggen.toml");
    let manifest_uri = Url::from_file_path(&manifest_path).expect("manifest url");
    let manifest_src = std::fs::read_to_string(&manifest_path).expect("read ggen.toml");

    // ONE pass: ggen.toml is a tpl_is_trigger surface → TPL (.tera) + OUT (ggen.toml).
    let _ = state
        .analyze_and_observe(&manifest_uri, &manifest_src)
        .await;

    normalized_sequence(&dst)
}

/// GENERATOR (NOT a CI assertion): captures the golden normalized sequence from the
/// CURRENT (pre-merge or post-merge) production code and writes it to the committed
/// fixture. Run with:
///
/// ```bash
/// cargo test -p ggen-lsp --test consolidate_002_sequence_equivalence -- --ignored --nocapture
/// ```
///
/// Per CONSOLIDATE-002 protocol, the golden is captured ONCE from PRE-MERGE code
/// (the pre-merge truth) and committed; the assertion test below then proves the
/// POST-MERGE code reproduces it byte-for-byte (modulo the normalized fields).
#[ignore = "generator: writes the committed CONSOLIDATE-002 golden sequence; run with --ignored"]
#[tokio::test]
async fn generate_golden_sequence() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let seq = capture_multispecies_sequence(tmp.path()).await;
    assert!(
        !seq.is_empty(),
        "generator precondition: the multi-species pass must emit a non-empty OCEL \
         sequence"
    );
    let ndjson = sequence_to_ndjson(&seq);
    std::fs::write(golden_path(), ndjson.as_bytes()).expect("write golden");
    eprintln!(
        "WROTE GOLDEN ({} events): {}",
        seq.len(),
        golden_path().display()
    );
    for e in &seq {
        eprintln!("  {} {} {}", e.activity, e.file, e.code);
    }
}

/// SEQUENCE-EQUIVALENCE assertion: the emitted normalized OCEL sequence for the
/// multi-species single-pass scenario MUST equal the committed golden, IN ORDER.
/// This catches a TPL↔OUT phase swap (which pass-counts cannot), a broken
/// `own_diags` merge-once (double-publish on the self-anchored `ggen.toml`), or any
/// other reordering introduced by the species-loop merge.
#[tokio::test]
async fn multispecies_sequence_matches_golden() {
    // ── Arrange: the committed golden (captured from pre-merge code).
    let golden_raw = std::fs::read_to_string(golden_path()).unwrap_or_else(|e| {
        panic!(
            "missing committed golden {} ({e}). Regenerate with: cargo test -p ggen-lsp \
             --test consolidate_002_sequence_equivalence -- --ignored",
            golden_path().display()
        )
    });
    let golden = ndjson_to_sequence(&golden_raw);
    assert!(
        !golden.is_empty(),
        "committed golden must be non-empty (else the equivalence is vacuous)"
    );

    // ── Act: capture the CURRENT code's emitted sequence for the same scenario.
    let tmp = tempfile::tempdir().expect("tempdir");
    let actual = capture_multispecies_sequence(tmp.path()).await;

    // ── Assert: byte/sequence-identical (normalized). Ordered equality — NOT a
    // multiset / pass-count comparison.
    assert_eq!(
        actual,
        golden,
        "multi-species OCEL sequence diverged from the committed golden. A reordered \
         TPL/OUT phase or a broken own_diags merge-once is FAKE-LIVE.\n\
         actual:\n{}\nexpected (golden):\n{}",
        sequence_to_ndjson(&actual),
        sequence_to_ndjson(&golden),
    );
}

/// Guards the scenario's load-bearing shape so a future fixture edit cannot make
/// the equivalence vacuous: the golden MUST contain BOTH species (a TPL-001 event
/// on the `.tera` AND an OUT-001 event on the `ggen.toml`), proving it is a genuine
/// multi-species pass.
#[test]
fn golden_is_genuinely_multispecies() {
    let golden_raw = std::fs::read_to_string(golden_path()).expect("committed golden must exist");
    let golden = ndjson_to_sequence(&golden_raw);

    let has_tpl_on_tera = golden
        .iter()
        .any(|e| e.code == "GGEN-TPL-001" && e.file.ends_with("item.tera"));
    let has_out_on_toml = golden
        .iter()
        .any(|e| e.code == "GGEN-OUT-001" && e.file.ends_with("ggen.toml"));

    assert!(
        has_tpl_on_tera,
        "golden must contain a GGEN-TPL-001 event anchored on the .tera (the TPL phase). \
         golden:\n{golden:#?}"
    );
    assert!(
        has_out_on_toml,
        "golden must contain a GGEN-OUT-001 event anchored on the ggen.toml (the OUT phase). \
         golden:\n{golden:#?}"
    );
}
