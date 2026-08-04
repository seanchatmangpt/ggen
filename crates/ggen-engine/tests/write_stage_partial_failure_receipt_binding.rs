//! Chicago-TDD proof for red-team finding F1 (engine-render-write,
//! contract-drift, `crates/ggen-engine/src/sync.rs`): the Write stage's
//! per-output loop applies real filesystem writes one-by-one; a mid-loop I/O
//! failure must not leave the outputs that *did* land on disk unrecorded by
//! the receipt. Real filesystem, real `sync()`, real BLAKE3 re-hash of the
//! actual on-disk bytes -- no mocks, no test doubles.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n";

/// `a.tmpl` sorts before `b.tmpl` (`load_templates` sorts paths before
/// processing them), so its write is always attempted -- and, absent
/// sabotage, always succeeds -- before `b.tmpl`'s write is attempted.
const TEMPLATE_A: &str = "---\nto: out/a.rs\nforce: true\n---\n// a content\n";
const TEMPLATE_B: &str = "---\nto: out/b.rs\nforce: true\n---\n// b content\n";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/a.tmpl"), TEMPLATE_A).expect("write template a");
    std::fs::write(root.join("templates/b.tmpl"), TEMPLATE_B).expect("write template b");
}

/// A real, unmocked mid-loop I/O failure: `out/b.rs`'s target path is
/// pre-occupied by an actual directory. `plan_write` (`crate::write`) reads
/// the existing target as UTF-8 to decide the write outcome
/// (`std::fs::read_to_string(&target)?`); reading a directory as a file is a
/// genuine `io::Error` ("Is a directory"), propagated via `?` through
/// `plan_write` -> `apply` -> the Write-stage loop in `sync()` -- exactly the
/// finding's "mid-loop I/O failure" shape, produced by real OS filesystem
/// semantics rather than an injected fault.
fn sabotage_second_output(root: &Path) {
    std::fs::create_dir_all(root.join("out/b.rs")).expect("pre-occupy out/b.rs with a directory");
}

/// Before the fix: `sync()` fails mid-loop (as expected -- this is not what
/// this test is proving), but the pre-fix code path returned that error
/// straight from the write loop and NEVER reached `write_receipt`, so
/// `.ggen-v2/receipt.json` would not exist at all afterward even though
/// `out/a.rs` was really written to disk. This test's job is the opposite
/// assertion: after the fix, a receipt IS written, and it correctly binds
/// `out/a.rs`'s real on-disk bytes while omitting `out/b.rs` (which never
/// actually landed).
#[test]
fn partial_write_failure_still_binds_succeeded_outputs_in_the_receipt() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    sabotage_second_output(dir.path());

    let result = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    );

    // The sabotage must actually be hit: sync fails.
    let err = result.expect_err("sync must fail: out/b.rs's target is occupied by a directory");
    let err_msg = err.to_string();
    assert!(
        !err_msg.is_empty(),
        "sync's error must carry a real message, got empty"
    );

    // The file that succeeded before the failure is really on disk --
    // this is the "already-written files stay permanently on disk" half of
    // the finding, which was never in dispute.
    let a_path = dir.path().join("out/a.rs");
    assert!(
        a_path.exists(),
        "out/a.rs must have been written before the loop hit out/b.rs's failure"
    );
    let a_bytes = std::fs::read(&a_path).expect("read out/a.rs");

    // The other half of the finding, now fixed: the receipt must exist and
    // must bind out/a.rs's REAL on-disk bytes, not silently omit it because
    // the run overall failed.
    let receipt_path = dir.path().join(RECEIPT_REL_PATH);
    assert!(
        receipt_path.exists(),
        "a receipt must be persisted for the outputs that succeeded before the \
         mid-loop failure, closing the contract-drift window (finding F1) -- \
         found no {} after a partial write failure",
        receipt_path.display()
    );
    let raw = std::fs::read_to_string(&receipt_path).expect("read receipt.json");
    let receipt: SyncReceipt = serde_json::from_str(&raw).expect("parse receipt.json");

    let expected_hash = blake3::hash(&a_bytes).to_hex().to_string();
    let recorded_hash = receipt.payload.outputs.get("out/a.rs").unwrap_or_else(|| {
        panic!(
            "receipt must record out/a.rs, got: {:?}",
            receipt.payload.outputs
        )
    });
    assert_eq!(
        recorded_hash, &expected_hash,
        "receipt's recorded hash for out/a.rs must match its real on-disk bytes \
         (this is the exact drift the finding describes: a receipt that does not \
         accurately describe what actually happened on disk)"
    );

    // out/b.rs never actually landed (the directory sabotage blocked it) --
    // it must not be falsely recorded as a bound output either.
    assert!(
        !receipt.payload.outputs.contains_key("out/b.rs"),
        "out/b.rs was never successfully written and must not appear in the \
         receipt's outputs: {:?}",
        receipt.payload.outputs
    );

    // `receipt verify` must accept this receipt: it is internally
    // consistent (payload hash / chain hash), even though it was written on
    // an error path.
    let verify = ggen_engine::verbs::handlers::handle_receipt_verify_in(dir.path())
        .expect("a partially-written receipt must still self-verify");
    assert_eq!(verify["valid"], serde_json::json!(true));
}
