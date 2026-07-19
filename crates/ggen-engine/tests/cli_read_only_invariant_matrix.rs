//! Combinatorial-maximalism suite (companion to `combinatorial_matrix.rs`,
//! same doctrine): cross every read-only CLI command against every
//! representative project state and assert the ONE invariant that matters
//! for all of them — the filesystem is byte-identical, mtime-identical,
//! and file-set-identical before and after the command runs, REGARDLESS of
//! whether the command exits 0 or nonzero.
//!
//! `graph validate`, `receipt verify`, `receipt history`, and `doctor run`
//! are all documented as read-only/non-invasive (`handlers.rs`'s own doc
//! comments; `doctor`'s explicitly says "non-invasive health check"). Every
//! existing test that calls these commands asserts only on exit
//! code/stdout/stderr — none re-snapshots the filesystem afterward to prove
//! the read-only claim itself. This suite closes that gap directly, via
//! `chicago_tdd_tools::cli_proof::CliHarness` (real subprocess, no mocks),
//! crossed over 3 representative states × 4 commands = 12 cases.
//!
//! `sync run`'s write effects (the one command that DOES write) are already
//! proven correct/atomic/capped elsewhere this session
//! (`write_behaviors_cli_e2e.rs`, `pack_behaviors_cli_e2e.rs`,
//! `sync_e2e.rs`) — not repeated here.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

/// (relative path, mtime, content hash) for every file under `root`,
/// recursively — a full disk fingerprint, not just a file-count.
fn snapshot(root: &Path) -> BTreeMap<PathBuf, (SystemTime, String)> {
    let mut out = BTreeMap::new();
    fn walk(dir: &Path, root: &Path, out: &mut BTreeMap<PathBuf, (SystemTime, String)>) {
        for entry in std::fs::read_dir(dir).expect("read_dir") {
            let entry = entry.expect("dir entry");
            let path = entry.path();
            if path.is_dir() {
                walk(&path, root, out);
            } else {
                let meta = std::fs::metadata(&path).expect("metadata");
                let mtime = meta.modified().expect("mtime");
                let bytes = std::fs::read(&path).expect("read file");
                let hash = blake3::hash(&bytes).to_hex().to_string();
                let rel = path.strip_prefix(root).unwrap_or(&path).to_path_buf();
                out.insert(rel, (mtime, hash));
            }
        }
    }
    walk(root, root, &mut out);
    out
}

fn assert_disk_unchanged(
    before: &BTreeMap<PathBuf, (SystemTime, String)>,
    after: &BTreeMap<PathBuf, (SystemTime, String)>, command: &str, state: &str,
) {
    assert_eq!(
        before.keys().collect::<Vec<_>>(),
        after.keys().collect::<Vec<_>>(),
        "`{command}` in state `{state}` changed the file set on disk"
    );
    for (path, before_val) in before {
        let after_val = after.get(path).unwrap_or_else(|| {
            panic!(
                "`{command}` in state `{state}` removed `{}`",
                path.display()
            )
        });
        assert_eq!(
            before_val,
            after_val,
            "`{command}` in state `{state}` touched `{}` (mtime or content changed) — \
             a read-only command must leave every file byte- and mtime-identical",
            path.display()
        );
    }
}

/// State 1: a project with `ggen.toml`/`ontology.ttl`/`templates/` present
/// but never synced — no outputs, no lock, no receipt exist yet.
fn state_never_synced() -> TempDir {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(dir.path().join("templates/one.tmpl"), TEMPLATE).expect("write template");
    dir
}

/// State 2: a project that has been synced once — outputs, `ggen.lock`
/// (none declared here, so absent is fine), receipt, and receipt log all
/// exist and are consistent.
fn state_freshly_synced() -> TempDir {
    let dir = state_never_synced();
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(dir.path())
        .run()
        .expect("seed sync")
        .assert_success();
    dir
}

/// State 3: an empty directory — no `ggen.toml` at all, the most degenerate
/// input every command must still fail closed (or find nothing) on without
/// creating anything.
fn state_no_project() -> TempDir {
    TempDir::new().expect("tempdir")
}

/// Run `args` via the real `ggen` binary in `root`, snapshotting the
/// filesystem immediately before and after — regardless of the command's
/// own exit code, the two snapshots must be identical.
fn assert_command_is_read_only(root: &Path, args: &[&str], state: &str) {
    let before = snapshot(root);
    let _ = CliHarness::cargo_bin("ggen")
        .args(args)
        .current_dir(root)
        .run()
        .expect("spawn ggen");
    let after = snapshot(root);
    assert_disk_unchanged(&before, &after, &args.join(" "), state);
}

macro_rules! read_only_matrix_test {
    ($test_name:ident, $state_fn:ident, $state_label:literal, $args:expr) => {
        #[test]
        fn $test_name() {
            let dir = $state_fn();
            assert_command_is_read_only(dir.path(), $args, $state_label);
        }
    };
}

// ── graph validate × 3 states ───────────────────────────────────────────
read_only_matrix_test!(
    graph_validate_is_read_only_never_synced,
    state_never_synced,
    "never-synced",
    &["graph", "validate"]
);
read_only_matrix_test!(
    graph_validate_is_read_only_freshly_synced,
    state_freshly_synced,
    "freshly-synced",
    &["graph", "validate"]
);
read_only_matrix_test!(
    graph_validate_is_read_only_no_project,
    state_no_project,
    "no-project",
    &["graph", "validate"]
);

// ── receipt verify × 3 states ────────────────────────────────────────────
read_only_matrix_test!(
    receipt_verify_is_read_only_never_synced,
    state_never_synced,
    "never-synced",
    &["receipt", "verify"]
);
read_only_matrix_test!(
    receipt_verify_is_read_only_freshly_synced,
    state_freshly_synced,
    "freshly-synced",
    &["receipt", "verify"]
);
read_only_matrix_test!(
    receipt_verify_is_read_only_no_project,
    state_no_project,
    "no-project",
    &["receipt", "verify"]
);

// ── receipt history × 3 states ───────────────────────────────────────────
read_only_matrix_test!(
    receipt_history_is_read_only_never_synced,
    state_never_synced,
    "never-synced",
    &["receipt", "history"]
);
read_only_matrix_test!(
    receipt_history_is_read_only_freshly_synced,
    state_freshly_synced,
    "freshly-synced",
    &["receipt", "history"]
);
read_only_matrix_test!(
    receipt_history_is_read_only_no_project,
    state_no_project,
    "no-project",
    &["receipt", "history"]
);

// ── doctor run × 3 states ────────────────────────────────────────────────
read_only_matrix_test!(
    doctor_run_is_read_only_never_synced,
    state_never_synced,
    "never-synced",
    &["doctor", "run"]
);
read_only_matrix_test!(
    doctor_run_is_read_only_freshly_synced,
    state_freshly_synced,
    "freshly-synced",
    &["doctor", "run"]
);
read_only_matrix_test!(
    doctor_run_is_read_only_no_project,
    state_no_project,
    "no-project",
    &["doctor", "run"]
);

// ── one extra, sharpest case: a tampered receipt is still read-only to
//    inspect, even though inspecting it is exactly what proves it's broken.
#[test]
fn receipt_verify_is_read_only_even_when_it_finds_tampering() {
    let dir = state_freshly_synced();
    let receipt_path = dir.path().join(".ggen-v2/receipt.json");
    let tampered = std::fs::read_to_string(&receipt_path)
        .expect("read receipt")
        .replace("\"graph_hash\"", "\"graph_hash_tampered_key\"");
    std::fs::write(&receipt_path, tampered).expect("write tampered");

    assert_command_is_read_only(dir.path(), &["receipt", "verify"], "tampered-receipt");
}
