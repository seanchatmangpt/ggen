//! Real subprocess proof for `just sync-dry`'s fix (specs/v26.8.1 disposition
//! repair): the `justfile`'s `sync-dry:` recipe used to run `ggen sync
//! --dry_run true`, which fails clap arg parsing before any generation runs
//! (`--dry-run` is a bare switch on the live `sync run` verb, not a
//! value-taking flag). The corrected recipe runs `ggen sync run --dry-run`.
//!
//! This test proves the corrected invocation's actual behavior at the real
//! CLI process boundary (`assert_cmd::Command::cargo_bin("ggen")`, same
//! pattern as `receipt_chain_e2e.rs`): a real `ggen sync run --dry-run`
//! process must exit 0 and must NOT mutate any file on disk -- neither the
//! would-be-generated output file, nor the receipt/receipt-log, nor create
//! any new file at all. Byte-for-byte content and mtime are both compared
//! before and after, not just presence/absence.

use std::{
    path::Path,
    time::{Duration, SystemTime},
};

use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "dry-run-demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str =
    "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n";

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

/// Snapshot of every regular file under `root`, mapping relative path to
/// (content bytes, mtime). Walks the whole tree so a dry-run that creates an
/// unexpected new file anywhere is caught, not just a change to a file we
/// already knew about.
fn snapshot(root: &Path) -> std::collections::BTreeMap<String, (Vec<u8>, SystemTime)> {
    fn walk(dir: &Path, root: &Path, out: &mut std::collections::BTreeMap<String, (Vec<u8>, SystemTime)>) {
        for entry in std::fs::read_dir(dir).expect("read_dir") {
            let entry = entry.expect("dir entry");
            let path = entry.path();
            if path.is_dir() {
                walk(&path, root, out);
            } else {
                let rel = path
                    .strip_prefix(root)
                    .expect("strip_prefix")
                    .to_string_lossy()
                    .to_string();
                let bytes = std::fs::read(&path).expect("read file");
                let mtime = entry.metadata().expect("metadata").modified().expect("mtime");
                out.insert(rel, (bytes, mtime));
            }
        }
    }
    let mut out = std::collections::BTreeMap::new();
    walk(root, root, &mut out);
    out
}

/// `ggen sync run --dry-run` (the corrected `sync-dry:` recipe's real
/// invocation) exits 0 and leaves every file on disk byte-for-byte and
/// mtime-for-mtime unchanged -- no generated output written, no receipt
/// written, no new file of any kind.
#[test]
fn dry_run_subprocess_does_not_mutate_any_file() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let before = snapshot(dir.path());
    assert!(
        !dir.path().join("out/names.txt").exists(),
        "precondition: output must not exist yet"
    );

    // mtimes have coarse (sometimes 1s) resolution on some filesystems; make
    // sure any real write would be observable as a content OR mtime diff.
    std::thread::sleep(Duration::from_millis(1100));

    let assert = assert_cmd::Command::cargo_bin("ggen")
        .expect("ggen binary")
        .current_dir(dir.path())
        .args(["sync", "run", "--dry-run"])
        .assert()
        .success();
    let stdout = String::from_utf8_lossy(&assert.get_output().stdout).to_string();
    let report: serde_json::Value = serde_json::from_str(&stdout).expect("dry-run stdout is JSON");
    // `written` is the *planned* write list even under --dry-run (real
    // evidence, captured above: `{"written":["out/names.txt"],...,
    // "decisions":{"out/names.txt":"planned: write (dry-run)"}}`) -- the
    // report is a preview, not a claim that the write happened. The actual
    // no-mutation guarantee this test proves is the on-disk snapshot
    // comparison below, not this field. Assert on `decisions` instead: every
    // entry must be explicitly marked planned/dry-run, never an actual
    // write.
    let decisions = report["decisions"]
        .as_object()
        .expect("decisions is an object");
    assert!(!decisions.is_empty(), "expected at least one decision: {report}");
    for (path, decision) in decisions {
        let text = decision.as_str().unwrap_or_default();
        assert!(
            text.contains("dry-run") || text.starts_with("skipped:"),
            "decision for {path} must be a dry-run preview or a no-op skip, got {text:?}: {report}"
        );
    }

    let after = snapshot(dir.path());

    assert!(
        !dir.path().join("out/names.txt").exists(),
        "dry-run must not write the generated output file"
    );
    assert!(
        !dir.path().join(".ggen-v2").exists(),
        "dry-run must not write a receipt"
    );
    assert_eq!(
        before, after,
        "dry-run must not mutate, create, or touch any pre-existing file's content or mtime"
    );
}
