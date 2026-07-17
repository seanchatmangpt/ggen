//! Chicago-TDD end-to-end proofs of every write.rs decision-table branch,
//! driven through the real `ggen` binary as a subprocess
//! (`chicago_tdd_tools::cli_proof::CliHarness`) — no library-level `sync()`
//! calls, no mocks. Mirrors write.rs's own module-doc decision order:
//!
//! 1. path escapes root / traversal → `Err`
//! 2. `unless_exists` && target exists → `Skipped`
//! 3. `skip_if` substring present in existing file → `Skipped`
//! 4. `freeze_policy: always` && target exists → `Skipped`
//! 5. `freeze_policy: checksum` && on-disk content diverged → `Skipped`
//! 6. `inject` (missing target / missing marker → `Err`; `backup` first)
//! 7. `force` (overwrite; `backup` first) → `Written`
//! 8. default: absent → `Written`; identical → `Skipped`; differs → `Err`
//!
//! Plus the two non-table hardening behaviors added this session: the
//! `MAX_OUTPUT_BYTES` size cap and the atomic render-then-write pass.

use std::path::Path;

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

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
}

fn write_template(root: &Path, name: &str, content: &str) {
    std::fs::write(root.join("templates").join(name), content).expect("write template");
}

fn run_sync(root: &Path) -> chicago_tdd_tools::cli_proof::CliOutput {
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(root)
        .run()
        .expect("spawn ggen sync run")
}

// ── 1. traversal ─────────────────────────────────────────────────────────

#[test]
fn to_path_escaping_the_project_root_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(dir.path(), "f.tmpl", "---\nto: ../escape.txt\n---\nnope\n");

    run_sync(dir.path())
        .assert_failure()
        .assert_stderr_contains("FM-WRITE-002");
    assert!(!dir
        .path()
        .parent()
        .expect("parent")
        .join("escape.txt")
        .exists());
}

// ── 2. unless_exists ─────────────────────────────────────────────────────

#[test]
fn unless_exists_skips_a_file_that_already_exists() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(dir.path().join("out.txt"), "human content\n").expect("pre-existing");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nunless_exists: true\n---\ngenerated content\n",
    );

    run_sync(dir.path()).assert_success();
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("out.txt"),
        "human content\n",
        "unless_exists must never touch a pre-existing file"
    );
}

// ── 3. skip_if ───────────────────────────────────────────────────────────

#[test]
fn skip_if_skips_when_the_marker_is_already_present() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("out.txt"),
        "// DO-NOT-REGENERATE\nhand code\n",
    )
    .expect("pre-existing");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nskip_if: \"DO-NOT-REGENERATE\"\n---\ngenerated content\n",
    );

    run_sync(dir.path()).assert_success();
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("out.txt"),
        "// DO-NOT-REGENERATE\nhand code\n"
    );
}

// ── 4. freeze_policy: always ─────────────────────────────────────────────

#[test]
fn freeze_always_skips_once_the_target_exists() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(dir.path().join("out.txt"), "scaffolded once\n").expect("pre-existing");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nfreeze_policy: always\n---\nnew scaffold\n",
    );

    run_sync(dir.path()).assert_success();
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("out.txt"),
        "scaffolded once\n"
    );
}

// ── 5. freeze_policy: checksum ───────────────────────────────────────────

#[test]
fn freeze_checksum_regenerates_until_a_human_edits_it_then_protects_it() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nfreeze_policy: checksum\nfreeze_slots_dir: .slots\nforce: true\n---\ngenerated v1\n",
    );

    // First sync: writes and records the checksum.
    run_sync(dir.path()).assert_success();
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("v1"),
        "generated v1\n"
    );

    // A human edits the file directly.
    std::fs::write(dir.path().join("out.txt"), "hand-edited\n").expect("human edit");

    // Second sync must protect the human edit, not overwrite it.
    run_sync(dir.path()).assert_success();
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("still hand-edited"),
        "hand-edited\n",
        "freeze_policy: checksum must detect the human edit and skip"
    );
}

// ── 6. inject ────────────────────────────────────────────────────────────

#[test]
fn inject_before_marker_inserts_content_and_backs_up_first() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("out.txt"),
        "line one\n// MARKER\nline two\n",
    )
    .expect("pre-existing");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\ninject: true\nbefore: \"MARKER\"\nbackup: true\n---\ninjected line\n",
    );

    run_sync(dir.path()).assert_success();
    let after = std::fs::read_to_string(dir.path().join("out.txt")).expect("out.txt");
    assert_eq!(after, "line one\ninjected line\n// MARKER\nline two\n");
    let backup = std::fs::read_to_string(dir.path().join("out.txt.bak")).expect("backup");
    assert_eq!(
        backup, "line one\n// MARKER\nline two\n",
        "backup must hold the pre-inject content"
    );
}

#[test]
fn inject_into_a_missing_target_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\ninject: true\n---\ninjected\n",
    );

    run_sync(dir.path())
        .assert_failure()
        .assert_stderr_contains("FM-WRITE-003");
    assert!(!dir.path().join("out.txt").exists());
}

#[test]
fn inject_with_a_missing_marker_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(dir.path().join("out.txt"), "no markers here\n").expect("pre-existing");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\ninject: true\nbefore: \"NOPE\"\n---\ninjected\n",
    );

    run_sync(dir.path())
        .assert_failure()
        .assert_stderr_contains("FM-WRITE-004");
}

// ── 7. force ─────────────────────────────────────────────────────────────

#[test]
fn force_overwrites_differing_content_and_backs_up_first() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(dir.path().join("out.txt"), "old content\n").expect("pre-existing");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nforce: true\nbackup: true\n---\nnew content\n",
    );

    run_sync(dir.path()).assert_success();
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("out.txt"),
        "new content\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt.bak")).expect("backup"),
        "old content\n"
    );
}

// ── 8. default (absent → Written; identical → Skipped; differs → Err) ──

#[test]
fn default_semantics_write_then_skip_then_refuse_on_drift() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\n---\nstable content\n",
    );

    // Absent → Written.
    run_sync(dir.path()).assert_success();
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("first write"),
        "stable content\n"
    );

    // Identical → Skipped (still succeeds, content untouched).
    run_sync(dir.path()).assert_success();
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("unchanged"),
        "stable content\n"
    );

    // Someone edits the file by hand; the template's rendered output now
    // differs from what's on disk → refuse the silent clobber.
    std::fs::write(dir.path().join("out.txt"), "manually diverged\n").expect("manual edit");
    run_sync(dir.path())
        .assert_failure()
        .assert_stderr_contains("FM-WRITE-005");
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("still diverged"),
        "manually diverged\n",
        "a refused write must never clobber the differing on-disk content"
    );
}

// ── size cap ─────────────────────────────────────────────────────────────

#[test]
fn oversized_rendered_output_is_refused_over_the_cli_boundary() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "huge.tmpl",
        "---\nto: out.txt\n---\n{% for i in range(end=200000) %}0123456789012345678901234567890123456789012345678901234567890123\n{% endfor %}",
    );

    run_sync(dir.path())
        .assert_failure()
        .assert_stderr_contains("byte cap");
    assert!(!dir.path().join("out.txt").exists());
}

// ── atomic all-or-nothing across templates ──────────────────────────────

#[test]
fn a_render_failure_in_one_template_leaves_no_writes_from_others_over_the_cli_boundary() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(dir.path(), "a_good.tmpl", "---\nto: good.txt\n---\nfine\n");
    write_template(
        dir.path(),
        "b_bad.tmpl",
        "---\nto: bad.txt\n---\n{{ row.nuon }}\n",
    );

    run_sync(dir.path())
        .assert_failure()
        .assert_stderr_contains("render failed");
    assert!(
        !dir.path().join("good.txt").exists(),
        "atomic sync must not write a_good's output"
    );
    assert!(!dir.path().join("bad.txt").exists());
}
