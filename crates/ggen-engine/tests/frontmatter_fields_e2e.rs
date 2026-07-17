//! Chicago-TDD end-to-end tests for the frontmatter fields added under the
//! PROJ-302 extension to `crates/ggen`: `sh_before`/`sh_after`, `backup`,
//! `shape`, `determinism`, `freeze_policy`/`freeze_slots_dir`, `from`.
//! Real filesystem, real sync pipeline — no mocks.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
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

#[test]
fn sh_before_and_sh_after_run_around_the_write() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "hook.tmpl",
        "---\nto: out.txt\nsh_before: \"echo before >> hooks.log\"\nsh_after: \"echo after >> hooks.log\"\n---\nbody\n",
    );

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    let log = std::fs::read_to_string(dir.path().join("hooks.log")).expect("hooks ran");
    assert_eq!(log, "before\nafter\n");
}

#[test]
fn dangerous_sh_command_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "evil.tmpl",
        "---\nto: out.txt\nsh_before: \"rm -rf /\"\n---\nbody\n",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("must refuse");
    assert!(err.to_string().contains("FM-SHELL-001"), "{err}");
}

#[test]
fn dry_run_never_executes_shell_hooks() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "hook.tmpl",
        "---\nto: out.txt\nsh_before: \"echo ran >> hooks.log\"\n---\nbody\n",
    );

    sync(
        dir.path(),
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .expect("dry run");
    assert!(
        !dir.path().join("hooks.log").exists(),
        "dry run must not run sh_before"
    );
}

#[test]
fn backup_copies_existing_file_before_force_overwrite() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(dir.path().join("out.txt"), "old content\n").expect("seed");
    write_template(
        dir.path(),
        "b.tmpl",
        "---\nto: out.txt\nforce: true\nbackup: true\n---\nnew content\n",
    );

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");

    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("new"),
        "new content\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt.bak")).expect("backup"),
        "old content\n"
    );
}

#[test]
fn missing_shape_file_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "s.tmpl",
        "---\nto: out.txt\nshape:\n  - shapes/missing.ttl\n---\nbody\n",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("must refuse");
    assert!(err.to_string().contains("FM-TPL-008"), "{err}");
}

#[test]
fn existing_shape_file_passes() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::create_dir_all(dir.path().join("shapes")).expect("mkdir shapes");
    std::fs::write(dir.path().join("shapes/present.ttl"), "# shape\n").expect("write shape");
    write_template(
        dir.path(),
        "s.tmpl",
        "---\nto: out.txt\nshape:\n  - shapes/present.ttl\n---\nbody\n",
    );

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed");
}

#[test]
fn determinism_true_passes_for_a_pure_template() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "d.tmpl",
        "---\nto: out.txt\ndeterminism: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name }\n---\n{% for row in results %}{{ row.name }}{% endfor %}",
    );

    let report = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed");
    assert_eq!(report.written, vec![std::path::PathBuf::from("out.txt")]);
}

#[test]
fn freeze_always_skips_once_target_exists() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(dir.path().join("out.txt"), "hand-written, never touch\n").expect("seed");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nforce: true\nfreeze_policy: always\n---\ngenerated content\n",
    );

    let report = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");
    assert!(
        report.written.is_empty(),
        "freeze:always must skip, got: {:?}",
        report.written
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("unchanged"),
        "hand-written, never touch\n"
    );
}

#[test]
fn freeze_checksum_allows_regen_until_manual_edit_then_protects_it() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    // Deliberately no `force: true`: this test proves freeze intercepts
    // *before* the normal decision table would otherwise refuse a
    // differing-content overwrite with FM-WRITE-005.
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nfreeze_policy: checksum\nfreeze_slots_dir: .ggen-freeze\n---\ngenerated v1\n",
    );

    // First sync: target absent, writes normally and records a checksum.
    let first = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");
    assert_eq!(first.written, vec![std::path::PathBuf::from("out.txt")]);
    assert!(
        dir.path().join(".ggen-freeze/out.txt.blake3").exists(),
        "checksum recorded"
    );

    // Second sync: on-disk content still matches the recorded checksum (no
    // human edit) and equals the rendered body, so it's a normal
    // Skipped(unchanged) — not a freeze skip.
    let second = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync");
    assert!(second.written.is_empty());
    assert!(
        second.skipped[0].1.contains("unchanged"),
        "{:?}",
        second.skipped
    );

    // Simulate a human hand-editing the generated file.
    std::fs::write(dir.path().join("out.txt"), "hand-edited by a human\n").expect("hand edit");

    let third = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("third sync");
    assert!(
        third.written.is_empty(),
        "checksum mismatch must freeze the file, got: {:?}",
        third.written
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("protected"),
        "hand-edited by a human\n",
        "the human edit must survive"
    );
}

#[test]
fn freeze_checksum_without_slots_dir_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(dir.path().join("out.txt"), "existing\n").expect("seed");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nforce: true\nfreeze_policy: checksum\n---\ngenerated\n",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("must refuse");
    assert!(err.to_string().contains("FM-WRITE-006"), "{err}");
}

#[test]
fn from_field_loads_body_from_referenced_sibling_file() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("templates/shared_body.tera"),
        "shared body content\n",
    )
    .expect("write shared body");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nfrom: shared_body.tera\n---\nignored inline body\n",
    );

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).expect("output"),
        "shared body content\n"
    );
}

#[test]
fn from_field_path_traversal_outside_template_dir_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    // A secret file OUTSIDE the templates/ directory the `from:` escape
    // attempts to read.
    std::fs::write(dir.path().join("secret.txt"), "top secret\n").expect("write secret");
    write_template(
        dir.path(),
        "f.tmpl",
        "---\nto: out.txt\nfrom: ../secret.txt\n---\nignored inline body\n",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("traversal outside the template's own directory must be refused");
    let msg = err.to_string();
    assert!(
        msg.contains("from:"),
        "error should name the from: field: {msg}"
    );
    assert!(
        !dir.path().join("out.txt").exists(),
        "no output must be written on refusal"
    );
}
