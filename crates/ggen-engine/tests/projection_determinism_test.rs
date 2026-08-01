//! G5 evidence: Tera projection behavior, `mode = "Create"`-equivalent
//! skip-existing-file semantics (`unless_exists`, per root `CLAUDE.md`'s
//! "mode=Create Semantics"), and byte-identical output-tree replay for the
//! frontmatter (`ggen_engine::config::GgenConfig`) schema. Chicago TDD: real
//! filesystem, real oxigraph/GraphLaw engine, real Tera rendering, two
//! independent real syncs compared byte-for-byte.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "projection-determinism"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:one   ex:seq "1" .
ex:two   ex:seq "2" .
ex:three ex:seq "3" .
"#;

const TEMPLATE: &str = "---\nto: out/seq.txt\nsparql:\n  rows: SELECT ?seq WHERE { ?s <http://example.org/seq> ?seq } ORDER BY ?seq\n---\n{% for row in results %}{{ row.seq }}\n{% endfor %}";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates").join("seq.tmpl"), TEMPLATE).expect("write template");
}

/// Positive witness: two independent, real, non-dry-run syncs of the exact
/// same fixture produce byte-identical output-tree content and an identical
/// receipt payload graph hash -- an exact output-tree replay.
#[test]
fn identical_fixtures_replay_to_byte_identical_output_trees() {
    let d1 = TempDir::new().expect("tempdir");
    let d2 = TempDir::new().expect("tempdir");
    scaffold(d1.path());
    scaffold(d2.path());

    let r1 = sync(
        d1.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync 1");
    let r2 = sync(
        d2.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync 2");

    assert_eq!(r1.graph_hash_hex, r2.graph_hash_hex);

    let body1 = std::fs::read(d1.path().join("out/seq.txt")).expect("read output 1");
    let body2 = std::fs::read(d2.path().join("out/seq.txt")).expect("read output 2");
    assert_eq!(
        body1, body2,
        "identical fixtures must produce byte-identical rendered output"
    );
    assert_eq!(body1, b"1\n2\n3\n".to_vec());

    // Re-run each sync a second time against its own project: the second
    // run must be fully unchanged (idempotent replay) -- no output listed
    // as freshly "written" a second time in a way that alters content.
    let r1_again = sync(
        d1.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync 1 again");
    let body1_again = std::fs::read(d1.path().join("out/seq.txt")).expect("read output 1 again");
    assert_eq!(body1_again, body1, "re-sync must not alter unchanged output");
    assert_eq!(
        r1_again.graph_hash_hex, r1.graph_hash_hex,
        "re-sync of an unchanged project must reproduce the same graph hash"
    );
}

/// Positive + negative witness: `unless_exists: true` in template
/// frontmatter implements `CLAUDE.md`'s "mode=Create" bootstrap-scaffold
/// semantics for the frontmatter schema -- a pre-existing target is left
/// completely untouched (hand edits survive), and the report records the
/// skip with its real reason.
#[test]
fn unless_exists_frontmatter_preserves_hand_edited_scaffold_file() {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    let scaffold_template = "---\nto: src/scaffold.rs\nunless_exists: true\n---\n// generated scaffold, should never be seen\n";
    std::fs::write(
        dir.path().join("templates").join("scaffold.tmpl"),
        scaffold_template,
    )
    .expect("write template");

    // Simulate a bootstrap scaffold that was already hand-completed by a
    // developer after a prior first-run generation.
    std::fs::create_dir_all(dir.path().join("src")).expect("mkdir src");
    let hand_written = "// hand-written implementation, do not overwrite\nfn real_impl() {}\n";
    std::fs::write(dir.path().join("src/scaffold.rs"), hand_written).expect("seed hand edit");

    let report = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed even though the scaffold target pre-exists");

    let on_disk = std::fs::read_to_string(dir.path().join("src/scaffold.rs")).expect("read scaffold");
    assert_eq!(
        on_disk, hand_written,
        "mode=Create (unless_exists) must never overwrite an existing target"
    );
    assert!(
        report.written.is_empty(),
        "the pre-existing scaffold must not appear in `written`: {:?}",
        report.written
    );
    let reason = report
        .decisions
        .get("src/scaffold.rs")
        .expect("decision recorded for the scaffold target");
    assert!(
        reason.contains("skip") || reason.contains("unless_exists"),
        "decision must name the real skip reason, got: {reason}"
    );
}

/// Negative falsifier for the same mechanism: with no pre-existing target,
/// `unless_exists: true` still writes the file on first run (it only skips
/// when the target already exists -- proving the test above is not
/// vacuously "nothing ever writes").
#[test]
fn unless_exists_frontmatter_writes_when_target_absent() {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    let scaffold_template =
        "---\nto: src/scaffold.rs\nunless_exists: true\n---\n// first-run scaffold\n";
    std::fs::write(
        dir.path().join("templates").join("scaffold.tmpl"),
        scaffold_template,
    )
    .expect("write template");

    let report = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed");

    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("src/scaffold.rs")],
        "with no pre-existing target, unless_exists must still write on first run"
    );
    assert!(dir.path().join("src/scaffold.rs").exists());
}
