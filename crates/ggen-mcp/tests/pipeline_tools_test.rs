//! Chicago TDD for the pipeline-touching tools. Real projects, real sync,
//! real files on disk -- including a real filesystem snapshot proving the
//! dry-run tool writes nothing.

mod common;

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use common::write_frontmatter_project;

use ggen_mcp::error::ErrorCategory;
use ggen_mcp::tools::{
    check_project::{check_project, CheckProjectParams},
    sync_dry_run::{sync_dry_run, SyncDryRunParams},
    write_apply::{write_apply, WriteApplyParams},
};

/// Full recursive (path -> content-hash) fingerprint of a directory tree.
/// Modeled on `ggen-engine`'s own `cli_read_only_invariant_matrix.rs`.
fn snapshot(root: &Path) -> BTreeMap<PathBuf, String> {
    let mut out = BTreeMap::new();
    collect(root, root, &mut out);
    out
}

fn collect(base: &Path, dir: &Path, out: &mut BTreeMap<PathBuf, String>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect(base, &path, out);
        } else if let Ok(bytes) = std::fs::read(&path) {
            let rel = path.strip_prefix(base).unwrap_or(&path).to_path_buf();
            out.insert(rel, blake3::hash(&bytes).to_hex().to_string());
        }
    }
}

// ---------------------------------------------------------------------------
// ggen_sync_dry_run
// ---------------------------------------------------------------------------

/// The whole contract of a dry run: it must plan a write AND leave the
/// filesystem byte-identical.
#[test]
fn dry_run_plans_a_write_without_touching_disk() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let before = snapshot(dir.path());
    let got = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("dry run");
    let after = snapshot(dir.path());

    assert_eq!(
        before, after,
        "dry run must not create, modify, or delete any file"
    );
    assert!(
        got.write_count > 0,
        "the fixture has a renderable template, so something should be planned"
    );
    assert!(
        !got.graph_hash.is_empty(),
        "graph hash proves which graph was used"
    );
}

/// Skip reasons must be typed, and an unrecognized reason must fall to
/// `other` WITH the raw string preserved -- never force-fitted into a
/// wrong bucket.
#[test]
fn dry_run_skip_reasons_are_typed_and_preserve_raw_text() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    // First apply for real, so a second dry run sees "unchanged" skips.
    let pre = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("pre-apply dry run");
    write_apply(&WriteApplyParams::new(
        dir.path().display().to_string(),
        true,
        pre.graph_hash,
    ))
    .expect("first apply");

    let got = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("dry run after apply");

    for skip in &got.would_skip {
        assert!(
            !skip.raw_reason.is_empty(),
            "raw reason is always preserved"
        );
        assert!(
            [
                "when_false",
                "zero_rows",
                "unchanged",
                "exists_no_overwrite",
                "skip_empty",
                "other"
            ]
            .contains(&skip.reason.as_str()),
            "reason must be from the closed typed set, got {:?}",
            skip.reason
        );
    }
}

/// The real, motivating case: a `for_each:` driving query that returns
/// zero rows must classify as `zero_rows`, not fall through to `other`.
/// `classify()` used to match only "zero row"/"no rows"/"empty result",
/// none of which appear in the engine's actual wording
/// (`for_each `{driver}` produced 0 rows (...)`) -- so this exact scenario,
/// the tool's own stated reason to exist, silently misclassified.
#[test]
fn dry_run_classifies_a_real_for_each_zero_row_skip_as_zero_rows() {
    let dir = tempfile::tempdir().expect("tempdir");
    let toml = r#"
[project]
name = "for-each-demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;
    let ontology = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:hasName "alice" .
"#;
    // The driving query filters everything out -- a real zero-row for_each.
    let template = "---\nto: registry.txt\nsparql:\n  entities: |\n    PREFIX ex: <http://example.org/>\n    SELECT ?name WHERE { ?s ex:hasName ?name . FILTER(?name = \"nobody\") }\nfor_each: entities\n---\n{{ row.name }}\n";

    std::fs::write(dir.path().join("ggen.toml"), toml).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), ontology).expect("write ontology.ttl");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(dir.path().join("templates/registry.tmpl"), template).expect("write template");

    let got = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("dry run");

    let skip = got
        .would_skip
        .iter()
        .find(|s| s.path == "registry.txt")
        .unwrap_or_else(|| panic!("expected a skip for registry.txt, got {:?}", got.would_skip));
    assert_eq!(
        skip.reason, "zero_rows",
        "a for_each zero-row skip must classify as zero_rows, not {:?} (raw: {:?})",
        skip.reason, skip.raw_reason
    );
    assert!(
        skip.raw_reason.contains("produced 0 rows"),
        "raw reason must be the real engine wording: {:?}",
        skip.raw_reason
    );
}

// ---------------------------------------------------------------------------
// ggen_write_apply
// ---------------------------------------------------------------------------

/// Without `confirm: true` the tool must refuse BEFORE doing any pipeline
/// work -- proven by the filesystem being untouched.
#[test]
fn write_apply_without_confirm_refuses_and_writes_nothing() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let before = snapshot(dir.path());
    let err = write_apply(&WriteApplyParams::new(
        dir.path().display().to_string(),
        false,
        String::new(),
    ))
    .expect_err("must refuse without confirm");
    let after = snapshot(dir.path());

    assert_eq!(err.category, ErrorCategory::Unsupported);
    assert_eq!(
        before, after,
        "a refused apply must not touch the filesystem"
    );
}

/// With confirmation, files actually land, and the reported BLAKE3 must
/// match the bytes really on disk -- evidence, not a claim.
#[test]
fn write_apply_writes_files_and_reports_verifiable_hashes() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let pre = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("pre-apply dry run");
    let got = write_apply(&WriteApplyParams::new(
        dir.path().display().to_string(),
        true,
        pre.graph_hash,
    ))
    .expect("apply");

    assert!(
        got.write_count > 0,
        "the fixture template must produce output"
    );
    for file in &got.written {
        let abs = dir.path().join(&file.path);
        assert!(abs.exists(), "{} must exist after apply", file.path);
        let actual = blake3::hash(&std::fs::read(&abs).expect("read back"))
            .to_hex()
            .to_string();
        assert_eq!(
            file.blake3.as_deref(),
            Some(actual.as_str()),
            "reported hash must match the bytes actually on disk for {}",
            file.path
        );
    }
}

/// A real sync writes a signed receipt; the tool reports where it is, and
/// it must actually be there.
#[test]
fn write_apply_produces_the_receipt_it_reports() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let pre = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("pre-apply dry run");
    let got = write_apply(&WriteApplyParams::new(
        dir.path().display().to_string(),
        true,
        pre.graph_hash,
    ))
    .expect("apply");

    let receipt = dir.path().join(&got.receipt_path);
    assert!(
        receipt.exists(),
        "the reported receipt path {} must exist -- a success claim with no receipt \
         would be exactly the decorative-completion failure this project forbids",
        got.receipt_path
    );
}

/// CP17: `confirm: true` with a fabricated/stale `expected_graph_hash` (the
/// exact bypass shape a careless in-process caller could construct without
/// ever running a real dry-run) must refuse and write nothing -- proven by
/// the filesystem being untouched, not just by an error being returned.
#[test]
fn write_apply_refuses_a_fabricated_graph_hash_bypass() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let before = snapshot(dir.path());
    let err = write_apply(&WriteApplyParams::new(
        dir.path().display().to_string(),
        true,
        "not-a-real-hash-from-any-dry-run".to_string(),
    ))
    .expect_err("a fabricated graph_hash must be refused, not silently accepted");
    let after = snapshot(dir.path());

    assert_eq!(err.category, ErrorCategory::Unsupported);
    assert_eq!(
        before, after,
        "a refused apply (fabricated hash) must not touch the filesystem"
    );
}

/// CP17's legitimate path: a real `ggen_sync_dry_run` call's own real
/// `graph_hash`, passed straight through, must be accepted.
#[test]
fn write_apply_accepts_a_real_dry_run_graph_hash() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let real_dry_run = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("real dry run");

    let got = write_apply(&WriteApplyParams::new(
        dir.path().display().to_string(),
        true,
        real_dry_run.graph_hash,
    ))
    .expect("a real dry-run's own graph_hash must be accepted by write_apply");
    assert!(got.ok);
}

/// Applying twice must be idempotent: the second run writes nothing new
/// and the tree is unchanged.
#[test]
fn write_apply_is_idempotent() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let pre = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("pre-apply dry run");
    let params = WriteApplyParams::new(dir.path().display().to_string(), true, pre.graph_hash);

    write_apply(&params).expect("first apply");
    let after_first = snapshot(dir.path());
    let second = write_apply(&params).expect("second apply");

    assert_eq!(
        second.write_count, 0,
        "nothing should change on an unchanged project"
    );
    // The receipt chain legitimately advances on each real sync, so compare
    // only the generated outputs, not the .ggen-v2 receipt state.
    let filtered = |m: &BTreeMap<PathBuf, String>| -> BTreeMap<PathBuf, String> {
        m.iter()
            .filter(|(p, _)| !p.starts_with(".ggen-v2") && !p.starts_with(".ggen"))
            .map(|(p, h)| (p.clone(), h.clone()))
            .collect()
    };
    assert_eq!(
        filtered(&after_first),
        filtered(&snapshot(dir.path())),
        "generated outputs must be byte-identical across repeated applies"
    );
}

// ---------------------------------------------------------------------------
// ggen_check_project
// ---------------------------------------------------------------------------

#[test]
fn check_project_runs_and_reports_counts() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let before = snapshot(dir.path());
    let got = check_project(&CheckProjectParams {
        root: dir.path().display().to_string(),
        paths: None,
        with_routes: false,
    })
    .expect("check");
    let after = snapshot(dir.path());

    assert_eq!(before, after, "checking a project must be read-only");
    assert!(got.ok);
    assert_eq!(
        got.has_errors,
        got.error_count > 0,
        "has_errors must agree with the count"
    );
    assert!(
        got.report.is_object(),
        "the full CheckReport must be carried through"
    );
}

/// An explicit path that escapes the root must be refused, not checked.
#[test]
fn check_project_refuses_path_traversal_in_explicit_paths() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());

    let err = check_project(&CheckProjectParams {
        root: dir.path().display().to_string(),
        paths: Some(vec!["../../../etc/passwd".to_string()]),
        with_routes: false,
    })
    .expect_err("traversal must be refused");
    assert_eq!(err.category, ErrorCategory::PathTraversal);
}
