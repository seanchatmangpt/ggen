//! L19 adversarial fail-open probes over the MCP authority surface.
//!
//! Chicago style: real handlers, real `TempDir` filesystems, assertions on
//! observable filesystem state and typed `ErrorCategory`.
//!
//! Real defect found + fixed: `ggen_pack_query`'s `pack_id` was joined into
//! `<packs_dir>/<pack_id>.toml` unvalidated (path traversal). The `pack_id`
//! courts below fail on the pre-fix code.
//! Clean probes (documented here, pinned so they stay clean): symlinked
//! `template_path` escape, `paths` traversal in `check_project`, `write_apply`
//! with a missing/empty hash, `receipt_verify` on an absent receipt.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests

use ggen_mcp::error::ErrorCategory;
use ggen_mcp::tools::check_project::{check_project, CheckProjectParams};
use ggen_mcp::tools::frontmatter_lint::{frontmatter_lint, FrontmatterLintParams};
use ggen_mcp::tools::pack_query::{pack_query, PackQueryParams};
use ggen_mcp::tools::receipt_verify::{receipt_verify, ReceiptVerifyParams};
use ggen_mcp::tools::write_apply::{write_apply, WriteApplyParams};
use tempfile::TempDir;

#[test]
fn pack_query_refuses_traversal_and_non_plain_pack_ids() {
    for bad in [
        "../../etc/passwd",
        "..",
        ".",
        "a/b",
        "a\\b",
        "/abs/path",
        "evil\0id",
        "",
        "has space",
    ] {
        let err = pack_query(&PackQueryParams {
            sparql: "SELECT * WHERE { ?s ?p ?o } LIMIT 1".to_string(),
            pack_id: Some(bad.to_string()),
        })
        .expect_err("hostile pack_id must be refused");
        // Empty is rejected earlier by the registry loader semantics; every
        // other id must be a typed PathTraversal refusal from this tool.
        assert_eq!(
            err.category,
            ErrorCategory::PathTraversal,
            "pack_id {bad:?} -> {err:?}"
        );
    }
}

#[test]
fn pack_query_traversal_does_not_read_outside_file() {
    // A parseable TOML placed outside any packs dir, reachable via ../ from
    // a relative packs dir, must never be loaded.
    let tmp = TempDir::new().unwrap();
    std::fs::write(tmp.path().join("outside.toml"), "[pack]\nid=\"x\"\n").unwrap();
    let rel = format!("../../..{}/outside", tmp.path().display());
    let err = pack_query(&PackQueryParams {
        sparql: "SELECT * WHERE { ?s ?p ?o }".to_string(),
        pack_id: Some(rel),
    })
    .expect_err("traversal pack_id refused");
    assert_eq!(err.category, ErrorCategory::PathTraversal);
}

#[test]
fn frontmatter_lint_refuses_symlink_escape_and_dotdot() {
    use std::os::unix::fs::symlink;
    let root = TempDir::new().unwrap();
    let outside = TempDir::new().unwrap();
    std::fs::write(outside.path().join("secret.tmpl"), "---\nto: x\n---\nsecret").unwrap();
    symlink(
        outside.path().join("secret.tmpl"),
        root.path().join("link.tmpl"),
    )
    .unwrap();
    for rel in ["link.tmpl", "../secret.tmpl", "/etc/passwd"] {
        let err = frontmatter_lint(&FrontmatterLintParams {
            root: root.path().display().to_string(),
            template_path: rel.to_string(),
        })
        .expect_err("escape must be refused");
        assert_eq!(err.category, ErrorCategory::PathTraversal, "{rel}");
    }
}

#[test]
fn check_project_refuses_paths_that_escape_root() {
    let root = TempDir::new().unwrap();
    let err = check_project(&CheckProjectParams {
        root: root.path().display().to_string(),
        paths: Some(vec!["../outside.ttl".to_string()]),
        with_routes: false,
    })
    .expect_err("escaping path refused");
    assert_eq!(err.category, ErrorCategory::PathTraversal);
}

#[test]
fn write_apply_refuses_without_confirm_empty_hash_or_bad_root_and_writes_nothing() {
    let root = TempDir::new().unwrap();
    let r = root.path().display().to_string();
    let e1 = write_apply(&WriteApplyParams::new(r.clone(), false, "abc".into()))
        .expect_err("confirm=false refused");
    assert_eq!(e1.category, ErrorCategory::Unsupported);
    let e2 = write_apply(&WriteApplyParams::new(r.clone(), true, "   ".into()))
        .expect_err("blank hash refused");
    assert_eq!(e2.category, ErrorCategory::Unsupported);
    let e3 = write_apply(&WriteApplyParams::new(
        format!("{r}/does-not-exist"),
        true,
        "abc".into(),
    ))
    .expect_err("bad root refused");
    assert_eq!(e3.category, ErrorCategory::PathTraversal);
    assert_eq!(std::fs::read_dir(root.path()).unwrap().count(), 0);
}

#[test]
fn receipt_verify_absent_receipt_is_not_valid() {
    let root = TempDir::new().unwrap();
    let res = receipt_verify(&ReceiptVerifyParams {
        root: root.path().display().to_string(),
    })
    .expect("reports rather than errors");
    assert!(!res.valid, "absent receipt must never read as valid");
    assert!(res.error_message.is_some());
}
