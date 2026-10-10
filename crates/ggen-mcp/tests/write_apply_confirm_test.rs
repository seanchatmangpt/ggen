//! Behavioral court for `ggen_write_apply`'s confirm gate (disjoint from
//! `tools_surface_parity_test.rs`, which only proves registration, and from
//! `write_apply_authorization_test.rs`, which proves receipt provenance).
//!
//! Chicago style: every court calls the real `write_apply` handler against a
//! real frontmatter project in a `TempDir` and asserts on filesystem state
//! after the call -- was the target file created, overwritten, or left
//! byte-identical? No doubles.
//!
//! Semantics pinned here (read from `src/tools/write_apply.rs` before
//! writing, confirmed by these runs):
//! 1. `confirm` gate fires FIRST, before root resolution or any pipeline
//!    work -- refusal is `ErrorCategory::Unsupported` and the filesystem is
//!    untouched (no target file, no `.ggen-v2/receipt.json`).
//! 2. `confirm` is a typed `bool` in `WriteApplyParams`; a JSON payload
//!    carrying a string (`"yes"`) fails `Deserialize` -- strict at the wire
//!    boundary, so a loose-truthy value can never reach the handler.
//! 3. `confirm: true` alone is NOT sufficient: `expected_graph_hash` from a
//!    real prior dry-run is independently required (CP17).
//! 4. Overwrite of a differing existing file additionally requires
//!    `force: true` in the TEMPLATE frontmatter (engine FM-WRITE-005):
//!    confirm without force refuses and the original bytes survive; with
//!    force the overwrite happens.
//! 5. A template whose `to:` path escapes the project root is refused by
//!    the engine's template validation (`FM-WRITE-002` "traversal
//!    component") at the preflight dry-run stage, surfacing through
//!    `write_apply` as `ErrorCategory::GraphLoadError`, and nothing is
//!    written outside the root.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests

use ggen_mcp::error::ErrorCategory;
use ggen_mcp::tools::sync_dry_run::{sync_dry_run, SyncDryRunParams};
use ggen_mcp::tools::write_apply::{write_apply, WriteApplyParams};
use tempfile::TempDir;

/// Real frontmatter project: one template rendering one file, so every
/// court has a concrete target whose on-disk state it can assert on.
fn write_frontmatter_project(root: &std::path::Path) {
    std::fs::write(
        root.join("ggen.toml"),
        "[project]\nname = \"confirm-gate\"\n[ontology]\nsource = \"model.ttl\"\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        root.join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\n",
    )
    .expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(
        root.join("templates/plain.tmpl"),
        "---\nto: out/plain.txt\n---\nplain\n",
    )
    .expect("write template");
}

fn root_str(tmp: &TempDir) -> String {
    tmp.path().display().to_string()
}

/// Fresh graph hash from a REAL prior `ggen_sync_dry_run` against this root
/// -- the same corroboration a real MCP client must supply (CP17).
fn real_graph_hash(tmp: &TempDir) -> String {
    sync_dry_run(&SyncDryRunParams {
        root: root_str(tmp),
    })
    .expect("dry run")
    .graph_hash
}

fn apply(
    tmp: &TempDir, confirm: bool, hash: String,
) -> Result<ggen_mcp::tools::write_apply::WriteApplyResult, ggen_mcp::error::McpError> {
    write_apply(&WriteApplyParams::new(root_str(tmp), confirm, hash))
}

/// (1) Without `confirm`, the tool refuses with the typed Unsupported
/// category BEFORE any pipeline work: no target file, no receipt, no
/// `.ggen-v2` directory at all -- the filesystem is untouched.
#[test]
fn without_confirm_refuses_and_writes_nothing() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());

    let err = apply(&tmp, false, real_graph_hash(&tmp)).expect_err("must refuse");
    assert_eq!(err.category, ErrorCategory::Unsupported);
    assert!(err.message.contains("confirm: true"), "{err}");

    assert!(
        !tmp.path().join("out/plain.txt").exists(),
        "target file must not be created without confirm"
    );
    assert!(
        !tmp.path().join(".ggen-v2").exists(),
        "no receipt directory may appear on a refused call"
    );
}

/// (2) With `confirm: true` AND a real prior dry-run's graph hash, the real
/// write happens and the content on disk matches the template body.
#[test]
fn with_confirm_and_real_hash_writes_target_content() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());
    let hash = real_graph_hash(&tmp);

    let result = apply(&tmp, true, hash).expect("apply must succeed");
    assert!(result.ok);
    assert_eq!(result.write_count, 1, "{result:?}");

    let target = tmp.path().join("out/plain.txt");
    assert_eq!(
        std::fs::read_to_string(&target).expect("read written target"),
        "plain\n",
        "on-disk content must match the template body"
    );
    assert!(
        tmp.path().join(".ggen-v2/receipt.json").exists(),
        "a real apply always writes its sync receipt"
    );
}

/// (2b) `confirm: true` with an empty `expected_graph_hash` is refused too:
/// the boolean alone is deliberately redundant, never sufficient (CP17).
#[test]
fn confirm_true_without_graph_hash_is_refused_before_any_write() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());

    let err = apply(&tmp, true, String::new()).expect_err("must refuse");
    assert_eq!(err.category, ErrorCategory::Unsupported);
    assert!(err.message.contains("expected_graph_hash"), "{err}");
    assert!(!tmp.path().join("out/plain.txt").exists());
}

/// (3) `confirm` is a typed `bool` at the wire boundary: a JSON payload
/// carrying the string `"yes"` fails `Deserialize` -- a loose-truthy value
/// can never reach the handler. (The handler itself needs no runtime check;
/// strictness is structural.)
#[test]
fn confirm_as_string_is_refused_at_deserialization() {
    let payload = serde_json::json!({
        "root": "/tmp/whatever",
        "confirm": "yes",
        "expected_graph_hash": "deadbeef"
    });
    let err = serde_json::from_value::<WriteApplyParams>(payload)
        .expect_err("a string confirm must not deserialize into a bool field");
    assert!(
        err.to_string().contains("expected a boolean"),
        "serde must reject the loose-truthy string: {err}"
    );
}

/// (4a) Overwrite is pinned actual behavior: `confirm: true` alone does NOT
/// clobber an existing differing file -- the engine refuses silent clobber
/// (FM-WRITE-005) unless the TEMPLATE itself declares `force: true`. With
/// that declaration the write proceeds and overwrites.
#[test]
fn with_confirm_and_template_force_overwrites_existing_file() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());
    std::fs::write(
        tmp.path().join("templates/plain.tmpl"),
        "---\nto: out/plain.txt\nforce: true\n---\nplain\n",
    )
    .expect("write force template");
    std::fs::create_dir_all(tmp.path().join("out")).expect("mkdir out");
    std::fs::write(tmp.path().join("out/plain.txt"), "STALE\n").expect("seed stale file");

    let hash = real_graph_hash(&tmp);
    apply(&tmp, true, hash).expect("apply must succeed with template force");

    assert_eq!(
        std::fs::read_to_string(tmp.path().join("out/plain.txt")).expect("read target"),
        "plain\n",
        "existing file must be overwritten with fresh rendered content"
    );
}

/// (4b) A differing existing file is NOT clobbered by `confirm: true`
/// alone: the engine refuses (FM-WRITE-005, `force: true` remediation) and
/// the original bytes survive byte-identically. The confirm gate is
/// necessary but not sufficient to destroy existing state.
#[test]
fn confirm_true_alone_refuses_clobber_and_original_content_survives() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());
    std::fs::create_dir_all(tmp.path().join("out")).expect("mkdir out");
    let original = "STALE\n".to_string();
    std::fs::write(tmp.path().join("out/plain.txt"), &original).expect("seed stale file");

    let err = apply(&tmp, true, real_graph_hash(&tmp)).expect_err("clobber must refuse");
    assert_eq!(err.category, ErrorCategory::GraphLoadError);
    assert!(err.message.contains("FM-WRITE-005"), "{err}");
    assert!(err.message.contains("force: true"), "{err}");

    assert_eq!(
        std::fs::read_to_string(tmp.path().join("out/plain.txt")).expect("read target"),
        original,
        "refused clobber must leave the original bytes intact"
    );
}

/// (5) A template whose `to:` escapes the project root is refused by the
/// engine's template-validation traversal guard (pinned actual behavior:
/// `[FM-WRITE-002]` at the preflight dry-run stage, surfacing through
/// `write_apply` as `GraphLoadError`) and nothing lands outside the root.
#[test]
fn template_path_escaping_root_is_refused_and_writes_nothing_outside() {
    let tmp = TempDir::new().expect("tempdir");
    let outside = TempDir::new().expect("tempdir-outside");
    write_frontmatter_project(tmp.path());
    // Hash from BEFORE the escaping template exists, so the apply call's own
    // preflight dry-run is what first validates the escaping template -- the
    // exact path a real caller would hit.
    let hash = real_graph_hash(&tmp);
    std::fs::write(
        tmp.path().join("templates/escape.tmpl"),
        "---\nto: ../escaped.txt\n---\npwn\n",
    )
    .expect("write escaping template");

    let err = apply(&tmp, true, hash).expect_err("escape must refuse");
    assert_eq!(err.category, ErrorCategory::GraphLoadError);
    assert!(
        err.message.contains("FM-WRITE-002") && err.message.contains("traversal"),
        "engine's typed traversal refusal expected, got: {err}"
    );

    assert!(
        !outside.path().join("escaped.txt").exists(),
        "no file may be written outside the project root"
    );
    assert!(
        !tmp.path().parent().unwrap().join("escaped.txt").exists(),
        "no file may be written beside the project root either"
    );
}
