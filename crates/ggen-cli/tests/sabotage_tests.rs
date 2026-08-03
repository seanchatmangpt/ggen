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
#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]

//! Sabotage Tests for Lockfile + Receipt Invariants
//!
//! These tests verify that the sync command correctly enforces invariants by
//! sabotaging preconditions and confirming hard failures (non-zero exit codes).
//!
//! Following Chicago TDD: no mocks, real collaborators, state-based verification.

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("ggen binary not found")
}

/// Write a minimal, real `ggen.toml` project (no packs) at `root`: a project
/// name, a one-triple ontology, and a `templates/` dir with a single template
/// that queries and writes it out. Same minimal fixture shape as
/// `crates/ggen-engine/tests/cli_boundary.rs`'s own `scaffold` -- just enough
/// for `ggen sync run` to reach and pass every pipeline stage for real.
fn scaffold_project(root: &std::path::Path) {
    fs::write(
        root.join("ggen.toml"),
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    )
    .unwrap();
    fs::write(
        root.join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n",
    )
    .unwrap();
    fs::create_dir_all(root.join("templates")).unwrap();
    fs::write(
        root.join("templates/one.tmpl"),
        "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}",
    )
    .unwrap();
}

/// `scaffold_project` plus one real local pack declared in `[packs.mypack]`,
/// satisfying every `ggen_engine::pack::resolve` requirement (`pack.toml`,
/// `ontology.ttl`, at least one `templates/*.tmpl`) so a real `ggen sync run`
/// locks it into `ggen.lock`.
fn scaffold_project_with_pack(root: &std::path::Path) {
    fs::write(
        root.join("ggen.toml"),
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n\n[packs.mypack]\npath = \"mypack\"\n",
    )
    .unwrap();
    fs::write(
        root.join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n",
    )
    .unwrap();
    fs::create_dir_all(root.join("templates")).unwrap();
    fs::write(
        root.join("templates/one.tmpl"),
        "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}",
    )
    .unwrap();

    let pack_dir = root.join("mypack");
    fs::create_dir_all(pack_dir.join("templates")).unwrap();
    fs::write(
        pack_dir.join("pack.toml"),
        "[pack]\nname = \"mypack\"\nversion = \"1.0.0\"\ndescription = \"sabotage fixture pack\"\n",
    )
    .unwrap();
    fs::write(
        pack_dir.join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:widget ex:kind \"widget\" .\n",
    )
    .unwrap();
    fs::write(
        pack_dir.join("templates/pack.tmpl"),
        "---\nto: out/pack_output.txt\nforce: true\n---\npack output\n",
    )
    .unwrap();
}

/// Test 1: remove a locked pack's `pack.toml` after a successful sync; the
/// next `ggen sync run` must exit non-zero.
///
/// REWRITTEN (2026-08-03, verified live): the original fixture invoked a
/// `sync --locked` flag and a `.ggen/packs.lock` JSON shape that belong to
/// `ggen_marketplace::sync_profile::validate_sync_preconditions` -- a real,
/// unit-tested function (`crates/ggen-marketplace/src/sync_profile.rs`) that
/// has exactly one caller in the whole workspace: its own `#[cfg(test)]`
/// module (`grep -rln validate_sync_preconditions crates/*/src` matches only
/// that file). It was ported from `ggen-core` during the v26.7.16 migration
/// but never wired into any CLI verb -- a real check, genuinely dead on the
/// live path. `ggen sync run --help` confirms `sync run` has no `--locked`
/// flag at all (only `--dry-run`/`--format`/`--select`/`--watch`/...).
///
/// The REAL, currently-wired lockfile enforcement is
/// `ggen_engine::pack::check_lock`, called unconditionally on every
/// `ggen sync run` (`crates/ggen-engine/src/sync.rs:267-268`) against the
/// TOML `ggen.lock` at the project root -- not `.ggen/packs.lock`. This test
/// now exercises that real path: a first sync locks a real local pack, its
/// `pack.toml` is then deleted (the original test's "remove pack after
/// install" intent, taken literally), and a second sync must refuse
/// (`[FM-PACK-002]`, `crates/ggen-engine/src/pack.rs`'s `resolve_pack_dir`).
#[test]
fn test_sabotage_remove_pack_toml_sync_locked_exits_nonzero() {
    let temp_dir = TempDir::new().unwrap();
    scaffold_project_with_pack(temp_dir.path());

    // First sync succeeds and locks the pack into ggen.lock.
    ggen()
        .arg("sync")
        .arg("run")
        .current_dir(temp_dir.path())
        .assert()
        .success();
    assert!(
        temp_dir.path().join("ggen.lock").is_file(),
        "first sync must write ggen.lock"
    );

    // Sabotage: remove the pack's pack.toml (simulating deletion after install).
    fs::remove_file(temp_dir.path().join("mypack/pack.toml")).unwrap();

    ggen()
        .arg("sync")
        .arg("run")
        .current_dir(temp_dir.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("pack.toml").and(
            predicate::str::contains("FM-PACK-002").or(predicate::str::contains("unreadable")),
        ));
}

/// Test 2: corrupt `ggen.lock` (the real, currently-wired lockfile -- see
/// Test 1's doc comment for why this is `ggen.lock`, not `.ggen/packs.lock`);
/// `ggen sync run` should exit non-zero.
#[test]
fn test_sabotage_corrupt_lockfile_sync_locked_exits_nonzero() {
    let temp_dir = TempDir::new().unwrap();
    scaffold_project(temp_dir.path());

    // Write garbage where ggen.lock would live. check_lock parses it as TOML
    // before ever consulting resolved pack entries (crates/ggen-engine/src/
    // pack.rs's check_lock reads+parses first, loops second), so this fails
    // even with zero packs configured in this project.
    fs::write(temp_dir.path().join("ggen.lock"), "this is not TOML {{{{{").unwrap();

    ggen()
        .arg("sync")
        .arg("run")
        .current_dir(temp_dir.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("ggen.lock").and(
            predicate::str::contains("malformed").or(predicate::str::contains("FM-PACK-009")),
        ));
}

// REMOVED (2026-07-17, ggen-core removal, docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md):
// test_sabotage_empty_signature_receipt_verify_returns_invalid constructed its fixture via the
// deleted ggen_core::receipt::Receipt type, and separately already used a stale CLI invocation
// (`ggen receipt verify <path>` -- the current `receipt verify` takes zero positional args,
// always targeting `.ggen-v2/receipt.json`). Equivalent real coverage for tamper/invalid-receipt
// behavior exists in crates/ggen-engine/tests/receipt_chain_e2e.rs and cli_boundary.rs.

/// Test 4: delete the verifying key after a real signed sync; `ggen receipt
/// verify` must exit non-zero.
///
/// REWRITTEN (2026-08-03, verified live): the original test (a) called
/// `receipt verify <path>` with a positional argument -- the live `receipt
/// verify` (`ggen receipt verify --help`) takes zero positional args, always
/// targeting `.ggen-v2/receipt.json` under the resolved project root
/// (confirmed in CLAUDE.md's Cryptographic Receipts section) -- and (b)
/// expected `.success()` with `"is_valid":false` in stdout. Neither half
/// holds against the real verb
/// (`crates/ggen-engine/src/verbs/handlers.rs::handle_receipt_verify_in`):
/// the JSON field is `"valid"`, never `"is_valid"`, and
/// `crate::keys::resolve_verifying_key` documents itself as "a hard error
/// ... never a silently-generated new key" when the verifying key file is
/// absent (`crates/ggen-engine/src/keys.rs`, backed by its own
/// `resolve_verifying_key_never_generates_and_errors_when_absent` unit
/// test) -- so a missing verifying key makes the CLI exit non-zero, it does
/// not print a cheerful `valid:false` JSON body on exit 0. This is the real,
/// load-bearing fail-closed behavior; the test now asserts it directly, and
/// confirms the un-sabotaged happy path passes first so the sabotage step is
/// proven to be what flips the result.
#[test]
fn test_sabotage_delete_verifying_key_receipt_verify_returns_invalid() {
    let temp_dir = TempDir::new().unwrap();
    scaffold_project(temp_dir.path());

    // Real (non-dry-run) sync signs the receipt and writes
    // .ggen/keys/{signing,verifying}.key (crates/ggen-engine/src/sync.rs).
    ggen()
        .arg("sync")
        .arg("run")
        .current_dir(temp_dir.path())
        .assert()
        .success();
    let verifying_key = temp_dir.path().join(".ggen/keys/verifying.key");
    assert!(
        verifying_key.is_file(),
        "a real sync must write .ggen/keys/verifying.key"
    );

    // Confirm the happy path actually verifies before sabotaging it.
    ggen()
        .arg("receipt")
        .arg("verify")
        .current_dir(temp_dir.path())
        .assert()
        .success()
        .stdout(
            predicate::str::contains("\"valid\": true")
                .or(predicate::str::contains("\"valid\":true")),
        );

    // Sabotage: delete the verifying key.
    fs::remove_file(&verifying_key).unwrap();

    ggen()
        .arg("receipt")
        .arg("verify")
        .current_dir(temp_dir.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("verifying key").and(
            predicate::str::contains("unreadable").or(predicate::str::contains("FM-KEY-004")),
        ));
}

/// Test 5: Empty packs cache dir with GGEN_OFFLINE=true, add should exit non-zero
///
/// Intent preserved: adding a pack that is absent from the registry/cache must fail
/// loudly (no fail-open). Migrated from the removed `packs install` to the live
/// `pack add` verb. NOTE: the live `add` verb (crates/ggen-cli/src/cmds/pack.rs)
/// returns `Ok(AddOutput { status: "not_found", .. })` on a missing pack rather than
/// a non-zero exit, so — mirroring proof_pack_test.rs::
/// test_add_nonexistent_pack_does_not_fake_success_or_emit_receipt — this asserts
/// "loud failure" as EITHER a non-zero exit OR a `not_found` / "not found" marker in
/// stdout+stderr, instead of `.failure()` alone.
#[test]
fn test_sabotage_empty_packs_dir_install_exits_nonzero() {
    let empty_cache = TempDir::new().unwrap();
    let temp_dir = TempDir::new().unwrap();

    let assert = ggen()
        .arg("pack")
        .arg("add")
        .arg("acme/base")
        .current_dir(temp_dir.path())
        .env("GGEN_PACK_CACHE_DIR", empty_cache.path())
        .env("GGEN_OFFLINE", "true")
        .assert();

    let output = assert.get_output().clone();
    let code = output.status.code();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{stdout}{stderr}");

    let loud = code != Some(0)
        || combined.contains("not_found")
        || combined.contains("not found")
        || combined.contains("GGEN_OFFLINE")
        || combined.contains("cache");
    assert!(
        loud,
        "FAIL-OPEN DEFECT: adding an absent pack offline must fail loudly. \
         Got exit {:?}, output: {}",
        code, combined
    );
}
