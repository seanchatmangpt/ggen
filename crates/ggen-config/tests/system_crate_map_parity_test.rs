//! System row evidence (v26.8.1 coverage matrix, `system` subsystem): proves the
//! "complete crate map" claim is not just a table someone once typed in -- it is
//! cross-checked, on every test run, against the two independent real sources:
//!
//! 1. `Cargo.toml`'s `[workspace] members = [...]` array (ground truth for what
//!    actually builds in this workspace).
//! 2. `.specify/repo-facts.ttl`'s `rf:Crate` individuals with `rf:dir "..."` facts
//!    (the RDF source CLAUDE.md's "Crate Map" table and `.claude/rules/architecture.md`
//!    are generated from -- see that file's own header comment).
//!
//! Chicago TDD: no mocks. Both files are read directly off disk from the real git
//! worktree and parsed with plain text/line scanning (no crate dependency on a full
//! `toml` parse is needed for a `members = [...]` array whose entries are one per line,
//! and repo-facts.ttl's `rf:dir "..."` facts are likewise line-oriented Turtle).
//!
//! This test was added after discovering a REAL divergence: `crates/openapi-cnv-reflect`
//! was present in `Cargo.toml`'s workspace members but absent from
//! `.specify/repo-facts.ttl`'s `rf:Crate` facts (confirmed 2026-07-31 by diffing the two
//! sets by hand before writing this test). `.specify/repo-facts.ttl` was extended with
//! `rf:crate_openapi_cnv_reflect` to close that gap; this test is the regression guard
//! that keeps it closed.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::BTreeSet;
use std::path::PathBuf;

/// Absolute path to the real workspace root `Cargo.toml` (two levels up from
/// `crates/ggen-config`, this crate's `CARGO_MANIFEST_DIR` at test time).
fn workspace_cargo_toml_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("Cargo.toml")
}

/// Absolute path to the real `.specify/repo-facts.ttl`.
fn repo_facts_ttl_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(".specify/repo-facts.ttl")
}

/// Parse `Cargo.toml`'s `[workspace] members = [...]` array and return the set of
/// `crates/<dir>` entries with the `crates/` prefix stripped, e.g. `"ggen-config"`.
/// Only lines that are literal `"crates/<name>",` entries inside the array are counted;
/// commented-out entries (`# "examples/..."`) are correctly excluded because they don't
/// match the `"crates/` line-start pattern the real array entries use after trimming.
fn cargo_toml_workspace_crate_dirs() -> BTreeSet<String> {
    let text = std::fs::read_to_string(workspace_cargo_toml_path())
        .unwrap_or_else(|e| panic!("failed to read root Cargo.toml: {e}"));
    let mut in_workspace_members = false;
    let mut dirs = BTreeSet::new();
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed == "members = [" {
            in_workspace_members = true;
            continue;
        }
        if in_workspace_members {
            if trimmed.starts_with(']') {
                break;
            }
            if let Some(rest) = trimmed.strip_prefix("\"crates/") {
                if let Some(name) = rest.split('"').next() {
                    dirs.insert(name.to_string());
                }
            }
        }
    }
    dirs
}

/// Parse `.specify/repo-facts.ttl` and return the set of `rf:dir "..."` values that
/// belong to an `a rf:Crate` individual (a Pack's `rf:dir` fact is a different class and
/// is intentionally excluded -- this test is about the crate map, not the pack
/// inventory). Implemented as a simple block scanner: an individual's block runs from a
/// line matching `... a rf:Crate ;` up to the terminating ` .` line, mirroring how the
/// rest of this file's individuals are laid out (confirmed by reading the file).
fn repo_facts_crate_dirs() -> BTreeSet<String> {
    let text = std::fs::read_to_string(repo_facts_ttl_path())
        .unwrap_or_else(|e| panic!("failed to read .specify/repo-facts.ttl: {e}"));
    let mut dirs = BTreeSet::new();
    let mut in_crate_block = false;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.contains("a rf:Crate") {
            in_crate_block = true;
        }
        if in_crate_block {
            if let Some(rest) = trimmed.strip_prefix("rf:dir \"") {
                if let Some(name) = rest.split('"').next() {
                    dirs.insert(name.to_string());
                }
            }
            if trimmed.ends_with(" .") {
                in_crate_block = false;
            }
        }
    }
    dirs
}

#[test]
fn cargo_toml_finds_real_workspace_crate_members() {
    // Sanity check on the parser itself before trusting it for the parity assertion
    // below: it must find crates known to exist right now (positive witness).
    let dirs = cargo_toml_workspace_crate_dirs();
    assert!(
        dirs.contains("ggen-config"),
        "parser did not find ggen-config in Cargo.toml workspace members: {dirs:?}"
    );
    assert!(
        dirs.contains("openapi-cnv-reflect"),
        "parser did not find openapi-cnv-reflect in Cargo.toml workspace members: {dirs:?}"
    );
    assert!(
        !dirs.is_empty(),
        "parser found zero workspace members -- almost certainly a parsing bug, not a real empty workspace"
    );
}

#[test]
fn repo_facts_ttl_crate_map_matches_cargo_toml_workspace_members() {
    let cargo_dirs = cargo_toml_workspace_crate_dirs();
    let mut facts_dirs = repo_facts_crate_dirs();
    // "ggen" is the workspace ROOT package (defined by root Cargo.toml's own
    // `[package]` table, not a `crates/<dir>` member) -- repo-facts.ttl legitimately
    // carries it as an `rf:Crate` individual (`rf:crate_ggen`, `rf:dir "ggen"`) even
    // though `cargo_toml_workspace_crate_dirs` (which only scans the `crates/` members
    // array) never will. Exclude it from the parity check rather than treat it as
    // stale.
    facts_dirs.remove("ggen");

    let missing_from_facts: Vec<_> = cargo_dirs.difference(&facts_dirs).collect();
    let stale_in_facts: Vec<_> = facts_dirs.difference(&cargo_dirs).collect();

    assert!(
        missing_from_facts.is_empty(),
        "crate(s) present in Cargo.toml [workspace] members but MISSING from \
         .specify/repo-facts.ttl's rf:Crate facts: {missing_from_facts:?}. \
         Add an rf:Crate individual with rf:dir \"<name>\" for each, per this repo's \
         'RDF is Truth' rule (CLAUDE.md), then `ggen sync run` to regenerate the \
         GENERATED crate-map table."
    );
    assert!(
        stale_in_facts.is_empty(),
        "crate(s) present in .specify/repo-facts.ttl's rf:Crate facts but NOT in \
         Cargo.toml [workspace] members (stale facts, real Contract Drift per \
         .claude/rules/coding-agent-mistakes.md): {stale_in_facts:?}"
    );
}
