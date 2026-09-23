//! Chicago-style integration tests for the live `ggen pack` CLI surface.
//!
//! Added 2026-08-24 (`docs/jira/v26.8.16/04-MARKETPLACE-TEST-SUITE-DISABLED.md`) as the
//! real replacement coverage for the seven v1/v2-transitional marketplace test modules
//! archived the same pass — see `crates/ggen-cli/tests/archive/marketplace/README.md` for
//! why those were archived rather than repaired. These tests drive the real compiled
//! `ggen` binary (`assert_cmd::Command::cargo_bin`) against the real local pack registry
//! under `marketplace/packs/` in this repo. No mocks, no stubs — state-based assertions on
//! actual stdout (JSON) from the actual process.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;

/// `ggen pack list` — real registry, real JSON output, at least one pack present.
#[test]
fn pack_list_returns_real_packs() {
    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary must be built for tests");

    let output = cmd
        .arg("pack")
        .arg("list")
        .output()
        .expect("ggen pack list must execute");

    assert!(
        output.status.success(),
        "ggen pack list exited non-zero: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8(output.stdout).expect("stdout must be valid UTF-8");
    let parsed: serde_json::Value =
        serde_json::from_str(&stdout).expect("ggen pack list must emit valid JSON");

    let packs = parsed["packs"]
        .as_array()
        .expect("response must contain a `packs` array");
    assert!(
        !packs.is_empty(),
        "expected at least one real pack in the local registry, got zero"
    );

    let total = parsed["total"]
        .as_u64()
        .expect("response must have `total`");
    assert_eq!(
        total as usize,
        packs.len(),
        "`total` must match the real length of `packs`"
    );

    // Every entry must carry the real fields the CLI's ListOutput/PackSummary type emits.
    for pack in packs {
        assert!(pack["id"].is_string(), "pack entry missing `id`: {pack}");
        assert!(
            pack["name"].is_string(),
            "pack entry missing `name`: {pack}"
        );
        assert!(
            pack["registry_type"].is_string(),
            "pack entry missing `registry_type`: {pack}"
        );
    }
}

/// `ggen pack search <query>` against a query known (from `pack list`) to match a real
/// local pack — asserts the real search actually found it, not just that the process ran.
#[test]
fn pack_search_finds_a_real_match() {
    // Ground the query in real registry state instead of a hardcoded guess: read the
    // first pack's name from `pack list` and search for one of its words.
    let list_output = Command::cargo_bin("ggen")
        .expect("ggen binary must be built for tests")
        .arg("pack")
        .arg("list")
        .output()
        .expect("ggen pack list must execute");
    assert!(list_output.status.success());
    let list_json: serde_json::Value =
        serde_json::from_str(&String::from_utf8_lossy(&list_output.stdout))
            .expect("ggen pack list must emit valid JSON");
    let packs = list_json["packs"].as_array().expect("`packs` array");
    assert!(
        !packs.is_empty(),
        "need at least one real pack to search for"
    );
    let seed_id = packs[0]["id"]
        .as_str()
        .expect("pack id is a string")
        .to_string();
    // Pack ids in this registry are kebab-case; the first token is a real search term.
    let query = seed_id.split('-').next().unwrap_or(&seed_id).to_string();

    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary must be built for tests");
    let assert = cmd.arg("pack").arg("search").arg(&query).assert();

    let output = assert.get_output();
    assert!(
        output.status.success(),
        "ggen pack search {query} exited non-zero: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let parsed: serde_json::Value =
        serde_json::from_str(&stdout).expect("ggen pack search must emit valid JSON");
    assert_eq!(parsed["query"], query, "response must echo the real query");
    let results = parsed["results"].as_array().expect("`results` array");
    assert!(
        !results.is_empty(),
        "searching for '{query}' (derived from a real pack id) found zero results"
    );
}

/// `ggen pack search` with a query that cannot match anything real — the command must
/// still succeed and report a real, honest zero, not fabricate a result.
#[test]
fn pack_search_with_no_match_returns_honest_zero() {
    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary must be built for tests");
    let assert = cmd
        .arg("pack")
        .arg("search")
        .arg("zzz-no-such-pack-should-ever-match-zzz")
        .assert();

    assert.success().stdout(
        predicate::str::contains("\"total\": 0").or(predicate::str::contains("\"total\":0")),
    );
}

/// `ggen pack show <id>` on a pack id taken from real `pack list` output — the detail
/// view must actually resolve, not just parse.
#[test]
fn pack_show_resolves_a_real_pack() {
    let list_output = Command::cargo_bin("ggen")
        .expect("ggen binary must be built for tests")
        .arg("pack")
        .arg("list")
        .output()
        .expect("ggen pack list must execute");
    let list_json: serde_json::Value =
        serde_json::from_str(&String::from_utf8_lossy(&list_output.stdout))
            .expect("ggen pack list must emit valid JSON");
    let packs = list_json["packs"].as_array().expect("`packs` array");
    assert!(!packs.is_empty(), "need at least one real pack to show");
    let seed_id = packs[0]["id"]
        .as_str()
        .expect("pack id is a string")
        .to_string();

    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary must be built for tests");
    let output = cmd
        .arg("pack")
        .arg("show")
        .arg(&seed_id)
        .output()
        .expect("ggen pack show must execute");

    assert!(
        output.status.success(),
        "ggen pack show {seed_id} exited non-zero: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(&seed_id),
        "ggen pack show {seed_id} output did not echo the pack id back: {stdout}"
    );
}

/// `ggen pack show` on an id that cannot exist — verifies the real error path names the
/// missing pack, rather than silently returning stale/cached data.
#[test]
fn pack_show_on_missing_pack_reports_not_found() {
    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary must be built for tests");
    let output = cmd
        .arg("pack")
        .arg("show")
        .arg("zzz-definitely-not-a-real-pack-id-zzz")
        .output()
        .expect("ggen pack show must execute");

    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("not found") || combined.contains("Failed to get pack"),
        "expected an honest not-found message, got: {combined}"
    );
}
