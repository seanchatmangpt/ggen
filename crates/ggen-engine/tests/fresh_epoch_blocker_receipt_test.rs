//! Staleness guard for `docs/jira/v26.11.15/FRESH-EPOCH-BLOCKER.md` (lane L09).
//!
//! The blocker receipt makes claims about repo state: `court-fresh-epoch` is
//! wired into `pre-commit`, its recipe runs the shim-court script, the script
//! maps a non-exercisable subject to exit 2 BLOCKED, and the stale-lease CLI
//! refusal exits 5. If any of those facts change, the receipt (and the
//! "BLOCKED-class" / "promoted" label) is stale and this test fails. Chicago
//! style: reads the real files, no doubles.

use std::path::PathBuf;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

fn read(rel: &str) -> String {
    std::fs::read_to_string(repo_root().join(rel)).unwrap_or_else(|e| panic!("read {rel}: {e}"))
}

#[test]
fn receipt_status_matches_justfile_wiring() {
    let justfile = read("justfile");
    let receipt = read("docs/jira/v26.11.15/FRESH-EPOCH-BLOCKER.md");

    let precommit = justfile
        .lines()
        .find(|l| l.starts_with("pre-commit:"))
        .expect("pre-commit recipe line");
    let wired = precommit.split_whitespace().any(|t| t == "court-fresh-epoch");

    let says_promoted = receipt.contains("STATUS: PROMOTED");
    let says_blocked = receipt.contains("STATUS: BLOCKED");
    assert!(
        says_promoted ^ says_blocked,
        "receipt must carry exactly one of `STATUS: PROMOTED` / `STATUS: BLOCKED`"
    );
    assert_eq!(
        wired, says_promoted,
        "receipt status is stale: justfile pre-commit wiring of court-fresh-epoch = {wired}"
    );
}

#[test]
fn recipe_still_runs_the_shim_court_and_script_keeps_typed_exit_two() {
    let justfile = read("justfile");
    let recipe_start = justfile
        .find("\ncourt-fresh-epoch:")
        .expect("court-fresh-epoch recipe");
    let recipe = &justfile[recipe_start..];
    let recipe = &recipe[..recipe.find("\n\n").unwrap_or(recipe.len())];
    assert!(
        recipe.contains("bash scripts/fresh_epoch_lease_court.sh"),
        "recipe must still run the shim court: {recipe}"
    );

    let script = read("scripts/fresh_epoch_lease_court.sh");
    assert!(
        script.contains("2 BLOCKED"),
        "script must keep exit 2 = BLOCKED"
    );
    assert!(
        script.contains("SHIM-COURT"),
        "script must keep the SHIM-COURT marker"
    );
}

#[test]
fn stale_lease_cli_refusal_keeps_exit_five_mapping() {
    let main_rs = read("crates/ggen-cli/src/main.rs");
    assert!(
        main_rs.contains("FM-WRITE-012") && main_rs.contains("exits 5"),
        "CLI main must still document/map FM-WRITE-012 to exit 5"
    );
    let boundary = read("crates/ggen-engine/tests/cli_boundary.rs");
    assert!(
        boundary.contains("fn sync_run_stale_lease_refusal_exits_five"),
        "end-to-end exit-5 test must remain"
    );
}
