//! Real Chicago-TDD boundary test for the `ggen-selfplay-explore` binary
//! (`crates/ggen-mcp/src/bin/ggen-selfplay-explore.rs`).
//!
//! Before this test, the binary existed and was documented but never
//! actually invoked by anything in the test suite (only *mentioned* in a
//! comment in `tests/self_play_test.rs`) — a real gap between "this exists"
//! and "this is exercised."
//!
//! The binary's normal mode makes a real, live HTTP call to a local LLM
//! server (`GGEN_SELFPLAY_ENDPOINT`, default `127.0.0.1:8080`) per its own
//! module doc — deliberately NOT put in any assertion path, since an LLM is
//! nondeterministic. This test does not try to fake that around a mock (this
//! repo's Chicago-TDD discipline forbids mocking a collaborator this way);
//! instead it exercises the binary's real, deterministic, offline-safe
//! boundary: `--packs 0` truncates the pack list to zero *before* any
//! network call is made (`packs.truncate(args.packs)` in `main()`), so this
//! is a real subprocess invocation, real CLI-argument parsing, real
//! filesystem read of `packs/`, and a real process exit code — with zero
//! LLM calls, not a mock standing in for one.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::process::Command;

fn selfplay_explore_bin() -> &'static str {
    env!("CARGO_BIN_EXE_ggen-selfplay-explore")
}

#[test]
fn dry_run_zero_packs_exits_clean_with_no_network_call() {
    let output = Command::new(selfplay_explore_bin())
        .args(["--packs", "0", "--dry-run"])
        .output()
        .expect("real subprocess invocation of ggen-selfplay-explore must succeed");

    assert!(
        output.status.success(),
        "expected exit 0 for --packs 0 --dry-run, got {:?}\nstdout: {}\nstderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("self-play explore: 0 pack(s)"),
        "expected the real summary line for zero packs, got:\n{stdout}"
    );
    assert!(
        stdout.contains("played 0 case(s); 0 tripped an invariant"),
        "expected the real zero-work summary, got:\n{stdout}"
    );
}

#[test]
fn unknown_argument_is_a_real_warning_not_a_crash() {
    // `parse_args()`'s real behavior for an unrecognized flag: print a
    // warning to stderr and continue, never panic/exit non-zero for this
    // alone. `--packs 0` keeps this run network-free, same as above.
    let output = Command::new(selfplay_explore_bin())
        .args(["--packs", "0", "--dry-run", "--not-a-real-flag"])
        .output()
        .expect("real subprocess invocation must succeed");

    assert!(
        output.status.success(),
        "an unknown flag must not crash the process: {:?}",
        output.status.code()
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("warning: ignoring unknown argument"),
        "expected the real unknown-argument warning, got:\n{stderr}"
    );
}
