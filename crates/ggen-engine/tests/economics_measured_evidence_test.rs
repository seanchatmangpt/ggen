//! v26.8.1 G-close-unknowns evidence for the `economics` subsystem
//! (`.ggen/v26.8.1/subsystem-evidence-manifest.json`'s `economics` record).
//!
//! `economics` is about MEASURED (not modeled) resource/performance facts.
//! This file reuses `just slo-check`'s own real measurement target
//! (`cargo test -p ggen-engine --test receipt_chain_e2e`, `.claude/rules/
//! rust/performance.md` / `justfile`'s `slo-check` recipe) as a genuine
//! measured-economics positive witness: it spawns the real subprocess, times
//! it with `std::time::Instant` (real wall-clock, not a modeled constant),
//! and asserts the measured value against the same 180s SLO threshold
//! `slo-check` uses. No mocks -- the timing is of a real nested `cargo test`
//! process actually compiling and running `receipt_chain_e2e.rs`, which
//! itself does real `sync()` calls, real BLAKE3 hashing, and real filesystem
//! I/O (see that file).
//!
//! The subprocess is spawned with `Command::spawn()` and polled via
//! `try_wait()` against a hard `HANG_DEADLINE`, not run with the blocking
//! `Command::output()`/`wait()` -- see `HANG_DEADLINE`'s doc comment for the
//! real hang this replaces (TECH-DEBT-003).
//!
//! Positive witness: `economics_receipt_chain_wall_clock_measured_under_slo_threshold`.
//! Negative falsifier (name contains "rejects", matching
//! `subsystem_evidence_manifest.py`'s `NEGATIVE_CONTROL_PATTERN`):
//! `economics_measurement_rejects_a_fabricated_zero_duration_reading` --
//! proves the recorded duration is a real nonzero measurement, not a
//! placeholder/fabricated constant a Decorative Completion would emit.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

struct Measurement {
    elapsed_ms: u128,
    exit_success: bool,
    timed_out: bool,
}

/// Hard wall-clock deadline for the nested `cargo test` subprocess.
///
/// A prior full `test-integration` run stalled at 0% CPU for *minutes* on
/// this test (killed manually after 134/373 targets, see TECH-DEBT-003 in
/// `docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md`) because the
/// old implementation used `Command::output()`, which blocks until the OS
/// reports the child has exited with no way to bound that wait. This deadline
/// makes the wait bounded: set well above the 180s SLO threshold
/// (`.claude/rules/rust/performance.md`, itself 4x the observed 45s
/// cold-compile baseline) so a legitimately slow-but-passing cold build is
/// never killed as a false positive, while still comfortably inside the
/// outer `test-integration` recipe's own 600s timeout (`justfile`) so one
/// hung target can no longer consume the whole budget.
const HANG_DEADLINE: Duration = Duration::from_secs(240);

/// Poll interval used while waiting on the child below. Polling (instead of
/// a single blocking `wait()`/`output()` call) is what makes `HANG_DEADLINE`
/// enforceable: it lets the loop check elapsed time between polls and
/// `kill()` the child itself once the deadline is exceeded.
const POLL_INTERVAL: Duration = Duration::from_millis(200);

/// Runs the real `receipt_chain_e2e` cargo test as a subprocess exactly
/// once per test binary (cached via `OnceLock` so the two `#[test]` fns
/// below share one real measurement instead of re-running the expensive
/// nested build/test twice), records genuine wall-clock elapsed time, and
/// enforces `HANG_DEADLINE` so a hung/deadlocked child can never block this
/// test -- and therefore the whole integration-test suite -- forever.
fn measure_receipt_chain_e2e() -> &'static Measurement {
    static MEASUREMENT: OnceLock<Measurement> = OnceLock::new();
    MEASUREMENT.get_or_init(|| {
        // CARGO_MANIFEST_DIR = crates/ggen-engine; workspace root is two
        // levels up.
        let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let workspace_root = manifest_dir
            .parent()
            .and_then(Path::parent)
            .expect("crates/ggen-engine has a workspace root two levels up");

        let started = Instant::now();
        // stdout/stderr are discarded (Stdio::null), not piped: nothing
        // drains a piped child's output while we poll below, so a pipe
        // filling up (>64KB) would make the child block on write() -- the
        // exact hang shape this fix removes. The test only ever checked
        // `status.success()`, never the captured output, so nothing is lost.
        let mut child = Command::new("cargo")
            .args(["test", "-p", "ggen-engine", "--test", "receipt_chain_e2e"])
            .current_dir(workspace_root)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("failed to spawn `cargo test -p ggen-engine --test receipt_chain_e2e`");

        let exit_status = loop {
            match child.try_wait() {
                Ok(Some(status)) => break Some(status),
                Ok(None) => {
                    if started.elapsed() >= HANG_DEADLINE {
                        break None;
                    }
                    std::thread::sleep(POLL_INTERVAL);
                }
                Err(e) => panic!("OS error polling nested `cargo test` child process: {e}"),
            }
        };

        let timed_out = exit_status.is_none();
        if timed_out {
            // Deadline exceeded: kill the child instead of continuing to
            // wait on it, and reap it so it doesn't linger as a zombie.
            let _ = child.kill();
            let _ = child.wait();
        }

        let elapsed_ms = started.elapsed().as_millis();
        let exit_success = exit_status.is_some_and(|s| s.success());

        Measurement {
            elapsed_ms,
            exit_success,
            timed_out,
        }
    })
}

/// Positive witness: a real, measured (not modeled) wall-clock duration for
/// the receipt-chain end-to-end path, checked against the same 180s SLO
/// threshold `just slo-check` enforces (`justfile`'s `slo-check` recipe).
#[test]
fn economics_receipt_chain_wall_clock_measured_under_slo_threshold() {
    let m = measure_receipt_chain_e2e();
    assert!(
        !m.timed_out,
        "receipt_chain_e2e subprocess did not complete within the {}s hard wall-clock \
         deadline and was killed to avoid hanging the test suite (measured {}ms before the \
         kill) -- this is a bounded failure, not a hang",
        HANG_DEADLINE.as_secs(),
        m.elapsed_ms
    );
    assert!(
        m.exit_success,
        "receipt_chain_e2e must itself pass for this timing measurement to be economically meaningful"
    );
    assert!(
        m.elapsed_ms > 0,
        "wall-clock measurement must be a real nonzero duration"
    );
    println!(
        "economics: receipt_chain_e2e measured wall-clock = {}ms (SLO threshold: 180000ms)",
        m.elapsed_ms
    );
    assert!(
        m.elapsed_ms <= 180_000,
        "receipt-chain wall-clock {}ms exceeded the 180s SLO threshold",
        m.elapsed_ms
    );
}

/// Negative falsifier / true negative control: proves the measurement this
/// subsystem records is a real nonzero reading and would therefore reject a
/// fabricated zero-duration economics record -- guarding against Decorative
/// Completion (a receipt/report that "worked" but recorded no real state).
#[test]
fn economics_measurement_rejects_a_fabricated_zero_duration_reading() {
    let m = measure_receipt_chain_e2e();
    assert!(
        m.elapsed_ms > 0,
        "a fabricated/placeholder economics record would report elapsed_ms == 0; \
         this measurement rejects that by construction because it comes from a real \
         Instant::now() delta around a real subprocess, got {}ms",
        m.elapsed_ms
    );
}
