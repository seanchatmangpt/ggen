#!/usr/bin/env bash
# scripts/fresh_epoch_lease_court.sh — FreshEpochLease shim court
# (v26.10.20, lane A17, per wave-1 lane L09 plan).
#
# Proves the A06 stale-lease gate (`crates/ggen-engine/src/write.rs`
# `check_lease_generation`, FM-WRITE-012) is NON-VACUOUS:
#
#   BEFORE  the backdated-receipt court test passes against the PRISTINE
#           gate (stale receipts are refused).
#   SHIM    the gate is marked SHIM-COURT and admits ANY receipt
#           (early `Ok(())` before the baseline comparison).
#   AFTER   the SAME court test must FAIL under the shim — the refusal
#           disappears, so the test flips. If the test still passes with
#           the gate disabled, the court is a VACUOUS GATE (exit 1).
#   RESTORE byte-exact sha256 restore of write.rs, fail-closed.
#
# Exits: 0 non-vacuous · 1 VACUOUS GATE (AFTER passed under shim) ·
#        2 BLOCKED (pristine subject not exercisable — e.g. compile
#        failure; courts land RED until engine lanes land, per
#        RESOLUTIONS.md sequencing) · 3 SKIP-loud (missing environment).

set -euo pipefail
cd "$(dirname "$0")/.."

source scripts/shim_court_lib.sh

WRITE_RS="crates/ggen-engine/src/write.rs"
TEST_CMD="cargo test -p ggen-engine --test fresh_epoch_lease_court_test backdated_receipt_refused_stale_lease"

# Self-check first (RESOLUTIONS.md): prove restore-detection is not vacuous
# before shimming anything real.
shim_court_self_check || shim_court_fail SELF-CHECK "lib self-check failed; refusing to run"

command -v cargo >/dev/null 2>&1 || shim_court_skip "cargo not found on PATH"

apply_shim() {
    # Mark the A06 gate to admit any receipt: insert an early Ok(()) at the
    # top of check_lease_generation's body, carrying the SHIM-COURT marker.
    python3 - "$SHIM_FILE" <<'PY'
import sys
path = sys.argv[1]
with open(path) as f:
    src = f.read()
needle = "fn check_lease_generation(root: &Path, rel_to: &str, receipt_generation: u64) -> Result<()> {\n"
marker = needle + "    return Ok(()); // SHIM-COURT: gate disabled — admits any receipt\n"
if "SHIM-COURT" in src:
    sys.exit(1)
if needle not in src:
    sys.exit(2)
with open(path, "w") as f:
    f.write(src.replace(needle, marker, 1))
PY
}

# BEFORE: backdated test passes against the pristine gate. A witness failure
# here means the subject is not exercisable (compile failure / RED court) —
# the lib exits 2 BLOCKED.
with_shim "$WRITE_RS" "$TEST_CMD" "! $TEST_CMD"
