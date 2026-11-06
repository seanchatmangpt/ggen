#!/usr/bin/env bash
# sjira_control_court.sh — GA-7 anti-vacuity court for the DS-6 control plan.
#
# Proves scripts/sjira_control_plan.py can actually fail: runs it against a
# TEMP-COPY of crates/ggen-sjira with two independent mutations; the checker
# must exit 1 with the expected typed VIOLATION class on each. A court that
# cannot flip is FAIL (exit 1). The canonical tree is only ever READ — all
# mutations land on a mktemp copy that is deleted on exit. Never runs git.
#
# Mutations:
#   M1 (baseline tamper): baseline pub_fns count inflated -> D4-pubfn-vanishing
#   M2 (tree drift):      a #[test] deleted from a suite copy -> D3-test-count-regression
#
# Exit 0 = both flips witnessed; exit 1 = VACUOUS GATE or court failure.
set -euo pipefail
REPO="$(cd "$(dirname "$0")/.." && pwd)"
CHECKER="$REPO/scripts/sjira_control_plan.py"
BASELINE="$REPO/docs/jira/v26.11.3/baselines/sjira-control-baseline.json"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

fail() { echo "[COURT-FAIL] $*" >&2; exit 1; }

[ -f "$CHECKER" ] || fail "checker missing: $CHECKER"
[ -f "$BASELINE" ] || fail "baseline missing: $BASELINE"
[ -d "$REPO/crates/ggen-sjira" ] || fail "crate missing: $REPO/crates/ggen-sjira"

# ── M1: baseline tamper (pub fn count inflated above live census) ──────────
mkdir -p "$TMP/m1"
cp -R "$REPO/crates/ggen-sjira" "$TMP/m1/crates_ggen-sjira"
mkdir -p "$TMP/m1/crates"
mv "$TMP/m1/crates_ggen-sjira" "$TMP/m1/crates/ggen-sjira"
python3 - "$BASELINE" "$TMP/m1/baseline.json" <<'PY'
import json, sys
b = json.load(open(sys.argv[1]))
mod = next(m for m, n in b["pub_fns"].items() if n > 0)
b["pub_fns"][mod] = b["pub_fns"][mod] + 5
json.dump(b, open(sys.argv[2], "w"), indent=2, sort_keys=True)
PY
out1=$(python3 "$CHECKER" "$TMP/m1" "$TMP/m1/baseline.json") && \
    fail "M1 VACUOUS GATE: checker exited 0 on inflated baseline pub_fns: $out1"
echo "$out1" | grep -q "VIOLATION class=D4-pubfn-vanishing" || \
    fail "M1 flipped but without the typed D4 violation line: $out1"
echo "[FLIP] M1: tampered baseline -> exit 1 with D4-pubfn-vanishing"

# ── M2: tree drift (one #[test] removed from a suite copy) ─────────────────
mkdir -p "$TMP/m2/crates"
cp -R "$REPO/crates/ggen-sjira" "$TMP/m2/crates/ggen-sjira"
python3 - "$TMP/m2/crates/ggen-sjira" <<'PY'
import glob, sys
for f in glob.glob(sys.argv[1] + "/tests/*.rs"):
    src = open(f).read()
    if "#[test]" in src:
        open(f, "w").write(src.replace("#[test]", "", 1))
        print("mutated", f)
        break
else:
    sys.exit("no suite file with #[test] found")
PY
out2=$(python3 "$CHECKER" "$TMP/m2" "$BASELINE") && \
    fail "M2 VACUOUS GATE: checker exited 0 after a #[test] deletion: $out2"
echo "$out2" | grep -q "VIOLATION class=D3-test-count-regression" || \
    fail "M2 flipped but without the typed D3 violation line: $out2"
echo "[FLIP] M2: #[test] deletion -> exit 1 with D3-test-count-regression"

echo "sjira-control-court: PASS (2/2 flips witnessed, canonical tree untouched)"
