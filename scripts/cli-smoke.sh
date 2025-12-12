#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BIN="${GGEN_BIN:-"$ROOT/target/debug/ggen"}"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/ggen-cli-smoke.XXXXXX")"

cleanup() { rm -rf "$TMP"; }
trap cleanup EXIT

command -v jq >/dev/null 2>&1 || { echo "jq is required for cli-smoke"; exit 1; }

if [[ ! -x "$BIN" ]]; then
  echo "building ggen binary..."
  (cd "$ROOT" && cargo make build >/dev/null)
fi

run() {
  local msg="$1"; shift
  echo "[cli-smoke] $msg" >&2
  "$@"
}

export TZ=UTC
export LANG=C
export GGEN_SEED="${GGEN_SEED:-123}"

run "help"        "$BIN" --help >/dev/null
run "version"     "$BIN" --version >/dev/null

WORKFLOW_FILE="$TMP/.workflows/smoke.json"

run "init"        "$BIN" workflow init --name "smoke" --output_dir "$TMP" >/dev/null
run "event1"      "$BIN" workflow event --workflow_file "$WORKFLOW_FILE" --case_id "c1" --activity "Submitted" >/dev/null
run "event2"      "$BIN" workflow event --workflow_file "$WORKFLOW_FILE" --case_id "c1" --activity "Reviewed" >/dev/null
run "event3"      "$BIN" workflow event --workflow_file "$WORKFLOW_FILE" --case_id "c2" --activity "Submitted" >/dev/null

ANALYZE_JSON="$TMP/analyze.json"
run "analyze"     "$BIN" workflow analyze --workflow_file "$WORKFLOW_FILE" | tee "$ANALYZE_JSON" >/dev/null
jq -e '.workflow_name=="smoke" and .total_events==3' "$ANALYZE_JSON" >/dev/null

DISCOVER_JSON="$TMP/discover.json"
run "discover"    "$BIN" workflow discover --workflow_file "$WORKFLOW_FILE" | tee "$DISCOVER_JSON" >/dev/null
jq -e '.workflow_name=="smoke" and .total_edges>=1' "$DISCOVER_JSON" >/dev/null

REPORT_JSON="$TMP/report.json"
run "report"      "$BIN" workflow report --workflow_file "$WORKFLOW_FILE" --format json --output "$REPORT_JSON" >/dev/null
jq -e '.workflow=="smoke" and .analysis.total_events==3 and .discovery.total_edges>=1' "$REPORT_JSON" >/dev/null

# Idempotence: rerun analyze and ensure counts unchanged
run "analyze-repeat" "$BIN" workflow analyze --workflow_file "$WORKFLOW_FILE" | jq -e '.total_events==3' >/dev/null

# Failure path: missing workflow should fail
set +e
"$BIN" workflow analyze --workflow_file "$TMP/missing.json" >/dev/null 2>&1
status=$?
set -e
if [ "$status" -eq 0 ]; then
  echo "expected failure for missing workflow file"
  exit 1
fi

echo "[cli-smoke] PASS"

