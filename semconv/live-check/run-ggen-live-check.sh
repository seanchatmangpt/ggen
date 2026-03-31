#!/usr/bin/env bash
# ggen Weaver Live-Check Orchestration
#
# Starts weaver live-check receiver, runs ggen tests with OTLP export,
# emits synthetic spans, stops weaver, and validates coverage via ggen_gate.py.
#
# Usage:
#   ./run-ggen-live-check.sh [--ci]
#
# Exit codes:
#   0  PASS — tests passed and gate passed
#   1  FAIL — tests failed or gate failed

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
SEMCONV_MODEL="$REPO_ROOT/semconv/model"
POLICY_FILE="$REPO_ROOT/semconv/policies/ggen.rego"

WEAVER_PORT="${WEAVER_PORT:-4317}"
WEAVER_ADMIN_PORT="${WEAVER_ADMIN_PORT:-4320}"
WEAVER_TIMEOUT="${WEAVER_TIMEOUT:-60}"
REPORT_DIR="${REPORT_DIR:-/tmp/ggen-weaver-live-check}"
GGEN_LIB_TEST_CMD="${GGEN_LIB_TEST_CMD:-cargo test -p ggen-a2a-mcp --features otel --lib --test otel_trace_capture --test a2a_mcp_generate_e2e --test a2a_mcp_yawl_e2e -- --test-threads=1}"
GGEN_GROQ_TEST_CMD="${GGEN_GROQ_TEST_CMD:-cargo test -p ggen-a2a-mcp --features otel --test groq_slo_timing --test a2a_groq_integration -- --test-threads=1}"

WEAVER_ENDPOINT="http://localhost:${WEAVER_PORT}"
PID_FILE="$REPORT_DIR/weaver.pid"
LOG_FILE="$REPORT_DIR/weaver-live-check.log"

CI_FLAG=""
if [[ "${1:-}" == "--ci" ]]; then
  CI_FLAG="--ci"
fi

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------

cleanup() {
  if [[ -n "${WEAVER_PID:-}" ]] && kill -0 "$WEAVER_PID" 2>/dev/null; then
    echo "  Stopping weaver receiver (PID $WEAVER_PID)..."
    curl -s -X POST "http://localhost:${WEAVER_ADMIN_PORT}/stop" >/dev/null 2>&1 || true
    # Wait up to 5s for graceful shutdown
    local i=0
    while kill -0 "$WEAVER_PID" 2>/dev/null && [[ $i -lt 5 ]]; do
      sleep 1
      i=$((i + 1))
    done
    kill "$WEAVER_PID" 2>/dev/null || true
    wait "$WEAVER_PID" 2>/dev/null || true
  fi
}

trap cleanup EXIT INT TERM

# ---------------------------------------------------------------------------
# Preflight
# ---------------------------------------------------------------------------

echo "=== ggen Weaver Live-Check ==="
echo ""

if ! command -v weaver &>/dev/null; then
  echo "ERROR: weaver is not installed."
  echo "  Install: curl -LsSf https://github.com/open-telemetry/weaver/releases/latest/download/weaver-installer.sh | sh"
  exit 1
fi
echo "  Weaver    : $(weaver --version 2>&1 | head -1)"

if ! command -v python3 &>/dev/null; then
  echo "ERROR: python3 is not installed."
  exit 1
fi
echo "  Python    : $(python3 --version 2>&1)"

if [[ ! -d "$SEMCONV_MODEL" ]]; then
  echo "ERROR: semconv model not found at $SEMCONV_MODEL"
  exit 1
fi
echo "  Model     : $SEMCONV_MODEL"

if [[ -z "${GROQ_API_KEY:-}" ]]; then
  echo "ERROR: GROQ_API_KEY not set. Live-check requires real Groq API calls for OTEL span validation."
  echo "  Set GROQ_API_KEY=<your-key> to run live-check."
  exit 1
fi
echo "  GROQ_API_KEY: ${GROQ_API_KEY:0:8}... (set)"

if [[ ! -f "$POLICY_FILE" ]]; then
  echo "  WARN: Policy file not found at $POLICY_FILE — running without policies"
  POLICY_FLAG=""
else
  POLICY_FLAG="--advice-policies $POLICY_FILE"
  echo "  Policies  : $POLICY_FILE"
fi

if lsof -ti :"$WEAVER_PORT" &>/dev/null; then
  echo "  WARN: Port $WEAVER_PORT is already in use. Set WEAVER_PORT=<port> to override."
fi

echo "  Endpoint  : $WEAVER_ENDPOINT"
echo "  Timeout   : ${WEAVER_TIMEOUT}s"
echo "  Report dir: $REPORT_DIR"
echo ""

# ---------------------------------------------------------------------------
# Step 1: Start weaver live-check receiver
# ---------------------------------------------------------------------------

echo "Step 1: Starting weaver live-check receiver..."
mkdir -p "$REPORT_DIR"

weaver registry live-check \
  -r "$SEMCONV_MODEL" \
  $POLICY_FLAG \
  --otlp-grpc-port "$WEAVER_PORT" \
  --admin-port "$WEAVER_ADMIN_PORT" \
  --inactivity-timeout "$WEAVER_TIMEOUT" \
  --output "$REPORT_DIR" \
  --format json \
  --no-stream \
  >"$LOG_FILE" 2>&1 &

WEAVER_PID=$!
echo "$WEAVER_PID" >"$PID_FILE"
sleep 2

if ! kill -0 "$WEAVER_PID" 2>/dev/null; then
  echo "ERROR: weaver failed to start. Check $LOG_FILE"
  cat "$LOG_FILE"
  exit 1
fi
echo "  Weaver PID: $WEAVER_PID"

# ---------------------------------------------------------------------------
# Step 2a: Run lib + integration tests with OTEL export
# ---------------------------------------------------------------------------

echo ""
echo "Step 2a: Running lib + integration tests with OTEL export..."
GTEST_EXIT=0
(
  cd "$REPO_ROOT"
  export OTEL_EXPORTER_OTLP_ENDPOINT="$WEAVER_ENDPOINT"
  export RUST_LOG=info
  $GGEN_LIB_TEST_CMD
) || GTEST_EXIT=$?
echo "  Lib test exit code: $GTEST_EXIT"

# ---------------------------------------------------------------------------
# Step 2b: Run real Groq integration tests with OTEL export
# ---------------------------------------------------------------------------

echo ""
echo "Step 2b: Running real Groq integration tests with OTEL export..."
GROQ_EXIT=0
(
  cd "$REPO_ROOT"
  export GROQ_API_KEY="$GROQ_API_KEY"
  export OTEL_EXPORTER_OTLP_ENDPOINT="$WEAVER_ENDPOINT"
  export RUST_LOG=info
  $GGEN_GROQ_TEST_CMD
) || GROQ_EXIT=$?
echo "  Groq test exit code: $GROQ_EXIT"

# ---------------------------------------------------------------------------
# Step 3: Wait for OTEL span flush, then stop weaver
# ---------------------------------------------------------------------------

echo ""
echo "Step 3: Waiting for OTEL batch exporter flush (6s)..."
sleep 6
# Step 4: Stop weaver receiver
# ---------------------------------------------------------------------------

echo ""
echo "Step 4: Stopping weaver receiver..."
curl -s -X POST "http://localhost:${WEAVER_ADMIN_PORT}/stop" >/dev/null 2>&1 || true
wait "$WEAVER_PID" 2>/dev/null || true
WEAVER_PID=""
rm -f "$PID_FILE"
echo "  Weaver stopped"

# ---------------------------------------------------------------------------
# Step 5: Locate report
# ---------------------------------------------------------------------------

echo ""
echo "Step 5: Locating coverage report..."
REPORT_FILE=""
if [[ -f "$REPORT_DIR/live_check.json" ]]; then
  REPORT_FILE="$REPORT_DIR/live_check.json"
else
  for f in "$REPORT_DIR"/*.json; do
    [[ -f "$f" ]] && REPORT_FILE="$f" && break
  done
fi

if [[ -z "$REPORT_FILE" ]]; then
  echo "ERROR: No JSON report generated in $REPORT_DIR"
  echo "  Check log: $LOG_FILE"
  exit 1
fi
echo "  Report: $REPORT_FILE"

# ---------------------------------------------------------------------------
# Step 6: Run gate
# ---------------------------------------------------------------------------

echo ""
echo "Step 6: Running ggen_gate.py..."
GATE_EXIT=0
python3 "$SCRIPT_DIR/ggen_gate.py" \
  "$REPORT_FILE" "$SEMCONV_MODEL" \
  $CI_FLAG \
  --evidence-dir "$REPORT_DIR" \
  || GATE_EXIT=$?

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo "=== Results ==="
echo "  Lib tests   : $([ $GTEST_EXIT -eq 0 ] && echo 'PASS' || echo 'FAIL (exit '$GTEST_EXIT')')"
echo "  Groq tests  : $([ $GROQ_EXIT -eq 0 ] && echo 'PASS' || echo 'FAIL (exit '$GROQ_EXIT')')"
echo "  Gate        : $([ $GATE_EXIT -eq 0 ] && echo 'PASS' || echo 'FAIL (exit '$GATE_EXIT')')"
echo "  Report      : $REPORT_FILE"
echo "  Log         : $LOG_FILE"

if [[ $GTEST_EXIT -ne 0 ]]; then
  echo ""
  echo "FAIL: Lib tests did not pass"
  exit 1
fi

if [[ $GROQ_EXIT -ne 0 ]]; then
  echo ""
  echo "FAIL: Groq integration tests did not pass"
  exit 1
fi

if [[ $GATE_EXIT -ne 0 ]]; then
  echo ""
  echo "FAIL: Gate validation failed"
  exit 1
fi

echo ""
echo "PASS: All checks passed"
exit 0
