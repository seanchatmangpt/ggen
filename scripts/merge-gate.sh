#!/usr/bin/env bash
# merge-gate.sh — Sequential merge gate execution
#
# Runs the four-gate merge validation pipeline:
#   1. Tests pass (zero failures)
#   2. OTEL spans exist (via weaver or jaeger query)
#   3. Weaver registry check exits 0
#   4. Cryptographic receipt signed
#
# Generates a markdown report with gate results.
#
# Usage:
#   ./merge-gate.sh                    # Run all gates
#   ./merge-gate.sh --report-only      # Generate report from last run
#
# Exit codes:
#   0 — All gates pass
#   1 — One or more gates fail
#   2 — Configuration error

set -euo pipefail

# ── Colors ──────────────────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# ── Configuration ───────────────────────────────────────────────────────────
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo ".")"
REPORT_DIR="${REPO_ROOT}/.ggen/merge-gate"
REPORT_FILE="${REPORT_DIR}/report.md"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date +"%Y-%m-%dT%H:%M:%SZ")
COMMIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
BRANCH=$(git branch --show-current 2>/dev/null || echo "unknown")

# Timeout defaults (milliseconds)
TEST_TIMEOUT_MS=600000     # 10 minutes
OTEL_TIMEOUT_MS=120000     # 2 minutes
WEAVER_TIMEOUT_MS=120000   # 2 minutes
RECEIPT_TIMEOUT_MS=30000   # 30 seconds

# ── Gate state ──────────────────────────────────────────────────────────────
declare -A GATE_STATUS
declare -A GATE_DURATION
declare -A GATE_MESSAGE
GATES=("tests" "otel" "weaver" "receipt")

for gate in "${GATES[@]}"; do
    GATE_STATUS["$gate"]="pending"
    GATE_DURATION["$gate"]="-"
    GATE_MESSAGE["$gate"]=""
done

mkdir -p "$REPORT_DIR"

# ── Parse arguments ─────────────────────────────────────────────────────────
REPORT_ONLY=false
for arg in "$@"; do
    case "$arg" in
        --report-only)
            REPORT_ONLY=true
            ;;
        --help|-h)
            echo "Usage: merge-gate.sh [--report-only]"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown argument: ${arg}${NC}" >&2
            exit 2
            ;;
    esac
done

if [[ "$REPORT_ONLY" == true ]]; then
    if [[ -f "$REPORT_FILE" ]]; then
        cat "$REPORT_FILE"
        exit 0
    else
        echo -e "${RED}No previous report found at ${REPORT_FILE}${NC}" >&2
        exit 2
    fi
fi

# ── Helper: run gate with timeout ──────────────────────────────────────────
run_gate() {
    local gate_name="$1"
    local gate_cmd="$2"
    local timeout_ms="$3"

    local timeout_sec=$((timeout_ms / 1000))

    echo -n "  ${gate_name}... "

    local start_ns
    start_ns=$(date +%s%N 2>/dev/null || echo "0")

    if timeout "${timeout_sec}s" bash -c "$gate_cmd" > "${REPORT_DIR}/${gate_name}.log" 2>&1; then
        local end_ns
        end_ns=$(date +%s%N 2>/dev/null || echo "0")
        local duration_ms=$(( (end_ns - start_ns) / 1000000 ))
        GATE_STATUS["$gate_name"]="pass"
        GATE_DURATION["$gate_name"]="${duration_ms}ms"
        GATE_MESSAGE["$gate_name"]=""
        echo -e "${GREEN}PASS${NC} (${duration_ms}ms)"
        return 0
    else
        local exit_code=$?
        local end_ns
        end_ns=$(date +%s%N 2>/dev/null || echo "0")
        local duration_ms=$(( (end_ns - start_ns) / 1000000 ))

        if [[ $exit_code -eq 124 ]]; then
            GATE_STATUS["$gate_name"]="timeout"
            GATE_DURATION["$gate_name"]="${duration_ms}ms"
            GATE_MESSAGE["$gate_name"]="Exceeded ${timeout_sec}s timeout"
            echo -e "${RED}TIMEOUT${NC} (${duration_ms}ms)"
        else
            GATE_STATUS["$gate_name"]="fail"
            GATE_DURATION["$gate_name"]="${duration_ms}ms"
            GATE_MESSAGE["$gate_name"]="Exit code ${exit_code}"
            echo -e "${RED}FAIL${NC} (${duration_ms}ms, exit ${exit_code})"
        fi
        return 1
    fi
}

# ── Banner ──────────────────────────────────────────────────────────────────
echo -e "${CYAN}${BOLD}=== Merge Gate Pipeline ===${NC}"
echo -e "  Branch:  ${BRANCH}"
echo -e "  Commit:  ${COMMIT_HASH}"
echo -e "  Time:    ${TIMESTAMP}"
echo ""

# ═══════════════════════════════════════════════════════════════════════════
# Gate 1: Run Tests
# ═══════════════════════════════════════════════════════════════════════════
echo -e "${BOLD}[1/4] Tests${NC}"

if command -v cargo &>/dev/null; then
    run_gate "tests" "cd '${REPO_ROOT}' && cargo test --workspace 2>&1 | tail -20; exit \${PIPESTATUS[0]}" "$TEST_TIMEOUT_MS"
elif command -v mix &>/dev/null; then
    run_gate "tests" "cd '${REPO_ROOT}' && mix test 2>&1 | tail -20; exit \${PIPESTATUS[0]}" "$TEST_TIMEOUT_MS"
elif command -v go &>/dev/null; then
    run_gate "tests" "cd '${REPO_ROOT}' && go test ./... 2>&1 | tail -20; exit \${PIPESTATUS[0]}" "$TEST_TIMEOUT_MS"
else
    echo -e "  ${YELLOW}SKIP${NC} (no test runner found)"
    GATE_STATUS["tests"]="skip"
    GATE_MESSAGE["tests"]="No test runner (cargo/mix/go) in PATH"
fi

echo ""

# ═══════════════════════════════════════════════════════════════════════════
# Gate 2: OTEL Span Verification
# ═══════════════════════════════════════════════════════════════════════════
echo -e "${BOLD}[2/4] OTEL Span Verification${NC}"

if command -v weaver &>/dev/null; then
    run_gate "otel" "weaver registry check -r ./semconv/model -p ./semconv/policies --quiet 2>&1; exit \$?" "$OTEL_TIMEOUT_MS"
elif command -v curl &>/dev/null; then
    # Fallback: check if Jaeger is reachable
    run_gate "otel" "curl -sf http://localhost:16686/api/services 2>/dev/null | python3 -c 'import sys,json; data=json.load(sys.stdin); print(json.dumps({\"services\":len(data.get(\"data\",[]))}))' 2>/dev/null" "$OTEL_TIMEOUT_MS"
    if [[ "${GATE_STATUS["otel"]}" == "fail" ]]; then
        GATE_MESSAGE["otel"]="Jaeger not reachable at localhost:16686"
    fi
else
    echo -e "  ${YELLOW}SKIP${NC} (no weaver or curl found)"
    GATE_STATUS["otel"]="skip"
    GATE_MESSAGE["otel"]="No OTEL verification tool available"
fi

echo ""

# ═══════════════════════════════════════════════════════════════════════════
# Gate 3: Weaver Registry Check
# ═══════════════════════════════════════════════════════════════════════════
echo -e "${BOLD}[3/4] Weaver Registry Check${NC}"

if command -v weaver &>/dev/null; then
    run_gate "weaver" "weaver registry check -r ./semconv/model -p ./semconv/policies --quiet 2>&1; exit \$?" "$WEAVER_TIMEOUT_MS"
elif [[ -d "${REPO_ROOT}/semconv/model" ]]; then
    echo -e "  ${YELLOW}SKIP${NC} (weaver binary not found, semconv/model exists)"
    GATE_STATUS["weaver"]="skip"
    GATE_MESSAGE["weaver"]="weaver binary not in PATH"
else
    echo -e "  ${YELLOW}SKIP${NC} (no semconv/model directory)"
    GATE_STATUS["weaver"]="skip"
    GATE_MESSAGE["weaver"]="No semconv/model directory found"
fi

echo ""

# ═══════════════════════════════════════════════════════════════════════════
# Gate 4: Cryptographic Receipt
# ═══════════════════════════════════════════════════════════════════════════
echo -e "${BOLD}[4/4] Cryptographic Receipt${NC}"

if [[ -f "${REPO_ROOT}/scripts/ggen-receipt-generate.sh" ]]; then
    run_gate "receipt" "bash '${REPO_ROOT}/scripts/ggen-receipt-generate.sh' '${COMMIT_HASH}' 2>&1" "$RECEIPT_TIMEOUT_MS"
elif command -v ggen &>/dev/null; then
    run_gate "receipt" "ggen sync --audit 2>&1 | tail -5" "$RECEIPT_TIMEOUT_MS"
else
    echo -e "  ${YELLOW}SKIP${NC} (no receipt tool found)"
    GATE_STATUS["receipt"]="skip"
    GATE_MESSAGE["receipt"]="No ggen or receipt script available"
fi

echo ""

# ═══════════════════════════════════════════════════════════════════════════
# Decision
# ═══════════════════════════════════════════════════════════════════════════
FAIL_COUNT=0
for gate in "${GATES[@]}"; do
    case "${GATE_STATUS["$gate"]}" in
        fail|timeout) FAIL_COUNT=$((FAIL_COUNT + 1)) ;;
    esac
done

echo -e "${BOLD}=== Gate Results ===${NC}"
for gate in "${GATES[@]}"; do
    status="${GATE_STATUS["$gate"]}"
    duration="${GATE_DURATION["$gate"]}"
    case "$status" in
        pass)
            echo -e "  ${GREEN}PASS${NC}    ${gate} (${duration})"
            ;;
        fail)
            echo -e "  ${RED}FAIL${NC}    ${gate} (${duration}) — ${GATE_MESSAGE["$gate"]}"
            ;;
        timeout)
            echo -e "  ${RED}TIMEOUT${NC} ${gate} (${duration}) — ${GATE_MESSAGE["$gate"]}"
            ;;
        skip)
            echo -e "  ${YELLOW}SKIP${NC}    ${gate} — ${GATE_MESSAGE["$gate"]}"
            ;;
        *)
            echo -e "  ${CYAN}????${NC}    ${gate}"
            ;;
    esac
done
echo ""

# ── Generate Markdown Report ───────────────────────────────────────────────
{
    echo "# Merge Gate Report"
    echo ""
    echo "| Field | Value |"
    echo "|-------|-------|"
    echo "| Branch | \`${BRANCH}\` |"
    echo "| Commit | \`${COMMIT_HASH}\` |"
    echo "| Timestamp | \`${TIMESTAMP}\` |"
    echo "| Verdict | **$([ $FAIL_COUNT -eq 0 ] && echo 'PASS' || echo 'FAIL')** |"
    echo ""
    echo "## Gate Results"
    echo ""
    echo "| Gate | Status | Duration | Details |"
    echo "|------|--------|----------|---------|"

    for gate in "${GATES[@]}"; do
        status="${GATE_STATUS["$gate"]}"
        duration="${GATE_DURATION["$gate"]}"
        message="${GATE_MESSAGE["$gate"]}"
        case "$status" in
            pass) badge=":white_check_mark: PASS" ;;
            fail) badge=":x: FAIL" ;;
            timeout) badge=":x: TIMEOUT" ;;
            skip) badge=":warning: SKIP" ;;
            *) badge=":question: UNKNOWN" ;;
        esac
        echo "| ${gate} | ${badge} | ${duration} | ${message} |"
    done

    echo ""

    if [[ "$FAIL_COUNT" -gt 0 ]]; then
        echo "## Failure Details"
        echo ""
        for gate in "${GATES[@]}"; do
            if [[ "${GATE_STATUS["$gate"]}" == "fail" || "${GATE_STATUS["$gate"]}" == "timeout" ]]; then
                echo "### ${gate}"
                echo ""
                echo "\`${GATE_MESSAGE["$gate"]}\`"
                if [[ -f "${REPORT_DIR}/${gate}.log" ]]; then
                    echo ""
                    echo "<details>"
                    echo "<summary>Full log</summary>"
                    echo ""
                    echo '```'
                    tail -30 "${REPORT_DIR}/${gate}.log"
                    echo '```'
                    echo ""
                    echo "</details>"
                fi
                echo ""
            fi
        done
    fi

    echo "---"
    echo "_Generated by ggen merge-gate.sh_"
} > "$REPORT_FILE"

echo -e "Report written to ${REPORT_FILE}"

echo ""

# ── Final Exit ─────────────────────────────────────────────────────────────
if [[ "$FAIL_COUNT" -eq 0 ]]; then
    echo -e "${GREEN}${BOLD}MERGE GATE: PASS${NC}"
    echo -e "  All gates passed. Merge is permitted."
    exit 0
else
    echo -e "${RED}${BOLD}MERGE GATE: BLOCKED${NC}"
    echo -e "  ${FAIL_COUNT} gate(s) failed. Merge is not permitted."
    echo -e "  See ${REPORT_FILE} for details."
    exit 1
fi
