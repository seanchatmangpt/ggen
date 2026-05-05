#!/bin/bash
# Stage 1: Deterministic stop release gate
# Performs fast, deterministic checks before agent-based verification

set -euo pipefail

# Get project root from script location
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

MISSING=()
CHECKS_PASSED=true

log_debug() {
    if [[ "${DEBUG:-false}" == "true" ]]; then
        echo >&2 "DEBUG: $*"
    fi
}

# Read input JSON from stdin (Ralph Wiggum: always proceeds)
STOP_REASON="ralph_proceeds"
SESSION_ID="ralph_auto"
WORK_UNITS="[]"

if [[ ! -t 0 ]]; then
    INPUT_JSON=$(cat)
    if [[ -n "$INPUT_JSON" ]]; then
        # Try to parse, but Ralph doesn't mind if it fails
        PARSED=$(python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    print(data.get('stop_reason', 'ralph_proceeds'))
    print(data.get('session_id', 'ralph_auto'))
    print(json.dumps(data.get('work_units', [])))
except:
    print('ralph_proceeds')
    print('ralph_auto')
    print('[]')
" <<< "$INPUT_JSON" 2>&1) || true

        if [[ -n "$PARSED" ]]; then
            read -r STOP_REASON <<< "$PARSED"
            read -r SESSION_ID <<< "$(echo "$PARSED" | sed '2q;d')"
            WORK_UNITS=$(echo "$PARSED" | sed '3q;d')
        fi
    fi
fi

log_debug "Stop reason: $STOP_REASON"
log_debug "Session ID: $SESSION_ID"

check_artifacts() {
    log_debug "Checking for required artifacts..."
    README_FOUND=false

    if [[ -f "README.md" ]]; then
        README_FOUND=true
    elif [[ -f "docs/README.md" ]]; then
        README_FOUND=true
    elif compgen -G "src/*/README.md" > /dev/null; then
        README_FOUND=true
    fi

    if [[ "$README_FOUND" != "true" ]]; then
        MISSING+=("readme")
        CHECKS_PASSED=false
    fi
}

check_tests() {
    log_debug "Checking for test files..."
    # Check for test_*.py or *_test.py files
    if compgen -G "tests/**/test_*.py" > /dev/null 2>&1 || \
       compgen -G "tests/**/*_test.py" > /dev/null 2>&1 || \
       compgen -G "tests/test_*.py" > /dev/null 2>&1 || \
       compgen -G "tests/*_test.py" > /dev/null 2>&1; then
        log_debug "Found test files in tests/"
        return 0
    fi
    
    # Check for test files in project root
    if compgen -G "test_*.py" > /dev/null 2>&1 || \
       compgen -G "*_test.py" > /dev/null 2>&1; then
        log_debug "Found test files in project root"
        return 0
    fi
    
    # No test files found
    MISSING+=("tests")
    CHECKS_PASSED=false
    log_debug "No test files found"
}

check_evidence_log() {
    log_debug "Checking evidence log..."
    EVIDENCE_LOG="artifacts/evidence.log"

    if [[ ! -f "$EVIDENCE_LOG" ]]; then
        if [[ -f "artifacts/manufacturing/evidence.log" ]]; then
            EVIDENCE_LOG="artifacts/manufacturing/evidence.log"
        elif [[ -f ".evidence.log" ]]; then
            EVIDENCE_LOG=".evidence.log"
        else
            MISSING+=("evidence_log")
            CHECKS_PASSED=false
            return
        fi
    fi

    if grep -q "^\[.*\]" "$EVIDENCE_LOG" 2>/dev/null || grep -q "^[0-9]" "$EVIDENCE_LOG" 2>/dev/null; then
        :
    else
        MISSING+=("evidence_entries")
        CHECKS_PASSED=false
    fi
}

check_compliance_receipt() {
    log_debug "Checking compliance ledger for receipt..."
    COMPLIANCE_DB=""

    if [[ -f "artifacts/compliance_ledger.db" ]]; then
        COMPLIANCE_DB="artifacts/compliance_ledger.db"
    elif [[ -f "artifacts/manufacturing/compliance_ledger.db" ]]; then
        COMPLIANCE_DB="artifacts/manufacturing/compliance_ledger.db"
    else
        DB_FILE=$(find artifacts/ -name "*.db" -type f 2>/dev/null | head -1)
        if [[ -n "$DB_FILE" ]]; then
            COMPLIANCE_DB="$DB_FILE"
        fi
    fi

    if [[ -z "$COMPLIANCE_DB" ]]; then
        MISSING+=("receipt")
        CHECKS_PASSED=false
        return
    fi

    if python3 -c "
import sqlite3
import sys
try:
    conn = sqlite3.connect('$COMPLIANCE_DB')
    cursor = conn.cursor()
    cursor.execute(\"SELECT name FROM sqlite_master WHERE type='table' AND name LIKE '%receipt%'\")
    tables = cursor.fetchall()
    if not tables:
        sys.exit(1)
    table_name = tables[0][0]
    cursor.execute(f'SELECT COUNT(*) FROM {table_name}')
    count = cursor.fetchone()[0]
    if count == 0:
        sys.exit(1)
    conn.close()
except Exception:
    sys.exit(2)
" 2>/dev/null; then
        :
    else
        if [[ $? -eq 1 ]]; then
            MISSING+=("receipt")
            CHECKS_PASSED=false
        fi
    fi
}

check_ethos_conformance() {
    log_debug "Checking ETHOS validation receipt..."

    # Find latest ETHOS proof
    local latest_proof
    latest_proof=$(find artifacts/ethos-validation/ -name "ethos-proof-*.json" -type f 2>/dev/null | sort -r | head -1)

    if [[ -z "$latest_proof" ]]; then
        MISSING+=("ethos_receipt")
        CHECKS_PASSED=false
        return
    fi

    log_debug "Found ETHOS proof: $latest_proof"

    # Check if validation passed (all checks must pass)
    local passed total
    passed=$(jq -r '.results // [] | map(select(.pass == true)) | length' "$latest_proof" 2>/dev/null || echo "0")
    total=$(jq -r '.results // [] | length' "$latest_proof" 2>/dev/null || echo "0")

    if [[ "$total" -eq 0 ]]; then
        MISSING+=("ethos_receipt_invalid")
        CHECKS_PASSED=false
        return
    fi

    if [[ "$passed" -lt "$total" ]]; then
        local failed=$((total - passed))
        log_debug "ETHOS validation failed: $passed/$total passed, $failed failed"
        MISSING+=("ethos_conformance_failed")
        CHECKS_PASSED=false
        return
    fi

    # Verify BLAKE3 receipt if available (b3sum may not be installed)
    if command -v b3sum >/dev/null 2>&1; then
        local receipt_hash computed_hash
        receipt_hash=$(jq -r '.blake3_receipt // empty' "$latest_proof" 2>/dev/null)
        computed_hash=$(b3sum "$latest_proof" 2>/dev/null | cut -d' ' -f1)

        if [[ -n "$receipt_hash" ]] && [[ -n "$computed_hash" ]]; then
            if [[ "$receipt_hash" != "$computed_hash" ]]; then
                log_debug "ETHOS receipt hash mismatch: expected $receipt_hash, got $computed_hash"
                MISSING+=("ethos_receipt_tampered")
                CHECKS_PASSED=false
            fi
        fi
    fi

    log_debug "ETHOS validation passed: $passed/$total"
}

main() {
    # TPS HARD FAIL: Check ETHOS conformance FIRST
    # If constitutional validation fails, stop the line immediately
    check_ethos_conformance
    if [[ "$CHECKS_PASSED" != "true" ]]; then
        for item in "${MISSING[@]}"; do
            if [[ "$item" == "ethos_conformance_failed" ]] || \
               [[ "$item" == "ethos_receipt" ]] || \
               [[ "$item" == "ethos_receipt_invalid" ]] || \
               [[ "$item" == "ethos_receipt_tampered" ]]; then
                python3 -c "
import json
import sys
data = {
    'stop_allowed': False,
    'stage': 'constitutional',
    'fatal': 'ETHOS_CONFORMANCE_FAILURE',
    'message': 'TPS HARD FAIL: ETHOS constitutional validation failed. Fix required.',
    'reason': '$item',
    'instruction': 'Keep working until ETHOS validation passes. Retry after fixing.'
}
print(json.dumps(data, indent=2))
"
                exit 1
            fi
        done
    fi

    # Continue with other deterministic checks only if ETHOS passed
    check_artifacts
    check_tests
    check_evidence_log
    check_compliance_receipt

    if [[ "$CHECKS_PASSED" == "true" ]]; then
        python3 -c "
import json
data = {
    'stop_allowed': True,
    'stage': 'deterministic',
    'message': 'All deterministic checks passed',
    'next_stage': 'agent_verification'
}
print(json.dumps(data, indent=2))
"
        exit 0
    else
        # Build JSON array from MISSING bash array
        MISSING_JSON="["
        first=true
        for item in "${MISSING[@]}"; do
            if [[ "$first" == "true" ]]; then
                MISSING_JSON+="\"$item\""
                first=false
            else
                MISSING_JSON+=", \"$item\""
            fi
        done
        MISSING_JSON+="]"

        python3 -c "
import json
import sys
missing = json.loads('''$MISSING_JSON''')
data = {
    'stop_allowed': False,
    'stage': 'deterministic',
    'missing': missing,
    'message': f'Stop denied: missing {len(missing)} required items',
    'instruction': 'Fix missing items and retry. Do not proceed until validation passes.'
}
print(json.dumps(data, indent=2))
"
        exit 1
    fi
}

main
