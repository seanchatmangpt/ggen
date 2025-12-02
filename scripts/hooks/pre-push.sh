#!/usr/bin/env bash
# pre-push.sh - Full Tier Git Hook
# DfLSS: Comprehensive validation before sharing code
# 80/20: Catch remaining 38% of defects in <60 seconds

set -e

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/hooks-common.sh"

# ============================================================
# CONFIGURATION
# ============================================================

# Pre-push specific timeouts (full tier)
TIMEOUT_CHECK=15    # 15 seconds for cargo check
TIMEOUT_LINT=60     # 60 seconds for clippy
TIMEOUT_FORMAT=10   # 10 seconds for format check
TIMEOUT_TEST=120    # 120 seconds for unit tests
TIMEOUT_AUDIT=30    # 30 seconds for security audit

# Counters for summary
PASSED=0
WARNED=0
FAILED=0

# ============================================================
# MAIN EXECUTION
# ============================================================

main() {
    echo ""
    echo -e "${BOLD}Pre-Push Validation (Full Tier)${NC}"
    echo -e "Target: <60 seconds | Value: 100% of defects"
    echo ""

    # Initialize environment
    if ! init_hook_env; then
        exit 1
    fi

    # ========================================================
    # Gate 1: Cargo Check (CRITICAL - 60% value)
    # ========================================================
    echo "Gate 1/5: Cargo Check..."
    if run_gate "Cargo Check" "cargo make check-pre-push" "$TIMEOUT_CHECK" 0 "$ANDON_RED"; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        andon_summary $PASSED $WARNED $FAILED
        exit 1
    fi

    # ========================================================
    # Gate 2: Clippy Lint (HIGH - 15% value)
    # ========================================================
    echo "Gate 2/5: Clippy Lint..."
    if run_gate "Clippy Lint" "cargo make lint" "$TIMEOUT_LINT" 1 "$ANDON_RED"; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        andon_summary $PASSED $WARNED $FAILED
        exit 1
    fi

    # ========================================================
    # Gate 3: Format Check (LOW - 2% value, auto-fixable)
    # ========================================================
    echo "Gate 3/5: Format Check..."
    if run_gate "Format Check" "cargo fmt --all -- --check" "$TIMEOUT_FORMAT" 0 "$ANDON_YELLOW"; then
        PASSED=$((PASSED + 1))
    else
        # Attempt auto-fix
        if autofix_format; then
            # Re-check after fix
            if run_gate "Format Recheck" "cargo fmt --all -- --check" "$TIMEOUT_FORMAT" 0 "$ANDON_RED"; then
                WARNED=$((WARNED + 1))
                andon_signal "$ANDON_YELLOW" "Format" "Auto-fixed. Verify changes."
            else
                FAILED=$((FAILED + 1))
                andon_summary $PASSED $WARNED $FAILED
                exit 1
            fi
        else
            FAILED=$((FAILED + 1))
            andon_summary $PASSED $WARNED $FAILED
            exit 1
        fi
    fi

    # ========================================================
    # Gate 4: Unit Tests (CRITICAL - 20% value)
    # ========================================================
    echo "Gate 4/5: Unit Tests..."
    if run_gate "Unit Tests" "cargo make test-unit" "$TIMEOUT_TEST" 0 "$ANDON_RED"; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        andon_summary $PASSED $WARNED $FAILED
        exit 1
    fi

    # ========================================================
    # Gate 5: Security Audit (MEDIUM - 3% value, warning only)
    # ========================================================
    echo "Gate 5/5: Security Audit..."
    if run_gate "Security Audit" "cargo make audit" "$TIMEOUT_AUDIT" 0 "$ANDON_YELLOW"; then
        PASSED=$((PASSED + 1))
    else
        # Non-blocking - just warn
        WARNED=$((WARNED + 1))
        andon_signal "$ANDON_YELLOW" "Security Audit" "Vulnerabilities found. Review with: cargo make audit"
    fi

    # ========================================================
    # Summary
    # ========================================================
    echo ""
    if andon_summary $PASSED $WARNED $FAILED; then
        echo -e "${GREEN}Pre-push passed. Push will proceed.${NC}"
        exit 0
    else
        echo -e "${RED}Pre-push failed. Fix issues before pushing.${NC}"
        exit 1
    fi
}

# Run main
main "$@"
