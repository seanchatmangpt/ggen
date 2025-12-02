#!/usr/bin/env bash
# pre-commit.sh - Fast Tier Git Hook
# DfLSS: Prevent defects early with fast feedback
# 80/20: Focus on 62% value in <5 seconds

set -e

# Source common library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/hooks-common.sh"

# ============================================================
# CONFIGURATION
# ============================================================

# Pre-commit specific timeouts (fast tier)
TIMEOUT_CHECK=5    # 5 seconds for cargo check
TIMEOUT_FORMAT=2   # 2 seconds for format check

# Counters for summary
PASSED=0
WARNED=0
FAILED=0

# ============================================================
# MAIN EXECUTION
# ============================================================

main() {
    echo ""
    echo -e "${BOLD}Pre-Commit Validation (Fast Tier)${NC}"
    echo -e "Target: <5 seconds | Value: 62% of defects"
    echo ""

    # Initialize environment
    if ! init_hook_env; then
        exit 1
    fi

    # ========================================================
    # Gate 1: Cargo Check (CRITICAL - 60% value)
    # ========================================================
    echo "Gate 1/2: Cargo Check..."
    if run_gate "Cargo Check" "cargo make check" "$TIMEOUT_CHECK" 0 "$ANDON_RED"; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        # Critical failure - stop immediately
        andon_summary $PASSED $WARNED $FAILED
        exit 1
    fi

    # ========================================================
    # Gate 2: Format Check (LOW - 2% value, auto-fixable)
    # ========================================================
    echo "Gate 2/2: Format Check..."
    if run_gate "Format Check" "cargo fmt --all -- --check" "$TIMEOUT_FORMAT" 0 "$ANDON_YELLOW"; then
        PASSED=$((PASSED + 1))
    else
        # Auto-fix formatting
        WARNED=$((WARNED + 1))
        if autofix_format; then
            andon_signal "$ANDON_YELLOW" "Format" "Code auto-formatted. Review changes before commit."
        else
            # Format fix failed - make it a failure
            FAILED=$((FAILED + 1))
            andon_summary $PASSED $WARNED $FAILED
            exit 1
        fi
    fi

    # ========================================================
    # Summary
    # ========================================================
    echo ""
    if andon_summary $PASSED $WARNED $FAILED; then
        echo -e "${GREEN}Pre-commit passed. Commit will proceed.${NC}"
        exit 0
    else
        echo -e "${RED}Pre-commit failed. Fix issues before committing.${NC}"
        exit 1
    fi
}

# Run main
main "$@"
