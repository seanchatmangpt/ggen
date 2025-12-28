#!/bin/bash
# Post-Bash Validation Hook
# Runs after bash commands complete
# Purpose: Validate command success, check for test failures, detect andon signals

set -e

# Read stdin for command result
COMMAND_RESULT=$(cat)

# Extract exit code and output
EXIT_CODE=$(echo "$COMMAND_RESULT" | jq -r '.exit_code' 2>/dev/null || echo "0")
COMMAND=$(echo "$COMMAND_RESULT" | jq -r '.command' 2>/dev/null || echo "unknown")
STDOUT=$(echo "$COMMAND_RESULT" | jq -r '.stdout' 2>/dev/null || echo "")
STDERR=$(echo "$COMMAND_RESULT" | jq -r '.stderr' 2>/dev/null || echo "")

# Color codes
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

# Check for andon signals (RED)
if [[ "$STDERR" == *"error:"* ]] || [[ "$EXIT_CODE" != "0" ]]; then
    if [[ "$COMMAND" == *"cargo make check"* ]]; then
        echo -e "${RED}[ANDON-RED] Compilation error detected${NC}"
        exit 2  # Block continuation
    fi

    if [[ "$COMMAND" == *"cargo make lint"* ]]; then
        echo -e "${RED}[ANDON-RED] Clippy warnings detected${NC}"
        exit 2  # Block continuation
    fi

    if [[ "$COMMAND" == *"cargo test"* ]]; then
        echo -e "${RED}[ANDON-RED] Test failure detected${NC}"
        # Extract failed test names
        FAILED=$(echo "$STDERR" | grep "test.*FAILED" | head -3)
        if [ -n "$FAILED" ]; then
            echo "Failed tests:"
            echo "$FAILED"
        fi
        exit 2  # Block continuation
    fi
fi

# Check for timeout signals (YELLOW)
if [[ "$STDOUT" == *"Timeout"* ]] || [[ "$STDERR" == *"timeout"* ]]; then
    echo -e "${YELLOW}[ANDON-YELLOW] Timeout detected - possible lock contention${NC}"
fi

# Check for performance regressions (YELLOW)
if [[ "$COMMAND" == *"cargo make bench"* ]]; then
    # Check for > 10% regression
    REGRESSION=$(echo "$STDOUT" | grep -i "regress" || true)
    if [ -n "$REGRESSION" ]; then
        echo -e "${YELLOW}[ANDON-YELLOW] Performance regression detected${NC}"
        echo "$REGRESSION"
    fi
fi

# Success case
if [[ "$EXIT_CODE" == "0" ]]; then
    if [[ "$COMMAND" == *"cargo make check"* ]]; then
        echo -e "${GREEN}[ANDON-GREEN] Compilation successful${NC}"
    fi

    if [[ "$COMMAND" == *"cargo make test"* ]]; then
        TEST_COUNT=$(echo "$STDOUT" | grep -o "test result:" || echo "0")
        echo -e "${GREEN}[ANDON-GREEN] All tests passed${NC}"
    fi

    if [[ "$COMMAND" == *"cargo make lint"* ]]; then
        echo -e "${GREEN}[ANDON-GREEN] Linter passed (zero warnings)${NC}"
    fi
fi

exit 0
