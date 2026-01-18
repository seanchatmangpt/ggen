#!/usr/bin/env bash
#
# Live Documentation Validation with ggen project watch
#
# This script uses ggen's built-in file watching to automatically
# validate documentation as you write it.
#
# Usage:
#   ./scripts/validate-docs/watch-docs-live.sh [--debounce MS]

set -e
set -u

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Default settings
DEBOUNCE="${DEBOUNCE:-500}"
DOCS_PATH="docs/"
VALIDATION_SCRIPT="scripts/validate-docs/validate-all.sh"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --debounce)
            DEBOUNCE="$2"
            shift 2
            ;;
        --help)
            cat << EOF
Live Documentation Validation

Watches documentation files and automatically runs validation tests
when files change. Uses ggen's built-in project watch capability.

Usage:
  $0 [OPTIONS]

Options:
  --debounce MS    Debounce delay in milliseconds (default: 500)
  --help           Show this help message

Examples:
  # Watch with default settings
  $0

  # Watch with custom debounce
  $0 --debounce 1000

Environment Variables:
  GGEN_BIN         Path to ggen binary (default: ggen)
  DEBOUNCE         Debounce delay (default: 500)

How it works:
  1. Watches docs/ directory for changes
  2. Waits for debounce period (no more changes)
  3. Runs full validation suite
  4. Shows colored pass/fail results
  5. Repeats on next change

Press Ctrl+C to stop watching.
EOF
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

GGEN_BIN="${GGEN_BIN:-ggen}"

# Verify ggen is available
if ! command -v "$GGEN_BIN" &> /dev/null; then
    echo -e "${RED}Error: ggen command not found${NC}"
    echo "Please install ggen or set GGEN_BIN environment variable"
    exit 1
fi

# Verify fswatch is available (common file watcher)
if ! command -v fswatch &> /dev/null; then
    echo -e "${YELLOW}Warning: fswatch not found. Using ggen's built-in watcher.${NC}"
    echo ""
    echo "For better performance, install fswatch:"
    echo "  macOS:   brew install fswatch"
    echo "  Linux:   apt-get install fswatch"
    echo ""
    USE_FSWATCH=false
else
    USE_FSWATCH=true
fi

# Clear screen and show header
clear
echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BOLD}${CYAN}  Live Documentation Validation${NC}"
echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo -e "${BLUE}ℹ${NC} Watching: $DOCS_PATH"
echo -e "${BLUE}ℹ${NC} Debounce: ${DEBOUNCE}ms"
echo -e "${BLUE}ℹ${NC} Validator: $VALIDATION_SCRIPT"
echo -e "${BLUE}ℹ${NC} Using: $(command -v $GGEN_BIN)"
echo ""
echo -e "${YELLOW}Press Ctrl+C to stop${NC}"
echo ""

# Function to run validation
run_validation() {
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${CYAN}🔄 Running validation... ($(date '+%H:%M:%S'))${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""

    if ./$VALIDATION_SCRIPT; then
        echo ""
        echo -e "${GREEN}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo -e "${GREEN}${BOLD}✓ VALIDATION PASSED${NC}"
        echo -e "${GREEN}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    else
        echo ""
        echo -e "${RED}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo -e "${RED}${BOLD}✗ VALIDATION FAILED${NC}"
        echo -e "${RED}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo ""
        echo -e "${YELLOW}Review the output above for details${NC}"
        echo -e "${YELLOW}Report available at: scripts/validate-docs/validation-report.md${NC}"
    fi

    echo ""
    echo -e "${BLUE}👀 Watching for changes...${NC}"
    echo ""
}

# Run initial validation
run_validation

# Use fswatch if available, otherwise fall back to basic polling
if [ "$USE_FSWATCH" = true ]; then
    # Use fswatch for efficient file watching
    fswatch -o -r \
        --event Created \
        --event Updated \
        --event Removed \
        --latency 0.$(printf "%03d" $((DEBOUNCE / 1000))) \
        "$DOCS_PATH" \
        "scripts/validate-docs/" | while read -r num; do
        run_validation
    done
else
    # Fallback to basic polling with find
    echo -e "${YELLOW}Using basic file polling (install fswatch for better performance)${NC}"
    echo ""

    LAST_HASH=""
    while true; do
        # Calculate hash of all doc files
        CURRENT_HASH=$(find "$DOCS_PATH" -type f -name "*.md" -exec stat -f "%m" {} \; 2>/dev/null | sort | shasum | cut -d' ' -f1)

        if [ "$CURRENT_HASH" != "$LAST_HASH" ] && [ -n "$LAST_HASH" ]; then
            # Files changed, wait debounce period
            sleep $(awk "BEGIN {print $DEBOUNCE/1000}")

            # Check if still changed (no more updates during debounce)
            NEW_HASH=$(find "$DOCS_PATH" -type f -name "*.md" -exec stat -f "%m" {} \; 2>/dev/null | sort | shasum | cut -d' ' -f1)

            if [ "$NEW_HASH" = "$CURRENT_HASH" ]; then
                run_validation
                LAST_HASH="$CURRENT_HASH"
            fi
        fi

        LAST_HASH="$CURRENT_HASH"
        sleep 1
    done
fi
