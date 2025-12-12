#!/bin/bash
# CLI Command Verification Script
# Part of Andon Signal Validation Framework
#
# Purpose: Verify all CLI commands work end-to-end
# When: Pre-commit, CI/CD, manual validation
# SLO: <30s execution time
#
# Andon Signals:
# - RED: Any command fails
# - YELLOW: Commands work but with warnings
# - GREEN: All commands pass

set -euo pipefail

# Colors for Andon signals
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Track results
FAILED=0
WARNINGS=0
PASSED=0

# Helper function to verify a command
verify_command() {
    local cmd="$1"
    local description="$2"
    local expected_output="${3:-}"
    
    echo -n "Verifying: $description... "
    
    # Execute command and capture output
    if output=$(eval "$cmd" 2>&1); then
        # Check for expected output if provided
        if [ -n "$expected_output" ]; then
            if echo "$output" | grep -q "$expected_output"; then
                echo -e "${GREEN}✓ PASSED${NC}"
                ((PASSED++))
                return 0
            else
                echo -e "${YELLOW}⚠ WARNING${NC} (output mismatch)"
                echo "  Expected: $expected_output"
                echo "  Got: $(echo "$output" | head -1)"
                ((WARNINGS++))
                return 0
            fi
        else
            echo -e "${GREEN}✓ PASSED${NC}"
            ((PASSED++))
            return 0
        fi
    else
        echo -e "${RED}✗ FAILED${NC}"
        echo "  Command: $cmd"
        echo "  Error: $output"
        ((FAILED++))
        return 1
    fi
}

# Helper function to verify file creation
verify_file_created() {
    local file="$1"
    local description="$2"
    
    echo -n "Verifying: $description... "
    
    if [ -f "$file" ]; then
        echo -e "${GREEN}✓ PASSED${NC}"
        ((PASSED++))
        return 0
    else
        echo -e "${RED}✗ FAILED${NC}"
        echo "  File not found: $file"
        ((FAILED++))
        return 1
    fi
}

echo "=========================================="
echo "CLI Command Verification"
echo "=========================================="
echo ""

# Find ggen binary
GGEN_BIN=""
if [ -f "target/release/ggen" ]; then
    GGEN_BIN="target/release/ggen"
elif [ -f "target/debug/ggen" ]; then
    GGEN_BIN="target/debug/ggen"
elif command -v ggen &> /dev/null; then
    GGEN_BIN="ggen"
else
    echo -e "${RED}Error: ggen binary not found${NC}"
    echo "  Build ggen first: cargo make build-release"
    exit 1
fi

echo "Using ggen binary: $GGEN_BIN"
echo ""

# Test directory for file creation tests
TEST_DIR=$(mktemp -d)
trap "rm -rf $TEST_DIR" EXIT

# Get absolute path to ggen binary
if [ "${GGEN_BIN:0:1}" != "/" ]; then
    # Relative path - make absolute
    GGEN_BIN="$(cd "$(dirname "$0")/.." && pwd)/$GGEN_BIN"
fi

# Test directory for file creation tests
TEST_DIR=$(mktemp -d)
trap "rm -rf $TEST_DIR" EXIT

cd "$TEST_DIR"

# 1. Version command
verify_command "$GGEN_BIN --version" "Version command" ""

# 2. Help command
verify_command "$GGEN_BIN --help" "Help command" "Usage:"

# 3. CI Workflow command
verify_command "$GGEN_BIN ci workflow --name test-workflow" "CI workflow generation" "Generated"
verify_file_created ".github/workflows/test-workflow.yml" "CI workflow file creation"

# 4. Workflow init command
verify_command "$GGEN_BIN workflow init --name test-workflow" "Workflow init" "initialized"
verify_file_created ".workflows/test-workflow.json" "Workflow file creation"

# 5. Paper new command
verify_command "$GGEN_BIN paper new --name 'Test Paper'" "Paper creation" "paper_name"
verify_file_created "Test Paper/Test Paper.rdf" "Paper RDF file creation"

# 6. Template show command (if template exists)
if [ -f "../../templates/hello.tmpl" ]; then
    verify_command "$GGEN_BIN template show --template hello.tmpl" "Template show" "template"
fi

# 7. Graph query command (basic validation)
verify_command "$GGEN_BIN graph query --sparql_query 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 1'" "Graph query" ""

# Summary
echo ""
echo "=========================================="
echo "Verification Summary"
echo "=========================================="
echo -e "${GREEN}Passed: $PASSED${NC}"
if [ $WARNINGS -gt 0 ]; then
    echo -e "${YELLOW}Warnings: $WARNINGS${NC}"
fi
if [ $FAILED -gt 0 ]; then
    echo -e "${RED}Failed: $FAILED${NC}"
fi
echo ""

# Andon Signal
if [ $FAILED -gt 0 ]; then
    echo -e "${RED}🚨 ANDON SIGNAL: RED${NC}"
    echo "Critical failures detected. Stop the line and fix issues."
    exit 1
elif [ $WARNINGS -gt 0 ]; then
    echo -e "${YELLOW}⚠️  ANDON SIGNAL: YELLOW${NC}"
    echo "Warnings detected. Proceed with caution."
    exit 0
else
    echo -e "${GREEN}✅ ANDON SIGNAL: GREEN${NC}"
    echo "All commands verified successfully."
    exit 0
fi

