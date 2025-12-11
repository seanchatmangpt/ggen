#!/usr/bin/env bash
#
# Master Documentation Validation Script
#
# Runs all documentation validation tests and generates a comprehensive report.
# This ensures that all tutorials, guides, and examples in the documentation
# actually work with the current version of ggen.

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

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Test suite counters
TOTAL_SUITES=0
PASSED_SUITES=0
FAILED_SUITES=0

# Individual test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Report file
REPORT_FILE="${REPORT_FILE:-$SCRIPT_DIR/validation-report.md}"

log_header() {
    echo ""
    echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}${CYAN}$1${NC}"
    echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

log_suite() {
    echo ""
    echo -e "${YELLOW}▶ Running: $1${NC}"
    echo ""
}

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

log_error() {
    echo -e "${RED}✗${NC} $1"
}

log_info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

# Function to run a validation script
run_validation() {
    local script_name=$1
    local display_name=$2
    local script_path="$SCRIPT_DIR/$script_name"

    ((TOTAL_SUITES++))

    if [ ! -f "$script_path" ]; then
        log_error "Script not found: $script_path"
        ((FAILED_SUITES++))
        return 1
    fi

    if [ ! -x "$script_path" ]; then
        chmod +x "$script_path"
    fi

    log_suite "$display_name"

    # Run script and capture output
    if OUTPUT=$("$script_path" 2>&1); then
        log_success "$display_name PASSED"
        ((PASSED_SUITES++))

        # Extract test counts from output
        if echo "$OUTPUT" | grep -q "Tests Passed:"; then
            SUITE_PASSED=$(echo "$OUTPUT" | grep "Tests Passed:" | grep -oE "[0-9]+" || echo "0")
            SUITE_FAILED=$(echo "$OUTPUT" | grep "Tests Failed:" | grep -oE "[0-9]+" || echo "0")
            ((TOTAL_TESTS += SUITE_PASSED + SUITE_FAILED))
            ((PASSED_TESTS += SUITE_PASSED))
            ((FAILED_TESTS += SUITE_FAILED))

            log_info "Individual tests: $SUITE_PASSED passed, $SUITE_FAILED failed"
        fi
        return 0
    else
        log_error "$display_name FAILED"
        ((FAILED_SUITES++))

        # Extract test counts from output
        if echo "$OUTPUT" | grep -q "Tests Passed:"; then
            SUITE_PASSED=$(echo "$OUTPUT" | grep "Tests Passed:" | grep -oE "[0-9]+" || echo "0")
            SUITE_FAILED=$(echo "$OUTPUT" | grep "Tests Failed:" | grep -oE "[0-9]+" || echo "0")
            ((TOTAL_TESTS += SUITE_PASSED + SUITE_FAILED))
            ((PASSED_TESTS += SUITE_PASSED))
            ((FAILED_TESTS += SUITE_FAILED))

            log_info "Individual tests: $SUITE_PASSED passed, $SUITE_FAILED failed"
        fi

        # Show last 20 lines of output for debugging
        echo ""
        echo -e "${RED}Last 20 lines of output:${NC}"
        echo "$OUTPUT" | tail -20 | sed 's/^/  /'
        return 1
    fi
}

# Start validation
START_TIME=$(date +%s)

log_header "ggen Documentation Validation Suite"

log_info "ggen version: $(ggen --version 2>&1 || echo 'not found')"
log_info "Timestamp: $(date '+%Y-%m-%d %H:%M:%S')"
log_info "Report will be saved to: $REPORT_FILE"

# ============================================================================
# Run Validation Suites
# ============================================================================

# Quick Start Tutorial
run_validation "validate-quick-start.sh" "Quick Start Tutorial (10 minutes)"

# SPARQL Query Guide
run_validation "validate-sparql-guide.sh" "SPARQL Query How-to Guide"

# CLI Reference
run_validation "validate-cli-reference.sh" "Complete CLI Command Reference"

# Watch Mode
run_validation "validate-watch-mode.sh" "Project Watch Mode Functionality"

# TOML Configuration Reference
run_validation "validate-toml-reference.sh" "TOML Configuration Reference"

# Diataxis Case Study
run_validation "validate-case-study.sh" "Diataxis Case Study Documentation"

# TypeScript Detection
run_validation "check-no-typescript.sh" "TypeScript Usage Detection"

# Broken Link Checker
run_validation "check-broken-links.sh" "Internal Link Validation"

# ============================================================================
# Summary
# ============================================================================

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

log_header "Validation Summary"

echo ""
echo "Test Suites:"
echo "  Total:   $TOTAL_SUITES"
echo -e "  ${GREEN}Passed:  $PASSED_SUITES${NC}"
echo -e "  ${RED}Failed:  $FAILED_SUITES${NC}"
echo ""
echo "Individual Tests:"
echo "  Total:   $TOTAL_TESTS"
echo -e "  ${GREEN}Passed:  $PASSED_TESTS${NC}"
echo -e "  ${RED}Failed:  $FAILED_TESTS${NC}"
echo ""
echo "Duration: ${DURATION}s"
echo ""

# ============================================================================
# Generate Markdown Report
# ============================================================================

log_info "Generating validation report..."

cat > "$REPORT_FILE" << EOF
# ggen Documentation Validation Report

**Generated**: $(date '+%Y-%m-%d %H:%M:%S')
**ggen Version**: $(ggen --version 2>&1 || echo 'unknown')
**Duration**: ${DURATION}s

---

## Summary

| Metric | Count |
|--------|-------|
| **Test Suites Run** | $TOTAL_SUITES |
| **Suites Passed** | ✅ $PASSED_SUITES |
| **Suites Failed** | ❌ $FAILED_SUITES |
| **Individual Tests Run** | $TOTAL_TESTS |
| **Tests Passed** | ✅ $PASSED_TESTS |
| **Tests Failed** | ❌ $FAILED_TESTS |

**Overall Status**: $(if [ $FAILED_SUITES -eq 0 ]; then echo "✅ **PASS**"; else echo "❌ **FAIL**"; fi)

---

## Test Suites

### 1. Quick Start Tutorial (10 minutes)

**Document**: \`docs/getting-started/quick-start.md\`
**Status**: $(if [ $PASSED_SUITES -ge 1 ]; then echo "✅ PASS"; else echo "❌ FAIL"; fi)

Tests that users can:
- Install ggen
- Create an RDF ontology
- Load data into the graph
- Query with SPARQL
- Extract schema to JSON
- Generate JavaScript + Zod code

### 2. SPARQL Query How-to Guide

**Document**: \`docs/how-to/generation/query-rdf-sparql.md\`
**Status**: $(if [ $PASSED_SUITES -ge 2 ]; then echo "✅ PASS"; else echo "❌ FAIL"; fi)

Tests all SPARQL query examples:
- Basic SELECT queries
- Filtering with FILTER
- Aggregation with COUNT and GROUP BY
- LIMIT and DISTINCT
- Common patterns

### 3. Complete CLI Command Reference

**Document**: \`docs/reference/commands/complete-cli-reference.md\`
**Status**: $(if [ $PASSED_SUITES -ge 3 ]; then echo "✅ PASS"; else echo "❌ FAIL"; fi)

Tests all documented CLI commands:
- Template commands (list, show, lint, etc.)
- Graph commands (load, query, export, visualize)
- Ontology commands (extract, validate, generate)
- Project commands (init, gen, watch, etc.)
- Global options (--help, --version)

---

## Recommendations

EOF

if [ $FAILED_SUITES -eq 0 ] && [ $FAILED_TESTS -eq 0 ]; then
    cat >> "$REPORT_FILE" << EOF
✅ **All documentation is validated and working!**

The documentation accurately reflects the current state of ggen. All tutorials,
guides, and examples have been tested and work as documented.

**Next Steps**:
- Documentation is ready for users
- Consider adding this validation to CI/CD pipeline
- Run this validation before each release
EOF
else
    cat >> "$REPORT_FILE" << EOF
⚠️  **Some documentation validation tests failed**

**Action Required**:
1. Review failed test output above
2. Fix failing commands or update documentation
3. Re-run validation: \`scripts/validate-docs/validate-all.sh\`
4. Do not merge documentation changes until all tests pass

**Common Issues**:
- Commands may have changed since documentation was written
- Examples may reference non-existent templates
- SPARQL queries may not match current data model
- CLI flags may have been renamed or removed
EOF
fi

cat >> "$REPORT_FILE" << EOF

---

## Running Validation

**Run all tests**:
\`\`\`bash
scripts/validate-docs/validate-all.sh
\`\`\`

**Run specific test**:
\`\`\`bash
scripts/validate-docs/validate-quick-start.sh
scripts/validate-docs/validate-sparql-guide.sh
scripts/validate-docs/validate-cli-reference.sh
\`\`\`

**Environment Variables**:
- \`GGEN_BIN\`: Path to ggen binary (default: \`ggen\`)
- \`REPORT_FILE\`: Path to report output (default: \`validation-report.md\`)

---

**Last Updated**: $(date '+%Y-%m-%d %H:%M:%S')
EOF

log_success "Report saved to: $REPORT_FILE"

# ============================================================================
# Exit
# ============================================================================

echo ""
if [ $FAILED_SUITES -eq 0 ]; then
    echo -e "${GREEN}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}${BOLD}✓ ALL DOCUMENTATION VALIDATION PASSED${NC}"
    echo -e "${GREEN}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 0
else
    echo -e "${RED}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${RED}${BOLD}✗ DOCUMENTATION VALIDATION FAILED${NC}"
    echo -e "${RED}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    echo -e "${YELLOW}Review the report for details:${NC}"
    echo "  cat $REPORT_FILE"
    exit 1
fi
