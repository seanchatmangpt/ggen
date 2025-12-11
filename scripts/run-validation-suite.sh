#!/usr/bin/env bash
#
# Complete Validation Suite Runner
#
# Runs all validation scripts for the ggen documentation project
# Integrates with ggen.toml configuration

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

# Get project root
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

log_header() {
    echo ""
    echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}${CYAN}$1${NC}"
    echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

log_info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

log_error() {
    echo -e "${RED}✗${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

# ============================================================================
# Pre-flight Checks
# ============================================================================

log_header "ggen Documentation Validation Suite"

log_info "Project root: $PROJECT_ROOT"
log_info "Environment: ${GGEN_ENV:-development}"

# Check for ggen.toml
if [ -f "ggen.toml" ]; then
    log_success "Found ggen.toml configuration"
else
    log_warning "No ggen.toml found (optional)"
fi

# Check for validation scripts
if [ -d "scripts/validate-docs" ]; then
    log_success "Found validation scripts directory"
else
    log_error "Missing scripts/validate-docs directory"
    exit 1
fi

# ============================================================================
# Run Validation Suite
# ============================================================================

log_header "Running Documentation Validation"

START_TIME=$(date +%s)

# Run master validation script
if [ -f "scripts/validate-docs/validate-all.sh" ]; then
    log_info "Running complete validation suite..."

    if ./scripts/validate-docs/validate-all.sh; then
        VALIDATION_STATUS="PASSED"
    else
        VALIDATION_STATUS="FAILED"
    fi
else
    log_error "Missing validate-all.sh"
    exit 1
fi

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# ============================================================================
# Generate Summary Report
# ============================================================================

log_header "Validation Summary"

echo ""
echo "Environment: ${GGEN_ENV:-development}"
echo "Duration: ${DURATION}s"
echo ""

if [ "$VALIDATION_STATUS" = "PASSED" ]; then
    echo -e "${GREEN}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}${BOLD}✓ ALL VALIDATIONS PASSED${NC}"
    echo -e "${GREEN}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    echo "Documentation is:"
    echo "  ✓ Structurally valid (Diataxis compliant)"
    echo "  ✓ Examples working (all code tested)"
    echo "  ✓ Links valid (cross-references checked)"
    echo "  ✓ TOML valid (configuration examples tested)"
    echo "  ✓ Case study complete (meta-level documentation)"
    echo ""
    echo "View detailed report:"
    echo "  cat scripts/validate-docs/validation-report.md"
    echo ""
    exit 0
else
    echo -e "${RED}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${RED}${BOLD}✗ VALIDATION FAILED${NC}"
    echo -e "${RED}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    echo "Review failures:"
    echo "  cat scripts/validate-docs/validation-report.md"
    echo ""
    exit 1
fi
