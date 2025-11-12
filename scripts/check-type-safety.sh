#!/usr/bin/env bash
# Type Safety Check Script - Root Cause Prevention
# Checks for common type mismatch patterns that indicate missing validation steps

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

ERRORS=0

log() {
    echo -e "${BLUE}[TYPE-SAFETY]${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

error() {
    echo -e "${RED}✗${NC} $1"
    ((ERRORS++))
}

warn() {
    echo -e "${YELLOW}⚠${NC} $1"
}

log "Checking for type safety issues..."

# Check 1: Builder pattern without validation
log "Checking for builder pattern without validation..."
BUILDER_WITHOUT_VALIDATE=$(grep -r "\.build()" --include="*.rs" crates/ | \
    grep -v "\.validate()" | \
    grep -v "test" | \
    grep -v "example" | \
    wc -l | tr -d ' ')

if [ "$BUILDER_WITHOUT_VALIDATE" -gt 0 ]; then
    warn "Found $BUILDER_WITHOUT_VALIDATE potential builder patterns without validation"
    warn "Review these to ensure validation step is not skipped:"
    grep -r "\.build()" --include="*.rs" crates/ | \
        grep -v "\.validate()" | \
        grep -v "test" | \
        grep -v "example" | \
        head -5
else
    success "No builder patterns without validation found"
fi

# Check 2: Type mismatch patterns (UnvalidatedPackage used where Package expected)
log "Checking for UnvalidatedPackage type mismatches..."
UNVALIDATED_MISMATCH=$(grep -r "UnvalidatedPackage" --include="*.rs" crates/ | \
    grep -E "(publish|install|get)" | \
    grep -v "validate" | \
    wc -l | tr -d ' ')

if [ "$UNVALIDATED_MISMATCH" -gt 0 ]; then
    error "Found $UNVALIDATED_MISMATCH potential UnvalidatedPackage type mismatches"
    error "These may indicate missing validation steps"
else
    success "No UnvalidatedPackage type mismatches found"
fi

# Check 3: Compilation check
log "Running cargo check to catch type errors..."
if cargo check --workspace --all-targets &> /dev/null; then
    success "All crates compile successfully"
else
    error "Compilation errors found - run 'cargo check' for details"
    ERRORS=$((ERRORS + 1))
fi

# Summary
echo ""
if [ $ERRORS -eq 0 ]; then
    success "Type safety checks passed"
    exit 0
else
    error "Type safety checks failed: $ERRORS error(s)"
    exit 1
fi

