#!/usr/bin/env bash
# Unified Marketplace Package Validation Script
# Validates all packages in marketplace/packages/ for production readiness

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
PACKAGES_DIR="$PROJECT_ROOT/marketplace/packages"
cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Counters
TOTAL=0
READY=0
NEEDS_IMPROVEMENT=0
NOT_READY=0
ERRORS=0

log() {
    echo -e "${BLUE}[VALIDATE]${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

error() {
    echo -e "${RED}✗${NC} $1"
}

warn() {
    echo -e "${YELLOW}⚠${NC} $1"
}

# Check if ggen CLI is available
if ! command -v cargo &> /dev/null; then
    error "cargo not found. Please install Rust toolchain."
    exit 1
fi

# Build ggen CLI if needed
log "Building ggen CLI..."
if ! cargo build --release --bin ggen -p ggen-cli-lib &> /dev/null; then
    error "Failed to build ggen CLI"
    exit 1
fi

GGEN_BIN="$PROJECT_ROOT/target/release/ggen"
if [ ! -f "$GGEN_BIN" ]; then
    GGEN_BIN="$PROJECT_ROOT/target/debug/ggen"
fi

if [ ! -f "$GGEN_BIN" ]; then
    error "ggen binary not found. Build failed."
    exit 1
fi

success "ggen CLI ready: $GGEN_BIN"

# Validate all packages
log "Validating all packages in $PACKAGES_DIR..."

# Run validation using ggen CLI
VALIDATION_OUTPUT=$("$GGEN_BIN" marketplace validate --packages-dir "$PACKAGES_DIR" --json 2>&1 || true)

# Parse JSON output if available
if command -v jq &> /dev/null && echo "$VALIDATION_OUTPUT" | jq . &> /dev/null; then
    TOTAL=$(echo "$VALIDATION_OUTPUT" | jq -r '.total_packages // 0')
    READY=$(echo "$VALIDATION_OUTPUT" | jq -r '.ready_count // 0')
    NEEDS_IMPROVEMENT=$(echo "$VALIDATION_OUTPUT" | jq -r '.needs_improvement_count // 0')
    NOT_READY=$(echo "$VALIDATION_OUTPUT" | jq -r '.not_ready_count // 0')
    
    # Extract individual package results
    echo "$VALIDATION_OUTPUT" | jq -r '.all_results[]? | "\(.package_name)|\(.score)|\(.production_ready)"' > /tmp/package_results.txt || true
else
    # Fallback: manual validation
    log "jq not available, running manual validation..."
    
    for package_dir in "$PACKAGES_DIR"/*; do
        if [ ! -d "$package_dir" ]; then
            continue
        fi
        
        package_name=$(basename "$package_dir")
        
        # Skip hidden directories and special files
        if [[ "$package_name" =~ ^\. ]]; then
            continue
        fi
        
        TOTAL=$((TOTAL + 1))
        
        log "Validating: $package_name"
        
        # Run validation for single package
        PKG_OUTPUT=$("$GGEN_BIN" marketplace validate "$package_name" --packages-dir "$PACKAGES_DIR" --json 2>&1 || true)
        
        if echo "$PKG_OUTPUT" | grep -q "production_ready.*true"; then
            READY=$((READY + 1))
            success "$package_name: Production ready"
        elif echo "$PKG_OUTPUT" | grep -q '"score":[0-9]\{1,2\}\.[0-9]*"'; then
            SCORE=$(echo "$PKG_OUTPUT" | grep -o '"score":[0-9]\{1,2\}\.[0-9]*' | cut -d: -f2)
            if (( $(echo "$SCORE >= 80" | bc -l) )); then
                NEEDS_IMPROVEMENT=$((NEEDS_IMPROVEMENT + 1))
                warn "$package_name: Needs improvement (score: $SCORE)"
            else
                NOT_READY=$((NOT_READY + 1))
                error "$package_name: Not ready (score: $SCORE)"
            fi
        else
            NOT_READY=$((NOT_READY + 1))
            error "$package_name: Validation failed"
            ERRORS=$((ERRORS + 1))
        fi
    done
fi

# Summary
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log "Validation Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Total packages: $TOTAL"
success "Production ready: $READY"
warn "Needs improvement: $NEEDS_IMPROVEMENT"
error "Not ready: $NOT_READY"

if [ $ERRORS -gt 0 ]; then
    error "Validation errors: $ERRORS"
fi

# Calculate percentage
if [ $TOTAL -gt 0 ]; then
    READY_PCT=$((READY * 100 / TOTAL))
    echo ""
    echo "Production readiness: $READY_PCT%"
    
    if [ $READY_PCT -ge 80 ]; then
        success "✅ Marketplace is in good shape!"
    elif [ $READY_PCT -ge 50 ]; then
        warn "⚠️  Marketplace needs improvement"
    else
        error "❌ Marketplace needs significant work"
    fi
fi

# Exit with error if any packages failed validation
if [ $ERRORS -gt 0 ] || [ $NOT_READY -gt 0 ]; then
    exit 1
fi

exit 0



