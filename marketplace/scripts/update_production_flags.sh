#!/usr/bin/env bash
# Update Production Flags Script
# Reads validation results and updates production_ready flags in both
# package.toml files and marketplace/registry/index.json

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
PACKAGES_DIR="$PROJECT_ROOT/marketplace/packages"
REGISTRY_INDEX="$PROJECT_ROOT/marketplace/registry/index.json"
cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log() {
    echo -e "${BLUE}[UPDATE-FLAGS]${NC} $1"
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

# Validate all packages and update flags
log "Validating packages and updating production_ready flags..."

# Run validation with --update flag
VALIDATION_OUTPUT=$("$GGEN_BIN" marketplace validate --packages-dir "$PACKAGES_DIR" --update --json 2>&1 || true)

UPDATED_COUNT=0
SKIPPED_COUNT=0

# Parse results and update index.json if jq is available
if command -v jq &> /dev/null && echo "$VALIDATION_OUTPUT" | jq . &> /dev/null; then
    log "Updating registry index.json..."
    
    # Read current index.json
    if [ ! -f "$REGISTRY_INDEX" ]; then
        error "Registry index not found: $REGISTRY_INDEX"
        exit 1
    fi
    
    # Create backup
    cp "$REGISTRY_INDEX" "$REGISTRY_INDEX.bak"
    
    # Update production_ready flags in index.json
    PACKAGE_RESULTS=$(echo "$VALIDATION_OUTPUT" | jq -r '.all_results[]? | "\(.package_name)|\(.production_ready)"')
    
    while IFS='|' read -r package_name production_ready; do
        if [ -z "$package_name" ]; then
            continue
        fi
        
        # Update index.json using jq
        if [ "$production_ready" = "true" ]; then
            # Use jq to update the production_ready flag
            if jq --arg name "$package_name" --argjson ready true \
                '(.packages[] | select(.name == $name) | .production_ready) = $ready' \
                "$REGISTRY_INDEX" > "$REGISTRY_INDEX.tmp"; then
                mv "$REGISTRY_INDEX.tmp" "$REGISTRY_INDEX"
                success "Updated $package_name: production_ready = true"
                UPDATED_COUNT=$((UPDATED_COUNT + 1))
            else
                warn "Failed to update $package_name in index.json"
                SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
            fi
        else
            # Set to false if not ready
            if jq --arg name "$package_name" --argjson ready false \
                '(.packages[] | select(.name == $name) | .production_ready) = $ready' \
                "$REGISTRY_INDEX" > "$REGISTRY_INDEX.tmp"; then
                mv "$REGISTRY_INDEX.tmp" "$REGISTRY_INDEX"
                log "Updated $package_name: production_ready = false"
            fi
        fi
    done <<< "$PACKAGE_RESULTS"
    
    success "Updated $UPDATED_COUNT package(s) in index.json"
    
    if [ $SKIPPED_COUNT -gt 0 ]; then
        warn "Skipped $SKIPPED_COUNT package(s)"
    fi
else
    warn "jq not available. Cannot update index.json automatically."
    warn "Please install jq or update index.json manually."
    warn "Validation results:"
    echo "$VALIDATION_OUTPUT"
fi

# Summary
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log "Update Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
success "Updated package.toml files: $UPDATED_COUNT"
if [ $SKIPPED_COUNT -gt 0 ]; then
    warn "Skipped: $SKIPPED_COUNT"
fi

log "Registry index updated: $REGISTRY_INDEX"
log "Backup saved: $REGISTRY_INDEX.bak"

exit 0



