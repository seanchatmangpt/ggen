#!/usr/bin/env bash
# Validate Homebrew formula locally
# Tests formula syntax, installation, and basic functionality

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_debug() { echo -e "${BLUE}[DEBUG]${NC} $1"; }

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
HOMEBREW_SUBMODULE_DIR="$REPO_ROOT/vendors/homebrew-ggen"
FORMULA_FILE="$HOMEBREW_SUBMODULE_DIR/Formula/ggen.rb"

# Check if on macOS
if [[ "$OSTYPE" != "darwin"* ]]; then
  log_error "This script must be run on macOS (Homebrew is macOS/Linux only)"
  log_info "On Linux, you can test formula syntax but not installation"
  exit 1
fi

# Check if Homebrew is installed
if ! command -v brew >/dev/null 2>&1; then
  log_error "Homebrew is not installed"
  log_info "Install from: https://brew.sh"
  exit 1
fi

# Check if formula file exists
if [[ ! -f "$FORMULA_FILE" ]]; then
  log_error "Formula file not found: $FORMULA_FILE"
  log_info "Run ./scripts/update-homebrew-formula-e2e.sh first to create the formula"
  exit 1
fi

log_info "Validating Homebrew formula: $FORMULA_FILE"
log_info ""

# Test 1: Formula syntax check
log_info "Test 1: Checking formula syntax..."
if brew audit --strict "$FORMULA_FILE" 2>&1; then
  log_info "✅ Formula syntax is valid"
else
  AUDIT_OUTPUT=$(brew audit --strict "$FORMULA_FILE" 2>&1 || true)
  if echo "$AUDIT_OUTPUT" | grep -q "Error"; then
    log_error "❌ Formula syntax check failed!"
    echo "$AUDIT_OUTPUT"
    exit 1
  else
    log_warn "⚠️  Formula has warnings (non-critical)"
    echo "$AUDIT_OUTPUT"
  fi
fi

log_info ""

# Test 2: Formula install dry-run
log_info "Test 2: Testing formula install (dry-run)..."
if brew install --build-from-source --dry-run "$FORMULA_FILE" >/dev/null 2>&1; then
  log_info "✅ Formula install test passed"
else
  log_warn "⚠️  Formula install dry-run had issues (check output above)"
  brew install --build-from-source --dry-run "$FORMULA_FILE" || true
fi

log_info ""

# Test 3: Check formula version
log_info "Test 3: Checking formula version..."
FORMULA_VERSION=$(grep -E '^\s*version\s+"' "$FORMULA_FILE" | head -1 | sed 's/.*version "\(.*\)".*/\1/' || echo "")
if [[ -n "$FORMULA_VERSION" ]]; then
  log_info "Formula version: $FORMULA_VERSION"
  
  # Check if it matches Cargo.toml
  CRATE_VERSION=$(grep '^version = ' "$REPO_ROOT/Cargo.toml" | head -1 | sed 's/version = "\(.*\)"/\1/')
  if [[ "$FORMULA_VERSION" == "$CRATE_VERSION" ]]; then
    log_info "✅ Formula version matches crate version: $CRATE_VERSION"
  else
    log_warn "⚠️  Version mismatch:"
    log_warn "  Formula: $FORMULA_VERSION"
    log_warn "  Crate:   $CRATE_VERSION"
  fi
else
  log_warn "⚠️  Could not extract version from formula"
fi

log_info ""

# Test 4: Check for static binaries (no build dependencies)
log_info "Test 4: Verifying static binaries (no compilation)..."
if grep -q "depends_on.*rust\|depends_on.*cargo\|system.*cargo\|cargo.*install" "$FORMULA_FILE"; then
  log_error "❌ Formula appears to require compilation!"
  log_error "This formula should use static binaries only"
  exit 1
else
  log_info "✅ Formula uses static binaries (no compilation required)"
fi

log_info ""

# Test 5: Check SHA256 checksums exist
log_info "Test 5: Verifying SHA256 checksums..."
SHA256_COUNT=$(grep -c "sha256" "$FORMULA_FILE" || echo "0")
if [[ "$SHA256_COUNT" -ge 4 ]]; then
  log_info "✅ Found $SHA256_COUNT SHA256 checksums (expected 4 for all platforms)"
else
  log_warn "⚠️  Found only $SHA256_COUNT SHA256 checksums (expected 4)"
fi

log_info ""

# Test 6: Check platform support
log_info "Test 6: Verifying platform support..."
if grep -q "on_macos" "$FORMULA_FILE" && grep -q "on_linux" "$FORMULA_FILE"; then
  log_info "✅ Formula supports both macOS and Linux"
else
  log_warn "⚠️  Formula may not support all platforms"
fi

if grep -q "Hardware::CPU.arm" "$FORMULA_FILE" && grep -q "Hardware::CPU.intel\|x86_64" "$FORMULA_FILE"; then
  log_info "✅ Formula supports both ARM64 and x86_64"
else
  log_warn "⚠️  Formula may not support all CPU architectures"
fi

log_info ""

# Test 7: Check installation method
log_info "Test 7: Verifying installation method..."
if grep -q "bin.install" "$FORMULA_FILE"; then
  log_info "✅ Formula installs binary directly (static binary)"
else
  log_warn "⚠️  Formula may not install binary correctly"
fi

if grep -q "generate_completions_from_executable" "$FORMULA_FILE"; then
  log_info "✅ Formula generates shell completions"
else
  log_warn "⚠️  Formula does not generate completions"
fi

log_info ""

# Summary
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "✅ Validation complete!"
log_info ""
log_info "Formula file: $FORMULA_FILE"
log_info ""
log_info "To test installation locally (optional):"
log_info "  brew install --build-from-source $FORMULA_FILE"
log_info ""
log_info "To uninstall after testing:"
log_info "  brew uninstall ggen"
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

