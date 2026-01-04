#!/bin/bash
# ggen Build and Release Script
# Fail-proof Debian package builder with poka-yoke error gates

set -euo pipefail  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Poka-yoke error gates
trap 'echo -e "${RED}❌ Build failed${NC}"; exit 1' ERR

log_info() { echo -e "${GREEN}✓${NC} $1"; }
log_warn() { echo -e "${YELLOW}⚠${NC} $1"; }
log_error() { echo -e "${RED}✗${NC} $1"; exit 1; }

# Gate 1: Check required tools
log_info "Gate 1: Checking required tools..."
command -v cargo &> /dev/null || log_error "cargo not found"
command -v debuild &> /dev/null || log_error "debuild not found"
command -v sha256sum &> /dev/null || log_error "sha256sum not found"
log_info "All required tools present"

# Gate 2: Verify ggen.toml exists
log_info "Gate 2: Validating project structure..."
[[ -f "ggen.toml" ]] || log_error "ggen.toml not found"
[[ -f "Cargo.toml" ]] || log_error "Cargo.toml not found"
[[ -d "debian/" ]] || log_error "debian/ directory not found"
[[ -f "debian/control" ]] || log_error "debian/control not found"
log_info "Project structure valid"

# Gate 3: Clean previous build artifacts (but NOT cargo target - we reuse the binary)
log_info "Gate 3: Cleaning previous artifacts..."
rm -rf ./debian/.debhelper || true
rm -rf ./debian/*.substvars || true
rm -rf ./debian/ggen || true
rm -rf ../*.deb ../*.changes ../*.dsc ../*.tar.* || true
log_info "Build directory cleaned"

# Gate 4: Verify binary builds
log_info "Gate 4: Building release binary..."
cargo build --release -p ggen-cli-lib --bin ggen || log_error "Binary build failed"
[[ -f "target/release/ggen" ]] || log_error "Binary not found at target/release/ggen"
log_info "Binary built successfully"

# Gate 5: Test binary functionality
log_info "Gate 5: Testing binary functionality..."
HELP_OUTPUT=$(/home/user/ggen/target/release/ggen --help 2>&1 || true)
echo "$HELP_OUTPUT" | grep -q "sync" || log_error "ggen binary missing 'sync' command"
log_info "Binary functionality verified"

# Gate 6: Check binary size (sanity check)
log_info "Gate 6: Validating binary size..."
BINARY_SIZE=$(stat -f%z "target/release/ggen" 2>/dev/null || stat -c%s "target/release/ggen")
[[ $BINARY_SIZE -gt 1000000 ]] || log_error "Binary suspiciously small ($BINARY_SIZE bytes)"
[[ $BINARY_SIZE -lt 100000000 ]] || log_error "Binary suspiciously large ($BINARY_SIZE bytes)"
log_info "Binary size valid: $(numfmt --to=iec $BINARY_SIZE 2>/dev/null || echo "$BINARY_SIZE bytes")"

# Gate 7: Create checksums BEFORE building .deb
log_info "Gate 7: Creating checksums..."
sha256sum target/release/ggen > ggen.sha256
log_info "Checksums created"

# Gate 8: Build .deb package
log_info "Gate 8: Building Debian package..."
debuild -us -uc -b -d 2>&1 | tee /tmp/debuild.log  # -d skips build dependency check (we have tools installed)
log_info ".deb build completed"

# Gate 9: Verify .deb was created
log_info "Gate 9: Verifying .deb package..."
DEB_FILE="../ggen_5.0.2-1_amd64.deb"
[[ -f "$DEB_FILE" ]] || log_error ".deb file not found at $DEB_FILE"
DEB_SIZE=$(stat -c%s "$DEB_FILE" 2>/dev/null || stat -f%z "$DEB_FILE" 2>/dev/null)
[[ $DEB_SIZE -gt 1000000 ]] || log_error ".deb suspiciously small ($DEB_SIZE bytes)"
log_info ".deb package valid: $(numfmt --to=iec $DEB_SIZE 2>/dev/null || echo "$DEB_SIZE bytes")"

# Gate 10: Create checksums for .deb
log_info "Gate 10: Creating .deb checksums..."
cd ..
sha256sum "ggen_5.0.2-1_amd64.deb" > "ggen_5.0.2-1_amd64.deb.sha256"
cd -
log_info "Checksums created for distribution"

# Gate 11: List distribution artifacts
log_info "Gate 11: Verifying distribution artifacts..."
echo ""
echo "📦 Build Complete! Distribution artifacts ready:"
echo ""
ls -lh "../ggen_5.0.2-1_amd64.deb"* 2>/dev/null || true
ls -lh "ggen.sha256" 2>/dev/null || true
echo ""

# Gate 12: Display next steps
echo -e "${GREEN}✅ All gates passed - ready for distribution!${NC}"
echo ""
echo "Next steps:"
echo "1. Upload to GitHub Release:"
echo "   gh release create v5.0.2 ../ggen_5.0.2-1_amd64.deb ggen.sha256"
echo ""
echo "2. Users install with:"
echo "   sudo apt install ./ggen_5.0.2-1_amd64.deb"
echo ""
echo "3. Verify installation:"
echo "   ggen --help"
