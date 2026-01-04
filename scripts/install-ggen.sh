#!/bin/bash
# ggen Installation Script
# Fail-proof installer with poka-yoke error gates and rollback capability

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Poka-yoke error gates
trap 'echo -e "${RED}❌ Installation failed${NC}"; [[ -n "$BACKUP_FILE" ]] && restore_backup; exit 1' ERR

BACKUP_FILE=""
VERSION="5.0.2"
DEB_URL="${1:-}"

log_info() { echo -e "${GREEN}✓${NC} $1"; }
log_warn() { echo -e "${YELLOW}⚠${NC} $1"; }
log_error() { echo -e "${RED}✗${NC} $1"; exit 1; }

restore_backup() {
    if [[ -f "$BACKUP_FILE" ]]; then
        log_warn "Restoring backed up version..."
        sudo mv "$BACKUP_FILE" /usr/bin/ggen
        log_info "Rollback completed"
    fi
}

# Gate 1: Check user is root or has sudo
log_info "Gate 1: Checking permissions..."
[[ $EUID -eq 0 ]] || [[ -n $(sudo -n true 2>/dev/null) ]] || log_error "This script requires sudo or root access"
log_info "Permissions verified"

# Gate 2: Validate system requirements
log_info "Gate 2: Checking system requirements..."
[[ "$(uname -s)" == "Linux" ]] || log_error "Only Linux is supported"
command -v dpkg &> /dev/null || log_error "dpkg not found - requires Debian/Ubuntu"
command -v sha256sum &> /dev/null || log_error "sha256sum not found"
log_info "System requirements met"

# Gate 3: Validate input
log_info "Gate 3: Validating installation source..."
if [[ -z "$DEB_URL" ]]; then
    # Local file installation
    DEB_FILE="${2:-.}"
    [[ -f "$DEB_FILE" ]] || log_error "No .deb file found - usage: $0 <path-to-ggen.deb>"
    [[ "$DEB_FILE" == *.deb ]] || log_error "File is not a .deb package"
    log_info "Local .deb file validated"
else
    # URL installation
    [[ "$DEB_URL" =~ ^https?:// ]] || log_error "Invalid URL format"
    log_info "URL validated: $DEB_URL"
fi

# Gate 4: Check for existing installation
log_info "Gate 4: Checking for existing installation..."
if command -v ggen &> /dev/null; then
    CURRENT_VERSION=$(ggen --version 2>/dev/null || echo "unknown")
    log_warn "Found existing ggen installation: $CURRENT_VERSION"
    GGEN_PATH=$(which ggen)
    BACKUP_FILE="/usr/bin/ggen.backup.$(date +%s)"
    log_warn "Backup will be saved to: $BACKUP_FILE"
fi

# Gate 5: Download .deb if needed
log_info "Gate 5: Preparing .deb file..."
if [[ -n "$DEB_URL" ]]; then
    DEB_FILE="/tmp/ggen_${VERSION}_amd64.deb"
    log_info "Downloading from $DEB_URL..."
    curl -fSL -o "$DEB_FILE" "$DEB_URL" || log_error "Failed to download .deb"
    log_info ".deb downloaded successfully"
else
    DEB_FILE="$(cd "$(dirname "$DEB_FILE")" && pwd)/$(basename "$DEB_FILE")"
fi

# Gate 6: Verify .deb integrity (if checksum available)
log_info "Gate 6: Verifying .deb integrity..."
DEB_SHA256_FILE="${DEB_FILE}.sha256"
if [[ -f "$DEB_SHA256_FILE" ]]; then
    (cd "$(dirname "$DEB_FILE")" && sha256sum -c "$(basename "$DEB_SHA256_FILE")" 2>&1 | grep -q "OK") || \
        log_error ".deb checksum verification failed - file may be corrupted"
    log_info ".deb checksum verified"
else
    log_warn ".deb checksum not available - skipping integrity verification"
fi

# Gate 7: Validate .deb package
log_info "Gate 7: Validating .deb package..."
dpkg -I "$DEB_FILE" > /dev/null || log_error ".deb package is invalid"
log_info ".deb package is valid"

# Gate 8: Backup existing binary
log_info "Gate 8: Preparing backup..."
if [[ -n "$BACKUP_FILE" && -f "/usr/bin/ggen" ]]; then
    sudo cp /usr/bin/ggen "$BACKUP_FILE" || log_error "Failed to backup existing binary"
    log_info "Existing binary backed up to $BACKUP_FILE"
fi

# Gate 9: Install .deb package
log_info "Gate 9: Installing package..."
sudo dpkg -i "$DEB_FILE" || log_error "dpkg installation failed"
log_info "Package installed"

# Gate 10: Verify installation
log_info "Gate 10: Verifying installation..."
command -v ggen &> /dev/null || log_error "ggen not found after installation"
NEW_VERSION=$(ggen --version 2>/dev/null || echo "unknown")
log_info "ggen installed successfully: $NEW_VERSION"

# Gate 11: Test functionality
log_info "Gate 11: Testing ggen functionality..."
HELP_OUTPUT=$(ggen --help 2>&1 || true)
echo "$HELP_OUTPUT" | grep -q "sync" || log_error "ggen help does not contain 'sync' command"
log_info "Functionality test passed"

# Gate 12: Cleanup
log_info "Gate 12: Cleaning up..."
[[ -f "$DEB_FILE" && "$DEB_FILE" == "/tmp/"* ]] && rm -f "$DEB_FILE"
rm -f "$BACKUP_FILE" || true
log_info "Cleanup completed"

# Success
echo ""
echo -e "${GREEN}✅ Installation complete!${NC}"
echo ""
echo "ggen is ready to use:"
echo "  ggen --help      # Show help"
echo "  ggen sync        # Run code generation"
echo ""
