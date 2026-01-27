#!/bin/bash
# TAIEA Release Execution Script
# Extracts and runs the compiled Erlang release tarball
# Usage: ./tools/run_release.sh [port]

set -e

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get the directory where this script resides
TOOLS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
RELEASE_DIR="$( dirname "$TOOLS_DIR" )"

# Configuration
TARBALL="${RELEASE_DIR}/_build/prod/rel/tai_autonomics/tai_autonomics.tar.gz"
PORT="${1:-8080}"
TMP_DIR="${TMPDIR:-/tmp}/taiea-release-$$"
RELEASE_NAME="tai_autonomics"

# Logging functions
log_info() {
    echo -e "${BLUE}ℹ${NC}  $1"
}

log_success() {
    echo -e "${GREEN}✓${NC}  $1"
}

log_warn() {
    echo -e "${YELLOW}⚠${NC}  $1"
}

log_error() {
    echo -e "${RED}✗${NC}  $1"
}

# Header
echo ""
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║        TAIEA Release Execution (CCW Smoke Testing)            ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

# Validate tarball exists
if [ ! -f "$TARBALL" ]; then
    log_error "Tarball not found at $TARBALL"
    echo ""
    echo "Please build the release first:"
    echo "  cd $RELEASE_DIR"
    echo "  rebar3 as prod release"
    exit 1
fi

log_success "Release tarball found"
log_info "Release path: $TARBALL"

# Create temporary directory
log_info "Extracting release to: $TMP_DIR"
mkdir -p "$TMP_DIR"
tar -xzf "$TARBALL" -C "$TMP_DIR" || {
    log_error "Failed to extract tarball"
    exit 1
}
log_success "Release extracted"

# Verify release structure
RELEASE_HOME="$TMP_DIR/$RELEASE_NAME"
if [ ! -d "$RELEASE_HOME" ]; then
    log_error "Release directory not found at $RELEASE_HOME"
    log_info "Contents of $TMP_DIR:"
    ls -la "$TMP_DIR"
    exit 1
fi

BIN_DIR="$RELEASE_HOME/bin"
if [ ! -d "$BIN_DIR" ]; then
    log_error "Binary directory not found at $BIN_DIR"
    exit 1
fi

EXECUTABLE="$BIN_DIR/$RELEASE_NAME"
if [ ! -f "$EXECUTABLE" ]; then
    log_error "Executable not found at $EXECUTABLE"
    log_info "Available files in $BIN_DIR:"
    ls -la "$BIN_DIR"
    exit 1
fi

log_success "Release structure verified"

# Set environment variables
export TAIEA_ENV=${TAIEA_ENV:-dev}
export PORT=${PORT}
export TAIEA_HOME="$RELEASE_HOME"
export ERL_CRASH_DUMP="$TMP_DIR/erl_crash.dump"

log_info "Environment configuration:"
log_info "  TAIEA_ENV: $TAIEA_ENV"
log_info "  PORT: $PORT"
log_info "  TAIEA_HOME: $TAIEA_HOME"

# Cleanup function (runs on exit)
cleanup() {
    local exit_code=$?
    echo ""
    if [ $exit_code -eq 0 ]; then
        log_success "Release execution completed successfully"
    else
        log_error "Release execution failed with exit code $exit_code"
    fi

    # Cleanup temporary directory
    if [ -d "$TMP_DIR" ]; then
        log_info "Cleaning up temporary directory: $TMP_DIR"
        rm -rf "$TMP_DIR"
    fi
}

trap cleanup EXIT

# Run the release
echo ""
log_info "Starting TAIEA release..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Execute in foreground for docker/container execution
"$EXECUTABLE" foreground

