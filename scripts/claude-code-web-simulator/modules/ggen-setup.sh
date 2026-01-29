#!/bin/bash

##############################################################################
# ggen Binary Detection & Installation Module
#
# Provides robust ggen binary detection and installation capabilities
# for the Claude Code Web Simulation environment.
#
# Functions:
#   detect_ggen_binary()      - Detect ggen in PATH or ~/.cargo/bin
#   install_ggen_if_needed()  - Install ggen if not found
#   export_ggen_env()         - Export GGEN_BIN environment variable
#
# Version: 1.0.0
# Author: ggen AI Team
##############################################################################

set -euo pipefail

# Import logging functions from caller or define locally
if [ -z "${LOG_FUNCTIONS_LOADED:-}" ]; then
  RED='\033[0;31m'
  GREEN='\033[0;32m'
  YELLOW='\033[1;33m'
  BLUE='\033[0;34m'
  NC='\033[0m' # No Color

  log_info() {
    echo -e "${BLUE}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
  }

  log_success() {
    echo -e "${GREEN}[✓]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
  }

  log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
  }

  log_error() {
    echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
  }
fi

##############################################################################
# Detect ggen binary location
#
# Returns:
#   0 - Success (path written to stdout)
#   1 - Failure (error message written to stderr)
#
# Output:
#   stdout: Path to ggen binary if found
#   stderr: Error message if not found
##############################################################################
detect_ggen_binary() {
  local ggen_path=""

  # Check 1: Look in PATH
  if command -v ggen &>/dev/null; then
    ggen_path="$(command -v ggen)"
    echo "$ggen_path"
    return 0
  fi

  # Check 2: Check ~/.cargo/bin/ggen (common location after cargo install)
  if [ -f "$HOME/.cargo/bin/ggen" ]; then
    ggen_path="$HOME/.cargo/bin/ggen"
    echo "$ggen_path"
    return 0
  fi

  # Check 3: Check current repository target directory
  if [ -f "./target/release/ggen" ]; then
    ggen_path="./target/release/ggen"
    echo "$ggen_path"
    return 0
  fi

  # Check 4: Check debug directory
  if [ -f "./target/debug/ggen" ]; then
    ggen_path="./target/debug/ggen"
    echo "$ggen_path"
    return 0
  fi

  # ggen not found
  echo "ggen binary not found in PATH or standard locations" >&2
  return 1
}

##############################################################################
# Install ggen binary using cargo install
#
# Returns:
#   0 - Success (ggen installed and verified)
#   1 - Failure (installation failed)
#
# Output:
#   stdout: Path to installed binary on success
#   stderr: Error details on failure
##############################################################################
install_ggen_if_needed() {
  log_info "Checking if ggen needs to be installed..."

  # First try to detect existing installation
  if local ggen_path; ggen_path=$(detect_ggen_binary 2>/dev/null); then
    log_success "ggen already installed at: $ggen_path"
    echo "$ggen_path"
    return 0
  fi

  # ggen not found, proceed with installation
  log_info "ggen not found. Installing via 'cargo install ggen --locked'..."

  # Verify cargo is available
  if ! command -v cargo &>/dev/null; then
    log_error "cargo not found. Please install Rust: https://rustup.rs/"
    return 1
  fi

  # Install ggen
  if ! cargo install ggen --locked 2>&1 | grep -v "^warning"; then
    log_error "Failed to install ggen via cargo install"
    return 1
  fi

  log_success "ggen installed successfully"

  # Verify installation
  if ! local ggen_path; ggen_path=$(detect_ggen_binary); then
    log_error "Installation verification failed - ggen still not detected after install"
    return 1
  fi

  log_success "Installation verified at: $ggen_path"
  echo "$ggen_path"
  return 0
}

##############################################################################
# Verify ggen binary is executable and working
#
# Arguments:
#   $1 - Path to ggen binary
#
# Returns:
#   0 - Binary is valid and working
#   1 - Binary is invalid or not working
##############################################################################
verify_ggen_binary() {
  local ggen_path="${1:-}"

  if [ -z "$ggen_path" ]; then
    log_error "No ggen path provided for verification"
    return 1
  fi

  if [ ! -f "$ggen_path" ]; then
    log_error "ggen binary not found at: $ggen_path"
    return 1
  fi

  if [ ! -x "$ggen_path" ]; then
    log_error "ggen binary is not executable: $ggen_path"
    return 1
  fi

  # Try running ggen help
  if ! "$ggen_path" --help &>/dev/null; then
    log_error "ggen binary is not functional (--help failed): $ggen_path"
    return 1
  fi

  log_success "ggen binary verified: $ggen_path"
  return 0
}

##############################################################################
# Export ggen environment variable
#
# Arguments:
#   $1 - Path to ggen binary (detected if not provided)
#
# Returns:
#   0 - Success
#   1 - Failure
#
# Side effects:
#   Sets GGEN_BIN environment variable
#   Sets GGEN_VERSION environment variable
##############################################################################
export_ggen_env() {
  local ggen_path="${1:-}"

  # Detect if not provided
  if [ -z "$ggen_path" ]; then
    if ! ggen_path=$(detect_ggen_binary 2>/dev/null); then
      log_error "Cannot export GGEN_BIN - ggen binary not found"
      return 1
    fi
  fi

  # Verify before exporting
  if ! verify_ggen_binary "$ggen_path"; then
    return 1
  fi

  # Export environment variables
  export GGEN_BIN="$ggen_path"

  # Try to get version
  local ggen_version=""
  if ggen_version=$("$ggen_path" --version 2>/dev/null || echo "unknown"); then
    export GGEN_VERSION="$ggen_version"
    log_success "Exported GGEN_BIN=$GGEN_BIN (version: $GGEN_VERSION)"
  else
    log_success "Exported GGEN_BIN=$GGEN_BIN"
  fi

  return 0
}

##############################################################################
# Initialize ggen setup (main entry point)
#
# This function orchestrates the complete ggen setup process:
# 1. Attempts to detect existing ggen binary
# 2. If not found, attempts installation
# 3. Verifies functionality
# 4. Exports environment variables
#
# Returns:
#   0 - Success (ggen is ready to use)
#   1 - Failure (ggen could not be set up)
##############################################################################
init_ggen_setup() {
  log_info "Initializing ggen setup..."

  local ggen_path=""

  # Step 1: Try to detect existing ggen
  if ggen_path=$(detect_ggen_binary 2>/dev/null); then
    log_success "ggen detected at: $ggen_path"
  else
    # Step 2: Attempt installation
    log_warn "ggen not detected, attempting installation..."
    if ! ggen_path=$(install_ggen_if_needed); then
      log_error "ggen setup failed - could not install"
      return 1
    fi
  fi

  # Step 3: Verify ggen is functional
  if ! verify_ggen_binary "$ggen_path"; then
    log_error "ggen verification failed"
    return 1
  fi

  # Step 4: Export environment variables
  if ! export_ggen_env "$ggen_path"; then
    log_error "Failed to export ggen environment variables"
    return 1
  fi

  log_success "ggen setup complete and ready"
  return 0
}

##############################################################################
# SessionStart hook integration
#
# This function should be called from main.sh SessionStart hook
# It sets up ggen and ensures GGEN_BIN is available for all child processes
##############################################################################
ggen_session_start_hook() {
  log_info "Running ggen SessionStart hook..."

  if init_ggen_setup; then
    log_success "ggen SessionStart hook completed successfully"
    return 0
  else
    log_error "ggen SessionStart hook failed"
    return 1
  fi
}

##############################################################################
# Diagnostic function for troubleshooting
#
# Outputs detailed information about ggen setup status
##############################################################################
ggen_diagnostics() {
  echo ""
  echo "=== ggen Diagnostics ==="
  echo ""
  echo "System Information:"
  echo "  OS: $(uname -s)"
  echo "  Architecture: $(uname -m)"
  echo "  Shell: $SHELL"
  echo ""

  echo "Cargo Information:"
  if command -v cargo &>/dev/null; then
    echo "  cargo: $(cargo --version)"
  else
    echo "  cargo: NOT FOUND"
  fi
  echo ""

  echo "ggen Detection:"
  if ggen_path=$(detect_ggen_binary 2>/dev/null); then
    echo "  Status: FOUND"
    echo "  Path: $ggen_path"
    if verify_ggen_binary "$ggen_path"; then
      echo "  Verification: PASSED"
      if version=$("$ggen_path" --version 2>/dev/null); then
        echo "  Version: $version"
      fi
    else
      echo "  Verification: FAILED"
    fi
  else
    echo "  Status: NOT FOUND"
  fi
  echo ""

  echo "Potential Installation Paths:"
  [ -f "$HOME/.cargo/bin/ggen" ] && echo "  ~/.cargo/bin/ggen: EXISTS" || echo "  ~/.cargo/bin/ggen: NOT FOUND"
  [ -f "./target/release/ggen" ] && echo "  ./target/release/ggen: EXISTS" || echo "  ./target/release/ggen: NOT FOUND"
  [ -f "./target/debug/ggen" ] && echo "  ./target/debug/ggen: EXISTS" || echo "  ./target/debug/ggen: NOT FOUND"
  echo ""

  echo "Environment Variables:"
  echo "  GGEN_BIN: ${GGEN_BIN:-NOT SET}"
  echo "  GGEN_VERSION: ${GGEN_VERSION:-NOT SET}"
  echo ""
}

# Export functions for use by main.sh
export -f detect_ggen_binary
export -f install_ggen_if_needed
export -f verify_ggen_binary
export -f export_ggen_env
export -f init_ggen_setup
export -f ggen_session_start_hook
export -f ggen_diagnostics
