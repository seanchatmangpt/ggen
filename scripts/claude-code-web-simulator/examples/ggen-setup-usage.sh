#!/bin/bash

##############################################################################
# ggen-setup Module Usage Examples
#
# This script demonstrates practical usage patterns for the ggen-setup module.
# Run with: ./examples/ggen-setup-usage.sh
##############################################################################

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(dirname "$SCRIPT_DIR")"

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   ggen-setup Module Usage Examples                        ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Source the module
source "${PARENT_DIR}/modules/ggen-setup.sh"

##############################################################################
# Example 1: Simple Detection
##############################################################################

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Example 1: Simple Binary Detection${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Code:"
echo "  if ggen_path=\$(detect_ggen_binary); then"
echo "    echo \"Found ggen at: \$ggen_path\""
echo "  else"
echo "    echo \"ggen not found\""
echo "  fi"
echo ""
echo "Execution:"

if ggen_path=$(detect_ggen_binary 2>/dev/null); then
  echo -e "  ${GREEN}✓${NC} Found ggen at: ${GREEN}$ggen_path${NC}"
else
  echo -e "  ${YELLOW}✗${NC} ggen not currently in PATH"
  echo "    To install: cargo install ggen --locked"
fi
echo ""

##############################################################################
# Example 2: Installation if Needed
##############################################################################

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Example 2: Check & Install if Needed${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Code:"
echo "  if ggen_path=\$(install_ggen_if_needed); then"
echo "    echo \"ggen ready at: \$ggen_path\""
echo "  else"
echo "    echo \"Failed to setup ggen\""
echo "    exit 1"
echo "  fi"
echo ""
echo "Execution:"
echo "  (Checking if installation is needed...)"

if ggen_path=$(detect_ggen_binary 2>/dev/null); then
  echo -e "  ${GREEN}✓${NC} ggen already installed"
  echo -e "    Path: ${GREEN}$ggen_path${NC}"
else
  echo -e "  ${YELLOW}ℹ${NC} ggen not found - would run: cargo install ggen --locked"
  echo "    (Skipping actual installation in example)"
fi
echo ""

##############################################################################
# Example 3: Verification
##############################################################################

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Example 3: Binary Verification${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Code:"
echo "  ggen_path=\"/path/to/ggen\""
echo "  if verify_ggen_binary \"\$ggen_path\"; then"
echo "    echo \"Binary is valid and working\""
echo "  else"
echo "    echo \"Binary verification failed\""
echo "    exit 1"
echo "  fi"
echo ""
echo "Execution (testing with nonexistent path):"

if verify_ggen_binary "/nonexistent/ggen" 2>/dev/null; then
  echo -e "  ${RED}✗${NC} Unexpected: nonexistent path was accepted"
else
  echo -e "  ${GREEN}✓${NC} Correctly rejected nonexistent path"
fi
echo ""

##############################################################################
# Example 4: Environment Variable Export
##############################################################################

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Example 4: Environment Variable Export${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Code:"
echo "  if export_ggen_env; then"
echo "    echo \"GGEN_BIN=\$GGEN_BIN\""
echo "    echo \"GGEN_VERSION=\$GGEN_VERSION\""
echo "  fi"
echo ""
echo "Execution:"

# Unset before testing
unset GGEN_BIN 2>/dev/null || true
unset GGEN_VERSION 2>/dev/null || true

if export_ggen_env 2>/dev/null; then
  echo -e "  ${GREEN}✓${NC} Environment variables exported:"
  echo -e "    GGEN_BIN=${GREEN}${GGEN_BIN}${NC}"
  [ -n "${GGEN_VERSION:-}" ] && echo -e "    GGEN_VERSION=${GREEN}${GGEN_VERSION}${NC}"
else
  echo -e "  ${YELLOW}ℹ${NC} ggen not available (cannot set GGEN_BIN)"
  echo "    After installation, this will export:"
  echo "    GGEN_BIN=/path/to/ggen"
  echo "    GGEN_VERSION=6.0.0 (or current version)"
fi
echo ""

##############################################################################
# Example 5: Complete Setup (Recommended)
##############################################################################

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Example 5: Complete Orchestrated Setup (Recommended)${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Code:"
echo "  if init_ggen_setup; then"
echo "    echo \"ggen ready: \$GGEN_BIN\""
echo "    echo \"Version: \$GGEN_VERSION\""
echo "    # Use ggen for generation"
echo "    \$GGEN_BIN sync --dry-run true"
echo "  else"
echo "    echo \"ggen setup failed\""
echo "    exit 1"
echo "  fi"
echo ""
echo "Execution:"

# Unset before testing
unset GGEN_BIN 2>/dev/null || true
unset GGEN_VERSION 2>/dev/null || true

if init_ggen_setup 2>/dev/null; then
  echo -e "  ${GREEN}✓${NC} ggen setup complete:"
  echo -e "    GGEN_BIN=${GREEN}${GGEN_BIN}${NC}"
  echo -e "    GGEN_VERSION=${GREEN}${GGEN_VERSION:-unknown}${NC}"
  echo ""
  echo "  Ready for code generation:"
  echo -e "    ${BLUE}\$ \$GGEN_BIN sync --dry-run true${NC}"
else
  echo -e "  ${YELLOW}ℹ${NC} ggen setup would:"
  echo "    1. Detect existing ggen installation"
  echo "    2. Install via 'cargo install ggen --locked' if needed"
  echo "    3. Verify binary functionality"
  echo "    4. Export GGEN_BIN and GGEN_VERSION variables"
fi
echo ""

##############################################################################
# Example 6: Diagnostics
##############################################################################

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Example 6: Troubleshooting with Diagnostics${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Code:"
echo "  ggen_diagnostics"
echo ""
echo "Execution:"
echo ""

ggen_diagnostics
echo ""

##############################################################################
# Example 7: Integration with main.sh
##############################################################################

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Example 7: Integration with main.sh${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Automatic integration (nothing to do!):"
echo ""
echo "  When you run: ${BLUE}./main.sh start${NC}"
echo ""
echo "  1. main.sh loads ggen-setup.sh module"
echo "  2. init_environment() calls ggen_session_start_hook()"
echo "  3. GGEN_BIN is automatically set and exported"
echo "  4. Ready for use in generation agents"
echo ""
echo "Available after main.sh initialization:"
echo ""
echo "  Environment:"
echo -e "    ${BLUE}\$GGEN_BIN${NC} - Path to ggen binary"
echo -e "    ${BLUE}\$GGEN_VERSION${NC} - ggen version string"
echo ""
echo "  Commands:"
echo -e "    ${BLUE}\$GGEN_BIN sync --dry-run true${NC} - Preview generation"
echo -e "    ${BLUE}\$GGEN_BIN validate --ontology spec.ttl${NC} - Validate"
echo ""

##############################################################################
# Summary
##############################################################################

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Summary${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Core Functions:"
echo -e "  ${GREEN}detect_ggen_binary()${NC}      - Find ggen in PATH or standard locations"
echo -e "  ${GREEN}install_ggen_if_needed()${NC}  - Install ggen if not found"
echo -e "  ${GREEN}verify_ggen_binary()${NC}      - Verify binary is valid and working"
echo -e "  ${GREEN}export_ggen_env()${NC}        - Export GGEN_BIN environment variable"
echo -e "  ${GREEN}init_ggen_setup()${NC}        - Complete orchestrated setup (recommended)"
echo -e "  ${GREEN}ggen_diagnostics()${NC}       - Troubleshooting and diagnostics"
echo ""
echo "Integration:"
echo -e "  ${GREEN}./main.sh start${NC}            - Automatic ggen initialization"
echo -e "  ${GREEN}./main.sh ggen-diagnostics${NC} - Show ggen status"
echo ""
echo "Documentation:"
echo -e "  See: ${BLUE}GGEN_SETUP_INTEGRATION.md${NC}"
echo ""
