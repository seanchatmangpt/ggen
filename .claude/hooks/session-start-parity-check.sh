#!/bin/bash
# Enhanced Session Start Hook - Environment Parity Check (Simplified for Iteration)
# Purpose: Ensure local environment matches Claude Code web environment
# Usage: Run this locally to verify your environment, then integrate into settings.json
#
# Matching requirements from research:
#   - Ubuntu 24.04.3 LTS
#   - Rust 1.91.1 + cargo-make
#   - Node 22.21.1, Python 3.11.14, Go 1.24.7, Java 21, Ruby 3.3.6
#   - GCC 13.3, Clang 18.1, pkg-config 1.8.1
#   - PostgreSQL 16 + Redis 7.0 clients
#   - Docker (for testcontainers)

PROJECT_DIR="${CLAUDE_PROJECT_DIR:-.}"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Counters
PASSED=0
FAILED=0
WARN=0

echo -e "${BLUE}╔══════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║ Environment Parity Check - Claude Code Web Mirror   ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════╝${NC}"
echo ""

# Helper: Check command exists
check_cmd() {
    if command -v "$1" &> /dev/null; then
        local version=$("$1" --version 2>&1 | head -1 || echo "found")
        echo -e "${GREEN}✓${NC} $2: $version"
        ((PASSED++))
    else
        echo -e "${RED}✗${NC} $2: NOT FOUND"
        ((FAILED++))
    fi
}

# Helper: Warning if missing
check_cmd_opt() {
    if command -v "$1" &> /dev/null; then
        local version=$("$1" --version 2>&1 | head -1 || echo "found")
        echo -e "${GREEN}✓${NC} $2: $version"
        ((PASSED++))
    else
        echo -e "${YELLOW}⚠${NC} $2: NOT FOUND (optional)"
        ((WARN++))
    fi
}

# =============================================================================
echo -e "${CYAN}[Core Requirements]${NC}"
check_cmd rustc "Rust compiler"
check_cmd cargo "Cargo"
check_cmd git "Git"

# cargo-make check (don't fail, just warn)
if command -v cargo-make &> /dev/null; then
    VERSION=$(cargo-make --version 2>&1)
    echo -e "${GREEN}✓${NC} cargo-make: $VERSION"
    ((PASSED++))
else
    echo -e "${YELLOW}⚠${NC} cargo-make: NOT FOUND (run: cargo install cargo-make)"
    ((WARN++))
fi

echo ""

# =============================================================================
echo -e "${CYAN}[Languages]${NC}"
check_cmd node "Node.js"
check_cmd python3 "Python"
check_cmd go "Go"
check_cmd java "Java"
check_cmd ruby "Ruby"

echo ""

# =============================================================================
echo -e "${CYAN}[Package Managers]${NC}"
check_cmd_opt npm "npm"
check_cmd_opt yarn "Yarn"
check_cmd_opt pip3 "pip3"
check_cmd_opt gem "gem"

echo ""

# =============================================================================
echo -e "${CYAN}[Build Tools]${NC}"
check_cmd gcc "GCC"
check_cmd clang "Clang"
check_cmd make "GNU Make"
check_cmd pkg-config "pkg-config"

echo ""

# =============================================================================
echo -e "${CYAN}[Database Clients]${NC}"
check_cmd_opt psql "PostgreSQL client"
check_cmd_opt redis-cli "Redis CLI"
check_cmd_opt sqlite3 "SQLite3"

echo ""

# =============================================================================
echo -e "${CYAN}[Docker & Testcontainers]${NC}"

if command -v docker &> /dev/null; then
    VERSION=$(docker --version)
    echo -e "${GREEN}✓${NC} Docker: $VERSION"
    ((PASSED++))

    if docker info &> /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Docker daemon: Running"
        ((PASSED++))
    else
        echo -e "${YELLOW}⚠${NC} Docker daemon: Not accessible"
        ((WARN++))
    fi
else
    echo -e "${YELLOW}⚠${NC} Docker: NOT FOUND (needed for E2E tests)"
    ((WARN++))
fi

# Check testcontainers in Cargo.toml
if grep -q "testcontainers" "$PROJECT_DIR/Cargo.toml" 2>/dev/null; then
    echo -e "${GREEN}✓${NC} testcontainers: In Cargo.toml"
    ((PASSED++))
else
    echo -e "${YELLOW}⚠${NC} testcontainers: Not configured"
    ((WARN++))
fi

echo ""

# =============================================================================
echo -e "${CYAN}[Project Structure]${NC}"

[ -d "$PROJECT_DIR/.specify" ] && echo -e "${GREEN}✓${NC} .specify/ exists" && ((PASSED++)) || echo -e "${RED}✗${NC} .specify/ missing" && ((FAILED++))
[ -f "$PROJECT_DIR/Makefile.toml" ] && echo -e "${GREEN}✓${NC} Makefile.toml exists" && ((PASSED++)) || echo -e "${RED}✗${NC} Makefile.toml missing" && ((FAILED++))
[ -d "$PROJECT_DIR/crates/ggen-e2e" ] && echo -e "${GREEN}✓${NC} ggen-e2e crate exists" && ((PASSED++)) || echo -e "${YELLOW}⚠${NC} ggen-e2e missing" && ((WARN++))

echo ""

# =============================================================================
echo -e "${CYAN}[Environment Setup]${NC}"

export RUST_BACKTRACE=1
export RUST_LOG=info
export CARGO_TERM_COLOR=always
export NODE_ENV=development

echo -e "${GREEN}✓${NC} Set RUST_BACKTRACE=1"
echo -e "${GREEN}✓${NC} Set RUST_LOG=info"
echo -e "${GREEN}✓${NC} Set CARGO_TERM_COLOR=always"
echo -e "${GREEN}✓${NC} Set NODE_ENV=development"

echo ""

# =============================================================================
TOTAL=$((PASSED + FAILED + WARN))
echo -e "${BLUE}╔══════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║ Summary:${NC} Passed: ${GREEN}$PASSED${NC} | Failed: ${RED}$FAILED${NC} | Warnings: ${YELLOW}$WARN${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════╝${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ Environment Ready${NC}"
    exit 0
else
    echo -e "${RED}✗ Missing critical components${NC}"
    exit 1
fi
