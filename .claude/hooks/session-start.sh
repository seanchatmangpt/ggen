#!/bin/bash
# Session Start Hook - ggen v5.2.0
# Runs at the beginning of each Claude Code session
# Purpose: Initialize environment, verify toolchain, enforce SLOs
# Constitutional Alignment: Poka-Yoke error-proofing, deterministic validation

set -e

PROJECT_DIR="${CLAUDE_PROJECT_DIR:-.}"
CLAUDE_ENV_FILE="${CLAUDE_ENV_FILE:-}"

# MSRV and recommended versions (from CLAUDE.md)
MSRV_MAJOR=1
MSRV_MINOR=75
RECOMMENDED_VERSION="1.91.1"

# Color codes for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║ ggen v5.2.0 - Session Initialization                      ║${NC}"
echo -e "${BLUE}║ Bleeding Edge 2026 Edition                                ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

# 1. Set project environment variables
export RUST_BACKTRACE=1
export RUST_LOG=info
export CARGO_TERM_COLOR=always
export NODE_ENV=development
export GGEN_VERSION="5.2.0"

# 2. Load custom environment variables if session file exists
if [ -n "$CLAUDE_ENV_FILE" ] && [ -f "$CLAUDE_ENV_FILE" ]; then
    echo -e "${BLUE}[ggen] Loading session environment...${NC}"
    set +e
    source "$CLAUDE_ENV_FILE"
    set -e
fi

# 3. Verify Rust toolchain with MSRV check
if command -v rustc &> /dev/null; then
    RUSTC_VERSION=$(rustc --version)
    RUSTC_SEMVER=$(rustc --version | grep -oP '\d+\.\d+\.\d+')

    # Extract major.minor for MSRV check
    RUST_MAJOR=$(echo "$RUSTC_SEMVER" | cut -d. -f1)
    RUST_MINOR=$(echo "$RUSTC_SEMVER" | cut -d. -f2)

    # Check MSRV (1.75+)
    if [ "$RUST_MAJOR" -lt "$MSRV_MAJOR" ] || ([ "$RUST_MAJOR" -eq "$MSRV_MAJOR" ] && [ "$RUST_MINOR" -lt "$MSRV_MINOR" ]); then
        echo -e "${RED}[ggen] ✗ Rust version $RUSTC_SEMVER below MSRV ${MSRV_MAJOR}.${MSRV_MINOR}${NC}"
        echo -e "${YELLOW}[ggen] Update with: rustup update stable${NC}"
        exit 1
    fi

    # Check if using recommended version
    if [ "$RUSTC_SEMVER" = "$RECOMMENDED_VERSION" ]; then
        echo -e "${GREEN}[ggen] ✓ Rust $RUSTC_SEMVER (recommended)${NC}"
    else
        echo -e "${GREEN}[ggen] ✓ Rust $RUSTC_SEMVER${NC}"
        echo -e "${YELLOW}[ggen] ℹ Recommended version: $RECOMMENDED_VERSION${NC}"
    fi
else
    echo -e "${RED}[ggen] ✗ Rust toolchain not found${NC}"
    echo -e "${YELLOW}[ggen] Install from: https://rustup.rs/${NC}"
    exit 1
fi

# 4. Verify cargo-make installed (CRITICAL: NEVER use direct cargo!)
if ! command -v cargo-make &> /dev/null; then
    echo -e "${YELLOW}[ggen] Installing cargo-make (required for SLO enforcement)...${NC}"
    cargo install cargo-make --quiet || {
        echo -e "${RED}[ggen] ✗ cargo-make installation failed${NC}"
        echo -e "${YELLOW}[ggen] Manual install: cargo install cargo-make${NC}"
        exit 1
    }
    # Verify installation succeeded
    if ! command -v cargo-make &> /dev/null; then
        echo -e "${RED}[ggen] ✗ cargo-make not available - CRITICAL${NC}"
        echo -e "${YELLOW}[ggen] All build commands require cargo-make for timeout enforcement${NC}"
        exit 1
    else
        echo -e "${GREEN}[ggen] ✓ cargo-make installed${NC}"
    fi
else
    CARGO_MAKE_VERSION=$(cargo make --version 2>&1 | head -n1 || echo "unknown")
    echo -e "${GREEN}[ggen] ✓ cargo-make available ($CARGO_MAKE_VERSION)${NC}"
fi

# 5. Verify Makefile.toml exists
if [ ! -f "$PROJECT_DIR/Makefile.toml" ]; then
    echo -e "${RED}[ggen] ✗ Makefile.toml not found${NC}"
    echo -e "${YELLOW}[ggen] Build system requires Makefile.toml for SLO enforcement${NC}"
    exit 1
else
    echo -e "${GREEN}[ggen] ✓ Makefile.toml present${NC}"
fi

# 6. Check git status
if cd "$PROJECT_DIR" && git rev-parse --git-dir > /dev/null 2>&1; then
    GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
    echo -e "${GREEN}[ggen] ✓ Branch: $GIT_BRANCH${NC}"

    # Check for uncommitted changes
    if ! git diff-index --quiet HEAD --; then
        CHANGES=$(git status --short | wc -l)
        echo -e "${YELLOW}[ggen] ⚠ Uncommitted changes: $CHANGES files${NC}"
    fi

    # Show last commit
    LAST_COMMIT=$(git log -1 --pretty=format:"%h - %s" 2>/dev/null || echo "No commits")
    echo -e "${BLUE}[ggen] Last commit: $LAST_COMMIT${NC}"
fi

# 7. Verify .specify/ structure (RDF-first specification system)
if [ ! -d "$PROJECT_DIR/.specify" ]; then
    echo -e "${YELLOW}[ggen] ⚠ .specify/ directory not found${NC}"
    echo -e "${YELLOW}[ggen] TTL specifications should be in .specify/specs/NNN-feature/${NC}"
else
    SPEC_COUNT=$(find "$PROJECT_DIR/.specify" -name "*.ttl" 2>/dev/null | wc -l)
    echo -e "${GREEN}[ggen] ✓ .specify/ present ($SPEC_COUNT TTL files)${NC}"
fi

# 8. Check workspace structure
if [ -f "$PROJECT_DIR/Cargo.toml" ]; then
    if grep -q "^\[workspace\]" "$PROJECT_DIR/Cargo.toml"; then
        CRATE_COUNT=$(find "$PROJECT_DIR/crates" -name "Cargo.toml" 2>/dev/null | wc -l)
        echo -e "${GREEN}[ggen] ✓ Workspace with $CRATE_COUNT crates${NC}"
    fi
fi

echo ""
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║ THREE PARADIGM SHIFTS (Bleeding Edge 2026)                ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo -e "${YELLOW}1. Big Bang 80/20${NC} - Specification closure BEFORE implementation"
echo -e "   ${GREEN}/speckit-verify${NC} → Verify 100% closure → Single-pass construction"
echo -e ""
echo -e "${YELLOW}2. EPIC 9${NC} - Parallel-first atomic cognitive cycle (2.8-4.4x speedup)"
echo -e "   ${GREEN}/bb80-parallel${NC} → 10 agents in parallel → Collision detection → Convergence"
echo -e ""
echo -e "${YELLOW}3. Deterministic Validation${NC} - Receipts replace review"
echo -e "   ${GREEN}cargo make pre-commit${NC} → Timestamped receipts → Evidence-based done"
echo ""
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║ FAST FEEDBACK LOOP (SLO Enforced)                         ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo -e "  ${GREEN}cargo make check${NC}          - Compilation (<5s SLO)"
echo -e "  ${GREEN}cargo make test-unit${NC}      - Unit tests (<10s SLO)"
echo -e "  ${GREEN}cargo make lint${NC}           - Clippy (<60s SLO)"
echo -e "  ${GREEN}cargo make test${NC}           - All tests (<30s SLO)"
echo -e "  ${GREEN}cargo make pre-commit${NC}     - Full validation (receipts)"
echo -e "  ${GREEN}cargo make ci${NC}             - Complete CI pipeline"
echo ""
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║ EPIC 9 WORKFLOW (Non-Trivial Tasks)                       ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo -e "  ${GREEN}/speckit-verify [feature]${NC} - Verify spec closure (MANDATORY first step)"
echo -e "  ${GREEN}/bb80-parallel \"[spec]\"${NC}  - Orchestrate 10+ parallel agents"
echo -e "  ${GREEN}/collision-detect${NC}         - Analyze agent convergence"
echo -e "  ${GREEN}/convergence${NC}              - Synthesize best solution"
echo ""
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║ QUALITY ASSURANCE COMMANDS                                ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo -e "  ${GREEN}/speckit-check${NC}            - Validate RDF specs (.specify/)"
echo -e "  ${GREEN}/test-audit${NC}               - Mutation testing + assertion analysis"
echo -e "  ${GREEN}/review-errors${NC}            - Audit error handling (no unwrap in prod)"
echo -e "  ${GREEN}/optimize${NC}                 - Performance analysis + SLO checks"
echo -e "  ${GREEN}/bench-compare${NC}            - Compare benchmarks across commits"
echo ""
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║ SPECIALIZED AGENTS                                         ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo -e "  ${GREEN}rust-coder${NC}                - Rust implementation (type-first thinking)"
echo -e "  ${GREEN}test-engineer${NC}             - Chicago TDD (state-based testing)"
echo -e "  ${GREEN}reviewer${NC}                  - Evidence-based code review"
echo -e "  ${GREEN}speckit-architect${NC}         - RDF/Turtle specifications"
echo -e "  ${GREEN}bb80-parallel-task-coordinator${NC} - EPIC 9 orchestration"
echo -e "  ${GREEN}bb80-collision-detector${NC}   - Analyze agent overlaps"
echo -e "  ${GREEN}bb80-convergence-orchestrator${NC} - Synthesize results"
echo ""
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║ CRITICAL CONSTITUTIONAL RULES                              ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo -e "${RED}✗ NEVER use direct cargo commands${NC} (always cargo make)"
echo -e "${RED}✗ NEVER unwrap/expect in production${NC} (use Result<T,E>)"
echo -e "${RED}✗ NEVER iterate without spec closure${NC} (use /speckit-verify)"
echo -e "${RED}✗ NEVER sequential for non-trivial tasks${NC} (use EPIC 9)"
echo -e "${RED}✗ NEVER narrative reviews${NC} (collect deterministic receipts)"
echo -e "${RED}✗ NEVER ignore Andon signals${NC} (stop the line on RED)"
echo ""
echo -e "${GREEN}✓ Specification closure before implementation${NC}"
echo -e "${GREEN}✓ Parallel-first for all non-trivial tasks${NC}"
echo -e "${GREEN}✓ Evidence over opinion (receipts replace review)${NC}"
echo -e "${GREEN}✓ Type-first thinking (express in types, not runtime)${NC}"
echo -e "${GREEN}✓ Zero-cost abstractions (generics > trait objects)${NC}"
echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}Session ready. Constitutional equation:${NC}"
echo -e "${YELLOW}spec.md = μ(feature.ttl) | EPIC 9 is default | Receipts replace review${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo ""

# 9. Save environment file for future use
if [ -n "$CLAUDE_ENV_FILE" ]; then
    mkdir -p "$(dirname "$CLAUDE_ENV_FILE")"
    {
        echo "# Generated by session-start.sh - ggen v5.2.0"
        echo "export RUST_BACKTRACE=1"
        echo "export RUST_LOG=info"
        echo "export CARGO_TERM_COLOR=always"
        echo "export NODE_ENV=development"
        echo "export GGEN_VERSION=5.2.0"
        echo "# MSRV: 1.75+, Recommended: 1.91.1"
        echo "# SLOs: check <5s, test <30s, lint <60s"
    } > "$CLAUDE_ENV_FILE"
fi

exit 0
