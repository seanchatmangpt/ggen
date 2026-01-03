#!/bin/bash

# =============================================================================
# testcontainers Availability Checker
# =============================================================================
# Checks if testcontainers infrastructure is available in current environment
# Usage: ./scripts/check-testcontainers.sh
# =============================================================================

set -e

echo "=== testcontainers Environment Check ==="
echo ""

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track results
DOCKER_AVAILABLE=0
TESTCONTAINERS_CRATE=0
DOCKER_SOCKET=0
CAN_RUN_TESTS=0

# 1. Check if Docker CLI is installed
echo "[1/5] Checking Docker CLI..."
if command -v docker &> /dev/null; then
    DOCKER_VERSION=$(docker --version)
    echo -e "${GREEN}✓${NC} Docker CLI found: $DOCKER_VERSION"
    DOCKER_AVAILABLE=1
else
    echo -e "${RED}✗${NC} Docker CLI not found"
fi
echo ""

# 2. Check if Docker daemon is accessible
echo "[2/5] Checking Docker daemon accessibility..."
if command -v docker &> /dev/null; then
    if docker info &> /dev/null; then
        echo -e "${GREEN}✓${NC} Docker daemon is accessible"
        DOCKER_SOCKET=1
    else
        echo -e "${YELLOW}⚠${NC} Docker CLI exists but daemon not accessible"
        echo "   Try: sudo service docker start"
        echo "   Or check: docker info"
    fi
else
    echo -e "${RED}✗${NC} Docker not installed (skipped)"
fi
echo ""

# 3. Check testcontainers crate in Cargo.toml
echo "[3/5] Checking testcontainers in Cargo.toml..."
if grep -q "testcontainers" Cargo.toml; then
    VERSION=$(grep "testcontainers" Cargo.toml | head -1)
    echo -e "${GREEN}✓${NC} testcontainers found in Cargo.toml"
    echo "   $VERSION"
    TESTCONTAINERS_CRATE=1
else
    echo -e "${RED}✗${NC} testcontainers not in Cargo.toml"
fi
echo ""

# 4. Check testcontainers-modules
echo "[4/5] Checking testcontainers-modules in Cargo.toml..."
if grep -q "testcontainers-modules" Cargo.toml; then
    VERSION=$(grep "testcontainers-modules" Cargo.toml | head -1)
    echo -e "${GREEN}✓${NC} testcontainers-modules found in Cargo.toml"
    echo "   $VERSION"
else
    echo -e "${YELLOW}⚠${NC} testcontainers-modules not in Cargo.toml"
fi
echo ""

# 5. Check ggen-e2e crate
echo "[5/5] Checking ggen-e2e crate..."
if [ -d "crates/ggen-e2e" ]; then
    echo -e "${GREEN}✓${NC} ggen-e2e crate exists"
    if [ -f "crates/ggen-e2e/Cargo.toml" ]; then
        echo -e "${GREEN}✓${NC} ggen-e2e/Cargo.toml exists"
        CAN_RUN_TESTS=1
    fi
else
    echo -e "${RED}✗${NC} ggen-e2e crate not found"
fi
echo ""

# Summary
echo "=== Summary ==="
echo ""

if [ $DOCKER_AVAILABLE -eq 1 ] && [ $DOCKER_SOCKET -eq 1 ] && [ $TESTCONTAINERS_CRATE -eq 1 ] && [ $CAN_RUN_TESTS -eq 1 ]; then
    echo -e "${GREEN}✓ READY${NC} - testcontainers is fully available"
    echo ""
    echo "You can run E2E tests with:"
    echo "  cargo test --package ggen-e2e"
    exit 0
elif [ $DOCKER_SOCKET -eq 1 ] && [ $TESTCONTAINERS_CRATE -eq 1 ]; then
    echo -e "${YELLOW}⚠ PARTIAL${NC} - Docker daemon is accessible, but some components missing"
    echo ""
    echo "You can try running E2E tests, but some may fail:"
    echo "  cargo test --package ggen-e2e"
    exit 1
else
    echo -e "${RED}✗ NOT AVAILABLE${NC} - testcontainers infrastructure missing"
    echo ""

    if [ $DOCKER_AVAILABLE -eq 0 ]; then
        echo "To fix:"
        echo "  1. Install Docker:"
        echo "     - Ubuntu/Debian: sudo apt-get install docker.io"
        echo "     - macOS: brew install docker"
        echo "     - Or use Docker Desktop"
    fi

    if [ $DOCKER_SOCKET -eq 0 ] && [ $DOCKER_AVAILABLE -eq 1 ]; then
        echo "To fix:"
        echo "  1. Start Docker daemon:"
        echo "     sudo service docker start"
        echo "  2. Or add current user to docker group:"
        echo "     sudo usermod -aG docker $USER"
        echo "     newgrp docker"
    fi

    if [ $TESTCONTAINERS_CRATE -eq 0 ]; then
        echo "To fix:"
        echo "  1. Add to Cargo.toml:"
        echo "     testcontainers = \"0.25\""
        echo "     testcontainers-modules = \"0.13\""
    fi

    exit 2
fi
