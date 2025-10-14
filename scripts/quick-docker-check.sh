#!/usr/bin/env bash

# Quick Docker Health Check
# Runs with strict timeouts to avoid hanging

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${YELLOW}Docker Quick Health Check${NC}"
echo "================================"

# Check 1: Docker command exists
if command -v docker &> /dev/null; then
    echo -e "${GREEN}✓${NC} Docker command is available"
else
    echo -e "${RED}✗${NC} Docker command not found"
    exit 1
fi

# Check 2: Docker version (with timeout)
if timeout 5 docker --version &> /dev/null; then
    echo -e "${GREEN}✓${NC} Docker version: $(docker --version)"
else
    echo -e "${RED}✗${NC} Docker version check timed out"
fi

# Check 3: Docker daemon (with timeout)
echo -n "Checking Docker daemon... "
if timeout 5 docker info &> /dev/null; then
    echo -e "${GREEN}✓ Running${NC}"
else
    echo -e "${RED}✗ Not responding (may be stopped or hanging)${NC}"
    echo ""
    echo "Troubleshooting suggestions:"
    echo "  1. Restart Docker: sudo systemctl restart docker (Linux)"
    echo "  2. Restart Docker Desktop (macOS/Windows)"
    echo "  3. Check Docker socket: ls -la /var/run/docker.sock"
    exit 1
fi

# Check 4: Container count (with timeout)
echo -n "Checking containers... "
if CONTAINER_COUNT=$(timeout 5 docker ps -a -q | wc -l 2>/dev/null); then
    echo -e "${GREEN}✓ ${CONTAINER_COUNT} containers found${NC}"
else
    echo -e "${RED}✗ Container query timed out${NC}"
fi

# Check 5: Recent testcontainers
echo -n "Checking for testcontainers... "
if TESTCONTAINER_COUNT=$(timeout 5 docker ps -a --filter "name=testcontainers" --format '{{.Names}}' 2>/dev/null | wc -l); then
    if [ "$TESTCONTAINER_COUNT" -gt 0 ]; then
        echo -e "${GREEN}✓ ${TESTCONTAINER_COUNT} found${NC}"
        timeout 5 docker ps -a --filter "name=testcontainers" --format 'table {{.Names}}\t{{.Status}}' 2>/dev/null | head -5
    else
        echo -e "${YELLOW}⚠ None found${NC}"
    fi
else
    echo -e "${RED}✗ Query timed out${NC}"
fi

echo ""
echo -e "${GREEN}Docker daemon is operational${NC}"
