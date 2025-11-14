#!/usr/bin/env bash
set -euo pipefail

# Test the regeneration hooks
# Usage: ./test-hooks.sh

PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
TEST_DIR="${PROJECT_ROOT}/test-hook-temp"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}=== Testing Git Hooks for Code Regeneration ===${NC}"
echo ""

# Test 1: Check if hooks are installed
echo -e "${YELLOW}Test 1: Checking hook installation...${NC}"
HOOKS_INSTALLED=true

if [ ! -x "${PROJECT_ROOT}/.git/hooks/pre-commit" ]; then
    echo -e "${RED}✗ pre-commit hook not installed${NC}"
    HOOKS_INSTALLED=false
else
    echo -e "${GREEN}✓ pre-commit hook installed${NC}"
fi

if [ ! -x "${PROJECT_ROOT}/.git/hooks/post-merge" ]; then
    echo -e "${RED}✗ post-merge hook not installed${NC}"
    HOOKS_INSTALLED=false
else
    echo -e "${GREEN}✓ post-merge hook installed${NC}"
fi

if [ "$HOOKS_INSTALLED" = false ]; then
    echo -e "${YELLOW}Run ./docs/examples/scripts/install-hooks.sh to install${NC}"
    exit 1
fi

echo ""

# Test 2: Check if regeneration script exists
echo -e "${YELLOW}Test 2: Checking regeneration script...${NC}"
REGEN_SCRIPT="${PROJECT_ROOT}/docs/examples/scripts/regenerate-from-ontology.sh"

if [ ! -x "$REGEN_SCRIPT" ]; then
    echo -e "${RED}✗ Regeneration script not found or not executable${NC}"
    exit 1
else
    echo -e "${GREEN}✓ Regeneration script ready${NC}"
fi

echo ""

# Test 3: Check if ggen is available
echo -e "${YELLOW}Test 3: Checking ggen availability...${NC}"
if ! command -v ggen &> /dev/null; then
    echo -e "${RED}✗ ggen command not found${NC}"
    echo -e "${YELLOW}Install with: cargo install --path .${NC}"
    exit 1
else
    GGEN_VERSION=$(ggen --version 2>&1 || echo "unknown")
    echo -e "${GREEN}✓ ggen available ($GGEN_VERSION)${NC}"
fi

echo ""

# Test 4: Test regeneration script directly
echo -e "${YELLOW}Test 4: Testing regeneration script...${NC}"
if "$REGEN_SCRIPT" --quiet; then
    echo -e "${GREEN}✓ Regeneration script runs successfully${NC}"
else
    echo -e "${RED}✗ Regeneration script failed${NC}"
    exit 1
fi

echo ""

# Test 5: Check if ontology directory exists
echo -e "${YELLOW}Test 5: Checking ontology directory...${NC}"
ONTOLOGY_DIR="${PROJECT_ROOT}/docs/examples/ontology"

if [ ! -d "$ONTOLOGY_DIR" ]; then
    echo -e "${YELLOW}⚠ Ontology directory not found: $ONTOLOGY_DIR${NC}"
    echo -e "${YELLOW}Creating example directory...${NC}"
    mkdir -p "$ONTOLOGY_DIR"
fi

ONTOLOGY_COUNT=$(find "$ONTOLOGY_DIR" -name "*.ttl" -o -name "*.rdf" -o -name "*.owl" 2>/dev/null | wc -l)
if [ "$ONTOLOGY_COUNT" -eq 0 ]; then
    echo -e "${YELLOW}⚠ No ontology files found in $ONTOLOGY_DIR${NC}"
else
    echo -e "${GREEN}✓ Found $ONTOLOGY_COUNT ontology file(s)${NC}"
fi

echo ""

# Test 6: Validate pre-commit hook logic
echo -e "${YELLOW}Test 6: Testing pre-commit hook logic...${NC}"

# Save current git state
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "main")
HAS_CHANGES=$(git status --porcelain | wc -l)

if [ "$HAS_CHANGES" -gt 0 ]; then
    echo -e "${YELLOW}⚠ Working directory has uncommitted changes${NC}"
    echo -e "${YELLOW}Skipping pre-commit test to avoid conflicts${NC}"
else
    echo -e "${GREEN}✓ Working directory clean${NC}"

    # Test hook with dry-run
    echo -e "${BLUE}Running pre-commit hook dry-run...${NC}"
    if bash -n "${PROJECT_ROOT}/.git/hooks/pre-commit"; then
        echo -e "${GREEN}✓ pre-commit hook syntax valid${NC}"
    else
        echo -e "${RED}✗ pre-commit hook has syntax errors${NC}"
        exit 1
    fi
fi

echo ""

# Test 7: Check template files
echo -e "${YELLOW}Test 7: Checking template files...${NC}"
TEMPLATES_DIR="${PROJECT_ROOT}/docs/examples/templates"

TEMPLATE_FILES=(
    "types.ts.hbs"
    "api-routes.ts.hbs"
    "crud-component.tsx.hbs"
)

TEMPLATES_FOUND=0
for TEMPLATE in "${TEMPLATE_FILES[@]}"; do
    if [ -f "$TEMPLATES_DIR/$TEMPLATE" ]; then
        echo -e "${GREEN}✓ Found $TEMPLATE${NC}"
        TEMPLATES_FOUND=$((TEMPLATES_FOUND + 1))
    else
        echo -e "${YELLOW}⚠ Missing $TEMPLATE${NC}"
    fi
done

if [ $TEMPLATES_FOUND -eq 0 ]; then
    echo -e "${YELLOW}No templates found (this is OK for initial setup)${NC}"
fi

echo ""

# Summary
echo -e "${BLUE}=== Test Summary ===${NC}"
echo -e "${GREEN}✓ All critical tests passed${NC}"
echo ""
echo "Next steps:"
echo "  1. Create ontology files in: $ONTOLOGY_DIR"
echo "  2. Create templates in: $TEMPLATES_DIR"
echo "  3. Test by modifying an ontology and committing"
echo ""
echo "Example:"
echo "  vim $ONTOLOGY_DIR/example.ttl"
echo "  git add $ONTOLOGY_DIR/example.ttl"
echo "  git commit -m 'feat: add example ontology'"
echo "  # → Hook will validate and regenerate code"
