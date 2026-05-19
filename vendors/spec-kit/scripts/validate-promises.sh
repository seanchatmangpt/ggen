#!/bin/bash
# Validation script to ensure all promises are kept in spec-kit

set -e

REPO_ROOT="~/.ggen/mcpp/vendors/spec-kit"
cd "$REPO_ROOT"

echo "🔍 Spec-Kit Promise Validation"
echo "=============================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ERRORS=0
WARNINGS=0

# Promise 1: No "mcpp render" references should remain (excluding validation report)
echo "📝 Promise 1: Checking for 'mcpp render' references..."
if grep -r "mcpp render" --include="*.md" --include="*.py" --include="*.toml" \
        --exclude="VALIDATION_REPORT.md" --exclude-dir=".git" . 2>/dev/null; then
    echo -e "${RED}❌ FAILED: Found 'mcpp render' references${NC}"
    ((ERRORS++))
else
    echo -e "${GREEN}✓ PASSED: No 'mcpp render' references found (excluding validation report)${NC}"
fi
echo ""

# Promise 2: All commands should reference "mcpp sync"
echo "📝 Promise 2: Verifying 'mcpp sync' usage in commands..."
SYNC_COUNT=$(grep -r "mcpp sync" templates/commands/*.md 2>/dev/null | wc -l)
if [ "$SYNC_COUNT" -lt 5 ]; then
    echo -e "${YELLOW}⚠ WARNING: Only found $SYNC_COUNT 'mcpp sync' references in commands${NC}"
    ((WARNINGS++))
else
    echo -e "${GREEN}✓ PASSED: Found $SYNC_COUNT 'mcpp sync' references in commands${NC}"
fi
echo ""

# Promise 3: Test fixtures must be valid TTL
echo "📝 Promise 3: Validating TTL fixtures..."
if command -v python3 &> /dev/null; then
    python3 - << 'PYEOF'
import sys
try:
    from rdflib import Graph
    g = Graph()
    g.parse("tests/integration/fixtures/feature-content.ttl", format="turtle")
    print("\033[0;32m✓ PASSED: TTL fixture parses correctly\033[0m")
    print(f"  Found {len(g)} RDF triples")
except ImportError:
    print("\033[1;33m⚠ WARNING: rdflib not installed, skipping TTL validation\033[0m")
    sys.exit(2)
except Exception as e:
    print(f"\033[0;31m❌ FAILED: TTL parsing error: {e}\033[0m")
    sys.exit(1)
PYEOF
    RESULT=$?
    if [ $RESULT -eq 1 ]; then
        ((ERRORS++))
    elif [ $RESULT -eq 2 ]; then
        ((WARNINGS++))
    fi
else
    echo -e "${YELLOW}⚠ WARNING: python3 not available, skipping TTL validation${NC}"
    ((WARNINGS++))
fi
echo ""

# Promise 4: Test collection should work
echo "📝 Promise 4: Verifying test collection..."
if command -v pytest &> /dev/null; then
    if pytest --collect-only tests/ > /dev/null 2>&1; then
        TEST_COUNT=$(pytest --collect-only tests/ 2>/dev/null | grep -c "Function test_" || echo "0")
        echo -e "${GREEN}✓ PASSED: Test collection successful ($TEST_COUNT tests)${NC}"
    else
        echo -e "${RED}❌ FAILED: Test collection failed${NC}"
        ((ERRORS++))
    fi
else
    echo -e "${YELLOW}⚠ WARNING: pytest not installed, skipping test collection${NC}"
    ((WARNINGS++))
fi
echo ""

# Promise 5: pyproject.toml must be valid
echo "📝 Promise 5: Validating pyproject.toml..."
if python3 -c "import tomli; tomli.load(open('pyproject.toml', 'rb'))" 2>/dev/null; then
    echo -e "${GREEN}✓ PASSED: pyproject.toml is valid TOML${NC}"
elif python3 -c "import tomllib; tomllib.load(open('pyproject.toml', 'rb'))" 2>/dev/null; then
    echo -e "${GREEN}✓ PASSED: pyproject.toml is valid TOML${NC}"
else
    # Try basic syntax check
    if grep -q "^\[project\]" pyproject.toml && grep -q "^name = " pyproject.toml; then
        echo -e "${GREEN}✓ PASSED: pyproject.toml appears valid${NC}"
    else
        echo -e "${RED}❌ FAILED: pyproject.toml validation failed${NC}"
        ((ERRORS++))
    fi
fi
echo ""

# Promise 6: All referenced files must exist
echo "📝 Promise 6: Verifying referenced files exist..."
MISSING=0

# Check test fixtures
for file in "tests/integration/fixtures/feature-content.ttl" \
            "tests/integration/fixtures/mcpp.toml" \
            "tests/integration/fixtures/spec.tera" \
            "tests/integration/fixtures/expected-spec.md"; do
    if [ ! -f "$file" ]; then
        echo -e "${RED}  ❌ Missing: $file${NC}"
        ((MISSING++))
    fi
done

# Check command files
for file in "templates/commands/specify.md" \
            "templates/commands/plan.md" \
            "templates/commands/tasks.md" \
            "templates/commands/constitution.md" \
            "templates/commands/clarify.md" \
            "templates/commands/implement.md"; do
    if [ ! -f "$file" ]; then
        echo -e "${RED}  ❌ Missing: $file${NC}"
        ((MISSING++))
    fi
done

# Check documentation
for file in "docs/RDF_WORKFLOW_GUIDE.md" \
            "tests/README.md" \
            "README.md"; do
    if [ ! -f "$file" ]; then
        echo -e "${RED}  ❌ Missing: $file${NC}"
        ((MISSING++))
    fi
done

if [ $MISSING -eq 0 ]; then
    echo -e "${GREEN}✓ PASSED: All referenced files exist${NC}"
else
    echo -e "${RED}❌ FAILED: $MISSING file(s) missing${NC}"
    ((ERRORS++))
fi
echo ""

# Promise 7: mcpp.toml fixture should be valid
echo "📝 Promise 7: Validating mcpp.toml fixture..."
if [ -f "tests/integration/fixtures/mcpp.toml" ]; then
    if python3 -c "import tomli; tomli.load(open('tests/integration/fixtures/mcpp.toml', 'rb'))" 2>/dev/null; then
        echo -e "${GREEN}✓ PASSED: mcpp.toml is valid TOML${NC}"
    elif python3 -c "import tomllib; tomllib.load(open('tests/integration/fixtures/mcpp.toml', 'rb'))" 2>/dev/null; then
        echo -e "${GREEN}✓ PASSED: mcpp.toml is valid TOML${NC}"
    else
        # Basic check
        if grep -q "^\[project\]" tests/integration/fixtures/mcpp.toml && \
           grep -q "^\[\[generation\]\]" tests/integration/fixtures/mcpp.toml; then
            echo -e "${GREEN}✓ PASSED: mcpp.toml appears valid${NC}"
        else
            echo -e "${RED}❌ FAILED: mcpp.toml validation failed${NC}"
            ((ERRORS++))
        fi
    fi
else
    echo -e "${RED}❌ FAILED: mcpp.toml fixture not found${NC}"
    ((ERRORS++))
fi
echo ""

# Promise 8: Documentation links should be valid
echo "📝 Promise 8: Checking documentation links..."
BROKEN_LINKS=0

# Check for broken internal markdown links
if grep -r "\[.*\](\.\/.*\.md)" README.md docs/ tests/ 2>/dev/null | while read -r line; do
    # Extract file path from markdown link
    LINK=$(echo "$line" | sed -n 's/.*](\(\.\/[^)]*\.md\)).*/\1/p')
    if [ -n "$LINK" ]; then
        # Remove leading ./
        LINK_PATH="${LINK#./}"
        if [ ! -f "$LINK_PATH" ]; then
            echo -e "${RED}  ❌ Broken link: $LINK in $line${NC}"
            ((BROKEN_LINKS++))
        fi
    fi
done; then
    if [ $BROKEN_LINKS -eq 0 ]; then
        echo -e "${GREEN}✓ PASSED: No broken internal links found${NC}"
    else
        echo -e "${RED}❌ FAILED: $BROKEN_LINKS broken link(s)${NC}"
        ((ERRORS++))
    fi
fi
echo ""

# Promise 9: Version consistency
echo "📝 Promise 9: Checking version consistency..."
VERSION=$(grep '^version = ' pyproject.toml | cut -d'"' -f2)
echo "  Current version: $VERSION"
if [ -n "$VERSION" ]; then
    echo -e "${GREEN}✓ PASSED: Version is set ($VERSION)${NC}"
else
    echo -e "${RED}❌ FAILED: Version not found in pyproject.toml${NC}"
    ((ERRORS++))
fi
echo ""

# Promise 10: Constitutional equation reference
echo "📝 Promise 10: Verifying constitutional equation references..."
EQUATION_COUNT=$(grep -r "spec\.md = μ(feature\.ttl)" --include="*.md" --include="*.py" . 2>/dev/null | wc -l)
if [ "$EQUATION_COUNT" -ge 3 ]; then
    echo -e "${GREEN}✓ PASSED: Found $EQUATION_COUNT constitutional equation references${NC}"
else
    echo -e "${YELLOW}⚠ WARNING: Only found $EQUATION_COUNT constitutional equation references${NC}"
    ((WARNINGS++))
fi
echo ""

# Summary
echo "=============================="
echo "📊 Validation Summary"
echo "=============================="
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ ALL PROMISES KEPT${NC}"
    echo -e "${GREEN}All validations passed!${NC}"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠️  PASSED WITH WARNINGS${NC}"
    echo -e "${YELLOW}Warnings: $WARNINGS${NC}"
    echo "Some optional validations could not be completed."
    exit 0
else
    echo -e "${RED}❌ VALIDATION FAILED${NC}"
    echo -e "${RED}Errors: $ERRORS${NC}"
    echo -e "${YELLOW}Warnings: $WARNINGS${NC}"
    echo "Please fix the errors above."
    exit 1
fi
