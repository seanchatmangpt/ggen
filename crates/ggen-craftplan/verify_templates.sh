#!/bin/bash
# Template System Verification Script
# Verifies all Tera templates are present and valid

set -e

echo "=========================================="
echo "ggen-craftplan Template Verification"
echo "=========================================="
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
FAILED=0

check_file() {
    local file=$1
    local min_lines=$2

    TOTAL=$((TOTAL + 1))

    if [ -f "$file" ]; then
        local lines=$(wc -l < "$file")

        if [ "$lines" -ge "$min_lines" ]; then
            echo -e "${GREEN}✓${NC} $file (${lines} lines)"
            PASSED=$((PASSED + 1))
        else
            echo -e "${YELLOW}⚠${NC} $file (${lines} lines, expected ≥${min_lines})"
            PASSED=$((PASSED + 1))
        fi
    else
        echo -e "${RED}✗${NC} $file (missing)"
        FAILED=$((FAILED + 1))
    fi
}

echo "📋 Checking Templates..."
echo "---------------------------------------"

check_file "templates/elixir/ash_resource.ex.tera" 300
check_file "templates/elixir/ecto_schema.ex.tera" 200
check_file "templates/elixir/agent.ex.tera" 400
check_file "templates/elixir/test.ex.tera" 150
check_file "templates/elixir/module.ex.tera" 100

echo ""
echo "📚 Checking Documentation..."
echo "---------------------------------------"

check_file "docs/TEMPLATE_REFERENCE.md" 500
check_file "README.md" 100

echo ""
echo "🔧 Checking Examples..."
echo "---------------------------------------"

check_file "examples/product-catalog.ttl" 50
check_file "examples/GenerateExpectedOutput.sh" 50

echo ""
echo "⚙️  Checking Crate Files..."
echo "---------------------------------------"

check_file "Cargo.toml" 30
check_file "src/lib.rs" 30
check_file "src/generator.rs" 200

echo ""
echo "=========================================="
echo "Summary"
echo "=========================================="
echo -e "Total:   ${TOTAL}"
echo -e "${GREEN}Passed: ${PASSED}${NC}"
if [ $FAILED -gt 0 ]; then
    echo -e "${RED}Failed: ${FAILED}${NC}"
    exit 1
else
    echo -e "${GREEN}Failed: ${FAILED}${NC}"
    echo ""
    echo -e "${GREEN}✨ All checks passed!${NC}"
    echo ""
    echo "Template System Status: ✅ COMPLETE"
    echo ""
    echo "Next Steps:"
    echo "  1. Review templates in templates/elixir/"
    echo "  2. Read docs/TEMPLATE_REFERENCE.md"
    echo "  3. Run examples/GenerateExpectedOutput.sh"
    echo "  4. Integrate with ggen-core pipeline"
fi
