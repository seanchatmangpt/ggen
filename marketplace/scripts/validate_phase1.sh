#!/usr/bin/env bash
# Phase 1 Validation Script - Comprehensive Quality Gates
set -euo pipefail

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}=== Phase 1: Core Power Packages Validation ===${NC}\n"

PACKAGES=("agent-editor" "agent-cli-copilot" "agent-context-crafter" "agent-memory-forge" "agent-reasoning-mcp")
PASS=0
FAIL=0

# Validation functions
validate_package_toml() {
    local pkg=$1
    local file="marketplace/packages/$pkg/package.toml"
    if [[ -f "$file" ]] && grep -q "version = \"1.0.0\"" "$file"; then
        echo -e "${GREEN}✓${NC} package.toml exists and version is 1.0.0"
        return 0
    else
        echo -e "${RED}✗${NC} package.toml missing or invalid version"
        return 1
    fi
}

validate_rdf_ontology() {
    local pkg=$1
    local file="marketplace/packages/$pkg/rdf/ontology.ttl"
    if [[ -f "$file" ]]; then
        local lines=$(wc -l < "$file")
        if [[ $lines -ge 200 ]]; then
            echo -e "${GREEN}✓${NC} RDF ontology exists ($lines lines, target: 200+)"
            return 0
        else
            echo -e "${YELLOW}⚠${NC} RDF ontology too short ($lines lines, target: 200+)"
            return 1
        fi
    else
        echo -e "${RED}✗${NC} RDF ontology missing"
        return 1
    fi
}

validate_sparql_queries() {
    local pkg=$1
    local count=$(find "marketplace/packages/$pkg/sparql" -name "*.rq" 2>/dev/null | wc -l)
    if [[ $count -ge 5 ]]; then
        echo -e "${GREEN}✓${NC} SPARQL queries exist ($count queries, target: 5+)"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} Not enough SPARQL queries ($count, target: 5+)"
        return 1
    fi
}

validate_examples() {
    local pkg=$1
    local rust=$(find "marketplace/packages/$pkg/examples" -name "*.rs" 2>/dev/null | wc -l)
    local ts=$(find "marketplace/packages/$pkg/examples" -name "*.ts" 2>/dev/null | wc -l)
    local py=$(find "marketplace/packages/$pkg/examples" -name "*.py" 2>/dev/null | wc -l)
    local total=$((rust + ts + py))

    if [[ $total -ge 3 ]]; then
        echo -e "${GREEN}✓${NC} Examples exist (Rust:$rust TS:$ts Python:$py, total:$total)"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} Not enough examples (total:$total, target: 3+)"
        return 1
    fi
}

validate_tests() {
    local pkg=$1
    local file="marketplace/packages/$pkg/tests/integration_test.rs"
    if [[ -f "$file" ]]; then
        local tests=$(grep -c "#\[test\]" "$file" || echo "0")
        echo -e "${GREEN}✓${NC} Tests exist ($tests test functions)"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} Test file missing"
        return 1
    fi
}

validate_docs() {
    local pkg=$1
    local readme="marketplace/packages/$pkg/README.md"
    local api="marketplace/packages/$pkg/docs/api.md"

    if [[ -f "$readme" ]] && [[ -f "$api" ]]; then
        echo -e "${GREEN}✓${NC} Documentation exists (README + API docs)"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} Missing documentation files"
        return 1
    fi
}

# Run validation for each package
for pkg in "${PACKAGES[@]}"; do
    echo -e "\n${BLUE}Validating: $pkg${NC}"

    PKG_PASS=0
    PKG_FAIL=0

    validate_package_toml "$pkg" && ((PKG_PASS++)) || ((PKG_FAIL++))
    validate_rdf_ontology "$pkg" && ((PKG_PASS++)) || ((PKG_FAIL++))
    validate_sparql_queries "$pkg" && ((PKG_PASS++)) || ((PKG_FAIL++))
    validate_examples "$pkg" && ((PKG_PASS++)) || ((PKG_FAIL++))
    validate_tests "$pkg" && ((PKG_PASS++)) || ((PKG_FAIL++))
    validate_docs "$pkg" && ((PKG_PASS++)) || ((PKG_FAIL++))

    PASS=$((PASS + PKG_PASS))
    FAIL=$((FAIL + PKG_FAIL))

    SCORE=$((PKG_PASS * 100 / 6))
    if [[ $SCORE -ge 95 ]]; then
        echo -e "${GREEN}Package Score: $SCORE/100 ✓ PRODUCTION READY${NC}"
    elif [[ $SCORE -ge 80 ]]; then
        echo -e "${YELLOW}Package Score: $SCORE/100 ⚠ NEEDS IMPROVEMENT${NC}"
    else
        echo -e "${RED}Package Score: $SCORE/100 ✗ NOT READY${NC}"
    fi
done

# Overall summary
echo -e "\n${BLUE}=== Validation Summary ===${NC}"
echo "Total checks: $((PASS + FAIL))"
echo -e "${GREEN}Passed: $PASS${NC}"
echo -e "${RED}Failed: $FAIL${NC}"

OVERALL_SCORE=$((PASS * 100 / (PASS + FAIL)))
echo -e "\n${BLUE}Overall Score: $OVERALL_SCORE/100${NC}"

if [[ $OVERALL_SCORE -ge 95 ]]; then
    echo -e "${GREEN}✅ PHASE 1 VALIDATION: PRODUCTION READY${NC}"
    exit 0
elif [[ $OVERALL_SCORE -ge 80 ]]; then
    echo -e "${YELLOW}⚠ PHASE 1 VALIDATION: NEEDS IMPROVEMENT${NC}"
    exit 1
else
    echo -e "${RED}✗ PHASE 1 VALIDATION: NOT READY${NC}"
    exit 1
fi
