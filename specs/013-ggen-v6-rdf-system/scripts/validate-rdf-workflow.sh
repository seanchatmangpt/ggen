#!/usr/bin/env bash
# ============================================================================
# RDF Workflow Validation for ggen v6 (013-ggen-v6-rdf-system)
# ============================================================================
# Purpose: Validate that all RDF-first architecture components are in place
# and configured correctly for the ggen v6 specification project.
#
# Constitutional Equation: spec.md = μ(feature.ttl)
# ============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

ERRORS=0
WARNINGS=0
PASSED=0

echo -e "${BLUE}============================================================================${NC}"
echo -e "${BLUE}ggen v6 RDF Workflow Validation${NC}"
echo -e "${BLUE}============================================================================${NC}"
echo ""
echo "Project: $(basename "$PROJECT_DIR")"
echo "Date: $(date '+%Y-%m-%d %H:%M:%S')"
echo ""

# ============================================================================
# Test 1: ggen.toml Configuration
# ============================================================================
echo -e "${BLUE}📝 Test 1: Validating ggen.toml configuration...${NC}"

if [[ ! -f "$PROJECT_DIR/ggen.toml" ]]; then
    echo -e "${RED}❌ FAILED: ggen.toml not found${NC}"
    ((ERRORS++))
else
    # Check v6 section
    if grep -q "^\[v6\]" "$PROJECT_DIR/ggen.toml"; then
        echo -e "${GREEN}✓ PASSED: [v6] section exists${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ FAILED: [v6] section missing${NC}"
        ((ERRORS++))
    fi

    # Check ontology sources
    if grep -q "ontology.*=.*feature-content.ttl" "$PROJECT_DIR/ggen.toml"; then
        echo -e "${GREEN}✓ PASSED: Ontology sources configured${NC}"
        ((PASSED++))
    else
        echo -e "${YELLOW}⚠ WARNING: Ontology sources may not be configured${NC}"
        ((WARNINGS++))
    fi

    # Check generation rules
    if grep -q "^\[\[generation\]\]" "$PROJECT_DIR/ggen.toml"; then
        GENERATION_COUNT=$(grep -c "^\[\[generation\]\]" "$PROJECT_DIR/ggen.toml")
        echo -e "${GREEN}✓ PASSED: Found $GENERATION_COUNT generation rule(s)${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ FAILED: No [[generation]] rules found${NC}"
        ((ERRORS++))
    fi

    # Verify SPARQL query present
    if grep -q "query.*=.*\"\"\"" "$PROJECT_DIR/ggen.toml"; then
        echo -e "${GREEN}✓ PASSED: SPARQL query configured${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ FAILED: SPARQL query not found${NC}"
        ((ERRORS++))
    fi

    # Check template configuration
    if grep -q "template.*=.*\.tera" "$PROJECT_DIR/ggen.toml"; then
        echo -e "${GREEN}✓ PASSED: Tera template configured${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ FAILED: Template not configured${NC}"
        ((ERRORS++))
    fi
fi
echo ""

# ============================================================================
# Test 2: RDF Ontology Files
# ============================================================================
echo -e "${BLUE}📝 Test 2: Validating RDF ontology files...${NC}"

ONTOLOGY_DIR="$PROJECT_DIR/ontology"

if [[ ! -d "$ONTOLOGY_DIR" ]]; then
    echo -e "${RED}❌ FAILED: ontology/ directory not found${NC}"
    ((ERRORS++))
else
    # Check feature-content.ttl
    if [[ -f "$ONTOLOGY_DIR/feature-content.ttl" ]]; then
        echo -e "${GREEN}✓ PASSED: feature-content.ttl exists${NC}"
        ((PASSED++))

        # Validate TTL syntax with Python rdflib (if available)
        if command -v python3 &>/dev/null; then
            python3 - <<'PYEOF' "$ONTOLOGY_DIR/feature-content.ttl"
import sys
try:
    from rdflib import Graph
    g = Graph()
    g.parse(sys.argv[1], format="turtle")
    print(f"  ✓ Valid Turtle syntax ({len(g)} triples)")
    sys.exit(0)
except ImportError:
    print("  ⚠ rdflib not installed, skipping syntax validation")
    sys.exit(2)
except Exception as e:
    print(f"  ✗ TTL parsing error: {e}")
    sys.exit(1)
PYEOF
            RESULT=$?
            if [[ $RESULT -eq 0 ]]; then
                ((PASSED++))
            elif [[ $RESULT -eq 2 ]]; then
                ((WARNINGS++))
            else
                ((ERRORS++))
            fi
        fi
    else
        echo -e "${RED}❌ FAILED: feature-content.ttl not found${NC}"
        ((ERRORS++))
    fi

    # Check mvp-80-20.ttl
    if [[ -f "$ONTOLOGY_DIR/mvp-80-20.ttl" ]]; then
        echo -e "${GREEN}✓ PASSED: mvp-80-20.ttl exists (80/20 analysis)${NC}"
        ((PASSED++))
    else
        echo -e "${YELLOW}⚠ WARNING: mvp-80-20.ttl not found${NC}"
        ((WARNINGS++))
    fi

    # Check spec-kit schema
    if [[ -f "$ONTOLOGY_DIR/spec-kit-schema.ttl" ]]; then
        echo -e "${GREEN}✓ PASSED: spec-kit-schema.ttl exists${NC}"
        ((PASSED++))
    else
        echo -e "${YELLOW}⚠ WARNING: spec-kit-schema.ttl not found (SHACL validation disabled)${NC}"
        ((WARNINGS++))
    fi
fi
echo ""

# ============================================================================
# Test 3: Template Files
# ============================================================================
echo -e "${BLUE}📝 Test 3: Validating Tera templates...${NC}"

TEMPLATES_DIR="$PROJECT_DIR/templates"

if [[ ! -d "$TEMPLATES_DIR" ]]; then
    echo -e "${RED}❌ FAILED: templates/ directory not found${NC}"
    ((ERRORS++))
else
    # Check spec.tera
    if [[ -f "$TEMPLATES_DIR/spec.tera" ]]; then
        echo -e "${GREEN}✓ PASSED: spec.tera template exists${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ FAILED: spec.tera not found${NC}"
        ((ERRORS++))
    fi

    # Count total templates
    TEMPLATE_COUNT=$(find "$TEMPLATES_DIR" -name "*.tera" -type f 2>/dev/null | wc -l)
    echo -e "${GREEN}✓ Found $TEMPLATE_COUNT Tera template(s)${NC}"
    ((PASSED++))
fi
echo ""

# ============================================================================
# Test 4: Generated Artifacts
# ============================================================================
echo -e "${BLUE}📝 Test 4: Checking generated artifacts...${NC}"

GENERATED_DIR="$PROJECT_DIR/generated"

if [[ ! -d "$GENERATED_DIR" ]]; then
    echo -e "${YELLOW}⚠ WARNING: generated/ directory not found (run 'ggen sync' to create)${NC}"
    ((WARNINGS++))
else
    if [[ -f "$GENERATED_DIR/spec.md" ]]; then
        echo -e "${GREEN}✓ PASSED: generated/spec.md exists${NC}"
        ((PASSED++))

        # Check if it has the "DO NOT EDIT" header
        if grep -q "Generated from.*DO NOT EDIT" "$GENERATED_DIR/spec.md" 2>/dev/null; then
            echo -e "${GREEN}✓ PASSED: spec.md has 'DO NOT EDIT' warning${NC}"
            ((PASSED++))
        else
            echo -e "${YELLOW}⚠ WARNING: spec.md missing 'DO NOT EDIT' header${NC}"
            ((WARNINGS++))
        fi
    else
        echo -e "${YELLOW}⚠ WARNING: spec.md not generated yet (run 'ggen sync')${NC}"
        ((WARNINGS++))
    fi
fi
echo ""

# ============================================================================
# Test 5: No "ggen render" References
# ============================================================================
echo -e "${BLUE}📝 Test 5: Checking for legacy 'ggen render' references...${NC}"

RENDER_REFS=$(grep -r "ggen render" --include="*.md" --include="*.sh" --include="*.toml" \
    --exclude-dir="generated" --exclude="VALIDATION_REPORT.md" \
    "$PROJECT_DIR" 2>/dev/null | wc -l)

if [[ $RENDER_REFS -eq 0 ]]; then
    echo -e "${GREEN}✓ PASSED: No 'ggen render' references found${NC}"
    ((PASSED++))
else
    echo -e "${RED}❌ FAILED: Found $RENDER_REFS 'ggen render' reference(s)${NC}"
    echo "  (Should use 'ggen sync' instead)"
    grep -rn "ggen render" --include="*.md" --include="*.sh" --include="*.toml" \
        --exclude-dir="generated" --exclude="VALIDATION_REPORT.md" \
        "$PROJECT_DIR" 2>/dev/null | head -5
    ((ERRORS++))
fi
echo ""

# ============================================================================
# Test 6: Constitutional Equation References
# ============================================================================
echo -e "${BLUE}📝 Test 6: Verifying constitutional equation references...${NC}"

EQUATION_REFS=$(grep -r "spec\.md = μ(.*\.ttl)\|code = μ(.*\.ttl)" --include="*.md" --include="*.ttl" \
    "$PROJECT_DIR" 2>/dev/null | wc -l)

if [[ $EQUATION_REFS -ge 1 ]]; then
    echo -e "${GREEN}✓ PASSED: Found $EQUATION_REFS constitutional equation reference(s)${NC}"
    ((PASSED++))
else
    echo -e "${YELLOW}⚠ WARNING: No constitutional equation references found${NC}"
    ((WARNINGS++))
fi
echo ""

# ============================================================================
# Test 7: 80/20 Markers in RDF
# ============================================================================
echo -e "${BLUE}📝 Test 7: Checking 80/20 prioritization markers...${NC}"

if [[ -f "$ONTOLOGY_DIR/feature-content.ttl" ]]; then
    if grep -q "eightyTwentyCategory" "$ONTOLOGY_DIR/feature-content.ttl"; then
        CORE_COUNT=$(grep -c "Core-20-Percent" "$ONTOLOGY_DIR/feature-content.ttl")
        DEFERRED_COUNT=$(grep -c "Deferred-80-Percent" "$ONTOLOGY_DIR/feature-content.ttl")
        echo -e "${GREEN}✓ PASSED: 80/20 markers found${NC}"
        echo -e "  Core (20%): $CORE_COUNT user stories"
        echo -e "  Deferred (80%): $DEFERRED_COUNT user stories"
        ((PASSED++))
    else
        echo -e "${YELLOW}⚠ WARNING: No 80/20 markers in feature-content.ttl${NC}"
        ((WARNINGS++))
    fi
fi
echo ""

# ============================================================================
# Test 8: Project Documentation
# ============================================================================
echo -e "${BLUE}📝 Test 8: Validating project documentation...${NC}"

# Check for 80/20 documentation
if [[ -f "$PROJECT_DIR/80-20-PLAN.md" ]]; then
    echo -e "${GREEN}✓ PASSED: 80-20-PLAN.md exists${NC}"
    ((PASSED++))
else
    echo -e "${YELLOW}⚠ WARNING: 80-20-PLAN.md not found${NC}"
    ((WARNINGS++))
fi

if [[ -f "$PROJECT_DIR/80-20-PRIORITIZATION.md" ]]; then
    echo -e "${GREEN}✓ PASSED: 80-20-PRIORITIZATION.md exists${NC}"
    ((PASSED++))
else
    echo -e "${YELLOW}⚠ WARNING: 80-20-PRIORITIZATION.md not found${NC}"
    ((WARNINGS++))
fi
echo ""

# ============================================================================
# Summary
# ============================================================================
echo -e "${BLUE}============================================================================${NC}"
echo -e "${BLUE}Validation Summary${NC}"
echo -e "${BLUE}============================================================================${NC}"

TOTAL=$((PASSED + WARNINGS + ERRORS))
echo "Total Tests: $TOTAL"
echo -e "${GREEN}Passed: $PASSED${NC}"
echo -e "${YELLOW}Warnings: $WARNINGS${NC}"
echo -e "${RED}Errors: $ERRORS${NC}"
echo ""

if [[ $ERRORS -eq 0 ]] && [[ $WARNINGS -eq 0 ]]; then
    echo -e "${GREEN}✅ ALL VALIDATIONS PASSED${NC}"
    echo -e "${GREEN}RDF-first architecture is correctly configured!${NC}"
    exit 0
elif [[ $ERRORS -eq 0 ]]; then
    echo -e "${YELLOW}⚠️  PASSED WITH WARNINGS${NC}"
    echo "RDF workflow is functional but has minor issues."
    exit 0
else
    echo -e "${RED}❌ VALIDATION FAILED${NC}"
    echo "Please fix the errors above before proceeding."
    exit 1
fi
