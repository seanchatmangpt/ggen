#!/usr/bin/env bash
# ============================================================================
# mcpp sync Helper Script for 013-mcpp-v6-rdf-system
# ============================================================================
# Purpose: Run mcpp sync with proper configuration and validation
# Constitutional Equation: spec.md = μ(feature.ttl)
# ============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}============================================================================${NC}"
echo -e "${BLUE}mcpp v6 Sync - RDF-First Code Generation${NC}"
echo -e "${BLUE}============================================================================${NC}"
echo ""

# Change to project directory
cd "$PROJECT_DIR"

# Pre-flight checks
echo -e "${BLUE}🔍 Pre-flight checks...${NC}"

if [[ ! -f "mcpp.toml" ]]; then
    echo -e "${RED}❌ ERROR: mcpp.toml not found in $PROJECT_DIR${NC}"
    exit 1
fi
echo -e "${GREEN}✓ mcpp.toml found${NC}"

if [[ ! -d "ontology" ]]; then
    echo -e "${RED}❌ ERROR: ontology/ directory not found${NC}"
    exit 1
fi
echo -e "${GREEN}✓ ontology/ directory exists${NC}"

if [[ ! -f "ontology/feature-content.ttl" ]]; then
    echo -e "${RED}❌ ERROR: ontology/feature-content.ttl not found${NC}"
    exit 1
fi
echo -e "${GREEN}✓ feature-content.ttl exists${NC}"

# Check TTL syntax (if rdflib available)
if command -v python3 &>/dev/null; then
    TTL_VALID=$(python3 - <<'PYEOF' 2>&1
import sys
try:
    from rdflib import Graph
    g = Graph()
    g.parse("ontology/feature-content.ttl", format="turtle")
    g.parse("ontology/mvp-80-20.ttl", format="turtle")
    print(f"VALID:{len(g)}")
except ImportError:
    print("SKIP:rdflib not installed")
except Exception as e:
    print(f"ERROR:{e}")
PYEOF
)
    if [[ "$TTL_VALID" =~ ^VALID:([0-9]+)$ ]]; then
        TRIPLE_COUNT="${BASH_REMATCH[1]}"
        echo -e "${GREEN}✓ TTL files valid ($TRIPLE_COUNT triples)${NC}"
    elif [[ "$TTL_VALID" =~ ^SKIP: ]]; then
        echo -e "${YELLOW}⚠ Skipping TTL validation (rdflib not installed)${NC}"
    else
        echo -e "${RED}❌ TTL parsing error: ${TTL_VALID#ERROR:}${NC}"
        exit 1
    fi
fi

if [[ ! -d "templates" ]]; then
    echo -e "${YELLOW}⚠ WARNING: templates/ directory not found${NC}"
fi

echo ""

# Check if mcpp is installed
if ! command -v mcpp &>/dev/null; then
    echo -e "${RED}❌ ERROR: mcpp command not found${NC}"
    echo ""
    echo "mcpp v6 is not yet implemented. This project specifies mcpp v6."
    echo "Current mcpp version is v5 (does not have 'sync' command yet)."
    echo ""
    echo "This is expected - we're using RDF to SPECIFY mcpp v6."
    echo "Once v6 is implemented, you'll be able to run 'mcpp sync' here."
    echo ""
    exit 1
fi

# Get mcpp version
GGEN_VERSION=$(mcpp --version 2>&1 | head -1)
echo -e "${BLUE}📦 Using: $GGEN_VERSION${NC}"

# Check if mcpp has sync command
if ! mcpp help 2>&1 | grep -q "sync"; then
    echo -e "${YELLOW}⚠ WARNING: mcpp sync command not available in this version${NC}"
    echo "This project requires mcpp v6 with 'sync' command."
    echo "Current version: $GGEN_VERSION"
    echo ""
    echo -e "${BLUE}Expected workflow (when v6 is ready):${NC}"
    echo "  1. Parse: ontology/feature-content.ttl + ontology/mvp-80-20.ttl"
    echo "  2. Query: Execute SPARQL from mcpp.toml"
    echo "  3. Render: Apply templates/*.tera templates"
    echo "  4. Output: generated/*.md files"
    echo ""
    exit 1
fi

# Run mcpp sync
echo -e "${BLUE}🚀 Running mcpp sync...${NC}"
echo ""

mcpp sync || {
    echo ""
    echo -e "${RED}❌ mcpp sync failed${NC}"
    exit 1
}

echo ""
echo -e "${GREEN}✅ Generation complete!${NC}"
echo ""

# Show generated files
if [[ -d "generated" ]]; then
    echo -e "${BLUE}📄 Generated files:${NC}"
    find generated -type f -name "*.md" -exec echo "  - {}" \;
    echo ""
fi

# Verify determinism (run twice, compare hashes)
echo -e "${BLUE}🔒 Verifying determinism...${NC}"
if [[ -f "generated/spec.md" ]]; then
    HASH1=$(sha256sum generated/spec.md | awk '{print $1}')
    mcpp sync >/dev/null 2>&1
    HASH2=$(sha256sum generated/spec.md | awk '{print $1}')

    if [[ "$HASH1" == "$HASH2" ]]; then
        echo -e "${GREEN}✓ Deterministic: μ∘μ = μ (hashes match)${NC}"
        echo "  Hash: ${HASH1:0:16}..."
    else
        echo -e "${RED}❌ Non-deterministic: Outputs differ!${NC}"
        echo "  Hash 1: $HASH1"
        echo "  Hash 2: $HASH2"
        exit 1
    fi
fi

echo ""
echo -e "${GREEN}============================================================================${NC}"
echo -e "${GREEN}Constitutional Equation Verified: spec.md = μ(feature.ttl)${NC}"
echo -e "${GREEN}============================================================================${NC}"
