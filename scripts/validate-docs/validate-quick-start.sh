#!/usr/bin/env bash
#
# Validate Quick Start Tutorial
# Tests: docs/getting-started/quick-start.md
#
# This script validates that every step in the Quick Start tutorial
# works exactly as documented.

set -e  # Exit on error
set -u  # Exit on undefined variable

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Utility functions
log_info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

log_success() {
    echo -e "${GREEN}✓${NC} $1"
    ((TESTS_PASSED++))
    ((TESTS_RUN++))
}

log_error() {
    echo -e "${RED}✗${NC} $1"
    ((TESTS_FAILED++))
    ((TESTS_RUN++))
}

log_section() {
    echo ""
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${YELLOW}$1${NC}"
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

# Create temporary workspace
WORKSPACE=$(mktemp -d)
trap "rm -rf $WORKSPACE" EXIT

cd "$WORKSPACE"
log_info "Working in: $WORKSPACE"

# Get the path to ggen binary
GGEN_BIN="${GGEN_BIN:-ggen}"

# Verify ggen is available
if ! command -v "$GGEN_BIN" &> /dev/null; then
    log_error "ggen command not found. Please install ggen or set GGEN_BIN."
    exit 1
fi

log_info "Using ggen: $(command -v $GGEN_BIN)"
log_info "Version: $($GGEN_BIN --version)"

# ============================================================================
# Step 1: Installation (verify only)
# ============================================================================
log_section "Step 1: Verify Installation"

if $GGEN_BIN --version &> /dev/null; then
    log_success "ggen is installed and executable"
else
    log_error "ggen --version failed"
    exit 1
fi

# ============================================================================
# Step 2: Create Product Ontology
# ============================================================================
log_section "Step 2: Create Product Ontology"

cat > product-ontology.ttl << 'EOF'
@prefix ex: <http://example.org/ecommerce/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Define Product class
ex:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the e-commerce catalog" .

# Define properties
ex:id a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "id" ;
    rdfs:comment "Unique product identifier" .

ex:name a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "name" ;
    rdfs:comment "Product name" .

ex:price a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" ;
    rdfs:comment "Product price in USD" .

ex:inStock a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:boolean ;
    rdfs:label "inStock" ;
    rdfs:comment "Whether product is currently in stock" .

# Sample product instance
ex:product001 a ex:Product ;
    ex:id "PROD-001" ;
    ex:name "Wireless Mouse" ;
    ex:price "29.99"^^xsd:decimal ;
    ex:inStock true .
EOF

if [ -f product-ontology.ttl ]; then
    log_success "Created product-ontology.ttl"
else
    log_error "Failed to create product-ontology.ttl"
    exit 1
fi

# Verify file has expected content
if grep -q "ex:Product a rdfs:Class" product-ontology.ttl; then
    log_success "Ontology contains Product class definition"
else
    log_error "Ontology missing Product class"
fi

# ============================================================================
# Step 3: Load RDF into Graph
# ============================================================================
log_section "Step 3: Load RDF into Graph"

OUTPUT=$($GGEN_BIN graph load --file product-ontology.ttl 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    log_success "Loaded RDF into graph"
    log_info "Output: $OUTPUT"

    # Check for expected triple count (should be around 13 triples)
    if echo "$OUTPUT" | grep -qE "Loaded [0-9]+ triples"; then
        TRIPLE_COUNT=$(echo "$OUTPUT" | grep -oE "[0-9]+" | head -1)
        if [ "$TRIPLE_COUNT" -ge 10 ]; then
            log_success "Loaded $TRIPLE_COUNT triples (expected ~13)"
        else
            log_error "Only loaded $TRIPLE_COUNT triples (expected ~13)"
        fi
    fi
else
    log_error "Failed to load RDF: $OUTPUT"
fi

# ============================================================================
# Step 4: Query Graph (Verify Data)
# ============================================================================
log_section "Step 4: Query Graph with SPARQL"

QUERY_OUTPUT=$($GGEN_BIN graph query --sparql_query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 5" 2>&1)
QUERY_EXIT=$?

if [ $QUERY_EXIT -eq 0 ]; then
    log_success "SPARQL query executed successfully"

    # Verify output contains expected data
    if echo "$QUERY_OUTPUT" | grep -q "product001\|Wireless Mouse\|29.99"; then
        log_success "Query results contain expected product data"
    else
        log_error "Query results missing expected data"
        log_info "Output: $QUERY_OUTPUT"
    fi
else
    log_error "SPARQL query failed: $QUERY_OUTPUT"
fi

# ============================================================================
# Step 5: Extract Schema
# ============================================================================
log_section "Step 5: Extract Ontology Schema"

EXTRACT_OUTPUT=$($GGEN_BIN ontology extract --ontology_file product-ontology.ttl --output product-schema.json 2>&1)
EXTRACT_EXIT=$?

if [ $EXTRACT_EXIT -eq 0 ]; then
    log_success "Extracted ontology schema to JSON"

    if [ -f product-schema.json ]; then
        log_success "product-schema.json created"

        # Verify JSON structure
        if command -v jq &> /dev/null; then
            if jq empty product-schema.json 2>/dev/null; then
                log_success "Schema JSON is valid"

                # Check for expected content
                if jq -e '.classes' product-schema.json &> /dev/null; then
                    log_success "Schema contains 'classes' array"
                else
                    log_error "Schema missing 'classes' array"
                fi
            else
                log_error "Schema JSON is invalid"
            fi
        else
            log_info "jq not installed, skipping JSON validation"
        fi

        # Show first 20 lines of schema
        log_info "Schema preview:"
        head -20 product-schema.json | sed 's/^/  /'
    else
        log_error "product-schema.json not created"
    fi
else
    log_error "Schema extraction failed: $EXTRACT_OUTPUT"
fi

# ============================================================================
# Step 6: Template Commands
# ============================================================================
log_section "Step 6: Verify Template Commands"

# Test template list
TEMPLATE_LIST=$($GGEN_BIN template list 2>&1)
if [ $? -eq 0 ]; then
    log_success "template list command works"

    if echo "$TEMPLATE_LIST" | grep -q "templates"; then
        TEMPLATE_COUNT=$(echo "$TEMPLATE_LIST" | grep -o "name" | wc -l)
        log_success "Found templates in output"
        log_info "Template count: $TEMPLATE_COUNT"
    fi
else
    log_error "template list failed"
fi

# ============================================================================
# Summary
# ============================================================================
log_section "Test Summary"

echo ""
echo "Total Tests Run:    $TESTS_RUN"
echo -e "${GREEN}Tests Passed:       $TESTS_PASSED${NC}"
echo -e "${RED}Tests Failed:       $TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}✓ Quick Start Tutorial: ALL TESTS PASSED${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 0
else
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${RED}✗ Quick Start Tutorial: TESTS FAILED${NC}"
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 1
fi
