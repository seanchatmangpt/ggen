#!/usr/bin/env bash
#
# Validate SPARQL Query How-to Guide
# Tests: docs/how-to/generation/query-rdf-sparql.md
#
# Validates that all SPARQL queries in the guide work correctly

set -e
set -u

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

log_info() { echo -e "${BLUE}ℹ${NC} $1"; }
log_success() { echo -e "${GREEN}✓${NC} $1"; ((++TESTS_PASSED)); ((++TESTS_RUN)); return 0; }
log_error() { echo -e "${RED}✗${NC} $1"; ((++TESTS_FAILED)); ((++TESTS_RUN)); return 0; }
log_section() {
    echo ""
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${YELLOW}$1${NC}"
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

# Resolve repo root and create workspace
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
WORKSPACE=$(mktemp -d)
trap "rm -rf $WORKSPACE" EXIT

# ensure tool versions available in temp
if [ -f "$REPO_ROOT/.tool-versions" ]; then
    cp "$REPO_ROOT/.tool-versions" "$WORKSPACE/.tool-versions"
fi

cd "$WORKSPACE"

DEFAULT_GGEN_BIN="$REPO_ROOT/target/debug/ggen"
if [ -x "$DEFAULT_GGEN_BIN" ]; then
    GGEN_BIN="${GGEN_BIN:-$DEFAULT_GGEN_BIN}"
else
    GGEN_BIN="${GGEN_BIN:-ggen}"
fi

if ! command -v "$GGEN_BIN" &> /dev/null; then
    log_error "ggen not found"
    exit 1
fi

log_info "Working in: $WORKSPACE"
log_info "Using ggen: $GGEN_BIN ($($GGEN_BIN --version))"

# ============================================================================
# Setup: Load Sample E-commerce Data
# ============================================================================
log_section "Setup: Load E-commerce Data"

cat > ecommerce.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix schema: <https://schema.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Products
ex:product001 a schema:Product ;
    schema:name "Laptop" ;
    schema:sku "LAP-2024-001" ;
    schema:price "999.99"^^xsd:decimal ;
    schema:category ex:electronics ;
    schema:inStock true .

ex:product002 a schema:Product ;
    schema:name "Wireless Mouse" ;
    schema:sku "MOU-2024-042" ;
    schema:price "29.99"^^xsd:decimal ;
    schema:category ex:electronics ;
    schema:inStock true .

ex:product003 a schema:Product ;
    schema:name "Desk Chair" ;
    schema:sku "CHR-2024-013" ;
    schema:price "249.99"^^xsd:decimal ;
    schema:category ex:furniture ;
    schema:inStock false .

# Categories
ex:electronics a schema:Category ;
    schema:name "Electronics" .

ex:furniture a schema:Category ;
    schema:name "Furniture" .
EOF

$GGEN_BIN graph load --file ecommerce.ttl &> /dev/null
if [ $? -eq 0 ]; then
    log_success "Loaded e-commerce sample data"
else
    log_error "Failed to load sample data"
    exit 1
fi

# ============================================================================
# Query 1: Get All Products
# ============================================================================
log_section "Query 1: Get All Product Names"

QUERY1='
PREFIX schema: <https://schema.org/>

SELECT ?name
WHERE {
  ?product a schema:Product .
  ?product schema:name ?name .
}
'

RESULT1=$($GGEN_BIN graph query --sparql_query "$QUERY1" --graph_file ecommerce.ttl 2>&1)
if [ $? -eq 0 ]; then
    log_success "Query 1 executed successfully"

    # Check for expected products
    if echo "$RESULT1" | grep -q "Laptop"; then
        log_success "Found 'Laptop' in results"
    else
        log_error "Missing 'Laptop' in results"
    fi

    if echo "$RESULT1" | grep -q "Wireless Mouse"; then
        log_success "Found 'Wireless Mouse' in results"
    else
        log_error "Missing 'Wireless Mouse'"
    fi

    if echo "$RESULT1" | grep -q "Desk Chair"; then
        log_success "Found 'Desk Chair' in results"
    else
        log_error "Missing 'Desk Chair'"
    fi
else
    log_error "Query 1 failed: $RESULT1"
fi

# ============================================================================
# Query 2: Products with Details
# ============================================================================
log_section "Query 2: Products with Name, SKU, Price"

QUERY2='
PREFIX schema: <https://schema.org/>

SELECT ?name ?sku ?price
WHERE {
  ?product a schema:Product ;
           schema:name ?name ;
           schema:sku ?sku ;
           schema:price ?price .
}
ORDER BY ?name
'

RESULT2=$($GGEN_BIN graph query --sparql_query "$QUERY2" --graph_file ecommerce.ttl 2>&1)
if [ $? -eq 0 ]; then
    log_success "Query 2 executed successfully"

    # Verify all 3 products returned
    PRODUCT_COUNT=$(echo "$RESULT2" | grep -o "LAP-\\|MOU-\\|CHR-" | wc -l | tr -d '[:space:]')
    if [ "$PRODUCT_COUNT" -ge 3 ]; then
        log_success "All 3 products returned with details"
    else
        log_error "Expected 3 products, got $PRODUCT_COUNT"
    fi
else
    log_error "Query 2 failed"
fi

# ============================================================================
# Query 3: Filter - Products In Stock
# ============================================================================
log_section "Query 3: Filter Products In Stock"

QUERY3='
PREFIX schema: <https://schema.org/>

SELECT ?name ?price
WHERE {
  ?product a schema:Product ;
           schema:name ?name ;
           schema:price ?price ;
           schema:inStock true .
}
'

RESULT3=$($GGEN_BIN graph query --sparql_query "$QUERY3" --graph_file ecommerce.ttl 2>&1)
if [ $? -eq 0 ]; then
    log_success "Query 3 executed successfully"

    # Should find Laptop and Mouse, but NOT Desk Chair
    if echo "$RESULT3" | grep -q "Laptop" && echo "$RESULT3" | grep -q "Mouse"; then
        log_success "Found in-stock products (Laptop, Mouse)"
    else
        log_error "Missing expected in-stock products"
    fi

    if echo "$RESULT3" | grep -q "Desk Chair"; then
        log_error "Desk Chair should be filtered out (not in stock)"
    else
        log_success "Correctly filtered out Desk Chair (not in stock)"
    fi
else
    log_error "Query 3 failed"
fi

# ============================================================================
# Query 4: Filter - Products Under $100
# ============================================================================
log_section "Query 4: Filter Products Under \$100"

QUERY4='
PREFIX schema: <https://schema.org/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?name ?price
WHERE {
  ?product a schema:Product ;
           schema:name ?name ;
           schema:price ?price .
  FILTER(?price < "100.00"^^xsd:decimal)
}
'

RESULT4=$($GGEN_BIN graph query --sparql_query "$QUERY4" --graph_file ecommerce.ttl 2>&1)
if [ $? -eq 0 ]; then
    log_success "Query 4 executed successfully"

    # Should only find Wireless Mouse ($29.99)
    if echo "$RESULT4" | grep -q "Wireless Mouse"; then
        log_success "Found Wireless Mouse (under \$100)"
    else
        log_error "Missing Wireless Mouse in results"
    fi

    # Should NOT find Laptop or Chair
    if echo "$RESULT4" | grep -qE "Laptop|Chair"; then
        log_error "Found expensive products (should be filtered out)"
    else
        log_success "Correctly filtered out products over \$100"
    fi
else
    log_error "Query 4 failed"
fi

# ============================================================================
# Query 5: Aggregation - Count by Category
# ============================================================================
log_section "Query 5: Count Products by Category"

QUERY5='
PREFIX schema: <https://schema.org/>

SELECT ?category_name (COUNT(?product) AS ?count)
WHERE {
  ?product a schema:Product ;
           schema:category ?category .
  ?category schema:name ?category_name .
}
GROUP BY ?category_name
ORDER BY DESC(?count)
'

RESULT5=$($GGEN_BIN graph query --sparql_query "$QUERY5" --graph_file ecommerce.ttl 2>&1)
if [ $? -eq 0 ]; then
    log_success "Query 5 executed successfully"

    # Check for expected counts
    if echo "$RESULT5" | grep -q "Electronics.*2\|2.*Electronics"; then
        log_success "Electronics category has count of 2"
    else
        log_error "Electronics category count incorrect"
    fi

    if echo "$RESULT5" | grep -q "Furniture.*1\|1.*Furniture"; then
        log_success "Furniture category has count of 1"
    else
        log_error "Furniture category count incorrect"
    fi
else
    log_error "Query 5 failed"
fi

# ============================================================================
# Query 6: Pattern - Find All Instances
# ============================================================================
log_section "Query 6: Common Pattern - Find All Product Instances"

QUERY6='
PREFIX schema: <https://schema.org/>

SELECT ?instance
WHERE {
  ?instance a schema:Product .
}
'

RESULT6=$($GGEN_BIN graph query --sparql_query "$QUERY6" --graph_file ecommerce.ttl 2>&1)
if [ $? -eq 0 ]; then
    log_success "Query 6 executed successfully"

    # Should find 3 product instances
    INSTANCE_COUNT=$(echo "$RESULT6" | grep -o "product00" | wc -l | tr -d '[:space:]')
    if [ "$INSTANCE_COUNT" -eq 3 ]; then
        log_success "Found all 3 product instances"
    else
        log_error "Expected 3 instances, found $INSTANCE_COUNT"
    fi
else
    log_error "Query 6 failed"
fi

# ============================================================================
# Query 7: LIMIT Results
# ============================================================================
log_section "Query 7: Limit Results to 2"

QUERY7='
PREFIX schema: <https://schema.org/>

SELECT ?name
WHERE {
  ?product a schema:Product ;
           schema:name ?name .
}
LIMIT 2
'

RESULT7=$($GGEN_BIN graph query --sparql_query "$QUERY7" --graph_file ecommerce.ttl 2>&1)
if [ $? -eq 0 ]; then
    log_success "Query 7 executed successfully"

    # Count results (should be exactly 2)
    RESULT_COUNT=$(echo "$RESULT7" | grep -o "Laptop\\|Mouse\\|Chair" | wc -l | tr -d '[:space:]')
    if [ "$RESULT_COUNT" -eq 2 ]; then
        log_success "LIMIT 2 correctly returned 2 results"
    else
        log_error "Expected 2 results from LIMIT, got $RESULT_COUNT"
    fi
else
    log_error "Query 7 failed"
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
    echo -e "${GREEN}✓ SPARQL Query Guide: ALL TESTS PASSED${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 0
else
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${RED}✗ SPARQL Query Guide: TESTS FAILED${NC}"
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 1
fi
