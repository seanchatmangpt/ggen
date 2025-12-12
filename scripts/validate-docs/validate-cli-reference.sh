#!/usr/bin/env bash
#
# Validate CLI Command Reference
# Tests: docs/reference/commands/complete-cli-reference.md
#
# Validates that all documented CLI commands work

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

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
DEFAULT_GGEN_BIN="$REPO_ROOT/target/debug/ggen"
if [ -x "$DEFAULT_GGEN_BIN" ]; then
    GGEN_BIN="${GGEN_BIN:-$DEFAULT_GGEN_BIN}"
else
    GGEN_BIN="${GGEN_BIN:-ggen}"
fi

if ! command -v "$GGEN_BIN" &> /dev/null && [ ! -x "$GGEN_BIN" ]; then
    log_error "ggen not found"
    exit 1
fi

log_info "Using ggen: $GGEN_BIN ($($GGEN_BIN --version))"

# Create workspace
WORKSPACE=$(mktemp -d)
trap "rm -rf $WORKSPACE" EXIT
if [ -f "$REPO_ROOT/.tool-versions" ]; then
    cp "$REPO_ROOT/.tool-versions" "$WORKSPACE/.tool-versions"
fi
mkdir -p "$WORKSPACE/templates"
cd "$WORKSPACE"

# ============================================================================
# Template Commands
# ============================================================================
log_section "Template Commands"

# template list
if $GGEN_BIN template list &> /dev/null; then
    log_success "ggen template list"
else
    log_error "ggen template list failed"
fi

# template show (requires valid template)
# Create a minimal template first
cat > templates/test.tmpl << 'EOF'
---
name: "test"
description: "Test template"
version: "1.0.0"
---
Hello {{ name }}!
EOF

if $GGEN_BIN template show --template templates/test.tmpl &> /dev/null; then
    log_success "ggen template show --template"
else
    log_error "ggen template show --template failed"
fi

# template lint
if $GGEN_BIN template lint --template templates/test.tmpl &> /dev/null; then
    log_success "ggen template lint --template"
else
    log_error "ggen template lint --template failed"
fi

# ============================================================================
# Graph Commands
# ============================================================================
log_section "Graph Commands"

# Create test RDF file
cat > test.ttl << 'EOF'
@prefix ex: <http://example.org/> .
ex:test ex:prop "value" .
EOF

# graph load
if $GGEN_BIN graph load --file test.ttl &> /dev/null; then
    log_success "ggen graph load --file"
else
    log_error "ggen graph load --file failed"
fi

# graph query
if $GGEN_BIN graph query --sparql_query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 1" --graph_file test.ttl &> /dev/null; then
    log_success "ggen graph query --sparql_query"
else
    log_error "ggen graph query --sparql_query failed"
fi

# graph export
if $GGEN_BIN graph export --input_file test.ttl --output export.ttl --format turtle &> /dev/null; then
    log_success "ggen graph export --output"
    if [ -f export.ttl ]; then
        log_success "Export file created"
    else
        log_error "Export file not created"
    fi
else
    log_error "ggen graph export failed"
fi

# ============================================================================
# Ontology Commands
# ============================================================================
log_section "Ontology Commands"

# Create ontology file
cat > ontology.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Product a rdfs:Class ;
    rdfs:label "Product" .

ex:name a rdf:Property ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string .
EOF

# ontology extract
if $GGEN_BIN ontology extract --ontology_file ontology.ttl --output schema.json &> /dev/null; then
    log_success "ggen ontology extract --ontology_file"
    if [ -f schema.json ]; then
        log_success "Schema JSON created"
    else
        log_error "Schema JSON not created"
    fi
else
    log_error "ggen ontology extract failed"
fi

# ontology validate (if schema.json exists)
if [ -f schema.json ]; then
    if $GGEN_BIN ontology validate --schema_file schema.json &> /dev/null; then
        log_success "ggen ontology validate"
    else
        log_error "ggen ontology validate failed"
    fi
fi

# ============================================================================
# Project Commands
# ============================================================================
log_section "Project Commands"

# project init
mkdir -p test-project
if $GGEN_BIN project init --path ./test-project --name test-proj &> /dev/null; then
    log_success "ggen project init --name"
else
    log_error "ggen project init failed"
fi

# ============================================================================
# Global Options
# ============================================================================
log_section "Global Options"

# --version
if $GGEN_BIN --version &> /dev/null; then
    log_success "ggen --version"
else
    log_error "ggen --version failed"
fi

# --help
if $GGEN_BIN --help &> /dev/null; then
    log_success "ggen --help"
else
    log_error "ggen --help failed"
fi

# Subcommand help
if $GGEN_BIN template --help &> /dev/null; then
    log_success "ggen template --help"
else
    log_error "ggen template --help failed"
fi

if $GGEN_BIN graph --help &> /dev/null; then
    log_success "ggen graph --help"
else
    log_error "ggen graph --help failed"
fi

if $GGEN_BIN ontology --help &> /dev/null; then
    log_success "ggen ontology --help"
else
    log_error "ggen ontology --help failed"
fi

if $GGEN_BIN project --help &> /dev/null; then
    log_success "ggen project --help"
else
    log_error "ggen project --help failed"
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
    echo -e "${GREEN}✓ CLI Reference: ALL TESTS PASSED${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 0
else
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${RED}✗ CLI Reference: TESTS FAILED${NC}"
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 1
fi
