#!/bin/bash
set -euo pipefail

# SPARQL CLI Validation Script
# Validates package structure, ontology, and functionality

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=========================================="
echo "SPARQL CLI Validation Script"
echo "=========================================="
echo "Project: $PROJECT_ROOT"
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASS_COUNT=0
FAIL_COUNT=0
WARN_COUNT=0

pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((PASS_COUNT++))
}

fail() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((FAIL_COUNT++))
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
    ((WARN_COUNT++))
}

check() {
    if [ $1 -eq 0 ]; then
        pass "$2"
    else
        fail "$2"
    fi
}

cd "$PROJECT_ROOT"

# 1. File Structure Validation
echo "1. Validating file structure..."

[ -f "package.toml" ] && pass "package.toml exists" || fail "package.toml missing"
[ -f "Cargo.toml" ] && pass "Cargo.toml exists" || fail "Cargo.toml missing"
[ -f "README.md" ] && pass "README.md exists" || fail "README.md missing"
[ -f "rdf/ontology.ttl" ] && pass "RDF ontology exists" || fail "RDF ontology missing"
[ -d "src" ] && pass "src/ directory exists" || fail "src/ directory missing"
[ -d "tests" ] && pass "tests/ directory exists" || fail "tests/ directory missing"
[ -d "docs/diagrams" ] && pass "docs/diagrams/ exists" || fail "docs/diagrams/ missing"

# Check for license files
if [ -f "LICENSE-MIT" ] && [ -f "LICENSE-APACHE" ]; then
    pass "Dual license files present"
elif [ -f "LICENSE-MIT" ] || [ -f "LICENSE-APACHE" ]; then
    warn "Only one license file present"
else
    fail "No license files found"
fi

# 2. RDF Ontology Validation
echo ""
echo "2. Validating RDF ontology..."

if [ -f "rdf/ontology.ttl" ]; then
    # Check Turtle syntax
    grep -q "@prefix rdf:" rdf/ontology.ttl && pass "RDF prefix defined" || fail "RDF prefix missing"
    grep -q "@prefix cnv:" rdf/ontology.ttl && pass "CNV prefix defined" || fail "CNV prefix missing"
    grep -q "@prefix sparql:" rdf/ontology.ttl && pass "SPARQL prefix defined" || fail "SPARQL prefix missing"

    # Count nouns
    NOUN_COUNT=$(grep -c "a cnv:Noun" rdf/ontology.ttl || true)
    if [ "$NOUN_COUNT" -ge 4 ]; then
        pass "Found $NOUN_COUNT nouns (expected >= 4)"
    else
        fail "Found $NOUN_COUNT nouns (expected >= 4)"
    fi

    # Count verbs
    VERB_COUNT=$(grep -c "a cnv:Verb" rdf/ontology.ttl || true)
    if [ "$VERB_COUNT" -ge 16 ]; then
        pass "Found $VERB_COUNT verbs (expected >= 16)"
    else
        fail "Found $VERB_COUNT verbs (expected >= 16)"
    fi

    # Check specific nouns
    grep -q "sparql:Query a cnv:Noun" rdf/ontology.ttl && pass "Query noun defined" || fail "Query noun missing"
    grep -q "sparql:Endpoint a cnv:Noun" rdf/ontology.ttl && pass "Endpoint noun defined" || fail "Endpoint noun missing"
    grep -q "sparql:Federation a cnv:Noun" rdf/ontology.ttl && pass "Federation noun defined" || fail "Federation noun missing"
    grep -q "sparql:Optimization a cnv:Noun" rdf/ontology.ttl && pass "Optimization noun defined" || fail "Optimization noun missing"

    # Check verb categories
    grep -q "sparql:execute a cnv:Verb" rdf/ontology.ttl && pass "execute verb defined" || fail "execute verb missing"
    grep -q "sparql:explain a cnv:Verb" rdf/ontology.ttl && pass "explain verb defined" || fail "explain verb missing"
    grep -q "sparql:register a cnv:Verb" rdf/ontology.ttl && pass "register verb defined" || fail "register verb missing"
    grep -q "sparql:merge a cnv:Verb" rdf/ontology.ttl && pass "merge verb defined" || fail "merge verb missing"
    grep -q "sparql:rewrite a cnv:Verb" rdf/ontology.ttl && pass "rewrite verb defined" || fail "rewrite verb missing"

    # Check argument definitions
    ARG_COUNT=$(grep -c "a cnv:Argument" rdf/ontology.ttl || true)
    if [ "$ARG_COUNT" -ge 15 ]; then
        pass "Found $ARG_COUNT arguments"
    else
        warn "Found $ARG_COUNT arguments (may need more)"
    fi

    # Check return types
    RETURN_COUNT=$(grep -c "a cnv:ReturnType" rdf/ontology.ttl || true)
    if [ "$RETURN_COUNT" -ge 10 ]; then
        pass "Found $RETURN_COUNT return types"
    else
        warn "Found $RETURN_COUNT return types (may need more)"
    fi
fi

# 3. Package.toml Validation
echo ""
echo "3. Validating package.toml..."

if [ -f "package.toml" ]; then
    grep -q 'name = "sparql-cli"' package.toml && pass "Package name correct" || fail "Package name incorrect"
    grep -q 'version = "1.0.0"' package.toml && pass "Version specified" || fail "Version missing"
    grep -q "sparql" package.toml && pass "SPARQL keyword present" || fail "SPARQL keyword missing"
    grep -q "clap-noun-verb" package.toml && pass "clap-noun-verb mentioned" || fail "clap-noun-verb missing"
    grep -q "ontology.ttl" package.toml && pass "Ontology reference present" || fail "Ontology reference missing"
fi

# 4. Cargo.toml Validation
echo ""
echo "4. Validating Cargo.toml..."

if [ -f "Cargo.toml" ]; then
    grep -q 'name = "sparql-cli"' Cargo.toml && pass "Cargo package name correct" || fail "Cargo package name incorrect"
    grep -q 'clap-noun-verb = ' Cargo.toml && pass "clap-noun-verb dependency present" || fail "clap-noun-verb dependency missing"
    grep -q 'oxigraph = ' Cargo.toml && pass "oxigraph dependency present" || fail "oxigraph dependency missing"
    grep -q 'spargebra = ' Cargo.toml && pass "spargebra dependency present" || fail "spargebra dependency missing"
    grep -q '\[features\]' Cargo.toml && pass "Features section present" || warn "Features section missing"
fi

# 5. Documentation Validation
echo ""
echo "5. Validating documentation..."

if [ -f "README.md" ]; then
    wc -l README.md | awk '{print $1}' | {
        read lines
        if [ "$lines" -ge 600 ]; then
            pass "README.md has $lines lines (>= 600)"
        else
            fail "README.md has $lines lines (< 600)"
        fi
    }

    grep -q "## Features" README.md && pass "Features section present" || fail "Features section missing"
    grep -q "## Installation" README.md && pass "Installation section present" || fail "Installation section missing"
    grep -q "## Usage" README.md && pass "Usage section present" || fail "Usage section missing"
    grep -q "## Examples" README.md && pass "Examples section present" || fail "Examples section missing"
    grep -q "SPARQL 1.1" README.md && pass "SPARQL 1.1 mentioned" || warn "SPARQL 1.1 not mentioned"
    grep -q "federation" README.md && pass "Federation documented" || fail "Federation not documented"
    grep -q "optimization" README.md && pass "Optimization documented" || fail "Optimization not documented"
fi

# 6. PlantUML Diagrams Validation
echo ""
echo "6. Validating PlantUML diagrams..."

if [ -f "docs/diagrams/sparql-query-flow.puml" ]; then
    pass "sparql-query-flow.puml exists"
    grep -q "@startuml" docs/diagrams/sparql-query-flow.puml && pass "Valid PlantUML syntax" || fail "Invalid PlantUML syntax"
else
    fail "sparql-query-flow.puml missing"
fi

if [ -f "docs/diagrams/sparql-federation.puml" ]; then
    pass "sparql-federation.puml exists"
    grep -q "Federation" docs/diagrams/sparql-federation.puml && pass "Federation diagram content valid" || warn "Federation content minimal"
else
    fail "sparql-federation.puml missing"
fi

if [ -f "docs/diagrams/sparql-optimization.puml" ]; then
    pass "sparql-optimization.puml exists"
    grep -q "Optimization" docs/diagrams/sparql-optimization.puml && pass "Optimization diagram content valid" || warn "Optimization content minimal"
else
    fail "sparql-optimization.puml missing"
fi

# 7. Script Validation
echo ""
echo "7. Validating scripts..."

[ -x "scripts/deploy.sh" ] && pass "deploy.sh is executable" || warn "deploy.sh not executable"
[ -x "scripts/validate.sh" ] && pass "validate.sh is executable" || warn "validate.sh not executable"
[ -f "scripts/benchmark.sh" ] && pass "benchmark.sh exists" || warn "benchmark.sh missing"

# 8. Source Code Validation
echo ""
echo "8. Validating source code..."

[ -f "src/main.rs" ] && pass "src/main.rs exists" || fail "src/main.rs missing"
[ -f "src/lib.rs" ] && pass "src/lib.rs exists" || fail "src/lib.rs missing"

if [ -f "src/main.rs" ]; then
    grep -q "fn main" src/main.rs && pass "main() function present" || fail "main() function missing"
fi

# 9. Test Validation
echo ""
echo "9. Validating tests..."

[ -f "tests/integration_test.rs" ] && pass "integration_test.rs exists" || fail "integration_test.rs missing"

if [ -f "tests/integration_test.rs" ]; then
    grep -q "#\[test\]" tests/integration_test.rs && pass "Test functions present" || warn "No test functions found"
fi

# 10. Example Validation
echo ""
echo "10. Validating examples..."

[ -f "examples/basic.rs" ] && pass "examples/basic.rs exists" || warn "examples/basic.rs missing"

if [ -f "examples/basic.rs" ]; then
    grep -q "fn main" examples/basic.rs && pass "Example has main() function" || warn "Example missing main()"
fi

# Summary
echo ""
echo "=========================================="
echo "Validation Summary"
echo "=========================================="
echo -e "${GREEN}Passed:${NC} $PASS_COUNT"
echo -e "${YELLOW}Warnings:${NC} $WARN_COUNT"
echo -e "${RED}Failed:${NC} $FAIL_COUNT"
echo "=========================================="

if [ $FAIL_COUNT -eq 0 ]; then
    echo -e "${GREEN}✓ All critical validations passed!${NC}"
    exit 0
else
    echo -e "${RED}✗ Validation failed with $FAIL_COUNT error(s)${NC}"
    exit 1
fi
