#!/bin/bash
set -euo pipefail

# Data Pipeline CLI - Validation Script
# Validates package structure, code quality, and RDF ontology

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo "=========================================="
echo "Data Pipeline CLI - Package Validation"
echo "=========================================="
echo

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0

# Functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_test() {
    echo -e "${BLUE}[TEST]${NC} $1"
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((PASSED++))
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((FAILED++))
}

# Test 1: Directory structure
test_directory_structure() {
    log_test "Validating directory structure..."

    local required_dirs=(
        "rdf"
        "docs/diagrams"
        "scripts"
        "src"
        "examples"
        "tests"
    )

    for dir in "${required_dirs[@]}"; do
        if [ -d "$PROJECT_DIR/$dir" ]; then
            log_pass "Directory exists: $dir"
        else
            log_fail "Missing directory: $dir"
        fi
    done
}

# Test 2: Required files
test_required_files() {
    log_test "Validating required files..."

    local required_files=(
        "Cargo.toml"
        "package.toml"
        "README.md"
        "LICENSE-MIT"
        "LICENSE-APACHE"
        ".gitignore"
        "rdf/ontology.ttl"
        "src/main.rs"
        "src/lib.rs"
    )

    for file in "${required_files[@]}"; do
        if [ -f "$PROJECT_DIR/$file" ]; then
            log_pass "File exists: $file"
        else
            log_fail "Missing file: $file"
        fi
    done
}

# Test 3: RDF ontology validation
test_rdf_ontology() {
    log_test "Validating RDF ontology..."

    local ontology="$PROJECT_DIR/rdf/ontology.ttl"

    if [ ! -f "$ontology" ]; then
        log_fail "Ontology file not found"
        return
    fi

    # Check for required namespaces
    local namespaces=("dpipe:" "clap:" "rdfs:" "owl:")
    for ns in "${namespaces[@]}"; do
        if grep -q "$ns" "$ontology"; then
            log_pass "Namespace defined: $ns"
        else
            log_fail "Missing namespace: $ns"
        fi
    done

    # Check for noun classes
    local nouns=("Pipeline" "Source" "Transform" "Sink")
    for noun in "${nouns[@]}"; do
        if grep -q "dpipe:$noun a owl:Class" "$ontology"; then
            log_pass "Noun class defined: $noun"
        else
            log_fail "Missing noun class: $noun"
        fi
    done

    # Check for verbs
    local verbs=("create" "run" "register" "test" "map" "filter" "write")
    for verb in "${verbs[@]}"; do
        if grep -q "dpipe:$verb a clap:Verb" "$ontology"; then
            log_pass "Verb defined: $verb"
        else
            log_fail "Missing verb: $verb"
        fi
    done

    # Line count check
    local line_count=$(wc -l < "$ontology")
    if [ "$line_count" -ge 300 ]; then
        log_pass "Ontology has $line_count lines (>= 300 required)"
    else
        log_fail "Ontology only has $line_count lines (< 300 required)"
    fi
}

# Test 4: Cargo.toml validation
test_cargo_toml() {
    log_test "Validating Cargo.toml..."

    local cargo_toml="$PROJECT_DIR/Cargo.toml"

    # Check required fields
    local fields=("name" "version" "description" "license" "edition")
    for field in "${fields[@]}"; do
        if grep -q "^$field = " "$cargo_toml"; then
            log_pass "Cargo.toml has field: $field"
        else
            log_fail "Cargo.toml missing field: $field"
        fi
    done

    # Check for clap-noun-verb dependency
    if grep -q "clap-noun-verb" "$cargo_toml"; then
        log_pass "clap-noun-verb dependency present"
    else
        log_fail "Missing clap-noun-verb dependency"
    fi
}

# Test 5: README validation
test_readme() {
    log_test "Validating README.md..."

    local readme="$PROJECT_DIR/README.md"

    if [ ! -f "$readme" ]; then
        log_fail "README.md not found"
        return
    fi

    # Check for required sections
    local sections=("Overview" "Installation" "Quick Start" "Usage" "Examples")
    for section in "${sections[@]}"; do
        if grep -qi "## .*$section" "$readme"; then
            log_pass "README has section: $section"
        else
            log_fail "README missing section: $section"
        fi
    done

    # Line count check
    local line_count=$(wc -l < "$readme")
    if [ "$line_count" -ge 500 ]; then
        log_pass "README has $line_count lines (>= 500 required)"
    else
        log_fail "README only has $line_count lines (< 500 required)"
    fi
}

# Test 6: PlantUML diagrams
test_plantuml_diagrams() {
    log_test "Validating PlantUML diagrams..."

    local diagrams=(
        "data-pipeline-architecture.puml"
        "etl-flow.puml"
        "data-transformation.puml"
    )

    for diagram in "${diagrams[@]}"; do
        local file="$PROJECT_DIR/docs/diagrams/$diagram"
        if [ -f "$file" ]; then
            if grep -q "@startuml" "$file" && grep -q "@enduml" "$file"; then
                log_pass "Valid PlantUML diagram: $diagram"
            else
                log_fail "Invalid PlantUML diagram: $diagram"
            fi
        else
            log_fail "Missing diagram: $diagram"
        fi
    done
}

# Test 7: Code structure
test_code_structure() {
    log_test "Validating code structure..."

    # Check main.rs
    if [ -f "$PROJECT_DIR/src/main.rs" ]; then
        if grep -q "ClapNounVerb" "$PROJECT_DIR/src/main.rs"; then
            log_pass "main.rs uses ClapNounVerb"
        else
            log_fail "main.rs missing ClapNounVerb integration"
        fi
    fi

    # Check lib.rs
    if [ -f "$PROJECT_DIR/src/lib.rs" ]; then
        if grep -q "pub mod" "$PROJECT_DIR/src/lib.rs"; then
            log_pass "lib.rs exports modules"
        else
            log_fail "lib.rs has no public modules"
        fi
    fi
}

# Test 8: Examples
test_examples() {
    log_test "Validating examples..."

    local examples=(
        "csv_to_rdf.rs"
        "api_ingestion.rs"
        "data_enrichment.rs"
    )

    for example in "${examples[@]}"; do
        if [ -f "$PROJECT_DIR/examples/$example" ]; then
            log_pass "Example exists: $example"
        else
            log_fail "Missing example: $example"
        fi
    done
}

# Test 9: Build check
test_build() {
    log_test "Validating build..."

    cd "$PROJECT_DIR"

    if cargo check --quiet 2>/dev/null; then
        log_pass "Cargo check passed"
    else
        log_fail "Cargo check failed"
    fi
}

# Test 10: Deployment scripts
test_deployment_scripts() {
    log_test "Validating deployment scripts..."

    local scripts=("deploy.sh" "validate.sh" "benchmark.sh")

    for script in "${scripts[@]}"; do
        local file="$PROJECT_DIR/scripts/$script"
        if [ -f "$file" ]; then
            if [ -x "$file" ]; then
                log_pass "Script exists and is executable: $script"
            else
                log_fail "Script not executable: $script"
            fi
        else
            log_fail "Missing script: $script"
        fi
    done
}

# Generate validation report
generate_report() {
    echo
    echo "=========================================="
    echo "Validation Report"
    echo "=========================================="
    echo
    echo "Tests Passed: $PASSED"
    echo "Tests Failed: $FAILED"
    echo "Total Tests: $((PASSED + FAILED))"
    echo

    local success_rate=$(( (PASSED * 100) / (PASSED + FAILED) ))
    echo "Success Rate: $success_rate%"
    echo

    if [ "$FAILED" -eq 0 ]; then
        log_info "All validation tests passed! ✓"
        echo
        log_info "Production Readiness: 100%"
        return 0
    else
        log_fail "Some validation tests failed"
        echo
        log_info "Production Readiness: $success_rate%"
        return 1
    fi
}

# Main validation flow
main() {
    test_directory_structure
    test_required_files
    test_rdf_ontology
    test_cargo_toml
    test_readme
    test_plantuml_diagrams
    test_code_structure
    test_examples
    test_build
    test_deployment_scripts

    generate_report
}

# Run main and exit with appropriate code
main
exit $?
