#!/bin/bash
set -euo pipefail

# Workflow Engine CLI Validation Script
# Validates BPMN workflows, ontology, and package structure

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

PASS=0
FAIL=0
WARN=0

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((PASS++))
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((FAIL++))
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
    ((WARN++))
}

validate_ontology() {
    echo "=== Validating RDF Ontology ==="

    ONTOLOGY="$PROJECT_ROOT/rdf/ontology.ttl"

    if [ ! -f "$ONTOLOGY" ]; then
        log_fail "Ontology file not found: $ONTOLOGY"
        return
    fi

    # Check file size
    SIZE=$(wc -l < "$ONTOLOGY")
    if [ "$SIZE" -ge 300 ]; then
        log_pass "Ontology size: $SIZE lines (>= 300 required)"
    else
        log_fail "Ontology size: $SIZE lines (< 300 required)"
    fi

    # Validate Turtle syntax
    if command -v rapper &> /dev/null; then
        if rapper -i turtle -o ntriples "$ONTOLOGY" > /dev/null 2>&1; then
            log_pass "Ontology syntax is valid Turtle"
        else
            log_fail "Ontology syntax is invalid"
        fi
    else
        log_warn "rapper not found, skipping syntax validation"
    fi

    # Check for required nouns
    for noun in Workflow Process Task Instance; do
        if grep -q "wfe:$noun" "$ONTOLOGY"; then
            log_pass "Noun found: $noun"
        else
            log_fail "Noun missing: $noun"
        fi
    done

    # Check for required verbs
    for verb in create deploy start pause assign complete; do
        if grep -q "wfe:$verb" "$ONTOLOGY"; then
            log_pass "Verb found: $verb"
        else
            log_fail "Verb missing: $verb"
        fi
    done

    # Check BPMN 2.0 references
    if grep -q "bpmn:" "$ONTOLOGY"; then
        log_pass "BPMN 2.0 namespace referenced"
    else
        log_fail "BPMN 2.0 namespace not found"
    fi
}

validate_package_structure() {
    echo ""
    echo "=== Validating Package Structure ==="

    # Required files
    REQUIRED_FILES=(
        "package.toml"
        "README.md"
        "Cargo.toml"
        "rdf/ontology.ttl"
        "src/main.rs"
        "src/lib.rs"
        "LICENSE-MIT"
        "LICENSE-APACHE"
    )

    for file in "${REQUIRED_FILES[@]}"; do
        if [ -f "$PROJECT_ROOT/$file" ]; then
            log_pass "File exists: $file"
        else
            log_fail "File missing: $file"
        fi
    done

    # Required directories
    REQUIRED_DIRS=(
        "docs/diagrams"
        "scripts"
        "src"
        "examples"
        "tests"
    )

    for dir in "${REQUIRED_DIRS[@]}"; do
        if [ -d "$PROJECT_ROOT/$dir" ]; then
            log_pass "Directory exists: $dir"
        else
            log_fail "Directory missing: $dir"
        fi
    done
}

validate_readme() {
    echo ""
    echo "=== Validating README ==="

    README="$PROJECT_ROOT/README.md"

    if [ ! -f "$README" ]; then
        log_fail "README.md not found"
        return
    fi

    # Check size
    SIZE=$(wc -l < "$README")
    if [ "$SIZE" -ge 500 ]; then
        log_pass "README size: $SIZE lines (>= 500 required)"
    else
        log_fail "README size: $SIZE lines (< 500 required)"
    fi

    # Check required sections
    SECTIONS=(
        "Overview"
        "Installation"
        "Quick Start"
        "BPMN 2.0 Support"
        "State Management"
        "Event Handling"
        "Performance"
    )

    for section in "${SECTIONS[@]}"; do
        if grep -q "## $section" "$README"; then
            log_pass "Section found: $section"
        else
            log_fail "Section missing: $section"
        fi
    done

    # Check for examples
    if grep -q '```bash' "$README"; then
        log_pass "Bash examples found"
    else
        log_fail "No bash examples found"
    fi

    if grep -q '```rust' "$README"; then
        log_pass "Rust examples found"
    else
        log_warn "No Rust examples found"
    fi
}

validate_diagrams() {
    echo ""
    echo "=== Validating PlantUML Diagrams ==="

    DIAGRAMS_DIR="$PROJECT_ROOT/docs/diagrams"

    REQUIRED_DIAGRAMS=(
        "workflow-engine-architecture.puml"
        "bpmn-execution-flow.puml"
        "task-lifecycle.puml"
    )

    for diagram in "${REQUIRED_DIAGRAMS[@]}"; do
        DIAGRAM_PATH="$DIAGRAMS_DIR/$diagram"
        if [ -f "$DIAGRAM_PATH" ]; then
            log_pass "Diagram exists: $diagram"

            # Validate PlantUML syntax
            if head -1 "$DIAGRAM_PATH" | grep -q "@startuml"; then
                log_pass "Valid PlantUML header: $diagram"
            else
                log_fail "Invalid PlantUML header: $diagram"
            fi

            if tail -1 "$DIAGRAM_PATH" | grep -q "@enduml"; then
                log_pass "Valid PlantUML footer: $diagram"
            else
                log_fail "Invalid PlantUML footer: $diagram"
            fi
        else
            log_fail "Diagram missing: $diagram"
        fi
    done
}

validate_cargo_toml() {
    echo ""
    echo "=== Validating Cargo.toml ==="

    CARGO_TOML="$PROJECT_ROOT/Cargo.toml"

    if [ ! -f "$CARGO_TOML" ]; then
        log_fail "Cargo.toml not found"
        return
    fi

    # Check required dependencies
    DEPS=(
        "clap-noun-verb"
        "tokio"
        "serde"
        "petgraph"
        "uuid"
    )

    for dep in "${DEPS[@]}"; do
        if grep -q "^$dep " "$CARGO_TOML"; then
            log_pass "Dependency found: $dep"
        else
            log_fail "Dependency missing: $dep"
        fi
    done

    # Check binary definition
    if grep -q '^\[\[bin\]\]' "$CARGO_TOML"; then
        log_pass "Binary definition found"
    else
        log_fail "Binary definition missing"
    fi

    # Validate Cargo.toml syntax
    if cargo read-manifest --manifest-path "$CARGO_TOML" > /dev/null 2>&1; then
        log_pass "Cargo.toml syntax is valid"
    else
        log_fail "Cargo.toml syntax is invalid"
    fi
}

validate_examples() {
    echo ""
    echo "=== Validating Examples ==="

    EXAMPLES_DIR="$PROJECT_ROOT/examples"

    REQUIRED_EXAMPLES=(
        "service_task.rs"
        "parallel_gateway.rs"
    )

    for example in "${REQUIRED_EXAMPLES[@]}"; do
        if [ -f "$EXAMPLES_DIR/$example" ]; then
            log_pass "Example exists: $example"

            # Check if example compiles
            if cargo check --example "${example%.rs}" --manifest-path "$PROJECT_ROOT/Cargo.toml" &> /dev/null; then
                log_pass "Example compiles: $example"
            else
                log_warn "Example may not compile: $example"
            fi
        else
            log_fail "Example missing: $example"
        fi
    done
}

validate_tests() {
    echo ""
    echo "=== Validating Tests ==="

    TESTS_DIR="$PROJECT_ROOT/tests"

    if [ -f "$TESTS_DIR/integration_test.rs" ]; then
        log_pass "Integration test exists"

        # Try to compile tests
        if cargo test --no-run --manifest-path "$PROJECT_ROOT/Cargo.toml" &> /dev/null; then
            log_pass "Tests compile successfully"
        else
            log_warn "Tests may not compile"
        fi
    else
        log_fail "Integration test missing"
    fi
}

generate_report() {
    echo ""
    echo "========================================="
    echo "Validation Summary"
    echo "========================================="
    echo -e "${GREEN}PASS: $PASS${NC}"
    echo -e "${YELLOW}WARN: $WARN${NC}"
    echo -e "${RED}FAIL: $FAIL${NC}"
    echo ""

    if [ "$FAIL" -eq 0 ]; then
        echo -e "${GREEN}All validations passed!${NC}"
        return 0
    else
        echo -e "${RED}Some validations failed!${NC}"
        return 1
    fi
}

main() {
    echo "Workflow Engine CLI - Validation Report"
    echo "========================================"
    echo ""

    validate_ontology
    validate_package_structure
    validate_readme
    validate_diagrams
    validate_cargo_toml
    validate_examples
    validate_tests

    generate_report
}

main "$@"
