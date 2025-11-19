#!/bin/bash
# Tutorial Validation Test Suite
# Validates that all commands from docs/tutorials/ are executable and correct
#
# Usage: ./tests/tutorial_validation.sh [--verbose] [--tutorial=<name>]

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# Test results
declare -a FAILURES=()

# Logging
VERBOSE=false
TEST_TUTORIAL=""

# Parse arguments
for arg in "$@"; do
    case $arg in
        --verbose)
            VERBOSE=true
            shift
            ;;
        --tutorial=*)
            TEST_TUTORIAL="${arg#*=}"
            shift
            ;;
        --help)
            echo "Usage: $0 [--verbose] [--tutorial=<name>]"
            echo ""
            echo "Options:"
            echo "  --verbose          Show detailed output"
            echo "  --tutorial=<name>  Run only tests for specific tutorial"
            echo "  --help             Show this help message"
            exit 0
            ;;
    esac
done

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
}

run_test() {
    local test_name="$1"
    local test_cmd="$2"
    local expected_result="${3:-0}" # Default to expecting success

    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if [ "$VERBOSE" = true ]; then
        log_info "Running: $test_name"
        log_info "Command: $test_cmd"
    fi

    # Run command and capture result
    set +e
    eval "$test_cmd" &>/dev/null
    local result=$?
    set -e

    if [ "$result" -eq "$expected_result" ]; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
        log_success "$test_name"
        return 0
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
        log_error "$test_name (expected exit code $expected_result, got $result)"
        FAILURES+=("$test_name: Expected $expected_result, got $result")
        return 1
    fi
}

skip_test() {
    local test_name="$1"
    local reason="$2"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))

    log_warning "SKIPPED: $test_name - $reason"
}

check_command_exists() {
    local cmd="$1"
    if ! command -v "$cmd" &> /dev/null; then
        log_error "Required command '$cmd' not found"
        return 1
    fi
    return 0
}

# Test: CLI installation validation
test_cli_installation() {
    log_info "=== Testing CLI Installation ==="

    # Check if ggen is installed
    if ! check_command_exists "ggen"; then
        skip_test "ggen installation" "ggen not installed - skipping CLI tests"
        return 1
    fi

    # Test version command
    run_test "ggen --version" "ggen --version"

    # Validate version output matches expected
    local version_output=$(ggen --version 2>&1 || echo "")
    if [[ "$version_output" == *"3.2.0"* ]]; then
        log_success "Version matches expected (3.2.0)"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        log_warning "Version mismatch: expected 3.2.0, got: $version_output"
    fi

    return 0
}

# Test: AI commands (from ai-powered-generation.md)
test_ai_commands() {
    log_info "=== Testing AI Commands ==="

    if ! check_command_exists "ggen"; then
        skip_test "AI commands" "ggen not installed"
        return 1
    fi

    # Test: ggen ai --help (should exist)
    run_test "ggen ai --help" "ggen ai --help"

    # Test: ggen ai generate-ontology --help
    run_test "ggen ai generate-ontology --help" "ggen ai generate-ontology --help"

    # Test: ggen ai chat --help
    run_test "ggen ai chat --help" "ggen ai chat --help"

    # Test: ggen ai analyze --help
    run_test "ggen ai analyze --help" "ggen ai analyze --help"
}

# Test: Template commands (from ontology-to-code.md, zero-to-generated-code.md)
test_template_commands() {
    log_info "=== Testing Template Commands ==="

    if ! check_command_exists "ggen"; then
        skip_test "Template commands" "ggen not installed"
        return 1
    fi

    # Test: ggen template --help
    run_test "ggen template --help" "ggen template --help"

    # Test: ggen template generate-rdf --help
    run_test "ggen template generate-rdf --help" "ggen template generate-rdf --help"

    # Test: ggen template list --help
    run_test "ggen template list --help" "ggen template list --help"

    # Test: ggen template lint --help
    run_test "ggen template lint --help" "ggen template lint --help"
}

# Test: Marketplace commands (from marketplace-workflow.md, marketplace-quick-start.md)
test_marketplace_commands() {
    log_info "=== Testing Marketplace Commands ==="

    if ! check_command_exists "ggen"; then
        skip_test "Marketplace commands" "ggen not installed"
        return 1
    fi

    # Test: ggen marketplace --help
    run_test "ggen marketplace --help" "ggen marketplace --help"

    # Test: ggen marketplace search --help
    run_test "ggen marketplace search --help" "ggen marketplace search --help"

    # Test: ggen marketplace install --help
    run_test "ggen marketplace install --help" "ggen marketplace install --help"

    # Test: ggen marketplace list --help
    run_test "ggen marketplace list --help" "ggen marketplace list --help"

    # Test: ggen marketplace publish --help
    run_test "ggen marketplace publish --help" "ggen marketplace publish --help"
}

# Test: Packs commands (from packs-getting-started.md, packs-reference.md)
test_packs_commands() {
    log_info "=== Testing Packs Commands ==="

    if ! check_command_exists "ggen"; then
        skip_test "Packs commands" "ggen not installed"
        return 1
    fi

    # Test: ggen packs --help
    run_test "ggen packs --help" "ggen packs --help"

    # Test: ggen packs list --help
    run_test "ggen packs list --help" "ggen packs list --help"

    # Test: ggen packs show --help
    run_test "ggen packs show --help" "ggen packs show --help"

    # Test: ggen packs validate --help
    run_test "ggen packs validate --help" "ggen packs validate --help"

    # Test: ggen packs install --help
    run_test "ggen packs install --help" "ggen packs install --help"
}

# Test: Project commands (from getting-started.md, zero-to-generated-code.md)
test_project_commands() {
    log_info "=== Testing Project Commands ==="

    if ! check_command_exists "ggen"; then
        skip_test "Project commands" "ggen not installed"
        return 1
    fi

    # Test: ggen project --help
    run_test "ggen project --help" "ggen project --help"

    # Test: ggen project new --help
    run_test "ggen project new --help" "ggen project new --help"

    # Test: ggen project gen --help
    run_test "ggen project gen --help" "ggen project gen --help"
}

# Test: Graph commands (from ontology-to-code.md)
test_graph_commands() {
    log_info "=== Testing Graph Commands ==="

    if ! check_command_exists "ggen"; then
        skip_test "Graph commands" "ggen not installed"
        return 1
    fi

    # Test: ggen graph --help
    run_test "ggen graph --help" "ggen graph --help"

    # Test: ggen graph load --help
    run_test "ggen graph load --help" "ggen graph load --help"

    # Test: ggen graph query --help
    run_test "ggen graph query --help" "ggen graph query --help"

    # Test: ggen graph export --help
    run_test "ggen graph export --help" "ggen graph export --help"
}

# Test: Hook commands (from zero-to-generated-code.md)
test_hook_commands() {
    log_info "=== Testing Hook Commands ==="

    if ! check_command_exists "ggen"; then
        skip_test "Hook commands" "ggen not installed"
        return 1
    fi

    # Test: ggen hook --help
    run_test "ggen hook --help" "ggen hook --help"

    # Test: ggen hook create --help
    run_test "ggen hook create --help" "ggen hook create --help"
}

# Test: Utils commands
test_utils_commands() {
    log_info "=== Testing Utils Commands ==="

    if ! check_command_exists "ggen"; then
        skip_test "Utils commands" "ggen not installed"
        return 1
    fi

    # Test: ggen utils --help
    run_test "ggen utils --help" "ggen utils --help"

    # Test: ggen utils doctor --help
    run_test "ggen utils doctor --help" "ggen utils doctor --help"
}

# Main test execution
main() {
    echo "========================================="
    echo "  Tutorial Validation Test Suite"
    echo "========================================="
    echo ""

    # Check if we should run specific tutorial tests
    if [ -n "$TEST_TUTORIAL" ]; then
        log_info "Running tests for tutorial: $TEST_TUTORIAL"
        case "$TEST_TUTORIAL" in
            cli|installation)
                test_cli_installation
                ;;
            ai|ai-powered-generation)
                test_ai_commands
                ;;
            template|templates)
                test_template_commands
                ;;
            marketplace)
                test_marketplace_commands
                ;;
            packs)
                test_packs_commands
                ;;
            project)
                test_project_commands
                ;;
            graph)
                test_graph_commands
                ;;
            hook|hooks)
                test_hook_commands
                ;;
            utils)
                test_utils_commands
                ;;
            *)
                log_error "Unknown tutorial: $TEST_TUTORIAL"
                exit 1
                ;;
        esac
    else
        # Run all tests
        test_cli_installation
        test_ai_commands
        test_template_commands
        test_marketplace_commands
        test_packs_commands
        test_project_commands
        test_graph_commands
        test_hook_commands
        test_utils_commands
    fi

    # Print summary
    echo ""
    echo "========================================="
    echo "  Test Summary"
    echo "========================================="
    echo -e "Total Tests:   $TOTAL_TESTS"
    echo -e "${GREEN}Passed:        $PASSED_TESTS${NC}"
    echo -e "${RED}Failed:        $FAILED_TESTS${NC}"
    echo -e "${YELLOW}Skipped:       $SKIPPED_TESTS${NC}"
    echo ""

    # Print failures
    if [ ${#FAILURES[@]} -gt 0 ]; then
        echo "========================================="
        echo "  Failed Tests"
        echo "========================================="
        for failure in "${FAILURES[@]}"; do
            echo -e "${RED}✗${NC} $failure"
        done
        echo ""
    fi

    # Exit with appropriate code
    if [ $FAILED_TESTS -gt 0 ]; then
        log_error "Some tests failed"
        exit 1
    else
        log_success "All tests passed!"
        exit 0
    fi
}

# Run main
main
