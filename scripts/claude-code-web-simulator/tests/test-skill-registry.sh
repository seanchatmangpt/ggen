#!/bin/bash

##############################################################################
# Skill Registry Tests
#
# Comprehensive test suite for the agent skill registry system.
# Tests: loading, registration, validation, and querying skills.
#
# Run with: bash tests/test-skill-registry.sh
##############################################################################

set -euo pipefail

# ============================================================================
# Setup
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
MODULES_DIR="${PROJECT_ROOT}/modules"
CONFIG_DIR="${PROJECT_ROOT}/config"
SKILLS_DIR="${CONFIG_DIR}/agent-skills"

# Source the skill registry module
source "${MODULES_DIR}/skill-registry.sh"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# ============================================================================
# Test Utilities
# ============================================================================

test_start() {
    local test_name="$1"
    echo -e "\n${BLUE}[TEST]${NC} $test_name"
    ((TESTS_RUN++))
}

test_pass() {
    echo -e "${GREEN}[PASS]${NC} Test passed"
    ((TESTS_PASSED++))
}

test_fail() {
    local reason="$1"
    echo -e "${RED}[FAIL]${NC} $reason"
    ((TESTS_FAILED++))
}

assert_equals() {
    local expected="$1"
    local actual="$2"
    local message="$3"

    if [[ "$expected" == "$actual" ]]; then
        echo -e "  ${GREEN}✓${NC} $message"
    else
        echo -e "  ${RED}✗${NC} $message"
        echo -e "    Expected: $expected"
        echo -e "    Actual: $actual"
        test_fail "$message"
        return 1
    fi
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local message="$3"

    if [[ "$haystack" =~ $needle ]]; then
        echo -e "  ${GREEN}✓${NC} $message"
    else
        echo -e "  ${RED}✗${NC} $message"
        echo -e "    Should contain: $needle"
        echo -e "    In: $haystack"
        test_fail "$message"
        return 1
    fi
}

assert_file_exists() {
    local file="$1"
    local message="$2"

    if [[ -f "$file" ]]; then
        echo -e "  ${GREEN}✓${NC} $message"
    else
        echo -e "  ${RED}✗${NC} $message"
        echo -e "    File not found: $file"
        test_fail "$message"
        return 1
    fi
}

# ============================================================================
# Test Cases
# ============================================================================

test_skill_load() {
    test_start "Load Turtle Parser Skill"

    local skill_json
    if skill_json=$(skill_load "turtle-parser"); then
        assert_contains "$skill_json" "turtle-parser" "Skill name present"
        assert_contains "$skill_json" "rdf" "Skill category is RDF"
        assert_contains "$skill_json" "parse_turtle_ontology" "Capability present"
        test_pass
    else
        test_fail "Failed to load turtle-parser skill"
    fi
}

test_skill_validation() {
    test_start "Validate Skill YAML Schema"

    local turtle_file="${SKILLS_DIR}/turtle-parser.yaml"
    if skill_validate_file "$turtle_file"; then
        test_pass
    else
        test_fail "Skill validation failed"
    fi
}

test_all_skills_valid() {
    test_start "Validate All Skill Files"

    local invalid_count=0
    for skill_file in "${SKILLS_DIR}"/*.yaml; do
        if ! skill_validate_file "$skill_file"; then
            invalid_count=$((invalid_count + 1))
        fi
    done

    if [[ $invalid_count -eq 0 ]]; then
        echo "  ${GREEN}✓${NC} All skill files are valid"
        test_pass
    else
        test_fail "$invalid_count skill files are invalid"
    fi
}

test_skill_register() {
    test_start "Register Skill in Registry"

    # Load skill
    local skill_json
    skill_json=$(skill_load "sparql-executor")

    # Register skill
    if skill_register "$skill_json"; then
        # Verify registration
        if [[ -f "${SKILLS_DIR}/.registry.json" ]]; then
            test_pass
        else
            test_fail "Registry database not created"
        fi
    else
        test_fail "Failed to register skill"
    fi
}

test_skill_get_by_name() {
    test_start "Get Skill by Name"

    local skill_json
    if skill_json=$(skill_get_by_name "sparql-executor"); then
        assert_contains "$skill_json" "sparql-executor" "Correct skill returned"
        assert_contains "$skill_json" "3000" "SLO correctly preserved"
        test_pass
    else
        test_fail "Failed to retrieve skill by name"
    fi
}

test_skill_get_by_agent_type() {
    test_start "Query Skills by Agent Type"

    # First, ensure skills are registered
    for skill_file in "${SKILLS_DIR}"/*.yaml; do
        local skill_name=$(basename "$skill_file" .yaml)
        skill_load "$skill_name" | skill_register || true
    done

    # Query skills for code-generator agent
    local skills_json
    if skills_json=$(skill_get_by_agent "code-generator"); then
        # Should contain tera-template-renderer
        assert_contains "$skills_json" "tera-template-renderer" "Code generator skill present"
        test_pass
    else
        test_fail "Failed to query skills by agent type"
    fi
}

test_skill_get_by_category() {
    test_start "Query Skills by Category"

    # Register all skills first
    for skill_file in "${SKILLS_DIR}"/*.yaml; do
        local skill_name=$(basename "$skill_file" .yaml)
        skill_load "$skill_name" | skill_register || true
    done

    # Query RDF category skills
    local skills_json
    if skills_json=$(skill_get_by_category "rdf"); then
        assert_contains "$skills_json" "turtle-parser" "RDF skill present"
        test_pass
    else
        test_fail "Failed to query skills by category"
    fi
}

test_skill_list_all() {
    test_start "List All Registered Skills"

    # Register all skills
    for skill_file in "${SKILLS_DIR}"/*.yaml; do
        local skill_name=$(basename "$skill_file" .yaml)
        skill_load "$skill_name" | skill_register || true
    done

    local skills_list
    if skills_list=$(skill_list_all); then
        # Should be a JSON array
        assert_contains "$skills_list" "\[" "Result is a JSON array"
        assert_contains "$skills_list" "turtle-parser" "At least one skill listed"
        test_pass
    else
        test_fail "Failed to list all skills"
    fi
}

test_skill_unregister() {
    test_start "Unregister Skill"

    # Register a skill first
    local skill_json
    skill_json=$(skill_load "receipt-generator")
    skill_register "$skill_json"

    # Now unregister it
    if skill_unregister "receipt-generator"; then
        # Verify it's gone
        if ! skill_get_by_name "receipt-generator" 2>/dev/null; then
            test_pass
        else
            test_fail "Skill still in registry after unregister"
        fi
    else
        test_fail "Failed to unregister skill"
    fi
}

test_skill_registry_stats() {
    test_start "Get Registry Statistics"

    # Register all skills
    for skill_file in "${SKILLS_DIR}"/*.yaml; do
        local skill_name=$(basename "$skill_file" .yaml)
        skill_load "$skill_name" | skill_register || true
    done

    local stats_json
    if stats_json=$(skill_registry_stats); then
        assert_contains "$stats_json" "total_skills" "Stats contains skill count"
        assert_contains "$stats_json" "total_categories" "Stats contains category count"
        assert_contains "$stats_json" "total_agent_types" "Stats contains agent type count"
        test_pass
    else
        test_fail "Failed to get registry statistics"
    fi
}

test_skill_slo_validation() {
    test_start "Validate Performance SLO Constraints"

    # Test max_duration_ms must be >= 100
    local invalid_slo_json='{
        "skill": {
            "name": "bad-slo-skill",
            "category": "rdf",
            "version": "1.0.0",
            "agent_types": ["test"],
            "description": "Test",
            "capabilities": ["test"],
            "performance_slo": {
                "max_duration_ms": 50,
                "success_rate": 0.99
            },
            "implementation": {
                "language": "bash",
                "entry_point": "test()"
            },
            "error_handling": {
                "retry_strategy": "none",
                "max_retries": 0
            }
        }
    }'

    if ! _validate_skill_schema "$invalid_slo_json" "bad-slo-skill" 2>&1 | grep -q "max_duration_ms"; then
        test_fail "Should reject max_duration_ms < 100"
    else
        echo "  ${GREEN}✓${NC} Correctly rejects invalid max_duration_ms"
        test_pass
    fi
}

test_skill_category_validation() {
    test_start "Validate Skill Category"

    # Test invalid category
    local invalid_category_json='{
        "skill": {
            "name": "bad-category",
            "category": "invalid_category",
            "version": "1.0.0",
            "agent_types": ["test"],
            "description": "Test",
            "capabilities": ["test"],
            "performance_slo": {
                "max_duration_ms": 5000,
                "success_rate": 0.99
            },
            "implementation": {
                "language": "bash",
                "entry_point": "test()"
            },
            "error_handling": {
                "retry_strategy": "none",
                "max_retries": 0
            }
        }
    }'

    if ! _validate_skill_schema "$invalid_category_json" "bad-category" 2>&1 | grep -q "Invalid category"; then
        test_fail "Should reject invalid category"
    else
        echo "  ${GREEN}✓${NC} Correctly rejects invalid category"
        test_pass
    fi
}

test_multiple_agent_types() {
    test_start "Skill with Multiple Agent Types"

    local skill_json
    skill_json=$(skill_load "turtle-parser")
    skill_register "$skill_json"

    # Should be available to multiple agent types
    local agents=$(echo "$skill_json" | jq -r '.skill.agent_types | length')
    if (( agents > 1 )); then
        echo "  ${GREEN}✓${NC} Skill supports $agents agent types"
        test_pass
    else
        test_fail "Skill should support multiple agent types"
    fi
}

test_skill_dependencies() {
    test_start "Verify Skill Tool Dependencies"

    local skill_json
    skill_json=$(skill_load "cargo-make-orchestrator")

    local has_deps
    has_deps=$(echo "$skill_json" | jq -e '.skill.requirements.tool_dependencies' >/dev/null 2>&1 && echo "true" || echo "false")

    if [[ "$has_deps" == "true" ]]; then
        echo "  ${GREEN}✓${NC} Skill has tool dependencies defined"
        test_pass
    else
        test_fail "Skill dependencies not properly defined"
    fi
}

# ============================================================================
# Main Test Execution
# ============================================================================

main() {
    echo -e "\n${BLUE}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}Skill Registry Test Suite${NC}"
    echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"

    # Verify skills directory exists
    if [[ ! -d "$SKILLS_DIR" ]]; then
        echo -e "${RED}Error: Skills directory not found at $SKILLS_DIR${NC}"
        exit 1
    fi

    # Verify at least one skill file exists
    if [[ -z "$(find "$SKILLS_DIR" -name "*.yaml" -type f)" ]]; then
        echo -e "${RED}Error: No skill YAML files found in $SKILLS_DIR${NC}"
        exit 1
    fi

    # Run all tests
    test_skill_load
    test_skill_validation
    test_all_skills_valid
    test_skill_register
    test_skill_get_by_name
    test_skill_get_by_agent_type
    test_skill_get_by_category
    test_skill_list_all
    test_skill_unregister
    test_skill_registry_stats
    test_skill_slo_validation
    test_skill_category_validation
    test_multiple_agent_types
    test_skill_dependencies

    # Print summary
    echo -e "\n${BLUE}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}Test Summary${NC}"
    echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
    echo "Total tests run: $TESTS_RUN"
    echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
    echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}\n"

    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "${GREEN}All tests passed!${NC}"
        return 0
    else
        echo -e "${RED}$TESTS_FAILED test(s) failed${NC}"
        return 1
    fi
}

# ============================================================================
# Run Tests
# ============================================================================

main "$@"
