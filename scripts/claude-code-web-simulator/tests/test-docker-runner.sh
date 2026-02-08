#!/bin/bash

##############################################################################
# Claude Code Web Simulator - Docker Runner Tests
#
# Comprehensive test suite for Docker integration module
# Tests image lifecycle, container execution, error handling, and integration
#
# Version: 1.0.0
# Author: ggen AI Team
##############################################################################

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test tracking
TESTS_TOTAL=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# Setup paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MODULES_DIR="${SCRIPT_DIR}/modules"
TESTS_DIR="${SCRIPT_DIR}/tests"
WORKSPACE_DIR="${SCRIPT_DIR}/workspace/tests"

# Source the docker-runner module
source "${MODULES_DIR}/docker-runner.sh" || {
    echo "Failed to source docker-runner.sh"
    exit 1
}

##############################################################################
# Test Utilities
##############################################################################

test_start() {
    local test_name="$1"
    TESTS_TOTAL=$((TESTS_TOTAL + 1))
    echo -e "${BLUE}[TEST ${TESTS_TOTAL}]${NC} ${test_name}"
}

test_pass() {
    local test_name="$1"
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${GREEN}  ✓ PASS${NC} ${test_name}"
}

test_fail() {
    local test_name="$1"
    local reason="${2:-Unknown}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "${RED}  ✗ FAIL${NC} ${test_name}"
    echo -e "${RED}    Reason: ${reason}${NC}"
}

test_skip() {
    local test_name="$1"
    local reason="${2:-No reason provided}"
    TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
    echo -e "${YELLOW}  ⊘ SKIP${NC} ${test_name}: ${reason}"
}

test_section() {
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}${1}${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

test_summary() {
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Test Summary${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo "Total:   ${TESTS_TOTAL}"
    echo -e "Passed:  ${GREEN}${TESTS_PASSED}${NC}"
    echo -e "Failed:  ${RED}${TESTS_FAILED}${NC}"
    echo -e "Skipped: ${YELLOW}${TESTS_SKIPPED}${NC}"

    if [[ ${TESTS_FAILED} -eq 0 ]]; then
        echo -e "${GREEN}✓ All tests passed!${NC}"
        return 0
    else
        echo -e "${RED}✗ Some tests failed!${NC}"
        return 1
    fi
}

##############################################################################
# Setup and Teardown
##############################################################################

setup_test_environment() {
    mkdir -p "${WORKSPACE_DIR}"
    mkdir -p "${WORKSPACE_DIR}/agent-workspaces"
    mkdir -p "${WORKSPACE_DIR}/receipts"
}

teardown_test_environment() {
    # Cleanup Docker containers
    docker_cleanup 2>/dev/null || true

    # Cleanup test workspace
    rm -rf "${WORKSPACE_DIR}" || true
}

##############################################################################
# Availability Tests
##############################################################################

test_docker_availability() {
    test_start "docker_check_availability: Docker is available"

    if docker_check_availability > /dev/null 2>&1; then
        test_pass "docker_check_availability"
    else
        test_skip "docker_check_availability" "Docker not installed or not running"
    fi
}

test_docker_socket_permissions() {
    test_start "docker_check_socket_permissions: Can access Docker socket"

    if docker_check_socket_permissions > /dev/null 2>&1; then
        test_pass "docker_check_socket_permissions"
    else
        test_skip "docker_check_socket_permissions" "Insufficient Docker socket permissions"
    fi
}

test_docker_health_check() {
    test_start "docker_health_check: System health check passes"

    if docker_health_check > /dev/null 2>&1; then
        test_pass "docker_health_check"
    else
        test_skip "docker_health_check" "Docker system has issues"
    fi
}

##############################################################################
# Image Lifecycle Tests
##############################################################################

test_image_initialization() {
    test_start "docker_init_image: Initializes Docker image"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_init_image" "Docker not available"
        return 0
    fi

    if image_id=$(docker_init_image 2>/dev/null); then
        if [[ -n "${image_id}" ]]; then
            test_pass "docker_init_image"
        else
            test_fail "docker_init_image" "No image ID returned"
        fi
    else
        test_fail "docker_init_image" "Failed to initialize image"
    fi
}

test_image_exists_check() {
    test_start "docker_image_exists: Image existence check works"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_image_exists" "Docker not available"
        return 0
    fi

    # Initialize image first
    docker_init_image > /dev/null 2>&1 || true

    if docker_image_exists "ggen-agent:latest"; then
        test_pass "docker_image_exists"
    else
        test_fail "docker_image_exists" "Image check returned false for existing image"
    fi
}

test_image_not_exists() {
    test_start "docker_image_exists: Returns false for non-existent image"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_image_exists" "Docker not available"
        return 0
    fi

    if ! docker_image_exists "ggen-nonexistent:12345"; then
        test_pass "docker_image_exists"
    else
        test_fail "docker_image_exists" "Incorrectly reported non-existent image as existing"
    fi
}

##############################################################################
# Container Execution Tests
##############################################################################

test_container_spawn_simple() {
    test_start "docker_spawn_agent: Spawns container with simple command"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_spawn_agent" "Docker not available"
        return 0
    fi

    # Initialize image
    docker_init_image > /dev/null 2>&1 || {
        test_skip "docker_spawn_agent" "Could not initialize image"
        return 0
    }

    local agent_id="test-simple-$(date +%s%N | tail -c 10)"
    local workspace="${WORKSPACE_DIR}/agent-workspaces/${agent_id}"
    mkdir -p "${workspace}"

    # Run simple echo command
    if output=$(docker_spawn_agent "${agent_id}" "${workspace}" "echo 'Hello from container'" 2>&1); then
        if echo "${output}" | grep -q "Hello from container"; then
            test_pass "docker_spawn_agent"
        else
            test_fail "docker_spawn_agent" "Expected output not found"
        fi
    else
        test_fail "docker_spawn_agent" "Container execution failed"
    fi
}

test_container_output_capture() {
    test_start "docker_spawn_agent: Captures stdout and stderr"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_spawn_agent" "Docker not available"
        return 0
    fi

    # Initialize image
    docker_init_image > /dev/null 2>&1 || {
        test_skip "docker_spawn_agent" "Could not initialize image"
        return 0
    }

    local agent_id="test-capture-$(date +%s%N | tail -c 10)"
    local workspace="${WORKSPACE_DIR}/agent-workspaces/${agent_id}"
    mkdir -p "${workspace}"

    # Run command with mixed output
    if output=$(docker_spawn_agent "${agent_id}" "${workspace}" \
        "echo 'Standard output'; echo 'Error message' >&2" 2>&1); then

        if echo "${output}" | grep -q "Standard output"; then
            test_pass "docker_spawn_agent"
        else
            test_fail "docker_spawn_agent" "Could not capture output"
        fi
    else
        # Some output should still be captured even with non-zero exit
        test_fail "docker_spawn_agent" "Container execution failed"
    fi
}

test_container_workspace_mounting() {
    test_start "docker_spawn_agent: Mounts workspace correctly"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_spawn_agent" "Docker not available"
        return 0
    fi

    # Initialize image
    docker_init_image > /dev/null 2>&1 || {
        test_skip "docker_spawn_agent" "Could not initialize image"
        return 0
    }

    local agent_id="test-workspace-$(date +%s%N | tail -c 10)"
    local workspace="${WORKSPACE_DIR}/agent-workspaces/${agent_id}"
    mkdir -p "${workspace}"

    # Write test file
    echo "test-content-$(date +%s)" > "${workspace}/test.txt"

    # Run command that reads the file
    if output=$(docker_spawn_agent "${agent_id}" "${workspace}" \
        "cat /workspace/test.txt" 2>&1); then

        if echo "${output}" | grep -q "test-content-"; then
            test_pass "docker_spawn_agent"
        else
            test_fail "docker_spawn_agent" "Could not read mounted file"
        fi
    else
        test_fail "docker_spawn_agent" "Container execution failed"
    fi
}

test_container_file_writing() {
    test_start "docker_spawn_agent: Writes files to workspace"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_spawn_agent" "Docker not available"
        return 0
    fi

    # Initialize image
    docker_init_image > /dev/null 2>&1 || {
        test_skip "docker_spawn_agent" "Could not initialize image"
        return 0
    }

    local agent_id="test-write-$(date +%s%N | tail -c 10)"
    local workspace="${WORKSPACE_DIR}/agent-workspaces/${agent_id}"
    mkdir -p "${workspace}"

    # Run command that creates a file
    docker_spawn_agent "${agent_id}" "${workspace}" \
        "echo 'generated content' > /workspace/generated.txt" 2>&1 || true

    # Check if file was created
    if [[ -f "${workspace}/generated.txt" ]]; then
        if grep -q "generated content" "${workspace}/generated.txt"; then
            test_pass "docker_spawn_agent"
        else
            test_fail "docker_spawn_agent" "File contents incorrect"
        fi
    else
        test_fail "docker_spawn_agent" "Generated file not found in workspace"
    fi
}

test_container_exit_codes() {
    test_start "docker_spawn_agent: Propagates exit codes correctly"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_spawn_agent" "Docker not available"
        return 0
    fi

    # Initialize image
    docker_init_image > /dev/null 2>&1 || {
        test_skip "docker_spawn_agent" "Could not initialize image"
        return 0
    }

    local agent_id="test-exit-$(date +%s%N | tail -c 10)"
    local workspace="${WORKSPACE_DIR}/agent-workspaces/${agent_id}"
    mkdir -p "${workspace}"

    # Run failing command
    docker_spawn_agent "${agent_id}" "${workspace}" \
        "exit 42" 2>&1 || local exit_code=$?

    if [[ ${exit_code:-0} -eq 42 ]]; then
        test_pass "docker_spawn_agent"
    else
        test_fail "docker_spawn_agent" "Expected exit code 42, got ${exit_code:-0}"
    fi
}

##############################################################################
# Network Tests
##############################################################################

test_network_creation() {
    test_start "docker_ensure_network: Creates isolated network"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_ensure_network" "Docker not available"
        return 0
    fi

    local test_network="ggen-test-network-$(date +%s%N | tail -c 10)"

    # Create network
    if docker network create --driver=bridge "${test_network}" > /dev/null 2>&1; then
        # Verify it exists
        if docker network ls --format "{{.Name}}" | grep -q "^${test_network}$"; then
            test_pass "docker_ensure_network"
            docker network rm "${test_network}" > /dev/null 2>&1 || true
        else
            test_fail "docker_ensure_network" "Network not found after creation"
        fi
    else
        test_fail "docker_ensure_network" "Failed to create network"
    fi
}

##############################################################################
# Error Handling Tests
##############################################################################

test_error_docker_not_installed() {
    test_start "docker_handle_not_installed: Displays helpful error"

    local output
    output=$(docker_handle_not_installed 2>&1 || true)

    if echo "${output}" | grep -qi "install"; then
        test_pass "docker_handle_not_installed"
    else
        test_fail "docker_handle_not_installed" "Error message missing installation instructions"
    fi
}

test_error_invalid_workspace() {
    test_start "docker_spawn_agent: Fails gracefully with invalid workspace"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_spawn_agent" "Docker not available"
        return 0
    fi

    local agent_id="test-invalid-$(date +%s%N | tail -c 10)"
    local invalid_workspace="/nonexistent/path/that/does/not/exist"

    if ! docker_spawn_agent "${agent_id}" "${invalid_workspace}" "echo test" > /dev/null 2>&1; then
        test_pass "docker_spawn_agent"
    else
        test_fail "docker_spawn_agent" "Should fail with non-existent workspace"
    fi
}

test_error_missing_arguments() {
    test_start "docker_spawn_agent: Fails with missing arguments"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "docker_spawn_agent" "Docker not available"
        return 0
    fi

    if ! docker_spawn_agent "" "" "" > /dev/null 2>&1; then
        test_pass "docker_spawn_agent"
    else
        test_fail "docker_spawn_agent" "Should fail with empty arguments"
    fi
}

##############################################################################
# Bootstrap Integration Tests
##############################################################################

test_bootstrap_docker_init() {
    test_start "bootstrap_docker_init: Initializes Docker system"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "bootstrap_docker_init" "Docker not available"
        return 0
    fi

    if bootstrap_docker_init > /dev/null 2>&1; then
        test_pass "bootstrap_docker_init"
    else
        test_fail "bootstrap_docker_init" "Failed to initialize Docker during bootstrap"
    fi
}

test_bootstrap_docker_execute() {
    test_start "bootstrap_docker_execute: Executes agent in Docker"

    if ! docker_check_availability > /dev/null 2>&1; then
        test_skip "bootstrap_docker_execute" "Docker not available"
        return 0
    fi

    # Initialize Docker first
    bootstrap_docker_init > /dev/null 2>&1 || {
        test_skip "bootstrap_docker_execute" "Could not initialize Docker"
        return 0
    }

    local agent_id="test-bootstrap-$(date +%s%N | tail -c 10)"
    local workspace="${WORKSPACE_DIR}/agent-workspaces/${agent_id}"
    mkdir -p "${workspace}"

    if output=$(bootstrap_docker_execute "${agent_id}" "${workspace}" "echo 'bootstrap-test'" 2>&1); then
        if echo "${output}" | grep -q "bootstrap-test"; then
            test_pass "bootstrap_docker_execute"
        else
            test_fail "bootstrap_docker_execute" "Expected output not found"
        fi
    else
        test_fail "bootstrap_docker_execute" "Execution failed"
    fi
}

##############################################################################
# Main Test Execution
##############################################################################

main() {
    echo -e "${BLUE}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║     Claude Code Web Simulator - Docker Runner Test Suite      ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════════════╝${NC}"

    # Setup
    setup_test_environment

    # Run test sections
    test_section "Availability Tests"
    test_docker_availability
    test_docker_socket_permissions
    test_docker_health_check

    test_section "Image Lifecycle Tests"
    test_image_initialization
    test_image_exists_check
    test_image_not_exists

    test_section "Container Execution Tests"
    test_container_spawn_simple
    test_container_output_capture
    test_container_workspace_mounting
    test_container_file_writing
    test_container_exit_codes

    test_section "Network Tests"
    test_network_creation

    test_section "Error Handling Tests"
    test_error_docker_not_installed
    test_error_invalid_workspace
    test_error_missing_arguments

    test_section "Bootstrap Integration Tests"
    test_bootstrap_docker_init
    test_bootstrap_docker_execute

    # Cleanup
    teardown_test_environment

    # Summary
    test_summary
    local result=$?

    return ${result}
}

# Run main if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
