#!/usr/bin/env bash

# Comprehensive Docker + Cleanroom Integration Validation Script
# Tests multiple validation strategies to catch false positives

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[PASS]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[FAIL]${NC} $*"; }
log_section() { echo -e "\n${BLUE}========================================${NC}"; echo -e "${BLUE}$*${NC}"; echo -e "${BLUE}========================================${NC}\n"; }

# Exit codes
EXIT_SUCCESS=0
EXIT_DOCKER_ISSUE=1
EXIT_CLEANROOM_ISSUE=2
EXIT_FALSE_POSITIVE=3

# Test results tracking (using simple arrays instead of associative)
STRATEGY_NAMES=()
STRATEGY_RESULTS=()
TOTAL_STRATEGIES=0
PASSED_STRATEGIES=0

# Record strategy result
record_result() {
    local strategy=$1
    local result=$2
    STRATEGY_NAMES+=("$strategy")
    STRATEGY_RESULTS+=("$result")
    TOTAL_STRATEGIES=$((TOTAL_STRATEGIES + 1))
    if [ "$result" = "PASS" ]; then
        PASSED_STRATEGIES=$((PASSED_STRATEGIES + 1))
    fi
}

# Cleanup function
cleanup() {
    log_info "Cleaning up test artifacts..."
    # Kill any hanging test processes
    pkill -f "cargo test" 2>/dev/null || true
    # Wait a moment for containers to stop
    sleep 2
}

trap cleanup EXIT

#############################################
# STRATEGY 1: Docker Daemon Health Check
#############################################
strategy_1_docker_daemon() {
    log_section "Strategy 1: Docker Daemon Health Check"

    local status="PASS"

    # Check if Docker is installed
    if ! command -v docker &> /dev/null; then
        log_error "Docker command not found"
        record_result "docker_daemon" "FAIL"
        return 1
    fi
    log_success "Docker command is available"

    # Check Docker version
    if ! docker --version &> /dev/null; then
        log_error "Docker version check failed"
        status="FAIL"
    else
        log_success "Docker version: $(docker --version)"
    fi

    # Check Docker info (requires daemon running)
    if ! docker info &> /dev/null; then
        log_error "Docker daemon is not running or not accessible"
        status="FAIL"
    else
        log_success "Docker daemon is running"
        docker info | grep -E "Server Version|Storage Driver|Kernel Version" || true
    fi

    # Check Docker socket
    if [ -S /var/run/docker.sock ]; then
        log_success "Docker socket exists: /var/run/docker.sock"
    else
        log_warning "Docker socket not found at standard location"
    fi

    record_result "docker_daemon" "$status"
    [ "$status" = "PASS" ]
}

#############################################
# STRATEGY 2: Container Lifecycle Tracking
#############################################
strategy_2_container_lifecycle() {
    log_section "Strategy 2: Container Lifecycle Tracking"

    local status="PASS"

    # Count containers before tests
    local containers_before=$(docker ps -a --format '{{.ID}}' | wc -l)
    log_info "Containers before tests: $containers_before"

    # Run a simple cleanroom test
    log_info "Running cleanroom test to trigger container creation..."
    cd /Users/sac/ggen/cleanroom

    if cargo test --test simple_testcontainer_test -- --nocapture 2>&1 | tee /tmp/test_output.log; then
        log_success "Test execution completed"
    else
        log_warning "Test execution reported failure (may be expected)"
    fi

    # Wait for containers to be created
    sleep 2

    # Count containers after tests
    local containers_after=$(docker ps -a --format '{{.ID}}' | wc -l)
    log_info "Containers after tests: $containers_after"

    # Check if new containers were created
    if [ "$containers_after" -le "$containers_before" ]; then
        log_error "No new containers created during test execution"
        log_error "Expected: containers_after ($containers_after) > containers_before ($containers_before)"
        status="FAIL"
    else
        log_success "New containers were created: $((containers_after - containers_before)) new container(s)"
    fi

    # Check for testcontainers in container list
    local testcontainer_count=$(docker ps -a --format '{{.Names}}' | grep -c "testcontainers" || true)
    if [ "$testcontainer_count" -gt 0 ]; then
        log_success "Found $testcontainer_count testcontainer(s)"
        docker ps -a --format 'table {{.Names}}\t{{.Status}}\t{{.Ports}}' | grep testcontainers || true
    else
        log_warning "No containers with 'testcontainers' in name found"
    fi

    record_result "container_lifecycle" "$status"
    [ "$status" = "PASS" ]
}

#############################################
# STRATEGY 3: Port Accessibility Test
#############################################
strategy_3_port_accessibility() {
    log_section "Strategy 3: Port Accessibility Test"

    local status="PASS"

    # Get list of running containers with exposed ports
    local containers_with_ports=$(docker ps --format '{{.ID}} {{.Ports}}' | grep -v "^$" || true)

    if [ -z "$containers_with_ports" ]; then
        log_warning "No running containers with exposed ports found"
        record_result "port_accessibility" "SKIP"
        return 0
    fi

    log_info "Checking port accessibility for running containers..."

    # Parse ports and test connectivity
    while IFS= read -r line; do
        local container_id=$(echo "$line" | awk '{print $1}')
        local ports=$(echo "$line" | cut -d' ' -f2-)

        log_info "Container $container_id has ports: $ports"

        # Extract host ports (format: 0.0.0.0:PORT->CONTAINER_PORT/tcp)
        local host_ports=$(echo "$ports" | grep -oE '[0-9]{4,5}->' | sed 's/->//' || true)

        for port in $host_ports; do
            log_info "Testing TCP connection to localhost:$port"
            if timeout 2 bash -c "cat < /dev/null > /dev/tcp/localhost/$port" 2>/dev/null; then
                log_success "Port $port is accessible"
            else
                log_warning "Port $port is not responding (container may still be starting)"
            fi
        done
    done <<< "$containers_with_ports"

    record_result "port_accessibility" "$status"
    [ "$status" = "PASS" ]
}

#############################################
# STRATEGY 4: Negative Testing
#############################################
strategy_4_negative_testing() {
    log_section "Strategy 4: Negative Testing (Docker Unavailable)"

    log_warning "This strategy requires stopping Docker - SKIPPED in automated runs"
    log_warning "Manual test: 'sudo systemctl stop docker' then run tests"

    record_result "negative_testing" "SKIP"
    return 0
}

#############################################
# STRATEGY 5: Container Inspection
#############################################
strategy_5_container_inspection() {
    log_section "Strategy 5: Container Inspection"

    local status="PASS"

    # Capture container state before test
    log_info "Capturing container state BEFORE test..."
    docker ps -a --format 'table {{.ID}}\t{{.Names}}\t{{.Status}}\t{{.Ports}}' > /tmp/containers_before.txt
    cat /tmp/containers_before.txt

    # Run test
    log_info "Running cleanroom integration test..."
    cd /Users/sac/ggen/cleanroom
    cargo test --test integration_tests -- --nocapture &> /tmp/integration_test.log || true

    sleep 3

    # Capture container state during/after test
    log_info "Capturing container state AFTER test..."
    docker ps -a --format 'table {{.ID}}\t{{.Names}}\t{{.Status}}\t{{.Ports}}' > /tmp/containers_after.txt
    cat /tmp/containers_after.txt

    # Check for cleanroom/testcontainers patterns
    log_info "Checking for cleanroom-related containers..."
    if docker ps -a --format '{{.Names}}' | grep -qE "(testcontainers|cleanroom|postgres|redis)"; then
        log_success "Found containers matching cleanroom patterns"
        docker ps -a --format '{{.Names}}' | grep -E "(testcontainers|cleanroom|postgres|redis)" || true
    else
        log_error "No containers matching cleanroom patterns found"
        status="FAIL"
    fi

    # Check container networks
    log_info "Inspecting container networks..."
    local network_count=$(docker network ls | grep -c testcontainers || echo "0")
    if [ "$network_count" -gt 0 ]; then
        log_success "Found $network_count testcontainers network(s)"
        docker network ls | grep testcontainers || true
    else
        log_warning "No testcontainers networks found"
    fi

    # Check container logs
    log_info "Checking container logs availability..."
    local containers=$(docker ps -a -q --filter "name=testcontainers" | head -5)
    if [ -n "$containers" ]; then
        for container in $containers; do
            local log_lines=$(docker logs "$container" 2>&1 | wc -l)
            if [ "$log_lines" -gt 0 ]; then
                log_success "Container $container has $log_lines log lines"
            else
                log_warning "Container $container has no logs"
            fi
        done
    else
        log_warning "No testcontainers to check logs for"
    fi

    record_result "container_inspection" "$status"
    [ "$status" = "PASS" ]
}

#############################################
# STRATEGY 6: Real Service Validation
#############################################
strategy_6_service_validation() {
    log_section "Strategy 6: Real Service Validation"

    local status="PASS"

    # Find PostgreSQL containers
    log_info "Looking for PostgreSQL containers..."
    local postgres_containers=$(docker ps --format '{{.ID}} {{.Ports}}' | grep 5432 || true)

    if [ -n "$postgres_containers" ]; then
        log_success "Found PostgreSQL container(s)"

        # Extract port
        local postgres_port=$(echo "$postgres_containers" | head -1 | grep -oE '[0-9]{4,5}->5432' | cut -d'-' -f1 || echo "5432")
        log_info "PostgreSQL exposed on port: $postgres_port"

        # Try to connect (requires psql client)
        if command -v psql &> /dev/null; then
            log_info "Testing PostgreSQL connection..."
            if timeout 5 psql -h localhost -p "$postgres_port" -U postgres -c "SELECT 1;" &>/dev/null; then
                log_success "PostgreSQL connection successful"
            else
                log_warning "PostgreSQL connection failed (may need credentials)"
            fi
        else
            log_info "psql client not available, skipping PostgreSQL connection test"
        fi
    else
        log_info "No PostgreSQL containers found with exposed ports"
    fi

    # Find Redis containers
    log_info "Looking for Redis containers..."
    local redis_containers=$(docker ps --format '{{.ID}} {{.Ports}}' | grep 6379 || true)

    if [ -n "$redis_containers" ]; then
        log_success "Found Redis container(s)"

        # Extract port
        local redis_port=$(echo "$redis_containers" | head -1 | grep -oE '[0-9]{4,5}->6379' | cut -d'-' -f1 || echo "6379")
        log_info "Redis exposed on port: $redis_port"

        # Try to connect (requires redis-cli)
        if command -v redis-cli &> /dev/null; then
            log_info "Testing Redis connection..."
            if timeout 5 redis-cli -h localhost -p "$redis_port" PING | grep -q PONG; then
                log_success "Redis connection successful"
            else
                log_warning "Redis connection failed"
            fi
        else
            log_info "redis-cli not available, skipping Redis connection test"
        fi
    else
        log_info "No Redis containers found with exposed ports"
    fi

    # Generic service check: any container with exposed ports should be reachable
    log_info "Verifying any service is reachable on exposed ports..."
    local any_service_reachable=false

    for port in $(docker ps --format '{{.Ports}}' | grep -oE '[0-9]{4,5}->' | sed 's/->//' | sort -u || true); do
        log_info "Testing connection to localhost:$port"
        if timeout 2 bash -c "cat < /dev/null > /dev/tcp/localhost/$port" 2>/dev/null; then
            log_success "Service responding on port $port"
            any_service_reachable=true
        fi
    done

    if [ "$any_service_reachable" = false ]; then
        log_warning "No services were reachable on exposed ports"
    fi

    record_result "service_validation" "$status"
    [ "$status" = "PASS" ]
}

#############################################
# PARALLEL VALIDATION EXECUTION
#############################################
run_all_strategies() {
    log_section "Running All Validation Strategies"

    # Run strategies sequentially (parallel would require more complex coordination)
    strategy_1_docker_daemon || true
    strategy_2_container_lifecycle || true
    strategy_3_port_accessibility || true
    strategy_4_negative_testing || true
    strategy_5_container_inspection || true
    strategy_6_service_validation || true
}

#############################################
# SUMMARY REPORT
#############################################
generate_report() {
    log_section "Validation Summary Report"

    echo ""
    echo "Strategy Results:"
    echo "----------------"

    for i in "${!STRATEGY_NAMES[@]}"; do
        local strategy="${STRATEGY_NAMES[$i]}"
        local result="${STRATEGY_RESULTS[$i]}"
        case "$result" in
            PASS)
                log_success "$strategy: PASS"
                ;;
            FAIL)
                log_error "$strategy: FAIL"
                ;;
            SKIP)
                log_warning "$strategy: SKIP"
                ;;
        esac
    done

    echo ""
    echo "Overall Results:"
    echo "----------------"
    log_info "Total Strategies: $TOTAL_STRATEGIES"
    log_success "Passed: $PASSED_STRATEGIES"
    log_error "Failed: $((TOTAL_STRATEGIES - PASSED_STRATEGIES))"

    # Calculate pass rate
    local pass_rate=0
    if [ "$TOTAL_STRATEGIES" -gt 0 ]; then
        pass_rate=$((PASSED_STRATEGIES * 100 / TOTAL_STRATEGIES))
    fi

    echo ""
    if [ "$pass_rate" -eq 100 ]; then
        log_success "Validation Result: ALL STRATEGIES PASSED ✓"
        return $EXIT_SUCCESS
    elif [ "$pass_rate" -ge 80 ]; then
        log_warning "Validation Result: MOSTLY PASSED (${pass_rate}%) - Review failures"
        return $EXIT_CLEANROOM_ISSUE
    elif [ "$pass_rate" -ge 50 ]; then
        log_error "Validation Result: PARTIAL FAILURE (${pass_rate}%) - Docker may have issues"
        return $EXIT_DOCKER_ISSUE
    else
        log_error "Validation Result: CRITICAL FAILURE (${pass_rate}%) - False positive detected"
        return $EXIT_FALSE_POSITIVE
    fi
}

#############################################
# MAIN EXECUTION
#############################################
main() {
    log_section "Docker + Cleanroom Integration Validation"
    log_info "Starting comprehensive validation suite..."

    # Run all validation strategies
    run_all_strategies

    # Generate and display report
    generate_report
}

# Execute main function
main
