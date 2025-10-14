#!/usr/bin/env bash
#
# Example: Using cleanroom CLI from Bash
#
# This script demonstrates how to integrate the cleanroom CLI into Bash scripts.
# It shows proper error handling, JSON parsing with jq, and common usage patterns.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
CLEANROOM_CLI="${CLEANROOM_CLI:-cleanroom}"

# Error handling
error() {
    echo -e "${RED}Error: $1${NC}" >&2
    exit 1
}

warn() {
    echo -e "${YELLOW}Warning: $1${NC}" >&2
}

success() {
    echo -e "${GREEN}$1${NC}"
}

# Check if jq is installed
check_dependencies() {
    if ! command -v jq &> /dev/null; then
        warn "jq is not installed. JSON parsing will be limited."
        warn "Install with: brew install jq (macOS) or apt-get install jq (Linux)"
        return 1
    fi
    return 0
}

# Helper function to run cleanroom with JSON output
cleanroom_json() {
    "$CLEANROOM_CLI" "$@" --output json
}

# Helper function to run cleanroom and parse JSON
cleanroom_query() {
    local query="$1"
    shift
    cleanroom_json "$@" | jq -r "$query"
}

# Example 1: Basic Usage
example_basic_usage() {
    echo "=== Basic Usage Example ==="
    echo

    # Create environment
    echo "Creating environment..."
    ENV_ID=$(cleanroom_json environment create --name "test-env" | jq -r '.id // "test-env"')
    success "Created environment: $ENV_ID"
    echo

    # Start PostgreSQL container
    echo "Starting PostgreSQL container..."
    CONTAINER=$(cleanroom_json container start postgres \
        --db testdb \
        --user testuser \
        --password testpass)

    PORT=$(echo "$CONTAINER" | jq -r '.port // "unknown"')
    success "Started container on port: $PORT"
    echo

    if [[ "$PORT" != "unknown" ]]; then
        echo "Connect to: postgresql://testuser:testpass@localhost:$PORT/testdb"
        echo
    fi

    # List running containers
    echo "Listing containers..."
    CONTAINERS=$(cleanroom_json container list)
    echo "$CONTAINERS" | jq '.'
    echo

    # Cleanup
    echo "Cleaning up..."
    cleanroom_json environment delete --name "test-env" > /dev/null
    success "Environment deleted"
    echo
}

# Example 2: Test Runner
example_test_runner() {
    echo "=== Test Runner Example ==="
    echo

    # Create test environment
    cleanroom_json environment create --name "test-env" > /dev/null

    # Start required services
    echo "Starting services..."
    cleanroom_json container start postgres --db testdb > /dev/null
    cleanroom_json container start redis > /dev/null
    success "Services started"
    echo

    # Run tests
    echo "Running tests..."
    RESULTS=$(cleanroom_json test run --file integration_tests.rs)

    # Parse test results
    PASSED=$(echo "$RESULTS" | jq -r '.passed // 0')
    FAILED=$(echo "$RESULTS" | jq -r '.failed // 0')
    DURATION=$(echo "$RESULTS" | jq -r '.duration // 0')

    echo "Tests passed: $PASSED"
    echo "Tests failed: $FAILED"
    echo "Duration: ${DURATION}s"
    echo

    if [[ "$FAILED" -gt 0 ]]; then
        warn "Some tests failed:"
        echo "$RESULTS" | jq -r '.failures[]? // "No details available"'
        echo
    fi

    # Get metrics
    echo "Collecting metrics..."
    METRICS=$(cleanroom_json metrics show)
    echo "$METRICS" | jq '.'
    echo

    # Cleanup
    cleanroom_json environment delete --name "test-env" > /dev/null
}

# Example 3: Multi-Container Setup
example_multi_container() {
    echo "=== Multi-Container Example ==="
    echo

    # Create environment
    cleanroom_json environment create --name "microservices" > /dev/null

    # Define services
    declare -A services=(
        ["postgres"]="--db users --user admin"
        ["redis"]=""
        ["rabbitmq"]=""
    )

    # Start services
    echo "Starting services..."
    for service in "${!services[@]}"; do
        echo "  Starting $service..."
        RESULT=$(cleanroom_json container start $service ${services[$service]})
        PORT=$(echo "$RESULT" | jq -r '.port // "N/A"')
        echo "    $service available on port $PORT"
    done
    echo
    success "All services started!"
    echo

    # Check health
    echo "Checking health..."
    HEALTH=$(cleanroom_json health check)
    echo "$HEALTH" | jq '.'
    echo

    # Wait for user input
    read -p "Press Enter to stop services..."
    echo

    # Cleanup
    echo "Stopping services..."
    cleanroom_json environment delete --name "microservices" > /dev/null
    success "Services stopped"
    echo
}

# Example 4: Error Handling
example_error_handling() {
    echo "=== Error Handling Example ==="
    echo

    # Test 1: Non-existent environment
    echo "Test 1: Deleting non-existent environment"
    if ! cleanroom_json environment delete --name "non-existent" 2>/dev/null; then
        success "Expected error caught"
    else
        warn "Expected error not caught"
    fi
    echo

    # Test 2: Invalid container
    echo "Test 2: Starting invalid container"
    if ! cleanroom_json container start invalid-service 2>/dev/null; then
        success "Expected error caught"
    else
        warn "Expected error not caught"
    fi
    echo

    # Test 3: Graceful degradation without jq
    echo "Test 3: Working without jq"
    RESULT=$(cleanroom_json environment create --name "test-env-2" 2>/dev/null || echo '{"id":"test-env-2"}')
    if [[ -n "$RESULT" ]]; then
        success "Command succeeded even without jq"
        cleanroom_json environment delete --name "test-env-2" > /dev/null 2>&1 || true
    fi
    echo
}

# Example 5: Advanced JSON Parsing
example_json_parsing() {
    echo "=== Advanced JSON Parsing Example ==="
    echo

    check_dependencies || {
        warn "Skipping advanced JSON parsing (jq required)"
        return
    }

    # Create environment and containers
    cleanroom_json environment create --name "parse-test" > /dev/null
    cleanroom_json container start postgres --db testdb > /dev/null
    cleanroom_json container start redis > /dev/null

    # Get metrics
    METRICS=$(cleanroom_json metrics show)

    # Parse specific fields
    echo "Parsing metrics with jq:"
    echo "  Startup time: $(echo "$METRICS" | jq -r '.startup_time // "N/A"')"
    echo "  Memory usage: $(echo "$METRICS" | jq -r '.memory_mb // 0') MB"
    echo "  CPU usage: $(echo "$METRICS" | jq -r '.cpu_percent // 0')%"
    echo "  Tests executed: $(echo "$METRICS" | jq -r '.tests_executed // 0')"
    echo

    # Complex query: Get all container ports
    echo "Container ports:"
    CONTAINERS=$(cleanroom_json container list)
    echo "$CONTAINERS" | jq -r '.[] | "  \(.name): \(.port)"'
    echo

    # Export to file
    echo "Exporting metrics to file..."
    echo "$METRICS" | jq '.' > /tmp/cleanroom-metrics.json
    success "Metrics exported to /tmp/cleanroom-metrics.json"
    echo

    # Cleanup
    cleanroom_json environment delete --name "parse-test" > /dev/null
}

# Example 6: CI/CD Integration
example_cicd_integration() {
    echo "=== CI/CD Integration Example ==="
    echo

    # Set CI environment
    export CI=true

    # Create environment
    ENV_NAME="ci-test-$(date +%s)"
    echo "Creating CI environment: $ENV_NAME"
    cleanroom_json environment create --name "$ENV_NAME" > /dev/null

    # Start services
    echo "Starting services..."
    cleanroom_json container start postgres --db testdb > /dev/null

    # Run tests with timeout
    echo "Running tests (with 60s timeout)..."
    if timeout 60s cleanroom_json test run --file test.rs > /tmp/test-results.json 2>&1; then
        RESULTS=$(cat /tmp/test-results.json)
        PASSED=$(echo "$RESULTS" | jq -r '.passed // 0')
        FAILED=$(echo "$RESULTS" | jq -r '.failed // 0')

        echo "Tests passed: $PASSED"
        echo "Tests failed: $FAILED"

        if [[ "$FAILED" -gt 0 ]]; then
            error "Tests failed in CI"
        else
            success "All tests passed in CI"
        fi
    else
        error "Tests timed out or failed"
    fi
    echo

    # Cleanup
    cleanroom_json environment delete --name "$ENV_NAME" > /dev/null || true
}

# Example 7: Metrics Collection and Reporting
example_metrics_reporting() {
    echo "=== Metrics Reporting Example ==="
    echo

    # Create environment
    cleanroom_json environment create --name "metrics-test" > /dev/null

    # Run operations
    echo "Running operations..."
    cleanroom_json container start postgres > /dev/null
    cleanroom_json test run --file test.rs > /dev/null 2>&1 || true

    # Collect metrics
    METRICS=$(cleanroom_json metrics show)

    # Generate report
    echo "Performance Report:"
    echo "==================="

    if check_dependencies; then
        echo "$METRICS" | jq -r 'to_entries | .[] | "  \(.key): \(.value)"'
    else
        echo "$METRICS"
    fi
    echo

    # Export in multiple formats
    echo "Exporting metrics..."
    echo "$METRICS" | jq '.' > /tmp/metrics.json

    if check_dependencies; then
        echo "$METRICS" | jq -r '@csv' > /tmp/metrics.csv
        success "Exported to JSON and CSV"
    else
        success "Exported to JSON"
    fi
    echo

    # Cleanup
    cleanroom_json environment delete --name "metrics-test" > /dev/null
}

# Main function
main() {
    echo "Cleanroom CLI Bash Integration Examples"
    echo "========================================"
    echo

    # Check if cleanroom is available
    if ! command -v "$CLEANROOM_CLI" &> /dev/null; then
        error "cleanroom CLI not found at: $CLEANROOM_CLI"
    fi

    # Run examples
    local examples=(
        "example_basic_usage"
        "example_test_runner"
        "example_multi_container"
        "example_error_handling"
        "example_json_parsing"
        "example_cicd_integration"
        "example_metrics_reporting"
    )

    for example in "${examples[@]}"; do
        $example
        echo "=========================================="
        echo
    done

    success "All examples completed!"
}

# Run main function
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
