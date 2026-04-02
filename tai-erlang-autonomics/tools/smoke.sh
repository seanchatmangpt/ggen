#!/bin/bash
# TAIEA Smoke Test Suite
# Validates that the running TAIEA service responds to core endpoints
# Usage: ./tools/smoke.sh [base_url]

set -e

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
BASE_URL="${1:-http://localhost:8080}"
TIMEOUT=10
MAX_RETRIES=30
RETRY_DELAY=1

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Logging functions
log_info() {
    echo -e "${BLUE}ℹ${NC}  $1"
}

log_success() {
    echo -e "${GREEN}✓${NC}  $1"
}

log_fail() {
    echo -e "${RED}✗${NC}  $1"
}

log_warn() {
    echo -e "${YELLOW}⚠${NC}  $1"
}

# Test helper function
run_test() {
    local test_name="$1"
    local method="${2:-GET}"
    local endpoint="$3"
    local data="$4"
    local expected_status="${5:-200}"

    TESTS_RUN=$((TESTS_RUN + 1))

    local url="${BASE_URL}${endpoint}"
    local curl_opts="-s -w %{http_code} -o /tmp/response.json --max-time $TIMEOUT"

    # Add method if not GET
    if [ "$method" != "GET" ]; then
        curl_opts="$curl_opts -X $method"
    fi

    # Add data if provided
    if [ -n "$data" ]; then
        curl_opts="$curl_opts -H 'Content-Type: application/json' -d '$data'"
    fi

    # Execute curl
    local response
    response=$(eval "curl $curl_opts '$url'")

    if [ "$response" = "$expected_status" ]; then
        log_success "$test_name (HTTP $response)"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        log_fail "$test_name (expected HTTP $expected_status, got $response)"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

# Wait for service to be available
wait_for_service() {
    local attempt=1
    local start_time=$(date +%s)

    log_info "Waiting for service to be available at $BASE_URL..."

    while [ $attempt -le $MAX_RETRIES ]; do
        if curl -s --max-time 2 "$BASE_URL/health" > /dev/null 2>&1; then
            log_success "Service is available"
            return 0
        fi

        echo -n "."
        sleep $RETRY_DELAY
        attempt=$((attempt + 1))
    done

    local elapsed=$(($(date +%s) - start_time))
    log_fail "Service did not become available after ${elapsed}s"
    return 1
}

# Header
echo ""
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║              TAIEA Smoke Test Suite (v1.0)                    ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

log_info "Service URL: $BASE_URL"
log_info "Timeout: ${TIMEOUT}s per request"
echo ""

# Wait for service
if ! wait_for_service; then
    log_fail "Cannot proceed: Service is unavailable"
    exit 1
fi

echo ""
log_info "Running smoke tests..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Test 1: Health Check (GET /health)
run_test "Test 1: Health check (GET /health)" "GET" "/health" "" "200"

# Test 2: Marketplace Event Submission (POST /marketplace)
run_test "Test 2: Marketplace event (POST /marketplace)" "POST" "/marketplace" \
    '{"tenant_id":"test-tenant-001","event_type":"sku_changed","event_data":{"sku":"professional","timestamp":1234567890}}' \
    "202"

# Test 3: PubSub Webhook (POST /pubsub)
run_test "Test 3: PubSub webhook (POST /pubsub)" "POST" "/pubsub" \
    '{"message":{"attributes":{},"data":"eyJ0ZXN0IjoidmFsdWUifQ=="},"subscription":"projects/test-project/subscriptions/test-sub"}' \
    "202"

# Test 4: Metrics (GET /metrics - Prometheus format)
run_test "Test 4: Metrics endpoint (GET /metrics)" "GET" "/metrics" "" "200"

# Test 5: Ready check (GET /ready or /health with specific status)
run_test "Test 5: Ready check (GET /ready)" "GET" "/ready" "" "200"

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Summary
log_info "Test Summary:"
echo ""
echo "  Tests run:    $TESTS_RUN"
echo "  Tests passed: ${GREEN}$TESTS_PASSED${NC}"
if [ $TESTS_FAILED -gt 0 ]; then
    echo "  Tests failed: ${RED}$TESTS_FAILED${NC}"
else
    echo "  Tests failed: $TESTS_FAILED"
fi
echo ""

# Determine exit code
if [ $TESTS_FAILED -eq 0 ] && [ $TESTS_PASSED -gt 0 ]; then
    echo "╔═══════════════════════════════════════════════════════════════╗"
    echo "║          ${GREEN}✓ All smoke tests PASSED${NC}                           ║"
    echo "╚═══════════════════════════════════════════════════════════════╝"
    echo ""
    exit 0
else
    echo "╔═══════════════════════════════════════════════════════════════╗"
    echo "║          ${RED}✗ Some smoke tests FAILED${NC}                          ║"
    echo "╚═══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "Debug Information:"
    log_info "Last response saved to: /tmp/response.json"
    if [ -f /tmp/response.json ]; then
        echo "  Content:"
        cat /tmp/response.json 2>/dev/null | head -20 || echo "  (could not read response)"
    fi
    echo ""
    exit 1
fi

