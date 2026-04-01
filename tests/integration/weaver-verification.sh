#!/usr/bin/env bash
# =============================================================================
# Weaver Live-Check Verification Script for ggen
# Runs 12 tests to verify weaver integration is working correctly.
#
# Based on ~/chatmangpt/scripts/verify-weaver-live-check.sh
#
# Usage:
#   docker compose -f docker-compose.otel-test.yml -f docker-compose.weaver.yml up -d
#   bash weaver-verification.sh
# =============================================================================

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0
SKIPPED=0

check_test() {
    local test_name="$1"
    local test_command="$2"

    echo -n "  $test_name ... "

    if eval "$test_command" > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        ((PASSED++))
        return 0
    else
        echo -e "${RED}FAIL${NC}"
        ((FAILED++))
        return 1
    fi
}

echo "============================================"
echo "Weaver Live-Check Verification (ggen)"
echo "============================================"
echo ""

# Test 1: Weaver container is running
echo "T1: Weaver container is running"
if docker ps --format '{{.Names}}' | grep -q 'ggen-weaver-live-check'; then
    echo -e "  ${GREEN}PASS${NC}: Container ggen-weaver-live-check is running"
    ((PASSED++))
else
    echo -e "  ${RED}FAIL${NC}: Container ggen-weaver-live-check is NOT running"
    ((FAILED++))
fi

# Test 2: OTEL collector gRPC port is reachable
echo "T2: OTEL collector gRPC port is reachable"
if check_test "Port 4317 reachable" "nc -z localhost 4317"; then
    : # Passed
fi

# Test 3: OTEL collector HTTP port is reachable
echo "T3: OTEL collector HTTP port is reachable"
if check_test "Port 4318 reachable" "nc -z localhost 4318"; then
    : # Passed
fi

# Test 4: Weaver admin port is reachable
echo "T4: Weaver admin port is reachable"
if check_test "Port 4320 reachable" "curl -sf http://localhost:4320/"; then
    : # Passed
fi

# Test 5: Send a valid ggen.llm.generation span
echo "T5: Send valid ggen.llm.generation span"
RESPONSE=$(curl -s -w "\n%{http_code}" -X POST http://localhost:4318/v1/traces \
    -H "Content-Type: application/json" \
    -d '{
        "resourceSpans": [{
            "resource": {
                "attributes": [{
                    "key": "service.name",
                    "value": { "stringValue": "ggen-test" }
                }]
            },
            "scopeSpans": [{
                "scope": { "name": "ggen-ai" },
                "spans": [{
                    "traceId": "4bf92f3577b34da6a3ce929d0e0e4736",
                    "spanId": "00f067aa0ba902b7",
                    "name": "ggen.llm.generation",
                    "kind": 2,
                    "startTimeUnixNano": "1645123456789000000",
                    "endTimeUnixNano": "1645123456790000000",
                    "attributes": [
                        { "key": "llm.model", "value": { "stringValue": "claude-3-5-sonnet" } },
                        { "key": "llm.prompt_tokens", "value": { "intValue": "100" } },
                        { "key": "llm.completion_tokens", "value": { "intValue": "50" } },
                        { "key": "llm.total_tokens", "value": { "intValue": "150" } }
                    ],
                    "status": { "code": 1 }
                }]
            }]
        }]
    }' 2>&1)

HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
if [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "202" ]; then
    echo -e "  ${GREEN}PASS${NC}: Valid span accepted by collector"
    ((PASSED++))
else
    echo -e "  ${RED}FAIL${NC}: Span rejected (HTTP $HTTP_CODE)"
    ((FAILED++))
fi

# Test 6: Send invalid span (unknown attribute)
echo "T6: Send span with unknown attribute (should trigger weaver warning)"
RESPONSE=$(curl -s -w "\n%{http_code}" -X POST http://localhost:4318/v1/traces \
    -H "Content-Type: application/json" \
    -d '{
        "resourceSpans": [{
            "resource": {
                "attributes": [{
                    "key": "service.name",
                    "value": { "stringValue": "ggen-test-invalid" }
                }]
            },
            "scopeSpans": [{
                "scope": { "name": "ggen-ai" },
                "spans": [{
                    "traceId": "5bf92f3577b34da6a3ce929d0e0e4737",
                    "spanId": "01f067aa0ba902b8",
                    "name": "ggen.llm.generation",
                    "kind": 2,
                    "startTimeUnixNano": "1645123456789000000",
                    "endTimeUnixNano": "1645123456790000000",
                    "attributes": [
                        { "key": "totally.bogus.attribute", "value": { "stringValue": "invalid" } }
                    ],
                    "status": { "code": 1 }
                }]
            }]
        }]
    }' 2>&1)

HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
if [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "202" ]; then
    echo -e "  ${GREEN}PASS${NC}: Invalid span accepted for validation"
    ((PASSED++))
else
    echo -e "  ${RED}FAIL${NC}: Span rejected (HTTP $HTTP_CODE)"
    ((FAILED++))
fi

# Test 7: Semconv registry mounted in container
echo "T7: Semconv registry mounted in container"
if docker exec ggen-weaver-live-check test -f /semconv/model/manifest.yaml; then
    echo -e "  ${GREEN}PASS${NC}: Semconv registry manifest.yaml found in container"
    ((PASSED++))
else
    echo -e "  ${RED}FAIL${NC}: Semconv registry manifest.yaml NOT found in container"
    ((FAILED++))
fi

# Test 8: Weaver binary works
echo "T8: Weaver binary works"
if docker exec ggen-weaver-live-check weaver --version > /dev/null 2>&1; then
    VERSION=$(docker exec ggen-weaver-live-check weaver --version)
    echo -e "  ${GREEN}PASS${NC}: Weaver binary works: $VERSION"
    ((PASSED++))
else
    echo -e "  ${RED}FAIL${NC}: Weaver binary not functional"
    ((FAILED++))
fi

# Test 9: Weaver producing logs
echo "T9: Weaver container producing logs"
if docker logs ggen-weaver-live-check --tail 5 | grep -q .; then
    echo -e "  ${GREEN}PASS${NC}: Weaver container producing log output"
    ((PASSED++))
    echo -e "  ${YELLOW}--- Last 5 lines of weaver logs ---${NC}"
    docker logs ggen-weaver-live-check --tail 5 | sed 's/^/    /'
    echo -e "  ${YELLOW}---${NC}"
else
    echo -e "  ${RED}FAIL${NC}: Weaver container not producing logs"
    ((FAILED++))
fi

# Test 10: Jaeger UI accessible
echo "T10: Jaeger UI is accessible"
if curl -sf http://localhost:16686 > /dev/null 2>&1; then
    echo -e "  ${GREEN}PASS${NC}: Jaeger UI accessible at http://localhost:16686"
    ((PASSED++))
else
    echo -e "  ${RED}FAIL${NC}: Jaeger UI not accessible"
    ((FAILED++))
fi

# Test 11: OTEL Collector health check
echo "T11: OTEL Collector health check"
if curl -sf http://localhost:13133 > /dev/null 2>&1; then
    echo -e "  ${GREEN}PASS${NC}: OTEL Collector health check passed"
    ((PASSED++))
else
    echo -e "  ${RED}FAIL${NC}: OTEL Collector health check failed"
    ((FAILED++))
fi

# Test 12: Reports directory has output
echo "T12: Weaver reports directory has output"
REPORT_COUNT=$(docker exec ggen-weaver-live-check ls -1 /reports/ 2>/dev/null | wc -l | tr -d ' ')
if [ "$REPORT_COUNT" -gt 0 ]; then
    echo -e "  ${GREEN}PASS${NC}: Weaver reports directory has $REPORT_COUNT file(s)"
    ((PASSED++))
else
    echo -e "  ${YELLOW}SKIP${NC}: Weaver reports directory empty (may need /stop call to flush)"
    ((SKIPPED++))
fi

echo ""
echo "============================================"
echo "Results: $PASSED passed, $FAILED failed, $SKIPPED skipped"
echo "============================================"

if [ $FAILED -gt 0 ]; then
    exit 1
else
    exit 0
fi
