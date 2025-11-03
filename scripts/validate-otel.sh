#!/bin/bash
# OTEL Integration Validation Script for ggen P2P Marketplace
# This script validates that OpenTelemetry traces are ACTUALLY emitted to collector

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
COMPOSE_FILE="$PROJECT_ROOT/tests/integration/docker-compose.otel-test.yml"
COLLECTOR_HEALTH="http://localhost:13133"
COLLECTOR_METRICS="http://localhost:8888/metrics"
JAEGER_UI="http://localhost:16686"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}ggen P2P Marketplace - OTEL Validation${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Function to print colored status
print_status() {
    local status=$1
    local message=$2
    if [ "$status" == "OK" ]; then
        echo -e "${GREEN}✅ $message${NC}"
    elif [ "$status" == "FAIL" ]; then
        echo -e "${RED}❌ $message${NC}"
    elif [ "$status" == "WARN" ]; then
        echo -e "${YELLOW}⚠️  $message${NC}"
    else
        echo -e "${BLUE}ℹ️  $message${NC}"
    fi
}

# Step 1: Check Docker
echo -e "${BLUE}Step 1: Checking Docker...${NC}"
if ! command -v docker &> /dev/null; then
    print_status "FAIL" "Docker not found. Please install Docker."
    exit 1
fi

if ! docker info &> /dev/null; then
    print_status "FAIL" "Docker daemon not running. Please start Docker."
    exit 1
fi

print_status "OK" "Docker is running"
echo ""

# Step 2: Start OTEL Infrastructure
echo -e "${BLUE}Step 2: Starting OTEL infrastructure...${NC}"
cd "$PROJECT_ROOT"

if [ ! -f "$COMPOSE_FILE" ]; then
    print_status "FAIL" "docker-compose.otel-test.yml not found"
    exit 1
fi

# Stop existing containers
docker-compose -f "$COMPOSE_FILE" down -v 2>/dev/null || true

# Start services
print_status "INFO" "Starting OTEL collector, Jaeger, and Prometheus..."
docker-compose -f "$COMPOSE_FILE" up -d

print_status "OK" "Services started"
echo ""

# Step 3: Wait for services to be healthy
echo -e "${BLUE}Step 3: Waiting for services to be healthy...${NC}"

wait_for_health() {
    local url=$1
    local service=$2
    local max_attempts=30
    local attempt=0

    while [ $attempt -lt $max_attempts ]; do
        if curl -s -f "$url" > /dev/null 2>&1; then
            print_status "OK" "$service is healthy"
            return 0
        fi
        attempt=$((attempt + 1))
        sleep 1
    done

    print_status "FAIL" "$service did not become healthy after ${max_attempts}s"
    return 1
}

wait_for_health "$COLLECTOR_HEALTH" "OTEL Collector" || exit 1
wait_for_health "$JAEGER_UI" "Jaeger UI" || exit 1
wait_for_health "http://localhost:9090/-/healthy" "Prometheus" || exit 1

echo ""

# Step 4: Verify collector baseline
echo -e "${BLUE}Step 4: Getting collector baseline metrics...${NC}"

get_span_count() {
    curl -s "$COLLECTOR_METRICS" | \
        grep "^otelcol_receiver_accepted_spans" | \
        grep -v "#" | \
        awk '{print $2}' | \
        head -n 1 || echo "0"
}

BASELINE_SPANS=$(get_span_count)
print_status "INFO" "Baseline span count: $BASELINE_SPANS"
echo ""

# Step 5: Run marketplace command with OTEL
echo -e "${BLUE}Step 5: Running marketplace search with OTEL enabled...${NC}"

export OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:4318"
export OTEL_SERVICE_NAME="ggen-marketplace-p2p"
export RUST_LOG="info"

print_status "INFO" "Executing: cargo run --bin ggen --features otel -- marketplace search test"

if cargo run --bin ggen --features otel -- marketplace search test 2>&1 | tee /tmp/ggen-otel-test.log; then
    print_status "OK" "Command executed successfully"
else
    print_status "WARN" "Command failed (may be expected if P2P backend not ready)"
fi

echo ""

# Step 6: Wait for spans to be exported
echo -e "${BLUE}Step 6: Waiting for spans to be exported...${NC}"
sleep 3

CURRENT_SPANS=$(get_span_count)
NEW_SPANS=$((CURRENT_SPANS - BASELINE_SPANS))

print_status "INFO" "Current span count: $CURRENT_SPANS"
print_status "INFO" "New spans received: $NEW_SPANS"
echo ""

# Step 7: Validate results
echo -e "${BLUE}Step 7: Validating OTEL integration...${NC}"

VALIDATION_PASSED=true

if [ "$NEW_SPANS" -gt 0 ]; then
    print_status "OK" "Collector received $NEW_SPANS new spans"
else
    print_status "FAIL" "No spans received by collector"
    VALIDATION_PASSED=false
fi

# Check collector logs for trace activity
echo ""
print_status "INFO" "Checking collector logs for trace activity..."
if docker-compose -f "$COMPOSE_FILE" logs otel-collector 2>&1 | grep -q "Traces"; then
    print_status "OK" "Collector processed traces"
else
    print_status "WARN" "No trace activity in collector logs"
fi

# Check Jaeger for traces
echo ""
print_status "INFO" "Checking Jaeger for traces..."
JAEGER_TRACES=$(curl -s "http://localhost:16686/api/traces?service=ggen-marketplace-p2p&limit=1" | \
    jq -r '.data | length' 2>/dev/null || echo "0")

if [ "$JAEGER_TRACES" -gt 0 ]; then
    print_status "OK" "Found traces in Jaeger UI"
else
    print_status "WARN" "No traces in Jaeger yet (may take time to propagate)"
fi

echo ""

# Step 8: Run comprehensive test suite
echo -e "${BLUE}Step 8: Running comprehensive OTEL test suite...${NC}"

print_status "INFO" "Executing: cargo test --test otel_validation_tests --features otel -- --ignored"

if cargo test --test otel_validation_tests --features otel -- --ignored --nocapture 2>&1 | tee /tmp/ggen-otel-tests.log; then
    print_status "OK" "OTEL test suite passed"
else
    print_status "WARN" "Some OTEL tests failed (check logs)"
fi

echo ""

# Step 9: Generate validation report
echo -e "${BLUE}Step 9: Generating validation report...${NC}"

REPORT_FILE="$PROJECT_ROOT/tests/integration/otel_validation_report.txt"

cat > "$REPORT_FILE" <<EOF
OTEL Integration Validation Report
==================================
Generated: $(date)

Infrastructure Status:
- OTEL Collector: HEALTHY
- Jaeger UI: HEALTHY ($JAEGER_UI)
- Prometheus: HEALTHY (http://localhost:9090)

Test Results:
- Baseline spans: $BASELINE_SPANS
- Current spans: $CURRENT_SPANS
- New spans received: $NEW_SPANS
- Traces in Jaeger: $JAEGER_TRACES

Validation: $([ "$VALIDATION_PASSED" = true ] && echo "PASSED ✅" || echo "FAILED ❌")

Endpoints:
- OTLP HTTP: http://localhost:4318
- OTLP gRPC: http://localhost:4317
- Collector Health: $COLLECTOR_HEALTH
- Collector Metrics: $COLLECTOR_METRICS
- Jaeger UI: $JAEGER_UI
- Prometheus: http://localhost:9090

Command Logs: /tmp/ggen-otel-test.log
Test Logs: /tmp/ggen-otel-tests.log

Collector Logs:
$(docker-compose -f "$COMPOSE_FILE" logs --tail=50 otel-collector 2>&1)
EOF

print_status "OK" "Report saved to: $REPORT_FILE"
echo ""

# Step 10: Summary
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Validation Summary${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

if [ "$VALIDATION_PASSED" = true ]; then
    echo -e "${GREEN}✅ OTEL integration validated successfully!${NC}"
    echo ""
    echo -e "Spans emitted: ${GREEN}$NEW_SPANS${NC}"
    echo -e "Jaeger UI: ${BLUE}$JAEGER_UI${NC}"
    echo -e "Prometheus: ${BLUE}http://localhost:9090${NC}"
else
    echo -e "${RED}❌ OTEL validation failed${NC}"
    echo ""
    echo -e "${YELLOW}Troubleshooting:${NC}"
    echo "1. Check collector logs: docker-compose -f $COMPOSE_FILE logs otel-collector"
    echo "2. Verify OTEL features enabled: cargo run --bin ggen --features otel"
    echo "3. Check network connectivity: curl $COLLECTOR_HEALTH"
    echo "4. Review report: cat $REPORT_FILE"
fi

echo ""
echo -e "${BLUE}To view live traces:${NC}"
echo "  Open Jaeger UI: $JAEGER_UI"
echo ""
echo -e "${BLUE}To stop services:${NC}"
echo "  docker-compose -f $COMPOSE_FILE down -v"
echo ""

exit $([ "$VALIDATION_PASSED" = true ] && echo 0 || echo 1)
