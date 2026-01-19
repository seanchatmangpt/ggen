#!/usr/bin/env bash
# tests/integration/docker_test.sh
# Docker-based integration tests for C hot path
# Uses Docker Compose to orchestrate test containers

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "=========================================="
echo "KNHK Docker Integration Tests"
echo "=========================================="

# Check Docker is available
if ! command -v docker &> /dev/null; then
    echo "Error: docker not found. Please install Docker."
    exit 1
fi

if ! command -v docker-compose &> /dev/null && ! command -v docker compose &> /dev/null; then
    echo "Error: docker-compose not found. Please install Docker Compose."
    exit 1
fi

# Use docker compose if available, otherwise docker-compose
DOCKER_COMPOSE_CMD="docker-compose"
if command -v docker &> /dev/null && docker compose version &> /dev/null 2>&1; then
    DOCKER_COMPOSE_CMD="docker compose"
fi

# Build test containers
echo "Building test containers..."
cd "$SCRIPT_DIR"
$DOCKER_COMPOSE_CMD build

# Start test infrastructure
echo "Starting test infrastructure..."
$DOCKER_COMPOSE_CMD up -d

# Wait for services to be ready
echo "Waiting for services to be ready..."
sleep 10

# Run integration tests
echo "Running integration tests..."
cd "$PROJECT_ROOT"

# Test 1: Kafka connector integration
echo "[TEST] Kafka Connector Integration"
export KAFKA_BOOTSTRAP_SERVERS="localhost:9092"
if ./tests/integration/test_kafka_integration; then
    echo "  ✓ Kafka connector test passed"
else
    echo "  ✗ Kafka connector test failed"
    cd "$SCRIPT_DIR"
    $DOCKER_COMPOSE_CMD down
    exit 1
fi

# Test 2: Lockchain integration
echo "[TEST] Lockchain Integration"
if ./tests/integration/test_lockchain_integration; then
    echo "  ✓ Lockchain test passed"
else
    echo "  ✗ Lockchain test failed"
    cd "$SCRIPT_DIR"
    $DOCKER_COMPOSE_CMD down
    exit 1
fi

# Test 3: ETL pipeline integration
echo "[TEST] ETL Pipeline Integration"
if ./tests/integration/test_etl_integration; then
    echo "  ✓ ETL pipeline test passed"
else
    echo "  ✗ ETL pipeline test failed"
    cd "$SCRIPT_DIR"
    $DOCKER_COMPOSE_CMD down
    exit 1
fi

# Cleanup
echo "Cleaning up..."
cd "$SCRIPT_DIR"
$DOCKER_COMPOSE_CMD down

echo "=========================================="
echo "All integration tests passed!"
echo "=========================================="

