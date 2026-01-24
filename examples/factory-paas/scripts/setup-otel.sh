#!/usr/bin/env bash
# Setup OpenTelemetry Integration for FactoryPaaS
#
# This script sets up the complete OTEL stack:
# - Local OTLP collector (Docker)
# - Jaeger for trace visualization
# - Prometheus for metrics
# - Environment configuration

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if Docker is installed
check_docker() {
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed. Please install Docker first."
        exit 1
    fi
    log_info "Docker found: $(docker --version)"
}

# Start OTLP collector
start_otlp_collector() {
    log_info "Starting OTLP collector..."

    if docker ps | grep -q otel-collector; then
        log_warn "OTLP collector already running. Stopping existing container..."
        docker stop otel-collector
        docker rm otel-collector
    fi

    docker run -d \
        --name otel-collector \
        -p 4317:4317 \
        -p 4318:4318 \
        -p 55679:55679 \
        otel/opentelemetry-collector:latest

    log_info "OTLP collector started on ports 4317 (gRPC) and 4318 (HTTP)"
}

# Start Jaeger for trace visualization
start_jaeger() {
    log_info "Starting Jaeger..."

    if docker ps | grep -q jaeger; then
        log_warn "Jaeger already running. Stopping existing container..."
        docker stop jaeger
        docker rm jaeger
    fi

    docker run -d \
        --name jaeger \
        -e COLLECTOR_OTLP_ENABLED=true \
        -p 16686:16686 \
        -p 4317:4317 \
        -p 4318:4318 \
        jaegertracing/all-in-one:latest

    log_info "Jaeger UI available at http://localhost:16686"
}

# Start Prometheus for metrics
start_prometheus() {
    log_info "Starting Prometheus..."

    # Create Prometheus config
    cat > /tmp/prometheus.yml <<EOF
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'otel-collector'
    static_configs:
      - targets: ['host.docker.internal:55679']
EOF

    if docker ps | grep -q prometheus; then
        log_warn "Prometheus already running. Stopping existing container..."
        docker stop prometheus
        docker rm prometheus
    fi

    docker run -d \
        --name prometheus \
        -p 9090:9090 \
        -v /tmp/prometheus.yml:/etc/prometheus/prometheus.yml \
        prom/prometheus:latest

    log_info "Prometheus UI available at http://localhost:9090"
}

# Configure environment variables
configure_env() {
    log_info "Configuring environment variables..."

    cat > .env.otel <<EOF
# OpenTelemetry Configuration
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317
OTEL_SAMPLING_RATIO=1.0
ENVIRONMENT=dev

# GCP Configuration (comment out for local development)
# GCP_PROJECT_ID=your-project-id
# GOOGLE_APPLICATION_CREDENTIALS=/path/to/service-account-key.json

# Service Configuration
SERVICE_NAME=factory-paas-attribution
SERVICE_VERSION=1.0.0
EOF

    log_info "Created .env.otel file. Source it with: source .env.otel"
}

# Generate world modules with OTEL templates
generate_world() {
    log_info "Generating world modules with OTEL instrumentation..."

    if ! command -v ggen &> /dev/null; then
        log_error "ggen is not installed. Please build it first."
        exit 1
    fi

    cd "$(dirname "$0")/.."
    ggen sync

    log_info "World modules generated with OTEL instrumentation"
}

# Run OTEL integration tests
run_tests() {
    log_info "Running OTEL integration tests..."

    cd world
    cargo test --test otel_integration_tests -- --nocapture

    log_info "OTEL integration tests passed"
}

# Health check
health_check() {
    log_info "Running health checks..."

    # Check OTLP collector
    if curl -s http://localhost:55679/metrics > /dev/null; then
        log_info "✓ OTLP collector is healthy"
    else
        log_warn "✗ OTLP collector is not responding"
    fi

    # Check Jaeger
    if curl -s http://localhost:16686 > /dev/null; then
        log_info "✓ Jaeger UI is healthy"
    else
        log_warn "✗ Jaeger UI is not responding"
    fi

    # Check Prometheus
    if curl -s http://localhost:9090 > /dev/null; then
        log_info "✓ Prometheus is healthy"
    else
        log_warn "✗ Prometheus is not responding"
    fi
}

# Stop all services
stop_all() {
    log_info "Stopping all OTEL services..."

    docker stop otel-collector jaeger prometheus 2>/dev/null || true
    docker rm otel-collector jaeger prometheus 2>/dev/null || true

    log_info "All OTEL services stopped"
}

# Main menu
main() {
    echo ""
    echo "================================================"
    echo "  OpenTelemetry Setup for FactoryPaaS"
    echo "================================================"
    echo ""

    case "${1:-all}" in
        all)
            check_docker
            start_jaeger
            start_prometheus
            configure_env
            health_check
            ;;
        collector)
            check_docker
            start_otlp_collector
            ;;
        jaeger)
            check_docker
            start_jaeger
            ;;
        prometheus)
            check_docker
            start_prometheus
            ;;
        env)
            configure_env
            ;;
        generate)
            generate_world
            ;;
        test)
            run_tests
            ;;
        health)
            health_check
            ;;
        stop)
            stop_all
            ;;
        *)
            echo "Usage: $0 {all|collector|jaeger|prometheus|env|generate|test|health|stop}"
            echo ""
            echo "Commands:"
            echo "  all        - Start all services (Jaeger, Prometheus, configure env)"
            echo "  collector  - Start OTLP collector only"
            echo "  jaeger     - Start Jaeger only"
            echo "  prometheus - Start Prometheus only"
            echo "  env        - Configure environment variables"
            echo "  generate   - Generate world modules with OTEL"
            echo "  test       - Run OTEL integration tests"
            echo "  health     - Check service health"
            echo "  stop       - Stop all services"
            exit 1
            ;;
    esac

    echo ""
    log_info "Setup complete!"
    echo ""
    echo "Next steps:"
    echo "  1. Source environment: source .env.otel"
    echo "  2. Generate world:     ./scripts/setup-otel.sh generate"
    echo "  3. Run tests:          ./scripts/setup-otel.sh test"
    echo "  4. Start kernel:       cd kernel && cargo run"
    echo ""
    echo "UIs:"
    echo "  - Jaeger:     http://localhost:16686"
    echo "  - Prometheus: http://localhost:9090"
    echo ""
}

main "$@"
