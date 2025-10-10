#!/bin/bash

# Adaptive Swarm Continuous Monitoring Script
# This script runs real-time monitoring and optimization

set -e

echo "🔍 Starting Adaptive Swarm Monitoring..."
echo "================================================"

SWARM_ID="${1:-swarm_1760081081210_fvfbrrjoj}"
INTERVAL="${2:-60}"  # Default 60 seconds
LOG_FILE="/Users/sac/ggen/ggen-mcp/swarm/metrics/monitoring.log"

# Create log directory if it doesn't exist
mkdir -p "$(dirname "$LOG_FILE")"

# Function to collect metrics
collect_metrics() {
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    echo "[$timestamp] Collecting swarm metrics..."

    # Get swarm status
    npx claude-flow@alpha mcp swarm_status --swarm-id "$SWARM_ID" >> "$LOG_FILE" 2>&1

    # Get agent metrics
    npx claude-flow@alpha mcp agent_metrics >> "$LOG_FILE" 2>&1

    # Get performance report
    npx claude-flow@alpha mcp performance_report --format json --timeframe 1h >> "$LOG_FILE" 2>&1

    echo "[$timestamp] Metrics collected successfully"
}

# Function to detect bottlenecks
detect_bottlenecks() {
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    echo "[$timestamp] Analyzing bottlenecks..."

    npx claude-flow@alpha mcp bottleneck_analyze \
        --component coordination \
        --metrics '["latency", "throughput", "success_rate"]' \
        >> "$LOG_FILE" 2>&1

    echo "[$timestamp] Bottleneck analysis complete"
}

# Function to optimize topology
optimize_topology() {
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    echo "[$timestamp] Optimizing swarm topology..."

    npx claude-flow@alpha mcp topology_optimize \
        --swarm-id "$SWARM_ID" \
        >> "$LOG_FILE" 2>&1

    echo "[$timestamp] Topology optimization complete"
}

# Function to train neural patterns
train_patterns() {
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    echo "[$timestamp] Training neural patterns..."

    npx claude-flow@alpha mcp neural_train \
        coordination \
        --training-data swarm_performance_history \
        --epochs 10 \
        >> "$LOG_FILE" 2>&1

    echo "[$timestamp] Neural training complete"
}

# Function to check and auto-scale
auto_scale() {
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    echo "[$timestamp] Checking auto-scale conditions..."

    # This would analyze metrics and trigger scaling if needed
    # For now, log the intent
    echo "[$timestamp] Auto-scale check: Agent utilization monitoring active"
}

# Main monitoring loop
monitor_loop() {
    local iteration=0

    while true; do
        iteration=$((iteration + 1))

        echo ""
        echo "=========================================="
        echo "Monitoring Iteration #$iteration"
        echo "Time: $(date)"
        echo "=========================================="

        # Collect metrics every iteration
        collect_metrics

        # Detect bottlenecks every 5 iterations (5 minutes if 60s interval)
        if [ $((iteration % 5)) -eq 0 ]; then
            detect_bottlenecks
        fi

        # Optimize topology every 15 iterations (15 minutes if 60s interval)
        if [ $((iteration % 15)) -eq 0 ]; then
            optimize_topology
        fi

        # Train patterns every 30 iterations (30 minutes if 60s interval)
        if [ $((iteration % 30)) -eq 0 ]; then
            train_patterns
        fi

        # Check auto-scale every iteration
        auto_scale

        echo "Next check in $INTERVAL seconds..."
        sleep "$INTERVAL"
    done
}

# Trap SIGINT and SIGTERM for graceful shutdown
trap 'echo "Monitoring stopped by user"; exit 0' SIGINT SIGTERM

# Display configuration
echo "Configuration:"
echo "  Swarm ID: $SWARM_ID"
echo "  Interval: ${INTERVAL}s"
echo "  Log File: $LOG_FILE"
echo ""

# Start monitoring
monitor_loop
