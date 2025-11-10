#!/bin/bash
set -euo pipefail

# Workflow Engine CLI Benchmark Script
# Performance benchmarks for workflow execution, task processing, and event handling

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

log_info() {
    echo -e "${CYAN}[INFO]${NC} $1"
}

log_result() {
    echo -e "${GREEN}[RESULT]${NC} $1"
}

run_criterion_benchmarks() {
    log_info "Running Criterion benchmarks..."

    cd "$PROJECT_ROOT"
    cargo bench --bench workflow_benchmarks
}

benchmark_workflow_validation() {
    log_info "Benchmarking workflow validation..."

    # Create test workflow
    cat > /tmp/test_workflow.bpmn <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL">
  <bpmn:process id="test-process" isExecutable="true">
    <bpmn:startEvent id="start"/>
    <bpmn:userTask id="task1"/>
    <bpmn:endEvent id="end"/>
  </bpmn:process>
</bpmn:definitions>
EOF

    START=$(date +%s%N)
    for i in {1..100}; do
        workflow-engine workflow validate --file /tmp/test_workflow.bpmn > /dev/null 2>&1 || true
    done
    END=$(date +%s%N)

    DURATION=$(( (END - START) / 1000000 ))
    RATE=$(( 100000 / DURATION ))

    log_result "Workflow validation: ~$RATE validations/sec"
}

benchmark_process_start() {
    log_info "Benchmarking process start..."

    # Assumes workflow-engine is running
    START=$(date +%s%N)
    for i in {1..1000}; do
        workflow-engine process start \
            --workflow-id test-workflow \
            --variables '{"test": true}' > /dev/null 2>&1 || true
    done
    END=$(date +%s%N)

    DURATION=$(( (END - START) / 1000000 ))
    RATE=$(( 1000000 / DURATION ))

    log_result "Process start: ~$RATE instances/sec"
}

benchmark_task_execution() {
    log_info "Benchmarking task execution..."

    START=$(date +%s%N)
    for i in {1..5000}; do
        workflow-engine task complete \
            --task-id test-task-$i \
            --variables '{"result": "success"}' > /dev/null 2>&1 || true
    done
    END=$(date +%s%N)

    DURATION=$(( (END - START) / 1000000 ))
    RATE=$(( 5000000 / DURATION ))

    log_result "Task execution: ~$RATE tasks/sec"
}

benchmark_event_processing() {
    log_info "Benchmarking event processing..."

    START=$(date +%s%N)
    for i in {1..10000}; do
        workflow-engine event send \
            --event-name test-event \
            --payload '{"data": "test"}' > /dev/null 2>&1 || true
    done
    END=$(date +%s%N)

    DURATION=$(( (END - START) / 1000000 ))
    RATE=$(( 10000000 / DURATION ))

    log_result "Event processing: ~$RATE events/sec"
}

benchmark_memory_usage() {
    log_info "Benchmarking memory usage..."

    # Start workflow engine in background
    workflow-engine daemon start > /dev/null 2>&1 &
    ENGINE_PID=$!

    sleep 2

    # Get initial memory
    INITIAL_MEM=$(ps -o rss= -p $ENGINE_PID)

    # Run workload
    for i in {1..100}; do
        workflow-engine process start --workflow-id test-workflow > /dev/null 2>&1 || true
    done

    # Get peak memory
    PEAK_MEM=$(ps -o rss= -p $ENGINE_PID)

    kill $ENGINE_PID 2> /dev/null || true

    log_result "Memory usage: Initial ${INITIAL_MEM}KB, Peak ${PEAK_MEM}KB"
}

main() {
    echo "========================================="
    echo "Workflow Engine Performance Benchmarks"
    echo "========================================="
    echo ""

    run_criterion_benchmarks

    echo ""
    echo "CLI Benchmarks:"
    echo "---------------"

    benchmark_workflow_validation
    benchmark_process_start
    benchmark_task_execution
    benchmark_event_processing
    benchmark_memory_usage

    echo ""
    echo "========================================="
    echo "Benchmark complete!"
    echo "========================================="
}

main "$@"
