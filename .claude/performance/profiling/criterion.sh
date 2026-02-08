#!/bin/bash
# Criterion benchmark runner with timeout and SLO validation
# Usage: ./criterion.sh [--baseline NAME] [BENCH_FILTER]

set -euo pipefail

# SLO: Benchmarks should complete within 5 minutes
TIMEOUT=300s
BASELINE=""
FILTER=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --baseline)
            BASELINE="$2"
            shift 2
            ;;
        *)
            FILTER="$1"
            shift
            ;;
    esac
done

echo "🧪 Running Criterion benchmarks (timeout: ${TIMEOUT})..."

# Build benchmark command
BENCH_CMD="cargo bench"
if [ -n "${FILTER}" ]; then
    BENCH_CMD="${BENCH_CMD} ${FILTER}"
fi
if [ -n "${BASELINE}" ]; then
    BENCH_CMD="${BENCH_CMD} -- --save-baseline ${BASELINE}"
fi

# Run with timeout
if timeout "${TIMEOUT}" ${BENCH_CMD}; then
    echo "✅ Benchmarks completed successfully"
    echo "📊 Results: target/criterion/report/index.html"

    # Check SLOs
    echo ""
    echo "🎯 Validating Performance SLOs..."
    cargo make slo-check || echo "⚠️  Some SLOs not met (non-blocking)"

    exit 0
else
    exit_code=$?
    if [ ${exit_code} -eq 124 ]; then
        echo "⏱️  Timeout exceeded (${TIMEOUT})"
        echo "💡 Try filtering benchmarks: ./criterion.sh BENCH_NAME"
    else
        echo "❌ Benchmarks failed (exit: ${exit_code})"
    fi
    exit ${exit_code}
fi
