#!/bin/bash
# Flamegraph generation wrapper with timeout
# Usage: ./flamegraph.sh [ggen-args]
# Generates interactive SVG flamegraph showing hotspots

set -euo pipefail

# SLO: Profiling should complete within 60s
TIMEOUT=60s

echo "🔥 Generating flamegraph (timeout: ${TIMEOUT})..."

# Check if cargo-flamegraph is installed
if ! command -v cargo-flamegraph &> /dev/null; then
    echo "❌ cargo-flamegraph not found. Install with:"
    echo "   cargo install flamegraph"
    exit 1
fi

# Run with timeout
if timeout "${TIMEOUT}" cargo flamegraph --bin ggen -- "$@"; then
    echo "✅ Flamegraph generated: flamegraph.svg"
    echo "📊 Open with: firefox flamegraph.svg"
    exit 0
else
    exit_code=$?
    if [ ${exit_code} -eq 124 ]; then
        echo "⏱️  Timeout exceeded (${TIMEOUT})"
    else
        echo "❌ Flamegraph generation failed (exit: ${exit_code})"
    fi
    exit ${exit_code}
fi
