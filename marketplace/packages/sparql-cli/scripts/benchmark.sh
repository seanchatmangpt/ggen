#!/bin/bash
set -euo pipefail

# SPARQL CLI Benchmark Script
# Performance testing for query execution, optimization, and federation

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=========================================="
echo "SPARQL CLI Benchmark Suite"
echo "=========================================="
echo ""

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

# Build release binary
info "Building optimized binary..."
cd "$PROJECT_ROOT"
cargo build --release

BINARY="$PROJECT_ROOT/target/release/sparql-cli"
[ -f "$BINARY" ] || { echo "Binary not found"; exit 1; }

# Run Criterion benchmarks
info "Running Criterion benchmarks..."
cargo bench --all-features

echo ""
echo "=========================================="
echo "Benchmark Complete"
echo "=========================================="
echo "Results saved in target/criterion/"
echo ""
