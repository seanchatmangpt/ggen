#!/bin/bash
set -euo pipefail

# Data Pipeline CLI - Benchmark Script
# Performance benchmarks for ETL operations

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo "=========================================="
echo "Data Pipeline CLI - Benchmarks"
echo "=========================================="
echo

# Run Rust benchmarks
run_rust_benchmarks() {
    echo "Running Rust benchmarks..."
    cd "$PROJECT_DIR"

    cargo bench --all-features
}

# Run integration benchmarks
run_integration_benchmarks() {
    echo
    echo "Running integration benchmarks..."

    # Build release binary
    cargo build --release --all-features

    local binary="$PROJECT_DIR/target/release/data-pipeline"

    # CSV ingestion benchmark
    echo
    echo "Benchmark 1: CSV Ingestion (100k rows)"
    time "$binary" pipeline run --name csv-benchmark --dry-run

    # Transformation benchmark
    echo
    echo "Benchmark 2: Data Transformation (mapping + filtering)"
    time "$binary" pipeline run --name transform-benchmark --dry-run

    # RDF write benchmark
    echo
    echo "Benchmark 3: RDF Triple Writing"
    time "$binary" pipeline run --name rdf-benchmark --dry-run
}

# Main
main() {
    run_rust_benchmarks
    run_integration_benchmarks

    echo
    echo "Benchmarks complete!"
}

main
