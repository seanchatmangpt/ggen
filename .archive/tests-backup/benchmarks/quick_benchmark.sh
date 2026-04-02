#!/bin/bash
# Quick Benchmark Runner - Fast performance check for marketplace commands

set -e

echo "╔══════════════════════════════════════════════════════════════════════╗"
echo "║  Quick Marketplace Performance Check                                 ║"
echo "╚══════════════════════════════════════════════════════════════════════╝"
echo ""

# Run quick benchmarks (reduced sample size for speed)
echo "🚀 Running quick performance benchmarks..."
echo ""

# Search performance (most critical)
echo "1️⃣  Testing Search Performance..."
cargo bench --bench marketplace_performance -- "marketplace_search" --sample-size 20 --warm-up-time 2 --measurement-time 5

# Maturity assessment
echo "2️⃣  Testing Maturity Assessment..."
cargo bench --bench marketplace_performance -- "maturity_assessment" --sample-size 20 --warm-up-time 2 --measurement-time 5

# Export performance
echo "3️⃣  Testing Export Performance..."
cargo bench --bench marketplace_performance -- "export_performance" --sample-size 20 --warm-up-time 2 --measurement-time 5

echo ""
echo "✅ Quick benchmark completed!"
echo ""
echo "📊 View detailed results:"
echo "   HTML Report: target/criterion/report/index.html"
echo ""
echo "💡 Run full benchmarks with: ./tests/benchmarks/run_benchmarks.sh"
echo ""
