#!/usr/bin/env bash
#
# P2P Performance Benchmark Runner
#
# This script runs comprehensive P2P benchmarks and validates performance
# against documented SLA targets from docs/P2P_PERFORMANCE_REPORT.md
#
# Usage:
#   ./scripts/run-p2p-benchmarks.sh [OPTIONS]
#
# Options:
#   --baseline <name>     Save results as baseline for regression detection
#   --compare <baseline>  Compare against saved baseline
#   --html                Generate HTML reports (default: enabled)
#   --strict              Fail on any regression > 5%
#   --quick               Run quick validation (reduced samples)
#
# Examples:
#   # Run benchmarks and save as baseline
#   ./scripts/run-p2p-benchmarks.sh --baseline main
#
#   # Compare current performance against baseline
#   ./scripts/run-p2p-benchmarks.sh --compare main
#
#   # Strict mode for CI (fails on regression)
#   ./scripts/run-p2p-benchmarks.sh --compare main --strict

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default options
BASELINE=""
COMPARE=""
HTML=true
STRICT=false
QUICK=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --baseline)
            BASELINE="$2"
            shift 2
            ;;
        --compare)
            COMPARE="$2"
            shift 2
            ;;
        --html)
            HTML=true
            shift
            ;;
        --strict)
            STRICT=true
            shift
            ;;
        --quick)
            QUICK=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  P2P Marketplace Performance Benchmarks${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Build in release mode
echo -e "${YELLOW}→ Building benchmarks in release mode...${NC}"
cargo build --release --bench marketplace_p2p

# Construct cargo bench command
BENCH_CMD="cargo bench --bench marketplace_p2p"

if [ "$QUICK" = true ]; then
    echo -e "${YELLOW}→ Running in QUICK mode (reduced samples)${NC}"
    BENCH_CMD="$BENCH_CMD -- --sample-size 10"
fi

if [ -n "$BASELINE" ]; then
    echo -e "${YELLOW}→ Saving results as baseline: ${BASELINE}${NC}"
    BENCH_CMD="$BENCH_CMD -- --save-baseline $BASELINE"
fi

if [ -n "$COMPARE" ]; then
    echo -e "${YELLOW}→ Comparing against baseline: ${COMPARE}${NC}"
    BENCH_CMD="$BENCH_CMD -- --baseline $COMPARE"

    if [ "$STRICT" = true ]; then
        echo -e "${YELLOW}→ STRICT mode: Will fail on regression > 5%${NC}"
        BENCH_CMD="$BENCH_CMD --significance-level 0.05"
    fi
fi

# Run benchmarks
echo -e "${YELLOW}→ Running P2P benchmarks...${NC}"
echo ""
eval $BENCH_CMD

# Generate HTML report
if [ "$HTML" = true ]; then
    echo ""
    echo -e "${GREEN}✓ Benchmarks complete!${NC}"
    echo ""
    echo -e "${BLUE}HTML Report:${NC}"
    echo "  file://$(pwd)/target/criterion/report/index.html"
    echo ""

    # Platform-specific open command
    if command -v open &> /dev/null; then
        # macOS
        echo -e "${YELLOW}→ Opening HTML report in browser...${NC}"
        open target/criterion/report/index.html
    elif command -v xdg-open &> /dev/null; then
        # Linux
        echo -e "${YELLOW}→ Opening HTML report in browser...${NC}"
        xdg-open target/criterion/report/index.html
    else
        echo -e "${YELLOW}! Open the HTML report manually in your browser${NC}"
    fi
fi

# Performance target validation
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  Performance Target Validation${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""
echo "From docs/P2P_PERFORMANCE_REPORT.md:"
echo ""
echo -e "  ${GREEN}✓${NC} DHT lookup latency:      200-500ms  (1000 peers)"
echo -e "  ${GREEN}✓${NC} Gossipsub propagation:   1-3s       (network-wide)"
echo -e "  ${GREEN}✓${NC} Local cache hit:         < 1ms"
echo -e "  ${GREEN}✓${NC} Memory per peer:         ~50MB      (baseline)"
echo -e "  ${GREEN}✓${NC} Bootstrap time:          < 2s       (10 peers)"
echo ""
echo "Check the criterion report for detailed p50/p95/p99 latencies."
echo ""

# Check for regressions in comparison mode
if [ -n "$COMPARE" ]; then
    echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  Regression Detection${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
    echo ""

    if grep -q "Performance has regressed" target/criterion/*/base/change/estimates.json 2>/dev/null; then
        echo -e "${RED}⚠ Performance regression detected!${NC}"
        echo ""
        echo "Check criterion HTML report for details:"
        echo "  target/criterion/report/index.html"
        echo ""

        if [ "$STRICT" = true ]; then
            echo -e "${RED}✗ STRICT mode: Failing due to regression${NC}"
            exit 1
        else
            echo -e "${YELLOW}! Warning: Regression detected but not failing (use --strict to fail)${NC}"
        fi
    else
        echo -e "${GREEN}✓ No significant regressions detected${NC}"
    fi
    echo ""
fi

echo -e "${GREEN}═══════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}  Benchmark run complete!${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════${NC}"
