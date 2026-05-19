#!/usr/bin/env bash
#
# Marketplace Performance Benchmarks
# Run comprehensive performance tests for all marketplace operations
#

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Marketplace Performance Benchmarks${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Check if cargo is available
if ! command -v cargo &> /dev/null; then
    echo -e "${YELLOW}Error: cargo not found${NC}"
    exit 1
fi

# Run benchmarks with Criterion
echo -e "${GREEN}Running performance benchmarks...${NC}"
echo ""

cargo bench --bench marketplace_performance -- --save-baseline marketplace

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Benchmark Results${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "HTML reports generated in: target/criterion/"
echo ""
echo "Key benchmarks:"
echo "  - Registry Loading: target/criterion/registry_loading/"
echo "  - Search Performance: target/criterion/search_performance/"
echo "  - Installation: target/criterion/installation_performance/"
echo "  - Dependency Resolution: target/criterion/dependency_resolution/"
echo "  - Cache Performance: target/criterion/cache_performance/"
echo "  - Concurrent Operations: target/criterion/concurrent_operations/"
echo ""
echo -e "${YELLOW}To compare with previous runs:${NC}"
echo "  cargo bench --bench marketplace_performance -- --baseline marketplace"
echo ""
echo -e "${YELLOW}To view results:${NC}"
echo "  open target/criterion/report/index.html"
echo ""

# Performance targets validation
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Performance Targets${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "Target                              | Expected      | Status"
echo "----------------------------------- | ------------- | ------"
echo "Registry load (1000 packages)       | <50ms         | See report"
echo "Search (any query)                  | <100ms        | See report"
echo "Install (no deps)                   | <500ms        | See report"
echo "Dependency resolution (50 packages) | <200ms        | See report"
echo ""
