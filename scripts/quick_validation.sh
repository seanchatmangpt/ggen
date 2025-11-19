#!/bin/bash
# Quick Performance Validation Script
# Runs fast validation checks for quick wins

set -e

echo "🚀 Quick Performance Validation (Week 3)"
echo "═══════════════════════════════════════"
echo ""

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

FAILED=0

# 1. Check Lazy RDF Loading implementation
echo "📊 Checking Quick Win 1: Lazy RDF Loading..."
if grep -q "!template.front.rdf_inline.is_empty()" crates/ggen-core/src/parallel_generator.rs; then
    echo -e "${GREEN}✅ Lazy RDF loading implementation found${NC}"
else
    echo -e "${RED}❌ Lazy RDF loading not found${NC}"
    FAILED=1
fi

# 2. Check Parallel Generation implementation
echo "📊 Checking Quick Win 2: Parallel Template Generation..."
if grep -q "par_iter" crates/ggen-core/src/parallel_generator.rs; then
    echo -e "${GREEN}✅ Parallel generation (Rayon) implementation found${NC}"
else
    echo -e "${RED}❌ Parallel generation not found${NC}"
    FAILED=1
fi

# 3. Check Cache Improvements
echo "📊 Checking Quick Win 3: Cache Improvements..."
if grep -q "Self::new(5000)" crates/ggen-core/src/template_cache.rs; then
    echo -e "${GREEN}✅ Cache capacity increased to 5000${NC}"
else
    echo -e "${RED}❌ Cache improvements not found${NC}"
    FAILED=1
fi

if grep -q "pub hits: u64" crates/ggen-core/src/template_cache.rs; then
    echo -e "${GREEN}✅ Cache hit/miss tracking implemented${NC}"
else
    echo -e "${RED}❌ Cache tracking not found${NC}"
    FAILED=1
fi

# 4. Check Benchmark Files
echo ""
echo "📊 Checking Benchmark Files..."
if [ -f "crates/ggen-core/benches/quick_wins_benchmark.rs" ]; then
    echo -e "${GREEN}✅ Quick wins benchmark file exists${NC}"
else
    echo -e "${RED}❌ Quick wins benchmark missing${NC}"
    FAILED=1
fi

if [ -f "crates/ggen-core/benches/medium_optimizations_benchmark.rs" ]; then
    echo -e "${GREEN}✅ Medium optimizations benchmark file exists${NC}"
else
    echo -e "${RED}❌ Medium optimizations benchmark missing${NC}"
    FAILED=1
fi

# 5. Run Quick Compilation Test
echo ""
echo "📊 Running Quick Compilation Test..."
if cargo check -p ggen-core --benches 2>&1 | grep -q "Finished"; then
    echo -e "${GREEN}✅ All benchmarks compile successfully${NC}"
else
    echo -e "${YELLOW}⚠️  Benchmark compilation issues (run 'cargo check -p ggen-core --benches' for details)${NC}"
    FAILED=1
fi

# Summary
echo ""
echo "═══════════════════════════════════════"
if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✅ Quick Validation PASSED${NC}"
    echo ""
    echo "Next steps:"
    echo "  1. Run full benchmarks: cargo bench -p ggen-core --bench quick_wins_benchmark"
    echo "  2. Generate dashboard: ./scripts/performance_dashboard.sh"
    echo "  3. View report: cat docs/PERFORMANCE_VALIDATION_REPORT.md"
    exit 0
else
    echo -e "${RED}❌ Quick Validation FAILED${NC}"
    echo ""
    echo "Some checks failed. Please review the output above."
    exit 1
fi
