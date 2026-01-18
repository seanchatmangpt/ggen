#!/bin/bash
# Comprehensive Benchmark Runner for Marketplace Commands
# Runs all benchmarks and generates analysis reports

set -e

echo "╔══════════════════════════════════════════════════════════════════════╗"
echo "║  GGEN Marketplace Performance Benchmark Suite                       ║"
echo "╚══════════════════════════════════════════════════════════════════════╝"
echo ""

# Configuration
BENCHMARK_DIR="tests/benchmarks"
RESULTS_DIR="$BENCHMARK_DIR/results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_FILE="$RESULTS_DIR/benchmark_results_$TIMESTAMP.json"
REPORT_FILE="$RESULTS_DIR/performance_report_$TIMESTAMP.md"

# Create results directory
mkdir -p "$RESULTS_DIR"

echo "📋 Configuration"
echo "  Results Directory: $RESULTS_DIR"
echo "  Timestamp: $TIMESTAMP"
echo ""

# Function to run benchmark group
run_benchmark_group() {
    local group_name=$1
    local benchmark_args=$2

    echo "🔧 Running benchmark group: $group_name"
    cargo bench --bench marketplace_performance -- "$benchmark_args" \
        --save-baseline "$group_name" \
        --output-format bencher
    echo "  ✅ Completed: $group_name"
    echo ""
}

# Phase 1: Search Performance Benchmarks
echo "═══════════════════════════════════════════════════════════════════════"
echo "Phase 1: Search Performance Benchmarks"
echo "═══════════════════════════════════════════════════════════════════════"
echo ""

run_benchmark_group "search_10" "marketplace_search/text_search/10"
run_benchmark_group "search_100" "marketplace_search/text_search/100"
run_benchmark_group "search_1000" "marketplace_search/text_search/1000"
run_benchmark_group "search_category" "marketplace_search/category_search"
run_benchmark_group "search_tags" "marketplace_search/tag_search"
run_benchmark_group "search_complex" "marketplace_search/complex_search"

# Phase 2: Maturity Assessment Benchmarks
echo "═══════════════════════════════════════════════════════════════════════"
echo "Phase 2: Maturity Assessment Benchmarks"
echo "═══════════════════════════════════════════════════════════════════════"
echo ""

run_benchmark_group "maturity_single" "maturity_assessment/single_package"
run_benchmark_group "maturity_batch_10" "maturity_assessment/batch_assessment/10"
run_benchmark_group "maturity_batch_100" "maturity_assessment/batch_assessment/100"
run_benchmark_group "maturity_parallel_10" "maturity_assessment/parallel_assessment/10"
run_benchmark_group "maturity_parallel_100" "maturity_assessment/parallel_assessment/100"

# Phase 3: Export Performance Benchmarks
echo "═══════════════════════════════════════════════════════════════════════"
echo "Phase 3: Export Performance Benchmarks"
echo "═══════════════════════════════════════════════════════════════════════"
echo ""

run_benchmark_group "export_csv_10" "export_performance/csv_export/10"
run_benchmark_group "export_csv_100" "export_performance/csv_export/100"
run_benchmark_group "export_json_10" "export_performance/json_export/10"
run_benchmark_group "export_json_100" "export_performance/json_export/100"
run_benchmark_group "export_html_10" "export_performance/html_export/10"
run_benchmark_group "export_html_100" "export_performance/html_export/100"
run_benchmark_group "export_md_10" "export_performance/markdown_export/10"
run_benchmark_group "export_md_100" "export_performance/markdown_export/100"

# Phase 4: Comparison Benchmarks
echo "═══════════════════════════════════════════════════════════════════════"
echo "Phase 4: Comparison Benchmarks"
echo "═══════════════════════════════════════════════════════════════════════"
echo ""

run_benchmark_group "comparison_two" "comparison/two_package_comparison"
run_benchmark_group "comparison_sequential" "comparison/sequential_comparisons"
run_benchmark_group "comparison_detailed" "comparison/detailed_comparison"

# Phase 5: Recommendation Engine Benchmarks
echo "═══════════════════════════════════════════════════════════════════════"
echo "Phase 5: Recommendation Engine Benchmarks"
echo "═══════════════════════════════════════════════════════════════════════"
echo ""

run_benchmark_group "recommend_basic_10" "recommendation_engine/basic_recommendations/10"
run_benchmark_group "recommend_basic_100" "recommendation_engine/basic_recommendations/100"
run_benchmark_group "recommend_ml_10" "recommendation_engine/ml_recommendations/10"
run_benchmark_group "recommend_ml_100" "recommendation_engine/ml_recommendations/100"
run_benchmark_group "recommend_rank_10" "recommendation_engine/ranking_accuracy/10"
run_benchmark_group "recommend_rank_100" "recommendation_engine/ranking_accuracy/100"

# Phase 6: Memory Usage Benchmarks
echo "═══════════════════════════════════════════════════════════════════════"
echo "Phase 6: Memory Usage Benchmarks"
echo "═══════════════════════════════════════════════════════════════════════"
echo ""

run_benchmark_group "memory_load_100" "memory_usage/package_loading/100"
run_benchmark_group "memory_load_1000" "memory_usage/package_loading/1000"
run_benchmark_group "memory_search_100" "memory_usage/search_and_filter/100"
run_benchmark_group "memory_search_1000" "memory_usage/search_and_filter/1000"

# Phase 7: End-to-End Workflow Benchmarks
echo "═══════════════════════════════════════════════════════════════════════"
echo "Phase 7: End-to-End Workflow Benchmarks"
echo "═══════════════════════════════════════════════════════════════════════"
echo ""

run_benchmark_group "e2e_search_assess_export" "e2e_workflows/search_assess_export"
run_benchmark_group "e2e_recommend_compare_export" "e2e_workflows/recommend_compare_export"

# Generate comparison report
echo "═══════════════════════════════════════════════════════════════════════"
echo "Generating Performance Analysis Report"
echo "═══════════════════════════════════════════════════════════════════════"
echo ""

# Compare with baseline (if exists)
if [ -d "target/criterion" ]; then
    echo "📊 Comparing with baseline..."
    cargo bench --bench marketplace_performance -- --save-baseline current

    # Generate criterion HTML report
    echo "  📈 Criterion HTML report: target/criterion/report/index.html"
fi

# Create summary table
echo "📋 Creating performance summary table..."

cat > "$REPORT_FILE" << 'EOF'
# Marketplace Performance Benchmark Results

## Executive Summary

This report presents comprehensive performance benchmarks for all marketplace commands.

### Performance Goals

- **Interactive Commands** (search, compare): < 500ms
- **Report Generation** (export, maturity assessment): < 5s
- **Memory Usage**: < 100MB
- **Success Rate**: > 99%

## Benchmark Results

### 1. Search Performance

#### Dataset Size: 10 Packages
| Operation | Mean (ms) | P95 (ms) | P99 (ms) | Throughput (ops/s) | Status |
|-----------|-----------|----------|----------|-------------------|--------|
| Text Search | TBD | TBD | TBD | TBD | ⏳ |
| Category Filter | TBD | TBD | TBD | TBD | ⏳ |
| Tag Search | TBD | TBD | TBD | TBD | ⏳ |
| Complex Search | TBD | TBD | TBD | TBD | ⏳ |

#### Dataset Size: 100 Packages
| Operation | Mean (ms) | P95 (ms) | P99 (ms) | Throughput (ops/s) | Status |
|-----------|-----------|----------|----------|-------------------|--------|
| Text Search | TBD | TBD | TBD | TBD | ⏳ |
| Category Filter | TBD | TBD | TBD | TBD | ⏳ |
| Tag Search | TBD | TBD | TBD | TBD | ⏳ |
| Complex Search | TBD | TBD | TBD | TBD | ⏳ |

#### Dataset Size: 1000 Packages
| Operation | Mean (ms) | P95 (ms) | P99 (ms) | Throughput (ops/s) | Status |
|-----------|-----------|----------|----------|-------------------|--------|
| Text Search | TBD | TBD | TBD | TBD | ⏳ |
| Category Filter | TBD | TBD | TBD | TBD | ⏳ |
| Tag Search | TBD | TBD | TBD | TBD | ⏳ |
| Complex Search | TBD | TBD | TBD | TBD | ⏳ |

### 2. Maturity Assessment Performance

| Operation | Dataset Size | Mean (ms) | Time/Package (ms) | Memory (MB) | Status |
|-----------|--------------|-----------|-------------------|-------------|--------|
| Single Package | 1 | TBD | TBD | TBD | ⏳ |
| Batch Assessment | 10 | TBD | TBD | TBD | ⏳ |
| Batch Assessment | 100 | TBD | TBD | TBD | ⏳ |
| Parallel Assessment | 10 | TBD | TBD | TBD | ⏳ |
| Parallel Assessment | 100 | TBD | TBD | TBD | ⏳ |

### 3. Export Performance

| Format | Dataset Size | Generation Time (ms) | File Size (KB) | Memory (MB) | Status |
|--------|--------------|----------------------|----------------|-------------|--------|
| CSV | 10 | TBD | TBD | TBD | ⏳ |
| CSV | 100 | TBD | TBD | TBD | ⏳ |
| JSON | 10 | TBD | TBD | TBD | ⏳ |
| JSON | 100 | TBD | TBD | TBD | ⏳ |
| HTML | 10 | TBD | TBD | TBD | ⏳ |
| HTML | 100 | TBD | TBD | TBD | ⏳ |
| Markdown | 10 | TBD | TBD | TBD | ⏳ |
| Markdown | 100 | TBD | TBD | TBD | ⏳ |

### 4. Comparison Performance

| Operation | Mean (ms) | P95 (ms) | Memory (MB) | Status |
|-----------|-----------|----------|-------------|--------|
| Two Package Comparison | TBD | TBD | TBD | ⏳ |
| Sequential Comparisons (10x) | TBD | TBD | TBD | ⏳ |
| Detailed Comparison | TBD | TBD | TBD | ⏳ |

### 5. Recommendation Engine Performance

| Operation | Dataset Size | Mean (ms) | Ranking Accuracy | Memory (MB) | Status |
|-----------|--------------|-----------|------------------|-------------|--------|
| Basic Recommendations | 10 | TBD | TBD | TBD | ⏳ |
| Basic Recommendations | 100 | TBD | TBD | TBD | ⏳ |
| ML Recommendations | 10 | TBD | TBD | TBD | ⏳ |
| ML Recommendations | 100 | TBD | TBD | TBD | ⏳ |

### 6. Memory Usage Analysis

| Operation | Dataset Size | Peak Memory (MB) | Avg Memory (MB) | Status |
|-----------|--------------|------------------|-----------------|--------|
| Package Loading | 100 | TBD | TBD | ⏳ |
| Package Loading | 1000 | TBD | TBD | ⏳ |
| Search & Filter | 100 | TBD | TBD | ⏳ |
| Search & Filter | 1000 | TBD | TBD | ⏳ |

### 7. End-to-End Workflows

| Workflow | Mean (ms) | P95 (ms) | Memory (MB) | Status |
|----------|-----------|----------|-------------|--------|
| Search → Assess → Export | TBD | TBD | TBD | ⏳ |
| Recommend → Compare → Export | TBD | TBD | TBD | ⏳ |

## Performance Trends

### Scaling Analysis

TBD: Complexity analysis (O(n), O(log n), etc.) for each operation

### Optimization Recommendations

TBD: Specific recommendations based on benchmark results

## Conclusion

TBD: Overall assessment and next steps

---

**Benchmark Date**: $(date)
**Environment**: $(uname -a)
**Rust Version**: $(rustc --version)
**CPU**: $(sysctl -n machdep.cpu.brand_string 2>/dev/null || echo "N/A")
**Memory**: $(sysctl -n hw.memsize 2>/dev/null | awk '{print $1/1024/1024/1024 " GB"}' || echo "N/A")
EOF

echo "  ✅ Report template created: $REPORT_FILE"
echo ""

# Final summary
echo "╔══════════════════════════════════════════════════════════════════════╗"
echo "║  Benchmark Suite Completed Successfully                              ║"
echo "╚══════════════════════════════════════════════════════════════════════╝"
echo ""
echo "📊 Results saved to: $RESULTS_DIR"
echo "📈 HTML Report: target/criterion/report/index.html"
echo "📋 Markdown Report: $REPORT_FILE"
echo ""
echo "Next Steps:"
echo "  1. Review the HTML report for detailed visualizations"
echo "  2. Check $REPORT_FILE for summary and recommendations"
echo "  3. Address any performance bottlenecks identified"
echo "  4. Run 'cargo bench --bench marketplace_performance' again to track improvements"
echo ""
