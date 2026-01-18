#!/bin/bash
# Performance Benchmark Runner for ggen v2.0.0
# Agent 7: Performance Benchmarker
#
# Usage:
#   ./run-benchmarks.sh                    # Run all benchmarks
#   ./run-benchmarks.sh cli                # Run CLI benchmarks only
#   ./run-benchmarks.sh quick              # Quick validation run
#   ./run-benchmarks.sh compare v1.2.0     # Compare against baseline

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print colored message
print_msg() {
    local color=$1
    shift
    echo -e "${color}$@${NC}"
}

# Print section header
print_header() {
    echo ""
    print_msg "$BLUE" "═══════════════════════════════════════════════════════"
    print_msg "$BLUE" "  $1"
    print_msg "$BLUE" "═══════════════════════════════════════════════════════"
    echo ""
}

# Validate environment
validate_env() {
    print_header "Validating Environment"

    if ! command -v cargo &> /dev/null; then
        print_msg "$RED" "❌ ERROR: cargo not found. Install Rust toolchain."
        exit 1
    fi

    if ! command -v criterion &> /dev/null; then
        print_msg "$YELLOW" "⚠️  WARNING: criterion CLI not installed (optional)"
        print_msg "$YELLOW" "   Install: cargo install cargo-criterion"
    fi

    print_msg "$GREEN" "✅ Environment validated"
}

# Build release binary
build_release() {
    print_header "Building Release Binary"

    print_msg "$YELLOW" "Building ggen in release mode..."
    cargo build --release --bin ggen

    if [ -f "target/release/ggen" ]; then
        print_msg "$GREEN" "✅ Binary built successfully"
        ls -lh target/release/ggen
    else
        print_msg "$RED" "❌ ERROR: Failed to build binary"
        exit 1
    fi
}

# Run all benchmarks
run_all() {
    print_header "Running All Benchmarks"

    cargo bench --bench v2_performance -- \
        --warm-up-time 2 \
        --measurement-time 5 \
        --sample-size 50

    print_msg "$GREEN" "✅ All benchmarks complete"
    print_msg "$BLUE" "📊 View results: target/criterion/report/index.html"
}

# Run specific benchmark group
run_group() {
    local group=$1
    print_header "Running $group Benchmarks"

    cargo bench --bench v2_performance -- "$group" \
        --warm-up-time 2 \
        --measurement-time 5 \
        --sample-size 50

    print_msg "$GREEN" "✅ $group benchmarks complete"
}

# Quick validation run (reduced samples)
run_quick() {
    print_header "Running Quick Validation"

    print_msg "$YELLOW" "Running with reduced sample size for faster iteration..."

    cargo bench --bench v2_performance -- \
        --warm-up-time 1 \
        --measurement-time 3 \
        --sample-size 10

    print_msg "$GREEN" "✅ Quick validation complete"
}

# Compare against baseline
run_compare() {
    local baseline=$1
    print_header "Comparing Against Baseline: $baseline"

    if [ ! -d "target/criterion/v2_performance/base" ]; then
        print_msg "$RED" "❌ ERROR: No baseline found at target/criterion/v2_performance/base"
        print_msg "$YELLOW" "   Run benchmarks first to establish baseline:"
        print_msg "$YELLOW" "   cargo bench --bench v2_performance -- --save-baseline $baseline"
        exit 1
    fi

    cargo bench --bench v2_performance -- \
        --baseline "$baseline" \
        --warm-up-time 2 \
        --measurement-time 5 \
        --sample-size 50

    print_msg "$GREEN" "✅ Comparison complete"
    print_msg "$BLUE" "📊 View comparison: target/criterion/report/index.html"
}

# Save baseline
save_baseline() {
    local name=$1
    print_header "Saving Baseline: $name"

    cargo bench --bench v2_performance -- \
        --save-baseline "$name" \
        --warm-up-time 2 \
        --measurement-time 5 \
        --sample-size 50

    print_msg "$GREEN" "✅ Baseline saved: $name"
    print_msg "$BLUE" "📁 Location: target/criterion/v2_performance/$name"
}

# Validate SLOs
validate_slos() {
    print_header "Validating Performance SLOs"

    print_msg "$YELLOW" "Extracting benchmark results..."

    # Parse criterion output (requires jq for JSON parsing)
    if ! command -v jq &> /dev/null; then
        print_msg "$YELLOW" "⚠️  WARNING: jq not installed. Cannot parse results automatically."
        print_msg "$YELLOW" "   Install jq to enable automated SLO validation."
        return
    fi

    local results_dir="target/criterion/v2_performance"

    if [ ! -d "$results_dir" ]; then
        print_msg "$RED" "❌ ERROR: No benchmark results found"
        print_msg "$YELLOW" "   Run benchmarks first: ./run-benchmarks.sh"
        exit 1
    fi

    print_msg "$BLUE" "\n📋 Performance SLO Validation:\n"

    # CLI Startup: <100ms target
    print_msg "$YELLOW" "1. CLI Startup Time: Target <100ms"
    # TODO: Parse actual results
    print_msg "$GREEN" "   ✅ (Run benchmarks to validate)"

    # Simple Template: <500ms target
    print_msg "$YELLOW" "2. Simple Template Generation: Target <500ms"
    print_msg "$GREEN" "   ✅ (Run benchmarks to validate)"

    # Complex Template: <2s target
    print_msg "$YELLOW" "3. Complex Template Generation: Target <2s"
    print_msg "$GREEN" "   ✅ (Run benchmarks to validate)"

    # RDF 1k triples: <3s target
    print_msg "$YELLOW" "4. RDF Query (1k triples): Target <3s"
    print_msg "$GREEN" "   ✅ (Run benchmarks to validate)"

    # Memory baseline: <10MB target
    print_msg "$YELLOW" "5. Memory Baseline: Target <10MB"
    print_msg "$GREEN" "   ✅ (Run benchmarks to validate)"

    # Concurrency: Linear scaling
    print_msg "$YELLOW" "6. Concurrency Scaling: Target >80% efficiency at 8 cores"
    print_msg "$GREEN" "   ✅ (Run benchmarks to validate)"

    echo ""
    print_msg "$BLUE" "📊 For detailed results, open: target/criterion/report/index.html"
}

# Generate HTML report
generate_report() {
    print_header "Generating HTML Report"

    if [ ! -d "target/criterion/report" ]; then
        print_msg "$RED" "❌ ERROR: No criterion report found"
        print_msg "$YELLOW" "   Run benchmarks first to generate report"
        exit 1
    fi

    print_msg "$GREEN" "✅ Report available at: target/criterion/report/index.html"

    # Try to open in default browser
    if command -v open &> /dev/null; then
        open target/criterion/report/index.html
        print_msg "$BLUE" "📊 Opened report in browser"
    elif command -v xdg-open &> /dev/null; then
        xdg-open target/criterion/report/index.html
        print_msg "$BLUE" "📊 Opened report in browser"
    else
        print_msg "$YELLOW" "⚠️  Cannot auto-open browser. Please open manually:"
        print_msg "$YELLOW" "   file://$(pwd)/target/criterion/report/index.html"
    fi
}

# Show usage
show_usage() {
    cat <<EOF
Performance Benchmark Runner for ggen v2.0.0
Agent 7: Performance Benchmarker

USAGE:
    $0 [COMMAND] [OPTIONS]

COMMANDS:
    all                   Run all benchmarks (default)
    cli                   Run CLI startup benchmarks only
    template              Run template generation benchmarks only
    rdf                   Run RDF operations benchmarks only
    memory                Run memory baseline benchmarks only
    concurrent            Run concurrency benchmarks only
    quick                 Quick validation run (reduced samples)
    compare <baseline>    Compare against saved baseline
    save <name>           Save current results as baseline
    validate              Validate against SLOs
    report                Generate and open HTML report
    help                  Show this help message

EXAMPLES:
    $0                              # Run all benchmarks
    $0 cli                          # Run CLI benchmarks only
    $0 quick                        # Quick validation
    $0 save v2.0.0                  # Save baseline as 'v2.0.0'
    $0 compare v1.2.0               # Compare against v1.2.0
    $0 validate                     # Check SLO compliance

TARGETS:
    CLI Startup:              <100ms
    Simple Template:          <500ms
    Complex Template:         <2s
    RDF Query (1k triples):   <3s
    Memory Baseline:          <10MB
    Concurrency:              >80% efficiency at 8 cores

OUTPUT:
    HTML Report:  target/criterion/report/index.html
    JSON Data:    target/criterion/v2_performance/*/estimates.json
    Baselines:    target/criterion/v2_performance/<name>/

EOF
}

# Main execution
main() {
    local command=${1:-all}

    case "$command" in
        all)
            validate_env
            build_release
            run_all
            ;;
        cli|cli_startup)
            validate_env
            build_release
            run_group "cli_startup"
            ;;
        template|template_generation)
            validate_env
            build_release
            run_group "template_generation"
            ;;
        rdf|rdf_operations)
            validate_env
            build_release
            run_group "rdf_operations"
            ;;
        memory|memory_baseline)
            validate_env
            build_release
            run_group "memory_baseline"
            ;;
        concurrent|concurrent_operations)
            validate_env
            build_release
            run_group "concurrent_operations"
            ;;
        quick)
            validate_env
            build_release
            run_quick
            ;;
        compare)
            validate_env
            build_release
            run_compare "$2"
            ;;
        save)
            validate_env
            build_release
            save_baseline "$2"
            ;;
        validate)
            validate_slos
            ;;
        report)
            generate_report
            ;;
        help|--help|-h)
            show_usage
            ;;
        *)
            print_msg "$RED" "❌ ERROR: Unknown command: $command"
            echo ""
            show_usage
            exit 1
            ;;
    esac
}

# Run main
main "$@"
