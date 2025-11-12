#!/bin/bash
# SHACL CLI - Benchmark Script
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[BENCHMARK]${NC} $1"; }
log_success() { echo -e "${GREEN}[RESULT]${NC} $1"; }

# Generate test data
generate_test_data() {
    local size=$1
    local output=$2

    log_info "Generating test data ($size nodes)..."

    cat > "$output" <<EOF
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

EOF

    for i in $(seq 1 "$size"); do
        cat >> "$output" <<EOF
:person$i a foaf:Person ;
  foaf:name "Person $i" ;
  foaf:mbox "person$i@example.com" ;
  foaf:age $((20 + RANDOM % 60)) .

EOF
    done
}

# Benchmark shape compilation
benchmark_compilation() {
    log_info "Benchmarking shape compilation..."

    local shapes="$PROJECT_ROOT/samples/person-shape.ttl"

    if [ ! -f "$shapes" ]; then
        log_info "Creating sample shapes..."
        mkdir -p "$PROJECT_ROOT/samples"
        cat > "$shapes" <<'EOF'
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:PersonShape a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
  ] ;
  sh:property [
    sh:path foaf:mbox ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
  ] .
EOF
    fi

    for level in 0 1 2 3; do
        log_info "Testing optimization level $level..."
        /usr/bin/time -p shacl-cli shape compile "$shapes" \
            --optimize "$level" \
            --output "/tmp/compiled-$level.bin" 2>&1 | grep real
    done
}

# Benchmark validation performance
benchmark_validation() {
    log_info "Benchmarking validation performance..."

    local shapes="$PROJECT_ROOT/samples/person-shape.ttl"
    local data_dir="/tmp/shacl-benchmark-data"

    mkdir -p "$data_dir"

    # Test different data sizes
    for size in 10 100 1000 10000; do
        log_info "Testing with $size nodes..."

        local data_file="$data_dir/data-$size.ttl"
        generate_test_data "$size" "$data_file"

        log_info "Single-threaded validation..."
        /usr/bin/time -p shacl-cli validation validate "$data_file" \
            --shapes "$shapes" \
            > /dev/null 2>&1 | grep real || true

        log_info "Batch validation (parallel)..."
        /usr/bin/time -p shacl-cli validation batch "$data_dir" \
            --shapes "$shapes" \
            --parallel 4 \
            > /dev/null 2>&1 | grep real || true
    done

    # Cleanup
    rm -rf "$data_dir"
}

# Benchmark report generation
benchmark_reporting() {
    log_info "Benchmarking report generation..."

    local results="/tmp/validation-results.ttl"

    # Create sample validation results
    cat > "$results" <<'EOF'
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix : <http://example.org/> .

:report a sh:ValidationReport ;
  sh:conforms false ;
  sh:result [
    a sh:ValidationResult ;
    sh:focusNode :person1 ;
    sh:resultSeverity sh:Violation ;
    sh:resultMessage "Email must be valid format" ;
  ] .
EOF

    for format in turtle json html markdown; do
        log_info "Testing $format format..."
        /usr/bin/time -p shacl-cli report generate "$results" \
            --format "$format" \
            > /dev/null 2>&1 | grep real || true
    done
}

# Memory usage benchmark
benchmark_memory() {
    log_info "Benchmarking memory usage..."

    local shapes="$PROJECT_ROOT/samples/person-shape.ttl"
    local data="/tmp/large-data.ttl"

    generate_test_data 10000 "$data"

    log_info "Measuring memory usage for 10,000 nodes..."
    /usr/bin/time -l shacl-cli validation validate "$data" \
        --shapes "$shapes" \
        > /dev/null 2>&1 | grep "maximum resident set size" || true

    rm -f "$data"
}

# Main benchmark suite
main() {
    echo ""
    log_info "Starting SHACL CLI benchmark suite..."
    echo ""

    benchmark_compilation
    echo ""

    benchmark_validation
    echo ""

    benchmark_reporting
    echo ""

    if [[ "$OSTYPE" == "darwin"* ]]; then
        benchmark_memory
        echo ""
    fi

    log_success "Benchmark suite completed!"
    echo ""
}

main "$@"
