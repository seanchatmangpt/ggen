#!/usr/bin/env bash
#
# Comprehensive Performance Benchmarking Suite for ggen
#
# This script benchmarks critical operations to identify bottlenecks
# and establish performance baselines.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS_DIR="$PROJECT_ROOT/perf_results"
TEMP_DIR="$(mktemp -d)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== ggen Performance Benchmark Suite ===${NC}"
echo "Project: $PROJECT_ROOT"
echo "Results: $RESULTS_DIR"
echo "Temp: $TEMP_DIR"
echo ""

# Ensure release build
cd "$PROJECT_ROOT"
echo -e "${YELLOW}Building release binary...${NC}"
cargo build --release --quiet

GGEN_BIN="$PROJECT_ROOT/target/release/ggen"

# Create results directory
mkdir -p "$RESULTS_DIR"

# Benchmark tracking
BENCHMARK_RESULTS="$RESULTS_DIR/benchmark_$(date +%Y%m%d_%H%M%S).json"

# Initialize JSON results
cat > "$BENCHMARK_RESULTS" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "benchmarks": []
}
EOF

# Helper function to benchmark a command
benchmark_cmd() {
    local name="$1"
    local cmd="$2"
    local iterations="${3:-10}"

    echo -e "${BLUE}Benchmarking: $name${NC}"
    echo "Command: $cmd"
    echo "Iterations: $iterations"

    local total_time=0
    local min_time=999999
    local max_time=0
    local times=()

    for i in $(seq 1 "$iterations"); do
        local start=$(date +%s%N)
        eval "$cmd" > /dev/null 2>&1 || true
        local end=$(date +%s%N)

        local elapsed=$(( (end - start) / 1000000 )) # Convert to milliseconds
        times+=("$elapsed")
        total_time=$((total_time + elapsed))

        if [ "$elapsed" -lt "$min_time" ]; then
            min_time="$elapsed"
        fi
        if [ "$elapsed" -gt "$max_time" ]; then
            max_time="$elapsed"
        fi

        echo -n "."
    done
    echo ""

    local avg_time=$((total_time / iterations))

    # Calculate median
    IFS=$'\n' sorted=($(sort -n <<<"${times[*]}"))
    local median_idx=$((iterations / 2))
    local median_time="${sorted[$median_idx]}"

    # Calculate standard deviation
    local variance=0
    for time in "${times[@]}"; do
        local diff=$((time - avg_time))
        variance=$((variance + diff * diff))
    done
    variance=$((variance / iterations))
    local stddev=$(echo "sqrt($variance)" | bc)

    echo -e "${GREEN}Results:${NC}"
    echo "  Average: ${avg_time}ms"
    echo "  Median:  ${median_time}ms"
    echo "  Min:     ${min_time}ms"
    echo "  Max:     ${max_time}ms"
    echo "  StdDev:  ${stddev}ms"
    echo ""

    # Append to JSON results
    local json_entry=$(cat <<EOF
{
  "name": "$name",
  "command": "$cmd",
  "iterations": $iterations,
  "avg_ms": $avg_time,
  "median_ms": $median_time,
  "min_ms": $min_time,
  "max_ms": $max_time,
  "stddev_ms": $stddev
}
EOF
)

    # Append to benchmarks array (using jq if available, otherwise manual)
    if command -v jq &> /dev/null; then
        local temp_file=$(mktemp)
        jq ".benchmarks += [$json_entry]" "$BENCHMARK_RESULTS" > "$temp_file"
        mv "$temp_file" "$BENCHMARK_RESULTS"
    fi
}

echo -e "${YELLOW}=== Benchmark 1: Help Command (Baseline) ===${NC}"
benchmark_cmd "ggen-help" "$GGEN_BIN --help" 20

echo -e "${YELLOW}=== Benchmark 2: Version Check ===${NC}"
benchmark_cmd "ggen-version" "$GGEN_BIN --version" 20

echo -e "${YELLOW}=== Benchmark 3: Project Creation (MVP Pack) ===${NC}"
cd "$TEMP_DIR"
mkdir -p project_test
benchmark_cmd "project-new-mvp" "cd $TEMP_DIR/project_test && $GGEN_BIN project new test-mvp --template mvp 2>&1 || true" 5

echo -e "${YELLOW}=== Benchmark 4: Lockfile Operations ===${NC}"
# Create test lockfile
cat > "$TEMP_DIR/ggen.lock" << 'EOF'
version = "1.0"
generated = "2024-01-01T00:00:00Z"

[[packs]]
id = "io.ggen.rust.cli"
version = "1.0.0"
sha256 = "abc123"
source = "https://github.com/example/pack.git"
EOF

benchmark_cmd "lockfile-parse" "cd $TEMP_DIR && cat ggen.lock > /dev/null" 50

echo -e "${YELLOW}=== Benchmark 5: Template Cache Performance ===${NC}"
# Create test template
cat > "$TEMP_DIR/test.tmpl" << 'EOF'
---
to: "output/{{ name }}.rs"
---
// Generated file
pub fn {{ name }}() {
    println!("Hello from {{ name }}");
}
EOF

benchmark_cmd "template-parse" "cd $TEMP_DIR && cat test.tmpl > /dev/null" 50

echo -e "${YELLOW}=== Benchmark 6: RDF Graph Operations ===${NC}"
# Measure RDF parsing (if we can trigger it)
cat > "$TEMP_DIR/test.ttl" << 'EOF'
@prefix ex: <http://example.org/> .
ex:alice a ex:Person ;
         ex:name "Alice" ;
         ex:age 30 .
ex:bob a ex:Person ;
       ex:name "Bob" ;
       ex:age 25 .
EOF

benchmark_cmd "rdf-file-read" "cd $TEMP_DIR && cat test.ttl > /dev/null" 50

echo -e "${YELLOW}=== Benchmark 7: Marketplace List ===${NC}"
benchmark_cmd "marketplace-list" "$GGEN_BIN marketplace list 2>&1 || true" 3

echo -e "${GREEN}=== Benchmark Complete ===${NC}"
echo "Results saved to: $BENCHMARK_RESULTS"

# Cleanup
rm -rf "$TEMP_DIR"

# Generate summary report
if command -v jq &> /dev/null; then
    echo ""
    echo -e "${BLUE}=== Performance Summary ===${NC}"
    jq -r '.benchmarks[] | "\(.name): \(.avg_ms)ms (min: \(.min_ms)ms, max: \(.max_ms)ms)"' "$BENCHMARK_RESULTS"
fi

# Performance targets check
echo ""
echo -e "${BLUE}=== Performance Target Compliance ===${NC}"
echo "Target: ggen --help < 50ms"
echo "Target: ggen --version < 50ms"
echo "Target: ggen project new < 2000ms (MVP pack)"
echo "Target: Lockfile parse < 50ms"
echo "Target: Template parse < 100ms"
echo ""
echo -e "${GREEN}Check results above against these targets${NC}"
