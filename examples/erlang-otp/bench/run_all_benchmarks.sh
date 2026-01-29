#!/bin/bash
# Run all benchmark suites and generate reports

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS_DIR="${SCRIPT_DIR}/bench_results"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
RESULTS_FILE="${RESULTS_DIR}/benchmark_${TIMESTAMP}.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Create results directory
mkdir -p "${RESULTS_DIR}"

echo "========================================="
echo "Erlang/OTP Benchmark Suite"
echo "========================================="
echo "Timestamp: ${TIMESTAMP}"
echo "Results: ${RESULTS_FILE}"
echo ""

# Check if Erlang is available
if ! command -v erl &> /dev/null; then
    echo -e "${RED}Error: Erlang not found in PATH${NC}"
    exit 1
fi

echo -e "${GREEN}✓${NC} Erlang/OTP found: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
echo ""

# Compile modules
echo "Compiling benchmark modules..."
cd "${SCRIPT_DIR}"

erl -noshell -eval '
    compile:file("throughput_bench.erl", [debug_info, {outdir, "."}]),
    compile:file("latency_bench.erl", [debug_info, {outdir, "."}]),
    compile:file("call_router_bench.erl", [debug_info, {outdir, "."}]),
    halt().
' || {
    echo -e "${RED}✗ Compilation failed${NC}"
    exit 1
}

echo -e "${GREEN}✓${NC} Compilation successful"
echo ""

# Function to run Erlang benchmark
run_erlang_bench() {
    local bench_name=$1
    local bench_code=$2

    echo "----------------------------------------"
    echo "Running: ${bench_name}"
    echo "----------------------------------------"

    erl -noshell -pa . -eval "${bench_code}" | tee -a "${RESULTS_FILE}"

    if [ ${PIPESTATUS[0]} -eq 0 ]; then
        echo -e "${GREEN}✓${NC} ${bench_name} completed"
    else
        echo -e "${RED}✗${NC} ${bench_name} failed"
        return 1
    fi
    echo ""
}

# Start application (if needed)
# Uncomment if your benchmarks require a running application
# echo "Starting application..."
# erl -noshell -sname bench_runner -eval 'application:ensure_all_started(your_app), timer:sleep(2000).' &
# APP_PID=$!
# sleep 3

# Run Throughput Benchmark
run_erlang_bench "Throughput Benchmark (100 workers, 60s)" '
    throughput_bench:run(#{
        workers => 100,
        duration_sec => 60,
        warmup_sec => 5
    }),
    halt().
' || echo -e "${YELLOW}Warning: Throughput benchmark failed${NC}"

# Run Latency Benchmark
run_erlang_bench "Latency Benchmark (10,000 samples)" '
    latency_bench:run(10000),
    halt().
' || echo -e "${YELLOW}Warning: Latency benchmark failed${NC}"

# Run Quick Throughput Test (for CI)
if [ "${CI}" = "true" ]; then
    echo "CI mode detected - running quick benchmarks"

    run_erlang_bench "Quick Throughput (10 workers, 10s)" '
        throughput_bench:run(#{
            workers => 10,
            duration_sec => 10,
            warmup_sec => 2
        }),
        halt().
    '

    run_erlang_bench "Quick Latency (1,000 samples)" '
        latency_bench:run(1000),
        halt().
    '
fi

# Stop application (if started)
# if [ -n "${APP_PID}" ]; then
#     kill ${APP_PID} 2>/dev/null || true
# fi

# Generate summary
echo "========================================="
echo "Benchmark Summary"
echo "========================================="
echo ""
echo "Results saved to: ${RESULTS_FILE}"
echo ""

# Extract key metrics if available
if [ -f "${RESULTS_FILE}" ]; then
    echo "Key Metrics:"
    echo "------------"

    # Extract throughput
    THROUGHPUT=$(grep -oP 'Throughput: \K[0-9.]+' "${RESULTS_FILE}" | head -1 || echo "N/A")
    echo "Throughput: ${THROUGHPUT} ops/sec"

    # Extract P99 latency
    P99=$(grep -oP 'P99:\s+\K[0-9]+' "${RESULTS_FILE}" | head -1 || echo "N/A")
    echo "P99 Latency: ${P99} μs"

    # Extract error rate
    ERROR_RATE=$(grep -oP 'Error Rate: \K[0-9.]+' "${RESULTS_FILE}" | head -1 || echo "N/A")
    echo "Error Rate: ${ERROR_RATE}%"
fi

echo ""
echo "========================================="
echo -e "${GREEN}Benchmark suite completed${NC}"
echo "========================================="

# Create symlink to latest results
ln -sf "benchmark_${TIMESTAMP}.txt" "${RESULTS_DIR}/latest.txt"

exit 0
