#!/bin/bash
# Run all stress tests and validate system resilience

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS_DIR="${SCRIPT_DIR}/stress_results"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
RESULTS_FILE="${RESULTS_DIR}/stress_${TIMESTAMP}.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Create results directory
mkdir -p "${RESULTS_DIR}"

echo "========================================="
echo "🐵 Erlang/OTP Stress Test Suite 🐵"
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
echo "Compiling stress test modules..."
cd "${SCRIPT_DIR}"

erl -noshell -eval '
    compile:file("chaos_monkey.erl", [debug_info, {outdir, "."}]),
    compile:file("fault_injection.erl", [debug_info, {outdir, "."}]),
    compile:file("cluster_stress.erl", [debug_info, {outdir, "."}]),
    halt().
' || {
    echo -e "${RED}✗ Compilation failed${NC}"
    exit 1
}

echo -e "${GREEN}✓${NC} Compilation successful"
echo ""

# Function to run stress test
run_stress_test() {
    local test_name=$1
    local test_code=$2
    local duration=$3

    echo "----------------------------------------"
    echo -e "${BLUE}Running: ${test_name}${NC}"
    echo "Estimated duration: ${duration}"
    echo "----------------------------------------"

    # Run test and capture output
    if erl -noshell -pa . -eval "${test_code}" 2>&1 | tee -a "${RESULTS_FILE}"; then
        echo -e "${GREEN}✓${NC} ${test_name} completed"
        return 0
    else
        echo -e "${RED}✗${NC} ${test_name} failed"
        return 1
    fi
    echo ""
}

# Determine test mode (quick vs full)
if [ "${CI}" = "true" ] || [ "$1" = "quick" ]; then
    echo -e "${YELLOW}Running in QUICK mode (for CI/testing)${NC}"
    echo ""
    TEST_MODE="quick"
    CHAOS_DURATION=30
    FAULT_ITERATIONS=3
else
    echo -e "${BLUE}Running in FULL mode${NC}"
    echo ""
    TEST_MODE="full"
    CHAOS_DURATION=300
    FAULT_ITERATIONS=10
fi

# 1. Chaos Monkey Tests
echo "========================================="
echo "1. Chaos Monkey Tests"
echo "========================================="
echo ""

if [ "${TEST_MODE}" = "quick" ]; then
    # Quick chaos test
    run_stress_test "Quick Chaos Monkey" '
        chaos_monkey:run(#{
            target_supervisor => call_router_sup,
            duration_sec => 30,
            scenarios => [
                {kill_random_worker, 0.50},
                {message_flood, 0.50}
            ],
            failure_interval_ms => 2000
        }),
        halt().
    ' "30 seconds"
else
    # Full chaos test
    run_stress_test "Full Chaos Monkey" '
        chaos_monkey:run(#{
            target_supervisor => call_router_sup,
            duration_sec => 300,
            scenarios => [
                {kill_random_worker, 0.30},
                {network_partition, 0.20},
                {message_flood, 0.20},
                {memory_pressure, 0.15},
                {kill_supervisor, 0.10},
                {cpu_spike, 0.05}
            ],
            failure_interval_ms => 1000,
            report_interval_sec => 30
        }),
        halt().
    ' "5 minutes"
fi

# 2. Fault Injection Tests
echo "========================================="
echo "2. Fault Injection Tests"
echo "========================================="
echo ""

# Test supervisor crash recovery
run_stress_test "Supervisor Crash Recovery" '
    fault_injection:inject_with_verification(
        supervisor_crash,
        call_router_sup,
        5000
    ),
    halt().
' "10 seconds" || echo -e "${YELLOW}Warning: Supervisor crash test failed${NC}"

# Test worker crash recovery
run_stress_test "Worker Crash Recovery" '
    fault_injection:inject_with_verification(
        worker_crash,
        #{supervisor => call_router_sup, index => 1},
        5000
    ),
    halt().
' "10 seconds" || echo -e "${YELLOW}Warning: Worker crash test failed${NC}"

# Test message queue overflow
run_stress_test "Message Queue Overflow" '
    Pid = whereis(call_router_server),
    fault_injection:inject_with_verification(
        message_queue_overflow,
        #{target_pid => Pid, num_messages => 10000},
        5000
    ),
    halt().
' "15 seconds" || echo -e "${YELLOW}Warning: Message queue test failed${NC}"

# Test invalid input handling
run_stress_test "Invalid Input Handling" '
    fault_injection:inject(invalid_input, #{}),
    halt().
' "5 seconds" || echo -e "${YELLOW}Warning: Invalid input test failed${NC}"

# 3. Memory Stress Test
echo "========================================="
echo "3. Memory Stress Test"
echo "========================================="
echo ""

run_stress_test "Memory Pressure Test" '
    fault_injection:inject_with_verification(
        memory_exhausted,
        #{size_mb => 100},
        10000
    ),
    halt().
' "30 seconds" || echo -e "${YELLOW}Warning: Memory stress test failed${NC}"

# 4. Cluster Stress Tests (if nodes available)
echo "========================================="
echo "4. Cluster Stress Tests"
echo "========================================="
echo ""

CLUSTER_NODES=$(erl -noshell -eval 'io:format("~p~n", [nodes()]), halt().')

if [ -n "${CLUSTER_NODES}" ] && [ "${CLUSTER_NODES}" != "[]" ]; then
    echo -e "${GREEN}Cluster nodes detected: ${CLUSTER_NODES}${NC}"
    echo ""

    # Test node failure
    run_stress_test "Cluster Node Failure" '
        Nodes = nodes(),
        case Nodes of
            [] ->
                io:format("No cluster nodes available~n"),
                ok;
            [Node | _] ->
                cluster_stress:test_node_failure(Node)
        end,
        halt().
    ' "20 seconds" || echo -e "${YELLOW}Warning: Node failure test failed${NC}"

    # Test split-brain scenario (only in full mode)
    if [ "${TEST_MODE}" = "full" ]; then
        run_stress_test "Split-Brain Scenario" '
            Nodes = nodes(),
            case length(Nodes) >= 2 of
                true ->
                    cluster_stress:test_split_brain([node() | Nodes]);
                false ->
                    io:format("Insufficient nodes for split-brain test~n"),
                    ok
            end,
            halt().
        ' "30 seconds" || echo -e "${YELLOW}Warning: Split-brain test failed${NC}"
    fi
else
    echo -e "${YELLOW}⚠ No cluster nodes detected - skipping cluster stress tests${NC}"
    echo "To run cluster tests, start Erlang nodes with:"
    echo "  erl -sname node1"
    echo "  erl -sname node2"
    echo ""
fi

# 5. System Health Verification
echo "========================================="
echo "5. Post-Test Health Verification"
echo "========================================="
echo ""

run_stress_test "System Health Check" '
    Health = cluster_stress:verify_cluster_health(),
    io:format("Final health status: ~p~n", [Health]),
    case Health of
        healthy -> halt(0);
        _ -> halt(1)
    end.
' "5 seconds" || {
    echo -e "${RED}✗ System health check FAILED${NC}"
    echo -e "${RED}WARNING: System may be in unhealthy state!${NC}"
}

# Generate summary report
echo ""
echo "========================================="
echo "Stress Test Summary"
echo "========================================="
echo ""
echo "Results saved to: ${RESULTS_FILE}"
echo ""

# Count test results
if [ -f "${RESULTS_FILE}" ]; then
    TOTAL_TESTS=$(grep -c "Running:" "${RESULTS_FILE}" || echo "0")
    PASSED_TESTS=$(grep -c "✓.*completed" "${RESULTS_FILE}" || echo "0")
    FAILED_TESTS=$(grep -c "✗.*failed" "${RESULTS_FILE}" || echo "0")

    echo "Test Results:"
    echo "-------------"
    echo "Total Tests: ${TOTAL_TESTS}"
    echo -e "${GREEN}Passed: ${PASSED_TESTS}${NC}"

    if [ "${FAILED_TESTS}" -gt 0 ]; then
        echo -e "${RED}Failed: ${FAILED_TESTS}${NC}"
    else
        echo "Failed: 0"
    fi

    # Calculate success rate
    if [ "${TOTAL_TESTS}" -gt 0 ]; then
        SUCCESS_RATE=$(awk "BEGIN {printf \"%.1f\", (${PASSED_TESTS}/${TOTAL_TESTS})*100}")
        echo "Success Rate: ${SUCCESS_RATE}%"
    fi

    echo ""

    # Extract chaos monkey stats if available
    if grep -q "Total Failures:" "${RESULTS_FILE}"; then
        echo "Chaos Monkey Stats:"
        echo "-------------------"
        grep "Total Failures:" "${RESULTS_FILE}" || true
        grep "Unrecovered Failures:" "${RESULTS_FILE}" || true
        echo ""
    fi
fi

# Create symlink to latest results
ln -sf "stress_${TIMESTAMP}.txt" "${RESULTS_DIR}/latest.txt"

echo "========================================="

# Determine exit code
if [ "${FAILED_TESTS}" -gt 0 ]; then
    echo -e "${RED}Stress tests completed with failures${NC}"
    exit 1
else
    echo -e "${GREEN}All stress tests passed!${NC}"
    exit 0
fi
