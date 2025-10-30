#!/usr/bin/env bash
# CLNRM Test Suite Runner
# Executes all .clnrm.toml tests with comprehensive validation

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
CLNRM_BIN="${HOME}/dev/clnrm/target/release/cleanroom"
OTEL_ENDPOINT="http://localhost:4318"
REPORT_DIR="/Users/sac/ggen/target/clnrm-reports"
GGEN_ROOT="/Users/sac/ggen"

# Create report directory
mkdir -p "${REPORT_DIR}"

echo -e "${BLUE}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║         CLNRM Test Suite Runner for ggen                ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check prerequisites
check_prerequisites() {
    echo -e "${YELLOW}Checking prerequisites...${NC}"

    # Check CLNRM binary
    if [ ! -f "${CLNRM_BIN}" ]; then
        echo -e "${RED}❌ CLNRM binary not found at ${CLNRM_BIN}${NC}"
        echo -e "${YELLOW}Build it with: cd ~/dev/clnrm && cargo build --release --bin cleanroom${NC}"
        exit 1
    fi
    echo -e "${GREEN}✅ CLNRM binary found${NC}"

    # Check OTEL collector
    if ! curl -s -o /dev/null -w "%{http_code}" "${OTEL_ENDPOINT}/v1/traces" | grep -q "405\|200"; then
        echo -e "${RED}❌ OTEL collector not responding at ${OTEL_ENDPOINT}${NC}"
        echo -e "${YELLOW}Start it with: docker run -d --name otel-collector -p 4318:4318 otel/opentelemetry-collector:latest${NC}"
        exit 1
    fi
    echo -e "${GREEN}✅ OTEL collector is running${NC}"

    # Check ggen binary
    if [ ! -f "${GGEN_ROOT}/target/release/ggen" ]; then
        echo -e "${YELLOW}⚠️  ggen binary not found, building...${NC}"
        cd "${GGEN_ROOT}"
        cargo build --release
    fi
    echo -e "${GREEN}✅ ggen binary ready${NC}"
    echo ""
}

# Validate all test files
validate_tests() {
    echo -e "${YELLOW}Validating CLNRM test configurations...${NC}"

    local failed=0

    for test_file in "${GGEN_ROOT}"/tests/clnrm/**/*.clnrm.toml; do
        if [ -f "$test_file" ]; then
            echo -n "  Validating $(basename "$test_file")... "
            if "${CLNRM_BIN}" validate "$test_file" &>/dev/null; then
                echo -e "${GREEN}✅${NC}"
            else
                echo -e "${RED}❌${NC}"
                failed=$((failed + 1))
            fi
        fi
    done

    if [ $failed -gt 0 ]; then
        echo -e "${RED}❌ ${failed} test(s) failed validation${NC}"
        exit 1
    fi

    echo -e "${GREEN}✅ All tests validated successfully${NC}"
    echo ""
}

# Run a single test
run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .clnrm.toml)
    local category=$(basename "$(dirname "$test_file")")
    local report_prefix="${REPORT_DIR}/${category}_${test_name}"

    echo -e "${BLUE}▶ Running ${category}/${test_name}...${NC}"

    local start_time=$(date +%s)

    if "${CLNRM_BIN}" run "$test_file" \
        --otel-endpoint "${OTEL_ENDPOINT}" \
        --report-json "${report_prefix}.json" \
        --report-junit "${report_prefix}.xml" \
        --digest "${report_prefix}.sha256" \
        --trace-output "${report_prefix}_traces.json" \
        > "${report_prefix}_stdout.log" 2> "${report_prefix}_stderr.log"; then

        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        echo -e "${GREEN}  ✅ PASSED${NC} (${duration}s)"
        echo -e "     Reports: ${report_prefix}.*"
        return 0
    else
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        echo -e "${RED}  ❌ FAILED${NC} (${duration}s)"
        echo -e "     Logs: ${report_prefix}_*.log"
        return 1
    fi
}

# Main execution
main() {
    check_prerequisites
    validate_tests

    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}           Running CLNRM Test Suite                        ${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo ""

    local total=0
    local passed=0
    local failed=0
    local start_time=$(date +%s)

    # Run marketplace tests
    echo -e "${YELLOW}📦 Marketplace Tests${NC}"
    for test_file in "${GGEN_ROOT}"/tests/clnrm/marketplace/*.clnrm.toml; do
        if [ -f "$test_file" ]; then
            total=$((total + 1))
            if run_test "$test_file"; then
                passed=$((passed + 1))
            else
                failed=$((failed + 1))
            fi
            echo ""
        fi
    done

    # Run lifecycle tests
    echo -e "${YELLOW}🔄 Lifecycle Tests${NC}"
    for test_file in "${GGEN_ROOT}"/tests/clnrm/lifecycle/*.clnrm.toml; do
        if [ -f "$test_file" ]; then
            total=$((total + 1))
            if run_test "$test_file"; then
                passed=$((passed + 1))
            else
                failed=$((failed + 1))
            fi
            echo ""
        fi
    done

    local end_time=$(date +%s)
    local total_duration=$((end_time - start_time))

    # Summary
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}           Test Suite Summary                              ${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo -e "  Total Tests: ${total}"
    echo -e "  ${GREEN}Passed: ${passed}${NC}"
    echo -e "  ${RED}Failed: ${failed}${NC}"
    echo -e "  Duration: ${total_duration}s"
    echo -e "  Reports: ${REPORT_DIR}/"
    echo ""

    if [ $failed -eq 0 ]; then
        echo -e "${GREEN}🎉 All tests passed!${NC}"
        echo ""
        echo -e "${YELLOW}Next steps:${NC}"
        echo -e "  1. Review reports in ${REPORT_DIR}/"
        echo -e "  2. Analyze traces with: jq . ${REPORT_DIR}/*_traces.json"
        echo -e "  3. Verify SHA-256 digests: cat ${REPORT_DIR}/*.sha256"
        echo ""
        return 0
    else
        echo -e "${RED}❌ ${failed} test(s) failed${NC}"
        echo ""
        echo -e "${YELLOW}Troubleshooting:${NC}"
        echo -e "  1. Check logs in ${REPORT_DIR}/*_stderr.log"
        echo -e "  2. Verify OTEL collector: docker logs otel-collector"
        echo -e "  3. Review test configuration: cat tests/clnrm/**/*.clnrm.toml"
        echo ""
        return 1
    fi
}

# Trap errors
trap 'echo -e "${RED}❌ Test suite failed with error${NC}"' ERR

# Run main
main "$@"
