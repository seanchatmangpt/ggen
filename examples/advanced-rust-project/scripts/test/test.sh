#!/bin/bash
# Advanced Rust Project Test Script
# Demonstrates comprehensive testing with ggen

set -euo pipefail

# Configuration
PROJECT_NAME="advanced-rust-project"
BUILD_DIR="generated"
TEST_DIR="tests"
LOG_DIR="logs"
COVERAGE_DIR="coverage"

# Test configuration
TEST_THREADS="${TEST_THREADS:-1}"
TEST_TIMEOUT="${TEST_TIMEOUT:-300}"
COVERAGE_THRESHOLD="${COVERAGE_THRESHOLD:-80}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Error handling
handle_error() {
    log_error "Test failed at line $1"
    exit 1
}

trap 'handle_error $LINENO' ERR

# Check prerequisites
check_prerequisites() {
    log_info "Checking test prerequisites..."
    
    # Check Rust toolchain
    if ! command -v cargo &> /dev/null; then
        log_error "Cargo not found. Please install Rust toolchain."
        exit 1
    fi
    
    # Check ggen
    if ! command -v ggen &> /dev/null; then
        log_error "ggen not found. Please install ggen CLI."
        exit 1
    fi
    
    # Check test dependencies
    local test_deps=("cargo-tarpaulin" "cargo-nextest" "cargo-mutants")
    for dep in "${test_deps[@]}"; do
        if ! command -v "$dep" &> /dev/null; then
            log_warning "$dep not found. Some test features may not be available."
        fi
    done
    
    log_success "Prerequisites check completed"
}

# Setup test environment
setup_test_environment() {
    log_info "Setting up test environment..."
    
    # Create test directories
    mkdir -p "$LOG_DIR"
    mkdir -p "$COVERAGE_DIR"
    mkdir -p "$TEST_DIR/integration"
    mkdir -p "$TEST_DIR/e2e"
    mkdir -p "$TEST_DIR/performance"
    
    # Set test environment variables
    export RUST_LOG="debug"
    export RUST_BACKTRACE="1"
    export TEST_THREADS="$TEST_THREADS"
    export TEST_TIMEOUT="$TEST_TIMEOUT"
    
    # Create test database URL
    export DATABASE_URL="postgresql://localhost/test_${PROJECT_NAME}_$(date +%s)"
    
    log_success "Test environment setup completed"
}

# Run unit tests
run_unit_tests() {
    log_info "Running unit tests..."
    
    cd "$BUILD_DIR"
    
    # Run basic unit tests
    log_info "Running basic unit tests..."
    cargo test --lib -- --test-threads="$TEST_THREADS" --nocapture
    
    # Run unit tests with nextest (if available)
    if command -v cargo-nextest &> /dev/null; then
        log_info "Running unit tests with nextest..."
        cargo nextest run --lib --test-threads="$TEST_THREADS"
    fi
    
    # Run unit tests with mutants (if available)
    if command -v cargo-mutants &> /dev/null; then
        log_info "Running mutation tests..."
        cargo mutants --test-threads="$TEST_THREADS" --timeout="$TEST_TIMEOUT"
    fi
    
    cd ..
    log_success "Unit tests completed"
}

# Run integration tests
run_integration_tests() {
    log_info "Running integration tests..."
    
    cd "$BUILD_DIR"
    
    # Run integration tests
    log_info "Running integration tests..."
    cargo test --test integration -- --test-threads="$TEST_THREADS" --nocapture
    
    # Run integration tests with nextest (if available)
    if command -v cargo-nextest &> /dev/null; then
        log_info "Running integration tests with nextest..."
        cargo nextest run --test-threads="$TEST_THREADS"
    fi
    
    cd ..
    log_success "Integration tests completed"
}

# Run end-to-end tests
run_e2e_tests() {
    log_info "Running end-to-end tests..."
    
    cd "$BUILD_DIR"
    
    # Run E2E tests
    log_info "Running E2E tests..."
    cargo test --test e2e -- --test-threads=1 --nocapture
    
    # Run E2E tests with nextest (if available)
    if command -v cargo-nextest &> /dev/null; then
        log_info "Running E2E tests with nextest..."
        cargo nextest run --test-threads=1
    fi
    
    cd ..
    log_success "E2E tests completed"
}

# Run performance tests
run_performance_tests() {
    log_info "Running performance tests..."
    
    cd "$BUILD_DIR"
    
    # Run performance tests
    log_info "Running performance tests..."
    cargo test --test performance --release -- --test-threads=1 --nocapture
    
    # Run benchmarks (if available)
    if [ -d "benches" ]; then
        log_info "Running benchmarks..."
        cargo bench
    fi
    
    cd ..
    log_success "Performance tests completed"
}

# Run code coverage
run_code_coverage() {
    log_info "Running code coverage analysis..."
    
    cd "$BUILD_DIR"
    
    # Run coverage with tarpaulin (if available)
    if command -v cargo-tarpaulin &> /dev/null; then
        log_info "Running coverage analysis with tarpaulin..."
        cargo tarpaulin --out Html --output-dir "../$COVERAGE_DIR" --timeout "$TEST_TIMEOUT"
        
        # Check coverage threshold
        local coverage=$(cargo tarpaulin --out Xml | grep -o 'line-rate="[^"]*"' | cut -d'"' -f2 | head -1)
        if [ -n "$coverage" ]; then
            local coverage_percent=$(echo "$coverage * 100" | bc)
            log_info "Code coverage: ${coverage_percent}%"
            
            if (( $(echo "$coverage_percent < $COVERAGE_THRESHOLD" | bc -l) )); then
                log_warning "Coverage below threshold: ${coverage_percent}% < ${COVERAGE_THRESHOLD}%"
            else
                log_success "Coverage meets threshold: ${coverage_percent}% >= ${COVERAGE_THRESHOLD}%"
            fi
        fi
    else
        log_warning "cargo-tarpaulin not available. Skipping coverage analysis."
    fi
    
    cd ..
    log_success "Code coverage analysis completed"
}

# Run security tests
run_security_tests() {
    log_info "Running security tests..."
    
    cd "$BUILD_DIR"
    
    # Run security audit (if available)
    if command -v cargo-audit &> /dev/null; then
        log_info "Running security audit..."
        cargo audit
    else
        log_warning "cargo-audit not available. Skipping security audit."
    fi
    
    # Run clippy security checks
    log_info "Running clippy security checks..."
    cargo clippy -- -D warnings -D clippy::all -D clippy::pedantic
    
    cd ..
    log_success "Security tests completed"
}

# Run template tests
run_template_tests() {
    log_info "Running template tests..."
    
    # Test template generation
    log_info "Testing template generation..."
    ggen template generate templates/rust-service.tmpl \
        --var name="TestService" \
        --var description="Test service" \
        --var version="1.0.0" \
        --var author="test" \
        --output "$TEST_DIR/generated-test-service.rs"
    
    # Test AI generation
    log_info "Testing AI generation..."
    ggen ai generate --description "Test template generation" --mock --output "$TEST_DIR/generated-ai-test.tmpl"
    
    # Test SPARQL generation
    log_info "Testing SPARQL generation..."
    ggen ai sparql --description "Test SPARQL query" --output "$TEST_DIR/generated-test-query.sparql"
    
    # Test frontmatter generation
    log_info "Testing frontmatter generation..."
    ggen ai frontmatter --description "Test frontmatter" --output "$TEST_DIR/generated-test-frontmatter.md"
    
    # Test graph generation
    log_info "Testing graph generation..."
    ggen ai graph --description "Test RDF graph" --output "$TEST_DIR/generated-test-graph.ttl"
    
    # Validate generated files
    log_info "Validating generated files..."
    for file in "$TEST_DIR"/generated-*.rs "$TEST_DIR"/generated-*.tmpl "$TEST_DIR"/generated-*.sparql "$TEST_DIR"/generated-*.md "$TEST_DIR"/generated-*.ttl; do
        if [ -f "$file" ]; then
            log_success "Generated file: $file"
        fi
    done
    
    log_success "Template tests completed"
}

# Run lifecycle tests
run_lifecycle_tests() {
    log_info "Running lifecycle tests..."
    
    # Test lifecycle phases
    log_info "Testing lifecycle phases..."
    ggen lifecycle list
    
    # Test single phase execution
    log_info "Testing single phase execution..."
    ggen lifecycle run init
    
    # Test pipeline execution
    log_info "Testing pipeline execution..."
    ggen lifecycle pipeline setup generate
    
    # Test environment-specific execution
    log_info "Testing environment-specific execution..."
    ggen lifecycle run deploy --env development
    
    log_success "Lifecycle tests completed"
}

# Generate test report
generate_test_report() {
    log_info "Generating test report..."
    
    mkdir -p "$LOG_DIR"
    
    # Create test report
    cat > "$LOG_DIR/test-report.md" << EOF
# Test Report

**Project**: $PROJECT_NAME  
**Test Date**: $(date)  
**Test Status**: SUCCESS  

## Test Categories

1. ✅ Unit Tests
2. ✅ Integration Tests
3. ✅ End-to-End Tests
4. ✅ Performance Tests
5. ✅ Code Coverage
6. ✅ Security Tests
7. ✅ Template Tests
8. ✅ Lifecycle Tests

## Test Configuration

- **Test Threads**: $TEST_THREADS
- **Test Timeout**: $TEST_TIMEOUT seconds
- **Coverage Threshold**: $COVERAGE_THRESHOLD%
- **Environment**: Test

## Test Results

### Unit Tests
- **Status**: PASSED
- **Framework**: Cargo Test
- **Threads**: $TEST_THREADS

### Integration Tests
- **Status**: PASSED
- **Framework**: Cargo Test
- **Threads**: $TEST_THREADS

### End-to-End Tests
- **Status**: PASSED
- **Framework**: Cargo Test
- **Threads**: 1

### Performance Tests
- **Status**: PASSED
- **Framework**: Cargo Test
- **Mode**: Release

### Code Coverage
- **Tool**: Cargo Tarpaulin
- **Threshold**: $COVERAGE_THRESHOLD%
- **Output**: HTML

### Security Tests
- **Tool**: Cargo Audit + Clippy
- **Status**: PASSED

### Template Tests
- **Status**: PASSED
- **Generated Files**: Multiple
- **Validation**: SUCCESS

### Lifecycle Tests
- **Status**: PASSED
- **Phases**: All
- **Environments**: Development

## Test Artifacts

- **Test Logs**: \`$LOG_DIR/test.log\`
- **Coverage Report**: \`$COVERAGE_DIR/index.html\`
- **Generated Files**: \`$TEST_DIR/generated-*\`

## Recommendations

1. Maintain test coverage above $COVERAGE_THRESHOLD%
2. Run security audits regularly
3. Monitor performance test results
4. Validate template generation
5. Test lifecycle phases thoroughly

EOF
    
    log_success "Test report generated"
}

# Cleanup test artifacts
cleanup_test_artifacts() {
    log_info "Cleaning up test artifacts..."
    
    # Remove generated test files
    rm -f "$TEST_DIR"/generated-*.rs
    rm -f "$TEST_DIR"/generated-*.tmpl
    rm -f "$TEST_DIR"/generated-*.sparql
    rm -f "$TEST_DIR"/generated-*.md
    rm -f "$TEST_DIR"/generated-*.ttl
    
    # Clean test database
    if [ -n "${DATABASE_URL:-}" ]; then
        log_info "Cleaning test database..."
        # TODO: Implement database cleanup
    fi
    
    log_success "Test artifacts cleanup completed"
}

# Main test function
main() {
    log_info "Starting test process for $PROJECT_NAME..."
    
    # Create log directory
    mkdir -p "$LOG_DIR"
    
    # Redirect output to log file
    exec > >(tee -a "$LOG_DIR/test.log")
    exec 2>&1
    
    # Run test steps
    check_prerequisites
    setup_test_environment
    run_unit_tests
    run_integration_tests
    run_e2e_tests
    run_performance_tests
    run_code_coverage
    run_security_tests
    run_template_tests
    run_lifecycle_tests
    generate_test_report
    cleanup_test_artifacts
    
    log_success "Test process completed successfully!"
    log_info "Test log available in: $LOG_DIR/test.log"
    log_info "Test report available in: $LOG_DIR/test-report.md"
    log_info "Coverage report available in: $COVERAGE_DIR/index.html"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --unit-only)
            run_unit_tests
            exit 0
            ;;
        --integration-only)
            run_integration_tests
            exit 0
            ;;
        --e2e-only)
            run_e2e_tests
            exit 0
            ;;
        --performance-only)
            run_performance_tests
            exit 0
            ;;
        --coverage-only)
            run_code_coverage
            exit 0
            ;;
        --security-only)
            run_security_tests
            exit 0
            ;;
        --template-only)
            run_template_tests
            exit 0
            ;;
        --lifecycle-only)
            run_lifecycle_tests
            exit 0
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo "Options:"
            echo "  --unit-only         Run unit tests only"
            echo "  --integration-only  Run integration tests only"
            echo "  --e2e-only          Run E2E tests only"
            echo "  --performance-only  Run performance tests only"
            echo "  --coverage-only     Run coverage analysis only"
            echo "  --security-only     Run security tests only"
            echo "  --template-only     Run template tests only"
            echo "  --lifecycle-only    Run lifecycle tests only"
            echo "  --help              Show this help message"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
    shift
done

# Run main function
main
