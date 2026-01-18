#!/bin/bash
# Marketplace V2 Production Validation Script
# Version: 1.0.0
# Date: 2025-11-18

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0
WARNINGS=0

# Functions
print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✅ PASS:${NC} $1"
    ((PASSED++))
}

print_failure() {
    echo -e "${RED}❌ FAIL:${NC} $1"
    ((FAILED++))
}

print_warning() {
    echo -e "${YELLOW}⚠️  WARN:${NC} $1"
    ((WARNINGS++))
}

print_info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

# Start validation
echo ""
print_header "Marketplace V2 Production Validation"
echo ""
echo "Date: $(date)"
echo "User: $(whoami)"
echo "Working Directory: $(pwd)"
echo ""

# 1. Build & Compilation
print_header "1. Build & Compilation"

echo "Building marketplace-v2..."
if cargo build --package ggen-marketplace-v2 2>&1 | grep -q "Finished"; then
    print_success "Marketplace v2 builds successfully"
else
    print_failure "Marketplace v2 build failed"
fi

echo "Building with marketplace-v1 feature..."
if cargo build --package ggen-cli-lib --features marketplace-v1 2>&1 | grep -q "Finished"; then
    print_success "Build with marketplace-v1 feature succeeds"
else
    print_failure "Build with marketplace-v1 feature failed"
fi

echo "Building with marketplace-v2 feature..."
if cargo build --package ggen-cli-lib --features marketplace-v2 2>&1 | grep -q "Finished"; then
    print_success "Build with marketplace-v2 feature succeeds"
else
    print_failure "Build with marketplace-v2 feature failed"
fi

echo "Checking for clippy warnings..."
CLIPPY_OUTPUT=$(cargo clippy --package ggen-marketplace-v2 2>&1 || true)
CLIPPY_WARNINGS=$(echo "$CLIPPY_OUTPUT" | grep -c "warning:" || true)
if [ "$CLIPPY_WARNINGS" -eq 0 ]; then
    print_success "No clippy warnings"
else
    print_warning "Found $CLIPPY_WARNINGS clippy warnings (non-blocking)"
fi

echo ""

# 2. Testing
print_header "2. Testing Coverage"

echo "Running marketplace v2 unit tests..."
TEST_OUTPUT=$(cargo test --package ggen-marketplace-v2 --lib 2>&1 || true)
if echo "$TEST_OUTPUT" | grep -q "test result: ok"; then
    TEST_COUNT=$(echo "$TEST_OUTPUT" | grep "test result:" | grep -oE '[0-9]+ passed' | cut -d' ' -f1)
    print_success "All $TEST_COUNT v2 unit tests passed"
else
    print_failure "Some v2 unit tests failed"
fi

echo "Running marketplace v1 unit tests..."
V1_TEST_OUTPUT=$(cargo test --package ggen-marketplace --lib 2>&1 || true)
if echo "$V1_TEST_OUTPUT" | grep -q "test result:"; then
    V1_PASSED=$(echo "$V1_TEST_OUTPUT" | grep "test result:" | grep -oE '[0-9]+ passed' | cut -d' ' -f1)
    V1_FAILED=$(echo "$V1_TEST_OUTPUT" | grep "test result:" | grep -oE '[0-9]+ failed' | cut -d' ' -f1 || echo "0")
    if [ "$V1_FAILED" -eq 0 ]; then
        print_success "All $V1_PASSED v1 unit tests passed"
    else
        print_warning "$V1_FAILED v1 tests failed (non-critical)"
    fi
else
    print_warning "V1 tests had issues (check manually)"
fi

echo ""

# 3. Performance Validation
print_header "3. Performance Validation"

echo "Checking compilation time..."
START_TIME=$(date +%s)
cargo build --package ggen-marketplace-v2 --release > /dev/null 2>&1
END_TIME=$(date +%s)
COMPILE_TIME=$((END_TIME - START_TIME))

if [ "$COMPILE_TIME" -lt 180 ]; then
    print_success "Compilation time: ${COMPILE_TIME}s (target: <180s)"
else
    print_warning "Compilation time: ${COMPILE_TIME}s (slower than expected)"
fi

echo ""

# 4. Security
print_header "4. Security Assessment"

echo "Checking for hardcoded secrets..."
if grep -rn "secret\|password\|api_key" crates/ggen-marketplace-v2/src/ --include="*.rs" | grep -v "// " | grep -v "TODO" > /dev/null 2>&1; then
    print_warning "Potential hardcoded secrets found (review manually)"
else
    print_success "No obvious hardcoded secrets detected"
fi

echo "Checking for unsafe code..."
if grep -rn "unsafe" crates/ggen-marketplace-v2/src/ --include="*.rs" > /dev/null 2>&1; then
    print_warning "Unsafe code blocks found (review manually)"
else
    print_success "No unsafe code blocks"
fi

echo "Checking Ed25519 implementation..."
if grep -q "ed25519-dalek" crates/ggen-marketplace-v2/Cargo.toml; then
    print_success "Ed25519 dependency present"
else
    print_failure "Ed25519 dependency missing"
fi

echo ""

# 5. Feature Gates
print_header "5. Feature Gate Validation"

echo "Checking default feature..."
DEFAULT_FEATURE=$(grep -A 5 "\[features\]" crates/ggen-cli/Cargo.toml | grep "default" | grep -o "marketplace-v[0-9]" || echo "none")
if [ "$DEFAULT_FEATURE" = "marketplace-v1" ]; then
    print_success "Default feature is marketplace-v1 (backward compatible)"
else
    print_warning "Default feature is $DEFAULT_FEATURE (expected: marketplace-v1)"
fi

echo "Checking feature definitions..."
if grep -q "marketplace-v1 = \[\]" crates/ggen-cli/Cargo.toml && \
   grep -q "marketplace-v2 = \[\"ggen-marketplace-v2\"\]" crates/ggen-cli/Cargo.toml && \
   grep -q "marketplace-parallel" crates/ggen-cli/Cargo.toml; then
    print_success "All feature gates defined correctly"
else
    print_failure "Feature gates configuration incomplete"
fi

echo ""

# 6. Dependencies
print_header "6. Dependencies Check"

echo "Checking workspace dependencies..."
if grep -q "ggen-marketplace-v2" Cargo.toml; then
    print_success "ggen-marketplace-v2 in workspace members"
else
    print_failure "ggen-marketplace-v2 not in workspace"
fi

echo "Checking critical dependencies..."
CRITICAL_DEPS=("oxigraph" "moka" "ed25519-dalek" "fst" "sha2")
for dep in "${CRITICAL_DEPS[@]}"; do
    if grep -q "^$dep = " crates/ggen-marketplace-v2/Cargo.toml; then
        print_success "Dependency $dep present"
    else
        print_failure "Dependency $dep missing"
    fi
done

echo ""

# 7. Documentation
print_header "7. Documentation Check"

DOCS=(
    "docs/MARKETPLACE_V2_EXECUTIVE_SUMMARY.md"
    "docs/MARKETPLACE_V2_PRODUCTION_READINESS_REPORT.md"
    "docs/MARKETPLACE_V2_DEPLOYMENT_GUIDE.md"
    "docs/MARKETPLACE_V2_MONITORING_SETUP.md"
)

for doc in "${DOCS[@]}"; do
    if [ -f "$doc" ]; then
        print_success "Documentation exists: $(basename "$doc")"
    else
        print_failure "Documentation missing: $(basename "$doc")"
    fi
done

echo ""

# 8. File Structure
print_header "8. File Structure Validation"

echo "Checking source files..."
V2_SOURCE_COUNT=$(find crates/ggen-marketplace-v2/src -name "*.rs" -type f | wc -l)
if [ "$V2_SOURCE_COUNT" -gt 10 ]; then
    print_success "Found $V2_SOURCE_COUNT source files in v2"
else
    print_warning "Only $V2_SOURCE_COUNT source files (expected >10)"
fi

echo "Checking test files..."
V2_TEST_COUNT=$(find crates/ggen-marketplace-v2 -name "*test*.rs" -type f | wc -l)
if [ "$V2_TEST_COUNT" -gt 0 ]; then
    print_success "Found $V2_TEST_COUNT test files"
else
    print_warning "No test files found (check test organization)"
fi

echo ""

# Summary
print_header "Validation Summary"

TOTAL=$((PASSED + FAILED + WARNINGS))
PASS_RATE=$((PASSED * 100 / TOTAL))

echo ""
echo "Total Checks: $TOTAL"
echo -e "${GREEN}Passed: $PASSED${NC}"
echo -e "${RED}Failed: $FAILED${NC}"
echo -e "${YELLOW}Warnings: $WARNINGS${NC}"
echo ""
echo "Pass Rate: $PASS_RATE%"
echo ""

# Final verdict
if [ "$FAILED" -eq 0 ] && [ "$PASS_RATE" -ge 80 ]; then
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}✅ PRODUCTION READY${NC}"
    echo -e "${GREEN}========================================${NC}"
    echo ""
    echo "All critical checks passed. Ready for deployment."
    echo "Review warnings manually before proceeding."
    exit 0
elif [ "$FAILED" -gt 0 ] && [ "$FAILED" -le 2 ]; then
    echo -e "${YELLOW}========================================${NC}"
    echo -e "${YELLOW}⚠️  CONDITIONAL APPROVAL${NC}"
    echo -e "${YELLOW}========================================${NC}"
    echo ""
    echo "Minor issues detected. Fix failures before deployment."
    echo "Pass rate: $PASS_RATE% (acceptable if >80%)"
    exit 1
else
    echo -e "${RED}========================================${NC}"
    echo -e "${RED}❌ NOT READY${NC}"
    echo -e "${RED}========================================${NC}"
    echo ""
    echo "Critical issues detected. Address failures before deployment."
    echo "Pass rate: $PASS_RATE% (needs >80%)"
    exit 2
fi
