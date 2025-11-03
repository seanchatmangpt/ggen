#!/bin/bash
# Examples Validation Script
# Comprehensive validation suite for ggen-core examples

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASS=0
FAIL=0
SKIP=0

# Utility functions
print_header() {
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"
}

print_test() {
    printf "%-50s" "$1"
}

print_pass() {
    echo -e "${GREEN}✓ PASS${NC}"
    ((PASS++))
}

print_fail() {
    echo -e "${RED}✗ FAIL${NC} - $1"
    ((FAIL++))
}

print_skip() {
    echo -e "${YELLOW}⊘ SKIP${NC} - $1"
    ((SKIP++))
}

# Change to project root
cd "$(dirname "$0")/.." || exit 1
PROJECT_ROOT=$(pwd)

print_header "🔍 ggen-core Examples Validation Suite"

# 1. COMPILATION VALIDATION
print_header "1. Compilation Validation"

print_test "Main crate compiles (cargo check)"
if cargo check --all-targets &>/dev/null; then
    print_pass
else
    print_fail "Compilation errors found"
fi

print_test "Main crate builds (cargo build)"
if cargo build &>/dev/null; then
    print_pass
else
    print_fail "Build errors found"
fi

print_test "Release build works (cargo build --release)"
if cargo build --release &>/dev/null; then
    print_pass
else
    print_fail "Release build failed"
fi

# 2. CONFIGURATION VALIDATION
print_header "2. Configuration Validation"

print_test "Workspace Cargo.toml is valid"
if cargo metadata --no-deps &>/dev/null; then
    print_pass
else
    print_fail "Invalid workspace configuration"
fi

print_test "Examples workspace exists"
if [ -f "examples/Cargo.toml" ]; then
    print_pass
else
    print_fail "examples/Cargo.toml not found"
fi

print_test "All workspace members exist"
MISSING_MEMBERS=0
for member in $(cargo metadata --no-deps --format-version 1 2>/dev/null | jq -r '.workspace_members[]' | cut -d' ' -f1); do
    if [ ! -d "$(cargo metadata --no-deps --format-version 1 2>/dev/null | jq -r '.packages[] | select(.name == "'$member'") | .manifest_path' | xargs dirname)" ]; then
        ((MISSING_MEMBERS++))
    fi
done
if [ $MISSING_MEMBERS -eq 0 ]; then
    print_pass
else
    print_fail "$MISSING_MEMBERS workspace members missing"
fi

# 3. MAKE.TOML VALIDATION
print_header "3. Lifecycle Configuration (make.toml)"

for example in examples/*/; do
    if [ -d "$example" ] && [ "$(basename "$example")" != "target" ]; then
        example_name=$(basename "$example")
        print_test "make.toml exists for $example_name"
        if [ -f "$example/make.toml" ]; then
            print_pass
        else
            print_fail "Not found"
        fi
    fi
done

# 4. SOURCE FILES VALIDATION
print_header "4. Source Files Validation"

for example in examples/*/; do
    if [ -d "$example" ] && [ "$(basename "$example")" != "target" ]; then
        example_name=$(basename "$example")
        print_test "Source files exist for $example_name"
        if [ -f "$example/src/main.rs" ] || [ -f "$example/src/lib.rs" ]; then
            print_pass
        else
            print_fail "No src/main.rs or src/lib.rs found"
        fi
    fi
done

# 5. TESTING VALIDATION
print_header "5. Testing Validation"

print_test "All tests compile"
if cargo test --workspace --no-run &>/dev/null; then
    print_pass
else
    print_fail "Test compilation failed"
fi

print_test "All tests pass"
if cargo test --workspace &>/dev/null; then
    print_pass
else
    print_fail "Some tests failed"
fi

print_test "Doc tests pass"
if cargo test --doc &>/dev/null; then
    print_pass
else
    print_fail "Doc tests failed"
fi

# 6. QUALITY VALIDATION
print_header "6. Code Quality Validation"

print_test "Format check (cargo fmt --check)"
if cargo fmt --check &>/dev/null; then
    print_pass
else
    print_fail "Code needs formatting"
fi

print_test "Clippy passes (no warnings)"
if cargo clippy --workspace -- -D warnings &>/dev/null; then
    print_pass
else
    print_fail "Clippy warnings/errors found"
fi

print_test "Security audit (cargo audit)"
if command -v cargo-audit &>/dev/null; then
    if cargo audit &>/dev/null; then
        print_pass
    else
        print_fail "Security vulnerabilities found"
    fi
else
    print_skip "cargo-audit not installed"
fi

# 7. DOCUMENTATION VALIDATION
print_header "7. Documentation Validation"

print_test "Documentation builds (cargo doc)"
if cargo doc --workspace --no-deps &>/dev/null; then
    print_pass
else
    print_fail "Documentation build failed"
fi

for example in examples/*/; do
    if [ -d "$example" ] && [ "$(basename "$example")" != "target" ]; then
        example_name=$(basename "$example")
        print_test "README exists for $example_name"
        if [ -f "$example/README.md" ]; then
            print_pass
        else
            print_fail "No README.md found"
        fi
    fi
done

# 8. LIFECYCLE VALIDATION
print_header "8. Lifecycle Execution Validation"

if command -v ggen &>/dev/null; then
    for example in examples/*/; do
        if [ -d "$example" ] && [ "$(basename "$example")" != "target" ] && [ -f "$example/make.toml" ]; then
            example_name=$(basename "$example")
            print_test "ggen run build for $example_name"
            if (cd "$example" && ggen run build &>/dev/null); then
                print_pass
            else
                print_fail "Build lifecycle failed"
            fi

            print_test "ggen run test for $example_name"
            if (cd "$example" && ggen run test &>/dev/null); then
                print_pass
            else
                print_fail "Test lifecycle failed"
            fi
        fi
    done
else
    print_skip "ggen not installed"
fi

# 9. PERFORMANCE VALIDATION
print_header "9. Performance Validation"

print_test "Benchmarks compile"
if cargo bench --workspace --no-run &>/dev/null; then
    print_pass
else
    print_fail "Benchmark compilation failed"
fi

print_test "Benchmarks run successfully"
if [ -n "$RUN_BENCHMARKS" ]; then
    if cargo bench --workspace &>/dev/null; then
        print_pass
    else
        print_fail "Benchmarks failed"
    fi
else
    print_skip "Set RUN_BENCHMARKS=1 to run benchmarks"
fi

# 10. SUMMARY
print_header "📊 Validation Summary"

TOTAL=$((PASS + FAIL + SKIP))
PASS_RATE=0
if [ $TOTAL -gt 0 ]; then
    PASS_RATE=$((PASS * 100 / TOTAL))
fi

echo -e "Total Tests:  ${BLUE}$TOTAL${NC}"
echo -e "Passed:       ${GREEN}$PASS${NC} (${PASS_RATE}%)"
echo -e "Failed:       ${RED}$FAIL${NC}"
echo -e "Skipped:      ${YELLOW}$SKIP${NC}"
echo ""

if [ $FAIL -eq 0 ]; then
    echo -e "${GREEN}✓ All validation checks passed!${NC}"
    exit 0
else
    echo -e "${RED}✗ Validation failed with $FAIL errors${NC}"
    echo -e "\nSee ${BLUE}docs/VALIDATION_REPORT.md${NC} for detailed analysis"
    exit 1
fi
