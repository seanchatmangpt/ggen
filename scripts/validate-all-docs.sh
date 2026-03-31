#!/bin/bash
# scripts/validate-all-docs.sh
# Runs all documentation validation tests

set -euo pipefail

echo "=== ggen Documentation Validation Suite ==="
echo ""

# Track failures
FAILURES=0

# Step 1: Unit tests for template rendering
echo "Step 1: Running unit tests..."
if cargo test -p ggen-core --test elixir_a2a_render_test 2>&1 | grep -q "test result: ok"; then
    echo "✓ Elixir A2A unit tests passed"
else
    echo "✗ Elixir A2A unit tests FAILED"
    FAILURES=$((FAILURES + 1))
fi

if cargo test -p ggen-core --test mcp_template_validation 2>&1 | grep -q "test result: ok"; then
    echo "✓ MCP template unit tests passed"
else
    echo "✗ MCP template unit tests FAILED"
    FAILURES=$((FAILURES + 1))
fi

# Step 2: End-to-end tests
echo ""
echo "Step 2: Running end-to-end tests..."
if cargo test -p ggen-core --test elixir_a2a_e2e_test 2>&1 | grep -q "test result: ok"; then
    echo "✓ Elixir A2A E2E test passed"
else
    echo "✗ Elixir A2A E2E test FAILED"
    FAILURES=$((FAILURES + 1))
fi

if cargo test -p ggen-core --test mcp_rmcp_e2e_test 2>&1 | grep -q "test result: ok"; then
    echo "✓ rmcp E2E test passed"
else
    echo "✗ rmcp E2E test FAILED"
    FAILURES=$((FAILURES + 1))
fi

# Step 3: Shell validation scripts
echo ""
echo "Step 3: Running shell validation scripts..."
if scripts/validate-elixir-a2a-docs.sh > /dev/null 2>&1; then
    echo "✓ Elixir A2A docs script passed"
else
    echo "✗ Elixir A2A docs script FAILED"
    FAILURES=$((FAILURES + 1))
fi

if scripts/validate-rmcp-docs.sh > /dev/null 2>&1; then
    echo "✓ rmcp docs script passed"
else
    echo "✗ rmcp docs script FAILED"
    FAILURES=$((FAILURES + 1))
fi

# Step 4: Benchmarks (optional — only run if --bench flag provided)
if [[ "${1:-}" == "--bench" ]]; then
    echo ""
    echo "Step 4: Running benchmarks..."
    cargo bench --bench elixir_a2a_bench
    cargo bench --bench mcp_template_bench
fi

# Step 5: Stress tests (optional — only run if --stress flag provided)
if [[ "${1:-}" == "--stress" ]]; then
    echo ""
    echo "Step 5: Running stress tests..."
    cargo test --test elixir_a2a_stress -- --ignored --test-threads=1
fi

# Summary
echo ""
echo "=== Validation Summary ==="
if [[ $FAILURES -eq 0 ]]; then
    echo "✓ All documentation validations passed!"
    exit 0
else
    echo "✗ $FAILURES validation(s) FAILED"
    exit 1
fi
