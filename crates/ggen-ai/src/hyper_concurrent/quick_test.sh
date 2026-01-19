#!/bin/bash
# Quick test runner for hyper_concurrent module

set -e

cd /home/user/ggen/crates/ggen-ai

echo "Running hyper_concurrent tests..."
echo "=================================="

# Run specific test module
cargo test --lib hyper_concurrent --no-fail-fast -- --nocapture 2>&1 | tee /tmp/hyper_concurrent_test_output.txt

# Count results
echo ""
echo "Test Summary:"
echo "=================================="
grep -E "(test result:|passed)" /tmp/hyper_concurrent_test_output.txt || echo "Tests completed"

# Show any failures
if grep -q "FAILED" /tmp/hyper_concurrent_test_output.txt; then
    echo ""
    echo "FAILURES DETECTED:"
    grep -A 10 "FAILED" /tmp/hyper_concurrent_test_output.txt
    exit 1
fi

echo ""
echo "All tests passed!"
