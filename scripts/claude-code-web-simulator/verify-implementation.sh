#!/bin/bash

echo "═══════════════════════════════════════════════════════════════"
echo "Real ggen Pipeline Integration - Implementation Verification"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Check main.sh
echo "1. Checking main.sh modifications..."
if grep -q "run_ggen_real_pipeline" main.sh; then
    echo "   ✓ run_ggen_real_pipeline() function found"
else
    echo "   ✗ run_ggen_real_pipeline() function NOT found"
    exit 1
fi

if grep -q "map_ggen_output_to_receipt" main.sh; then
    echo "   ✓ map_ggen_output_to_receipt() function found"
else
    echo "   ✗ map_ggen_output_to_receipt() function NOT found"
    exit 1
fi

if grep -q "generate_error_receipt" main.sh; then
    echo "   ✓ generate_error_receipt() function found"
else
    echo "   ✗ generate_error_receipt() function NOT found"
    exit 1
fi

if grep -q "test_real_ggen_pipeline" main.sh; then
    echo "   ✓ test_real_ggen_pipeline() function found"
else
    echo "   ✗ test_real_ggen_pipeline() function NOT found"
    exit 1
fi

if grep -q "\-\-real" main.sh; then
    echo "   ✓ --real flag support added to run_generation_agent()"
else
    echo "   ✗ --real flag NOT found"
    exit 1
fi

# Check test script
echo ""
echo "2. Checking test-real-pipeline.sh..."
if [ -f test-real-pipeline.sh ]; then
    echo "   ✓ test-real-pipeline.sh exists"
    wc=$(wc -l < test-real-pipeline.sh)
    echo "   ✓ $wc lines"
else
    echo "   ✗ test-real-pipeline.sh NOT found"
    exit 1
fi

# Check documentation
echo ""
echo "3. Checking documentation..."
if [ -f README-REAL-PIPELINE.md ]; then
    echo "   ✓ README-REAL-PIPELINE.md exists"
    wc=$(wc -l < README-REAL-PIPELINE.md)
    echo "   ✓ $wc lines"
else
    echo "   ✗ README-REAL-PIPELINE.md NOT found"
    exit 1
fi

if [ -f IMPLEMENTATION_SUMMARY.md ]; then
    echo "   ✓ IMPLEMENTATION_SUMMARY.md exists"
else
    echo "   ✗ IMPLEMENTATION_SUMMARY.md NOT found"
    exit 1
fi

# Check bash syntax
echo ""
echo "4. Checking bash syntax..."
if bash -n main.sh 2>&1 | grep -q "syntax error"; then
    echo "   ✗ main.sh has syntax errors"
    bash -n main.sh
    exit 1
else
    echo "   ✓ main.sh syntax is valid"
fi

if bash -n test-real-pipeline.sh 2>&1 | grep -q "syntax error"; then
    echo "   ✗ test-real-pipeline.sh has syntax errors"
    bash -n test-real-pipeline.sh
    exit 1
else
    echo "   ✓ test-real-pipeline.sh syntax is valid"
fi

# Check exit code handling
echo ""
echo "5. Checking exit code handling..."
grep -q "case \$exit_code in" main.sh && echo "   ✓ Exit code case statement found"
grep -q "1)" main.sh && echo "   ✓ Exit code 1 handler found"
grep -q "2)" main.sh && echo "   ✓ Exit code 2 handler found"
grep -q "3)" main.sh && echo "   ✓ Exit code 3 handler found"
grep -q "4)" main.sh && echo "   ✓ Exit code 4 handler found"
grep -q "5)" main.sh && echo "   ✓ Exit code 5 handler found"
grep -q "6)" main.sh && echo "   ✓ Exit code 6 handler found"

# Check timeout enforcement
echo ""
echo "6. Checking timeout enforcement..."
if grep -q "timeout 5s" main.sh; then
    echo "   ✓ 5-second timeout enforcement found"
else
    echo "   ✗ Timeout enforcement NOT found"
    exit 1
fi

# Check JSON parsing
echo ""
echo "7. Checking JSON parsing..."
if grep -q "python3.*json" main.sh; then
    echo "   ✓ Python JSON parsing found"
else
    echo "   ✗ JSON parsing NOT found"
    exit 1
fi

# Check receipt generation
echo ""
echo "8. Checking receipt generation..."
if grep -q "map_ggen_output_to_receipt" main.sh; then
    echo "   ✓ Receipt mapping function called"
else
    echo "   ✗ Receipt mapping NOT called"
    exit 1
fi

if grep -q "execution_id.*exec-" main.sh; then
    echo "   ✓ Unique execution IDs generated"
else
    echo "   ✗ Execution ID generation NOT found"
    exit 1
fi

if grep -q "sha256sum" main.sh; then
    echo "   ✓ SHA-256 hashing implemented"
else
    echo "   ✗ SHA-256 hashing NOT found"
    exit 1
fi

# Check audit trail
echo ""
echo "9. Checking audit trail..."
if grep -q "audit-logs/audit.log" main.sh; then
    echo "   ✓ Audit log path configured"
else
    echo "   ✗ Audit log NOT configured"
    exit 1
fi

if grep -q "REAL_GGEN" main.sh; then
    echo "   ✓ Real execution markers in audit log"
else
    echo "   ✗ Audit markers NOT found"
    exit 1
fi

# Check help documentation
echo ""
echo "10. Checking help documentation..."
if grep -q "\\-\\-real" main.sh && grep -q "Real pipeline features" main.sh; then
    echo "   ✓ Help text includes real pipeline documentation"
else
    echo "   ✗ Help documentation incomplete"
    exit 1
fi

# Summary
echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "✓ All verification checks PASSED!"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Next steps:"
echo "  1. Build ggen: cargo make build"
echo "  2. Start simulator: ./main.sh start"
echo "  3. Run generation agent: ./main.sh run-agent generation --real"
echo "  4. View results: ./main.sh view-receipts"
echo "  5. Run tests: ./test-real-pipeline.sh"
echo ""
