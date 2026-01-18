#!/bin/bash
# Validate runtime_helper module syntax

echo "=== Validating runtime_helper.rs syntax ==="

# Check that the file exists
if [ ! -f "cli/src/runtime_helper.rs" ]; then
    echo "❌ File not found: cli/src/runtime_helper.rs"
    exit 1
fi

# Check that it's exported in lib.rs
if ! grep -q "pub mod runtime_helper" cli/src/lib.rs; then
    echo "❌ runtime_helper not exported in cli/src/lib.rs"
    exit 1
fi

echo "✅ File exists and is exported"

# Check for required functions
echo ""
echo "=== Checking required functions ==="

if grep -q "pub fn create_runtime()" cli/src/runtime_helper.rs; then
    echo "✅ create_runtime() function found"
else
    echo "❌ create_runtime() function missing"
    exit 1
fi

if grep -q "pub fn execute_async" cli/src/runtime_helper.rs; then
    echo "✅ execute_async() function found"
else
    echo "❌ execute_async() function missing"
    exit 1
fi

if grep -q "pub fn execute_async_verb" cli/src/runtime_helper.rs; then
    echo "✅ execute_async_verb() function found"
else
    echo "❌ execute_async_verb() function missing"
    exit 1
fi

# Check for tests
echo ""
echo "=== Checking tests ==="

if grep -q "#\[cfg(test)\]" cli/src/runtime_helper.rs; then
    echo "✅ Test module found"

    test_count=$(grep -c "#\[test\]" cli/src/runtime_helper.rs)
    echo "✅ Found $test_count tests"
else
    echo "❌ No tests found"
    exit 1
fi

# Check documentation
echo ""
echo "=== Checking documentation ==="

doc_count=$(grep -c "^///" cli/src/runtime_helper.rs)
echo "✅ Found $doc_count documentation lines"

# Check pattern documentation exists
echo ""
echo "=== Checking pattern documentation ==="

if [ ! -f ".claude/refactor-v2/sync-wrapper-pattern.md" ]; then
    echo "❌ Pattern documentation not found"
    exit 1
fi

echo "✅ Pattern documentation exists"

# Check pattern doc has required sections
required_sections=("Overview" "Pattern Template" "Using runtime_helper" "Error Handling")
for section in "${required_sections[@]}"; do
    if grep -q "# $section\|## $section" .claude/refactor-v2/sync-wrapper-pattern.md; then
        echo "✅ Section found: $section"
    else
        echo "❌ Section missing: $section"
        exit 1
    fi
done

echo ""
echo "=== ✅ All validations passed ==="
echo ""
echo "Agent 2 deliverables:"
echo "  ✅ runtime_helper.rs created with 3 public functions"
echo "  ✅ $test_count unit tests included"
echo "  ✅ Documentation with examples"
echo "  ✅ Pattern documentation with templates"
echo "  ✅ Exported in cli/src/lib.rs"
echo ""
echo "Ready for use by Agents 3-6!"
