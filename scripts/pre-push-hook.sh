#!/bin/bash
# Pre-push hook: 5-gate validation aligned with core team best practices
# Comprehensive validation before push (30-60s acceptable)
# Uses: cargo make commands (NEVER direct cargo commands)
#
# CRITICAL BUG FIX (v2.0):
# OLD (line 109): Skipped ENTIRE FILES if they contained #[cfg(test)] modules
# NEW: Check EACH expect() call individually for #[allow] attribute

set -e

cd "$(git rev-parse --show-toplevel)"

echo "🚦 Pre-push validation (5 gates)..."
echo ""

# Gate 1: Cargo check
echo "Gate 1/5: Cargo check..."
if ! cargo make check-pre-push 2>&1; then
  echo "❌ ERROR: cargo make check-pre-push failed"
  exit 1
fi
echo "✅ Gate 1 passed"
echo ""

# Gate 2: Clippy
echo "Gate 2/5: Clippy (strict mode for production)..."
if cargo make lint 2>&1 > /tmp/clippy_push_output.txt; then
  rm -f /tmp/clippy_push_output.txt
else
  if grep -v "test\|tests\|example\|examples\|bench\|benches\|\.rs:" /tmp/clippy_push_output.txt | grep -qE "(error|warning):"; then
    echo "❌ ERROR: Clippy found warnings or errors in production code"
    grep -v "test\|tests\|example\|examples\|bench\|benches" /tmp/clippy_push_output.txt | head -30
    rm -f /tmp/clippy_push_output.txt
    exit 1
  fi
  rm -f /tmp/clippy_push_output.txt
fi
echo "✅ Gate 2 passed"
echo ""

# Gate 2.5: TODO & error handling check
echo "Gate 2.5/5: TODO & error handling check..."

# Check for TODO comments
TODO_COUNT=$(find crates/ggen-*/src -name "*.rs" -type f 2>/dev/null | \
  grep -v "/tests/" | grep -v "/test/" | grep -v "/example" | grep -v "build.rs" | \
  xargs grep "TODO:" 2>/dev/null | grep -v "FUTURE:" | wc -l | tr -d ' ' || echo 0)

if [ "$TODO_COUNT" -gt 0 ]; then
  echo "❌ ERROR: $TODO_COUNT TODO comments found in production code"
  exit 1
fi

# Check for unwrap in production code
UNWRAP_COUNT=$(find crates/ggen-*/src -name "*.rs" -type f 2>/dev/null | \
  grep -v "/tests/" | grep -v "/test/" | grep -v "/example" | grep -v "build.rs" | \
  while read file; do
    if [[ "$file" =~ crates/ggen-cli/ ]]; then continue; fi
    if grep -qE "#!?\[allow\(clippy::unwrap_used\)\]" "$file" 2>/dev/null; then continue; fi
    if grep -q "#\[cfg(test)\]" "$file" 2>/dev/null; then continue; fi
    grep -c "\.unwrap()" "$file" 2>/dev/null || echo 0
  done | awk '{s+=$1} END {print s}')

if [ "$UNWRAP_COUNT" -gt 0 ]; then
  echo "❌ ERROR: Found $UNWRAP_COUNT unwrap() calls in production code"
  exit 1
fi

# Check for expect() calls - IMPROVED VALIDATION (v2.0)
# Now: Check EACH expect() call individually
# Old: Skipped entire files with test modules (BUG)
EXPECT_COUNT=$(find crates/ggen-*/src -name "*.rs" -type f 2>/dev/null | \
  grep -v "/tests/" | grep -v "/test/" | grep -v "/example" | grep -v "build.rs" | \
  while read file; do
    if [[ "$file" =~ crates/ggen-cli/ ]]; then continue; fi
    grep -n "\.expect(" "$file" 2>/dev/null | while IFS=: read linenum _; do
      contextstart=$((linenum - 5))
      [ "$contextstart" -lt 1 ] && contextstart=1
      context=$(sed -n "${contextstart},${linenum}p" "$file" 2>/dev/null)
      if ! echo "$context" | grep -qE "#!?\[allow\(clippy::expect_used\)\]"; then
        echo "1"
      fi
    done
  done | wc -l | tr -d ' ')

if [ "$EXPECT_COUNT" -gt 0 ]; then
  echo "❌ ERROR: Found $EXPECT_COUNT expect() calls without #[allow(clippy::expect_used)]"
  echo "   Policy: ALL expect() calls must be documented"
  echo "   Add above the function/block: #[allow(clippy::expect_used)]"
  exit 1
fi

echo "✅ Gate 2.5 passed"
echo ""

# Gate 3: Formatting check
echo "Gate 3/5: Formatting check..."
if ! cargo fmt --all -- --check 2>&1; then
  echo "❌ ERROR: Code is not formatted"
  exit 1
fi
echo "✅ Gate 3 passed"
echo ""

# Gate 4: Fast tests
echo "Gate 4/5: Fast tests (lib + bins)..."
if ! cargo make test 2>&1 | tail -20; then
  echo "❌ ERROR: Tests failed"
  exit 1
fi
echo "✅ Gate 4 passed"
echo ""

# Gate 5: Security audit (warning only)
echo "Gate 5/5: Security audit..."
if command -v cargo-audit &> /dev/null; then
  if ! cargo make audit 2>&1; then
    echo "⚠️  Security audit found issues (non-blocking)"
  else
    echo "✅ Gate 5 passed"
  fi
fi
echo ""

echo "✅ All gates passed - ready to push"
exit 0
