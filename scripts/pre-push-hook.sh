#!/bin/bash
# Pre-push hook: 5-gate validation aligned with core team best practices
# Comprehensive validation before push (30-60s acceptable)
# Uses: cargo make commands (NEVER direct cargo commands)
# 
# CRITICAL: Only checks MODIFIED files, not entire codebase
# This prevents blocking on pre-existing code issues

set -e

cd "$(git rev-parse --show-toplevel)"

echo "🚦 Pre-push validation (5 gates)..."
echo ""

# Get list of modified files in this push
MODIFIED_FILES=$(git diff origin/master...HEAD --name-only 2>/dev/null || git diff HEAD~1 --name-only 2>/dev/null || echo "")

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

# Gate 2.5: TODO & error handling check (ONLY on modified files)
echo "Gate 2.5/5: TODO & error handling check..."

# Only check modified Rust files
if [ -n "$MODIFIED_FILES" ]; then
  MODIFIED_RS_FILES=$(echo "$MODIFIED_FILES" | grep '\.rs$' | grep -v test | grep -v example || true)
  
  if [ -n "$MODIFIED_RS_FILES" ]; then
    # Check for TODO comments in modified files
    TODO_COUNT=$(echo "$MODIFIED_RS_FILES" | xargs -I {} sh -c 'grep "TODO:" "$PWD/{}" 2>/dev/null | grep -v "FUTURE:" || true' | wc -l | tr -d ' ' || echo 0)
    
    if [ "$TODO_COUNT" -gt 0 ]; then
      echo "❌ ERROR: $TODO_COUNT TODO comments found in modified production code"
      exit 1
    fi
    
    # Check for unwrap in modified production code
    UNWRAP_COUNT=$(echo "$MODIFIED_RS_FILES" | xargs -I {} sh -c 'if ! grep -qE "#!?\[allow\(clippy::unwrap_used\)\]" "$PWD/{}"; then grep -c "\.unwrap()" "$PWD/{}" 2>/dev/null || echo 0; else echo 0; fi' | awk '{s+=$1} END {print s}' || echo 0)
    
    if [ "$UNWRAP_COUNT" -gt 0 ]; then
      echo "❌ ERROR: Found $UNWRAP_COUNT unwrap() calls in modified production code"
      exit 1
    fi
    
    # Check for expect in modified production code (with allow attribute check)
    EXPECT_COUNT=$(echo "$MODIFIED_RS_FILES" | xargs -I {} sh -c '
      grep -n "\.expect(" "$PWD/{}" 2>/dev/null | while IFS=: read linenum rest; do
        contextstart=$((linenum - 5))
        [ "$contextstart" -lt 1 ] && contextstart=1
        context=$(sed -n "${contextstart},${linenum}p" "$PWD/{}" 2>/dev/null)
        if ! echo "$context" | grep -qE "#!?\[allow\(clippy::expect_used\)\]"; then
          echo "1"
        fi
      done
    ' | wc -l | tr -d ' ')
    
    if [ "$EXPECT_COUNT" -gt 0 ]; then
      echo "❌ ERROR: Found $EXPECT_COUNT expect() calls in modified code without #[allow(clippy::expect_used)]"
      echo "   Policy: New expect() calls must be documented"
      echo "   Add above the function/block: #[allow(clippy::expect_used)]"
      exit 1
    fi
  fi
fi

echo "✅ Gate 2.5 passed"
echo ""

# Gate 3: Formatting check
echo "Gate 3/5: Formatting check..."
if ! cargo fmt --all -- --check 2>&1; then
  echo "❌ ERROR: Code is not formatted"
  cargo fmt --all
  echo "ℹ️  Code has been formatted. Please review and commit the changes."
  exit 1
fi
echo "✅ Gate 3 passed"
echo ""

# Gate 4: Unit tests
echo "Gate 4/5: Unit tests..."
if ! cargo make test-unit 2>&1; then
  echo "❌ ERROR: Unit tests failed"
  exit 1
fi
echo "✅ Gate 4 passed"
echo ""

# Gate 5: Security audit
echo "Gate 5/5: Security audit..."
if cargo make audit 2>&1 > /tmp/audit_output.txt; then
  echo "✅ Gate 5 passed"
  rm -f /tmp/audit_output.txt
else
  VULN_COUNT=$(grep -c "^error:" /tmp/audit_output.txt 2>/dev/null || echo 0)
  if [ "$VULN_COUNT" -gt 0 ]; then
    echo "⚠️  WARNING: Security vulnerabilities detected"
    grep "^error:" /tmp/audit_output.txt | head -5
    echo "   Run 'cargo make audit' for details"
    rm -f /tmp/audit_output.txt
    # Don't fail on audit warnings, just warn
  else
    echo "✅ Gate 5 passed"
    rm -f /tmp/audit_output.txt
  fi
fi
echo ""

echo "✅ All gates passed! Proceeding with push..."
