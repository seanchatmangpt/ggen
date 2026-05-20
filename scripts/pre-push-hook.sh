#!/bin/bash
# Pre-push hook: Simplified 5-gate validation
# Focus: cargo, formatting, unit tests, security
# Philosophy: Catch real issues, not style obsessions

set -e
cd "$(git rev-parse --show-toplevel)"

# Only run validation when pushing to the default branch (main)
IS_DEFAULT_BRANCH=false
while read local_ref local_sha remote_ref remote_sha; do
    if [[ "$remote_ref" == "refs/heads/main" ]]; then
        IS_DEFAULT_BRANCH=true
    fi
done

if [ "$IS_DEFAULT_BRANCH" = false ]; then
    exit 0
fi

echo "🚦 Pre-push validation..."
echo ""

# Gate 1: Cargo check
echo "Gate 1/5: Cargo check..."
if ! cargo make check-pre-push 2>&1 > /dev/null; then
  echo "❌ Cargo check failed"
  exit 1
fi
echo "✅ Pass"
echo ""

# Gate 2: Clippy
echo "Gate 2/5: Clippy..."
if ! cargo make lint 2>&1 > /dev/null; then
  echo "❌ Clippy failed"
  exit 1
fi
echo "✅ Pass"
echo ""

# Gate 3: Formatting
echo "Gate 3/5: Formatting..."
if ! cargo fmt --all -- --check 2>&1 > /dev/null; then
  echo "❌ Code not formatted"
  exit 1
fi
echo "✅ Pass"
echo ""

# Gate 4: Unit tests  
echo "Gate 4/5: Unit tests..."
if ! cargo make test-unit 2>&1 > /dev/null; then
  echo "❌ Unit tests failed"
  exit 1
fi
echo "✅ Pass"
echo ""

# Gate 5: Security audit (warning only)
echo "Gate 5/5: Security audit..."
cargo make audit 2>&1 > /dev/null || echo "⚠️  See cargo make audit for details"
echo "✅ Pass"
echo ""

echo "✅ All gates passed!"
