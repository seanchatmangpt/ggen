#!/usr/bin/env bash
set -euo pipefail

# scripts/validate-readme-claims.sh
# Verify specific claims made in README.md

echo "### Validating README.md Claims"
echo ""

FAILED=0

# Claim 1: "30+ Examples"
echo "#### Claim: '30+ Examples'"
EXAMPLE_COUNT=$(find examples -maxdepth 1 -type d ! -name examples ! -name "_*" | wc -l | tr -d ' ')
if [[ $EXAMPLE_COUNT -ge 30 ]]; then
    echo "✅ PASSED: $EXAMPLE_COUNT examples found"
else
    echo "❌ FAILED: only $EXAMPLE_COUNT examples (claim: 30+)"
    ((FAILED++))
fi

# Claim 2: "cargo make test green"
echo ""
echo "#### Claim: 'cargo make test green'"
if timeout 60s cargo test --workspace --tests --lib 2>&1 | grep -q "test result: ok"; then
    echo "✅ PASSED: tests pass"
else
    echo "❌ FAILED: tests do not pass"
    ((FAILED++))
fi

# Claim 3: Feature bullet points exist
echo ""
echo "#### Claim: Feature bullet points documented"
FEATURES=(
    "Elixir A2A"
    "MCP Server"
    "Protocol Integration"
    "OTel Weaver"
)

for feature in "${FEATURES[@]}"; do
    if grep -q "$feature" README.md; then
        echo "✅ '$feature' documented"
    else
        echo "❌ '$feature' NOT documented"
        ((FAILED++))
    fi
done

# Claim 4: Documentation links exist
echo ""
echo "#### Claim: Documentation links exist"
DOCS=(
    "docs/ELIXIR_A2A_NOTES.md"
    "docs/RMCP_NOTES.md"
    "examples/README.md"
)

for doc in "${DOCS[@]}"; do
    if [[ -f "$doc" ]]; then
        echo "✅ $doc exists"
    else
        echo "❌ $doc MISSING"
        ((FAILED++))
    fi
done

echo ""
if [[ $FAILED -gt 0 ]]; then
    echo "**README Claims:** ❌ $FAILED failed"
    exit 1
else
    echo "**README Claims:** ✅ All verified"
    exit 0
fi
