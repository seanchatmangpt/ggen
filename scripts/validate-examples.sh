#!/usr/bin/env bash
set -euo pipefail

# scripts/validate-examples.sh
# Verify all 30+ examples run ggen sync successfully

echo "### Validating Examples"
echo ""

EXAMPLE_COUNT=$(find examples -maxdepth 1 -type d ! -name examples ! -name "_*" | wc -l | tr -d ' ')
echo "Found $EXAMPLE_COUNT example directories"
echo ""

FAILED_EXAMPLES=()

for example_dir in examples/*/; do
    if [[ ! -f "$example_dir/ggen.toml" ]]; then
        continue
    fi

    example_name=$(basename "$example_dir")
    echo -n "Testing $example_name... "

    if (cd "$example_dir" && timeout 30s ggen sync > /dev/null 2>&1); then
        echo "✅"
    else
        echo "❌"
        FAILED_EXAMPLES+=("$example_name")
    fi
done

echo ""
echo "**Results:**"
echo "- Total examples: $EXAMPLE_COUNT"
echo "- Passed: $((EXAMPLE_COUNT - ${#FAILED_EXAMPLES[@]}))"
echo "- Failed: ${#FAILED_EXAMPLES[@]}"

if [[ ${#FAILED_EXAMPLES[@]} -gt 0 ]]; then
    echo ""
    echo "**Failed examples:**"
    for ex in "${FAILED_EXAMPLES[@]}"; do
        echo "- $ex"
    done
    exit 1
fi

exit 0
