#!/usr/bin/env bash
set -euo pipefail

# scripts/validate-docs-runner.sh
# Main entry point for documentation validation

echo "# ggen Documentation Validation Report"
echo "**Generated:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo ""

FAILED=0
PASSED=0

# Run all validation sub-scripts
for script in scripts/validate-*.sh; do
    if [[ "$script" == *"validate-docs-runner.sh" ]]; then
        continue
    fi

    echo "## Running $(basename "$script")"
    if bash "$script"; then
        ((PASSED++))
        echo "✅ PASSED"
    else
        ((FAILED++))
        echo "❌ FAILED"
    fi
    echo ""
done

echo "## Summary"
echo "- **Passed:** $PASSED"
echo "- **Failed:** $FAILED"
echo ""

if [[ $FAILED -gt 0 ]]; then
    echo "❌ VALIDATION FAILED"
    exit 1
else
    echo "✅ ALL VALIDATIONS PASSED"
    exit 0
fi
