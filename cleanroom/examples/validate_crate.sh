#!/usr/bin/env bash
#
# Example: Using the cargo publish validator
#
# This script demonstrates how to use the validate-crate tool for
# pre-publish validation of Rust crates.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=========================================="
echo "  Cargo Publish Validator Demo"
echo "=========================================="
echo ""

# Example 1: Basic validation
echo "Example 1: Basic Validation"
echo "----------------------------"
echo "Command: ./bin/validate-crate"
echo ""
echo "This runs all validation checks with default settings (10s timeout)."
echo "Press Enter to run..."
read -r

if "${PROJECT_ROOT}/bin/validate-crate"; then
    echo "✅ Validation passed!"
else
    echo "❌ Validation failed (this is expected if there are issues)"
fi

echo ""
echo "Press Enter to continue..."
read -r

# Example 2: Verbose output
echo ""
echo "Example 2: Verbose Validation"
echo "------------------------------"
echo "Command: ./bin/validate-crate -v"
echo ""
echo "This shows detailed output of each validation step."
echo "Press Enter to run..."
read -r

if "${PROJECT_ROOT}/bin/validate-crate" -v -t 15; then
    echo "✅ Verbose validation completed!"
else
    echo "❌ Validation failed with detailed output"
fi

echo ""
echo "Press Enter to continue..."
read -r

# Example 3: Custom output location
echo ""
echo "Example 3: Custom Report Location"
echo "----------------------------------"
echo "Command: ./bin/validate-crate -o /tmp/my-validation.json"
echo ""
echo "This saves the validation report to a specific location."
echo "Press Enter to run..."
read -r

REPORT_PATH="/tmp/cleanroom-validation-demo.json"
if "${PROJECT_ROOT}/bin/validate-crate" -o "$REPORT_PATH" -t 15; then
    echo "✅ Validation report generated!"
    if [ -f "$REPORT_PATH" ]; then
        echo ""
        echo "Report contents:"
        cat "$REPORT_PATH" | python3 -m json.tool || cat "$REPORT_PATH"
    fi
else
    echo "❌ Validation failed (report may still be generated)"
    if [ -f "$REPORT_PATH" ]; then
        echo ""
        echo "Report contents:"
        cat "$REPORT_PATH" | python3 -m json.tool || cat "$REPORT_PATH"
    fi
fi

echo ""
echo "Press Enter to continue..."
read -r

# Example 4: Checking validation results programmatically
echo ""
echo "Example 4: Programmatic Validation Check"
echo "----------------------------------------"
echo "This shows how to use validation results in scripts."
echo ""

REPORT_PATH="/tmp/cleanroom-validation-check.json"
"${PROJECT_ROOT}/bin/validate-crate" -o "$REPORT_PATH" -t 15 || true

if [ -f "$REPORT_PATH" ]; then
    # Check if ready to publish using jq
    if command -v jq >/dev/null 2>&1; then
        READY=$(jq -r '.ready_to_publish' "$REPORT_PATH")
        CHECKS_PASSED=$(jq -r '.checks.passed' "$REPORT_PATH")
        CHECKS_FAILED=$(jq -r '.checks.failed' "$REPORT_PATH")
        WARNINGS=$(jq -r '.checks.warnings' "$REPORT_PATH")
        DURATION=$(jq -r '.duration_seconds' "$REPORT_PATH")

        echo "Validation Results:"
        echo "  Ready to publish: $READY"
        echo "  Checks passed:    $CHECKS_PASSED"
        echo "  Checks failed:    $CHECKS_FAILED"
        echo "  Warnings:         $WARNINGS"
        echo "  Duration:         ${DURATION}s"

        if [ "$READY" = "true" ]; then
            echo ""
            echo "✅ Crate is ready for publishing!"
            echo ""
            echo "Next steps:"
            echo "  1. cargo publish --dry-run  # Final pre-publish check"
            echo "  2. cargo publish             # Publish to crates.io"
        else
            echo ""
            echo "❌ Crate is not ready for publishing"
            echo ""
            echo "Fix the issues above and re-run validation."
        fi
    else
        echo "jq not installed - showing raw report:"
        cat "$REPORT_PATH"
    fi
else
    echo "⚠️  Validation report not found"
fi

echo ""
echo "Press Enter to continue..."
read -r

# Example 5: Integration with CI/CD
echo ""
echo "Example 5: CI/CD Integration"
echo "---------------------------"
echo "This shows how to use the validator in automated workflows."
echo ""

cat <<'EOF'
GitHub Actions example:
```yaml
- name: Validate crate
  run: |
    chmod +x bin/validate-crate
    ./bin/validate-crate -v
```

GitLab CI example:
```yaml
validate:
  script:
    - chmod +x bin/validate-crate
    - ./bin/validate-crate -v
```

Pre-commit hook example:
```bash
#!/bin/bash
./bin/validate-crate || exit 1
```
EOF

echo ""
echo "Press Enter to finish..."
read -r

# Cleanup
echo ""
echo "Cleaning up temporary files..."
rm -f /tmp/cleanroom-validation-*.json

echo ""
echo "=========================================="
echo "  Demo Complete!"
echo "=========================================="
echo ""
echo "Key takeaways:"
echo "  1. Fast validation (<10s) for quick feedback"
echo "  2. Comprehensive checks (format, lint, test)"
echo "  3. JSON reports for automation"
echo "  4. CI/CD integration ready"
echo "  5. Hermetic testing with cleanroom"
echo ""
echo "For more information, see:"
echo "  - docs/validation.md"
echo "  - bin/README.md"
echo "  - bin/validate-crate --help"
