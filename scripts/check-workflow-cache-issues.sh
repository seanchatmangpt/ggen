#!/usr/bin/env bash
# Check all workflows for potential cache lock contention issues

set -uo pipefail

echo "🔍 Analyzing GitHub Actions workflows for cache lock contention issues..."
echo ""

issues_found=0
workflows_checked=0

check_workflow() {
    local workflow_file="$1"
    local has_concurrency=false
    local has_target_cache=false
    local has_static_key=false
    local uses_v3=false
    
    workflows_checked=$((workflows_checked + 1))
    
    # Check for concurrency control
    if grep -q "^concurrency:" "$workflow_file"; then
        has_concurrency=true
    fi
    
    # Check for target directory caching
    if grep -q "path:.*target" "$workflow_file" || grep -q "path: target" "$workflow_file"; then
        has_target_cache=true
    fi
    
    # Check for static cache keys (not using hashFiles)
    if grep -q "key:.*target" "$workflow_file" && ! grep -q "hashFiles" "$workflow_file"; then
        has_static_key=true
    fi
    
    # Check for outdated cache action
    if grep -q "actions/cache@v3" "$workflow_file"; then
        uses_v3=true
    fi
    
    # Report issues
    local issues=0
    if [ "$has_target_cache" = true ] && [ "$has_concurrency" = false ]; then
        echo "⚠️  $workflow_file: Missing concurrency control (caches target directory)"
        issues=$((issues + 1))
    fi
    
    if [ "$has_static_key" = true ]; then
        echo "⚠️  $workflow_file: Uses static cache key (should use hashFiles)"
        issues=$((issues + 1))
    fi
    
    if [ "$uses_v3" = true ]; then
        echo "⚠️  $workflow_file: Uses outdated cache@v3 (should use v4)"
        issues=$((issues + 1))
    fi
    
    if [ $issues -eq 0 ] && [ "$has_target_cache" = true ]; then
        echo "✅ $workflow_file: OK (has concurrency control and dynamic cache keys)"
    fi
    
    issues_found=$((issues_found + issues))
}

# Check all workflow files
for workflow in .github/workflows/*.yml; do
    if [ -f "$workflow" ]; then
        check_workflow "$workflow"
    fi
done

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Summary:"
echo "  Workflows checked: $workflows_checked"
echo "  Issues found: $issues_found"
echo ""

if [ $issues_found -eq 0 ]; then
    echo "✅ All workflows look good!"
    exit 0
else
    echo "⚠️  Some workflows may have lock contention issues"
    exit 1
fi




