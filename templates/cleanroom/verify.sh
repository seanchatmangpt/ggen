#!/usr/bin/env bash
# Verification script for cleanroom gpack templates

set -e

echo "🔍 Verifying Cleanroom Gpack Templates"
echo "========================================"
echo ""

# Check template files exist
echo "📋 Checking template files..."
TEMPLATES=(
    "test-environment.tmpl"
    "postgres-container.tmpl"
    "redis-container.tmpl"
    "swarm-coordinator.tmpl"
    "ci-pipeline.tmpl"
    "benchmark-suite.tmpl"
)

for tmpl in "${TEMPLATES[@]}"; do
    if [ -f "$tmpl" ]; then
        echo "  ✅ $tmpl"
    else
        echo "  ❌ $tmpl (missing)"
        exit 1
    fi
done

# Check manifest exists
echo ""
echo "📦 Checking gpack manifest..."
if [ -f "gpack.toml" ]; then
    echo "  ✅ gpack.toml"
else
    echo "  ❌ gpack.toml (missing)"
    exit 1
fi

# Check documentation exists
echo ""
echo "📚 Checking documentation..."
if [ -f "README.md" ]; then
    echo "  ✅ README.md"
else
    echo "  ❌ README.md (missing)"
    exit 1
fi

if [ -f "EXAMPLES.md" ]; then
    echo "  ✅ EXAMPLES.md"
else
    echo "  ❌ EXAMPLES.md (missing)"
    exit 1
fi

# Check YAML frontmatter in templates
echo ""
echo "🔧 Validating template frontmatter..."
for tmpl in "${TEMPLATES[@]}"; do
    if head -n 1 "$tmpl" | grep -q "^---$"; then
        echo "  ✅ $tmpl has valid frontmatter"
    else
        echo "  ❌ $tmpl missing frontmatter"
        exit 1
    fi
done

# Count total lines
echo ""
echo "📊 Template Statistics"
echo "---------------------"
for tmpl in "${TEMPLATES[@]}"; do
    lines=$(wc -l < "$tmpl")
    echo "  $tmpl: $lines lines"
done

total_lines=$(cat "${TEMPLATES[@]}" gpack.toml README.md EXAMPLES.md 2>/dev/null | wc -l)
echo ""
echo "  Total: $total_lines lines"

echo ""
echo "✅ All verification checks passed!"
echo "🚀 Cleanroom gpack templates are ready for use!"
