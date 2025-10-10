#!/bin/bash

# Verify mdBook structure is complete and valid

echo "🔍 GGen Cookbook 2nd Edition - Structure Verification"
echo "======================================================"
echo ""

cd "$(dirname "$0")/.." || exit 1

# Check required files
echo "✓ Checking required files..."
required_files=(
    "book.toml"
    "README.md"
    ".gitignore"
    "src/SUMMARY.md"
    "src/preface.md"
    "src/introduction.md"
)

missing=0
for file in "${required_files[@]}"; do
    if [[ -f "$file" ]]; then
        echo "  ✓ $file"
    else
        echo "  ✗ MISSING: $file"
        ((missing++))
    fi
done

echo ""

# Count markdown files
echo "📊 File Statistics:"
total_md=$(find src -name "*.md" | wc -l | tr -d ' ')
echo "  Total markdown files: $total_md"

part1=$(find src/part-1 -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
part2=$(find src/part-2 -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
part3=$(find src/part-3 -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
part4=$(find src/part-4 -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
part5=$(find src/part-5 -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
part6=$(find src/part-6 -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
appendices=$(find src/appendices -name "*.md" 2>/dev/null | wc -l | tr -d ' ')

echo "  Part I (Foundation): $part1 files"
echo "  Part II (Core Engine): $part2 files"
echo "  Part III (Authoring): $part3 files"
echo "  Part IV (Autonomic): $part4 files"
echo "  Part V (Ecosystem): $part5 files"
echo "  Part VI (Advanced): $part6 files"
echo "  Appendices: $appendices files"

echo ""

# Test mdBook build
echo "🔨 Testing mdBook build..."
if command -v mdbook &> /dev/null; then
    if mdbook build &> /dev/null; then
        echo "  ✓ mdBook build successful"

        if [[ -d "book/html" ]]; then
            html_files=$(find book/html -name "*.html" | wc -l | tr -d ' ')
            echo "  ✓ Generated $html_files HTML files"
        fi
    else
        echo "  ✗ mdBook build failed"
        ((missing++))
    fi
else
    echo "  ⚠ mdBook not installed (run: cargo install mdbook)"
fi

echo ""

# Verify SUMMARY.md structure
echo "📚 Verifying SUMMARY.md structure..."
if grep -q "Part I: The Foundation" src/SUMMARY.md && \
   grep -q "Part II: Core Engine & CLI" src/SUMMARY.md && \
   grep -q "Part III: Authoring Language" src/SUMMARY.md && \
   grep -q "Part IV: Autonomic System" src/SUMMARY.md && \
   grep -q "Part V: The Ecosystem" src/SUMMARY.md && \
   grep -q "Part VI: Advanced & Enterprise" src/SUMMARY.md; then
    echo "  ✓ All 6 parts present in SUMMARY.md"
else
    echo "  ✗ Missing parts in SUMMARY.md"
    ((missing++))
fi

echo ""

# Final result
if [[ $missing -eq 0 ]]; then
    echo "✅ Structure verification PASSED"
    echo ""
    echo "Next steps:"
    echo "  1. Run 'mdbook serve' to view locally"
    echo "  2. Begin content authoring in src/"
    echo "  3. Add code examples and diagrams"
    exit 0
else
    echo "❌ Structure verification FAILED ($missing issues)"
    exit 1
fi
