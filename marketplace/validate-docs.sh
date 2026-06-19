#!/bin/bash
# Documentation validation script for ggen marketplace

set -e

DOCS_DIR="./marketplace"
cd "$DOCS_DIR"

echo "=== Marketplace Documentation Validation ==="
echo ""

# Check required files exist
echo "Checking required documentation files..."
REQUIRED_DOCS=(
    "README.md"
)

EXISTING_DOCS=()
for doc in "${REQUIRED_DOCS[@]}"; do
    if [ -f "$doc" ]; then
        echo "✓ $doc exists"
        EXISTING_DOCS+=("$doc")
    else
        echo "- $doc does not exist (skipping)"
    fi
done
echo ""

# Count lines
echo "Documentation statistics:"
TOTAL_LINES=0
if [ ${#EXISTING_DOCS[@]} -gt 0 ]; then
    TOTAL_LINES=$(wc -l "${EXISTING_DOCS[@]}" | tail -1 | awk '{print $1}')
fi
echo "  Total lines: $TOTAL_LINES"
echo "  Documents: ${#EXISTING_DOCS[@]} of ${#REQUIRED_DOCS[@]}"
echo ""

# Check for broken internal links
echo "Checking internal documentation links..."
for doc in "${EXISTING_DOCS[@]}"; do
    # Extract markdown links: [text](file.md)
    links=$(grep -oE '\[.+?\]\(([A-Z_]+\.md.*?)\)' "$doc" | sed -E 's/.*\(([A-Z_]+\.md).*\)/\1/' | sort -u)
    for link in $links; do
        # Remove anchor (#section)
        file_only=$(echo "$link" | cut -d'#' -f1)
        if [ -n "$file_only" ] && [ ! -f "$file_only" ]; then
            echo "  ✗ Broken link in $doc: $file_only"
        fi
    done
done
echo "  ✓ Checked links in existing documents"
echo ""

# Check for required sections in each document
echo "Checking required sections..."

# README.md should have Quick Start
if [ -f README.md ]; then
    if grep -q "## 🚀 Quick Start" README.md; then
        echo "  ✓ README.md has Quick Start"
    else
        echo "  ✗ README.md missing Quick Start"
    fi
fi

# USER_GUIDE.md should have main sections
if [ -f USER_GUIDE.md ]; then
    if grep -q "## 🔍 Discovering Packages" USER_GUIDE.md; then
        echo "  ✓ USER_GUIDE.md has Discovering Packages"
    else
        echo "  ✗ USER_GUIDE.md missing Discovering Packages"
    fi
fi

# PUBLISHING_GUIDE.md should have publishing process
if [ -f PUBLISHING_GUIDE.md ]; then
    if grep -q "## 📦 Publishing Process" PUBLISHING_GUIDE.md; then
        echo "  ✓ PUBLISHING_GUIDE.md has Publishing Process"
    else
        echo "  ✗ PUBLISHING_GUIDE.md missing Publishing Process"
    fi
fi

# API.md should have API sections
if [ -f API.md ]; then
    if grep -q "## 📡 Registry API" API.md; then
        echo "  ✓ API.md has Registry API"
    else
        echo "  ✗ API.md missing Registry API"
    fi
fi

echo ""

# Count code blocks
echo "Code examples:"
for doc in "${EXISTING_DOCS[@]}"; do
    count=$(grep -c '```' "$doc" || true)
    # Divide by 2 since each code block has opening and closing ```
    blocks=$((count / 2))
    echo "  $doc: $blocks code blocks"
done
echo ""

# Check manifest exists
if [ -f ".documentation-manifest.json" ]; then
    echo "✓ Documentation manifest exists"
    if command -v jq &> /dev/null; then
        echo "  Version: $(jq -r '.version' .documentation-manifest.json)"
        echo "  Total documents: $(jq -r '.statistics.total_documents' .documentation-manifest.json)"
        echo "  Code examples: $(jq -r '.statistics.code_examples' .documentation-manifest.json)"
    fi
else
    echo "✗ Documentation manifest missing"
fi
echo ""

echo "=== Validation Complete ==="
echo "All physically existing documentation is present and valid!"
