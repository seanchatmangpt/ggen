#!/usr/bin/env bash
#
# TypeScript Detection Script
#
# Validates that documentation uses JavaScript + JSDoc (NOT TypeScript)
# Per project requirement: "we don't do typescript, do javascript, zod, jsdoc"

set -e
set -u

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Counters
TOTAL_FILES=0
VIOLATIONS=0

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

log_error() {
    echo -e "${RED}✗${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "TypeScript Detection in Documentation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Find all markdown files in docs
DOCS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)/docs"

if [ ! -d "$DOCS_DIR" ]; then
    log_error "Documentation directory not found: $DOCS_DIR"
    exit 1
fi

# Check for TypeScript usage patterns
echo "Scanning documentation for TypeScript patterns..."
echo ""

# Pattern 1: TypeScript code blocks (```typescript)
echo "Checking for TypeScript code blocks..."
TYPESCRIPT_BLOCKS=$(find "$DOCS_DIR" -name "*.md" -exec grep -l '```typescript' {} \; 2>/dev/null || true)

if [ -n "$TYPESCRIPT_BLOCKS" ]; then
    echo ""
    log_error "Found TypeScript code blocks in:"
    echo "$TYPESCRIPT_BLOCKS" | while read -r file; do
        VIOLATIONS=$((VIOLATIONS + 1))
        line_num=$(grep -n '```typescript' "$file" | cut -d: -f1 | head -1)
        echo "  - $file:$line_num"
    done
else
    log_success "No TypeScript code blocks found"
fi

# Pattern 2: TypeScript interface/type keywords
echo ""
echo "Checking for TypeScript interface/type keywords..."
INTERFACE_FILES=$(find "$DOCS_DIR" -name "*.md" -exec grep -l 'interface.*{' {} \; 2>/dev/null || true)

if [ -n "$INTERFACE_FILES" ]; then
    echo ""
    log_warning "Found 'interface' keyword in:"
    echo "$INTERFACE_FILES" | while read -r file; do
        # Check if it's in a code block (not just prose)
        if grep -A5 'interface.*{' "$file" | grep -q '^```'; then
            VIOLATIONS=$((VIOLATIONS + 1))
            line_num=$(grep -n 'interface.*{' "$file" | cut -d: -f1 | head -1)
            echo "  - $file:$line_num"
        fi
    done
else
    log_success "No TypeScript interfaces found"
fi

# Pattern 3: TypeScript import type
echo ""
echo "Checking for TypeScript 'import type' statements..."
IMPORT_TYPE_FILES=$(find "$DOCS_DIR" -name "*.md" -exec grep -l 'import type' {} \; 2>/dev/null || true)

if [ -n "$IMPORT_TYPE_FILES" ]; then
    echo ""
    log_error "Found 'import type' statements in:"
    echo "$IMPORT_TYPE_FILES" | while read -r file; do
        VIOLATIONS=$((VIOLATIONS + 1))
        line_num=$(grep -n 'import type' "$file" | cut -d: -f1 | head -1)
        echo "  - $file:$line_num"
    done
else
    log_success "No 'import type' statements found"
fi

# Pattern 4: TypeScript type annotations (: Type)
echo ""
echo "Checking for TypeScript type annotations..."
TYPE_ANNOTATION_FILES=$(find "$DOCS_DIR" -name "*.md" -exec grep -l ': \(string\|number\|boolean\|Promise\)' {} \; 2>/dev/null || true)

if [ -n "$TYPE_ANNOTATION_FILES" ]; then
    echo ""
    log_warning "Found potential type annotations (manual review needed):"
    echo "$TYPE_ANNOTATION_FILES" | while read -r file; do
        # Only flag if in JavaScript code blocks (not JSDoc comments)
        if grep -B3 ': \(string\|number\|boolean\|Promise\)' "$file" | grep -q '^```javascript' && ! grep -q '@param\|@property\|@typedef' "$file"; then
            VIOLATIONS=$((VIOLATIONS + 1))
            echo "  - $file (contains type annotations outside JSDoc)"
        fi
    done
else
    log_success "No suspicious type annotations found"
fi

# Summary
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "TypeScript Detection Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

TOTAL_FILES=$(find "$DOCS_DIR" -name "*.md" | wc -l | tr -d ' ')
echo "Files scanned: $TOTAL_FILES"
echo "Violations found: $VIOLATIONS"
echo ""

if [ "$VIOLATIONS" -eq 0 ]; then
    echo -e "${GREEN}✓ ALL DOCUMENTATION USES JAVASCRIPT + JSDOC${NC}"
    echo ""
    echo "Compliant with project standard:"
    echo "  'we don't do typescript, do javascript, zod, jsdoc'"
    echo ""
    exit 0
else
    echo -e "${RED}✗ TYPESCRIPT USAGE DETECTED${NC}"
    echo ""
    echo "Action required:"
    echo "  1. Replace TypeScript interfaces with JSDoc typedefs"
    echo "  2. Replace 'import type' with regular imports"
    echo "  3. Replace type annotations with JSDoc @param/@property"
    echo "  4. Change code block language from 'typescript' to 'javascript'"
    echo ""
    echo "Example fix:"
    echo "  ❌ interface Foo { bar: string }"
    echo "  ✅ /** @typedef {Object} Foo @property {string} bar */"
    echo ""
    exit 1
fi
