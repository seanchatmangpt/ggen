#!/usr/bin/env bash
#
# Broken Link Checker
#
# Validates all internal markdown links in documentation

set -e
set -u

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Counters
TOTAL_LINKS=0
BROKEN_LINKS=0

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

log_error() {
    echo -e "${RED}✗${NC} $1"
}

log_info() {
    echo "  $1"
}

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Documentation Link Validation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Get project root
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DOCS_DIR="$PROJECT_ROOT/docs"

if [ ! -d "$DOCS_DIR" ]; then
    log_error "Documentation directory not found: $DOCS_DIR"
    exit 1
fi

echo "Scanning documentation for internal links..."
echo ""

# Find all markdown files
MD_FILES=$(find "$DOCS_DIR" -name "*.md" -o -name "*.markdown")

# Check each file for links
for file in $MD_FILES; do
    # Extract markdown links [text](path.md) - exclude http/https URLs
    LINKS=$(grep -oE '\[([^]]+)\]\(([^)]+)\)' "$file" | grep -v 'http://' | grep -v 'https://' || true)

    if [ -z "$LINKS" ]; then
        continue
    fi

    # Get relative directory of current file
    FILE_DIR=$(dirname "$file")

    # Check each link
    echo "$LINKS" | while IFS= read -r link; do
        # Extract path from [text](path)
        LINK_PATH=$(echo "$link" | sed -n 's/.*\](\([^)]*\)).*/\1/p')

        # Skip anchor-only links (#section)
        if [[ "$LINK_PATH" == \#* ]]; then
            continue
        fi

        # Remove anchor from path (path.md#section -> path.md)
        LINK_PATH_NO_ANCHOR=$(echo "$LINK_PATH" | cut -d'#' -f1)

        # Skip empty paths
        if [ -z "$LINK_PATH_NO_ANCHOR" ]; then
            continue
        fi

        TOTAL_LINKS=$((TOTAL_LINKS + 1))

        # Resolve relative path
        if [[ "$LINK_PATH_NO_ANCHOR" == /* ]]; then
            # Absolute path from project root
            RESOLVED_PATH="$PROJECT_ROOT$LINK_PATH_NO_ANCHOR"
        else
            # Relative path from current file
            RESOLVED_PATH="$FILE_DIR/$LINK_PATH_NO_ANCHOR"
        fi

        # Normalize path (remove ..)
        RESOLVED_PATH=$(cd "$FILE_DIR" && realpath --relative-to="$PROJECT_ROOT" "$LINK_PATH_NO_ANCHOR" 2>/dev/null || echo "$LINK_PATH_NO_ANCHOR")
        RESOLVED_PATH="$PROJECT_ROOT/$RESOLVED_PATH"

        # Check if target exists
        if [ ! -f "$RESOLVED_PATH" ] && [ ! -d "$RESOLVED_PATH" ]; then
            log_error "Broken link: $LINK_PATH"
            log_info "  In: $file"
            log_info "  Resolved to: $RESOLVED_PATH"
            log_info "  Link text: $(echo "$link" | sed -n 's/\[\([^]]*\)\].*/\1/p')"
            echo ""
            BROKEN_LINKS=$((BROKEN_LINKS + 1))
        fi
    done
done

# Check for common typos in validation script references
echo "Checking for validation script references..."
SCRIPT_REFS=$(find "$DOCS_DIR" -name "*.md" -exec grep -n 'validate-.*\.sh' {} + | grep -v 'validate-docs/' || true)

if [ -n "$SCRIPT_REFS" ]; then
    log_error "Found validation script references without proper path:"
    echo "$SCRIPT_REFS" | while read -r ref; do
        log_info "$ref"
        log_info "  → Should be: ./scripts/validate-docs/[script-name].sh"
        BROKEN_LINKS=$((BROKEN_LINKS + 1))
    done
    echo ""
fi

# Summary
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Link Validation Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

echo "Total links checked: $TOTAL_LINKS"
echo "Broken links: $BROKEN_LINKS"
echo ""

if [ "$BROKEN_LINKS" -eq 0 ]; then
    echo -e "${GREEN}✓ ALL LINKS VALID${NC}"
    echo ""
    echo "All internal documentation links are working correctly."
    echo ""
    exit 0
else
    echo -e "${RED}✗ BROKEN LINKS DETECTED${NC}"
    echo ""
    echo "Action required:"
    echo "  1. Fix or remove broken links"
    echo "  2. Verify file paths are correct"
    echo "  3. Update references to moved/renamed files"
    echo "  4. Use relative paths (../file.md) not absolute (/file.md)"
    echo ""
    echo "Re-run after fixes: ./scripts/validate-docs/check-broken-links.sh"
    echo ""
    exit 1
fi
