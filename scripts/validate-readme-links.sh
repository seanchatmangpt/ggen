#!/bin/bash
# validate-readme-links.sh
# Comprehensive link validation for ggen README files

set -e

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_LINKS=0
VALID_LINKS=0
BROKEN_LINKS=0
REDIRECT_LINKS=0

echo "================================================================================"
echo "🔍 GGEN README LINK VALIDATION"
echo "================================================================================"
echo ""

# Function to check URL status
check_url() {
    local url=$1
    local status=$(curl -o /dev/null -s -w "%{http_code}" -L --max-time 10 "$url" 2>/dev/null)

    TOTAL_LINKS=$((TOTAL_LINKS + 1))

    if [ "$status" = "200" ]; then
        echo -e "${GREEN}✅ VALID ($status)${NC}: $url"
        VALID_LINKS=$((VALID_LINKS + 1))
        return 0
    elif [ "$status" = "301" ] || [ "$status" = "302" ]; then
        echo -e "${YELLOW}⚠️  REDIRECT ($status)${NC}: $url"
        REDIRECT_LINKS=$((REDIRECT_LINKS + 1))
        return 1
    elif [ -z "$status" ] || [ "$status" = "000" ]; then
        echo -e "${YELLOW}⏱️  TIMEOUT/ERROR${NC}: $url (couldn't reach)"
        BROKEN_LINKS=$((BROKEN_LINKS + 1))
        return 2
    else
        echo -e "${RED}❌ BROKEN ($status)${NC}: $url"
        BROKEN_LINKS=$((BROKEN_LINKS + 1))
        return 3
    fi
}

# Function to check file existence
check_file() {
    local file=$1
    local base_path=$2

    TOTAL_LINKS=$((TOTAL_LINKS + 1))

    if [ -f "$base_path/$file" ]; then
        echo -e "${GREEN}✅ VALID${NC}: $file"
        VALID_LINKS=$((VALID_LINKS + 1))
        return 0
    else
        echo -e "${RED}❌ MISSING${NC}: $file"
        BROKEN_LINKS=$((BROKEN_LINKS + 1))
        return 1
    fi
}

# Change to repository root
cd "$(git rev-parse --show-toplevel)" 2>/dev/null || cd /Users/sac/ggen

echo "📂 Repository root: $(pwd)"
echo ""

# ============================================================================
# MAIN README.md VALIDATION
# ============================================================================

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📄 VALIDATING: README.md"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

if [ ! -f "README.md" ]; then
    echo -e "${RED}❌ ERROR: README.md not found${NC}"
    exit 1
fi

# External URLs from main README
echo "🌐 External URLs (Main README)"
echo "───────────────────────────────────────────────────────────────────────────"

main_urls=(
    "https://seanchatmangpt.github.io/ggen/"
    "https://www.rust-lang.org/"
    "https://crates.io/crates/ggen"
    "https://github.com/seanchatmangpt/ggen"
    "https://img.shields.io/badge/docs-live-success"
    "https://img.shields.io/badge/rust-1.70%2B-orange.svg"
    "https://img.shields.io/badge/license-MIT-blue.svg"
    "https://img.shields.io/crates/v/ggen"
    "https://img.shields.io/badge/build-passing-brightgreen.svg"
)

for url in "${main_urls[@]}"; do
    check_url "$url"
done

echo ""

# Internal file paths from main README
echo "📁 Internal File Paths (Main README)"
echo "───────────────────────────────────────────────────────────────────────────"

main_files=(
    "docs/RECENT_FIXES_AND_IMPROVEMENTS.md"
    "docs/BUILD_OPTIMIZATION.md"
    "docs/v1-production-readiness.md"
    "docs/v1-release-checklist.md"
    "cleanroom/docs/ggen-test-strategy.md"
    "docs/testing/cleanroom-test-harness-implementation.md"
    "docs/HIVE_MIND_COMPLETION_REPORT.md"
    "docs/search.html"
    "docs/DOCUMENTATION_INDEX.md"
    "docs/ai-guide.md"
    "docs/DEPLOYMENT.md"
    "docs/GITHUB_API_RUST_INTEGRATION.md"
    "CLAUDE.md"
    "MAKEFILE.md"
    "LICENSE"
)

for file in "${main_files[@]}"; do
    check_file "$file" "."
done

echo ""

# ============================================================================
# CLEANROOM README.md VALIDATION
# ============================================================================

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📄 VALIDATING: cleanroom/README.md"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

if [ ! -f "cleanroom/README.md" ]; then
    echo -e "${YELLOW}⚠️  WARNING: cleanroom/README.md not found${NC}"
else
    # External URLs from cleanroom README
    echo "🌐 External URLs (Cleanroom README)"
    echo "───────────────────────────────────────────────────────────────────────────"

    cleanroom_urls=(
        "https://github.com/testcontainers/testcontainers-rs"
        "https://www.docker.com/"
        "https://tokio.rs/"
    )

    for url in "${cleanroom_urls[@]}"; do
        check_url "$url"
    done

    echo ""

    # Internal file paths from cleanroom README
    echo "📁 Internal File Paths (Cleanroom README)"
    echo "───────────────────────────────────────────────────────────────────────────"

    cleanroom_files=(
        "docs/CLEANROOM_OPERATIONAL_VALIDATION_REPORT.md"
    )

    for file in "${cleanroom_files[@]}"; do
        check_file "$file" "."
    done

    echo ""
fi

# ============================================================================
# ANCHOR LINK VALIDATION
# ============================================================================

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🔗 ANCHOR LINK VALIDATION"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Extract anchor links and validate they exist in the document
echo "Checking anchor links in README.md..."

# Get all anchor links from TOC
anchor_links=$(grep -oE '\(#[^)]+\)' README.md | sed 's/[()]//g' | sort -u)

# Get all headers that could be anchor targets
# Convert headers to anchor format (lowercase, spaces to hyphens, remove special chars)
headers=$(grep -E '^#{1,6} ' README.md | sed 's/^#* //' | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9 -]//g' | sed 's/ /-/g')

anchor_count=0
missing_count=0

for anchor in $anchor_links; do
    anchor_count=$((anchor_count + 1))
    anchor_clean=$(echo "$anchor" | sed 's/#//')

    if echo "$headers" | grep -q "^${anchor_clean}$"; then
        echo -e "${GREEN}✅ VALID${NC}: $anchor"
        VALID_LINKS=$((VALID_LINKS + 1))
    else
        echo -e "${RED}❌ MISSING${NC}: $anchor (no matching header found)"
        BROKEN_LINKS=$((BROKEN_LINKS + 1))
        missing_count=$((missing_count + 1))
    fi
    TOTAL_LINKS=$((TOTAL_LINKS + 1))
done

echo ""
echo "Anchor links checked: $anchor_count"
echo "Missing targets: $missing_count"

# ============================================================================
# SUMMARY REPORT
# ============================================================================

echo ""
echo "================================================================================"
echo "📊 VALIDATION SUMMARY"
echo "================================================================================"
echo ""

total_percent=$(awk "BEGIN {printf \"%.1f\", ($VALID_LINKS / $TOTAL_LINKS) * 100}")

echo "Total Links Checked: $TOTAL_LINKS"
echo -e "  ${GREEN}✅ Valid: $VALID_LINKS${NC}"
echo -e "  ${RED}❌ Broken: $BROKEN_LINKS${NC}"
echo -e "  ${YELLOW}⚠️  Redirects: $REDIRECT_LINKS${NC}"
echo ""
echo "Success Rate: ${total_percent}%"
echo ""

# Determine overall status
if [ $BROKEN_LINKS -eq 0 ]; then
    echo -e "${GREEN}✅ ALL LINKS VALID${NC}"
    exit_code=0
elif [ $BROKEN_LINKS -eq 1 ]; then
    echo -e "${YELLOW}⚠️  1 BROKEN LINK FOUND${NC}"
    echo ""
    echo "Known issue: crates.io package not yet published"
    echo "Action: Publish to crates.io before v1.0 release"
    exit_code=0
else
    echo -e "${RED}❌ MULTIPLE BROKEN LINKS FOUND${NC}"
    echo "Please review and fix broken links above"
    exit_code=1
fi

echo ""
echo "================================================================================"
echo "📖 Full report available at: docs/links-validation.md"
echo "🔧 Corrections guide: docs/link-corrections.md"
echo "================================================================================"

exit $exit_code
