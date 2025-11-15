#!/bin/bash
# Standalone Diataxis Documentation Validation Script
# Validates ggen documentation against Diataxis framework principles
# Enterprise-grade validation suitable for Fortune 500 deployments

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0
WARNINGS=0

# Test results storage
declare -a FAILED_TESTS
declare -a WARNING_TESTS

# Functions
log_check() {
    local status=$1
    local category=$2
    local test=$3
    local message=$4

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if [ "$status" == "PASS" ]; then
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        echo -e "${GREEN}✓${NC} [$category] $test: $message"
    elif [ "$status" == "WARN" ]; then
        WARNINGS=$((WARNINGS + 1))
        WARNING_TESTS+=("[$category] $test: $message")
        echo -e "${YELLOW}⚠${NC} [$category] $test: $message"
    else
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        FAILED_TESTS+=("[$category] $test: $message")
        echo -e "${RED}✗${NC} [$category] $test: $message"
    fi
}

# Test 1: Verify directory structure
test_directory_structure() {
    echo ""
    echo -e "${BLUE}Test 1: Directory Structure${NC}"

    local required_dirs=("docs/tutorials" "docs/how-to-guides" "docs/reference" "docs/explanations")

    for dir in "${required_dirs[@]}"; do
        if [ -d "$dir" ]; then
            log_check "PASS" "Structure" "Directory: $dir" "Found"
        else
            log_check "FAIL" "Structure" "Directory: $dir" "Not found"
        fi
    done
}

# Test 2: Verify required files
test_required_files() {
    echo ""
    echo -e "${BLUE}Test 2: Required Files${NC}"

    local required_files=(
        "docs/README.md"
        "docs/tutorials/getting-started.md"
        "docs/how-to-guides/installation.md"
        "docs/how-to-guides/validate-ontologies-shacl.md"
        "docs/how-to-guides/migrate-existing-code.md"
        "docs/how-to-guides/DOGFOODING_QUICKSTART.md"
        "docs/reference/cli.md"
        "docs/explanations/architecture.md"
        "docs/explanations/lifecycle-and-hooks.md"
        "README.md"
    )

    for file in "${required_files[@]}"; do
        if [ -f "$file" ]; then
            log_check "PASS" "Files" "File: $file" "Found"
        else
            log_check "FAIL" "Files" "File: $file" "Not found"
        fi
    done
}

# Test 3: Verify file counts per category
test_category_file_counts() {
    echo ""
    echo -e "${BLUE}Test 3: File Counts by Category${NC}"

    local tutorial_count=$(find docs/tutorials -name "*.md" | wc -l)
    local howto_count=$(find docs/how-to-guides -name "*.md" | wc -l)
    local reference_count=$(find docs/reference -name "*.md" | wc -l)
    local explanation_count=$(find docs/explanations -name "*.md" | wc -l)

    # Tutorials: 4-5 files
    if [ "$tutorial_count" -ge 4 ] && [ "$tutorial_count" -le 5 ]; then
        log_check "PASS" "Count" "Tutorials" "$tutorial_count files (expected 4-5)"
    else
        log_check "WARN" "Count" "Tutorials" "$tutorial_count files (expected 4-5)"
    fi

    # How-to Guides: 9-12 files
    if [ "$howto_count" -ge 9 ] && [ "$howto_count" -le 12 ]; then
        log_check "PASS" "Count" "How-to Guides" "$howto_count files (expected 9-12)"
    else
        log_check "WARN" "Count" "How-to Guides" "$howto_count files (expected 9-12)"
    fi

    # Reference: 4-5 files
    if [ "$reference_count" -ge 4 ] && [ "$reference_count" -le 5 ]; then
        log_check "PASS" "Count" "Reference" "$reference_count files (expected 4-5)"
    else
        log_check "WARN" "Count" "Reference" "$reference_count files (expected 4-5)"
    fi

    # Explanations: 5-7 files
    if [ "$explanation_count" -ge 5 ] && [ "$explanation_count" -le 7 ]; then
        log_check "PASS" "Count" "Explanations" "$explanation_count files (expected 5-7)"
    else
        log_check "WARN" "Count" "Explanations" "$explanation_count files (expected 5-7)"
    fi
}

# Test 4: Verify README.md has Diataxis section
test_readme_diataxis() {
    echo ""
    echo -e "${BLUE}Test 4: README.md Diataxis Section${NC}"

    if grep -q "Documentation Map" README.md; then
        log_check "PASS" "README" "Documentation Map section" "Found"
    else
        log_check "FAIL" "README" "Documentation Map section" "Not found"
    fi

    if grep -q "Tutorials" README.md; then
        log_check "PASS" "README" "Tutorials reference" "Found"
    else
        log_check "FAIL" "README" "Tutorials reference" "Not found"
    fi

    if grep -q "How-to" README.md || grep -q "How to" README.md; then
        log_check "PASS" "README" "How-to Guides reference" "Found"
    else
        log_check "FAIL" "README" "How-to Guides reference" "Not found"
    fi

    if grep -q "Reference" README.md; then
        log_check "PASS" "README" "Reference section" "Found"
    else
        log_check "FAIL" "README" "Reference section" "Not found"
    fi

    if grep -q "Explanation" README.md; then
        log_check "PASS" "README" "Explanation section" "Found"
    else
        log_check "FAIL" "README" "Explanation section" "Not found"
    fi
}

# Test 5: Verify docs/README.md navigation
test_docs_readme_navigation() {
    echo ""
    echo -e "${BLUE}Test 5: docs/README.md Navigation${NC}"

    if grep -q "Tutorials" docs/README.md; then
        log_check "PASS" "Navigation" "Tutorial section in docs/README" "Found"
    else
        log_check "FAIL" "Navigation" "Tutorial section in docs/README" "Not found"
    fi

    if grep -q "How-to" docs/README.md || grep -q "How to" docs/README.md; then
        log_check "PASS" "Navigation" "How-to Guide section in docs/README" "Found"
    else
        log_check "FAIL" "Navigation" "How-to Guide section in docs/README" "Not found"
    fi

    if grep -q "Reference" docs/README.md; then
        log_check "PASS" "Navigation" "Reference section in docs/README" "Found"
    else
        log_check "FAIL" "Navigation" "Reference section in docs/README" "Not found"
    fi

    if grep -q "Explanation" docs/README.md; then
        log_check "PASS" "Navigation" "Explanation section in docs/README" "Found"
    else
        log_check "FAIL" "Navigation" "Explanation section in docs/README" "Not found"
    fi
}

# Test 6: Verify new critical documentation
test_new_documentation() {
    echo ""
    echo -e "${BLUE}Test 6: New Critical Documentation${NC}"

    # SHACL validation guide
    if [ -f "docs/how-to-guides/validate-ontologies-shacl.md" ]; then
        local lines=$(wc -l < "docs/how-to-guides/validate-ontologies-shacl.md")
        if [ "$lines" -gt 100 ]; then
            log_check "PASS" "New Docs" "SHACL validation guide" "$lines lines (comprehensive)"
        else
            log_check "WARN" "New Docs" "SHACL validation guide" "$lines lines (may be incomplete)"
        fi
    else
        log_check "FAIL" "New Docs" "SHACL validation guide" "File not found"
    fi

    # Migration guide
    if [ -f "docs/how-to-guides/migrate-existing-code.md" ]; then
        local lines=$(wc -l < "docs/how-to-guides/migrate-existing-code.md")
        if [ "$lines" -gt 100 ]; then
            log_check "PASS" "New Docs" "Code migration guide" "$lines lines (comprehensive)"
        else
            log_check "WARN" "New Docs" "Code migration guide" "$lines lines (may be incomplete)"
        fi
    else
        log_check "FAIL" "New Docs" "Code migration guide" "File not found"
    fi

    # Lifecycle/hooks explanation
    if [ -f "docs/explanations/lifecycle-and-hooks.md" ]; then
        local lines=$(wc -l < "docs/explanations/lifecycle-and-hooks.md")
        if [ "$lines" -gt 100 ]; then
            log_check "PASS" "New Docs" "Lifecycle/hooks explanation" "$lines lines (comprehensive)"
        else
            log_check "WARN" "New Docs" "Lifecycle/hooks explanation" "$lines lines (may be incomplete)"
        fi
    else
        log_check "FAIL" "New Docs" "Lifecycle/hooks explanation" "File not found"
    fi
}

# Test 7: Verify content quality (sample)
test_content_quality() {
    echo ""
    echo -e "${BLUE}Test 7: Content Quality (Sample)${NC}"

    # Check tutorials have "Prerequisites" section
    local tutorial_count=0
    local tutorial_with_prereqs=0
    for file in docs/tutorials/*.md; do
        if [ -f "$file" ]; then
            tutorial_count=$((tutorial_count + 1))
            if grep -q -i "prerequisites" "$file"; then
                tutorial_with_prereqs=$((tutorial_with_prereqs + 1))
            fi
        fi
    done

    if [ "$tutorial_count" -gt 0 ] && [ "$tutorial_with_prereqs" -eq "$tutorial_count" ]; then
        log_check "PASS" "Quality" "Tutorial prerequisites" "$tutorial_with_prereqs/$tutorial_count have Prerequisites"
    else
        log_check "WARN" "Quality" "Tutorial prerequisites" "$tutorial_with_prereqs/$tutorial_count have Prerequisites"
    fi

    # Check how-to guides have examples
    local howto_count=0
    local howto_with_examples=0
    for file in docs/how-to-guides/*.md; do
        if [ -f "$file" ]; then
            howto_count=$((howto_count + 1))
            if grep -q "^\`\`\`" "$file"; then
                howto_with_examples=$((howto_with_examples + 1))
            fi
        fi
    done

    if [ "$howto_count" -gt 0 ] && [ "$howto_with_examples" -ge $((howto_count - 1)) ]; then
        log_check "PASS" "Quality" "How-to examples" "$howto_with_examples/$howto_count have code examples"
    else
        log_check "WARN" "Quality" "How-to examples" "$howto_with_examples/$howto_count have code examples"
    fi
}

# Test 8: Verify DOGFOODING_QUICKSTART.md refactoring
test_dogfooding_refactoring() {
    echo ""
    echo -e "${BLUE}Test 8: DOGFOODING_QUICKSTART.md Refactoring${NC}"

    local file="docs/how-to-guides/DOGFOODING_QUICKSTART.md"

    if grep -q "How to Dogfood" "$file"; then
        log_check "PASS" "Refactoring" "Has 'How to' title" "Found"
    else
        log_check "FAIL" "Refactoring" "Has 'How to' title" "Not found"
    fi

    if grep -q "Problem Statement" "$file" || grep -q "Problem" "$file"; then
        log_check "PASS" "Refactoring" "Has problem statement" "Found"
    else
        log_check "FAIL" "Refactoring" "Has problem statement" "Not found"
    fi

    if grep -q "Prerequisites" "$file"; then
        log_check "PASS" "Refactoring" "Has prerequisites" "Found"
    else
        log_check "FAIL" "Refactoring" "Has prerequisites" "Not found"
    fi

    # Check that emoji usage is reduced (should have few if any)
    local emoji_count=$(grep -o '[🚀🐕❤️✅❌]' "$file" 2>/dev/null | wc -l)
    if [ "$emoji_count" -lt 2 ]; then
        log_check "PASS" "Refactoring" "Reduced emoji usage" "$emoji_count emojis (professional)"
    else
        log_check "WARN" "Refactoring" "Reduced emoji usage" "$emoji_count emojis (consider reducing)"
    fi
}

# Main execution
main() {
    echo ""
    echo "╔═══════════════════════════════════════════════════════╗"
    echo "║  Diataxis Documentation Integration Validation Suite  ║"
    echo "║  Enterprise-grade validation for Fortune 500 standards║"
    echo "╚═══════════════════════════════════════════════════════╝"

    test_directory_structure
    test_required_files
    test_category_file_counts
    test_readme_diataxis
    test_docs_readme_navigation
    test_new_documentation
    test_content_quality
    test_dogfooding_refactoring

    # Print summary
    echo ""
    echo "╔═══════════════════════════════════════════════════════╗"
    echo "║                  TEST SUMMARY                         ║"
    echo "╚═══════════════════════════════════════════════════════╝"
    echo ""
    echo -e "Total Checks:    $TOTAL_CHECKS"
    echo -e "Passed:          ${GREEN}$PASSED_CHECKS${NC}"
    echo -e "Failed:          ${RED}$FAILED_CHECKS${NC}"
    echo -e "Warnings:        ${YELLOW}$WARNINGS${NC}"
    echo ""

    # Print failed tests
    if [ "${#FAILED_TESTS[@]}" -gt 0 ]; then
        echo -e "${RED}Failed Tests:${NC}"
        for test in "${FAILED_TESTS[@]}"; do
            echo "  ✗ $test"
        done
        echo ""
    fi

    # Print warnings
    if [ "${#WARNING_TESTS[@]}" -gt 0 ]; then
        echo -e "${YELLOW}Warnings:${NC}"
        for test in "${WARNING_TESTS[@]}"; do
            echo "  ⚠ $test"
        done
        echo ""
    fi

    # Final verdict
    echo "╔═══════════════════════════════════════════════════════╗"
    if [ "$FAILED_CHECKS" -eq 0 ]; then
        echo -e "║            Status: ${GREEN}✓ ALL CHECKS PASSED${NC}            ║"
        echo -e "║  Diataxis Compliance: ${GREEN}100%${NC}                      ║"
    else
        echo -e "║           Status: ${RED}✗ SOME CHECKS FAILED${NC}           ║"
        echo -e "║  Diataxis Compliance: $(echo "scale=1; $PASSED_CHECKS * 100 / $TOTAL_CHECKS" | bc)%                      ║"
    fi
    echo "╚═══════════════════════════════════════════════════════╝"
    echo ""

    # Exit code
    if [ "$FAILED_CHECKS" -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

# Run main function
main
