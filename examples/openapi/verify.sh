#!/bin/bash
# Verification script for OpenAPI example
# Compares generated output with golden files

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GENERATED_DIR="${SCRIPT_DIR}/lib"
GOLDEN_DIR="${SCRIPT_DIR}/golden/lib"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "🔍 Verifying generated output against golden files..."
echo ""

# Check if generated directory exists
if [ ! -d "${GENERATED_DIR}" ]; then
    echo -e "${RED}❌ Error: Generated directory not found: ${GENERATED_DIR}${NC}"
    echo "   Run 'ggen sync' first to generate output"
    exit 1
fi

# Check if golden directory exists
if [ ! -d "${GOLDEN_DIR}" ]; then
    echo -e "${RED}❌ Error: Golden directory not found: ${GOLDEN_DIR}${NC}"
    exit 1
fi

# Function to compare directories
compare_files() {
    local dir1="$1"
    local dir2="$2"
    local errors=0

    # Find all files in generated directory
    while IFS= read -r -d '' file; do
        rel_path="${file#$dir1/}"
        golden_file="${dir2}/${rel_path}"

        if [ ! -f "${golden_file}" ]; then
            echo -e "${RED}❌ Missing in golden: ${rel_path}${NC}"
            errors=$((errors + 1))
            continue
        fi

        if ! diff -q "${file}" "${golden_file}" > /dev/null 2>&1; then
            echo -e "${RED}❌ Mismatch: ${rel_path}${NC}"
            echo "   Run: diff ${file} ${golden_file}"
            errors=$((errors + 1))
        else
            echo -e "${GREEN}✅ Match: ${rel_path}${NC}"
        fi
    done < <(find "${dir1}" -type f -print0 | sort -z)

    # Check for extra files in golden
    while IFS= read -r -d '' file; do
        rel_path="${file#$dir2/}"
        generated_file="${dir1}/${rel_path}"

        if [ ! -f "${generated_file}" ]; then
            echo -e "${YELLOW}⚠️  Extra in golden (not generated): ${rel_path}${NC}"
        fi
    done < <(find "${dir2}" -type f -print0 | sort -z)

    return $errors
}

# Compare all files
ERRORS=0
compare_files "${GENERATED_DIR}" "${GOLDEN_DIR}" || ERRORS=$?

echo ""
if [ $ERRORS -eq 0 ]; then
    echo -e "${GREEN}✅ All files match golden files!${NC}"
    exit 0
else
    echo -e "${RED}❌ Found ${ERRORS} file(s) with differences${NC}"
    echo ""
    echo "To see differences, run:"
    echo "  diff -r lib/ golden/lib/"
    exit 1
fi

