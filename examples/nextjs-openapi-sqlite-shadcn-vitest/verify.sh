#!/usr/bin/env bash
# Verify generated output matches golden files
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIB_DIR="${SCRIPT_DIR}/lib"
GOLDEN_DIR="${SCRIPT_DIR}/golden/lib"

echo "=== Validating Generated Output ==="
echo ""

if [[ ! -d "${LIB_DIR}" ]]; then
    echo -e "${RED}ERROR: lib/ not found. Run 'ggen sync' first.${NC}"
    exit 2
fi

if [[ ! -d "${GOLDEN_DIR}" ]]; then
    echo -e "${YELLOW}WARNING: golden/lib/ not found. Skipping validation.${NC}"
    exit 0
fi

differences=0

while IFS= read -r -d '' golden_file; do
    rel_path="${golden_file#${GOLDEN_DIR}/}"
    gen_file="${LIB_DIR}/${rel_path}"

    if [[ ! -f "${gen_file}" ]]; then
        echo -e "${RED}MISSING:${NC} ${rel_path}"
        differences=1
    elif ! diff -q "${golden_file}" "${gen_file}" > /dev/null 2>&1; then
        echo -e "${YELLOW}DIFFERENT:${NC} ${rel_path}"
        differences=1
    else
        echo -e "${GREEN}OK:${NC} ${rel_path}"
    fi
done < <(find "${GOLDEN_DIR}" -type f -print0)

echo ""
if [[ ${differences} -eq 0 ]]; then
    echo -e "${GREEN}✓ All files match golden reference${NC}"
    exit 0
else
    echo -e "${RED}✗ Differences detected${NC}"
    exit 1
fi
