#!/usr/bin/env bash
# Rebuild marketplace registry index from package metadata
# Scans all package.toml files and generates marketplace/index.json

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
MARKETPLACE_DIR="${PROJECT_ROOT}/marketplace"
PACKAGES_DIR="${MARKETPLACE_DIR}/packages"
INDEX_FILE="${MARKETPLACE_DIR}/index.json"
PYTHON_SCRIPT="${SCRIPT_DIR}/rebuild-index.py"

echo -e "${GREEN}=== Rebuilding Marketplace Registry Index ===${NC}"
echo "Scanning: ${PACKAGES_DIR}"
echo "Output: ${INDEX_FILE}"
echo ""

# Check if packages directory exists
if [[ ! -d "${PACKAGES_DIR}" ]]; then
    echo -e "${RED}Error: Packages directory not found: ${PACKAGES_DIR}${NC}"
    exit 1
fi

# Find all package.toml files
PACKAGE_TOML_FILES=($(find "${PACKAGES_DIR}" -name 'package.toml'))
TOTAL_PACKAGES=${#PACKAGE_TOML_FILES[@]}

if [[ ${TOTAL_PACKAGES} -eq 0 ]]; then
    echo -e "${RED}Error: No package.toml files found in ${PACKAGES_DIR}${NC}"
    exit 1
fi

echo -e "${YELLOW}Found ${TOTAL_PACKAGES} package.toml files${NC}"
echo ""

# Run Python script to rebuild index
python3 "${PYTHON_SCRIPT}" "${PACKAGES_DIR}" "${INDEX_FILE}"

# Verify output
FINAL_COUNT=$(jq '. | length' "${INDEX_FILE}" 2>/dev/null || echo "0")

echo ""
echo -e "${GREEN}=== Index Rebuild Complete ===${NC}"
echo "Total packages indexed: ${FINAL_COUNT}"
echo "Index file: ${INDEX_FILE}"
echo ""

# Validate JSON structure
if ! jq empty "${INDEX_FILE}" 2>/dev/null; then
    echo -e "${RED}Error: Generated invalid JSON${NC}"
    exit 1
fi

# Show sample entries
echo "Sample entries (first 5):"
jq '.[:5]' "${INDEX_FILE}"

# Show packages with descriptions
echo ""
echo "Packages with descriptions (first 5):"
jq '.[] | select(.description != null) | {id, version, description}' "${INDEX_FILE}" | head -20

echo ""
echo -e "${GREEN}✓ Marketplace index rebuilt successfully${NC}"
