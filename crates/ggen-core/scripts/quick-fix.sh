#!/bin/bash
# Quick Fix Script for Critical Validation Blockers
# This script fixes the immediate compilation errors

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}🔧 ggen-core Quick Fix Script${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

cd "$(dirname "$0")/.." || exit 1

# Backup function
backup_file() {
    local file=$1
    if [ -f "$file" ]; then
        cp "$file" "${file}.bak"
        echo -e "${YELLOW}📋 Backed up: ${file}.bak${NC}"
    fi
}

# Fix 1: Type error in validation.rs
echo -e "\n${BLUE}[1/5]${NC} Fixing type error in validation.rs..."
FILE="src/lifecycle/validation.rs"
if [ -f "$FILE" ]; then
    backup_file "$FILE"
    sed -i '' 's/path: "readiness.toml".to_string(),/path: "readiness.toml".into(),/g' "$FILE"
    echo -e "${GREEN}✓ Fixed PathBuf type error${NC}"
else
    echo -e "${RED}✗ File not found: $FILE${NC}"
fi

# Fix 2: Remove unused HashMap import from production.rs
echo -e "\n${BLUE}[2/5]${NC} Removing unused HashMap import from production.rs..."
FILE="src/lifecycle/production.rs"
if [ -f "$FILE" ]; then
    backup_file "$FILE"
    sed -i '' 's/use std::collections::{BTreeMap, HashMap};/use std::collections::BTreeMap;/g' "$FILE"
    echo -e "${GREEN}✓ Removed HashMap import${NC}"
else
    echo -e "${RED}✗ File not found: $FILE${NC}"
fi

# Fix 3: Remove unused ReadinessStatus import from validation.rs
echo -e "\n${BLUE}[3/5]${NC} Removing unused ReadinessStatus import..."
FILE="src/lifecycle/validation.rs"
if [ -f "$FILE" ]; then
    sed -i '' 's/ReadinessCategory, ReadinessStatus/ReadinessCategory/g' "$FILE"
    echo -e "${GREEN}✓ Removed ReadinessStatus import${NC}"
else
    echo -e "${RED}✗ File not found: $FILE${NC}"
fi

# Fix 4: Remove unused chrono::Utc import (safer approach)
echo -e "\n${BLUE}[4/5]${NC} Removing unused chrono::Utc import..."
FILE="src/lifecycle/validation.rs"
if [ -f "$FILE" ]; then
    # Remove lines containing only "use chrono::Utc;"
    sed -i '' '/^[[:space:]]*use chrono::Utc;[[:space:]]*$/d' "$FILE"
    echo -e "${GREEN}✓ Removed chrono::Utc import${NC}"
else
    echo -e "${RED}✗ File not found: $FILE${NC}"
fi

# Fix 5: Remove unused BTreeMap import from behavior_tests.rs
echo -e "\n${BLUE}[5/5]${NC} Removing unused BTreeMap import..."
FILE="src/lifecycle/behavior_tests.rs"
if [ -f "$FILE" ]; then
    backup_file "$FILE"
    sed -i '' '/^[[:space:]]*use std::collections::BTreeMap;[[:space:]]*$/d' "$FILE"
    echo -e "${GREEN}✓ Removed BTreeMap import${NC}"
else
    echo -e "${RED}✗ File not found: $FILE${NC}"
fi

# Verify fixes
echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}🔍 Verifying fixes...${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

echo -e "${YELLOW}Running cargo check...${NC}"
if cargo check --all-targets 2>&1 | tee /tmp/cargo-check-output.txt; then
    echo -e "\n${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}✓ SUCCESS! All fixes applied successfully!${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

    # Check for warnings
    if grep -q "warning:" /tmp/cargo-check-output.txt; then
        echo -e "${YELLOW}⚠️  Warnings detected. Run 'cargo clippy' for details.${NC}\n"
    else
        echo -e "${GREEN}✓ No warnings! Code is clean.${NC}\n"
    fi

    exit 0
else
    echo -e "\n${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${RED}✗ Compilation still has errors!${NC}"
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"
    echo -e "${YELLOW}Check the output above for remaining issues.${NC}"
    echo -e "${YELLOW}Backup files (.bak) have been created.${NC}\n"
    exit 1
fi
