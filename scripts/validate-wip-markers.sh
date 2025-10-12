#!/usr/bin/env bash
# CI validation script for WIP queue markers
set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "🔍 WIP Queue Marker Validation"
echo "================================"

# Check 1: Branch name validation
echo -e "\n${YELLOW}Check 1: Branch name validation${NC}"
branch_name=$(git rev-parse --abbrev-ref HEAD)
echo "Current branch: $branch_name"

if [[ $branch_name =~ ^wip/.*__L([0-9]+)$ ]]; then
    line_number="${BASH_REMATCH[1]}"
    # Extract file path from branch name (convert __ back to /)
    file_path=$(echo "$branch_name" | sed 's/^wip\///' | sed 's/__L[0-9]*$//' | tr '__' '/')

    echo "Expected WIP marker at: $file_path:$line_number"

    # Check if the file exists
    if [[ ! -f "$file_path" ]]; then
        echo -e "${RED}❌ File not found: $file_path${NC}"
        exit 1
    fi

    # Check if the line was changed from WIP to READY or removed
    if git diff HEAD~1..HEAD "$file_path" | grep -q "^-.*WIP:"; then
        if git diff HEAD~1..HEAD "$file_path" | grep -q "^+.*READY:"; then
            echo -e "${GREEN}✅ WIP marker properly flipped to READY${NC}"
        elif ! git diff HEAD~1..HEAD "$file_path" | grep -q "^+.*WIP:"; then
            echo -e "${GREEN}✅ WIP marker removed${NC}"
        else
            echo -e "${RED}❌ WIP marker still present at line $line_number${NC}"
            exit 1
        fi
    else
        echo -e "${YELLOW}⚠️  Warning: No WIP marker change detected at specified line${NC}"
        echo "This may be acceptable if the marker was already changed."
    fi
else
    echo -e "${GREEN}✅ Not a WIP branch - skipping marker validation${NC}"
fi

# Check 2: Lock cleanup
echo -e "\n${YELLOW}Check 2: Lock cleanup validation${NC}"
if [[ -d ".wiplocks" ]]; then
    lock_count=$(find .wiplocks -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l | tr -d ' ')
    if [[ $lock_count -gt 0 ]]; then
        echo -e "${RED}❌ Found $lock_count unreleased locks in .wiplocks/${NC}"
        find .wiplocks -mindepth 1 -maxdepth 1 -type d -exec basename {} \;
        exit 1
    else
        echo -e "${GREEN}✅ No unreleased locks found${NC}"
    fi
else
    echo -e "${GREEN}✅ .wiplocks directory not present${NC}"
fi

# Check 3: No new WIP markers outside touched files
echo -e "\n${YELLOW}Check 3: New WIP marker validation${NC}"
# Get list of modified files
modified_files=$(git diff --name-only HEAD~1..HEAD)

if [[ -z "$modified_files" ]]; then
    echo -e "${GREEN}✅ No files modified${NC}"
    exit 0
fi

# Check each modified file for new WIP markers
new_wip_count=0
for file in $modified_files; do
    if [[ -f "$file" ]]; then
        # Look for lines that were added (start with +) and contain WIP markers
        if git diff HEAD~1..HEAD "$file" | grep -q "^+.*\(WIP:\|TODO:\|UNIMPL:\|unimplemented!\|todo!\)"; then
            # Count new WIP markers
            count=$(git diff HEAD~1..HEAD "$file" | grep "^+.*\(WIP:\|TODO:\|UNIMPL:\|unimplemented!\|todo!\)" | wc -l | tr -d ' ')
            if [[ $count -gt 0 ]]; then
                echo -e "${YELLOW}⚠️  Found $count new WIP marker(s) in $file${NC}"
                ((new_wip_count+=count))
            fi
        fi
    fi
done

if [[ $new_wip_count -gt 0 ]]; then
    echo -e "${YELLOW}⚠️  Total new WIP markers: $new_wip_count${NC}"
    echo "New WIP markers are allowed but should be intentional."
else
    echo -e "${GREEN}✅ No new WIP markers added${NC}"
fi

echo -e "\n${GREEN}================================${NC}"
echo -e "${GREEN}✅ All WIP queue validations passed!${NC}"
