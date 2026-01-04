#!/bin/bash

# Post-Collision Detection Hook
# Validates collision analysis before proceeding to convergence
# Ensures sufficient overlap was detected to proceed

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

main() {
    echo -e "${GREEN}→ Post-Collision Detection Validation${NC}"

    # Check if collision report exists (should be passed as argument)
    local collision_report="$1"

    if [ -z "$collision_report" ]; then
        echo -e "${YELLOW}⚠ No collision report provided${NC}"
        echo -e "  This hook is called after collision detection phase"
        exit 0
    fi

    # Validate collision report structure (if JSON file)
    if [ -f "$collision_report" ]; then
        if ! command -v jq &> /dev/null; then
            echo -e "${YELLOW}⚠ jq not available, skipping JSON validation${NC}"
            exit 0
        fi

        # Check for collision analysis field
        if ! jq -e '.collision_analysis' "$collision_report" > /dev/null 2>&1; then
            echo -e "${RED}✗ INVALID: Collision report missing collision_analysis field${NC}"
            exit 1
        fi

        # Count overlaps found
        local total_decisions=$(jq '.collision_analysis | length' "$collision_report" 2>/dev/null || echo "0")

        if [ "$total_decisions" -eq 0 ]; then
            echo -e "${RED}✗ FAILURE: No collision detected (all agents diverged)${NC}"
            echo -e "  Action: Return to /speckit-verify - specification may be incomplete"
            exit 1
        fi

        # Check for HIGH_CONFIDENCE decisions (GREEN collisions)
        local high_confidence=$(jq '[.collision_analysis[] | select(.status == "GREEN_COLLISION")] | length' "$collision_report" 2>/dev/null || echo "0")

        if [ "$high_confidence" -gt 0 ]; then
            echo -e "${GREEN}✓ HIGH CONFIDENCE: $high_confidence decisions with ≥60% agent agreement${NC}"
        else
            echo -e "${YELLOW}⚠ CAUTION: No high-confidence decisions${NC}"
            echo -e "  Agents may be diverging on architecture or approach"
        fi

        # Check for ready_for_convergence flag
        if jq -e '.ready_for_convergence' "$collision_report" > /dev/null 2>&1; then
            local ready=$(jq -r '.ready_for_convergence' "$collision_report")
            if [ "$ready" = "true" ]; then
                echo -e "${GREEN}✓ Collision analysis: READY FOR CONVERGENCE${NC}"
                exit 0
            else
                echo -e "${RED}✗ Collision analysis indicates NOT ready for convergence${NC}"
                exit 1
            fi
        fi

        echo -e "${GREEN}✓ Collision detection: Sufficient overlap found, proceeding to convergence${NC}"
        exit 0
    fi

    echo -e "${GREEN}✓ Post-collision check complete${NC}"
    exit 0
}

main "$@"
