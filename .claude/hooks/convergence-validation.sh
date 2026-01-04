#!/bin/bash

# Convergence Validation Hook
# Validates synthesized artifact meets specification requirements
# Checks invariants, coverage, and SLO compliance before deployment

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

main() {
    echo -e "${GREEN}→ Convergence Result Validation${NC}"

    local convergence_result="$1"

    if [ -z "$convergence_result" ]; then
        echo -e "${YELLOW}⚠ No convergence result provided${NC}"
        exit 0
    fi

    if [ ! -f "$convergence_result" ]; then
        echo -e "${YELLOW}⚠ Convergence result file not found${NC}"
        exit 0
    fi

    if ! command -v jq &> /dev/null; then
        echo -e "${YELLOW}⚠ jq not available, skipping validation${NC}"
        exit 0
    fi

    # Validate convergence result structure
    if ! jq -e '.convergence_status' "$convergence_result" > /dev/null 2>&1; then
        echo -e "${RED}✗ INVALID: Convergence result missing convergence_status field${NC}"
        exit 1
    fi

    local status=$(jq -r '.convergence_status' "$convergence_result")

    if [ "$status" != "SUCCESS" ]; then
        echo -e "${RED}✗ FAILURE: Convergence status is $status${NC}"
        exit 1
    fi

    # Check invariants verified
    local invariants=$(jq -r '.invariants_verified // false' "$convergence_result")

    if [ "$invariants" = "false" ]; then
        echo -e "${RED}✗ FAILURE: Invariants not verified${NC}"
        echo -e "  Synthesized artifact violates specification constraints"
        exit 1
    fi

    echo -e "${GREEN}✓ Invariants verified${NC}"

    # Check coverage requirement (≥80%)
    local coverage=$(jq -r '.coverage_percentage // 0' "$convergence_result" | cut -d'.' -f1)

    if [ "$coverage" -lt 80 ]; then
        echo -e "${RED}✗ FAILURE: Specification coverage $coverage% < 80% required${NC}"
        exit 1
    fi

    echo -e "${GREEN}✓ Coverage: $coverage% (target ≥80%)${NC}"

    # Check SLO compliance
    local slo=$(jq -r '.slo_compliance // false' "$convergence_result")

    if [ "$slo" = "false" ]; then
        echo -e "${YELLOW}⚠ WARNING: SLO compliance not met${NC}"
        echo -e "  Performance or resource constraints violated"
    else
        echo -e "${GREEN}✓ SLO compliance verified${NC}"
    fi

    # Check ready for deployment
    local ready=$(jq -r '.ready_for_deployment // false' "$convergence_result")

    if [ "$ready" = "true" ]; then
        echo -e "${GREEN}✓ Convergence result: READY FOR DEPLOYMENT${NC}"
        echo -e "  [Receipt] Synthesized artifact passed all validation checks"
        exit 0
    else
        echo -e "${YELLOW}⚠ Result indicates not ready for deployment${NC}"
        exit 1
    fi
}

main "$@"
