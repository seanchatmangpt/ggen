#!/bin/bash

# Pre-Specification Check Hook
# Validates specification closure before EPIC 9 fan-out
# Called on UserPromptSubmit when task involves implementation

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if this is a non-trivial task (heuristic)
detect_non_trivial() {
    local prompt="$1"

    # Check for trigger keywords
    if echo "$prompt" | grep -iE "(implement|feature|design|architecture|refactor|bug|fix|add|create|build)" > /dev/null 2>&1; then
        return 0  # Non-trivial (return success)
    fi
    return 1  # Trivial (return failure)
}

# Check for vague specification keywords
check_specification_openness() {
    local prompt="$1"
    local vague_count=0

    # Count vague keywords
    vague_count=$(echo "$prompt" | grep -io -E "(TBD|TODO|maybe|probably|might|should|unclear|undefined|TK|TBC|tbd)" | wc -l)

    if [ "$vague_count" -gt 0 ]; then
        echo -e "${YELLOW}⚠ WARNING: Specification contains $vague_count vague terms${NC}"
        echo -e "  Vague keywords found: $(echo "$prompt" | grep -io -E "(TBD|TODO|maybe|probably|might|should|unclear|undefined|TK|TBC)" | sort -u | tr '\n' ' ')"
        return 1
    fi
    return 0
}

# Check if specification is reasonably complete
check_specification_completeness() {
    local prompt="$1"
    local word_count=$(echo "$prompt" | wc -w)

    # Very short specifications likely incomplete (heuristic)
    if [ "$word_count" -lt 20 ]; then
        echo -e "${YELLOW}⚠ WARNING: Specification may be incomplete (only $word_count words)${NC}"
        echo -e "  Provide clear inputs, outputs, constraints, and acceptance criteria"
        return 1
    fi

    # Check for key specification elements
    local has_inputs=0
    local has_outputs=0
    local has_criteria=0

    echo "$prompt" | grep -iE "(input|parameter|argument|receive|given)" > /dev/null 2>&1 && has_inputs=1
    echo "$prompt" | grep -iE "(output|return|produce|result|generate)" > /dev/null 2>&1 && has_outputs=1
    echo "$prompt" | grep -iE "(must|should|expect|verify|assert|test|scenario|acceptance|requirement)" > /dev/null 2>&1 && has_criteria=1

    local coverage=$((has_inputs + has_outputs + has_criteria))

    if [ "$coverage" -lt 2 ]; then
        echo -e "${YELLOW}⚠ WARNING: Specification may be incomplete${NC}"
        echo -e "  Include:"
        [ "$has_inputs" -eq 0 ] && echo -e "    - Clear input/parameter description"
        [ "$has_outputs" -eq 0 ] && echo -e "    - Clear output/result specification"
        [ "$has_criteria" -eq 0 ] && echo -e "    - Acceptance criteria or test scenarios"
    fi

    return 0
}

main() {
    # Read the prompt from stdin or argument
    local prompt="${1:--}"
    if [ "$prompt" = "-" ]; then
        prompt=$(cat)
    fi

    # Skip check for trivial tasks
    if ! detect_non_trivial "$prompt"; then
        echo -e "${GREEN}✓ Trivial task detected (skip specification check)${NC}"
        exit 0
    fi

    echo -e "${GREEN}→ Specification Pre-Check${NC}"

    # Check specification openness (has vague keywords?)
    if ! check_specification_openness "$prompt"; then
        echo -e "${RED}✗ ISSUE: Specification contains vague terms${NC}"
        echo -e "  Action: Clarify vague terms before EPIC 9 fan-out"
    fi

    # Check specification completeness
    if ! check_specification_completeness "$prompt"; then
        echo -e "${YELLOW}⚠ CAUTION: Specification may lack detail${NC}"
        echo -e "  Recommendation: Use /speckit-verify to validate closure"
    fi

    echo -e "${GREEN}✓ Pre-check complete (proceed with caution if warnings above)${NC}"
    exit 0
}

main "$@"
