#!/bin/bash
# KNHKS v0.4.0 Release Checklist
# Interactive script for release managers

set -euo pipefail

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Checklist items
declare -A CHECKLIST=(
    ["cli_commands"]="All CLI commands implemented and tested"
    ["integration_tests"]="End-to-end integration tests passing"
    ["network_integrations"]="Real network integrations working (HTTP, gRPC, Kafka, OTEL)"
    ["performance"]="Performance validation confirms ≤8 ticks compliance"
    ["configuration"]="Configuration management in place"
    ["documentation"]="Documentation updated with examples"
    ["build"]="All components build successfully"
    ["bugs"]="No known critical bugs"
)

# Status tracking
declare -A STATUS=()

# Functions
print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}KNHKS v0.4.0 Release Checklist${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo ""
}

print_item() {
    local key=$1
    local description=$2
    local status=${STATUS[$key]:-unchecked}
    
    case $status in
        "passed")
            echo -e "${GREEN}✓${NC} $description"
            ;;
        "failed")
            echo -e "${RED}✗${NC} $description"
            ;;
        "warning")
            echo -e "${YELLOW}⚠${NC} $description"
            ;;
        *)
            echo -e "☐ $description"
            ;;
    esac
}

prompt_item() {
    local key=$1
    local description=$2
    
    echo ""
    echo -e "${BLUE}$description${NC}"
    echo "Status:"
    echo "  [p] Passed"
    echo "  [f] Failed"
    echo "  [w] Warning"
    echo "  [s] Skip"
    echo -n "Choice: "
    
    read -r choice
    case $choice in
        p|P)
            STATUS[$key]="passed"
            ;;
        f|F)
            STATUS[$key]="failed"
            ;;
        w|W)
            STATUS[$key]="warning"
            ;;
        s|S|"")
            STATUS[$key]="skipped"
            ;;
        *)
            echo "Invalid choice, skipping..."
            STATUS[$key]="skipped"
            ;;
    esac
}

run_validation() {
    echo ""
    echo -e "${BLUE}Running automated validation...${NC}"
    if ./scripts/validate_v0.4.0.sh; then
        STATUS["automated_validation"]="passed"
    else
        STATUS["automated_validation"]="failed"
    fi
}

generate_report() {
    local report_file="release_checklist_report_$(date +%Y%m%d_%H%M%S).txt"
    
    {
        echo "KNHKS v0.4.0 Release Checklist Report"
        echo "Generated: $(date)"
        echo ""
        echo "========================================"
        echo ""
        
        for key in "${!CHECKLIST[@]}"; do
            local description="${CHECKLIST[$key]}"
            local status=${STATUS[$key]:-unchecked}
            echo "[$status] $description"
        done
        
        echo ""
        echo "========================================"
        echo ""
        
        local passed=0
        local failed=0
        local warnings=0
        local skipped=0
        
        for status in "${STATUS[@]}"; do
            case $status in
                "passed")
                    ((passed++))
                    ;;
                "failed")
                    ((failed++))
                    ;;
                "warning")
                    ((warnings++))
                    ;;
                "skipped")
                    ((skipped++))
                    ;;
            esac
        done
        
        echo "Summary:"
        echo "  Passed: $passed"
        echo "  Failed: $failed"
        echo "  Warnings: $warnings"
        echo "  Skipped: $skipped"
        echo ""
        
        if [ $failed -eq 0 ]; then
            echo "Status: ✅ READY FOR RELEASE"
        else
            echo "Status: ❌ NOT READY FOR RELEASE"
        fi
    } > "$report_file"
    
    echo ""
    echo -e "${GREEN}Report saved to: $report_file${NC}"
    cat "$report_file"
}

# Main
print_header

echo "This script will guide you through the v0.4.0 release checklist."
echo "You can mark each item as passed, failed, warning, or skip."
echo ""

# Run automated validation first
echo -n "Run automated validation script? [Y/n]: "
read -r run_auto
if [[ "$run_auto" != "n" && "$run_auto" != "N" ]]; then
    run_validation
fi

# Go through each checklist item
for key in "${!CHECKLIST[@]}"; do
    prompt_item "$key" "${CHECKLIST[$key]}"
done

# Additional items
prompt_item "automated_validation" "Automated validation script passed"
prompt_item "code_review" "Code review completed"
prompt_item "qa_review" "QA review completed"

# Generate report
generate_report

# Final status
echo ""
if [ "${STATUS[automated_validation]:-}" = "passed" ] && [ "${STATUS[cli_commands]:-}" = "passed" ] && [ "${STATUS[integration_tests]:-}" = "passed" ]; then
    echo -e "${GREEN}✓ Release checklist complete!${NC}"
    exit 0
else
    echo -e "${RED}✗ Release checklist incomplete. Please review failed items.${NC}"
    exit 1
fi

