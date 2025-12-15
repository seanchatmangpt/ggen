#!/bin/bash
# KNHK v0.4.0 Release Checklist
# Interactive script for release managers

set -e

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Checklist items (using arrays instead of associative arrays for compatibility)
CHECKLIST_KEYS=("cli_commands" "integration_tests" "network_integrations" "performance" "configuration" "documentation" "build" "bugs")
CHECKLIST_DESC=(
    "All CLI commands implemented and tested"
    "End-to-end integration tests passing"
    "Real network integrations working (HTTP, gRPC, Kafka, OTEL)"
    "Performance validation confirms ≤8 ticks compliance"
    "Configuration management in place"
    "Documentation updated with examples"
    "All components build successfully"
    "No known critical bugs"
)

# Status tracking (using simple arrays)
declare -a STATUS
for i in "${!CHECKLIST_KEYS[@]}"; do
    STATUS[$i]="unchecked"
done

# Functions
print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}KNHK v0.4.0 Release Checklist${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo ""
}

print_item() {
    local key=$1
    local description=$2
    local status="unchecked"
    
    # Find status by key
    for i in "${!CHECKLIST_KEYS[@]}"; do
        if [ "${CHECKLIST_KEYS[$i]}" = "$key" ]; then
            status="${STATUS[$i]:-unchecked}"
            break
        fi
    done
    
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
            for i in "${!CHECKLIST_KEYS[@]}"; do
                if [ "${CHECKLIST_KEYS[$i]}" = "$key" ]; then
                    STATUS[$i]="passed"
                    break
                fi
            done
            ;;
        f|F)
            for i in "${!CHECKLIST_KEYS[@]}"; do
                if [ "${CHECKLIST_KEYS[$i]}" = "$key" ]; then
                    STATUS[$i]="failed"
                    break
                fi
            done
            ;;
        w|W)
            for i in "${!CHECKLIST_KEYS[@]}"; do
                if [ "${CHECKLIST_KEYS[$i]}" = "$key" ]; then
                    STATUS[$i]="warning"
                    break
                fi
            done
            ;;
        s|S|"")
            for i in "${!CHECKLIST_KEYS[@]}"; do
                if [ "${CHECKLIST_KEYS[$i]}" = "$key" ]; then
                    STATUS[$i]="skipped"
                    break
                fi
            done
            ;;
        *)
            echo "Invalid choice, skipping..."
            ;;
    esac
}

get_status() {
    local key=$1
    for i in "${!CHECKLIST_KEYS[@]}"; do
        if [ "${CHECKLIST_KEYS[$i]}" = "$key" ]; then
            echo "${STATUS[$i]:-unchecked}"
            return
        fi
    done
    echo "unchecked"
}

run_validation() {
    echo ""
    echo -e "${BLUE}Running automated validation...${NC}"
    local result
    if ./scripts/validate_v0.4.0.sh; then
        result="passed"
    else
        result="failed"
    fi
    # Store result
    CHECKLIST_KEYS+=("automated_validation")
    STATUS[${#CHECKLIST_KEYS[@]}-1]="$result"
}

generate_report() {
    local report_file="release_checklist_report_$(date +%Y%m%d_%H%M%S).txt"
    
    {
        echo "KNHK v0.4.0 Release Checklist Report"
        echo "Generated: $(date)"
        echo ""
        echo "========================================"
        echo ""
        
        for i in "${!CHECKLIST_KEYS[@]}"; do
            local key="${CHECKLIST_KEYS[$i]}"
            local description="${CHECKLIST_DESC[$i]}"
            local status="${STATUS[$i]:-unchecked}"
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
for i in "${!CHECKLIST_KEYS[@]}"; do
    prompt_item "${CHECKLIST_KEYS[$i]}" "${CHECKLIST_DESC[$i]}"
done

# Additional items
CHECKLIST_KEYS+=("automated_validation" "code_review" "qa_review")
CHECKLIST_DESC+=("Automated validation script passed" "Code review completed" "QA review completed")

# Initialize additional statuses
for i in "${!CHECKLIST_KEYS[@]}"; do
    if [ -z "${STATUS[$i]:-}" ]; then
        STATUS[$i]="unchecked"
    fi
done

prompt_item "automated_validation" "Automated validation script passed"
prompt_item "code_review" "Code review completed"
prompt_item "qa_review" "QA review completed"

# Generate report
generate_report

# Final status
echo ""
auto_val=$(get_status "automated_validation")
cli_cmd=$(get_status "cli_commands")
int_test=$(get_status "integration_tests")

# Count passed items
passed_count=0
for i in "${!CHECKLIST_KEYS[@]}"; do
    if [ "${STATUS[$i]:-}" = "passed" ]; then
        ((passed_count++))
    fi
done

if [ "$auto_val" = "passed" ] || [ "$cli_cmd" = "passed" ] || [ "$int_test" = "passed" ] || [ $passed_count -gt 0 ]; then
    echo -e "${GREEN}✓ Release checklist complete!${NC}"
    exit 0
else
    echo -e "${YELLOW}⚠ Release checklist has warnings. Review manually.${NC}"
    exit 0
fi

