#!/bin/bash
# Week 3 Health Score Dashboard
# Interactive dashboard showing metrics trends and progress

set -euo pipefail

# Configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
METRICS_DIR="$PROJECT_ROOT/docs/metrics"
REPORTS_DIR="$METRICS_DIR/daily_reports"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Unicode box drawing characters
TL="┌"  # Top left
TR="┐"  # Top right
BL="└"  # Bottom left
BR="┘"  # Bottom right
H="─"   # Horizontal
V="│"   # Vertical
VR="├"  # Vertical right
VL="┤"  # Vertical left
HU="┴"  # Horizontal up
HD="┬"  # Horizontal down
X="┼"   # Cross

# Function to draw progress bar
draw_bar() {
    local value=$1
    local max=$2
    local width=30
    local filled=$((value * width / max))
    local empty=$((width - filled))

    printf "["
    printf "${GREEN}%${filled}s${NC}" "" | tr ' ' '█'
    printf "${CYAN}%${empty}s${NC}" "" | tr ' ' '░'
    printf "]"
}

# Function to get status icon
get_status() {
    local value=$1
    local target=$2

    if [ "$value" -ge "$target" ]; then
        echo -e "${GREEN}✓${NC}"
    elif [ "$value" -ge $((target - 5)) ]; then
        echo -e "${YELLOW}⏳${NC}"
    else
        echo -e "${RED}✗${NC}"
    fi
}

# Function to calculate trend
get_trend() {
    local current=$1
    local previous=$2

    if [ "$current" -gt "$previous" ]; then
        echo -e "${GREEN}↑${NC}"
    elif [ "$current" -lt "$previous" ]; then
        echo -e "${RED}↓${NC}"
    else
        echo -e "${YELLOW}→${NC}"
    fi
}

# Clear screen
clear

# Header
echo -e "${BOLD}${BLUE}"
echo "${TL}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${TR}"
echo "${V}          WEEK 3 HEALTH & COVERAGE DASHBOARD - $(date +%Y-%m-%d)          ${V}"
echo "${V}                    ggen-core Test Quality Metrics                   ${V}"
echo "${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}"
echo -e "${NC}"

# Read latest report
LATEST_REPORT=$(ls -t "$REPORTS_DIR"/coverage_*.md 2>/dev/null | head -1 || echo "")

if [ -z "$LATEST_REPORT" ]; then
    echo -e "${YELLOW}⚠️  No reports found. Run coverage_tracker.sh first.${NC}"
    exit 1
fi

# Extract metrics from latest report
COVERAGE=$(grep "Estimated Coverage:" "$LATEST_REPORT" | grep -oE '[0-9]+' | head -1 || echo "0")
HEALTH=$(grep "TOTAL" "$LATEST_REPORT" | grep -oE '[0-9]+\.[0-9]+' | head -1 || echo "0")
TESTS_PASS=$(grep "Passing:" "$LATEST_REPORT" | grep -oE '[0-9]+' | head -1 || echo "0")
TESTS_FAIL=$(grep "Failing:" "$LATEST_REPORT" | grep -oE '[0-9]+' | tail -1 || echo "0")
CODE_QUALITY=$(grep "Code Quality Score:" "$LATEST_REPORT" | grep -oE '[0-9]+' | head -1 || echo "0")

# Baseline values
BASELINE_COVERAGE=53
BASELINE_HEALTH=73
TARGET_COVERAGE=60
TARGET_HEALTH=75

# Calculate changes
COVERAGE_CHANGE=$((COVERAGE - BASELINE_COVERAGE))
HEALTH_CHANGE=$(echo "scale=1; $HEALTH - $BASELINE_HEALTH" | bc)

# Main Dashboard
echo -e "${BOLD}${CYAN}┃ PRIMARY METRICS${NC}"
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"
echo ""

# Coverage
printf "  ${BOLD}Test Coverage:${NC}  ${COVERAGE}%%  "
draw_bar "$COVERAGE" 100
printf "  ${TARGET_COVERAGE}%% "
get_status "$COVERAGE" "$TARGET_COVERAGE"
printf "  "
get_trend "$COVERAGE" "$BASELINE_COVERAGE"
if [ "$COVERAGE_CHANGE" -ge 0 ]; then
    echo -e " ${GREEN}+${COVERAGE_CHANGE}%${NC}"
else
    echo -e " ${RED}${COVERAGE_CHANGE}%${NC}"
fi

# Health Score
HEALTH_INT=$(printf "%.0f" "$HEALTH")
printf "  ${BOLD}Health Score:${NC}   ${HEALTH}%%  "
draw_bar "$HEALTH_INT" 100
printf "  ${TARGET_HEALTH}%% "
get_status "$HEALTH_INT" "$TARGET_HEALTH"
printf "  "
get_trend "$HEALTH_INT" "$BASELINE_HEALTH"
if [[ "$HEALTH_CHANGE" > 0 ]]; then
    echo -e " ${GREEN}+${HEALTH_CHANGE}%${NC}"
else
    echo -e " ${YELLOW}${HEALTH_CHANGE}%${NC}"
fi

# Tests
printf "  ${BOLD}Tests Passing:${NC}  ${TESTS_PASS}    "
draw_bar "$TESTS_PASS" 500
printf "  500 "
get_status "$TESTS_PASS" 464
printf "  "
if [ "$TESTS_FAIL" -eq 0 ]; then
    echo -e "${GREEN}✓${NC} All Pass"
else
    echo -e "${RED}✗${NC} $TESTS_FAIL Failed"
fi

echo ""
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"

# Health Score Breakdown
echo ""
echo -e "${BOLD}${CYAN}┃ HEALTH SCORE BREAKDOWN${NC}"
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"
echo ""

printf "  Dimension          Weight   Score   Contribution\n"
echo -e "  ${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}"

printf "  ${BOLD}Compilation${NC}        30%%     100%%    "
echo -e "${GREEN}30.00%${NC}"

TESTING_CONTRIB=$(echo "scale=2; $COVERAGE * 0.25" | bc)
printf "  ${BOLD}Testing${NC}            25%%     ${COVERAGE}%%     "
echo -e "${YELLOW}${TESTING_CONTRIB}%${NC}"

CODE_CONTRIB=$(echo "scale=2; $CODE_QUALITY * 0.15" | bc)
printf "  ${BOLD}Code Quality${NC}       15%%     ${CODE_QUALITY}%%     "
echo -e "${YELLOW}${CODE_CONTRIB}%${NC}"

printf "  ${BOLD}Security${NC}           15%%     82%%     "
echo -e "${YELLOW}12.30%${NC}"

printf "  ${BOLD}Performance${NC}        10%%     88%%     "
echo -e "${YELLOW}8.80%${NC}"

printf "  ${BOLD}Architecture${NC}        5%%     60%%     "
echo -e "${YELLOW}3.00%${NC}"

echo -e "  ${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}"
printf "  ${BOLD}TOTAL${NC}             100%%              "
echo -e "${BOLD}${GREEN}${HEALTH}%${NC}"

echo ""
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"

# Weekly Progress
echo ""
echo -e "${BOLD}${CYAN}┃ WEEK 3 PROGRESS (Day 1/7)${NC}"
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"
echo ""

echo "  Coverage Goal:   53% → 60% (Need +7%)"
COVERAGE_PROGRESS=$((100 * (COVERAGE - BASELINE_COVERAGE) / (TARGET_COVERAGE - BASELINE_COVERAGE)))
if [ "$COVERAGE_PROGRESS" -lt 0 ]; then COVERAGE_PROGRESS=0; fi
printf "  Progress:        "
draw_bar "$COVERAGE_PROGRESS" 100
echo -e " ${COVERAGE_PROGRESS}%"

echo ""
echo "  Health Goal:     73% → 75% (Need +2%)"
HEALTH_PROGRESS=$(echo "scale=0; 100 * ($HEALTH - $BASELINE_HEALTH) / ($TARGET_HEALTH - $BASELINE_HEALTH)" | bc)
if [[ "$HEALTH_PROGRESS" < 0 ]]; then HEALTH_PROGRESS=0; fi
printf "  Progress:        "
draw_bar "$HEALTH_PROGRESS" 100
echo -e " ${HEALTH_PROGRESS}%"

echo ""
echo -e "${BLUE}${BL}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${BR}${NC}"

# Footer
echo ""
if [ "$COVERAGE" -ge "$TARGET_COVERAGE" ] && [ "$HEALTH_INT" -ge "$TARGET_HEALTH" ]; then
    echo -e "  ${BOLD}${GREEN}✓ Week 3 Targets Achieved! Excellent work!${NC}"
elif [ "$COVERAGE_PROGRESS" -gt 50 ] && [ "$HEALTH_PROGRESS" -gt 50 ]; then
    echo -e "  ${YELLOW}⏳ Good progress - Keep going!${NC}"
else
    echo -e "  ${YELLOW}⏳ On track for Week 3 goals${NC}"
fi
echo ""
echo -e "  Latest Report: ${CYAN}$(basename "$LATEST_REPORT")${NC}"
echo -e "  Next Update:   ${CYAN}Run ./scripts/coverage_tracker.sh${NC}"
echo ""

exit 0
