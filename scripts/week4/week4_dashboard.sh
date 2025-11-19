#!/bin/bash
# Week 4 Interactive Dashboard
# Comprehensive metrics visualization and trend analysis

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
METRICS_DIR="$PROJECT_ROOT/docs/metrics/week4"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
BOLD='\033[1m'
NC='\033[0m'

# Unicode characters
TL="┌"; TR="┐"; BL="└"; BR="┘"; H="─"; V="│"
VR="├"; VL="┤"; HU="┴"; HD="┬"; X="┼"

# Draw progress bar
draw_bar() {
    local value=$1
    local max=$2
    local width=40
    local filled=$((value * width / max))
    local empty=$((width - filled))

    printf "["
    printf "${GREEN}%${filled}s${NC}" "" | tr ' ' '█'
    printf "${CYAN}%${empty}s${NC}" "" | tr ' ' '░'
    printf "]"
}

# Get status icon
get_status() {
    local value=$1
    local target=$2

    if [ "$value" -ge "$target" ]; then
        echo -e "${GREEN}✓${NC}"
    elif [ "$value" -ge $((target - 3)) ]; then
        echo -e "${YELLOW}⏳${NC}"
    else
        echo -e "${RED}✗${NC}"
    fi
}

# Get trend arrow
get_trend() {
    local current=$1
    local previous=$2

    if [ "$current" -gt "$previous" ]; then
        echo -e "${GREEN}↗${NC}"
    elif [ "$current" -lt "$previous" ]; then
        echo -e "${RED}↘${NC}"
    else
        echo -e "${YELLOW}→${NC}"
    fi
}

clear

# Header
echo -e "${BOLD}${BLUE}"
cat <<EOF
${TL}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${TR}
${V}          WEEK 4: METRICS TRACKING & HEALTH SCORE VALIDATION              ${V}
${V}               Health Score Journey: 81% → 85% (Days 1-5)                 ${V}
${V}                     Generated: $(date '+%Y-%m-%d %H:%M:%S')                      ${V}
${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}
EOF
echo -e "${NC}"

# Read latest report
LATEST_REPORT=$(ls -t "$METRICS_DIR"/daily/*.md 2>/dev/null | head -1 || echo "")

if [ -z "$LATEST_REPORT" ]; then
    echo -e "${YELLOW}⚠️  No reports found. Run daily_tracker.sh first.${NC}"
    exit 1
fi

# Extract metrics
HEALTH=$(grep "^| \*\*Health Score\*\*" "$LATEST_REPORT" | awk -F'|' '{print $3}' | tr -d ' %' || echo "81")
COVERAGE=$(grep "^| \*\*Coverage\*\*" "$LATEST_REPORT" | awk -F'|' '{print $3}' | tr -d ' %' || echo "50")
TESTS=$(grep "^| \*\*Tests Passing\*\*" "$LATEST_REPORT" | awk -F'|' '{print $3}' | tr -d ' ' || echo "464")
COMPILE=$(grep "^| \*\*Compilation\*\*" "$LATEST_REPORT" | awk -F'|' '{print $3}' | tr -d ' %' || echo "100")

# Baseline values
BASELINE_HEALTH=81
TARGET_HEALTH=85
BASELINE_COVERAGE=50
TARGET_COVERAGE=65
BASELINE_TESTS=464
TARGET_TESTS=560

# Calculate changes
HEALTH_CHANGE=$(echo "scale=1; $HEALTH - $BASELINE_HEALTH" | bc)
COVERAGE_CHANGE=$((COVERAGE - BASELINE_COVERAGE))
TESTS_CHANGE=$((TESTS - BASELINE_TESTS))

# Primary Metrics Section
echo -e "${BOLD}${CYAN}┃ PRIMARY METRICS - WEEK 4 TARGETS${NC}"
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"
echo ""

# Health Score
printf "  ${BOLD}Health Score:${NC}    ${HEALTH}%%  "
draw_bar "$HEALTH" 100
printf "  ${TARGET_HEALTH}%% "
get_status "$HEALTH" "$TARGET_HEALTH"
printf "  "
get_trend "$HEALTH" "$BASELINE_HEALTH"
if [[ "$HEALTH_CHANGE" > 0 ]]; then
    echo -e " ${GREEN}+${HEALTH_CHANGE}%${NC}"
else
    echo -e " ${YELLOW}${HEALTH_CHANGE}%${NC}"
fi

# Coverage
printf "  ${BOLD}Test Coverage:${NC}   ${COVERAGE}%%  "
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

# Tests
printf "  ${BOLD}Tests Passing:${NC}   ${TESTS}  "
draw_bar "$TESTS" "$TARGET_TESTS"
printf "  ${TARGET_TESTS} "
get_status "$TESTS" "$BASELINE_TESTS"
printf "  "
get_trend "$TESTS" "$BASELINE_TESTS"
if [ "$TESTS_CHANGE" -ge 0 ]; then
    echo -e " ${GREEN}+${TESTS_CHANGE}${NC}"
else
    echo -e " ${RED}${TESTS_CHANGE}${NC}"
fi

echo ""
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"

# Health Score Breakdown
echo ""
echo -e "${BOLD}${CYAN}┃ HEALTH SCORE BREAKDOWN (Target: 85%)${NC}"
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"
echo ""

printf "  ${BOLD}Dimension          Weight   Current  Target   Contribution${NC}\n"
echo -e "  ${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}"

printf "  ${BOLD}Compilation${NC}        30%%     ${COMPILE}%%    100%%    "
COMPILE_CONTRIB=$(echo "scale=2; $COMPILE * 0.30" | bc)
echo -e "${GREEN}${COMPILE_CONTRIB}% / 30.0%${NC}"

printf "  ${BOLD}Testing${NC}            25%%     ${COVERAGE}%%     65%%     "
TESTING_CONTRIB=$(echo "scale=2; $COVERAGE * 0.25" | bc)
TARGET_TESTING=$(echo "scale=2; 65 * 0.25" | bc)
if [ "${TESTING_CONTRIB%.*}" -ge "${TARGET_TESTING%.*}" ]; then
    echo -e "${GREEN}${TESTING_CONTRIB}% / ${TARGET_TESTING}%${NC}"
else
    echo -e "${YELLOW}${TESTING_CONTRIB}% / ${TARGET_TESTING}%${NC}"
fi

CODE_QUALITY=$(grep "Code Quality Score:" "$LATEST_REPORT" | grep -oE '[0-9]+' | head -1 || echo "96")
printf "  ${BOLD}Code Quality${NC}       15%%     ${CODE_QUALITY}%%     96%%     "
CODE_CONTRIB=$(echo "scale=2; $CODE_QUALITY * 0.15" | bc)
echo -e "${YELLOW}${CODE_CONTRIB}% / 14.4%${NC}"

printf "  ${BOLD}Security${NC}           15%%     82%%     85%%     "
echo -e "${YELLOW}12.30% / 12.75%${NC}"

printf "  ${BOLD}Performance${NC}        10%%     88%%     92%%     "
echo -e "${YELLOW}8.80% / 9.20%${NC}"

printf "  ${BOLD}Architecture${NC}        5%%     60%%     65%%     "
echo -e "${YELLOW}3.00% / 3.25%${NC}"

echo -e "  ${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}"
printf "  ${BOLD}TOTAL${NC}             100%%     ${HEALTH}%%     85%%     "
echo -e "${BOLD}${GREEN}${HEALTH}% / 85.0%${NC}"

echo ""
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"

# Week 4 Progress Trajectory
echo ""
echo -e "${BOLD}${CYAN}┃ WEEK 4 TRAJECTORY (81% → 85%)${NC}"
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"
echo ""

# Health score progress
echo "  ${BOLD}Health Score Progress: 81% → 85% (Need +4 points)${NC}"
HEALTH_PROGRESS=$(echo "scale=0; 100 * ($HEALTH - 81) / 4" | bc)
if [ "$HEALTH_PROGRESS" -lt 0 ]; then HEALTH_PROGRESS=0; fi
if [ "$HEALTH_PROGRESS" -gt 100 ]; then HEALTH_PROGRESS=100; fi
printf "  Current: ${HEALTH}%  "
draw_bar "$HEALTH_PROGRESS" 100
echo -e " ${HEALTH_PROGRESS}%"

echo ""
echo "  ${BOLD}Coverage Progress: 50% → 65% (Need +15 points)${NC}"
COVERAGE_PROGRESS=$(echo "scale=0; 100 * ($COVERAGE - 50) / 15" | bc)
if [ "$COVERAGE_PROGRESS" -lt 0 ]; then COVERAGE_PROGRESS=0; fi
if [ "$COVERAGE_PROGRESS" -gt 100 ]; then COVERAGE_PROGRESS=100; fi
printf "  Current: ${COVERAGE}%  "
draw_bar "$COVERAGE_PROGRESS" 100
echo -e " ${COVERAGE_PROGRESS}%"

echo ""
echo "  ${BOLD}Daily Targets:${NC}"
echo "    Day 1: 81.0% health, 55% coverage (Compilation fix)"
echo "    Day 2: 81.5% health, 52% coverage (+25 tests)"
echo "    Day 3: 82.0% health, 54% coverage (+55 tests)"
echo "    Day 4: 84.0% health, 58% coverage (+85 tests)"
echo "    Day 5: 85.0% health, 65% coverage (+100 tests) ✅ TARGET"

echo ""
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"

# Coverage by Module
echo ""
echo -e "${BOLD}${CYAN}┃ COVERAGE BY MODULE (Week 4 Targets)${NC}"
echo -e "${BLUE}${VR}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${VL}${NC}"
echo ""

cat <<EOF
  ${BOLD}Module              Current  Target   Gap    Priority${NC}
  ${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}
  graph/core.rs       10%      40%     30%    🔴 Critical
  ontology/*.rs       15%      30%     15%    🔴 Critical
  generator.rs        52%      70%     18%    🟡 High
  templates/*.rs      50%      65%     15%    🟡 High
  cli/*.rs            40%      65%     25%    🟡 High
  utils/*.rs          60%      75%     15%    🟢 Medium
  marketplace/*.rs    38%      55%     17%    🟠 Medium
EOF

echo ""
echo -e "${BLUE}${BL}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${H}${BR}${NC}"

# Footer
echo ""
if [ "$HEALTH" -ge "$TARGET_HEALTH" ] && [ "$COVERAGE" -ge "$TARGET_COVERAGE" ]; then
    echo -e "  ${BOLD}${GREEN}🎉 Week 4 Targets ACHIEVED! Health: ${HEALTH}% ≥ 85%, Coverage: ${COVERAGE}% ≥ 65%${NC}"
elif [ "$HEALTH_PROGRESS" -gt 75 ] && [ "$COVERAGE_PROGRESS" -gt 75 ]; then
    echo -e "  ${BOLD}${GREEN}✓ Excellent progress! On track to achieve Week 4 targets.${NC}"
elif [ "$HEALTH_PROGRESS" -gt 50 ] && [ "$COVERAGE_PROGRESS" -gt 50 ]; then
    echo -e "  ${YELLOW}⏳ Good progress - Continue pushing toward 85% health, 65% coverage.${NC}"
else
    echo -e "  ${YELLOW}⚠️ Behind schedule - Prioritize test coverage and code quality improvements.${NC}"
fi

echo ""
echo -e "  Latest Report: ${CYAN}$(basename "$LATEST_REPORT")${NC}"
echo -e "  Update Tracker: ${CYAN}./scripts/week4/daily_tracker.sh${NC}"
echo -e "  View Dashboard: ${CYAN}./scripts/week4/week4_dashboard.sh${NC}"
echo ""

exit 0
