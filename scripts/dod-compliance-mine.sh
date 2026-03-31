#!/usr/bin/env bash
# dod-compliance-mine.sh — Mine DoD compliance from git history
#
# Usage: ./dod-compliance-mine.sh [sprint_count] [git-log-args...]
#   Example: ./dod-compliance-mine.sh 4
#   Example: ./dod-compliance-mine.sh 4 --since="2026-01-01"
#
# Output: DoD compliance report to stdout
#
# Pipeline:
#   1. Extract git log with commit messages
#   2. Classify commits by DoD category (testing, verification, soundness, etc.)
#   3. Count compliance events per DoD step
#   4. Detect gaps (DoD steps with no evidence in commits)
#   5. Pareto analysis: which DoD steps are most often skipped
#   6. Generate compliance report

set -euo pipefail

# Configuration
SPRINT_COUNT="${1:-4}"
shift 2>/dev/null || true
GIT_EXTRA_ARGS=("${@:-}")

# Script directory (for finding git-to-xes.sh)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# DoD steps and their detection patterns
declare -A DOD_STEPS=(
    ["TDD Red Phase"]="testing"
    ["TDD Green Phase"]="testing"
    ["OTEL Span Proof"]="verification"
    ["Weaver Schema Conformance"]="verification"
    ["WvdA Deadlock Freedom"]="soundness"
    ["WvdA Liveness"]="soundness"
    ["WvdA Boundedness"]="soundness"
    ["Armstrong Supervision"]="soundness"
    ["Armstrong No Shared State"]="soundness"
    ["Commit Convention"]="documentation"
    ["Compiler Clean"]="testing"
)

# Detection patterns for each DoD step
declare -A DOD_PATTERNS=(
    ["TDD Red Phase"]="test|tdd|red.?phase|failing.?test|_test\.|_test\.exs|_test\.rs"
    ["TDD Green Phase"]="pass|green.?phase|implement|fix.?test|cargo.?test|mix.?test|go.?test"
    ["OTEL Span Proof"]="otel|opentelemetry|span|trace|telemetry|jaeger"
    ["Weaver Schema Conformance"]="weaver|semconv|registry.?check|spans\.yaml"
    ["WvdA Deadlock Freedom"]="deadlock|timeout|blocking|wait|timeout_ms"
    ["WvdA Liveness"]="liveness|bounded|loop|escape|iteration"
    ["WvdA Boundedness"]="bounded|queue|cache|max_size|limit|evict"
    ["Armstrong Supervision"]="supervision|supervisor|restart|let.?it.?crash"
    ["Armstrong No Shared State"]="shared.?state|message.?passing|no.?mutex|channel"
    ["Commit Convention"]="^(feat|fix|docs|refactor|test|chore|perf|ci|build|style)\("
    ["Compiler Clean"]="clippy|warnings.?as.?errors|go.?vet|cargo.?fmt|mix.?format|compiler"
)

# Severity for each DoD gap
declare -A DOD_SEVERITY=(
    ["TDD Red Phase"]="high"
    ["TDD Green Phase"]="high"
    ["OTEL Span Proof"]="high"
    ["Weaver Schema Conformance"]="high"
    ["WvdA Deadlock Freedom"]="critical"
    ["WvdA Liveness"]="critical"
    ["WvdA Boundedness"]="critical"
    ["Armstrong Supervision"]="high"
    ["Armstrong No Shared State"]="high"
    ["Commit Convention"]="medium"
    ["Compiler Clean"]="medium"
)

# Colors for terminal output (if stdout is a terminal)
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    NC='\033[0m'
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    NC=''
fi

# Count commits matching a pattern
count_pattern_matches() {
    local pattern="$1"
    local count
    count=$(git log --format="%s" --all "${GIT_EXTRA_ARGS[@]}" 2>/dev/null \
        | grep -ciE "$pattern" || true)
    echo "$count"
}

# Total commits in the log
total_commits() {
    local count
    count=$(git log --format="%H" --all "${GIT_EXTRA_ARGS[@]}" 2>/dev/null | wc -l | tr -d ' ')
    echo "$count"
}

# Generate sprint date ranges (approximate, based on commit history)
generate_sprint_ranges() {
    local count="$1"
    local total
    total=$(total_commits)
    local per_sprint=$(( (total + count - 1) / count ))

    for i in $(seq 0 $((count - 1))); do
        local skip=$((i * per_sprint))
        local end_skip=$(( (i + 1) * per_sprint ))
        local sprint_name="Sprint $((i + 1))"
        local commits
        commits=$(git log --format="%H" --all "${GIT_EXTRA_ARGS[@]}" --skip="$skip" -n "$per_sprint" 2>/dev/null | wc -l | tr -d ' ')
        echo "${sprint_name}|${commits}"
    done
}

# Print report header
print_header() {
    local project_name
    project_name=$(basename "$(git rev-parse --show-toplevel 2>/dev/null || echo "unknown")")
    local total
    total=$(total_commits)

    echo "# Definition of Done (DoD) Compliance Report"
    echo ""
    echo "> **Project:** ${project_name}"
    echo "> **Sprints Analyzed:** ${SPRINT_COUNT}"
    echo "> **Total Commits Analyzed:** ${total}"
    echo "> **Generated:** $(date -u +"%Y-%m-%d %H:%M:%S UTC")"
    echo "> **Source:** Git history process mining via ggen"
    echo ""
}

# Print DoD step compliance breakdown
print_step_breakdown() {
    local total
    total=$(total_commits)
    echo "## DoD Step Compliance Breakdown"
    echo ""
    echo "| DoD Step | Category | Evidence Count | Compliance Rate |"
    echo "|----------|----------|---------------|----------------|"

    for step in "${!DOD_STEPS[@]}"; do
        local pattern="${DOD_PATTERNS[$step]}"
        local category="${DOD_STEPS[$step]}"
        local count
        count=$(count_pattern_matches "$pattern")
        local rate
        if [ "$total" -gt 0 ]; then
            rate=$(( (count * 100) / total ))
        else
            rate=0
        fi

        # Color code the rate
        if [ "$rate" -ge 80 ]; then
            printf "| %-30s | %-12s | %13d | ${GREEN}%-14s${NC} |\n" "$step" "$category" "$count" "${rate}%"
        elif [ "$rate" -ge 50 ]; then
            printf "| %-30s | %-12s | %13d | ${YELLOW}%-14s${NC} |\n" "$step" "$category" "$count" "${rate}%"
        else
            printf "| %-30s | %-12s | %13d | ${RED}%-14s${NC} |\n" "$step" "$category" "$count" "${rate}%"
        fi
    done
    echo ""
}

# Print Pareto analysis
print_pareto() {
    local total
    total=$(total_commits)

    echo "## Pareto Analysis: Most Frequently Skipped DoD Steps"
    echo ""
    echo "> 80/20 rule: The top 20% of skipped steps account for 80% of compliance gaps."
    echo ""

    # Collect gap data
    local gaps=()
    local total_gaps=0

    for step in "${!DOD_STEPS[@]}"; do
        local pattern="${DOD_PATTERNS[$step]}"
        local count
        count=$(count_pattern_matches "$pattern")
        local gap=$((total - count))
        local severity="${DOD_SEVERITY[$step]}"

        if [ "$gap" -gt 0 ]; then
            gaps+=("$step|$gap|$severity")
            total_gaps=$((total_gaps + gap))
        fi
    done

    # Sort by gap frequency (descending)
    IFS=$'\n' sorted_gaps=($(sort -t'|' -k2 -rn <<<"${gaps[*]}"))
    unset IFS

    echo "| Rank | DoD Step | Gap Count | % of Total Gaps | Cumulative % |"
    echo "|------|----------|-----------|-----------------|--------------|"

    local cumulative=0
    local rank=1
    for entry in "${sorted_gaps[@]}"; do
        IFS='|' read -r step gap severity <<< "$entry"
        local percent=0
        if [ "$total_gaps" -gt 0 ]; then
            percent=$(( (gap * 100) / total_gaps ))
        fi
        cumulative=$((cumulative + percent))

        if [ "$rank" -le 3 ]; then
            printf "| %-4d | ${RED}%-30s${NC} | %-9d | %-15s | %-12d%% |\n" "$rank" "$step" "$gap" "${percent}%" "$cumulative"
        else
            printf "| %-4d | %-30s | %-9d | %-15s | %-12d%% |\n" "$rank" "$step" "$gap" "${percent}%" "$cumulative"
        fi

        rank=$((rank + 1))
    done
    echo ""

    # Pareto insight
    if [ ${#sorted_gaps[@]} -gt 0 ]; then
        echo "### Pareto Insight"
        echo ""
        local top_count
        top_count=$((${#sorted_gaps[@]} / 5 + 1))
        echo "The top ${top_count} skipped steps account for the majority of all compliance gaps."
        echo "Focus remediation on:"
        echo ""

        local idx=0
        for entry in "${sorted_gaps[@]}"; do
            [ "$idx" -ge "$top_count" ] && break
            IFS='|' read -r step gap severity <<< "$entry"
            echo "${idx}. **${step}** -- ${gap} gaps (severity: ${severity})"
            idx=$((idx + 1))
        done
        echo ""
    else
        echo "### Pareto Insight"
        echo ""
        echo "No compliance gaps detected. All DoD steps have evidence in commit history."
        echo ""
    fi
}

# Print sprint breakdown
print_sprint_breakdown() {
    echo "## Sprint Breakdown"
    echo ""

    while IFS='|' read -r sprint_name commits; do
        echo "### ${sprint_name}"
        echo "- **Commits:** ${commits}"
        echo ""

        # Count DoD events in this sprint's commits
        local sprint_events=0
        for step in "${!DOD_STEPS[@]}"; do
            local pattern="${DOD_PATTERNS[$step]}"
            # Note: This counts across all commits for simplicity
            # A full implementation would scope to sprint date ranges
            :
        done

        echo ""
    done < <(generate_sprint_ranges "$SPRINT_COUNT")
}

# Print recommendations
print_recommendations() {
    local total
    total=$(total_commits)

    echo "## Recommendations"
    echo ""

    # Find critical gaps
    for step in "${!DOD_STEPS[@]}"; do
        local pattern="${DOD_PATTERNS[$step]}"
        local count
        count=$(count_pattern_matches "$pattern")
        local severity="${DOD_SEVERITY[$step]}"

        if [ "$severity" = "critical" ] && [ "$count" -eq 0 ]; then
            echo "### CRITICAL: ${step} has no evidence"
            echo "- **Impact:** No commits reference ${step}"
            echo "- **Action:** Add pre-commit hook to detect missing ${step} evidence"
            echo "- **Action:** Update PR template to require ${step} verification"
            echo ""
        fi
    done

    echo "### Process Improvement"
    echo ""
    echo "1. **Automate Detection:** Add a \`make verify\` target that checks all DoD steps."
    echo "2. **Visible Dashboard:** Publish compliance scores in a public dashboard (Visual Management / Toyota TPS)."
    echo "3. **Weekly Kaizen:** Review one DoD step per week. Fix the process, not the people."
    echo "4. **Gemba Walk:** At least one person runs the full DoD checklist manually each week."
    echo ""
}

# Print XES event log generation info
print_xes_info() {
    echo "---"
    echo ""
    echo "## XES Event Log"
    echo ""
    echo "To generate a full XES event log for further process mining analysis:"
    echo ""
    echo '```bash'
    echo "${SCRIPT_DIR}/git-to-xes.sh ${GIT_EXTRA_ARGS[*]} > dod-events.xes"
    echo '```'
    echo ""
    echo "The XES log can be loaded into process mining tools (ProM, pm4py, Celonis) for:"
    echo "- Process discovery (Petri net mining)"
    echo "- Conformance checking"
    echo "- Bottleneck analysis"
    echo "- Social network analysis (developer collaboration)"
    echo ""
}

# Main
main() {
    # Verify we are in a git repository
    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "Error: Not a git repository. Run this script from within a git project." >&2
        exit 1
    fi

    print_header
    print_step_breakdown
    print_pareto
    print_sprint_breakdown
    print_recommendations
    print_xes_info
}

main "$@"
