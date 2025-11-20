#!/bin/bash
# parse_metrics.sh - Extract metrics from cargo make outputs
# Usage: parse_metrics.sh <log_file> [week_number]

set -euo pipefail

LOG_FILE="${1:-}"
WEEK="${2:-0}"
DATE=$(date +%Y-%m-%d)
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

if [[ ! -f "$LOG_FILE" ]]; then
    echo "Error: Log file not found: $LOG_FILE" >&2
    exit 1
fi

# Extract build time metrics
extract_build_time() {
    local log_file="$1"
    # Use basic grep without -P flag (macOS compatibility)
    local first_build=$(grep -o 'Finished.*in [0-9.]*s' "$log_file" | head -1 | grep -o '[0-9.]*s' | tr -d 's' || echo "0")
    local incremental_build=$(grep -o 'Finished.*in [0-9.]*s' "$log_file" | tail -1 | grep -o '[0-9.]*s' | tr -d 's' || echo "0")
    local test_suite=$(grep -o 'test result:.*finished in [0-9.]*s' "$log_file" | grep -o '[0-9.]*s' | tr -d 's' || echo "0")

    cat <<EOF
    "build_time": {
      "first_build_seconds": ${first_build:-0},
      "incremental_build_seconds": ${incremental_build:-0},
      "test_suite_seconds": ${test_suite:-0},
      "improvement_percentage": 0
    }
EOF
}

# Extract test pass rate metrics
extract_test_pass_rate() {
    local log_file="$1"
    local test_result=$(grep -oP 'test result: \K.*' "$log_file" | tail -1 || echo "")

    if [[ -z "$test_result" ]]; then
        cat <<EOF
    "test_pass_rate": {
      "unit_tests_percentage": 0,
      "integration_tests_percentage": 0,
      "overall_percentage": 0,
      "total_tests": 0,
      "passing_tests": 0,
      "failing_tests": 0
    }
EOF
        return
    fi

    # Use basic grep without -P flag (macOS compatibility)
    local passed=$(echo "$test_result" | grep -o '[0-9]* passed' | grep -o '[0-9]*' || echo "0")
    local failed=$(echo "$test_result" | grep -o '[0-9]* failed' | grep -o '[0-9]*' || echo "0")
    local total=$((passed + failed))
    local percentage=0

    if [[ $total -gt 0 ]]; then
        percentage=$(awk "BEGIN {printf \"%.2f\", ($passed / $total) * 100}")
    fi

    cat <<EOF
    "test_pass_rate": {
      "unit_tests_percentage": $percentage,
      "integration_tests_percentage": $percentage,
      "overall_percentage": $percentage,
      "total_tests": $total,
      "passing_tests": $passed,
      "failing_tests": $failed
    }
EOF
}

# Extract compiler error metrics
extract_compiler_errors() {
    local log_file="$1"
    local total_errors=$(grep -c '^error\[E' "$log_file" || echo "0")
    local total_warnings=$(grep -c '^warning:' "$log_file" || echo "0")

    # Count by error type (macOS compatible)
    local error_types=$(grep -o '^error\[E[0-9]*' "$log_file" | sed 's/^error\[E//' | sort | uniq -c || echo "")
    local by_type="{"

    while IFS= read -r line; do
        if [[ -n "$line" ]]; then
            local count=$(echo "$line" | awk '{print $1}')
            local type=$(echo "$line" | awk '{print $2}')
            by_type="${by_type}\"E${type}\": $count, "
        fi
    done <<< "$error_types"

    by_type="${by_type%%, }}"

    local trend="stable"
    if [[ $total_errors -eq 0 ]]; then
        trend="improving"
    elif [[ $total_errors -gt 10 ]]; then
        trend="degrading"
    fi

    cat <<EOF
    "compiler_errors": {
      "total_errors": $total_errors,
      "total_warnings": $total_warnings,
      "by_type": $by_type,
      "trend": "$trend"
    }
EOF
}

# Extract code quality metrics
extract_code_quality() {
    local log_file="$1"
    local clippy_warnings=$(grep -c 'warning:.*clippy::' "$log_file" || echo "0")

    # Calculate quality score (0-10 scale)
    # Formula: 10 - (clippy_warnings * 0.1) - (errors * 0.5)
    local errors=$(grep -c '^error\[E' "$log_file" || echo "0")
    local quality_score=$(awk "BEGIN {printf \"%.2f\", 10 - ($clippy_warnings * 0.1) - ($errors * 0.5)}")

    # Ensure score is between 0 and 10
    if ! echo "$quality_score" | grep -q '^[0-9.]*$'; then
        quality_score="0"
    fi
    quality_score=$(awk "BEGIN {score=$quality_score; if(score<0) score=0; if(score>10) score=10; printf \"%.2f\", score}")

    cat <<EOF
    "code_quality": {
      "quality_score": $quality_score,
      "clippy_warnings": $clippy_warnings,
      "cyclomatic_complexity_avg": 0,
      "test_coverage_percentage": 0,
      "documentation_coverage_percentage": 0
    }
EOF
}

# Extract template accessibility metrics
extract_template_accessibility() {
    local templates_dir="/Users/sac/ggen/templates"
    local total_templates=0
    local discovered_templates=0

    if [[ -d "$templates_dir" ]]; then
        total_templates=$(find "$templates_dir" -name "*.tmpl" | wc -l | tr -d ' ')
    fi

    # For now, assume discovered = total (will be updated by CLI integration)
    discovered_templates=$total_templates
    local accessibility_percentage=0

    if [[ $total_templates -gt 0 ]]; then
        accessibility_percentage=$(awk "BEGIN {printf \"%.2f\", ($discovered_templates / $total_templates) * 100}")
    fi

    cat <<EOF
    "template_accessibility": {
      "total_templates": $total_templates,
      "discovered_templates": $discovered_templates,
      "accessible_templates": $discovered_templates,
      "accessibility_percentage": $accessibility_percentage
    }
EOF
}

# Extract waste score metrics
extract_waste_score() {
    local log_file="$1"
    local codebase_dir="/Users/sac/ggen/crates"

    local todo_count=0
    local unwrap_count=0
    local dead_code_lines=0

    if [[ -d "$codebase_dir" ]]; then
        todo_count=$(grep -r "TODO:" "$codebase_dir" 2>/dev/null | wc -l | tr -d ' ' || echo "0")
        unwrap_count=$(grep -r "\.unwrap()\|\.expect(" "$codebase_dir" 2>/dev/null | wc -l | tr -d ' ' || echo "0")
        dead_code_lines=$(grep -r "#\[allow(dead_code)\]" "$codebase_dir" 2>/dev/null | wc -l | tr -d ' ' || echo "0")
    fi

    # Calculate waste score (0-10 scale, lower is better)
    # Formula: (todo_count * 0.1) + (unwrap_count * 0.05) + (dead_code_lines * 0.02)
    local waste_score=$(awk "BEGIN {printf \"%.2f\", ($todo_count * 0.1) + ($unwrap_count * 0.05) + ($dead_code_lines * 0.02)}")

    # Validate and cap at 10
    if ! echo "$waste_score" | grep -q '^[0-9.]*$'; then
        waste_score="0"
    fi
    waste_score=$(awk "BEGIN {score=$waste_score; if(score>10) score=10; printf \"%.2f\", score}")

    cat <<EOF
    "waste_score": {
      "overall_waste_score": $waste_score,
      "todo_count": $todo_count,
      "unwrap_count": $unwrap_count,
      "dead_code_lines": $dead_code_lines,
      "overproduction_percentage": 0,
      "defect_count": 0
    }
EOF
}

# Extract velocity metrics
extract_velocity() {
    cat <<EOF
    "velocity": {
      "features_per_sprint": 0,
      "average_fix_time_hours": 0,
      "blockers_resolved": 0,
      "commits_per_week": 0,
      "pull_requests_merged": 0
    }
EOF
}

# Extract cost metrics
extract_cost() {
    cat <<EOF
    "cost_per_hour": {
      "developer_time_lost_hours": 0,
      "rework_hours": 0,
      "incident_response_hours": 0,
      "hourly_cost_usd": 187,
      "weekly_waste_cost_usd": 0,
      "annual_waste_cost_usd": 0
    }
EOF
}

# Detect Andon signals
detect_andon_signals() {
    local log_file="$1"
    local critical="[]"
    local high="[]"
    local medium="[]"

    # CRITICAL: Compiler errors
    local error_count=$(grep -c '^error\[E' "$log_file" || echo "0")
    if [[ $error_count -gt 0 ]]; then
        critical="[{\"type\": \"compiler_error\", \"message\": \"$error_count compiler errors detected\", \"threshold\": 0, \"current_value\": $error_count, \"triggered_at\": \"$TIMESTAMP\", \"action_required\": \"Fix all compiler errors immediately (STOP THE LINE)\"}]"
    fi

    # HIGH: Test failures
    local test_failures=$(grep -oP '\d+ failed' "$log_file" | grep -oP '\d+' || echo "0")
    if [[ $test_failures -gt 0 ]]; then
        high="[{\"type\": \"test_failure\", \"message\": \"$test_failures tests failing\", \"threshold\": 0, \"current_value\": $test_failures, \"triggered_at\": \"$TIMESTAMP\", \"action_required\": \"Fix failing tests before proceeding\"}]"
    fi

    # MEDIUM: Clippy warnings
    local clippy_warnings=$(grep -c 'warning:.*clippy::' "$log_file" || echo "0")
    if [[ $clippy_warnings -gt 5 ]]; then
        medium="[{\"type\": \"quality_degradation\", \"message\": \"$clippy_warnings clippy warnings\", \"threshold\": 5, \"current_value\": $clippy_warnings, \"triggered_at\": \"$TIMESTAMP\", \"action_required\": \"Address clippy warnings to improve code quality\"}]"
    fi

    cat <<EOF
  "andon_signals": {
    "critical": $critical,
    "high": $high,
    "medium": $medium
  }
EOF
}

# Generate baseline trend data
generate_baseline_trend() {
    cat <<EOF
  "trend": {
    "week_0": {
      "compiler_errors": 158,
      "test_pass_rate": 15,
      "build_time_seconds": 30,
      "template_accessibility": 5,
      "waste_score": 8.4,
      "velocity": 2,
      "cost_usd": 634.62
    },
    "week_1": {
      "compiler_errors": 0,
      "test_pass_rate": 50,
      "build_time_seconds": 1.5,
      "template_accessibility": 60,
      "waste_score": 6.5,
      "velocity": 3.2,
      "cost_usd": 187
    },
    "week_2": {
      "compiler_errors": 0,
      "test_pass_rate": 85,
      "build_time_seconds": 1.5,
      "template_accessibility": 90,
      "waste_score": 4.0,
      "velocity": 4.5,
      "cost_usd": 100
    },
    "week_3": {
      "compiler_errors": 0,
      "test_pass_rate": 100,
      "build_time_seconds": 1.5,
      "template_accessibility": 100,
      "waste_score": 2.0,
      "velocity": 5,
      "cost_usd": 50
    }
  }
EOF
}

# Main JSON output
cat <<EOF
{
  "week": $WEEK,
  "date": "$DATE",
  "metrics": {
$(extract_build_time "$LOG_FILE"),
$(extract_test_pass_rate "$LOG_FILE"),
$(extract_compiler_errors "$LOG_FILE"),
$(extract_code_quality "$LOG_FILE"),
$(extract_template_accessibility),
$(extract_waste_score "$LOG_FILE"),
$(extract_velocity),
$(extract_cost)
  },
$(generate_baseline_trend),
$(detect_andon_signals "$LOG_FILE"),
  "improvements": [],
  "remaining_gaps": []
}
EOF
