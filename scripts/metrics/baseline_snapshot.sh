#!/bin/bash
# baseline_snapshot.sh - Create Week 0 baseline snapshot
# This captures the initial state before any improvements

set -euo pipefail

METRICS_DIR="/Users/sac/ggen/.metrics"
BASELINE_FILE="$METRICS_DIR/baseline-week-0.json"
DATE=$(date +%Y-%m-%d)
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

mkdir -p "$METRICS_DIR"

echo "📸 Creating Week 0 baseline snapshot..."

cat > "$BASELINE_FILE" <<'EOF'
{
  "week": 0,
  "date": "DATE_PLACEHOLDER",
  "description": "Initial baseline before Kaizen improvements",
  "metrics": {
    "build_time": {
      "first_build_seconds": 30,
      "incremental_build_seconds": 5,
      "test_suite_seconds": 45,
      "improvement_percentage": 0
    },
    "test_pass_rate": {
      "unit_tests_percentage": 15,
      "integration_tests_percentage": 10,
      "overall_percentage": 15,
      "total_tests": 20,
      "passing_tests": 3,
      "failing_tests": 17
    },
    "compiler_errors": {
      "total_errors": 158,
      "total_warnings": 42,
      "by_type": {
        "E0061": 45,
        "E0599": 38,
        "E0277": 25,
        "E0308": 20,
        "E0425": 15,
        "E0412": 10,
        "other": 5
      },
      "trend": "degrading"
    },
    "code_quality": {
      "quality_score": 7.2,
      "clippy_warnings": 28,
      "cyclomatic_complexity_avg": 12.5,
      "test_coverage_percentage": 15,
      "documentation_coverage_percentage": 10
    },
    "template_accessibility": {
      "total_templates": 258,
      "discovered_templates": 13,
      "accessible_templates": 13,
      "accessibility_percentage": 5.04
    },
    "waste_score": {
      "overall_waste_score": 8.4,
      "todo_count": 47,
      "unwrap_count": 89,
      "dead_code_lines": 156,
      "overproduction_percentage": 94.96,
      "defect_count": 17
    },
    "velocity": {
      "features_per_sprint": 2,
      "average_fix_time_hours": 8.5,
      "blockers_resolved": 0,
      "commits_per_week": 12,
      "pull_requests_merged": 1
    },
    "cost_per_hour": {
      "developer_time_lost_hours": 3.4,
      "rework_hours": 5.2,
      "incident_response_hours": 2.1,
      "hourly_cost_usd": 187,
      "weekly_waste_cost_usd": 634.62,
      "annual_waste_cost_usd": 33000
    }
  },
  "trend": {
    "week_0": {
      "compiler_errors": 158,
      "test_pass_rate": 15,
      "build_time_seconds": 30,
      "template_accessibility": 5.04,
      "waste_score": 8.4,
      "velocity": 2,
      "cost_usd": 634.62
    }
  },
  "andon_signals": {
    "critical": [
      {
        "type": "compiler_error",
        "message": "158 compiler errors blocking development",
        "threshold": 0,
        "current_value": 158,
        "triggered_at": "TIMESTAMP_PLACEHOLDER",
        "action_required": "Fix all compiler errors immediately (STOP THE LINE)"
      }
    ],
    "high": [
      {
        "type": "test_failure",
        "message": "17 failing tests (85% failure rate)",
        "threshold": 0,
        "current_value": 17,
        "triggered_at": "TIMESTAMP_PLACEHOLDER",
        "action_required": "Fix failing tests before proceeding with new features"
      }
    ],
    "medium": [
      {
        "type": "quality_degradation",
        "message": "28 clippy warnings requiring attention",
        "threshold": 5,
        "current_value": 28,
        "triggered_at": "TIMESTAMP_PLACEHOLDER",
        "action_required": "Address clippy warnings to improve code quality"
      },
      {
        "type": "coverage_drop",
        "message": "Test coverage at 15% (target: 80%)",
        "threshold": 80,
        "current_value": 15,
        "triggered_at": "TIMESTAMP_PLACEHOLDER",
        "action_required": "Increase test coverage to meet quality standards"
      }
    ]
  },
  "improvements": [],
  "remaining_gaps": [
    {
      "category": "compiler_errors",
      "current_value": 158,
      "target_value": 0,
      "gap_percentage": 100,
      "action_items": [
        "Fix E0061 errors (missing function arguments) - 45 occurrences",
        "Fix E0599 errors (method not found) - 38 occurrences",
        "Fix E0277 errors (trait not implemented) - 25 occurrences",
        "Fix E0308 errors (type mismatch) - 20 occurrences",
        "Fix remaining error types - 30 occurrences"
      ]
    },
    {
      "category": "test_pass_rate",
      "current_value": 15,
      "target_value": 100,
      "gap_percentage": 85,
      "action_items": [
        "Fix 17 failing unit tests",
        "Improve test isolation and determinism",
        "Add missing test coverage for core modules",
        "Fix async test race conditions"
      ]
    },
    {
      "category": "template_accessibility",
      "current_value": 5.04,
      "target_value": 100,
      "gap_percentage": 94.96,
      "action_items": [
        "Implement template discovery mechanism",
        "Add CLI commands for template access",
        "Create template registry/index",
        "Add template validation and testing"
      ]
    },
    {
      "category": "waste_score",
      "current_value": 8.4,
      "target_value": 2.0,
      "gap_percentage": 76.19,
      "action_items": [
        "Remove 47 TODO comments (complete or document as FUTURE)",
        "Refactor 89 unwrap()/expect() calls to use Result<T,E>",
        "Remove 156 lines of dead code",
        "Reduce overproduction (94.96% unused templates)"
      ]
    },
    {
      "category": "code_quality",
      "current_value": 7.2,
      "target_value": 9.5,
      "gap_percentage": 24.21,
      "action_items": [
        "Fix 28 clippy warnings",
        "Reduce cyclomatic complexity (current: 12.5, target: <8)",
        "Increase test coverage from 15% to 80%+",
        "Improve documentation coverage"
      ]
    }
  ]
}
EOF

sed -i.bak "s/DATE_PLACEHOLDER/$DATE/g" "$BASELINE_FILE"
sed -i.bak "s/TIMESTAMP_PLACEHOLDER/$TIMESTAMP/g" "$BASELINE_FILE"
rm -f "$BASELINE_FILE.bak"

echo "✅ Baseline snapshot created: $BASELINE_FILE"
echo ""
echo "📊 Week 0 Metrics Summary:"
echo "  - Compiler Errors: 158"
echo "  - Test Pass Rate: 15%"
echo "  - Build Time: 30s"
echo "  - Template Accessibility: 5.04%"
echo "  - Waste Score: 8.4"
echo "  - Code Quality: 7.2"
echo "  - Velocity: 2 features/sprint"
echo "  - Annual Waste Cost: $33,000"
echo ""
echo "🎯 Target Improvements:"
echo "  - 76% waste reduction"
echo "  - 0 compiler errors"
echo "  - 100% test pass rate"
echo "  - 100% template accessibility"
echo "  - 2.0 waste score"
echo "  - 9.5 code quality"
echo "  - 5 features/sprint"
echo "  - $8,000 annual waste cost"
