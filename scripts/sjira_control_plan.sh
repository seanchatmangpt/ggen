#!/usr/bin/env bash
# sjira_control_plan.sh — DS-6 control plan entry point.
# Thin wrapper: all logic lives in sjira_control_plan.py (same directory).
# Exit 0 = in control; exit 1 = drift with typed VIOLATION lines.
# Usage: scripts/sjira_control_plan.sh [repo_root] [baseline.json]
set -euo pipefail
REPO="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
BASELINE="${2:-$REPO/docs/jira/v26.11.3/baselines/sjira-control-baseline.json}"
exec python3 "$(dirname "$0")/sjira_control_plan.py" "$REPO" "$BASELINE"
