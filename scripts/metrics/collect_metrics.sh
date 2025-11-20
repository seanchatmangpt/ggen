#!/bin/bash
# collect_metrics.sh - Automated daily metrics collection
# Usage: collect_metrics.sh [week_number]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
METRICS_DIR="/Users/sac/ggen/.metrics"
DATE=$(date +%Y-%m-%d)
WEEK="${1:-1}"
LOG_FILE="$METRICS_DIR/logs/$DATE.log"

mkdir -p "$METRICS_DIR/logs"
mkdir -p "$METRICS_DIR/daily"
mkdir -p "/Users/sac/ggen/docs/metrics"

echo "🔍 Collecting metrics for Week $WEEK - $DATE"

# Step 1: Run cargo make check and test
echo "Running cargo make check..."
timeout 10s cargo make check > "$LOG_FILE" 2>&1 || echo "cargo make check completed with errors"

echo "Running cargo make test..."
timeout 30s cargo make test >> "$LOG_FILE" 2>&1 || echo "cargo make test completed with failures"

echo "Running cargo make lint..."
timeout 10s cargo make lint >> "$LOG_FILE" 2>&1 || echo "cargo make lint completed with warnings"

# Step 2: Parse metrics
echo "Parsing metrics..."
METRICS_JSON="$METRICS_DIR/daily/$DATE.json"
"$SCRIPT_DIR/parse_metrics.sh" "$LOG_FILE" "$WEEK" > "$METRICS_JSON"

# Step 3: Generate dashboard
echo "Generating dashboard..."
"$SCRIPT_DIR/generate_dashboard.sh" "$METRICS_JSON"

# Step 4: Create latest symlink
ln -sf "$METRICS_JSON" "$METRICS_DIR/latest.json"
ln -sf "/Users/sac/ggen/docs/metrics/dashboard-$DATE.html" "/Users/sac/ggen/docs/metrics/latest.html"

# Step 5: Check for Andon signals
echo "Checking for Andon signals..."
CRITICAL_SIGNALS=$(jq -r '.andon_signals.critical | length' "$METRICS_JSON" 2>/dev/null || echo "0")
HIGH_SIGNALS=$(jq -r '.andon_signals.high | length' "$METRICS_JSON" 2>/dev/null || echo "0")

if [[ $CRITICAL_SIGNALS -gt 0 ]]; then
    echo "🔴 CRITICAL ANDON SIGNALS DETECTED - STOP THE LINE"
    jq -r '.andon_signals.critical[] | "  - \(.message)"' "$METRICS_JSON"
    exit 1
elif [[ $HIGH_SIGNALS -gt 0 ]]; then
    echo "🟡 HIGH PRIORITY SIGNALS DETECTED"
    jq -r '.andon_signals.high[] | "  - \(.message)"' "$METRICS_JSON"
fi

echo "✅ Metrics collection complete"
echo "📊 Dashboard: /Users/sac/ggen/docs/metrics/latest.html"
echo "📈 Metrics JSON: $METRICS_JSON"
