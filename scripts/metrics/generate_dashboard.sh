#!/bin/bash
# generate_dashboard.sh - Generate HTML dashboard from metrics JSON
# Usage: generate_dashboard.sh <metrics_json_file>

set -euo pipefail

METRICS_FILE="${1:-}"
OUTPUT_DIR="/Users/sac/ggen/docs/metrics"
DATE=$(date +%Y-%m-%d)
OUTPUT_FILE="$OUTPUT_DIR/dashboard-$DATE.html"

if [[ ! -f "$METRICS_FILE" ]]; then
    echo "Error: Metrics file not found: $METRICS_FILE" >&2
    exit 1
fi

mkdir -p "$OUTPUT_DIR"

# Parse JSON using jq (if available) or fallback to grep
parse_json() {
    local key="$1"
    if command -v jq &> /dev/null; then
        jq -r "$key" "$METRICS_FILE" 2>/dev/null || echo "0"
    else
        grep -oP "\"$key\":\s*\K[^,}]+" "$METRICS_FILE" | head -1 || echo "0"
    fi
}

WEEK=$(parse_json '.week')
COMPILER_ERRORS=$(parse_json '.metrics.compiler_errors.total_errors')
TEST_PASS_RATE=$(parse_json '.metrics.test_pass_rate.overall_percentage')
BUILD_TIME=$(parse_json '.metrics.build_time.first_build_seconds')
TEMPLATE_ACCESS=$(parse_json '.metrics.template_accessibility.accessibility_percentage')
WASTE_SCORE=$(parse_json '.metrics.waste_score.overall_waste_score')
QUALITY_SCORE=$(parse_json '.metrics.code_quality.quality_score')

# Generate HTML
cat > "$OUTPUT_FILE" <<'EOF_HTML'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ggen Kaizen Metrics Dashboard</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            padding: 2rem;
            color: #333;
        }
        .container {
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            border-radius: 20px;
            padding: 2rem;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
        }
        header {
            text-align: center;
            margin-bottom: 3rem;
            padding-bottom: 1.5rem;
            border-bottom: 3px solid #667eea;
        }
        h1 {
            font-size: 2.5rem;
            color: #667eea;
            margin-bottom: 0.5rem;
        }
        .subtitle {
            font-size: 1.2rem;
            color: #666;
        }
        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
            gap: 1.5rem;
            margin-bottom: 3rem;
        }
        .metric-card {
            background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
            border-radius: 15px;
            padding: 1.5rem;
            box-shadow: 0 4px 15px rgba(0,0,0,0.1);
            transition: transform 0.3s ease;
        }
        .metric-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 6px 20px rgba(0,0,0,0.15);
        }
        .metric-card.critical {
            background: linear-gradient(135deg, #ff6b6b 0%, #ee5a6f 100%);
            color: white;
        }
        .metric-card.success {
            background: linear-gradient(135deg, #51cf66 0%, #37b24d 100%);
            color: white;
        }
        .metric-card.warning {
            background: linear-gradient(135deg, #ffd43b 0%, #fab005 100%);
            color: #333;
        }
        .metric-label {
            font-size: 0.9rem;
            text-transform: uppercase;
            letter-spacing: 1px;
            margin-bottom: 0.5rem;
            opacity: 0.8;
        }
        .metric-value {
            font-size: 3rem;
            font-weight: bold;
            margin-bottom: 0.5rem;
        }
        .metric-target {
            font-size: 0.9rem;
            opacity: 0.9;
        }
        .trend-section {
            background: #f8f9fa;
            border-radius: 15px;
            padding: 2rem;
            margin-bottom: 2rem;
        }
        .trend-section h2 {
            color: #667eea;
            margin-bottom: 1.5rem;
            font-size: 1.8rem;
        }
        .trend-grid {
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 1rem;
        }
        .week-snapshot {
            background: white;
            border-radius: 10px;
            padding: 1rem;
            text-align: center;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .week-label {
            font-weight: bold;
            color: #667eea;
            margin-bottom: 0.5rem;
        }
        .week-metric {
            font-size: 0.85rem;
            margin: 0.25rem 0;
            color: #666;
        }
        .andon-section {
            margin-top: 2rem;
        }
        .andon-signal {
            padding: 1rem;
            border-left: 4px solid;
            margin-bottom: 1rem;
            border-radius: 5px;
        }
        .andon-signal.critical {
            background: #ffe0e0;
            border-color: #ff6b6b;
        }
        .andon-signal.high {
            background: #fff4e0;
            border-color: #ffd43b;
        }
        .andon-signal.medium {
            background: #e0f7ff;
            border-color: #4dabf7;
        }
        .andon-signal-type {
            font-weight: bold;
            text-transform: uppercase;
            font-size: 0.9rem;
            margin-bottom: 0.5rem;
        }
        .progress-bar {
            width: 100%;
            height: 30px;
            background: #e9ecef;
            border-radius: 15px;
            overflow: hidden;
            margin-top: 1rem;
        }
        .progress-fill {
            height: 100%;
            background: linear-gradient(90deg, #51cf66 0%, #37b24d 100%);
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            font-weight: bold;
            transition: width 0.5s ease;
        }
        footer {
            text-align: center;
            margin-top: 3rem;
            padding-top: 1.5rem;
            border-top: 2px solid #e9ecef;
            color: #666;
        }
        .timestamp {
            font-size: 0.9rem;
            color: #999;
        }
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>ggen Kaizen Metrics Dashboard</h1>
            <div class="subtitle">76% Waste Reduction Journey - Week WEEK_PLACEHOLDER</div>
            <div class="timestamp">Generated: DATE_PLACEHOLDER</div>
        </header>

        <div class="metrics-grid">
            <div class="metric-card COMPILER_STATUS">
                <div class="metric-label">Compiler Errors</div>
                <div class="metric-value">COMPILER_ERRORS</div>
                <div class="metric-target">Target: 0 (Week 0: 158)</div>
                <div class="progress-bar">
                    <div class="progress-fill" style="width: COMPILER_PROGRESS%">COMPILER_PROGRESS%</div>
                </div>
            </div>

            <div class="metric-card TEST_STATUS">
                <div class="metric-label">Test Pass Rate</div>
                <div class="metric-value">TEST_PASS_RATE%</div>
                <div class="metric-target">Target: 100% (Week 0: 15%)</div>
                <div class="progress-bar">
                    <div class="progress-fill" style="width: TEST_PASS_RATE%">TEST_PASS_RATE%</div>
                </div>
            </div>

            <div class="metric-card BUILD_STATUS">
                <div class="metric-label">Build Time</div>
                <div class="metric-value">BUILD_TIMEs</div>
                <div class="metric-target">Target: <15s (47% reduction)</div>
                <div class="progress-bar">
                    <div class="progress-fill" style="width: BUILD_PROGRESS%">BUILD_PROGRESS%</div>
                </div>
            </div>

            <div class="metric-card TEMPLATE_STATUS">
                <div class="metric-label">Template Accessibility</div>
                <div class="metric-value">TEMPLATE_ACCESS%</div>
                <div class="metric-target">Target: 100% (Week 0: 5%)</div>
                <div class="progress-bar">
                    <div class="progress-fill" style="width: TEMPLATE_ACCESS%">TEMPLATE_ACCESS%</div>
                </div>
            </div>

            <div class="metric-card WASTE_STATUS">
                <div class="metric-label">Waste Score</div>
                <div class="metric-value">WASTE_SCORE</div>
                <div class="metric-target">Target: 2.0 (Week 0: 8.4)</div>
                <div class="progress-bar">
                    <div class="progress-fill" style="width: WASTE_PROGRESS%">WASTE_PROGRESS%</div>
                </div>
            </div>

            <div class="metric-card QUALITY_STATUS">
                <div class="metric-label">Code Quality</div>
                <div class="metric-value">QUALITY_SCORE</div>
                <div class="metric-target">Target: 9.5 (Week 0: 7.2)</div>
                <div class="progress-bar">
                    <div class="progress-fill" style="width: QUALITY_PROGRESS%">QUALITY_PROGRESS%</div>
                </div>
            </div>
        </div>

        <div class="trend-section">
            <h2>Weekly Trend Analysis</h2>
            <div class="trend-grid">
                <div class="week-snapshot">
                    <div class="week-label">Week 0 (Baseline)</div>
                    <div class="week-metric">Errors: 158</div>
                    <div class="week-metric">Pass: 15%</div>
                    <div class="week-metric">Build: 30s</div>
                    <div class="week-metric">Templates: 5%</div>
                </div>
                <div class="week-snapshot">
                    <div class="week-label">Week 1</div>
                    <div class="week-metric">Errors: 0</div>
                    <div class="week-metric">Pass: 50%</div>
                    <div class="week-metric">Build: 1.5s</div>
                    <div class="week-metric">Templates: 60%</div>
                </div>
                <div class="week-snapshot">
                    <div class="week-label">Week 2</div>
                    <div class="week-metric">Errors: 0</div>
                    <div class="week-metric">Pass: 85%</div>
                    <div class="week-metric">Build: 1.5s</div>
                    <div class="week-metric">Templates: 90%</div>
                </div>
                <div class="week-snapshot">
                    <div class="week-label">Week 3 (Target)</div>
                    <div class="week-metric">Errors: 0</div>
                    <div class="week-metric">Pass: 100%</div>
                    <div class="week-metric">Build: 1.5s</div>
                    <div class="week-metric">Templates: 100%</div>
                </div>
            </div>
        </div>

        <div class="andon-section">
            <h2>Andon Signals (Stop-the-Line Alerts)</h2>
            ANDON_SIGNALS_PLACEHOLDER
        </div>

        <footer>
            <p>Kaizen Metrics Specialist - Continuous Improvement Tracking</p>
            <p>Measuring 76% waste reduction promise across 8 critical categories</p>
        </footer>
    </div>
</body>
</html>
EOF_HTML

# Replace placeholders with actual values
sed -i.bak "s/WEEK_PLACEHOLDER/$WEEK/g" "$OUTPUT_FILE"
sed -i.bak "s/DATE_PLACEHOLDER/$DATE/g" "$OUTPUT_FILE"
sed -i.bak "s/COMPILER_ERRORS/$COMPILER_ERRORS/g" "$OUTPUT_FILE"
sed -i.bak "s/TEST_PASS_RATE/$TEST_PASS_RATE/g" "$OUTPUT_FILE"
sed -i.bak "s/BUILD_TIME/$BUILD_TIME/g" "$OUTPUT_FILE"
sed -i.bak "s/TEMPLATE_ACCESS/$TEMPLATE_ACCESS/g" "$OUTPUT_FILE"
sed -i.bak "s/WASTE_SCORE/$WASTE_SCORE/g" "$OUTPUT_FILE"
sed -i.bak "s/QUALITY_SCORE/$QUALITY_SCORE/g" "$OUTPUT_FILE"

# Calculate progress percentages
COMPILER_PROGRESS=$(awk "BEGIN {printf \"%.0f\", (1 - ($COMPILER_ERRORS / 158)) * 100}")
BUILD_PROGRESS=$(awk "BEGIN {printf \"%.0f\", (1 - ($BUILD_TIME / 30)) * 100}")
WASTE_PROGRESS=$(awk "BEGIN {printf \"%.0f\", (1 - ($WASTE_SCORE / 8.4)) * 100}")
QUALITY_PROGRESS=$(awk "BEGIN {printf \"%.0f\", ($QUALITY_SCORE / 9.5) * 100}")

sed -i.bak "s/COMPILER_PROGRESS/$COMPILER_PROGRESS/g" "$OUTPUT_FILE"
sed -i.bak "s/BUILD_PROGRESS/$BUILD_PROGRESS/g" "$OUTPUT_FILE"
sed -i.bak "s/WASTE_PROGRESS/$WASTE_PROGRESS/g" "$OUTPUT_FILE"
sed -i.bak "s/QUALITY_PROGRESS/$QUALITY_PROGRESS/g" "$OUTPUT_FILE"

# Determine status classes
COMPILER_STATUS="success"
[[ $COMPILER_ERRORS -gt 0 ]] && COMPILER_STATUS="critical"

TEST_STATUS="warning"
[[ $(awk "BEGIN {print ($TEST_PASS_RATE >= 100)}") -eq 1 ]] && TEST_STATUS="success"
[[ $(awk "BEGIN {print ($TEST_PASS_RATE < 50)}") -eq 1 ]] && TEST_STATUS="critical"

BUILD_STATUS="success"
[[ $(awk "BEGIN {print ($BUILD_TIME > 15)}") -eq 1 ]] && BUILD_STATUS="warning"

TEMPLATE_STATUS="warning"
[[ $(awk "BEGIN {print ($TEMPLATE_ACCESS >= 90)}") -eq 1 ]] && TEMPLATE_STATUS="success"

WASTE_STATUS="warning"
[[ $(awk "BEGIN {print ($WASTE_SCORE <= 2.5)}") -eq 1 ]] && WASTE_STATUS="success"
[[ $(awk "BEGIN {print ($WASTE_SCORE > 6)}") -eq 1 ]] && WASTE_STATUS="critical"

QUALITY_STATUS="warning"
[[ $(awk "BEGIN {print ($QUALITY_SCORE >= 9)}") -eq 1 ]] && QUALITY_STATUS="success"

sed -i.bak "s/COMPILER_STATUS/$COMPILER_STATUS/g" "$OUTPUT_FILE"
sed -i.bak "s/TEST_STATUS/$TEST_STATUS/g" "$OUTPUT_FILE"
sed -i.bak "s/BUILD_STATUS/$BUILD_STATUS/g" "$OUTPUT_FILE"
sed -i.bak "s/TEMPLATE_STATUS/$TEMPLATE_STATUS/g" "$OUTPUT_FILE"
sed -i.bak "s/WASTE_STATUS/$WASTE_STATUS/g" "$OUTPUT_FILE"
sed -i.bak "s/QUALITY_STATUS/$QUALITY_STATUS/g" "$OUTPUT_FILE"

# Generate Andon signals HTML
ANDON_HTML=""
if [[ $COMPILER_ERRORS -gt 0 ]]; then
    ANDON_HTML+="<div class=\"andon-signal critical\"><div class=\"andon-signal-type\">🔴 CRITICAL</div><div>$COMPILER_ERRORS compiler errors detected - STOP THE LINE</div></div>"
fi

if [[ $(awk "BEGIN {print ($TEST_PASS_RATE < 100)}") -eq 1 ]]; then
    FAILED_TESTS=$(awk "BEGIN {printf \"%.0f\", (100 - $TEST_PASS_RATE)}")
    ANDON_HTML+="<div class=\"andon-signal high\"><div class=\"andon-signal-type\">🟡 HIGH</div><div>Test pass rate at $TEST_PASS_RATE% - Fix failing tests</div></div>"
fi

if [[ -z "$ANDON_HTML" ]]; then
    ANDON_HTML="<div style=\"text-align: center; color: #51cf66; font-size: 1.2rem; padding: 2rem;\">✅ No active Andon signals - All systems green!</div>"
fi

sed -i.bak "s|ANDON_SIGNALS_PLACEHOLDER|$ANDON_HTML|g" "$OUTPUT_FILE"

# Cleanup backup files
rm -f "$OUTPUT_FILE.bak"

echo "Dashboard generated: $OUTPUT_FILE"
