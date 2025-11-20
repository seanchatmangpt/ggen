#!/usr/bin/env bash
# Generate HTML dashboard from collected metrics

set -e

METRICS_DIR=".metrics"
OUTPUT_FILE="$METRICS_DIR/dashboard.html"

echo "📊 Generating metrics dashboard..."

# Check if metrics directory exists
if [ ! -d "$METRICS_DIR" ]; then
    echo "Error: Metrics directory not found. Run 'cargo run --bin collect-metrics' first."
    exit 1
fi

# Count metrics files
METRICS_COUNT=$(find "$METRICS_DIR" -name "*.json" | wc -l | tr -d ' ')

if [ "$METRICS_COUNT" -eq 0 ]; then
    echo "Error: No metrics files found. Run 'cargo run --bin collect-metrics' first."
    exit 1
fi

# Generate HTML
cat > "$OUTPUT_FILE" <<'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ggen Metrics Dashboard</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
        }
        h1 {
            color: #333;
        }
        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-top: 20px;
        }
        .metric-card {
            background: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .metric-title {
            font-size: 14px;
            color: #666;
            margin-bottom: 5px;
        }
        .metric-value {
            font-size: 32px;
            font-weight: bold;
            color: #333;
        }
        .metric-trend {
            font-size: 12px;
            margin-top: 5px;
        }
        .trend-up { color: #22c55e; }
        .trend-down { color: #ef4444; }
        .chart {
            margin-top: 30px;
            background: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>📊 ggen Metrics Dashboard</h1>
        <p>Last updated: <span id="last-update"></span></p>

        <div class="metrics-grid" id="metrics-grid"></div>

        <div class="chart">
            <h2>Metrics Over Time</h2>
            <canvas id="metrics-chart"></canvas>
        </div>
    </div>

    <script>
        // Load all metrics files
        async function loadMetrics() {
            const metricsFiles = [
EOF

# Add metrics files to JavaScript
for file in "$METRICS_DIR"/*.json; do
    if [ -f "$file" ]; then
        basename=$(basename "$file")
        echo "                '$basename'," >> "$OUTPUT_FILE"
    fi
done

cat >> "$OUTPUT_FILE" <<'EOF'
            ];

            const metrics = [];
            for (const file of metricsFiles) {
                try {
                    const response = await fetch(file);
                    const data = await response.json();
                    metrics.push(data);
                } catch (e) {
                    console.error('Failed to load', file, e);
                }
            }

            return metrics.sort((a, b) => a.date.localeCompare(b.date));
        }

        function displayMetrics(metrics) {
            if (metrics.length === 0) return;

            const latest = metrics[metrics.length - 1];
            document.getElementById('last-update').textContent = latest.date;

            const grid = document.getElementById('metrics-grid');

            // Display key metrics
            const cards = [
                {
                    title: 'Build Time',
                    value: `${latest.build_time_seconds.toFixed(2)}s`,
                    trend: calculateTrend(metrics, 'build_time_seconds')
                },
                {
                    title: 'Test Pass Rate',
                    value: `${(latest.test_pass_rate * 100).toFixed(1)}%`,
                    trend: calculateTrend(metrics, 'test_pass_rate')
                },
                {
                    title: 'Total Tests',
                    value: latest.total_tests,
                    trend: calculateTrend(metrics, 'total_tests')
                },
                {
                    title: 'Compiler Errors',
                    value: latest.compiler_errors,
                    trend: calculateTrend(metrics, 'compiler_errors', true)
                },
                {
                    title: 'Compiler Warnings',
                    value: latest.compiler_warnings,
                    trend: calculateTrend(metrics, 'compiler_warnings', true)
                },
                {
                    title: 'Clippy Warnings',
                    value: latest.clippy_warnings,
                    trend: calculateTrend(metrics, 'clippy_warnings', true)
                },
                {
                    title: 'Code Lines',
                    value: latest.code_lines.toLocaleString(),
                    trend: null
                },
                {
                    title: 'Test Lines',
                    value: latest.test_lines.toLocaleString(),
                    trend: null
                }
            ];

            cards.forEach(card => {
                const div = document.createElement('div');
                div.className = 'metric-card';
                div.innerHTML = `
                    <div class="metric-title">${card.title}</div>
                    <div class="metric-value">${card.value}</div>
                    ${card.trend ? `<div class="metric-trend ${card.trend.class}">${card.trend.text}</div>` : ''}
                `;
                grid.appendChild(div);
            });
        }

        function calculateTrend(metrics, key, inverse = false) {
            if (metrics.length < 2) return null;

            const current = metrics[metrics.length - 1][key];
            const previous = metrics[metrics.length - 2][key];

            if (current === previous) return null;

            const increasing = current > previous;
            const improving = inverse ? !increasing : increasing;

            return {
                class: improving ? 'trend-up' : 'trend-down',
                text: improving ? '↑ Improving' : '↓ Declining'
            };
        }

        // Load and display metrics on page load
        loadMetrics().then(displayMetrics);
    </script>
</body>
</html>
EOF

echo "✅ Dashboard generated: $OUTPUT_FILE"
echo "   Open in browser: open $OUTPUT_FILE"
