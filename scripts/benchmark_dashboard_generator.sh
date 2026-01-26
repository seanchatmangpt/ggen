#!/bin/bash

################################################################################
# Benchmark Dashboard Generator
# Creates HTML visualization of benchmark results and trends
################################################################################

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
METRICS_DIR="${PROJECT_ROOT}/.metrics"
DASHBOARD_FILE="${METRICS_DIR}/benchmark_dashboard.html"

################################################################################
# HTML Dashboard Generation
################################################################################

generate_html_header() {
    cat << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ggen Benchmark Dashboard</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@3.9.1/dist/chart.min.js"></script>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }

        .container {
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            border-radius: 10px;
            box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);
            padding: 40px;
        }

        h1 {
            color: #333;
            margin-bottom: 10px;
            font-size: 32px;
        }

        .header {
            border-bottom: 3px solid #667eea;
            padding-bottom: 20px;
            margin-bottom: 30px;
        }

        .timestamp {
            color: #666;
            font-size: 14px;
        }

        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-bottom: 40px;
        }

        .metric-card {
            background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
            padding: 20px;
            border-radius: 8px;
            border-left: 4px solid #667eea;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }

        .metric-card.pass {
            border-left-color: #10b981;
        }

        .metric-card.fail {
            border-left-color: #ef4444;
        }

        .metric-card.warning {
            border-left-color: #f59e0b;
        }

        .metric-label {
            color: #666;
            font-size: 12px;
            text-transform: uppercase;
            letter-spacing: 1px;
            margin-bottom: 8px;
            font-weight: 600;
        }

        .metric-value {
            color: #333;
            font-size: 24px;
            font-weight: bold;
            margin-bottom: 4px;
        }

        .metric-status {
            color: #666;
            font-size: 12px;
        }

        .status-badge {
            display: inline-block;
            padding: 4px 8px;
            border-radius: 4px;
            font-size: 11px;
            font-weight: 600;
            margin-left: 4px;
        }

        .status-badge.pass {
            background: #d1fae5;
            color: #065f46;
        }

        .status-badge.fail {
            background: #fee2e2;
            color: #7f1d1d;
        }

        .status-badge.warning {
            background: #fef3c7;
            color: #92400e;
        }

        .chart-section {
            margin-bottom: 40px;
            background: #f9fafb;
            padding: 20px;
            border-radius: 8px;
            border: 1px solid #e5e7eb;
        }

        .chart-title {
            color: #333;
            font-size: 18px;
            font-weight: 600;
            margin-bottom: 20px;
        }

        .chart-container {
            position: relative;
            height: 300px;
            margin-bottom: 20px;
        }

        .slo-table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 20px;
        }

        .slo-table th {
            background: #f3f4f6;
            color: #374151;
            padding: 12px;
            text-align: left;
            font-weight: 600;
            border-bottom: 2px solid #e5e7eb;
        }

        .slo-table td {
            padding: 12px;
            border-bottom: 1px solid #e5e7eb;
        }

        .slo-table tr:hover {
            background: #f9fafb;
        }

        .status-pass {
            color: #10b981;
            font-weight: 600;
        }

        .status-fail {
            color: #ef4444;
            font-weight: 600;
        }

        .status-warning {
            color: #f59e0b;
            font-weight: 600;
        }

        .footer {
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #e5e7eb;
            color: #666;
            font-size: 12px;
        }

        .regression-section {
            background: #fef2f2;
            border-left: 4px solid #ef4444;
            padding: 15px;
            border-radius: 4px;
            margin-bottom: 20px;
        }

        .regression-title {
            color: #7f1d1d;
            font-weight: 600;
            margin-bottom: 8px;
        }

        .regression-item {
            color: #991b1b;
            font-size: 13px;
            margin-bottom: 4px;
        }

        @media (max-width: 768px) {
            .container {
                padding: 20px;
            }

            h1 {
                font-size: 24px;
            }

            .metrics-grid {
                grid-template-columns: 1fr;
            }
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>📊 ggen Benchmark Dashboard</h1>
            <p class="timestamp">Last Updated: <span id="timestamp"></span></p>
        </div>

        <div class="metrics-grid" id="metrics-grid">
            <!-- Populated by JavaScript -->
        </div>

        <div class="chart-section">
            <div class="chart-title">Build Time Trend</div>
            <div class="chart-container">
                <canvas id="buildTimeChart"></canvas>
            </div>
        </div>

        <div class="chart-section">
            <div class="chart-title">Test Execution Time Trend</div>
            <div class="chart-container">
                <canvas id="testTimeChart"></canvas>
            </div>
        </div>

        <div class="chart-section">
            <div class="chart-title">Binary Size Trend</div>
            <div class="chart-container">
                <canvas id="binarySizeChart"></canvas>
            </div>
        </div>

        <div class="chart-section">
            <div class="chart-title">SLO Compliance Status</div>
            <table class="slo-table">
                <thead>
                    <tr>
                        <th>Metric</th>
                        <th>Target</th>
                        <th>Current</th>
                        <th>Status</th>
                        <th>Deviation</th>
                    </tr>
                </thead>
                <tbody id="slo-table-body">
                    <!-- Populated by JavaScript -->
                </tbody>
            </table>
        </div>

        <div id="regression-section"></div>

        <div class="footer">
            <p>Generated by ggen Benchmark Dashboard System</p>
            <p>SLO targets: First Build ≤15s | Incremental ≤2s | RDF Processing ≤5s | Memory ≤100MB | Binary ≤500MB</p>
        </div>
    </div>

    <script>
        // Update timestamp
        document.getElementById('timestamp').textContent = new Date().toLocaleString();

        // Sample data - in production, this would come from the server
        const metricsData = {
            firstBuild: { value: 12, target: 15, unit: 's', status: 'pass' },
            incrementalBuild: { value: 1.5, target: 2, unit: 's', status: 'pass' },
            testExecution: { value: 28, target: 30, unit: 's', status: 'pass' },
            binarySize: { value: 385, target: 500, unit: 'MB', status: 'pass' },
            memory: { value: 85, target: 100, unit: 'MB', status: 'pass' }
        };

        // Populate metrics grid
        const metricsGrid = document.getElementById('metrics-grid');
        Object.entries(metricsData).forEach(([key, metric]) => {
            const card = document.createElement('div');
            card.className = `metric-card ${metric.status}`;
            card.innerHTML = `
                <div class="metric-label">${key.replace(/([A-Z])/g, ' $1').toLowerCase()}</div>
                <div class="metric-value">${metric.value}${metric.unit}</div>
                <div class="metric-status">
                    Target: ${metric.target}${metric.unit}
                    <span class="status-badge ${metric.status}">${metric.status.toUpperCase()}</span>
                </div>
            `;
            metricsGrid.appendChild(card);
        });

        // Chart configuration
        const chartOptions = {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                legend: {
                    display: true,
                    position: 'top',
                }
            },
            scales: {
                y: {
                    beginAtZero: true,
                }
            }
        };

        // Build Time Chart
        const buildTimeCtx = document.getElementById('buildTimeChart').getContext('2d');
        new Chart(buildTimeCtx, {
            type: 'line',
            data: {
                labels: ['1h ago', '2h ago', '3h ago', '4h ago', '5h ago', 'Now'],
                datasets: [
                    {
                        label: 'Build Time (seconds)',
                        data: [14, 13, 15, 12, 14, 12],
                        borderColor: '#667eea',
                        backgroundColor: 'rgba(102, 126, 234, 0.1)',
                        tension: 0.4,
                        fill: true,
                    },
                    {
                        label: 'SLO Target (15s)',
                        data: [15, 15, 15, 15, 15, 15],
                        borderColor: '#ef4444',
                        borderDash: [5, 5],
                        fill: false,
                    }
                ]
            },
            options: chartOptions
        });

        // Test Time Chart
        const testTimeCtx = document.getElementById('testTimeChart').getContext('2d');
        new Chart(testTimeCtx, {
            type: 'line',
            data: {
                labels: ['1h ago', '2h ago', '3h ago', '4h ago', '5h ago', 'Now'],
                datasets: [
                    {
                        label: 'Test Time (seconds)',
                        data: [29, 27, 30, 28, 29, 28],
                        borderColor: '#10b981',
                        backgroundColor: 'rgba(16, 185, 129, 0.1)',
                        tension: 0.4,
                        fill: true,
                    }
                ]
            },
            options: chartOptions
        });

        // Binary Size Chart
        const binarySizeCtx = document.getElementById('binarySizeChart').getContext('2d');
        new Chart(binarySizeCtx, {
            type: 'bar',
            data: {
                labels: ['1h ago', '2h ago', '3h ago', '4h ago', '5h ago', 'Now'],
                datasets: [
                    {
                        label: 'Binary Size (MB)',
                        data: [390, 388, 392, 387, 389, 385],
                        backgroundColor: '#764ba2',
                    }
                ]
            },
            options: chartOptions
        });

        // Populate SLO table
        const sloTableBody = document.getElementById('slo-table-body');
        const sloMetrics = [
            { name: 'First Build Time', target: '≤15s', current: '12s', status: 'pass', deviation: '-20%' },
            { name: 'Incremental Build Time', target: '≤2s', current: '1.5s', status: 'pass', deviation: '-25%' },
            { name: 'Test Execution Time', target: '≤30s', current: '28s', status: 'pass', deviation: '-7%' },
            { name: 'Binary Size', target: '≤500MB', current: '385MB', status: 'pass', deviation: '-23%' },
            { name: 'Memory Usage', target: '≤100MB', current: '85MB', status: 'pass', deviation: '-15%' }
        ];

        sloMetrics.forEach(metric => {
            const row = document.createElement('tr');
            row.innerHTML = `
                <td>${metric.name}</td>
                <td>${metric.target}</td>
                <td>${metric.current}</td>
                <td><span class="status-${metric.status}">${metric.status.toUpperCase()}</span></td>
                <td>${metric.deviation}</td>
            `;
            sloTableBody.appendChild(row);
        });
    </script>
</body>
</html>
EOF
}

################################################################################
# Main Entry Point
################################################################################

main() {
    echo "[INFO] Generating benchmark dashboard..."

    mkdir -p "$METRICS_DIR"

    generate_html_header > "$DASHBOARD_FILE"

    echo "[INFO] ✅ Dashboard generated: $DASHBOARD_FILE"
    echo "[INFO] Open in browser: file://$DASHBOARD_FILE"
}

main "$@"
