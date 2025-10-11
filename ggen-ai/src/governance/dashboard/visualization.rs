//! Data visualization and time-series analysis

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use tracing::debug;

use super::super::error::Result;

/// Time-series metrics for tracking performance over time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimescaleMetrics {
    pub hourly_decisions: Vec<TimeSeriesPoint>,
    pub hourly_violations: Vec<TimeSeriesPoint>,
    pub success_rate_trend: Vec<TimeSeriesPoint>,
}

/// Time series data point
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeSeriesPoint {
    pub timestamp: DateTime<Utc>,
    pub value: f64,
}

/// Data visualization generator
pub struct DataVisualizer {}

impl DataVisualizer {
    /// Create a new data visualizer
    pub fn new() -> Self {
        Self {}
    }

    /// Generate timescale metrics for visualization
    pub async fn generate_timescale_metrics(&self, hours: usize) -> Result<TimescaleMetrics> {
        debug!("Generating timescale metrics for {} hours", hours);

        let mut hourly_decisions = Vec::new();
        let mut hourly_violations = Vec::new();
        let mut success_rate_trend = Vec::new();

        // Generate mock data for demonstration
        // In production, this would query the audit trail
        for hour in 0..hours {
            let timestamp = Utc::now() - chrono::Duration::hours(hour as i64);

            // Mock decision data
            hourly_decisions.push(TimeSeriesPoint {
                timestamp,
                value: (10 + hour * 2) as f64,
            });

            // Mock violation data
            hourly_violations.push(TimeSeriesPoint {
                timestamp,
                value: (2 + hour) as f64,
            });

            // Mock success rate trend
            success_rate_trend.push(TimeSeriesPoint {
                timestamp,
                value: 0.85 + (hour as f64 * 0.01), // Slight improvement over time
            });
        }

        Ok(TimescaleMetrics {
            hourly_decisions,
            hourly_violations,
            success_rate_trend,
        })
    }

    /// Calculate trend direction
    pub fn calculate_trend(&self, data: &[TimeSeriesPoint]) -> TrendDirection {
        if data.len() < 2 {
            return TrendDirection::Stable;
        }

        let first_half: f64 = data
            .iter()
            .take(data.len() / 2)
            .map(|p| p.value)
            .sum::<f64>()
            / (data.len() / 2) as f64;
        let second_half: f64 = data
            .iter()
            .skip(data.len() / 2)
            .map(|p| p.value)
            .sum::<f64>()
            / (data.len() - data.len() / 2) as f64;

        let change_percent = ((second_half - first_half) / first_half) * 100.0;

        if change_percent > 5.0 {
            TrendDirection::Increasing
        } else if change_percent < -5.0 {
            TrendDirection::Decreasing
        } else {
            TrendDirection::Stable
        }
    }

    /// Calculate moving average
    pub fn moving_average(&self, data: &[TimeSeriesPoint], window_size: usize) -> Vec<TimeSeriesPoint> {
        if data.len() < window_size {
            return data.to_vec();
        }

        let mut result = Vec::new();
        for i in window_size - 1..data.len() {
            let avg = data[i - window_size + 1..=i]
                .iter()
                .map(|p| p.value)
                .sum::<f64>()
                / window_size as f64;

            result.push(TimeSeriesPoint {
                timestamp: data[i].timestamp,
                value: avg,
            });
        }

        result
    }
}

/// Trend direction for data analysis
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrendDirection {
    Increasing,
    Decreasing,
    Stable,
}

impl Default for DataVisualizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_timescale_metrics() {
        let visualizer = DataVisualizer::new();
        let metrics = visualizer
            .generate_timescale_metrics(24)
            .await
            .expect("Timescale metrics should succeed");

        assert_eq!(metrics.hourly_decisions.len(), 24);
        assert_eq!(metrics.hourly_violations.len(), 24);
        assert_eq!(metrics.success_rate_trend.len(), 24);

        // Check that success rate improves over time
        let first_rate = metrics.success_rate_trend[0].value;
        let last_rate = metrics.success_rate_trend[23].value;
        assert!(last_rate > first_rate);
    }

    #[test]
    fn test_trend_calculation() {
        let visualizer = DataVisualizer::new();
        let now = Utc::now();

        // Test increasing trend
        let increasing_data = vec![
            TimeSeriesPoint {
                timestamp: now,
                value: 10.0,
            },
            TimeSeriesPoint {
                timestamp: now,
                value: 15.0,
            },
            TimeSeriesPoint {
                timestamp: now,
                value: 20.0,
            },
            TimeSeriesPoint {
                timestamp: now,
                value: 25.0,
            },
        ];

        assert_eq!(
            visualizer.calculate_trend(&increasing_data),
            TrendDirection::Increasing
        );

        // Test stable trend
        let stable_data = vec![
            TimeSeriesPoint {
                timestamp: now,
                value: 10.0,
            },
            TimeSeriesPoint {
                timestamp: now,
                value: 10.5,
            },
            TimeSeriesPoint {
                timestamp: now,
                value: 10.2,
            },
            TimeSeriesPoint {
                timestamp: now,
                value: 10.3,
            },
        ];

        assert_eq!(
            visualizer.calculate_trend(&stable_data),
            TrendDirection::Stable
        );
    }

    #[test]
    fn test_moving_average() {
        let visualizer = DataVisualizer::new();
        let now = Utc::now();

        let data = vec![
            TimeSeriesPoint {
                timestamp: now,
                value: 10.0,
            },
            TimeSeriesPoint {
                timestamp: now,
                value: 20.0,
            },
            TimeSeriesPoint {
                timestamp: now,
                value: 30.0,
            },
            TimeSeriesPoint {
                timestamp: now,
                value: 40.0,
            },
        ];

        let avg = visualizer.moving_average(&data, 2);
        assert_eq!(avg.len(), 3);
        assert_eq!(avg[0].value, 15.0);
        assert_eq!(avg[1].value, 25.0);
        assert_eq!(avg[2].value, 35.0);
    }
}
