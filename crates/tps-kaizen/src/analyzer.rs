//! Kaizen metric analysis and improvement recommendations
//!
//! Analyzes metrics for improvement opportunities:
//! - Pattern detection (is circuit breaker opening too often?)
//! - SLO attainment tracking
//! - Trend analysis (is latency improving or degrading?)
//! - Root cause detection (what changed when metric degraded?)
//! - Actionable recommendations (scale service, add workers, etc.)

use crate::{KaizenMetrics, Recommendation, Result, Slo, TimestampedValue};
use std::collections::HashMap;
use std::sync::Arc;

/// Statistical analysis result
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct TrendAnalysis {
    /// Metric name
    pub metric: String,
    /// Current value
    pub current_value: f64,
    /// Mean over window
    pub mean: f64,
    /// Trend direction (-1.0 to 1.0, negative = getting worse)
    pub trend: f64,
    /// Percentile P95
    pub p95: f64,
    /// Percentile P99
    pub p99: f64,
}

/// Metric analyzer for SLO tracking and recommendations
#[derive(Clone)]
pub struct MetricAnalyzer {
    metrics: Arc<KaizenMetrics>,
}

impl MetricAnalyzer {
    /// Create new metric analyzer
    pub fn new(metrics: Arc<KaizenMetrics>) -> Self {
        Self { metrics }
    }

    /// Check SLO attainment
    pub async fn check_slo_attainment(&self, slo: &Slo) -> Result<(bool, f64)> {
        let history = self.metrics.get_history(&slo.metric);

        if history.is_empty() {
            return Ok((true, 100.0)); // No data = assume attainment
        }

        let now = chrono::Utc::now();
        let window_start = now - chrono::Duration::seconds(slo.window_secs as i64);

        let values: Vec<f64> = history
            .iter()
            .filter(|v| v.timestamp >= window_start && v.timestamp <= now)
            .map(|v| v.value)
            .collect();

        if values.is_empty() {
            return Ok((true, 100.0));
        }

        let mean = values.iter().sum::<f64>() / values.len() as f64;
        let attainment = if slo.is_maximum {
            // For maximums (like success rate), check if we're above target
            if mean >= slo.target {
                100.0
            } else {
                (mean / slo.target) * 100.0
            }
        } else {
            // For minimums (like latency), check if we're below target
            if mean <= slo.target {
                100.0
            } else {
                (slo.target / mean) * 100.0
            }
        };

        let met = attainment >= 99.0; // Allow 1% variance
        Ok((met, attainment.min(100.0)))
    }

    /// Analyze trend in metric (returns -1.0 to 1.0 where negative = degrading)
    pub async fn analyze_trend(&self, metric: &str, window_secs: u64) -> Result<TrendAnalysis> {
        let history = self.metrics.get_history(metric);

        if history.len() < 2 {
            return Ok(TrendAnalysis {
                metric: metric.to_string(),
                current_value: 0.0,
                mean: 0.0,
                trend: 0.0,
                p95: 0.0,
                p99: 0.0,
            });
        }

        let now = chrono::Utc::now();
        let window_start = now - chrono::Duration::seconds(window_secs as i64);

        let mut values: Vec<(chrono::DateTime<chrono::Utc>, f64)> = history
            .iter()
            .filter(|v| v.timestamp >= window_start && v.timestamp <= now)
            .map(|v| (v.timestamp, v.value))
            .collect();

        values.sort_by_key(|v| v.0);

        if values.is_empty() {
            return Ok(TrendAnalysis {
                metric: metric.to_string(),
                current_value: 0.0,
                mean: 0.0,
                trend: 0.0,
                p95: 0.0,
                p99: 0.0,
            });
        }

        let values_only: Vec<f64> = values.iter().map(|v| v.1).collect();
        let mean = values_only.iter().sum::<f64>() / values_only.len() as f64;
        let current_value = values_only.last().copied().unwrap_or(0.0);

        // Calculate trend using linear regression
        let n = values.len() as f64;
        let x_mean = (n - 1.0) / 2.0; // Time indices 0, 1, 2, ... n-1

        let mut numerator = 0.0;
        let mut denominator = 0.0;

        for (i, &(_, y)) in values.iter().enumerate() {
            let x = i as f64;
            numerator += (x - x_mean) * (y - mean);
            denominator += (x - x_mean).powi(2);
        }

        let slope = if denominator.abs() > f64::EPSILON {
            numerator / denominator
        } else {
            0.0
        };

        // Normalize slope to -1.0 to 1.0 range
        let trend = if mean.abs() > f64::EPSILON {
            (slope / mean).clamp(-1.0, 1.0)
        } else {
            0.0
        };

        // Calculate percentiles
        let mut sorted_values = values_only.clone();
        sorted_values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let p95_idx = ((sorted_values.len() as f64 * 0.95) as usize).min(sorted_values.len() - 1);
        let p99_idx = ((sorted_values.len() as f64 * 0.99) as usize).min(sorted_values.len() - 1);

        let p95 = sorted_values[p95_idx];
        let p99 = sorted_values[p99_idx];

        Ok(TrendAnalysis {
            metric: metric.to_string(),
            current_value,
            mean,
            trend,
            p95,
            p99,
        })
    }

    /// Generate improvement recommendations
    pub async fn analyze(&self) -> Result<Vec<Recommendation>> {
        let mut recommendations = Vec::new();

        // Check Jidoka (circuit breaker health)
        if self.metrics.jidoka.circuit_open_percent.get() > 10.0 {
            recommendations.push(Recommendation {
                title: "Reduce Circuit Breaker Failures".to_string(),
                description: "Too many circuits are open. Service health is degraded.".to_string(),
                metric: "jidoka_circuit_open_percent".to_string(),
                current_value: self.metrics.jidoka.circuit_open_percent.get(),
                target_value: 5.0,
                confidence: 0.9,
                action: "Scale service, add redundancy, or investigate root cause of failures".to_string(),
                priority: 1, // High priority
            });
        }

        // Check Kanban (queue health)
        let queue_depth = self.metrics.kanban.queue_depth.get();
        let latency_p99 = self.metrics.kanban.latency_p99_ms.get();

        if queue_depth > 100.0 && latency_p99 > 500.0 {
            recommendations.push(Recommendation {
                title: "Add Workers to Kanban Queue".to_string(),
                description: "Queue is deep and latency is high. Need more processing capacity.".to_string(),
                metric: "kanban_queue_depth".to_string(),
                current_value: queue_depth,
                target_value: 50.0,
                confidence: 0.95,
                action: "Spawn additional workers to process queue faster".to_string(),
                priority: 1,
            });
        }

        // Check Andon (alert fatigue)
        let alert_freq = self.metrics.andon.alert_frequency_per_min.get();
        if alert_freq > 5.0 {
            recommendations.push(Recommendation {
                title: "Reduce Alert Fatigue".to_string(),
                description: "Too many alerts are firing. May indicate systemic issues.".to_string(),
                metric: "andon_alerts_per_min".to_string(),
                current_value: alert_freq,
                target_value: 2.0,
                confidence: 0.8,
                action: "Tune alert thresholds, fix underlying issues, reduce noisy alerts".to_string(),
                priority: 2,
            });
        }

        // Check Heijunka (load balance)
        let load_balance = self.metrics.heijunka.load_balance_coefficient.get();
        if load_balance < 0.7 {
            recommendations.push(Recommendation {
                title: "Improve Load Balancing".to_string(),
                description: "Load is unevenly distributed across workers.".to_string(),
                metric: "heijunka_load_balance_coeff".to_string(),
                current_value: load_balance,
                target_value: 0.9,
                confidence: 0.85,
                action: "Rebalance work distribution, adjust worker scheduling".to_string(),
                priority: 2,
            });
        }

        // Check for latency degradation trend
        let latency_trend = self.analyze_trend("kanban_latency_p99_ms", 3600).await?;
        if latency_trend.trend < -0.1 {
            // Negative trend means degrading performance
            recommendations.push(Recommendation {
                title: "Investigate Latency Degradation".to_string(),
                description: format!(
                    "Latency is trending worse. Current P99: {:.1}ms, Mean: {:.1}ms",
                    latency_trend.current_value, latency_trend.mean
                ),
                metric: "kanban_latency_p99_ms".to_string(),
                current_value: latency_trend.current_value,
                target_value: latency_trend.mean,
                confidence: 0.75,
                action: "Root cause analysis: check resource utilization, external dependencies, data growth".to_string(),
                priority: 1,
            });
        }

        // Sort by priority
        recommendations.sort_by_key(|r| r.priority);

        Ok(recommendations)
    }

    /// Get comprehensive health score (0-100)
    pub async fn health_score(&self) -> Result<f64> {
        let mut scores = Vec::new();

        // Jidoka health (lower circuit open % is better)
        let jidoka_score = (100.0 - self.metrics.jidoka.circuit_open_percent.get().min(100.0)).max(0.0);
        scores.push(jidoka_score);

        // Kanban health (latency and queue depth)
        let latency_score = (100.0 - self.metrics.kanban.latency_p99_ms.get().min(1000.0) / 10.0).max(0.0);
        let queue_score = (100.0 - self.metrics.kanban.queue_depth.get().min(1000.0) / 10.0).max(0.0);
        scores.push((latency_score + queue_score) / 2.0);

        // Andon health (fewer alerts is better)
        let alert_score = (100.0 - self.metrics.andon.alert_frequency_per_min.get().min(100.0) * 10.0).max(0.0);
        scores.push(alert_score);

        // Heijunka health (load balance closer to 1.0 is better)
        let heijunka_score = self.metrics.heijunka.load_balance_coefficient.get() * 100.0;
        scores.push(heijunka_score);

        Ok(scores.iter().sum::<f64>() / scores.len() as f64)
    }

    /// Detect if there's a correlation between events and metric changes
    pub async fn detect_correlation(&self, event_type: &str, metric: &str) -> Result<f64> {
        // Simplified correlation: count events within window of metric spikes
        // In production, this would use proper statistical correlation (Pearson, Spearman)

        let history = self.metrics.get_history(metric);
        if history.len() < 2 {
            return Ok(0.0);
        }

        let mean = history.iter().map(|v| v.value).sum::<f64>() / history.len() as f64;

        // Count "spikes" (values > 1.5 * mean)
        let spike_count = history.iter().filter(|v| v.value > mean * 1.5).count();

        // In real implementation, would correlate with actual event timestamps
        // For now, return rough estimate based on spike frequency
        let correlation = (spike_count as f64 / history.len() as f64).min(1.0);

        Ok(correlation)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_health_score() {
        let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
        let analyzer = MetricAnalyzer::new(metrics);

        let score = analyzer.health_score().await.expect("Failed to get health score");

        assert!(score >= 0.0 && score <= 100.0);
        assert!(score > 80.0); // Should be high with default metrics
    }

    #[tokio::test]
    async fn test_slo_attainment() {
        let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
        let analyzer = MetricAnalyzer::new(metrics.clone());

        let slo = Slo {
            name: "latency_p99".to_string(),
            metric: "kanban_latency_p99_ms".to_string(),
            target: 100.0,
            window_secs: 300,
            is_maximum: false,
        };

        let (met, attainment) = analyzer
            .check_slo_attainment(&slo)
            .await
            .expect("Failed to check SLO");

        assert!(met);
        assert_eq!(attainment, 100.0); // No data = assume attainment
    }

    #[tokio::test]
    async fn test_trend_analysis() {
        let metrics = Arc::new(KaizenMetrics::new().expect("Failed to create metrics"));
        let analyzer = MetricAnalyzer::new(metrics.clone());

        // Simulate latency getting worse over time
        for i in 0..10 {
            let mut tags = std::collections::HashMap::new();
            tags.insert("metric".to_string(), "latency".to_string());
            metrics.record_value("test_metric".to_string(), 100.0 + (i as f64 * 10.0), tags);
        }

        let trend = analyzer
            .analyze_trend("test_metric", 300)
            .await
            .expect("Failed to analyze trend");

        assert!(trend.trend > 0.0); // Should be positive (getting worse)
    }
}
