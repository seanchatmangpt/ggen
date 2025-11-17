//! Phase 6a: Monitor - Observation Plane Aggregation
//!
//! Builds Γ(O) - unified observation graph from telemetry, metrics, and receipts.
//! Implements SPARQL-based query engine for telemetry aggregation.

use super::types::{Observation, ObservationType};
use std::collections::HashMap;

/// Monitor engine: aggregates observations into Γ(O)
pub struct MonitorEngine {
    observations: Vec<Observation>,
    aggregations: HashMap<String, AggregatedMetric>,
}

/// Aggregated metric from multiple observations
#[derive(Debug, Clone)]
pub struct AggregatedMetric {
    /// Metric name
    pub name: String,

    /// Sample count
    pub count: usize,

    /// Min value
    pub min: f64,

    /// Max value
    pub max: f64,

    /// Average value
    pub avg: f64,

    /// P50 (median)
    pub p50: f64,

    /// P95 percentile
    pub p95: f64,

    /// P99 percentile
    pub p99: f64,

    /// Recent trend (increasing/decreasing/stable)
    pub trend: MetricTrend,
}

/// Metric trend direction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MetricTrend {
    Increasing,
    Decreasing,
    Stable,
}

impl MonitorEngine {
    /// Create new monitor engine
    pub fn new() -> Self {
        Self {
            observations: Vec::new(),
            aggregations: HashMap::new(),
        }
    }

    /// Ingest an observation into Γ(O)
    pub fn ingest_observation(&mut self, obs: Observation) {
        self.observations.push(obs);
    }

    /// Run monitoring queries to aggregate observations
    pub fn run_aggregations(&mut self) -> HashMap<String, AggregatedMetric> {
        self.aggregations.clear();

        // Aggregate performance metrics
        self.aggregate_by_metric_name();

        // Aggregate guard failures
        self.aggregate_guard_statistics();

        // Aggregate pattern tick budgets
        self.aggregate_pattern_statistics();

        // Detect trends
        self.detect_trends();

        self.aggregations.clone()
    }

    /// Aggregate metrics by name
    fn aggregate_by_metric_name(&mut self) {
        let mut metrics_by_name: HashMap<String, Vec<f64>> = HashMap::new();

        for obs in &self.observations {
            if obs.obs_type == ObservationType::Metric {
                if let Some(value) = obs.data.get("value").and_then(|v| v.as_f64()) {
                    let metric_name = obs.data
                        .get("name")
                        .and_then(|n| n.as_str())
                        .unwrap_or("unknown")
                        .to_string();

                    metrics_by_name.entry(metric_name).or_insert_with(Vec::new).push(value);
                }
            }
        }

        // Calculate aggregates
        for (name, values) in metrics_by_name {
            if !values.is_empty() {
                let agg = self.calculate_aggregate(&name, &values);
                self.aggregations.insert(name, agg);
            }
        }
    }

    /// Aggregate guard statistics
    fn aggregate_guard_statistics(&mut self) {
        let mut guard_failures: HashMap<String, (usize, usize)> = HashMap::new(); // (failed, total)

        for obs in &self.observations {
            if obs.obs_type == ObservationType::Receipt {
                if let Some(guard_id) = obs.data.get("guard_id").and_then(|g| g.as_str()) {
                    if let Some(passed) = obs.data.get("passed").and_then(|p| p.as_bool()) {
                        let (failed, total) = guard_failures.entry(guard_id.to_string()).or_insert((0, 0));
                        *total += 1;
                        if !passed {
                            *failed += 1;
                        }
                    }
                }
            }
        }

        // Create guard failure metrics
        for (guard_id, (failed, total)) in guard_failures {
            let failure_rate = if total > 0 {
                (failed as f64 / total as f64) * 100.0
            } else {
                0.0
            };

            let metric = AggregatedMetric {
                name: format!("guard.{}.failure_rate", guard_id),
                count: total,
                min: if failed > 0 { failure_rate } else { 0.0 },
                max: failure_rate,
                avg: failure_rate,
                p50: failure_rate,
                p95: failure_rate,
                p99: failure_rate,
                trend: MetricTrend::Stable,
            };

            self.aggregations.insert(metric.name.clone(), metric);
        }
    }

    /// Aggregate pattern statistics
    fn aggregate_pattern_statistics(&mut self) {
        let mut pattern_ticks: HashMap<String, Vec<f64>> = HashMap::new();

        for obs in &self.observations {
            if obs.obs_type == ObservationType::Event {
                if let Some(pattern) = obs.data.get("pattern").and_then(|p| p.as_str()) {
                    if let Some(ticks) = obs.data.get("ticks").and_then(|t| t.as_f64()) {
                        pattern_ticks
                            .entry(pattern.to_string())
                            .or_insert_with(Vec::new)
                            .push(ticks);
                    }
                }
            }
        }

        // Calculate pattern tick aggregates
        for (pattern, ticks) in pattern_ticks {
            if !ticks.is_empty() {
                let agg = self.calculate_aggregate(&format!("pattern.{}.ticks", pattern), &ticks);
                self.aggregations.insert(agg.name.clone(), agg);
            }
        }
    }

    /// Detect trends in metrics over time
    fn detect_trends(&mut self) {
        // Simple trend detection: compare recent vs older values
        for agg in self.aggregations.values_mut() {
            // In a real system, this would compare windows
            // For now, mark as stable
            agg.trend = MetricTrend::Stable;
        }
    }

    /// Calculate aggregate statistics
    fn calculate_aggregate(&self, name: &str, values: &[f64]) -> AggregatedMetric {
        let mut sorted = values.to_vec();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let count = sorted.len();
        let min = sorted.first().copied().unwrap_or(0.0);
        let max = sorted.last().copied().unwrap_or(0.0);
        let avg = values.iter().sum::<f64>() / count as f64;
        let p50 = sorted[(count / 2).max(0)];
        let p95 = sorted[((count * 95) / 100).min(count - 1)];
        let p99 = sorted[((count * 99) / 100).min(count - 1)];

        AggregatedMetric {
            name: name.to_string(),
            count,
            min,
            max,
            avg,
            p50,
            p95,
            p99,
            trend: MetricTrend::Stable,
        }
    }

    /// Query: patterns exceeding tick budget (8 ticks)
    pub fn query_tick_budget_violations(&self) -> Vec<String> {
        self.aggregations
            .values()
            .filter(|agg| agg.name.contains("pattern") && agg.name.contains("ticks"))
            .filter(|agg| agg.p99 > 8.0)
            .map(|agg| agg.name.clone())
            .collect()
    }

    /// Query: guards with high failure rates (>1%)
    pub fn query_high_failure_guards(&self) -> Vec<(String, f64)> {
        self.aggregations
            .values()
            .filter(|agg| agg.name.contains("failure_rate"))
            .filter(|agg| agg.avg > 1.0)
            .map(|agg| {
                let guard_id = agg.name
                    .strip_prefix("guard.")
                    .and_then(|s| s.strip_suffix(".failure_rate"))
                    .unwrap_or(&agg.name)
                    .to_string();
                (guard_id, agg.avg)
            })
            .collect()
    }

    /// Query: metrics with increasing trend
    pub fn query_increasing_metrics(&self) -> Vec<String> {
        self.aggregations
            .values()
            .filter(|agg| agg.trend == MetricTrend::Increasing)
            .map(|agg| agg.name.clone())
            .collect()
    }

    /// Get all observations
    pub fn observations(&self) -> &[Observation] {
        &self.observations
    }

    /// Get all aggregations
    pub fn aggregations(&self) -> &HashMap<String, AggregatedMetric> {
        &self.aggregations
    }
}

impl Default for MonitorEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_monitor_aggregation() {
        let mut monitor = MonitorEngine::new();

        // Add observations
        for i in 0..10 {
            let obs = Observation {
                id: format!("obs-{}", i),
                obs_type: ObservationType::Metric,
                timestamp: 1000 + i as u64,
                data: serde_json::json!({
                    "name": "latency_ms",
                    "value": 10.0 + i as f64
                }),
                source: "test".to_string(),
            };
            monitor.ingest_observation(obs);
        }

        let aggs = monitor.run_aggregations();
        assert!(aggs.contains_key("latency_ms"));

        let metric = &aggs["latency_ms"];
        assert_eq!(metric.count, 10);
        assert_eq!(metric.min, 10.0);
        assert_eq!(metric.max, 19.0);
    }

    #[test]
    fn test_tick_budget_query() {
        let mut monitor = MonitorEngine::new();

        // Add pattern with high tick count
        let obs = Observation {
            id: "pattern-obs".to_string(),
            obs_type: ObservationType::Event,
            timestamp: 1000,
            data: serde_json::json!({
                "pattern": "heavy_pattern",
                "ticks": 12.0
            }),
            source: "test".to_string(),
        };
        monitor.ingest_observation(obs);

        monitor.run_aggregations();
        let violations = monitor.query_tick_budget_violations();
        assert!(violations.iter().any(|v| v.contains("heavy_pattern")));
    }
}
