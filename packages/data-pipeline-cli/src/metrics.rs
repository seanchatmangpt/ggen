//! Metrics module - Performance monitoring

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metrics {
    pub throughput: f64,
    pub latency_ms: f64,
    pub error_rate: f64,
}

pub struct MetricsCollector {
    metrics: Metrics,
}

impl MetricsCollector {
    pub fn new() -> Self {
        Self {
            metrics: Metrics {
                throughput: 0.0,
                latency_ms: 0.0,
                error_rate: 0.0,
            },
        }
    }

    pub fn collect(&self) -> &Metrics {
        &self.metrics
    }
}

impl Default for MetricsCollector {
    fn default() -> Self {
        Self::new()
    }
}
