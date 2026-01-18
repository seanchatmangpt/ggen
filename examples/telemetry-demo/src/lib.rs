use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metric {
    pub name: String,
    pub value: f64,
}

#[derive(Debug, Clone)]
pub struct Telemetry {
    metrics: Vec<Metric>,
}

impl Default for Telemetry {
    fn default() -> Self {
        Self::new()
    }
}

impl Telemetry {
    pub fn new() -> Self {
        Self {
            metrics: Vec::new(),
        }
    }

    pub fn record(&mut self, metric: Metric) {
        self.metrics.push(metric);
    }

    pub fn metrics(&self) -> &[Metric] {
        &self.metrics
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_telemetry_creation() {
        let telemetry = Telemetry::new();
        assert_eq!(telemetry.metrics().len(), 0);
    }

    #[test]
    fn test_record_metric() {
        let mut telemetry = Telemetry::new();
        telemetry.record(Metric {
            name: "latency_ms".to_string(),
            value: 42.5,
        });
        assert_eq!(telemetry.metrics().len(), 1);
    }
}
