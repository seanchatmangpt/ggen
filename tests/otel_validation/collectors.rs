//! Trace collectors and exporters
//!
//! Utilities for collecting and exporting OpenTelemetry traces
//! for validation purposes.

use super::*;
use opentelemetry::trace::{Span, SpanKind, Status, Tracer};
use std::sync::Arc;
use tracing_subscriber::layer::SubscriberExt;

/// In-memory trace exporter for testing
pub struct InMemoryExporter {
    collector: Arc<TraceCollector>,
}

impl InMemoryExporter {
    pub fn new(collector: Arc<TraceCollector>) -> Self {
        Self { collector }
    }

    pub fn export_span(&self, span: SpanRecord) {
        self.collector.record_span(span);
    }
}

/// Span builder for test validation
pub struct TestSpanBuilder {
    name: String,
    attributes: HashMap<String, String>,
}

impl TestSpanBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            attributes: HashMap::new(),
        }
    }

    pub fn with_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.attributes.insert(key.into(), value.into());
        self
    }

    pub fn build(self, duration_ms: f64, status: SpanStatus) -> SpanRecord {
        SpanRecord {
            name: self.name,
            duration_ms,
            attributes: self.attributes,
            status,
        }
    }
}

/// Trace assertion helper
pub struct TraceAsserter<'a> {
    collector: &'a TraceCollector,
}

impl<'a> TraceAsserter<'a> {
    pub fn new(collector: &'a TraceCollector) -> Self {
        Self { collector }
    }

    /// Assert that a span with the given name exists
    pub fn span_exists(&self, name: &str) -> Result<&Self> {
        self.collector.assert_span_exists(name)?;
        Ok(self)
    }

    /// Assert that a span completed successfully
    pub fn span_succeeded(&self, name: &str) -> Result<&Self> {
        self.collector.assert_span_success(name)?;
        Ok(self)
    }

    /// Assert that a span completed within duration
    pub fn span_duration_under(&self, name: &str, max_ms: f64) -> Result<&Self> {
        self.collector.assert_duration_under(name, max_ms)?;
        Ok(self)
    }

    /// Assert that a span has specific attribute
    pub fn span_has_attribute(&self, name: &str, key: &str, value: &str) -> Result<&Self> {
        let span = self
            .collector
            .find_span(name)
            .context(format!("Span '{}' not found", name))?;

        let actual = span
            .attributes
            .get(key)
            .context(format!("Attribute '{}' not found in span '{}'", key, name))?;

        if actual != value {
            anyhow::bail!(
                "Attribute '{}' in span '{}': expected '{}', got '{}'",
                key,
                name,
                value,
                actual
            );
        }

        Ok(self)
    }

    /// Assert metric value
    pub fn metric_equals(&self, name: &str, expected: f64) -> Result<&Self> {
        let metrics = self.collector.get_metrics();
        let actual = metrics
            .get(name)
            .context(format!("Metric '{}' not found", name))?;

        if (actual - expected).abs() > f64::EPSILON {
            anyhow::bail!("Metric '{}': expected {}, got {}", name, expected, actual);
        }

        Ok(self)
    }

    /// Assert metric is within range
    pub fn metric_in_range(&self, name: &str, min: f64, max: f64) -> Result<&Self> {
        let metrics = self.collector.get_metrics();
        let actual = metrics
            .get(name)
            .context(format!("Metric '{}' not found", name))?;

        if *actual < min || *actual > max {
            anyhow::bail!(
                "Metric '{}': expected in range [{}, {}], got {}",
                name,
                min,
                max,
                actual
            );
        }

        Ok(self)
    }
}

/// Performance metrics collector
#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    pub generation_time_ms: f64,
    pub memory_usage_mb: f64,
    pub cpu_usage_percent: f64,
}

impl PerformanceMetrics {
    pub fn validate_slos(&self) -> Result<()> {
        // Validate against README performance SLOs
        if self.generation_time_ms > 3000.0 {
            anyhow::bail!(
                "Generation time {}ms exceeds SLO of 3000ms",
                self.generation_time_ms
            );
        }

        if self.memory_usage_mb > 100.0 {
            anyhow::bail!(
                "Memory usage {}MB exceeds SLO of 100MB",
                self.memory_usage_mb
            );
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_builder() {
        let span = TestSpanBuilder::new("test")
            .with_attribute("key1", "value1")
            .with_attribute("key2", "value2")
            .build(100.0, SpanStatus::Ok);

        assert_eq!(span.name, "test");
        assert_eq!(span.duration_ms, 100.0);
        assert_eq!(span.attributes.len(), 2);
        assert_eq!(span.status, SpanStatus::Ok);
    }

    #[test]
    fn test_trace_asserter() {
        let collector = TraceCollector::new();
        let span = TestSpanBuilder::new("test")
            .with_attribute("attr", "value")
            .build(100.0, SpanStatus::Ok);

        collector.record_span(span);

        let asserter = TraceAsserter::new(&collector);
        asserter
            .span_exists("test")
            .unwrap()
            .span_succeeded("test")
            .unwrap()
            .span_duration_under("test", 200.0)
            .unwrap()
            .span_has_attribute("test", "attr", "value")
            .unwrap();
    }

    #[test]
    fn test_performance_metrics_validation() {
        let metrics = PerformanceMetrics {
            generation_time_ms: 2000.0,
            memory_usage_mb: 50.0,
            cpu_usage_percent: 60.0,
        };

        assert!(metrics.validate_slos().is_ok());

        let bad_metrics = PerformanceMetrics {
            generation_time_ms: 5000.0, // Exceeds SLO
            memory_usage_mb: 50.0,
            cpu_usage_percent: 60.0,
        };

        assert!(bad_metrics.validate_slos().is_err());
    }
}
