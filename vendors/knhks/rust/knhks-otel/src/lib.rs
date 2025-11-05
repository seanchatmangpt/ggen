// rust/knhks-otel/src/lib.rs
// OpenTelemetry Observability Integration
// Provides metrics, traces, and spans for KNHKS operations

#![no_std]
extern crate alloc;

use alloc::vec::Vec;
use alloc::string::String;
use alloc::collections::BTreeMap;

/// Trace ID (128-bit)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraceId(pub u128);

/// Span ID (64-bit)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpanId(pub u64);

/// Span context
#[derive(Debug, Clone)]
pub struct SpanContext {
    pub trace_id: TraceId,
    pub span_id: SpanId,
    pub parent_span_id: Option<SpanId>,
    pub flags: u8,
}

/// Span attributes
pub type Attributes = BTreeMap<String, String>;

/// Span event
#[derive(Debug, Clone)]
pub struct SpanEvent {
    pub name: String,
    pub timestamp_ms: u64,
    pub attributes: Attributes,
}

/// Span
#[derive(Debug, Clone)]
pub struct Span {
    pub context: SpanContext,
    pub name: String,
    pub start_time_ms: u64,
    pub end_time_ms: Option<u64>,
    pub attributes: Attributes,
    pub events: Vec<SpanEvent>,
    pub status: SpanStatus,
}

/// Span status
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanStatus {
    Ok,
    Error,
    Unset,
}

/// Metric value
#[derive(Debug, Clone)]
pub enum MetricValue {
    Counter(u64),
    Gauge(f64),
    Histogram(Vec<u64>),
}

/// Metric
#[derive(Debug, Clone)]
pub struct Metric {
    pub name: String,
    pub value: MetricValue,
    pub timestamp_ms: u64,
    pub attributes: Attributes,
}

/// OTEL tracer
pub struct Tracer {
    spans: Vec<Span>,
    metrics: Vec<Metric>,
}

impl Tracer {
    pub fn new() -> Self {
        Self {
            spans: Vec::new(),
            metrics: Vec::new(),
        }
    }

    /// Start a new span
    pub fn start_span(&mut self, name: String, parent: Option<SpanContext>) -> SpanContext {
        let trace_id = parent.map(|p| p.trace_id).unwrap_or_else(|| {
            // Generate new trace ID
            TraceId(0) // Placeholder - real implementation uses random
        });

        let span_id = SpanId(0); // Placeholder - real implementation uses random
        let parent_span_id = parent.map(|p| p.span_id);

        let context = SpanContext {
            trace_id,
            span_id,
            parent_span_id,
            flags: 1, // sampled
        };

        let span = Span {
            context,
            name: name.clone(),
            start_time_ms: 0, // TODO: Get actual timestamp
            end_time_ms: None,
            attributes: BTreeMap::new(),
            events: Vec::new(),
            status: SpanStatus::Unset,
        };

        self.spans.push(span);
        context
    }

    /// End a span
    pub fn end_span(&mut self, context: SpanContext, status: SpanStatus) {
        if let Some(span) = self.spans.iter_mut().find(|s| s.context.span_id == context.span_id) {
            span.end_time_ms = Some(0); // TODO: Get actual timestamp
            span.status = status;
        }
    }

    /// Add event to span
    pub fn add_event(&mut self, context: SpanContext, event: SpanEvent) {
        if let Some(span) = self.spans.iter_mut().find(|s| s.context.span_id == context.span_id) {
            span.events.push(event);
        }
    }

    /// Add attribute to span
    pub fn add_attribute(&mut self, context: SpanContext, key: String, value: String) {
        if let Some(span) = self.spans.iter_mut().find(|s| s.context.span_id == context.span_id) {
            span.attributes.insert(key, value);
        }
    }

    /// Record metric
    pub fn record_metric(&mut self, metric: Metric) {
        self.metrics.push(metric);
    }

    /// Get span by ID
    pub fn get_span(&self, span_id: SpanId) -> Option<&Span> {
        self.spans.iter().find(|s| s.context.span_id == span_id)
    }

    /// Get all spans
    pub fn spans(&self) -> &[Span] {
        &self.spans
    }

    /// Get all metrics
    pub fn metrics(&self) -> &[Metric] {
        &self.metrics
    }

    /// Get metrics for specific name
    pub fn get_metrics(&self, name: &str) -> Vec<&Metric> {
        self.metrics.iter().filter(|m| m.name == name).collect()
    }
}

impl Default for Tracer {
    fn default() -> Self {
        Self::new()
    }
}

/// Metrics helper functions
pub struct MetricsHelper;

impl MetricsHelper {
    /// Record hook execution latency
    pub fn record_hook_latency(tracer: &mut Tracer, ticks: u32, operation: &str) {
        let metric = Metric {
            name: "knhks.hook.latency.ticks".to_string(),
            value: MetricValue::Histogram(vec![ticks as u64]),
            timestamp_ms: 0, // TODO: Get actual timestamp
            attributes: {
                let mut attrs = BTreeMap::new();
                attrs.insert("operation".to_string(), operation.to_string());
                attrs
            },
        };
        tracer.record_metric(metric);
    }

    /// Record receipt generation
    pub fn record_receipt(tracer: &mut Tracer, receipt_id: &str) {
        let metric = Metric {
            name: "knhks.receipt.generated".to_string(),
            value: MetricValue::Counter(1),
            timestamp_ms: 0, // TODO: Get actual timestamp
            attributes: {
                let mut attrs = BTreeMap::new();
                attrs.insert("receipt_id".to_string(), receipt_id.to_string());
                attrs
            },
        };
        tracer.record_metric(metric);
    }

    /// Record guard violation
    pub fn record_guard_violation(tracer: &mut Tracer, guard_type: &str) {
        let metric = Metric {
            name: "knhks.guard.violation".to_string(),
            value: MetricValue::Counter(1),
            timestamp_ms: 0, // TODO: Get actual timestamp
            attributes: {
                let mut attrs = BTreeMap::new();
                attrs.insert("guard_type".to_string(), guard_type.to_string());
                attrs
            },
        };
        tracer.record_metric(metric);
    }

    /// Record connector throughput
    pub fn record_connector_throughput(tracer: &mut Tracer, connector_id: &str, triples: usize) {
        let metric = Metric {
            name: "knhks.connector.throughput".to_string(),
            value: MetricValue::Counter(triples as u64),
            timestamp_ms: 0, // TODO: Get actual timestamp
            attributes: {
                let mut attrs = BTreeMap::new();
                attrs.insert("connector_id".to_string(), connector_id.to_string());
                attrs
            },
        };
        tracer.record_metric(metric);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tracer_span() {
        let mut tracer = Tracer::new();
        let context = tracer.start_span("test_span".to_string(), None);
        
        tracer.add_attribute(context, "key".to_string(), "value".to_string());
        tracer.end_span(context, SpanStatus::Ok);

        assert_eq!(tracer.spans().len(), 1);
    }

    #[test]
    fn test_metrics_recording() {
        let mut tracer = Tracer::new();
        MetricsHelper::record_hook_latency(&mut tracer, 5, "ASK_SP");
        MetricsHelper::record_receipt(&mut tracer, "receipt1");

        assert_eq!(tracer.metrics().len(), 2);
    }
}

