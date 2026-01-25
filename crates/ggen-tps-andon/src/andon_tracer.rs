//! OpenTelemetry distributed tracing for Andon system
//!
//! Implements distributed tracing similar to Jaeger:
//! - Span creation and context propagation
//! - Request ID tracking through system
//! - Trace visualization with timing information
//! - Automatic span creation for async operations

use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Tracer configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracerConfig {
    /// Service name for tracing
    #[serde(default = "default_service_name")]
    pub service_name: String,

    /// Service version
    #[serde(default = "default_service_version")]
    pub service_version: String,

    /// Enable tracing
    #[serde(default = "default_tracing_enabled")]
    pub enabled: bool,

    /// Trace sampling ratio (0.0 to 1.0)
    /// - 1.0 = trace all requests
    /// - 0.01 = trace 1% of requests (production)
    /// - 0.001 = trace 0.1% of requests (high-volume)
    #[serde(default = "default_sampling_ratio")]
    pub sampling_ratio: f64,

    /// OTLP exporter endpoint (Jaeger, Cloud Trace)
    #[serde(default = "default_otlp_endpoint")]
    pub otlp_endpoint: String,

    /// Maximum span batch size before flushing
    #[serde(default = "default_batch_size")]
    pub batch_size: usize,

    /// Batch timeout (seconds)
    #[serde(default = "default_batch_timeout")]
    pub batch_timeout_secs: u64,
}

fn default_service_name() -> String {
    "ggen-andon".to_string()
}

fn default_service_version() -> String {
    "0.1.0".to_string()
}

fn default_tracing_enabled() -> bool {
    true
}

fn default_sampling_ratio() -> f64 {
    0.01 // 1% sampling in production
}

fn default_otlp_endpoint() -> String {
    "http://localhost:4317".to_string()
}

fn default_batch_size() -> usize {
    512
}

fn default_batch_timeout() -> u64 {
    5
}

impl Default for TracerConfig {
    fn default() -> Self {
        Self {
            service_name: default_service_name(),
            service_version: default_service_version(),
            enabled: true,
            sampling_ratio: default_sampling_ratio(),
            otlp_endpoint: default_otlp_endpoint(),
            batch_size: default_batch_size(),
            batch_timeout_secs: default_batch_timeout(),
        }
    }
}

/// Span context for trace propagation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanContext {
    /// Trace ID (unique per request)
    pub trace_id: String,

    /// Span ID (unique per operation)
    pub span_id: String,

    /// Parent span ID (if nested)
    pub parent_span_id: Option<String>,

    /// Sampling decision
    pub sampled: bool,

    /// Baggage (key-value pairs carried through trace)
    pub baggage: std::collections::HashMap<String, String>,
}

impl SpanContext {
    /// Create a new root span context
    pub fn new_root() -> Self {
        Self {
            trace_id: uuid::Uuid::new_v4().to_string(),
            span_id: uuid::Uuid::new_v4().to_string(),
            parent_span_id: None,
            sampled: true,
            baggage: std::collections::HashMap::new(),
        }
    }

    /// Create a child span context
    pub fn child(&self) -> Self {
        Self {
            trace_id: self.trace_id.clone(),
            span_id: uuid::Uuid::new_v4().to_string(),
            parent_span_id: Some(self.span_id.clone()),
            sampled: self.sampled,
            baggage: self.baggage.clone(),
        }
    }

    /// Add baggage item (carried through entire trace)
    pub fn with_baggage(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.baggage.insert(key.into(), value.into());
        self
    }

    /// Export as W3C Trace Context headers
    pub fn to_w3c_headers(&self) -> Vec<(String, String)> {
        let trace_flags = if self.sampled { "01" } else { "00" };
        let trace_context = format!("00-{}-{}-{}", self.trace_id, self.span_id, trace_flags);

        let mut headers = vec![("traceparent".to_string(), trace_context)];

        // Add baggage as custom header
        if !self.baggage.is_empty() {
            let baggage_str = self
                .baggage
                .iter()
                .map(|(k, v)| format!("{}={}", k, v))
                .collect::<Vec<_>>()
                .join(",");
            headers.push(("baggage".to_string(), baggage_str));
        }

        headers
    }
}

/// Distributed tracing system (OpenTelemetry equivalent)
pub struct AndonTracer {
    config: TracerConfig,
    context: Arc<parking_lot::RwLock<SpanContext>>,
    active_spans: Arc<dashmap::DashMap<String, SpanRecord>>,
}

/// Record of an active span
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanRecord {
    /// Span context
    pub context: SpanContext,

    /// Span name
    pub name: String,

    /// Start time (ISO 8601)
    pub start_time: String,

    /// End time (ISO 8601, if completed)
    pub end_time: Option<String>,

    /// Span status
    pub status: SpanStatus,

    /// Attributes (key-value context)
    pub attributes: serde_json::Map<String, serde_json::Value>,

    /// Events (logged messages within span)
    pub events: Vec<SpanEvent>,

    /// Duration in seconds (if completed)
    pub duration_secs: Option<f64>,
}

/// Span execution status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "UPPERCASE")]
pub enum SpanStatus {
    /// Span is currently running
    #[serde(rename = "UNSET")]
    Unset,

    /// Span completed successfully
    #[serde(rename = "OK")]
    Ok,

    /// Span encountered an error
    #[serde(rename = "ERROR")]
    Error,
}

/// Event logged within a span
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanEvent {
    /// Event name
    pub name: String,

    /// Event timestamp (ISO 8601)
    pub timestamp: String,

    /// Event attributes
    pub attributes: serde_json::Map<String, serde_json::Value>,
}

impl AndonTracer {
    /// Create a new distributed tracer
    pub fn new(config: TracerConfig) -> Result<Self> {
        // In production, would initialize OpenTelemetry provider
        // For now, create local tracer

        let tracer = Self {
            config,
            context: Arc::new(parking_lot::RwLock::new(SpanContext::new_root())),
            active_spans: Arc::new(dashmap::DashMap::new()),
        };

        Ok(tracer)
    }

    /// Get current trace ID
    pub fn trace_id(&self) -> String {
        self.context.read().trace_id.clone()
    }

    /// Start a new span
    pub fn start_span(&self, name: impl Into<String>) -> Result<String> {
        let ctx = self.context.read().clone();
        let child_ctx = ctx.child();
        let span_id = child_ctx.span_id.clone();

        let record = SpanRecord {
            context: child_ctx,
            name: name.into(),
            start_time: chrono::Utc::now().to_rfc3339(),
            end_time: None,
            status: SpanStatus::Unset,
            attributes: serde_json::Map::new(),
            events: Vec::new(),
            duration_secs: None,
        };

        self.active_spans.insert(span_id.clone(), record);

        Ok(span_id)
    }

    /// End a span
    pub fn end_span(&self, span_id: &str, status: SpanStatus) -> Result<()> {
        if let Some((_, mut record)) = self.active_spans.remove(span_id) {
            let end_time = chrono::Utc::now();
            record.end_time = Some(end_time.to_rfc3339());
            record.status = status;

            // Calculate duration
            if let Ok(start) = chrono::DateTime::parse_from_rfc3339(&record.start_time) {
                let duration = end_time
                    .signed_duration_since(start.with_timezone(&chrono::Utc))
                    .num_milliseconds() as f64
                    / 1000.0;
                record.duration_secs = Some(duration);
            }

            // In production, would export span to Jaeger/Cloud Trace
            tracing::debug!(
                "Span ended: {} (id: {}, duration: {:?}s)",
                record.name,
                span_id,
                record.duration_secs
            );
        }

        Ok(())
    }

    /// Add an attribute to a span
    pub fn add_span_attribute(
        &self, span_id: &str, key: impl Into<String>, value: serde_json::Value,
    ) -> Result<()> {
        if let Some(mut record) = self.active_spans.get_mut(span_id) {
            record.attributes.insert(key.into(), value);
        }
        Ok(())
    }

    /// Log an event within a span
    pub fn span_event(
        &self, span_id: &str, name: impl Into<String>,
        attributes: Option<serde_json::Map<String, serde_json::Value>>,
    ) -> Result<()> {
        if let Some(mut record) = self.active_spans.get_mut(span_id) {
            record.events.push(SpanEvent {
                name: name.into(),
                timestamp: chrono::Utc::now().to_rfc3339(),
                attributes: attributes.unwrap_or_default(),
            });
        }
        Ok(())
    }

    /// Get span context
    pub fn get_span_context(&self) -> SpanContext {
        self.context.read().clone()
    }

    /// Set current span context
    pub fn set_span_context(&self, context: SpanContext) {
        *self.context.write() = context;
    }

    /// Get W3C Trace Context headers
    pub fn w3c_headers(&self) -> Vec<(String, String)> {
        self.context.read().to_w3c_headers()
    }

    /// Export span as JSON
    pub fn export_span(&self, span_id: &str) -> Result<Option<String>> {
        if let Some(record) = self.active_spans.get(span_id) {
            let json = serde_json::to_string(&*record).map_err(|e| {
                crate::error::AndonError::tracer(format!("Serialization failed: {}", e))
            })?;
            return Ok(Some(json));
        }
        Ok(None)
    }

    /// Get all active spans
    pub fn get_active_spans(&self) -> Vec<SpanRecord> {
        self.active_spans
            .iter()
            .map(|r| r.value().clone())
            .collect()
    }

    /// Shutdown tracer (flush pending spans)
    pub async fn shutdown(&self) -> Result<()> {
        // In production, would flush all spans to backend
        let pending = self.active_spans.len();
        if pending > 0 {
            tracing::warn!("Shutting down with {} pending spans", pending);
        }
        Ok(())
    }
}

impl std::fmt::Debug for AndonTracer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AndonTracer")
            .field("config", &self.config)
            .field("active_spans", &self.active_spans.len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_context_creation() {
        let ctx = SpanContext::new_root();
        assert!(!ctx.trace_id.is_empty());
        assert!(!ctx.span_id.is_empty());
        assert!(ctx.parent_span_id.is_none());
        assert!(ctx.sampled);
    }

    #[test]
    fn test_span_context_child() {
        let parent = SpanContext::new_root();
        let child = parent.child();

        assert_eq!(parent.trace_id, child.trace_id); // Same trace
        assert_ne!(parent.span_id, child.span_id); // Different span
        assert_eq!(child.parent_span_id, Some(parent.span_id.clone()));
    }

    #[test]
    fn test_span_context_baggage() {
        let ctx = SpanContext::new_root()
            .with_baggage("user_id", "12345")
            .with_baggage("request_path", "/api/test");

        assert_eq!(ctx.baggage.get("user_id"), Some(&"12345".to_string()));
        assert_eq!(
            ctx.baggage.get("request_path"),
            Some(&"/api/test".to_string())
        );
    }

    #[test]
    fn test_w3c_trace_context_headers() {
        let ctx = SpanContext::new_root().with_baggage("user_id", "123");

        let headers = ctx.to_w3c_headers();
        assert!(headers.len() >= 1);
        assert!(headers[0].0 == "traceparent");
        assert!(headers[0].1.contains("00-"));
    }

    #[tokio::test]
    async fn test_tracer_creation() {
        let config = TracerConfig::default();
        let tracer = AndonTracer::new(config);
        assert!(tracer.is_ok());
    }

    #[tokio::test]
    async fn test_start_and_end_span() {
        let config = TracerConfig::default();
        let tracer = AndonTracer::new(config).unwrap();

        let span_id = tracer.start_span("test-operation").unwrap();
        assert!(!span_id.is_empty());

        tracer.end_span(&span_id, SpanStatus::Ok).unwrap();
        let exported = tracer.export_span(&span_id).unwrap();
        assert!(exported.is_none()); // Span was removed after ending
    }

    #[tokio::test]
    async fn test_span_attributes() {
        let config = TracerConfig::default();
        let tracer = AndonTracer::new(config).unwrap();

        let span_id = tracer.start_span("test").unwrap();
        tracer
            .add_span_attribute(&span_id, "user_id", serde_json::json!("123"))
            .unwrap();

        let active = tracer.get_active_spans();
        assert!(active.len() > 0);
    }
}
