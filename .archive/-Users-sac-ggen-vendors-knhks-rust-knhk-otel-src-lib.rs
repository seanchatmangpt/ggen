// rust/knhk-otel/src/lib.rs
// OpenTelemetry Observability Integration
// Provides metrics, traces, and spans for KNHKS operations

#![no_std]
extern crate alloc;

use alloc::vec::Vec;
use alloc::string::String;
use alloc::collections::BTreeMap;

#[cfg(feature = "std")]
use std::time::{SystemTime, UNIX_EPOCH};

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

/// OTLP exporter for sending spans/metrics to collectors
#[cfg(feature = "std")]
pub struct OtlpExporter {
    endpoint: String,
}

/// Weaver live-check integration for telemetry validation
#[cfg(feature = "std")]
pub struct WeaverLiveCheck {
    registry_path: Option<String>,
    otlp_grpc_address: String,
    otlp_grpc_port: u16,
    admin_port: u16,
    inactivity_timeout: u64,
    format: String,
    output: Option<String>,
}

#[cfg(feature = "std")]
impl WeaverLiveCheck {
    /// Create a new Weaver live-check instance
    pub fn new() -> Self {
        Self {
            registry_path: None,
            otlp_grpc_address: "127.0.0.1".to_string(),
            otlp_grpc_port: 4317,
            admin_port: 8080,
            inactivity_timeout: 60,
            format: "json".to_string(),
            output: None,
        }
    }

    /// Set the semantic convention registry path
    pub fn with_registry(mut self, registry_path: String) -> Self {
        self.registry_path = Some(registry_path);
        self
    }

    /// Set the OTLP gRPC address
    pub fn with_otlp_address(mut self, address: String) -> Self {
        self.otlp_grpc_address = address;
        self
    }

    /// Set the OTLP gRPC port
    pub fn with_otlp_port(mut self, port: u16) -> Self {
        self.otlp_grpc_port = port;
        self
    }

    /// Set the admin HTTP port
    pub fn with_admin_port(mut self, port: u16) -> Self {
        self.admin_port = port;
        self
    }

    /// Set the inactivity timeout in seconds
    pub fn with_inactivity_timeout(mut self, timeout: u64) -> Self {
        self.inactivity_timeout = timeout;
        self
    }

    /// Set the output format (json, ansi)
    pub fn with_format(mut self, format: String) -> Self {
        self.format = format;
        self
    }

    /// Set the output directory (for JSON reports)
    pub fn with_output(mut self, output: String) -> Self {
        self.output = Some(output);
        self
    }

    /// Run live-check and return the process handle
    /// The caller should send telemetry to the configured OTLP endpoint
    pub fn start(&self) -> Result<std::process::Child, String> {
        use std::process::Command;
        
        let mut cmd = Command::new("weaver");
        
        cmd.args(&["registry", "live-check"]);
        
        if let Some(ref registry) = self.registry_path {
            cmd.args(&["--registry", registry]);
        }
        
        cmd.args(&["--otlp-grpc-address", &self.otlp_grpc_address]);
        cmd.args(&["--otlp-grpc-port", &self.otlp_grpc_port.to_string()]);
        cmd.args(&["--admin-port", &self.admin_port.to_string()]);
        cmd.args(&["--inactivity-timeout", &self.inactivity_timeout.to_string()]);
        cmd.args(&["--format", &self.format]);
        
        if let Some(ref output) = self.output {
            cmd.args(&["--output", output]);
        }
        
        cmd.spawn()
            .map_err(|e| format!("Failed to start Weaver live-check: {}", e))
    }

    /// Stop the live-check process via HTTP admin endpoint
    pub fn stop(&self) -> Result<(), String> {
        use reqwest::blocking::Client;
        use std::time::Duration;
        
        let client = Client::builder()
            .timeout(Duration::from_secs(5))
            .build()
            .map_err(|e| format!("Failed to create HTTP client: {}", e))?;
        
        let url = format!("http://{}:{}/stop", self.otlp_grpc_address, self.admin_port);
        
        match client.post(&url).send() {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("Failed to stop Weaver live-check: {}", e)),
        }
    }

    /// Get the OTLP gRPC endpoint for sending telemetry
    /// Note: Weaver live-check listens on gRPC, but exporters typically use HTTP
    /// This returns the address:port format for configuration
    pub fn otlp_endpoint(&self) -> String {
        format!("{}:{}", self.otlp_grpc_address, self.otlp_grpc_port)
    }
}

#[cfg(feature = "std")]
impl Default for WeaverLiveCheck {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "std")]
impl OtlpExporter {
    pub fn new(endpoint: String) -> Self {
        Self { endpoint }
    }
    
    pub fn export_spans(&self, spans: &[Span]) -> Result<(), String> {
        if spans.is_empty() {
            return Ok(());
        }

        #[cfg(all(feature = "std", feature = "reqwest"))]
        {
            use reqwest::blocking::Client;
            use std::time::Duration;
            
            // Build OTLP JSON payload
            let payload = self.build_otlp_spans_payload(spans);
            
            // Create HTTP client
            let client = Client::builder()
                .timeout(Duration::from_secs(30))
                .build()
                .map_err(|e| format!("Failed to create HTTP client: {}", e))?;
            
            // Send spans to OTLP endpoint (traces endpoint)
            let traces_endpoint = format!("{}/v1/traces", self.endpoint.trim_end_matches('/'));
            
            match client
                .post(&traces_endpoint)
                .json(&payload)
                .header("Content-Type", "application/json")
                .send()
            {
                Ok(response) => {
                    if response.status().is_success() {
                        Ok(())
                    } else {
                        Err(format!("OTLP export failed: HTTP {}", response.status()))
                    }
                }
                Err(e) => Err(format!("OTLP export failed: {}", e)),
            }
        }
        
        #[cfg(not(all(feature = "std", feature = "reqwest")))]
        {
            // Fallback: log spans (for no_std or when reqwest not available)
            eprintln!("OTLP Export to {}: {} spans (HTTP client not available)", self.endpoint, spans.len());
            Ok(())
        }
    }
    
    #[cfg(all(feature = "std", feature = "serde_json"))]
    fn build_otlp_spans_payload(&self, spans: &[Span]) -> serde_json::Value {
        use serde_json::json;
        
        let spans_json: Vec<_> = spans.iter().map(|span| {
            let mut span_json = json!({
                "traceId": format!("{:032x}", span.context.trace_id.0),
                "spanId": format!("{:016x}", span.context.span_id.0),
                "name": span.name,
                "startTimeUnixNano": span.start_time_ms * 1_000_000,
                "endTimeUnixNano": span.end_time_ms.unwrap_or(span.start_time_ms) * 1_000_000,
                "status": {
                    "code": match span.status {
                        SpanStatus::Ok => 1,
                        SpanStatus::Error => 2,
                        SpanStatus::Unset => 0,
                    }
                }
            });
            
            // Add parent span ID if present
            if let Some(parent_id) = span.context.parent_span_id {
                span_json["parentSpanId"] = json!(format!("{:016x}", parent_id.0));
            }
            
            // Add attributes
            if !span.attributes.is_empty() {
                span_json["attributes"] = json!(span.attributes.iter().map(|(k, v)| {
                    json!({
                        "key": k,
                        "value": {"stringValue": v}
                    })
                }).collect::<Vec<_>>());
            }
            
            span_json
        }).collect();
        
        json!({
            "resourceSpans": [{
                "resource": {},
                "instrumentationLibrarySpans": [{
                    "instrumentationLibrary": {},
                    "spans": spans_json
                }]
            }]
        })
    }
    
    #[cfg(not(all(feature = "std", feature = "serde_json")))]
    fn build_otlp_spans_payload(&self, _spans: &[Span]) -> String {
        // Fallback: return empty JSON
        "{}".to_string()
    }
    
    pub fn export_metrics(&self, metrics: &[Metric]) -> Result<(), String> {
        if metrics.is_empty() {
            return Ok(());
        }

        #[cfg(all(feature = "std", feature = "reqwest"))]
        {
            use reqwest::blocking::Client;
            use std::time::Duration;
            
            // Build OTLP JSON payload
            let payload = self.build_otlp_metrics_payload(metrics);
            
            // Create HTTP client
            let client = Client::builder()
                .timeout(Duration::from_secs(30))
                .build()
                .map_err(|e| format!("Failed to create HTTP client: {}", e))?;
            
            // Send metrics to OTLP endpoint (metrics endpoint)
            let metrics_endpoint = format!("{}/v1/metrics", self.endpoint.trim_end_matches('/'));
            
            match client
                .post(&metrics_endpoint)
                .json(&payload)
                .header("Content-Type", "application/json")
                .send()
            {
                Ok(response) => {
                    if response.status().is_success() {
                        Ok(())
                    } else {
                        Err(format!("OTLP export failed: HTTP {}", response.status()))
                    }
                }
                Err(e) => Err(format!("OTLP export failed: {}", e)),
            }
        }
        
        #[cfg(not(all(feature = "std", feature = "reqwest")))]
        {
            // Fallback: log metrics (for no_std or when reqwest not available)
            eprintln!("OTLP Export to {}: {} metrics (HTTP client not available)", self.endpoint, metrics.len());
            Ok(())
        }
    }
    
    #[cfg(all(feature = "std", feature = "serde_json"))]
    fn build_otlp_metrics_payload(&self, metrics: &[Metric]) -> serde_json::Value {
        use serde_json::json;
        
        let metrics_json: Vec<_> = metrics.iter().map(|metric| {
            let value_json = match &metric.value {
                MetricValue::Counter(c) => json!({
                    "asInt": format!("{}", c)
                }),
                MetricValue::Gauge(g) => json!({
                    "asDouble": *g
                }),
                MetricValue::Histogram(h) => json!({
                    "asInt": format!("{}", h.len())
                }),
            };
            
            let mut metric_json = json!({
                "name": metric.name,
                "timestamp": metric.timestamp_ms * 1_000_000,
                "value": value_json
            });
            
            // Add attributes
            if !metric.attributes.is_empty() {
                metric_json["attributes"] = json!(metric.attributes.iter().map(|(k, v)| {
                    json!({
                        "key": k,
                        "value": {"stringValue": v}
                    })
                }).collect::<Vec<_>>());
            }
            
            metric_json
        }).collect();
        
        json!({
            "resourceMetrics": [{
                "resource": {},
                "instrumentationLibraryMetrics": [{
                    "instrumentationLibrary": {},
                    "metrics": metrics_json
                }]
            }]
        })
    }
    
    #[cfg(not(all(feature = "std", feature = "serde_json")))]
    fn build_otlp_metrics_payload(&self, _metrics: &[Metric]) -> String {
        // Fallback: return empty JSON
        "{}".to_string()
    }
}

/// OTEL tracer
pub struct Tracer {
    spans: Vec<Span>,
    metrics: Vec<Metric>,
    #[cfg(feature = "std")]
    exporter: Option<OtlpExporter>,
}

impl Tracer {
    pub fn new() -> Self {
        Self {
            spans: Vec::new(),
            metrics: Vec::new(),
            #[cfg(feature = "std")]
            exporter: None,
        }
    }
    
    #[cfg(feature = "std")]
    pub fn with_otlp_exporter(endpoint: String) -> Self {
        Self {
            spans: Vec::new(),
            metrics: Vec::new(),
            exporter: Some(OtlpExporter::new(endpoint)),
        }
    }
    
    #[cfg(feature = "std")]
    pub fn export(&mut self) -> Result<(), String> {
        if let Some(ref mut exporter) = self.exporter {
            exporter.export_spans(&self.spans)?;
            exporter.export_metrics(&self.metrics)?;
        }
        Ok(())
    }

    /// Start a new span
    pub fn start_span(&mut self, name: String, parent: Option<SpanContext>) -> SpanContext {
        let trace_id = parent.map(|p| p.trace_id).unwrap_or_else(|| {
            // Generate new trace ID (128-bit random)
            TraceId(generate_trace_id())
        });

        let span_id = SpanId(generate_span_id());
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
            start_time_ms: get_timestamp_ms(),
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
            span.end_time_ms = Some(get_timestamp_ms());
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

    /// Export telemetry to Weaver live-check endpoint for validation
    #[cfg(feature = "std")]
    pub fn export_to_weaver(&mut self, weaver_endpoint: &str) -> Result<(), String> {
        // Create a temporary exporter pointing to Weaver's OTLP endpoint
        let weaver_exporter = OtlpExporter::new(weaver_endpoint.to_string());
        weaver_exporter.export_spans(&self.spans)?;
        weaver_exporter.export_metrics(&self.metrics)?;
        Ok(())
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
            name: "knhk.hook.latency.ticks".to_string(),
            value: MetricValue::Histogram(vec![ticks as u64]),
            timestamp_ms: get_timestamp_ms(),
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
            name: "knhk.receipt.generated".to_string(),
            value: MetricValue::Counter(1),
            timestamp_ms: get_timestamp_ms(),
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
            name: "knhk.guard.violation".to_string(),
            value: MetricValue::Counter(1),
            timestamp_ms: get_timestamp_ms(),
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
            name: "knhk.connector.throughput".to_string(),
            value: MetricValue::Counter(triples as u64),
            timestamp_ms: get_timestamp_ms(),
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

// Helper functions for generating IDs and timestamps

/// Generate 128-bit trace ID
fn generate_trace_id() -> u128 {
    #[cfg(feature = "std")]
    {
        use rand::RngCore;
        let mut rng = rand::thread_rng();
        rng.next_u128()
    }
    #[cfg(not(feature = "std"))]
    {
        // For no_std, use simple hash-based generation
        // In production, use hardware RNG or external source
        use core::hash::{Hash, Hasher};
        use hashbrown::hash_map::DefaultHasher;
        let mut hasher = DefaultHasher::new();
        "trace".hash(&mut hasher);
        hasher.finish() as u128 | ((hasher.finish() as u128) << 64)
    }
}

/// Generate 64-bit span ID (public API)
pub fn generate_span_id() -> u64 {
    #[cfg(feature = "std")]
    {
        use rand::RngCore;
        let mut rng = rand::thread_rng();
        rng.next_u64()
    }
    #[cfg(not(feature = "std"))]
    {
        // For no_std, use hash-based generation with timestamp
        use core::hash::{Hash, Hasher};
        use hashbrown::hash_map::DefaultHasher;
        let mut hasher = DefaultHasher::new();
        "span".hash(&mut hasher);
        let timestamp = get_timestamp_ms();
        timestamp.hash(&mut hasher);
        hasher.finish()
    }
}

/// Get current timestamp in milliseconds
fn get_timestamp_ms() -> u64 {
    #[cfg(feature = "std")]
    {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0)
    }
    #[cfg(not(feature = "std"))]
    {
        // For no_std, return 0 as placeholder
        // In production, use a no_std-compatible time source
        0
    }
}

