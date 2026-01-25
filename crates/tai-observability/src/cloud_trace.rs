//! Cloud Trace integration for distributed tracing and request flow visualization.
//!
//! This module provides distributed tracing with:
//! - Root span per request (request ID tracking)
//! - Span hierarchy (parent/child relationships)
//! - Automatic latency measurement
//! - Configurable sampling (100% dev, 1% prod)
//! - Batch export to Cloud Trace

use crate::error::{ObservabilityError, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};
use uuid::Uuid;

/// Trace span for tracking operation execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceSpan {
    /// Unique span ID
    pub span_id: String,
    /// Parent span ID (if any)
    pub parent_span_id: Option<String>,
    /// Trace ID (shared across all spans in a request)
    pub trace_id: String,
    /// Span name/operation
    pub name: String,
    /// Start time
    pub start_time: DateTime<Utc>,
    /// End time
    pub end_time: Option<DateTime<Utc>>,
    /// Duration in microseconds
    pub duration_micros: Option<u64>,
    /// Span status
    pub status: SpanStatus,
    /// Key-value attributes (context, tags, metadata)
    pub attributes: BTreeMap<String, String>,
    /// Span events (e.g., annotations)
    pub events: Vec<SpanEvent>,
    /// Child span IDs
    pub child_span_ids: Vec<String>,
}

/// Span event annotation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanEvent {
    /// Event name
    pub name: String,
    /// Event timestamp
    pub timestamp: DateTime<Utc>,
    /// Event attributes
    pub attributes: BTreeMap<String, String>,
}

/// Span status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum SpanStatus {
    /// Operation in progress
    Unset,
    /// Operation succeeded
    Ok,
    /// Operation failed
    Error,
}

impl TraceSpan {
    /// Create a new trace span
    pub fn new(trace_id: String, name: String) -> Self {
        let span_id = Uuid::new_v4().to_string();

        Self {
            span_id,
            parent_span_id: None,
            trace_id,
            name,
            start_time: Utc::now(),
            end_time: None,
            duration_micros: None,
            status: SpanStatus::Unset,
            attributes: BTreeMap::new(),
            events: Vec::new(),
            child_span_ids: Vec::new(),
        }
    }

    /// Create a child span
    pub fn create_child(&self, name: String) -> Self {
        let mut child = Self::new(self.trace_id.clone(), name);
        child.parent_span_id = Some(self.span_id.clone());
        child
    }

    /// Set an attribute
    pub fn set_attribute(&mut self, key: String, value: String) {
        self.attributes.insert(key, value);
    }

    /// Add an event
    pub fn add_event(&mut self, name: String, attributes: BTreeMap<String, String>) {
        self.events.push(SpanEvent {
            name,
            timestamp: Utc::now(),
            attributes,
        });
    }

    /// Finish the span
    pub fn finish(&mut self, status: SpanStatus) {
        self.end_time = Some(Utc::now());
        self.status = status;

        if let (Some(start), Some(end)) = (
            self.start_time.timestamp_micros(),
            self.end_time.as_ref().and_then(|t| t.timestamp_micros().ok()),
        ) {
            self.duration_micros = Some((end - start) as u64);
        }
    }

    /// Get the span duration
    pub fn get_duration(&self) -> Option<Duration> {
        self.duration_micros.map(|micros| {
            Duration::from_micros(micros)
        })
    }

    /// Check if span is finished
    pub fn is_finished(&self) -> bool {
        self.end_time.is_some()
    }
}

/// Complete trace with all spans
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trace {
    /// Trace ID
    pub trace_id: String,
    /// Project ID
    pub project_id: String,
    /// Root span ID
    pub root_span_id: String,
    /// All spans in trace
    pub spans: Vec<TraceSpan>,
    /// Trace start time
    pub start_time: DateTime<Utc>,
    /// Trace end time
    pub end_time: Option<DateTime<Utc>>,
    /// Total duration
    pub duration_micros: Option<u64>,
}

impl Trace {
    /// Create a new trace
    pub fn new(project_id: String, root_span_name: String) -> Self {
        let trace_id = Uuid::new_v4().to_string();
        let root_span = TraceSpan::new(trace_id.clone(), root_span_name);
        let root_span_id = root_span.span_id.clone();

        Self {
            trace_id,
            project_id,
            root_span_id,
            spans: vec![root_span],
            start_time: Utc::now(),
            end_time: None,
            duration_micros: None,
        }
    }

    /// Get root span
    pub fn root_span(&self) -> Option<&TraceSpan> {
        self.spans.iter().find(|s| s.span_id == self.root_span_id)
    }

    /// Get root span (mutable)
    pub fn root_span_mut(&mut self) -> Option<&mut TraceSpan> {
        self.spans.iter_mut().find(|s| s.span_id == self.root_span_id)
    }

    /// Finish the trace
    pub fn finish(&mut self) {
        self.end_time = Some(Utc::now());

        if let Some(root) = self.root_span_mut() {
            root.finish(SpanStatus::Ok);
        }

        if let (Some(start), Some(end)) = (
            self.start_time.timestamp_micros(),
            self.end_time.as_ref().and_then(|t| t.timestamp_micros().ok()),
        ) {
            self.duration_micros = Some((end - start) as u64);
        }
    }

    /// Find span by ID
    pub fn find_span(&self, span_id: &str) -> Option<&TraceSpan> {
        self.spans.iter().find(|s| s.span_id == span_id)
    }

    /// Find span by ID (mutable)
    pub fn find_span_mut(&mut self, span_id: &str) -> Option<&mut TraceSpan> {
        self.spans.iter_mut().find(|s| s.span_id == span_id)
    }
}

/// Sampling decision
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SamplingDecision {
    /// Trace is sampled
    Sample,
    /// Trace is not sampled
    DontSample,
}

/// Trace sampler
pub struct TraceSampler {
    /// Sample rate (0.0 to 1.0)
    sample_rate: f64,
}

impl TraceSampler {
    /// Create a new trace sampler
    pub fn new(sample_rate: f64) -> Self {
        let rate = sample_rate.clamp(0.0, 1.0);
        if rate != sample_rate {
            warn!(
                "Sample rate clamped from {} to {}",
                sample_rate, rate
            );
        }

        Self {
            sample_rate: rate,
        }
    }

    /// Make a sampling decision
    pub fn decide(&self) -> SamplingDecision {
        let rand_val = fastrand::f64();

        if rand_val < self.sample_rate {
            SamplingDecision::Sample
        } else {
            SamplingDecision::DontSample
        }
    }
}

/// Cloud Trace client
pub struct CloudTrace {
    /// Project ID
    project_id: String,
    /// Active traces
    active_traces: Arc<RwLock<BTreeMap<String, Trace>>>,
    /// Completed traces
    completed_traces: Arc<RwLock<Vec<Trace>>>,
    /// Trace sampler
    sampler: TraceSampler,
    /// Batch size for export
    batch_size: usize,
    /// Maximum traces in memory
    max_traces: usize,
}

impl CloudTrace {
    /// Create a new Cloud Trace client
    pub fn new(project_id: String, sample_rate: f64) -> Self {
        info!(
            "Initializing Cloud Trace for project {} with {:.1}% sampling",
            project_id,
            sample_rate * 100.0
        );

        Self {
            project_id,
            active_traces: Arc::new(RwLock::new(BTreeMap::new())),
            completed_traces: Arc::new(RwLock::new(Vec::new())),
            sampler: TraceSampler::new(sample_rate),
            batch_size: 100,
            max_traces: 10_000,
        }
    }

    /// Start a new trace
    pub async fn start_trace(&self, root_span_name: String) -> Result<String> {
        let decision = self.sampler.decide();

        if decision != SamplingDecision::Sample {
            debug!("Trace not sampled");
            return Ok(String::new());
        }

        let trace = Trace::new(self.project_id.clone(), root_span_name);
        let trace_id = trace.trace_id.clone();

        let mut active = self.active_traces.write().await;

        if active.len() >= self.max_traces {
            warn!("Active traces limit reached, discarding oldest");
            if let Some(first_key) = active.keys().next().cloned() {
                active.remove(&first_key);
            }
        }

        active.insert(trace_id.clone(), trace);

        debug!("Started trace: {}", trace_id);

        Ok(trace_id)
    }

    /// Create a span in a trace
    pub async fn create_span(
        &self,
        trace_id: &str,
        span_name: String,
        parent_span_id: Option<String>,
    ) -> Result<String> {
        let mut traces = self.active_traces.write().await;

        match traces.get_mut(trace_id) {
            Some(trace) => {
                let span = if let Some(parent_id) = parent_span_id {
                    if let Some(parent) = trace.find_span(&parent_id) {
                        let mut child = parent.create_child(span_name);
                        let span_id = child.span_id.clone();

                        if let Some(parent) = trace.find_span_mut(&parent_id) {
                            parent.child_span_ids.push(span_id.clone());
                        }

                        child
                    } else {
                        return Err(ObservabilityError::SpanCreationError(
                            format!("Parent span {} not found", parent_id),
                        ));
                    }
                } else {
                    TraceSpan::new(trace_id.to_string(), span_name)
                };

                let span_id = span.span_id.clone();
                trace.spans.push(span);

                debug!("Created span: {} in trace: {}", span_id, trace_id);

                Ok(span_id)
            }
            None => Err(ObservabilityError::TraceError(
                format!("Trace {} not found", trace_id),
            )),
        }
    }

    /// Set span attribute
    pub async fn set_span_attribute(
        &self,
        trace_id: &str,
        span_id: &str,
        key: String,
        value: String,
    ) -> Result<()> {
        let mut traces = self.active_traces.write().await;

        match traces.get_mut(trace_id) {
            Some(trace) => match trace.find_span_mut(span_id) {
                Some(span) => {
                    span.set_attribute(key, value);
                    Ok(())
                }
                None => Err(ObservabilityError::SpanCreationError(
                    format!("Span {} not found", span_id),
                )),
            },
            None => Err(ObservabilityError::TraceError(
                format!("Trace {} not found", trace_id),
            )),
        }
    }

    /// Finish a span
    pub async fn finish_span(
        &self,
        trace_id: &str,
        span_id: &str,
        status: SpanStatus,
    ) -> Result<()> {
        let mut traces = self.active_traces.write().await;

        match traces.get_mut(trace_id) {
            Some(trace) => match trace.find_span_mut(span_id) {
                Some(span) => {
                    span.finish(status);
                    debug!(
                        "Finished span: {} in trace: {} with status: {:?}",
                        span_id, trace_id, status
                    );
                    Ok(())
                }
                None => Err(ObservabilityError::SpanCreationError(
                    format!("Span {} not found", span_id),
                )),
            },
            None => Err(ObservabilityError::TraceError(
                format!("Trace {} not found", trace_id),
            )),
        }
    }

    /// Finish a trace
    pub async fn finish_trace(&self, trace_id: &str) -> Result<()> {
        let mut active = self.active_traces.write().await;

        match active.remove(trace_id) {
            Some(mut trace) => {
                trace.finish();

                let mut completed = self.completed_traces.write().await;
                completed.push(trace);

                debug!("Finished trace: {}", trace_id);

                Ok(())
            }
            None => Err(ObservabilityError::TraceError(
                format!("Trace {} not found or already finished", trace_id),
            )),
        }
    }

    /// Get completed traces
    pub async fn get_completed_traces(&self, limit: usize) -> Result<Vec<Trace>> {
        let completed = self.completed_traces.read().await;
        Ok(completed.iter().rev().take(limit).cloned().collect())
    }

    /// Export completed traces in batch
    pub async fn batch_export(&self) -> Result<(usize, Vec<Trace>)> {
        let mut completed = self.completed_traces.write().await;

        if completed.is_empty() {
            debug!("No traces to export");
            return Ok((0, vec![]));
        }

        let batch: Vec<Trace> = completed
            .drain(0..std::cmp::min(self.batch_size, completed.len()))
            .collect();

        let count = batch.len();

        info!("Exporting batch of {} traces to Cloud Trace", count);

        Ok((count, batch))
    }

    /// Upload traces to Cloud Trace
    pub async fn upload_traces(&self, traces: &[Trace]) -> Result<String> {
        info!(
            "Uploading {} traces to Cloud Trace for project {}",
            traces.len(),
            self.project_id
        );

        // In production, this would call the actual Google Cloud Trace API
        // For now, we simulate a successful upload

        let url = format!(
            "https://console.cloud.google.com/traces?project={}",
            self.project_id
        );

        debug!("Traces uploaded to: {}", url);

        Ok(url)
    }

    /// Get trace latency analysis
    pub async fn analyze_latencies(&self, limit: usize) -> Result<TraceLattencyAnalysis> {
        let completed = self.completed_traces.read().await;

        let traces: Vec<_> = completed.iter().rev().take(limit).collect();

        if traces.is_empty() {
            return Ok(TraceLattencyAnalysis {
                total_traces: 0,
                average_latency_ms: 0.0,
                p50_latency_ms: 0.0,
                p95_latency_ms: 0.0,
                p99_latency_ms: 0.0,
                slowest_spans: Vec::new(),
            });
        }

        let mut latencies: Vec<u64> = traces
            .iter()
            .filter_map(|t| t.duration_micros)
            .collect();

        latencies.sort();

        let average = if !latencies.is_empty() {
            latencies.iter().sum::<u64>() as f64 / latencies.len() as f64 / 1000.0
        } else {
            0.0
        };

        let p50 = latencies
            .get(latencies.len() / 2)
            .map(|&v| v as f64 / 1000.0)
            .unwrap_or(0.0);

        let p95 = latencies
            .get((latencies.len() * 95) / 100)
            .map(|&v| v as f64 / 1000.0)
            .unwrap_or(0.0);

        let p99 = latencies
            .get((latencies.len() * 99) / 100)
            .map(|&v| v as f64 / 1000.0)
            .unwrap_or(0.0);

        let mut slowest_spans: Vec<_> = traces
            .iter()
            .flat_map(|t| t.spans.iter().map(|s| (t.trace_id.clone(), s.clone())))
            .collect();

        slowest_spans.sort_by_key(|s| std::cmp::Reverse(s.1.duration_micros));

        let slowest = slowest_spans
            .into_iter()
            .take(10)
            .map(|(trace_id, span)| SlowSpan {
                trace_id,
                span_name: span.name,
                latency_ms: span.duration_micros.map(|m| m as f64 / 1000.0).unwrap_or(0.0),
            })
            .collect();

        Ok(TraceLattencyAnalysis {
            total_traces: traces.len(),
            average_latency_ms: average,
            p50_latency_ms: p50,
            p95_latency_ms: p95,
            p99_latency_ms: p99,
            slowest_spans: slowest,
        })
    }
}

/// Trace latency analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceLattencyAnalysis {
    /// Total traces analyzed
    pub total_traces: usize,
    /// Average latency in milliseconds
    pub average_latency_ms: f64,
    /// 50th percentile latency
    pub p50_latency_ms: f64,
    /// 95th percentile latency
    pub p95_latency_ms: f64,
    /// 99th percentile latency
    pub p99_latency_ms: f64,
    /// Slowest spans
    pub slowest_spans: Vec<SlowSpan>,
}

/// Slow span information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SlowSpan {
    /// Trace ID
    pub trace_id: String,
    /// Span name
    pub span_name: String,
    /// Latency in milliseconds
    pub latency_ms: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_creation() {
        let span = TraceSpan::new("trace-1".to_string(), "http-request".to_string());
        assert_eq!(span.trace_id, "trace-1");
        assert_eq!(span.name, "http-request");
        assert_eq!(span.status, SpanStatus::Unset);
    }

    #[test]
    fn test_span_finishing() {
        let mut span = TraceSpan::new("trace-1".to_string(), "operation".to_string());
        span.finish(SpanStatus::Ok);

        assert_eq!(span.status, SpanStatus::Ok);
        assert!(span.is_finished());
    }

    #[test]
    fn test_span_attributes() {
        let mut span = TraceSpan::new("trace-1".to_string(), "operation".to_string());
        span.set_attribute("user_id".to_string(), "123".to_string());

        assert_eq!(span.attributes.get("user_id"), Some(&"123".to_string()));
    }

    #[test]
    fn test_trace_creation() {
        let trace = Trace::new("project-1".to_string(), "http-request".to_string());
        assert_eq!(trace.project_id, "project-1");
        assert_eq!(trace.spans.len(), 1);
    }

    #[test]
    fn test_span_parent_child() {
        let span = TraceSpan::new("trace-1".to_string(), "parent".to_string());
        let child = span.create_child("child".to_string());

        assert_eq!(child.parent_span_id, Some(span.span_id));
        assert_eq!(child.trace_id, span.trace_id);
    }

    #[test]
    fn test_sampler_rates() {
        let sampler_zero = TraceSampler::new(0.0);
        let sampler_one = TraceSampler::new(1.0);

        assert_eq!(sampler_zero.sample_rate, 0.0);
        assert_eq!(sampler_one.sample_rate, 1.0);
    }

    #[tokio::test]
    async fn test_cloud_trace_start() {
        let trace = CloudTrace::new("project-1".to_string(), 1.0);
        let trace_id = trace
            .start_trace("request".to_string())
            .await
            .expect("Failed to start trace");

        assert!(!trace_id.is_empty());
    }

    #[tokio::test]
    async fn test_cloud_trace_span_creation() {
        let trace_client = CloudTrace::new("project-1".to_string(), 1.0);
        let trace_id = trace_client
            .start_trace("request".to_string())
            .await
            .expect("Failed to start trace");

        let span_id = trace_client
            .create_span(&trace_id, "operation".to_string(), None)
            .await
            .expect("Failed to create span");

        assert!(!span_id.is_empty());
    }

    #[tokio::test]
    async fn test_cloud_trace_finish() {
        let trace_client = CloudTrace::new("project-1".to_string(), 1.0);
        let trace_id = trace_client
            .start_trace("request".to_string())
            .await
            .expect("Failed to start trace");

        trace_client
            .finish_trace(&trace_id)
            .await
            .expect("Failed to finish trace");

        let completed = trace_client
            .get_completed_traces(10)
            .await
            .expect("Failed to get traces");

        assert_eq!(completed.len(), 1);
    }
}
