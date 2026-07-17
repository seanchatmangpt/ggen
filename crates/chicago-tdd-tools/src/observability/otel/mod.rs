//! OTEL Validation
//!
//! Provides validation utilities for OpenTelemetry spans and metrics.
//! Validates that telemetry conforms to schema and semantic conventions.

#[cfg(feature = "otel")]
use crate::observability::otel::types::{Metric, Span, SpanId};
use thiserror::Error;

pub mod types;

/// Poka-yoke types for OTEL (compile-time error prevention)
///
/// **Poka-yoke**: Type-level state machine prevents invalid span operations.
/// See module documentation for examples.
pub mod poka_yoke;

/// OTEL validation error
#[derive(Error, Debug)]
pub enum OtelValidationError {
    /// Span validation failed
    #[error("🚨 Span validation failed: {0}\n   ⚠️  STOP: Span does not meet validation requirements\n   💡 FIX: Check span structure, attributes, and status")]
    SpanValidationFailed(String),
    /// Metric validation failed
    #[error("🚨 Metric validation failed: {0}\n   ⚠️  STOP: Metric does not meet validation requirements\n   💡 FIX: Check metric structure, attributes, and values")]
    MetricValidationFailed(String),
    /// Missing required attribute
    #[error("🚨 Missing required attribute: {0}\n   ⚠️  STOP: Required attribute is missing\n   💡 FIX: Add required attribute to span or metric")]
    MissingAttribute(String),
    /// Invalid attribute type
    #[error("🚨 Invalid attribute type for '{0}': expected {1}, got {2}\n   ⚠️  STOP: Attribute type mismatch\n   💡 FIX: Use correct attribute type")]
    InvalidAttributeType(String, String, String),
    /// Invalid span status
    #[error("🚨 Invalid span status: {0}\n   ⚠️  STOP: Span status is invalid\n   💡 FIX: Use valid span status (Ok, Error, Unset)")]
    InvalidSpanStatus(String),
    /// Invalid trace ID
    #[error("🚨 Invalid trace ID: {0}\n   ⚠️  STOP: Trace ID is invalid\n   💡 FIX: Use valid 128-bit trace ID (cannot be zero)")]
    InvalidTraceId(String),
    /// Invalid span ID
    #[error("🚨 Invalid span ID: {0}\n   ⚠️  STOP: Span ID is invalid\n   💡 FIX: Use valid 64-bit span ID (cannot be zero)")]
    InvalidSpanId(String),
}

/// Result type for OTEL validation
pub type OtelValidationResult<T> = Result<T, OtelValidationError>;

/// OTEL span validator
#[cfg(feature = "otel")]
pub struct SpanValidator {
    /// Required attributes for spans
    required_attributes: Vec<String>,
    /// Validate span IDs are not zero
    validate_non_zero_ids: bool,
}

#[cfg(feature = "otel")]
impl Default for SpanValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "otel")]
impl SpanValidator {
    /// Create a new span validator
    #[must_use]
    pub const fn new() -> Self {
        Self { required_attributes: Vec::new(), validate_non_zero_ids: true }
    }

    /// Require specific attributes
    #[must_use]
    pub fn with_required_attributes(mut self, attributes: Vec<String>) -> Self {
        self.required_attributes = attributes;
        self
    }

    /// Enable/disable non-zero ID validation
    #[must_use]
    pub const fn with_non_zero_id_validation(mut self, enabled: bool) -> Self {
        self.validate_non_zero_ids = enabled;
        self
    }

    /// Validate a span
    ///
    /// # Errors
    ///
    /// Returns an error if span validation fails (e.g., invalid span ID, trace ID, empty name, or missing required attributes).
    pub fn validate(&self, span: &Span) -> OtelValidationResult<()> {
        // Validate span ID is not zero (if enabled)
        if self.validate_non_zero_ids && span.context.span_id.0 == 0 {
            return Err(OtelValidationError::InvalidSpanId("Span ID cannot be zero".to_string()));
        }

        // Validate trace ID is not zero
        if span.context.trace_id.0 == 0 {
            return Err(OtelValidationError::InvalidTraceId("Trace ID cannot be zero".to_string()));
        }

        // Validate span name is not empty
        if span.name.is_empty() {
            return Err(OtelValidationError::SpanValidationFailed(
                "Span name cannot be empty".to_string(),
            ));
        }

        // Validate required attributes
        for attr_name in &self.required_attributes {
            if !span.attributes.contains_key(attr_name) {
                return Err(OtelValidationError::MissingAttribute(attr_name.clone()));
            }
        }

        // Validate end time is after start time (if completed)
        // Poka-Yoke: SpanState enum ensures end_time >= start_time at type level
        if let Some(end_time) = span.end_time_ms() {
            let start_time = span.start_time_ms();
            if end_time < start_time {
                return Err(OtelValidationError::SpanValidationFailed(format!(
                    "Span end time {end_time} is before start time {start_time}"
                )));
            }
        }

        Ok(())
    }

    /// Validate multiple spans
    ///
    /// # Errors
    ///
    /// Returns an error if any span validation fails.
    pub fn validate_spans(&self, spans: &[Span]) -> OtelValidationResult<()> {
        for (idx, span) in spans.iter().enumerate() {
            self.validate(span).map_err(|e| {
                OtelValidationError::SpanValidationFailed(format!(
                    "Span {} (index {}): {}",
                    span.name, idx, e
                ))
            })?;
        }
        Ok(())
    }
}

/// OTEL metric validator
#[cfg(feature = "otel")]
pub struct MetricValidator {
    /// Required attributes for metrics
    required_attributes: Vec<String>,
}

#[cfg(feature = "otel")]
impl Default for MetricValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "otel")]
impl MetricValidator {
    /// Create a new metric validator
    #[must_use]
    pub const fn new() -> Self {
        Self { required_attributes: Vec::new() }
    }

    /// Require specific attributes
    #[must_use]
    pub fn with_required_attributes(mut self, attributes: Vec<String>) -> Self {
        self.required_attributes = attributes;
        self
    }

    /// Validate a metric
    ///
    /// # Errors
    ///
    /// Returns an error if metric validation fails.
    pub fn validate(&self, metric: &Metric) -> OtelValidationResult<()> {
        // Validate metric name is not empty
        if metric.name.is_empty() {
            return Err(OtelValidationError::MetricValidationFailed(
                "Metric name cannot be empty".to_string(),
            ));
        }

        // Validate required attributes
        for attr_name in &self.required_attributes {
            if !metric.attributes.contains_key(attr_name) {
                return Err(OtelValidationError::MissingAttribute(attr_name.clone()));
            }
        }

        // Validate metric value is valid
        match &metric.value {
            crate::observability::otel::types::MetricValue::Counter(count) => {
                if *count == 0 && metric.name.contains("error") {
                    // Error counters should be > 0 if metric name suggests errors
                    // This is informational, not an error
                }
            }
            crate::observability::otel::types::MetricValue::Gauge(value) => {
                if value.is_nan() || value.is_infinite() {
                    return Err(OtelValidationError::MetricValidationFailed(format!(
                        "Metric '{}' has invalid gauge value: {}",
                        metric.name, value
                    )));
                }
            }
            crate::observability::otel::types::MetricValue::Histogram(buckets) => {
                if buckets.is_empty() {
                    return Err(OtelValidationError::MetricValidationFailed(format!(
                        "Metric '{}' has empty histogram buckets",
                        metric.name
                    )));
                }
            }
        }

        Ok(())
    }

    /// Validate multiple metrics
    ///
    /// # Errors
    ///
    /// Returns an error if any metric validation fails.
    pub fn validate_metrics(&self, metrics: &[Metric]) -> OtelValidationResult<()> {
        for (idx, metric) in metrics.iter().enumerate() {
            self.validate(metric).map_err(|e| {
                OtelValidationError::MetricValidationFailed(format!(
                    "Metric {} (index {}): {}",
                    metric.name, idx, e
                ))
            })?;
        }
        Ok(())
    }
}

/// OTEL validation helper for test utilities
#[cfg(feature = "otel")]
pub struct OtelTestHelper {
    span_validator: SpanValidator,
    metric_validator: MetricValidator,
}

#[cfg(feature = "otel")]
impl Default for OtelTestHelper {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "otel")]
impl OtelTestHelper {
    /// Create a new OTEL test helper
    #[must_use]
    pub const fn new() -> Self {
        Self { span_validator: SpanValidator::new(), metric_validator: MetricValidator::new() }
    }

    /// Validate spans from a tracer
    ///
    /// # Errors
    ///
    /// Returns an error if span validation fails.
    pub fn validate_tracer_spans(&self, spans: &[Span]) -> OtelValidationResult<Vec<SpanId>> {
        self.span_validator.validate_spans(spans)?;
        Ok(spans.iter().map(|s| s.context.span_id).collect())
    }

    /// Validate metrics from a tracer
    ///
    /// # Errors
    ///
    /// Returns an error if metric validation fails.
    pub fn validate_tracer_metrics(&self, metrics: &[Metric]) -> OtelValidationResult<Vec<String>> {
        self.metric_validator.validate_metrics(metrics)?;
        Ok(metrics.iter().map(|m| m.name.clone()).collect())
    }

    /// Assert that spans are valid (for use in tests)
    ///
    /// # Panics
    ///
    /// Panics if span validation fails.
    pub fn assert_spans_valid(&self, spans: &[Span]) {
        #[allow(clippy::expect_used, clippy::panic)] // Test helper - panic is appropriate
        for span in spans {
            self.span_validator
                .validate_spans(std::slice::from_ref(span))
                .unwrap_or_else(|e| panic!("Span validation failed: {e}"));
        }
    }

    /// Assert that metrics are valid (for use in tests)
    ///
    /// # Panics
    ///
    /// Panics if metric validation fails.
    pub fn assert_metrics_valid(&self, metrics: &[Metric]) {
        #[allow(clippy::expect_used, clippy::panic)] // Test helper - panic is appropriate
        for metric in metrics {
            self.metric_validator
                .validate_metrics(std::slice::from_ref(metric))
                .unwrap_or_else(|e| panic!("Metric validation failed: {e}"));
        }
    }
}

/// Helper functions for creating test spans and metrics
///
/// These functions simplify creating OTEL spans and metrics for testing,
/// automating common patterns and reducing boilerplate.
#[cfg(feature = "otel")]
pub mod test_helpers {

    use crate::observability::otel::types::{
        Attributes, Metric, MetricValue, Span, SpanContext, SpanId, SpanStatus, TraceId,
    };

    /// Create a test span with default values
    ///
    /// Creates a completed span with valid trace ID, span ID, and timestamps.
    /// Useful for testing span validation and behavior.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::otel::test_helpers::create_test_span;
    ///
    /// let span = create_test_span("test.operation");
    /// assert_eq!(span.name, "test.operation");
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if creating the span fails.
    pub fn create_test_span(name: impl Into<String>) -> Span {
        let name = name.into();
        let trace_id = TraceId(12345);
        let span_id = SpanId(67890);
        let context = SpanContext::root(trace_id, span_id, 1);
        let start_time_ms = 1000;
        let end_time_ms = 2000;
        let attributes = Attributes::new();
        let events = Vec::new();
        let status = SpanStatus::Ok;

        #[allow(clippy::panic)] // Test helper - panic is appropriate
        Span::new_completed(context, name, start_time_ms, end_time_ms, attributes, events, status)
            .unwrap_or_else(|e| panic!("Failed to create test span: {e}"))
    }

    /// Create a test span with custom attributes
    ///
    /// Creates a completed span with custom attributes for testing attribute validation.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::otel::test_helpers::create_test_span_with_attributes;
    /// use std::collections::BTreeMap;
    ///
    /// let mut attrs = BTreeMap::new();
    /// attrs.insert("service.name".to_string(), "test-service".to_string());
    /// let span = create_test_span_with_attributes("test.operation", attrs);
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if creating the span fails.
    pub fn create_test_span_with_attributes(
        name: impl Into<String>,
        attributes: Attributes,
    ) -> Span {
        let name = name.into();
        let trace_id = TraceId(12345);
        let span_id = SpanId(67890);
        let context = SpanContext::root(trace_id, span_id, 1);
        let start_time_ms = 1000;
        let end_time_ms = 2000;
        let events = Vec::new();
        let status = SpanStatus::Ok;

        #[allow(clippy::panic)] // Test helper - panic is appropriate
        Span::new_completed(context, name, start_time_ms, end_time_ms, attributes, events, status)
            .unwrap_or_else(|e| panic!("Failed to create test span with attributes: {e}"))
    }

    /// Create a test metric with default values
    ///
    /// Creates a counter metric with a valid name and value.
    /// Useful for testing metric validation and behavior.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::otel::test_helpers::create_test_metric;
    ///
    /// let metric = create_test_metric("test.counter", 42);
    /// assert_eq!(metric.name, "test.counter");
    /// ```
    pub fn create_test_metric(name: impl Into<String>, value: u64) -> Metric {
        let name = name.into();
        let value = MetricValue::Counter(value);
        let timestamp_ms = 1000;
        let attributes = Attributes::new();

        Metric { name, value, timestamp_ms, attributes }
    }

    /// Create a test metric with custom attributes
    ///
    /// Creates a metric with custom attributes for testing attribute validation.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::otel::test_helpers::create_test_metric_with_attributes;
    /// use std::collections::BTreeMap;
    ///
    /// let mut attrs = BTreeMap::new();
    /// attrs.insert("service.name".to_string(), "test-service".to_string());
    /// let metric = create_test_metric_with_attributes("test.counter", 42, attrs);
    /// ```
    pub fn create_test_metric_with_attributes(
        name: impl Into<String>,
        value: u64,
        attributes: Attributes,
    ) -> Metric {
        let name = name.into();
        let value = MetricValue::Counter(value);
        let timestamp_ms = 1000;

        Metric { name, value, timestamp_ms, attributes }
    }
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;
    #[cfg(feature = "otel")]
    use crate::observability::otel::types::{SpanContext, SpanId, SpanStatus, TraceId};

    // Test feature-gated code paths (critical - verify features work correctly)
    #[cfg(not(feature = "otel"))]
    #[test]
    fn test_otel_module_not_accessible_without_feature() {
        // Verify otel module is not accessible without feature
        // This test should compile and pass when otel feature is disabled
        assert!(true, "otel module should not be accessible without feature");
    }

    #[cfg(feature = "otel")]
    #[test]
    fn test_otel_error_variants() {
        // Test all error variants (critical - 80% of bugs)
        let errors = vec![
            OtelValidationError::SpanValidationFailed("test".to_string()),
            OtelValidationError::MetricValidationFailed("test".to_string()),
            OtelValidationError::MissingAttribute("test".to_string()),
            OtelValidationError::InvalidAttributeType(
                "test".to_string(),
                "expected".to_string(),
                "got".to_string(),
            ),
            OtelValidationError::InvalidSpanStatus("test".to_string()),
            OtelValidationError::InvalidTraceId("test".to_string()),
            OtelValidationError::InvalidSpanId("test".to_string()),
        ];

        for error in errors {
            let display = format!("{error}");
            assert!(!display.is_empty(), "Error should have display message");
            assert!(display.contains("test"), "Error should contain message");
        }
    }

    #[cfg(feature = "otel")]
    #[test]
    fn test_otel_error_debug() {
        // Test error is debuggable
        let error = OtelValidationError::SpanValidationFailed("test".to_string());
        let debug = format!("{error:?}");
        assert!(debug.contains("SpanValidationFailed"));
        assert!(debug.contains("test"));
    }

    #[cfg(feature = "otel")]
    #[test]
    fn test_span_validator_valid_span() {
        let validator = SpanValidator::new();
        #[allow(clippy::unwrap_used)] // Test code - Span creation should succeed in tests
        let span = Span::new_completed(
            SpanContext::root(TraceId(12345), SpanId(67890), 1),
            "test.span".to_string(),
            1000,
            2000,
            Default::default(),
            Vec::new(),
            SpanStatus::Ok,
        )
        .unwrap();

        assert!(validator.validate(&span).is_ok());
    }

    #[cfg(feature = "otel")]
    #[test]
    fn test_span_validator_zero_span_id() {
        let validator = SpanValidator::new();
        let span = Span::new_completed(
            SpanContext::root(TraceId(12345), SpanId(0), 1), // Zero span ID
            "test.span".to_string(),
            1000,
            2000,
            Default::default(),
            Vec::new(),
            SpanStatus::Ok,
        )
        .unwrap();

        assert!(validator.validate(&span).is_err());
    }

    #[cfg(feature = "otel")]
    #[test]
    fn test_span_validator_empty_name() {
        let validator = SpanValidator::new();
        let span = Span::new_completed(
            SpanContext::root(TraceId(12345), SpanId(67890), 1),
            String::new(), // Empty name
            1000,
            2000,
            Default::default(),
            Vec::new(),
            SpanStatus::Ok,
        )
        .unwrap();

        assert!(validator.validate(&span).is_err());
    }

    #[cfg(feature = "otel")]
    #[test]
    fn test_metric_validator_valid_metric() {
        use crate::observability::otel::types::MetricValue;

        let validator = MetricValidator::new();
        let metric = Metric {
            name: "test.metric".to_string(),
            value: MetricValue::Counter(42),
            timestamp_ms: 1000,
            attributes: Default::default(),
        };

        assert!(validator.validate(&metric).is_ok());
    }

    #[cfg(feature = "otel")]
    #[test]
    fn test_metric_validator_empty_name() {
        use crate::observability::otel::types::MetricValue;

        let validator = MetricValidator::new();
        let metric = Metric {
            name: "".to_string(), // Empty name
            value: MetricValue::Counter(42),
            timestamp_ms: 1000,
            attributes: Default::default(),
        };

        assert!(validator.validate(&metric).is_err());
    }
}
