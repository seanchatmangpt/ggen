//! OpenTelemetry Span Correlation and Distributed Tracing
//!
//! Maintains parent-child span relationships across ETL → Orchestrator → Workflow
//! to enable end-to-end tracing.

use std::collections::HashMap;
use uuid::Uuid;

/// Span correlation context for distributed tracing
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SpanContext {
    /// Root trace ID (follows transaction_id from ETL)
    pub trace_id: String,

    /// Parent span ID from calling system
    pub parent_span_id: String,

    /// This system's span ID
    pub span_id: String,

    /// Attributes attached to this span
    pub attributes: HashMap<String, String>,
}

impl SpanContext {
    /// Create a new root span context (ETL entry point)
    #[must_use]
    pub fn root(transaction_id: String) -> Self {
        let span_id = Uuid::new_v4().to_string();

        Self {
            trace_id: transaction_id,
            parent_span_id: String::new(),
            span_id,
            attributes: HashMap::new(),
        }
    }

    /// Create a child span context
    #[must_use]
    pub fn child(&self) -> Self {
        Self {
            trace_id: self.trace_id.clone(),
            parent_span_id: self.span_id.clone(),
            span_id: Uuid::new_v4().to_string(),
            attributes: HashMap::new(),
        }
    }

    /// Add a span attribute
    pub fn add_attribute(&mut self, key: String, value: String) {
        self.attributes.insert(key, value);
    }

    /// Get a span attribute
    #[must_use]
    pub fn get_attribute(&self, key: &str) -> Option<String> {
        self.attributes.get(key).cloned()
    }

    /// Format for OpenTelemetry W3C Trace Context header
    ///
    /// Returns string like: "00-<trace_id>-<span_id>-01"
    #[must_use]
    pub fn to_w3c_trace_context(&self) -> String {
        format!("00-{}-{}-01", self.trace_id, self.span_id)
    }
}

/// Span correlation matrix maintaining relationships across systems
pub struct SpanCorrelation {
    /// Map of span ID → SpanContext
    spans: HashMap<String, SpanContext>,

    /// Map of transaction ID → root span ID
    traces: HashMap<String, String>,
}

impl SpanCorrelation {
    /// Create a new span correlation matrix
    #[must_use]
    pub fn new() -> Self {
        Self {
            spans: HashMap::new(),
            traces: HashMap::new(),
        }
    }

    /// Register a root span (ETL event entry point)
    pub fn register_root(&mut self, transaction_id: String) -> SpanContext {
        let span = SpanContext::root(transaction_id.clone());
        let span_id = span.span_id.clone();

        self.spans.insert(span_id.clone(), span.clone());
        self.traces.insert(transaction_id, span_id);

        span
    }

    /// Register a child span under an existing parent
    pub fn register_child(&mut self, parent_span_id: &str) -> Option<SpanContext> {
        let parent = self.spans.get(parent_span_id)?.clone();
        let child = parent.child();
        let child_span_id = child.span_id.clone();

        self.spans.insert(child_span_id, child.clone());
        Some(child)
    }

    /// Get a span context by ID
    #[must_use]
    pub fn get_span(&self, span_id: &str) -> Option<SpanContext> {
        self.spans.get(span_id).cloned()
    }

    /// Get root span for a transaction
    #[must_use]
    pub fn get_root_span(&self, transaction_id: &str) -> Option<SpanContext> {
        self.traces
            .get(transaction_id)
            .and_then(|span_id| self.spans.get(span_id))
            .cloned()
    }

    /// Get full span chain for a span (root → ... → span)
    #[must_use]
    pub fn get_span_chain(&self, span_id: &str) -> Vec<SpanContext> {
        let mut chain = Vec::new();
        let mut current_id = span_id.to_string();

        while let Some(span) = self.spans.get(&current_id) {
            chain.push(span.clone());
            if span.parent_span_id.is_empty() {
                break; // Reached root
            }
            current_id = span.parent_span_id.clone();
        }

        chain.reverse(); // Root to leaf
        chain
    }

    /// Record a span attribute
    pub fn set_attribute(&mut self, span_id: &str, key: String, value: String) {
        if let Some(span) = self.spans.get_mut(span_id) {
            span.add_attribute(key, value);
        }
    }

    /// Check if all spans in a trace are complete (sanity check)
    #[must_use]
    pub fn is_trace_complete(&self, transaction_id: &str) -> bool {
        if let Some(root_span_id) = self.traces.get(transaction_id) {
            // A complete trace should have root span
            self.spans.contains_key(root_span_id)
        } else {
            false
        }
    }

    /// Get all spans for a transaction
    #[must_use]
    pub fn get_all_spans(&self, transaction_id: &str) -> Vec<SpanContext> {
        if let Some(root_span_id) = self.traces.get(transaction_id) {
            let root = self.spans.get(root_span_id).cloned();
            let mut spans = if let Some(r) = root {
                vec![r]
            } else {
                vec![]
            };

            // Find all children (simple depth-first)
            self.spans
                .values()
                .filter(|s| s.trace_id == transaction_id && s.span_id != *root_span_id)
                .for_each(|s| spans.push(s.clone()));

            spans
        } else {
            vec![]
        }
    }

    /// Total number of spans tracked
    #[must_use]
    pub fn span_count(&self) -> usize {
        self.spans.len()
    }

    /// Total number of traces tracked
    #[must_use]
    pub fn trace_count(&self) -> usize {
        self.traces.len()
    }
}

impl Default for SpanCorrelation {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_context_creation() {
        let span = SpanContext::root("txn-123".to_string());

        assert_eq!(span.trace_id, "txn-123");
        assert!(span.parent_span_id.is_empty());
        assert!(!span.span_id.is_empty());
    }

    #[test]
    fn test_span_context_child() {
        let parent = SpanContext::root("txn-123".to_string());
        let child = parent.child();

        assert_eq!(child.trace_id, parent.trace_id);
        assert_eq!(child.parent_span_id, parent.span_id);
        assert_ne!(child.span_id, parent.span_id);
    }

    #[test]
    fn test_w3c_trace_context() {
        let span = SpanContext::root("txn-123".to_string());
        let w3c = span.to_w3c_trace_context();

        assert!(w3c.starts_with("00-"));
        assert!(w3c.ends_with("-01"));
        assert!(w3c.contains(&span.trace_id));
        assert!(w3c.contains(&span.span_id));
    }

    #[test]
    fn test_span_attributes() {
        let mut span = SpanContext::root("txn-123".to_string());
        span.add_attribute("key1".to_string(), "value1".to_string());

        assert_eq!(span.get_attribute("key1"), Some("value1".to_string()));
        assert_eq!(span.get_attribute("key2"), None);
    }

    #[test]
    fn test_span_correlation_registration() {
        let mut correlation = SpanCorrelation::new();
        let root = correlation.register_root("txn-123".to_string());

        assert_eq!(correlation.span_count(), 1);
        assert_eq!(correlation.trace_count(), 1);
        assert_eq!(correlation.get_span(&root.span_id), Some(root.clone()));
    }

    #[test]
    fn test_span_correlation_child_registration() {
        let mut correlation = SpanCorrelation::new();
        let root = correlation.register_root("txn-123".to_string());
        let child = correlation.register_child(&root.span_id).unwrap();

        assert_eq!(correlation.span_count(), 2);
        assert_eq!(child.parent_span_id, root.span_id);
    }

    #[test]
    fn test_span_chain_retrieval() {
        let mut correlation = SpanCorrelation::new();
        let root = correlation.register_root("txn-123".to_string());
        let child = correlation.register_child(&root.span_id).unwrap();

        let chain = correlation.get_span_chain(&child.span_id);
        assert_eq!(chain.len(), 2);
        assert_eq!(chain[0].span_id, root.span_id);
        assert_eq!(chain[1].span_id, child.span_id);
    }

    #[test]
    fn test_get_root_span() {
        let mut correlation = SpanCorrelation::new();
        let root = correlation.register_root("txn-123".to_string());

        let retrieved = correlation.get_root_span("txn-123");
        assert_eq!(retrieved, Some(root));
    }

    #[test]
    fn test_trace_completeness() {
        let mut correlation = SpanCorrelation::new();
        correlation.register_root("txn-123".to_string());

        assert!(correlation.is_trace_complete("txn-123"));
        assert!(!correlation.is_trace_complete("txn-999"));
    }

    #[test]
    fn test_get_all_spans() {
        let mut correlation = SpanCorrelation::new();
        let root = correlation.register_root("txn-123".to_string());
        let _child1 = correlation.register_child(&root.span_id).unwrap();
        let _child2 = correlation.register_child(&root.span_id).unwrap();

        let spans = correlation.get_all_spans("txn-123");
        assert_eq!(spans.len(), 3);
    }
}
