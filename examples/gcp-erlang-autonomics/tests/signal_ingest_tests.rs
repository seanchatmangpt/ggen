//! Signal ingestion tests: Normalize raw GCP events into canonical signals
//!
//! Chicago TDD pattern:
//! - Arrange: Set up raw GCP events
//! - Act: Process via SignalIngest
//! - Assert: Verify output signals match expected schema and are state-verified
//!
//! Uses real collaborators (serde_json, chrono), state-based verification.

use serde_json::{json, Value};
use std::collections::HashMap;

/// Signal ingestion component that normalizes raw GCP events
#[derive(Debug, Clone)]
pub struct SignalIngest {
    /// Processed signals
    signals: Vec<NormalizedSignal>,
    /// Duplicate detection cache (per-tenant)
    seen_ids: HashMap<String, std::collections::HashSet<String>>,
}

/// A normalized signal with schema validation
#[derive(Debug, Clone, PartialEq)]
pub struct NormalizedSignal {
    /// Signal ID (unique per tenant)
    pub id: String,
    /// Signal type
    pub signal_type: String,
    /// Signal payload
    pub payload: Value,
    /// Tenant ID
    pub tenant_id: String,
    /// Timestamp (ISO 8601)
    pub timestamp: String,
    /// Validation status
    pub is_valid: bool,
    /// Error details if invalid
    pub validation_errors: Vec<String>,
}

impl SignalIngest {
    pub fn new() -> Self {
        Self {
            signals: Vec::new(),
            seen_ids: HashMap::new(),
        }
    }

    /// Ingest a raw GCP billing event
    pub fn ingest_billing_event(
        &mut self,
        tenant_id: &str,
        event: &Value,
    ) -> Result<NormalizedSignal, String> {
        // Validate required fields
        let mut errors = Vec::new();

        // Check for required billing fields
        if !event.get("project_id").is_some() {
            errors.push("missing required field: project_id".to_string());
        }
        if !event.get("service").is_some() {
            errors.push("missing required field: service".to_string());
        }
        if !event.get("cost").is_some() {
            errors.push("missing required field: cost".to_string());
        }

        let signal_id = format!(
            "{}-{}-{}",
            tenant_id,
            event.get("project_id").and_then(|v| v.as_str()).unwrap_or("unknown"),
            chrono::Utc::now().timestamp_millis()
        );

        // Check for duplicates
        self.seen_ids
            .entry(tenant_id.to_string())
            .or_insert_with(std::collections::HashSet::new);

        let is_duplicate = self
            .seen_ids
            .get(tenant_id)
            .map(|s| s.contains(&signal_id))
            .unwrap_or(false);

        if is_duplicate {
            errors.push("duplicate signal detected".to_string());
        }

        let is_valid = errors.is_empty();

        let signal = NormalizedSignal {
            id: signal_id.clone(),
            signal_type: "billing.cost".to_string(),
            payload: event.clone(),
            tenant_id: tenant_id.to_string(),
            timestamp: chrono::Utc::now().to_rfc3339(),
            is_valid,
            validation_errors: errors,
        };

        if is_valid {
            self.seen_ids
                .get_mut(tenant_id)
                .unwrap()
                .insert(signal_id);
        }

        self.signals.push(signal.clone());
        Ok(signal)
    }

    /// Ingest a raw GCP monitoring event (metrics)
    pub fn ingest_monitoring_event(
        &mut self,
        tenant_id: &str,
        event: &Value,
    ) -> Result<NormalizedSignal, String> {
        let mut errors = Vec::new();

        // Validate monitoring event structure
        if !event.get("metric_name").is_some() {
            errors.push("missing required field: metric_name".to_string());
        }
        if !event.get("value").is_some() {
            errors.push("missing required field: value".to_string());
        }

        let signal_id = format!(
            "{}-{}-{}",
            tenant_id,
            event.get("metric_name").and_then(|v| v.as_str()).unwrap_or("unknown"),
            chrono::Utc::now().timestamp_millis()
        );

        // Duplicate check
        self.seen_ids
            .entry(tenant_id.to_string())
            .or_insert_with(std::collections::HashSet::new);

        let is_duplicate = self
            .seen_ids
            .get(tenant_id)
            .map(|s| s.contains(&signal_id))
            .unwrap_or(false);

        if is_duplicate {
            errors.push("duplicate signal detected".to_string());
        }

        let is_valid = errors.is_empty();

        let signal = NormalizedSignal {
            id: signal_id.clone(),
            signal_type: "monitoring.metric".to_string(),
            payload: event.clone(),
            tenant_id: tenant_id.to_string(),
            timestamp: chrono::Utc::now().to_rfc3339(),
            is_valid,
            validation_errors: errors,
        };

        if is_valid {
            self.seen_ids
                .get_mut(tenant_id)
                .unwrap()
                .insert(signal_id);
        }

        self.signals.push(signal.clone());
        Ok(signal)
    }

    /// Ingest a raw GCP logging event
    pub fn ingest_logging_event(
        &mut self,
        tenant_id: &str,
        event: &Value,
    ) -> Result<NormalizedSignal, String> {
        let mut errors = Vec::new();

        // Validate logging event
        if !event.get("severity").is_some() {
            errors.push("missing required field: severity".to_string());
        }
        if !event.get("message").is_some() {
            errors.push("missing required field: message".to_string());
        }

        let signal_id = format!(
            "{}-log-{}",
            tenant_id,
            chrono::Utc::now().timestamp_millis()
        );

        self.seen_ids
            .entry(tenant_id.to_string())
            .or_insert_with(std::collections::HashSet::new);

        let is_duplicate = self
            .seen_ids
            .get(tenant_id)
            .map(|s| s.contains(&signal_id))
            .unwrap_or(false);

        if is_duplicate {
            errors.push("duplicate signal detected".to_string());
        }

        let is_valid = errors.is_empty();

        let signal = NormalizedSignal {
            id: signal_id.clone(),
            signal_type: "logging.entry".to_string(),
            payload: event.clone(),
            tenant_id: tenant_id.to_string(),
            timestamp: chrono::Utc::now().to_rfc3339(),
            is_valid,
            validation_errors: errors,
        };

        if is_valid {
            self.seen_ids
                .get_mut(tenant_id)
                .unwrap()
                .insert(signal_id);
        }

        self.signals.push(signal.clone());
        Ok(signal)
    }

    /// Get all ingested signals
    pub fn signals(&self) -> &[NormalizedSignal] {
        &self.signals
    }

    /// Get signals for a specific tenant
    pub fn signals_for_tenant(&self, tenant_id: &str) -> Vec<&NormalizedSignal> {
        self.signals
            .iter()
            .filter(|s| s.tenant_id == tenant_id)
            .collect()
    }

    /// Get valid signals only
    pub fn valid_signals(&self) -> Vec<&NormalizedSignal> {
        self.signals.iter().filter(|s| s.is_valid).collect()
    }

    /// Get invalid signals (with errors)
    pub fn invalid_signals(&self) -> Vec<&NormalizedSignal> {
        self.signals.iter().filter(|s| !s.is_valid).collect()
    }
}

impl Default for SignalIngest {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test 1: Basic billing event normalization (AAA pattern)
    #[test]
    fn test_billing_event_normalization() {
        // Arrange: Create a raw GCP billing event
        let mut ingest = SignalIngest::new();
        let billing_event = json!({
            "project_id": "my-project",
            "service": "compute",
            "cost": 123.45,
            "currency": "USD"
        });

        // Act: Ingest the event
        let result = ingest.ingest_billing_event("tenant-1", &billing_event);

        // Assert: Verify the signal was normalized correctly
        assert!(result.is_ok(), "Billing event ingestion should succeed");

        let signal = result.unwrap();
        assert_eq!(signal.signal_type, "billing.cost", "Signal type should be billing.cost");
        assert_eq!(signal.tenant_id, "tenant-1", "Tenant ID should be preserved");
        assert!(signal.is_valid, "Signal should be valid");
        assert!(signal.validation_errors.is_empty(), "No validation errors expected");
        assert!(signal.payload.get("cost").is_some(), "Cost field should be preserved");

        // State verification: Check that signal is stored
        let signals = ingest.signals();
        assert_eq!(signals.len(), 1, "One signal should be stored");
        assert_eq!(signals[0].id, signal.id, "Signal ID should match");
    }

    // Test 2: Monitoring event with metric normalization
    #[test]
    fn test_monitoring_event_normalization() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let monitoring_event = json!({
            "metric_name": "cpu_usage_percent",
            "value": 85.5,
            "resource": "instance-1"
        });

        // Act
        let result = ingest.ingest_monitoring_event("tenant-2", &monitoring_event);

        // Assert
        assert!(result.is_ok());
        let signal = result.unwrap();
        assert_eq!(signal.signal_type, "monitoring.metric");
        assert_eq!(signal.tenant_id, "tenant-2");
        assert!(signal.is_valid);
        assert_eq!(signal.payload.get("metric_name").unwrap().as_str().unwrap(), "cpu_usage_percent");

        // State: Verify stored
        assert_eq!(ingest.signals().len(), 1);
    }

    // Test 3: Logging event normalization
    #[test]
    fn test_logging_event_normalization() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let log_event = json!({
            "severity": "ERROR",
            "message": "Service unavailable",
            "timestamp": "2025-01-25T10:00:00Z"
        });

        // Act
        let result = ingest.ingest_logging_event("tenant-1", &log_event);

        // Assert
        assert!(result.is_ok());
        let signal = result.unwrap();
        assert_eq!(signal.signal_type, "logging.entry");
        assert!(signal.is_valid);
        assert_eq!(signal.payload.get("severity").unwrap().as_str().unwrap(), "ERROR");
    }

    // Test 4: Missing required fields (malformed data)
    #[test]
    fn test_billing_event_missing_project_id() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let incomplete_event = json!({
            "service": "compute",
            "cost": 100.0
            // Missing project_id
        });

        // Act
        let result = ingest.ingest_billing_event("tenant-1", &incomplete_event);

        // Assert
        assert!(result.is_ok());
        let signal = result.unwrap();
        assert!(!signal.is_valid, "Signal should be invalid");
        assert!(signal.validation_errors.len() > 0, "Should have validation errors");
        assert!(
            signal.validation_errors[0].contains("project_id"),
            "Should report missing project_id"
        );

        // State: Invalid signal still stored for audit
        assert_eq!(ingest.signals().len(), 1);
        assert_eq!(ingest.invalid_signals().len(), 1);
        assert_eq!(ingest.valid_signals().len(), 0);
    }

    // Test 5: Duplicate detection for same project within millisecond
    #[test]
    fn test_duplicate_detection_same_millisecond() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let event = json!({
            "project_id": "project-1",
            "service": "compute",
            "cost": 50.0
        });

        // Act: Ingest event
        let signal1 = ingest.ingest_billing_event("tenant-1", &event).unwrap();
        assert!(signal1.is_valid, "First signal should be valid");
        assert_eq!(ingest.signals().len(), 1);

        // Act: Immediately ingest same event again (likely same millisecond)
        // Signal IDs include timestamps in milliseconds, so if both called in same ms,
        // they generate the same ID and duplicate detection triggers
        let signal2 = ingest.ingest_billing_event("tenant-1", &event).unwrap();

        // Assert: Second signal is marked as duplicate (same millisecond = same ID)
        assert!(!signal2.is_valid, "Second signal should be marked invalid (duplicate)");
        assert!(
            signal2.validation_errors.iter().any(|e| e.contains("duplicate")),
            "Should report duplicate error"
        );

        // State: Both stored, one valid one invalid
        assert_eq!(ingest.signals().len(), 2);
        assert_eq!(ingest.valid_signals().len(), 1);
        assert_eq!(ingest.invalid_signals().len(), 1);
    }

    // Test 6: Multi-tenant isolation
    #[test]
    fn test_multi_tenant_isolation() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let event1 = json!({"project_id": "proj-1", "service": "compute", "cost": 100.0});
        let event2 = json!({"project_id": "proj-2", "service": "storage", "cost": 50.0});

        // Act: Ingest for different tenants
        ingest.ingest_billing_event("tenant-1", &event1).unwrap();
        ingest.ingest_billing_event("tenant-2", &event2).unwrap();

        // Assert: Signals are isolated per tenant
        let tenant1_signals = ingest.signals_for_tenant("tenant-1");
        let tenant2_signals = ingest.signals_for_tenant("tenant-2");

        assert_eq!(tenant1_signals.len(), 1, "Tenant 1 should have one signal");
        assert_eq!(tenant2_signals.len(), 1, "Tenant 2 should have one signal");
        assert_eq!(tenant1_signals[0].tenant_id, "tenant-1");
        assert_eq!(tenant2_signals[0].tenant_id, "tenant-2");

        // State: Total is sum of both tenants
        assert_eq!(ingest.signals().len(), 2);
    }

    // Test 7: Duplicate detection is per-tenant
    #[test]
    fn test_duplicate_detection_per_tenant() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let event = json!({"project_id": "same-proj", "service": "compute", "cost": 75.0});

        // Act: Ingest same event for different tenants
        let signal_t1 = ingest.ingest_billing_event("tenant-1", &event).unwrap();
        let signal_t2 = ingest.ingest_billing_event("tenant-2", &event).unwrap();

        // Assert: Both valid (duplicates only within same tenant)
        assert!(signal_t1.is_valid, "First tenant's signal should be valid");
        assert!(signal_t2.is_valid, "Second tenant's signal should be valid");
        assert_ne!(signal_t1.id, signal_t2.id, "Different tenants should have different IDs");

        // State
        assert_eq!(ingest.signals().len(), 2);
        assert_eq!(ingest.valid_signals().len(), 2);
    }

    // Test 8: Multiple missing fields
    #[test]
    fn test_billing_event_multiple_missing_fields() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let sparse_event = json!({"service": "compute"}); // Missing project_id and cost

        // Act
        let signal = ingest.ingest_billing_event("tenant-1", &sparse_event).unwrap();

        // Assert: Multiple error messages
        assert!(!signal.is_valid);
        assert!(signal.validation_errors.len() >= 2, "Should report multiple missing fields");

        let error_text = signal.validation_errors.join(", ");
        assert!(error_text.contains("project_id"), "Should mention project_id");
        assert!(error_text.contains("cost"), "Should mention cost");
    }

    // Test 9: Complex monitoring event with nested structure
    #[test]
    fn test_monitoring_event_with_nested_data() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let complex_event = json!({
            "metric_name": "query_latency_p99",
            "value": 245.67,
            "dimensions": {
                "region": "us-central1",
                "service": "database"
            },
            "labels": ["production", "critical"]
        });

        // Act
        let signal = ingest.ingest_monitoring_event("tenant-3", &complex_event).unwrap();

        // Assert
        assert!(signal.is_valid, "Complex event should be valid");
        assert_eq!(signal.tenant_id, "tenant-3");

        // Verify nested structure preserved
        let payload = &signal.payload;
        assert_eq!(payload.get("metric_name").unwrap().as_str().unwrap(), "query_latency_p99");
        assert_eq!(
            payload.get("dimensions").unwrap().get("region").unwrap().as_str().unwrap(),
            "us-central1"
        );
    }

    // Test 10: Signal count by type and tenant
    #[test]
    fn test_signal_filtering_by_type_and_tenant() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let billing = json!({"project_id": "p1", "service": "compute", "cost": 100.0});
        let monitoring = json!({"metric_name": "cpu", "value": 85.0});
        let logging = json!({"severity": "WARN", "message": "test"});

        // Act: Mix of event types and tenants
        ingest.ingest_billing_event("tenant-1", &billing).unwrap();
        ingest.ingest_monitoring_event("tenant-1", &monitoring).unwrap();
        ingest.ingest_logging_event("tenant-2", &logging).unwrap();

        // Assert: Total and per-tenant counts
        assert_eq!(ingest.signals().len(), 3, "Total should be 3 signals");
        assert_eq!(ingest.signals_for_tenant("tenant-1").len(), 2, "Tenant-1 should have 2");
        assert_eq!(ingest.signals_for_tenant("tenant-2").len(), 1, "Tenant-2 should have 1");

        // All should be valid
        assert_eq!(ingest.valid_signals().len(), 3);
        assert_eq!(ingest.invalid_signals().len(), 0);
    }

    // Test 11: Edge case - empty payload still requires fields
    #[test]
    fn test_empty_billing_event() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let empty_event = json!({});

        // Act
        let signal = ingest.ingest_billing_event("tenant-1", &empty_event).unwrap();

        // Assert
        assert!(!signal.is_valid);
        assert_eq!(signal.validation_errors.len(), 3, "Should have 3 missing fields");

        // State: Signal stored but invalid
        assert_eq!(ingest.signals().len(), 1);
        assert_eq!(ingest.invalid_signals().len(), 1);
    }

    // Test 12: Timestamp is set correctly
    #[test]
    fn test_signal_timestamp_is_set() {
        // Arrange
        let mut ingest = SignalIngest::new();
        let event = json!({"project_id": "p1", "service": "compute", "cost": 50.0});

        // Act
        let signal = ingest.ingest_billing_event("tenant-1", &event).unwrap();

        // Assert: Timestamp is ISO 8601 formatted and current
        assert!(!signal.timestamp.is_empty(), "Timestamp should be set");
        assert!(signal.timestamp.contains("T"), "Timestamp should be ISO 8601");
        assert!(signal.timestamp.contains(":"), "Timestamp should have time component");
    }
}
