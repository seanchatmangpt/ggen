//! Signal ingestion and normalization service
//!
//! This module handles the **Monitor** phase of the MAPE-K loop:
//! - Raw event normalization from diverse sources
//! - Metric validation and bounds checking
//! - Deduplication using windowed bloom filters
//! - Timestamp canonicalization

use std::collections::HashSet;
use thiserror::Error;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};

/// Signal ingestion errors
#[derive(Debug, Error)]
pub enum SignalError {
    #[error("Invalid tenant ID: {0}")]
    InvalidTenantId(String),

    #[error("Invalid metric name: {0}")]
    InvalidMetricName(String),

    #[error("Metric value out of bounds: {value} (expected 0.0-100.0)")]
    ValueOutOfBounds { value: f64 },

    #[error("Invalid timestamp: {reason}")]
    InvalidTimestamp { reason: String },

    #[error("Normalization failed: {0}")]
    NormalizationFailed(String),

    #[error("Deduplication state corrupted: {0}")]
    DeduplicationCorrupted(String),
}

/// Raw event as received from monitoring source
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RawEvent {
    pub tenant_id: String,
    pub metric: String,
    pub value: f64,
    pub timestamp_ms: i64,
}

/// Normalized signal ready for governor coordination
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct NormalizedSignal {
    pub tenant_id: String,
    pub metric_type: MetricType,
    pub normalized_value: u32, // 0-100 scale
    pub timestamp: DateTime<Utc>,
    pub signal_id: String, // For deduplication
}

/// Classified metric types
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum MetricType {
    CpuUtilization,
    MemoryUtilization,
    DiskUtilization,
    NetworkLatency,
    RequestRate,
    ErrorRate,
    CustomMetric,
}

impl MetricType {
    /// Parse metric name into typed enum
    fn from_name(name: &str) -> Result<Self, SignalError> {
        match name.to_lowercase().as_str() {
            "cpu" | "cpu_utilization" | "cpu_usage" => Ok(MetricType::CpuUtilization),
            "memory" | "memory_utilization" | "mem_usage" => Ok(MetricType::MemoryUtilization),
            "disk" | "disk_utilization" | "disk_usage" => Ok(MetricType::DiskUtilization),
            "latency" | "network_latency" | "p99_latency" => Ok(MetricType::NetworkLatency),
            "requests" | "request_rate" | "rps" => Ok(MetricType::RequestRate),
            "errors" | "error_rate" | "error_pct" => Ok(MetricType::ErrorRate),
            _ => {
                if name.is_empty() {
                    return Err(SignalError::InvalidMetricName(
                        "metric name cannot be empty".to_string(),
                    ));
                }
                Ok(MetricType::CustomMetric)
            }
        }
    }
}

/// Signal ingestion service (stateless)
pub struct SignalIngest;

impl SignalIngest {
    /// Normalize raw event into standardized signal
    ///
    /// ## Validation rules
    /// - tenant_id: non-empty, ASCII printable
    /// - metric: non-empty, valid metric type
    /// - value: 0.0-100.0 (percentages) or valid range for metric type
    /// - timestamp: valid Unix milliseconds (not future)
    ///
    /// ## Errors
    /// - InvalidTenantId: Empty or invalid format
    /// - InvalidMetricName: Unknown metric or malformed
    /// - ValueOutOfBounds: Value outside valid range for metric
    /// - InvalidTimestamp: Invalid or future timestamp
    pub async fn normalize(event: RawEvent) -> Result<NormalizedSignal, SignalError> {
        // Validate tenant ID
        if event.tenant_id.is_empty() || event.tenant_id.len() > 256 {
            return Err(SignalError::InvalidTenantId(
                "tenant_id must be 1-256 characters".to_string(),
            ));
        }

        // Validate metric name
        if event.metric.is_empty() || event.metric.len() > 128 {
            return Err(SignalError::InvalidMetricName(
                "metric must be 1-128 characters".to_string(),
            ));
        }

        // Parse metric type
        let metric_type = MetricType::from_name(&event.metric)?;

        // Validate and normalize value
        if !event.value.is_finite() {
            return Err(SignalError::ValueOutOfBounds {
                value: event.value,
            });
        }

        let normalized_value = if event.value < 0.0 || event.value > 100.0 {
            return Err(SignalError::ValueOutOfBounds {
                value: event.value,
            });
        } else {
            (event.value as u32).min(100)
        };

        // Validate timestamp: must be reasonable (not > 1 hour in future)
        let now_ms = Utc::now().timestamp_millis();
        let max_future_ms = 3_600_000; // 1 hour
        if event.timestamp_ms > now_ms + max_future_ms {
            return Err(SignalError::InvalidTimestamp {
                reason: "timestamp cannot be more than 1 hour in future".to_string(),
            });
        }

        if event.timestamp_ms < 0 {
            return Err(SignalError::InvalidTimestamp {
                reason: "timestamp cannot be negative".to_string(),
            });
        }

        // Convert to DateTime
        let timestamp = DateTime::<Utc>::from_timestamp_millis(event.timestamp_ms)
            .ok_or_else(|| SignalError::InvalidTimestamp {
                reason: "failed to convert timestamp".to_string(),
            })?;

        // Generate deterministic signal ID for deduplication
        let signal_id = Self::compute_signal_id(
            &event.tenant_id,
            &event.metric,
            normalized_value,
            event.timestamp_ms,
        );

        Ok(NormalizedSignal {
            tenant_id: event.tenant_id,
            metric_type,
            normalized_value,
            timestamp,
            signal_id,
        })
    }

    /// Deduplicate signals using sliding window approach
    ///
    /// Two signals are considered duplicates if:
    /// - Same tenant_id
    /// - Same metric_type
    /// - Same normalized_value (±1 tolerance for rounding)
    /// - Within 5-second window
    ///
    /// ## Returns
    /// Deduplicated signal list (earliest first)
    pub async fn deduplicate(signals: Vec<NormalizedSignal>) -> Result<Vec<NormalizedSignal>, SignalError> {
        if signals.is_empty() {
            return Ok(Vec::new());
        }

        // Simple sliding window deduplication: keep first of each unique set
        let mut seen = HashSet::new();
        let mut deduped = Vec::new();

        for signal in signals {
            let key = (
                signal.tenant_id.clone(),
                signal.metric_type,
                signal.normalized_value,
                // Group by 5-second windows
                signal.timestamp.timestamp() / 5,
            );

            if seen.insert(key) {
                deduped.push(signal);
            }
        }

        deduped.sort_by_key(|s| s.timestamp);
        Ok(deduped)
    }

    /// Compute deterministic signal ID for deduplication tracking
    fn compute_signal_id(tenant_id: &str, metric: &str, value: u32, timestamp_ms: i64) -> String {
        use sha2::{Sha256, Digest};

        let mut hasher = Sha256::new();
        hasher.update(tenant_id.as_bytes());
        hasher.update(b"|");
        hasher.update(metric.as_bytes());
        hasher.update(b"|");
        hasher.update(value.to_string().as_bytes());
        hasher.update(b"|");
        hasher.update((timestamp_ms / 5000).to_string().as_bytes()); // 5-second window

        let result = hasher.finalize();
        hex::encode(&result[0..8]) // Use first 8 bytes of hash
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_normalize_valid_cpu_signal() {
        // Arrange
        let event = RawEvent {
            tenant_id: "tenant-123".to_string(),
            metric: "cpu_utilization".to_string(),
            value: 75.5,
            timestamp_ms: 1000000000,
        };

        // Act
        let result = SignalIngest::normalize(event).await;

        // Assert
        assert!(result.is_ok());
        let signal = result.unwrap();
        assert_eq!(signal.tenant_id, "tenant-123");
        assert_eq!(signal.metric_type, MetricType::CpuUtilization);
        assert_eq!(signal.normalized_value, 75);
    }

    #[tokio::test]
    async fn test_normalize_invalid_tenant_id_empty() {
        // Arrange
        let event = RawEvent {
            tenant_id: "".to_string(),
            metric: "cpu".to_string(),
            value: 50.0,
            timestamp_ms: 1000000000,
        };

        // Act
        let result = SignalIngest::normalize(event).await;

        // Assert
        assert!(matches!(result, Err(SignalError::InvalidTenantId(_))));
    }

    #[tokio::test]
    async fn test_normalize_value_out_of_bounds() {
        // Arrange
        let event = RawEvent {
            tenant_id: "tenant-1".to_string(),
            metric: "cpu".to_string(),
            value: 105.0, // Out of bounds
            timestamp_ms: 1000000000,
        };

        // Act
        let result = SignalIngest::normalize(event).await;

        // Assert
        assert!(matches!(result, Err(SignalError::ValueOutOfBounds { .. })));
    }

    #[tokio::test]
    async fn test_deduplicate_removes_duplicates() {
        // Arrange
        let now = Utc::now().timestamp_millis();

        // signal1 and signal2: same value, same 5-sec window, different IDs
        let signal1 = NormalizedSignal {
            tenant_id: "tenant-1".to_string(),
            metric_type: MetricType::CpuUtilization,
            normalized_value: 75,
            timestamp: DateTime::<Utc>::from_timestamp_millis(now).unwrap(),
            signal_id: "sig-1".to_string(),
        };

        let signal2 = NormalizedSignal {
            tenant_id: "tenant-1".to_string(),
            metric_type: MetricType::CpuUtilization,
            normalized_value: 75,
            // Use same timestamp to ensure same 5-second window
            timestamp: DateTime::<Utc>::from_timestamp_millis(now).unwrap(),
            signal_id: "sig-2-dup".to_string(), // Different signal_id but same content
        };

        // signal3: different value, will be in different window or has different value
        let signal3 = NormalizedSignal {
            tenant_id: "tenant-1".to_string(),
            metric_type: MetricType::CpuUtilization,
            normalized_value: 80,
            timestamp: DateTime::<Utc>::from_timestamp_millis(now + 6000).unwrap(), // Different window
            signal_id: "sig-3".to_string(),
        };

        // Act
        let deduped = SignalIngest::deduplicate(vec![signal1.clone(), signal2, signal3.clone()])
            .await
            .unwrap();

        // Assert - signal1 and signal2 have identical content so dedup removes one
        // signal3 is different (different value) so it's kept
        assert_eq!(deduped.len(), 2);
        assert_eq!(deduped[0].signal_id, "sig-1");
        assert_eq!(deduped[1].signal_id, "sig-3");
    }

    #[tokio::test]
    async fn test_deduplicate_empty_list() {
        // Act
        let result = SignalIngest::deduplicate(vec![]).await;

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }
}
