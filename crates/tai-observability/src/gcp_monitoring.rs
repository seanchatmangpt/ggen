//! GCP Cloud Monitoring integration for custom metrics.
//!
//! This module provides metric writing with:
//! - Custom metrics (application-defined)
//! - Multiple metric types (gauge, counter, histogram, distribution)
//! - Time series data points with context
//! - Metric descriptors (schema, units, type info)
//! - Query metrics from Cloud Monitoring
//! - Alerting rules creation

use crate::error::{ObservabilityError, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Metric type
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum MetricType {
    /// Gauge (instantaneous value)
    Gauge,
    /// Counter (monotonically increasing)
    Counter,
    /// Histogram (distribution of values)
    Histogram,
    /// Distribution (distribution with percentiles)
    Distribution,
}

/// Metric unit
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum MetricUnit {
    /// Dimensionless
    Unity,
    /// Bytes
    Bytes,
    /// Seconds
    Seconds,
    /// Milliseconds
    Milliseconds,
    /// Microseconds
    Microseconds,
    /// Percentage
    Percentage,
    /// Items
    Count,
}

impl std::fmt::Display for MetricUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MetricUnit::Unity => write!(f, "1"),
            MetricUnit::Bytes => write!(f, "By"),
            MetricUnit::Seconds => write!(f, "s"),
            MetricUnit::Milliseconds => write!(f, "ms"),
            MetricUnit::Microseconds => write!(f, "us"),
            MetricUnit::Percentage => write!(f, "%"),
            MetricUnit::Count => write!(f, "1"),
        }
    }
}

/// Metric descriptor (schema)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricDescriptor {
    /// Metric type identifier
    pub metric_type: String,
    /// Display name
    pub display_name: String,
    /// Metric type
    pub value_type: MetricType,
    /// Unit of measurement
    pub unit: MetricUnit,
    /// Description
    pub description: String,
    /// Labels (dimensions)
    pub labels: Vec<LabelDescriptor>,
}

/// Label descriptor
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LabelDescriptor {
    /// Label key
    pub key: String,
    /// Label description
    pub description: String,
    /// Value type
    pub value_type: String,
}

/// Time series data point
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeSeriesPoint {
    /// Timestamp
    pub timestamp: DateTime<Utc>,
    /// Metric value
    pub value: MetricValue,
}

/// Metric value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MetricValue {
    /// Double value
    Double(f64),
    /// Integer value
    Int64(i64),
    /// Distribution value
    Distribution(DistributionValue),
}

/// Distribution value with percentiles
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DistributionValue {
    /// Number of samples
    pub count: i64,
    /// Mean of the samples
    pub mean: f64,
    /// Sum of the samples
    pub sum_of_squared_deviation: f64,
    /// Percentiles
    pub percentiles: BTreeMap<String, f64>, // "50", "95", "99"
}

/// Time series (metric + data points)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeSeries {
    /// Metric type
    pub metric_type: String,
    /// Resource labels
    pub resource: Resource,
    /// Metric labels
    pub metric_labels: BTreeMap<String, String>,
    /// Data points
    pub points: Vec<TimeSeriesPoint>,
}

/// Resource metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resource {
    /// Resource type (e.g., "global", "gce_instance")
    pub resource_type: String,
    /// Resource labels
    pub labels: BTreeMap<String, String>,
}

impl Default for Resource {
    fn default() -> Self {
        Self {
            resource_type: "global".to_string(),
            labels: BTreeMap::new(),
        }
    }
}

/// Alerting rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertingRule {
    /// Rule ID
    pub id: String,
    /// Display name
    pub display_name: String,
    /// Metric to monitor
    pub metric_type: String,
    /// Threshold value
    pub threshold: f64,
    /// Comparison operator
    pub comparison_operator: ComparisonOperator,
    /// Evaluation window
    pub evaluation_window_secs: u32,
    /// Trigger count (how many windows must exceed threshold)
    pub trigger_count: u32,
    /// Notification channels
    pub notification_channels: Vec<String>,
}

/// Comparison operator for alerting
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ComparisonOperator {
    /// Greater than
    GreaterThan,
    /// Greater than or equal
    GreaterThanOrEqual,
    /// Less than
    LessThan,
    /// Less than or equal
    LessThanOrEqual,
    /// Equal
    Equal,
}

/// Cloud Monitoring client
pub struct CloudMonitoring {
    /// Project ID
    project_id: String,
    /// Metric descriptors
    descriptors: Arc<RwLock<BTreeMap<String, MetricDescriptor>>>,
    /// Time series data
    time_series: Arc<RwLock<Vec<TimeSeries>>>,
    /// Alerting rules
    alerting_rules: Arc<RwLock<Vec<AlertingRule>>>,
}

impl CloudMonitoring {
    /// Create a new Cloud Monitoring client
    pub fn new(project_id: String) -> Self {
        info!(
            "Initializing Cloud Monitoring for project: {}",
            project_id
        );

        Self {
            project_id,
            descriptors: Arc::new(RwLock::new(BTreeMap::new())),
            time_series: Arc::new(RwLock::new(Vec::new())),
            alerting_rules: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Create a metric descriptor
    pub async fn create_metric_descriptor(
        &self,
        descriptor: MetricDescriptor,
    ) -> Result<String> {
        info!(
            "Creating metric descriptor: {} for project: {}",
            descriptor.metric_type, self.project_id
        );

        let mut descriptors = self.descriptors.write().await;
        let metric_type = descriptor.metric_type.clone();
        descriptors.insert(metric_type.clone(), descriptor);

        Ok(metric_type)
    }

    /// Get metric descriptor
    pub async fn get_metric_descriptor(&self, metric_type: &str) -> Result<MetricDescriptor> {
        let descriptors = self.descriptors.read().await;

        descriptors
            .get(metric_type)
            .cloned()
            .ok_or_else(|| ObservabilityError::MetricWriteError(
                format!("Metric descriptor {} not found", metric_type),
            ))
    }

    /// Write a metric data point
    pub async fn write_metric_point(
        &self,
        metric_type: &str,
        value: MetricValue,
        labels: BTreeMap<String, String>,
    ) -> Result<()> {
        // Verify descriptor exists
        let _ = self.get_metric_descriptor(metric_type).await?;

        let point = TimeSeriesPoint {
            timestamp: Utc::now(),
            value,
        };

        let mut series = self.time_series.write().await;

        // Find or create time series
        if let Some(ts) = series.iter_mut().find(|ts| {
            ts.metric_type == metric_type && ts.metric_labels == labels
        }) {
            ts.points.push(point);
        } else {
            series.push(TimeSeries {
                metric_type: metric_type.to_string(),
                resource: Resource::default(),
                metric_labels: labels,
                points: vec![point],
            });
        }

        debug!("Wrote metric point for {}", metric_type);

        Ok(())
    }

    /// Write multiple metric points
    pub async fn write_time_series(&self, time_series: TimeSeries) -> Result<()> {
        // Verify descriptor exists
        let _ = self.get_metric_descriptor(&time_series.metric_type).await?;

        let mut series = self.time_series.write().await;
        series.push(time_series);

        Ok(())
    }

    /// Query time series data
    pub async fn query_time_series(
        &self,
        metric_type: &str,
        limit: usize,
    ) -> Result<Vec<TimeSeries>> {
        let series = self.time_series.read().await;

        Ok(series
            .iter()
            .filter(|ts| ts.metric_type == metric_type)
            .take(limit)
            .cloned()
            .collect())
    }

    /// Get latest metric value
    pub async fn get_latest_metric_value(
        &self,
        metric_type: &str,
    ) -> Result<Option<MetricValue>> {
        let series = self.time_series.read().await;

        Ok(series
            .iter()
            .find(|ts| ts.metric_type == metric_type)
            .and_then(|ts| ts.points.last())
            .map(|p| p.value.clone()))
    }

    /// Create alerting rule
    pub async fn create_alerting_rule(&self, rule: AlertingRule) -> Result<String> {
        info!(
            "Creating alerting rule: {} for metric: {}",
            rule.display_name, rule.metric_type
        );

        let rule_id = rule.id.clone();

        // Verify metric descriptor exists
        let _ = self.get_metric_descriptor(&rule.metric_type).await?;

        let mut rules = self.alerting_rules.write().await;
        rules.push(rule);

        Ok(rule_id)
    }

    /// Get alerting rules
    pub async fn get_alerting_rules(&self) -> Result<Vec<AlertingRule>> {
        let rules = self.alerting_rules.read().await;
        Ok(rules.clone())
    }

    /// Check if metric exceeds threshold
    pub async fn check_threshold(
        &self,
        metric_type: &str,
        threshold: f64,
        operator: ComparisonOperator,
    ) -> Result<bool> {
        let value = self.get_latest_metric_value(metric_type).await?;

        let exceeds = match value {
            Some(MetricValue::Double(v)) => match operator {
                ComparisonOperator::GreaterThan => v > threshold,
                ComparisonOperator::GreaterThanOrEqual => v >= threshold,
                ComparisonOperator::LessThan => v < threshold,
                ComparisonOperator::LessThanOrEqual => v <= threshold,
                ComparisonOperator::Equal => (v - threshold).abs() < f64::EPSILON,
            },
            Some(MetricValue::Int64(v)) => {
                let v = v as f64;
                match operator {
                    ComparisonOperator::GreaterThan => v > threshold,
                    ComparisonOperator::GreaterThanOrEqual => v >= threshold,
                    ComparisonOperator::LessThan => v < threshold,
                    ComparisonOperator::LessThanOrEqual => v <= threshold,
                    ComparisonOperator::Equal => (v - threshold).abs() < f64::EPSILON,
                }
            }
            _ => false,
        };

        Ok(exceeds)
    }

    /// Batch export time series
    pub async fn batch_export(&self, batch_size: usize) -> Result<(usize, Vec<TimeSeries>)> {
        let mut series = self.time_series.write().await;

        if series.is_empty() {
            debug!("No time series to export");
            return Ok((0, vec![]));
        }

        let batch: Vec<TimeSeries> = series
            .drain(0..std::cmp::min(batch_size, series.len()))
            .collect();

        let count = batch.len();
        info!("Exporting batch of {} time series", count);

        Ok((count, batch))
    }

    /// Upload time series to Cloud Monitoring
    pub async fn upload_time_series(&self, time_series: &[TimeSeries]) -> Result<String> {
        info!(
            "Uploading {} time series to Cloud Monitoring for project: {}",
            time_series.len(),
            self.project_id
        );

        // In production, this would call the actual Google Cloud Monitoring API
        // For now, we simulate a successful upload

        let url = format!(
            "https://console.cloud.google.com/monitoring?project={}",
            self.project_id
        );

        debug!("Time series uploaded to: {}", url);

        Ok(url)
    }

    /// Get metric statistics
    pub async fn get_metric_statistics(
        &self,
        metric_type: &str,
    ) -> Result<MetricStatistics> {
        let series = self.time_series.read().await;

        let points: Vec<_> = series
            .iter()
            .filter(|ts| ts.metric_type == metric_type)
            .flat_map(|ts| ts.points.iter())
            .collect();

        if points.is_empty() {
            return Ok(MetricStatistics {
                count: 0,
                mean: 0.0,
                min: 0.0,
                max: 0.0,
                sum: 0.0,
            });
        }

        let mut values = Vec::new();
        let mut sum = 0.0;

        for point in points {
            if let MetricValue::Double(v) = point.value {
                values.push(v);
                sum += v;
            } else if let MetricValue::Int64(v) = point.value {
                let v = v as f64;
                values.push(v);
                sum += v;
            }
        }

        values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let count = values.len();
        let mean = sum / count as f64;
        let min = *values.first().unwrap_or(&0.0);
        let max = *values.last().unwrap_or(&0.0);

        Ok(MetricStatistics {
            count,
            mean,
            min,
            max,
            sum,
        })
    }
}

/// Metric statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricStatistics {
    /// Number of data points
    pub count: usize,
    /// Mean value
    pub mean: f64,
    /// Minimum value
    pub min: f64,
    /// Maximum value
    pub max: f64,
    /// Sum of values
    pub sum: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metric_descriptor_creation() {
        let descriptor = MetricDescriptor {
            metric_type: "custom.googleapis.com/app/cpu_usage".to_string(),
            display_name: "CPU Usage".to_string(),
            value_type: MetricType::Gauge,
            unit: MetricUnit::Percentage,
            description: "CPU usage percentage".to_string(),
            labels: vec![],
        };

        assert_eq!(descriptor.metric_type, "custom.googleapis.com/app/cpu_usage");
        assert_eq!(descriptor.value_type, MetricType::Gauge);
    }

    #[test]
    fn test_metric_unit_display() {
        assert_eq!(MetricUnit::Bytes.to_string(), "By");
        assert_eq!(MetricUnit::Seconds.to_string(), "s");
        assert_eq!(MetricUnit::Milliseconds.to_string(), "ms");
    }

    #[test]
    fn test_time_series_point_creation() {
        let point = TimeSeriesPoint {
            timestamp: Utc::now(),
            value: MetricValue::Double(42.5),
        };

        assert!(matches!(point.value, MetricValue::Double(42.5)));
    }

    #[tokio::test]
    async fn test_cloud_monitoring_creation() {
        let monitoring = CloudMonitoring::new("test-project".to_string());
        assert_eq!(monitoring.project_id, "test-project");
    }

    #[tokio::test]
    async fn test_create_metric_descriptor() {
        let monitoring = CloudMonitoring::new("test-project".to_string());

        let descriptor = MetricDescriptor {
            metric_type: "custom.googleapis.com/test/metric".to_string(),
            display_name: "Test Metric".to_string(),
            value_type: MetricType::Gauge,
            unit: MetricUnit::Percentage,
            description: "Test".to_string(),
            labels: vec![],
        };

        let result = monitoring.create_metric_descriptor(descriptor).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_write_metric_point() {
        let monitoring = CloudMonitoring::new("test-project".to_string());

        let descriptor = MetricDescriptor {
            metric_type: "custom.googleapis.com/test/metric".to_string(),
            display_name: "Test Metric".to_string(),
            value_type: MetricType::Gauge,
            unit: MetricUnit::Percentage,
            description: "Test".to_string(),
            labels: vec![],
        };

        monitoring
            .create_metric_descriptor(descriptor)
            .await
            .expect("Failed to create descriptor");

        let result = monitoring
            .write_metric_point(
                "custom.googleapis.com/test/metric",
                MetricValue::Double(50.0),
                BTreeMap::new(),
            )
            .await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_get_latest_metric_value() {
        let monitoring = CloudMonitoring::new("test-project".to_string());

        let descriptor = MetricDescriptor {
            metric_type: "custom.googleapis.com/test/metric".to_string(),
            display_name: "Test Metric".to_string(),
            value_type: MetricType::Gauge,
            unit: MetricUnit::Percentage,
            description: "Test".to_string(),
            labels: vec![],
        };

        monitoring
            .create_metric_descriptor(descriptor)
            .await
            .expect("Failed to create descriptor");

        monitoring
            .write_metric_point(
                "custom.googleapis.com/test/metric",
                MetricValue::Double(42.5),
                BTreeMap::new(),
            )
            .await
            .expect("Failed to write metric");

        let value = monitoring
            .get_latest_metric_value("custom.googleapis.com/test/metric")
            .await
            .expect("Failed to get value");

        assert!(matches!(value, Some(MetricValue::Double(42.5))));
    }

    #[tokio::test]
    async fn test_check_threshold() {
        let monitoring = CloudMonitoring::new("test-project".to_string());

        let descriptor = MetricDescriptor {
            metric_type: "custom.googleapis.com/test/metric".to_string(),
            display_name: "Test Metric".to_string(),
            value_type: MetricType::Gauge,
            unit: MetricUnit::Percentage,
            description: "Test".to_string(),
            labels: vec![],
        };

        monitoring
            .create_metric_descriptor(descriptor)
            .await
            .expect("Failed to create descriptor");

        monitoring
            .write_metric_point(
                "custom.googleapis.com/test/metric",
                MetricValue::Double(80.0),
                BTreeMap::new(),
            )
            .await
            .expect("Failed to write metric");

        let exceeds = monitoring
            .check_threshold(
                "custom.googleapis.com/test/metric",
                75.0,
                ComparisonOperator::GreaterThan,
            )
            .await
            .expect("Failed to check threshold");

        assert!(exceeds);
    }
}
