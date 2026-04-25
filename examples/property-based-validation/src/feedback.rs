//! Feedback loop for continuous improvement (kaizen)
//!
//! Records validation results and feeds them back into ggen's improvement cycle.

use crate::PropertyValidationResult;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Kaizen feedback loop (Plan-Do-Check-Act)
///
/// Records validation results and provides metrics for continuous improvement.
pub struct FeedbackLoop {
    /// Storage for validation records
    records_path: PathBuf,
}

impl FeedbackLoop {
    /// Create a new feedback loop
    pub fn new() -> Self {
        let records_path = std::path::PathBuf::from(".kaizen-records.jsonl");
        Self { records_path }
    }

    /// Record validation result for kaizen analysis
    ///
    /// # Arguments
    /// * `result` - Validation result to record
    ///
    /// # Kaizen Cycle
    ///
    /// 1. **Plan**: What we wanted to validate (from generation receipt)
    /// 2. **Do**: Ran the 7-property validation
    /// 3. **Check**: Got validation result (passed/failed properties)
    /// 4. **Act**: Fix issues, adjust thresholds, improve process
    pub async fn record_validation(
        &self,
        result: &PropertyValidationResult,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let record = KaizenRecord::from_result(result);

        // Append to records file (JSONL format)
        let json = serde_json::to_string(&record)?;
        let mut file = tokio::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .append(true)
            .open(&self.records_path)
            .await?;

        use tokio::io::AsyncWriteExt;
        file.write_all(json.as_bytes()).await?;
        file.write_all(b"\n").await?;
        file.flush().await?;

        tracing::debug!(
            passed = result.passed_count(),
            failed = result.failed_count(),
            "Recorded kaizen validation"
        );

        Ok(())
    }

    /// Get validation history and trends
    pub async fn get_history(&self) -> Result<Vec<KaizenRecord>, Box<dyn std::error::Error>> {
        if !self.records_path.exists() {
            return Ok(Vec::new());
        }

        let content = tokio::fs::read_to_string(&self.records_path).await?;
        let mut records = Vec::new();

        for line in content.lines() {
            if let Ok(record) = serde_json::from_str::<KaizenRecord>(line) {
                records.push(record);
            }
        }

        Ok(records)
    }

    /// Calculate improvement metrics
    ///
    /// Returns: (pass_rate, avg_failed_properties, trend)
    pub async fn calculate_metrics(&self) -> KaizenMetrics {
        let history = self.get_history().await.unwrap_or_default();

        if history.is_empty() {
            return KaizenMetrics::default();
        }

        let total = history.len();
        let passed = history.iter().filter(|r| r.all_passed).count();

        // Calculate average failed properties per validation
        let failed_counts: Vec<usize> = history
            .iter()
            .map(|r| r.failed_properties_count)
            .collect();
        let avg_failed = if failed_counts.is_empty() {
            0.0
        } else {
            failed_counts.iter().sum::<usize>() as f64 / failed_counts.len() as f64
        };

        // Calculate trend (last 10 validations)
        let recent = history.iter().rev().take(10).collect::<Vec<_>>();
        let recent_pass_rate = if recent.is_empty() {
            0.0
        } else {
            recent.iter().filter(|r| r.all_passed).count() as f64 / recent.len() as f64
        };

        KaizenMetrics {
            total_validations: total,
            pass_rate: passed as f64 / total as f64,
            avg_failed_properties: avg_failed,
            recent_trend: recent_pass_rate,
        }
    }
}

/// Kaizen record (Plan-Do-Check-Act)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KaizenRecord {
    /// Timestamp of validation
    pub timestamp: chrono::DateTime<chrono::Utc>,

    /// Generation receipt (BLAKE3 hash)
    pub generation_receipt: String,

    /// Whether all 7 properties passed
    pub all_passed: bool,

    /// Number of properties that passed (0-7)
    pub passed_properties_count: usize,

    /// Number of properties that failed (0-7)
    pub failed_properties_count: usize,

    /// Which properties failed (list of property types)
    pub failed_properties: Vec<String>,

    /// Total violations found
    pub total_violations: usize,

    /// Check duration in milliseconds
    pub total_duration_ms: u64,
}

impl KaizenRecord {
    /// Create kaizen record from validation result
    pub fn from_result(result: &PropertyValidationResult) -> Self {
        let failed_properties: Vec<String> = result
            .failed_properties()
            .iter()
            .map(|p| format!("{:?}", p))
            .collect();

        KaizenRecord {
            timestamp: chrono::Utc::now(),
            generation_receipt: "placeholder-receipt".to_string(), // TODO: get from result
            all_passed: result.all_passed(),
            passed_properties_count: result.passed_count(),
            failed_properties_count: result.failed_count(),
            failed_properties,
            total_violations: result.violations().len(),
            total_duration_ms: 0, // TODO: sum from result.checks
        }
    }
}

/// Kaizen improvement metrics
#[derive(Debug, Clone, Default)]
pub struct KaizenMetrics {
    /// Total number of validations performed
    pub total_validations: usize,

    /// Overall pass rate (0.0 - 1.0)
    pub pass_rate: f64,

    /// Average number of failed properties per validation
    pub avg_failed_properties: f64,

    /// Recent trend (last 10 validations pass rate)
    pub recent_trend: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_feedback_loop_creation() {
        let feedback = FeedbackLoop::new();
        assert_eq!(feedback.records_path, PathBuf::from(".kaizen-records.jsonl"));
    }

    #[tokio::test]
    async fn test_record_and_retrieve() {
        let feedback = FeedbackLoop::new();

        // Create a mock validation result
        let result = PropertyValidationResult {
            checks: vec![],
        };

        // Record it
        feedback.record_validation(&result).await.unwrap();

        // Retrieve history
        let history = feedback.get_history().await.unwrap();
        assert_eq!(history.len(), 1);
    }

    #[tokio::test]
    async fn test_metrics_calculation() {
        let feedback = FeedbackLoop::new();
        let metrics = feedback.calculate_metrics().await;

        // Empty history should return zeros
        assert_eq!(metrics.total_validations, 0);
        assert_eq!(metrics.pass_rate, 0.0);
    }
}
