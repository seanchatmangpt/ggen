//! Fixed snapshot management for cleanroom testing framework
//!
//! This module provides snapshot testing following core team best practices:
//! - Snapshot creation and verification
//! - Snapshot data serialization
//! - Snapshot validation and comparison
//! - Snapshot statistics and reporting
//! - Snapshot cleanup and management

use crate::error::{Result, CleanroomError};
use crate::serializable_instant::SerializableInstant;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Snapshot manager for handling test snapshots
#[derive(Debug)]
pub struct SnapshotManager {
    /// Session ID
    session_id: Uuid,
    /// Snapshots storage
    snapshots: Arc<RwLock<HashMap<String, Snapshot>>>,
    /// Snapshot statistics
    statistics: Arc<RwLock<SnapshotStatistics>>,
    /// Manager enabled status
    enabled: bool,
}

/// Snapshot data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Snapshot {
    /// Snapshot ID
    pub id: Uuid,
    /// Snapshot name
    pub name: String,
    /// Snapshot type
    pub snapshot_type: SnapshotType,
    /// Snapshot data
    pub data: SnapshotData,
    /// Creation timestamp
    pub created_at: SerializableInstant,
    /// Validation status
    pub validation_status: SnapshotValidationStatus,
    /// Hash of snapshot data
    pub hash: String,
    /// Metadata
    pub metadata: HashMap<String, String>,
}

/// Snapshot data content
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotData {
    /// Raw data content
    pub content: String,
    /// Data format (json, yaml, text, etc.)
    pub format: String,
    /// Data size in bytes
    pub size_bytes: usize,
    /// Checksum
    pub checksum: String,
}

/// Snapshot type enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SnapshotType {
    /// JSON snapshot
    Json,
    /// YAML snapshot
    Yaml,
    /// Text snapshot
    Text,
    /// Binary snapshot
    Binary,
    /// HTML snapshot
    Html,
    /// XML snapshot
    Xml,
    /// Custom snapshot type
    Custom(String),
}

/// Snapshot validation status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SnapshotValidationStatus {
    /// Not validated
    NotValidated,
    /// Validation passed
    Validated,
    /// Validation failed
    ValidationFailed,
    /// Validation in progress
    Validating,
}

/// Snapshot statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotStatistics {
    /// Total snapshots created
    pub total_snapshots: usize,
    /// Validated snapshots
    pub validated_snapshots: usize,
    /// Failed validations
    pub failed_validations: usize,
    /// Total data size
    pub total_data_size: usize,
    /// Average snapshot size
    pub average_snapshot_size: f64,
    /// Snapshot types distribution
    pub snapshot_types: HashMap<String, usize>,
}

/// Snapshot report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotReport {
    /// Session ID
    pub session_id: Uuid,
    /// Snapshot statistics
    pub statistics: SnapshotStatistics,
    /// Snapshots list
    pub snapshots: Vec<Snapshot>,
    /// Generated timestamp
    pub generated_at: SerializableInstant,
}

/// Snapshot summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotSummary {
    /// Total snapshots
    pub total_snapshots: usize,
    /// Validated snapshots
    pub validated_snapshots: usize,
    /// Failed validations
    pub failed_validations: usize,
    /// Total data size
    pub total_data_size: usize,
    /// Average snapshot size
    pub average_snapshot_size: f64,
    /// Most common snapshot type
    pub most_common_type: Option<String>,
}

impl SnapshotManager {
    /// Create a new snapshot manager
    pub fn new() -> Self {
        Self::new_with_enabled(true)
    }

    /// Create a new snapshot manager with enabled status
    pub fn new_with_enabled(enabled: bool) -> Self {
        let session_id = Uuid::new_v4();
        let snapshots = Arc::new(RwLock::new(HashMap::new()));
        let statistics = Arc::new(RwLock::new(SnapshotStatistics::new()));

        Self {
            session_id,
            snapshots,
            statistics,
            enabled,
        }
    }

    /// Create a disabled snapshot manager
    pub fn disabled() -> Self {
        Self::new_with_enabled(false)
    }

    /// Check if snapshot manager is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Capture a snapshot
    pub async fn capture_snapshot(
        &self,
        name: String,
        content: String,
        format: String,
        snapshot_type: SnapshotType,
    ) -> Result<Snapshot> {
        if !self.enabled {
            return Err(CleanroomError::snapshot_error("Snapshot manager is disabled"));
        }

        let id = Uuid::new_v4();
        let size_bytes = content.len();
        let checksum = self.calculate_hash(&content);
        let hash = self.calculate_hash(&format!("{}{}{}", name, content, format));

        let snapshot_data = SnapshotData {
            content,
            format,
            size_bytes,
            checksum,
        };

        let snapshot = Snapshot {
            id,
            name: name.clone(),
            snapshot_type,
            data: snapshot_data,
            created_at: SerializableInstant::from(Instant::now()),
            validation_status: SnapshotValidationStatus::NotValidated,
            hash,
            metadata: HashMap::new(),
        };

        // Store snapshot
        {
            let mut snapshots = self.snapshots.write().await;
            snapshots.insert(name.clone(), snapshot.clone());
        }

        // Update statistics
        {
            let mut stats = self.statistics.write().await;
            stats.add_snapshot(&snapshot);
        }

        Ok(snapshot)
    }

    /// Verify a snapshot
    pub async fn verify_snapshot(&self, name: &str, expected_content: &str) -> Result<bool> {
        if !self.enabled {
            return Err(CleanroomError::snapshot_error("Snapshot manager is disabled"));
        }

        let mut snapshots = self.snapshots.write().await;
        if let Some(snapshot) = snapshots.get_mut(name) {
            let matches = snapshot.data.content == expected_content;
            snapshot.validation_status = if matches {
                SnapshotValidationStatus::Validated
            } else {
                SnapshotValidationStatus::ValidationFailed
            };

            // Update statistics
            {
                let mut stats = self.statistics.write().await;
                if matches {
                    stats.validated_snapshots += 1;
                } else {
                    stats.failed_validations += 1;
                }
            }

            Ok(matches)
        } else {
            Err(CleanroomError::snapshot_error("Snapshot not found"))
        }
    }

    /// Get a snapshot by name
    pub async fn get_snapshot(&self, name: &str) -> Result<Snapshot> {
        let snapshots = self.snapshots.read().await;
        snapshots
            .get(name)
            .cloned()
            .ok_or_else(|| CleanroomError::snapshot_error("Snapshot not found"))
    }

    /// Get all snapshots
    pub async fn get_all_snapshots(&self) -> Vec<Snapshot> {
        let snapshots = self.snapshots.read().await;
        snapshots.values().cloned().collect()
    }

    /// Delete a snapshot
    pub async fn delete_snapshot(&self, name: &str) -> Result<()> {
        if !self.enabled {
            return Err(CleanroomError::snapshot_error("Snapshot manager is disabled"));
        }

        let mut snapshots = self.snapshots.write().await;
        if snapshots.remove(name).is_some() {
            // Update statistics
            {
                let mut stats = self.statistics.write().await;
                stats.total_snapshots = stats.total_snapshots.saturating_sub(1);
            }
            Ok(())
        } else {
            Err(CleanroomError::snapshot_error("Snapshot not found"))
        }
    }

    /// Clear all snapshots
    pub async fn clear_snapshots(&self) -> Result<()> {
        if !self.enabled {
            return Err(CleanroomError::snapshot_error("Snapshot manager is disabled"));
        }

        let mut snapshots = self.snapshots.write().await;
        snapshots.clear();

        // Reset statistics
        {
            let mut stats = self.statistics.write().await;
            *stats = SnapshotStatistics::new();
        }

        Ok(())
    }

    /// Get snapshot data
    pub async fn get_snapshot_data(&self, name: &str) -> Result<SnapshotData> {
        let snapshot = self.get_snapshot(name).await?;
        Ok(snapshot.data)
    }

    /// Generate snapshot report
    pub async fn generate_snapshot_report(&self) -> Result<SnapshotReport> {
        let statistics = self.get_statistics().await;
        let snapshots = self.get_all_snapshots().await;

        Ok(SnapshotReport {
            session_id: self.session_id,
            statistics,
            snapshots,
            generated_at: SerializableInstant::from(Instant::now()),
        })
    }

    /// Calculate hash for content
    fn calculate_hash(&self, content: &str) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        content.hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }

    /// Update statistics
    pub async fn update_statistics(&self) -> Result<()> {
        let mut stats = self.statistics.write().await;
        stats.update_statistics();
        Ok(())
    }

    /// Get statistics
    pub async fn get_statistics(&self) -> SnapshotStatistics {
        let stats = self.statistics.read().await;
        stats.clone()
    }

    /// Generate recommendations
    pub async fn generate_recommendations(&self) -> Vec<String> {
        let stats = self.get_statistics().await;
        let mut recommendations = Vec::new();

        if stats.failed_validations > 0 {
            recommendations.push("Some snapshots failed validation. Review and update snapshots as needed.".to_string());
        }

        if stats.average_snapshot_size > 1024.0 * 1024.0 {
            recommendations.push("Average snapshot size is large. Consider optimizing snapshot content.".to_string());
        }

        if stats.total_snapshots > 100 {
            recommendations.push("Large number of snapshots. Consider cleanup of old snapshots.".to_string());
        }

        if stats.validated_snapshots as f64 / stats.total_snapshots as f64 < 0.8 {
            recommendations.push("Low validation rate. Consider improving snapshot quality.".to_string());
        }

        recommendations
    }
}

impl SnapshotStatistics {
    /// Create new snapshot statistics
    pub fn new() -> Self {
        Self {
            total_snapshots: 0,
            validated_snapshots: 0,
            failed_validations: 0,
            total_data_size: 0,
            average_snapshot_size: 0.0,
            snapshot_types: HashMap::new(),
        }
    }

    /// Add snapshot to statistics
    pub fn add_snapshot(&mut self, snapshot: &Snapshot) {
        self.total_snapshots += 1;
        self.total_data_size += snapshot.data.size_bytes;
        self.average_snapshot_size = self.total_data_size as f64 / self.total_snapshots as f64;

        let type_name = match &snapshot.snapshot_type {
            SnapshotType::Json => "json".to_string(),
            SnapshotType::Yaml => "yaml".to_string(),
            SnapshotType::Text => "text".to_string(),
            SnapshotType::Binary => "binary".to_string(),
            SnapshotType::Html => "html".to_string(),
            SnapshotType::Xml => "xml".to_string(),
            SnapshotType::Custom(name) => name.clone(),
        };

        *self.snapshot_types.entry(type_name).or_insert(0) += 1;
    }

    /// Update statistics
    pub fn update_statistics(&mut self) {
        if self.total_snapshots > 0 {
            self.average_snapshot_size = self.total_data_size as f64 / self.total_snapshots as f64;
        }
    }
}

impl SnapshotSummary {
    /// Create new snapshot summary
    pub fn new() -> Self {
        Self {
            total_snapshots: 0,
            validated_snapshots: 0,
            failed_validations: 0,
            total_data_size: 0,
            average_snapshot_size: 0.0,
            most_common_type: None,
        }
    }

    /// Create summary from statistics
    pub fn from_statistics(stats: &SnapshotStatistics) -> Self {
        let most_common_type = stats
            .snapshot_types
            .iter()
            .max_by_key(|(_, count)| *count)
            .map(|(name, _)| name.clone());

        Self {
            total_snapshots: stats.total_snapshots,
            validated_snapshots: stats.validated_snapshots,
            failed_validations: stats.failed_validations,
            total_data_size: stats.total_data_size,
            average_snapshot_size: stats.average_snapshot_size,
            most_common_type,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_snapshot_manager_creation() {
        let manager = SnapshotManager::new();
        assert!(manager.is_enabled());
        assert!(!manager.session_id.is_nil());
    }

    #[test]
    fn test_snapshot_manager_disabled() {
        let manager = SnapshotManager::disabled();
        assert!(!manager.is_enabled());
    }

    #[tokio::test]
    async fn test_capture_snapshot() {
        let manager = SnapshotManager::new();
        
        let snapshot = manager.capture_snapshot(
            "test_snapshot".to_string(),
            r#"{"key": "value"}"#.to_string(),
            "json".to_string(),
            SnapshotType::Json,
        ).await.unwrap();

        assert_eq!(snapshot.name, "test_snapshot");
        assert_eq!(snapshot.data.content, r#"{"key": "value"}"#);
        assert_eq!(snapshot.data.format, "json");
        assert_eq!(snapshot.data.size_bytes, 15);
        assert!(matches!(snapshot.validation_status, SnapshotValidationStatus::NotValidated));
    }

    #[tokio::test]
    async fn test_capture_snapshot_disabled() {
        let manager = SnapshotManager::disabled();
        
        let result = manager.capture_snapshot(
            "test_snapshot".to_string(),
            "content".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await;

        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("disabled"));
    }

    #[tokio::test]
    async fn test_verify_snapshot() {
        let manager = SnapshotManager::new();
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            r#"{"key": "value"}"#.to_string(),
            "json".to_string(),
            SnapshotType::Json,
        ).await.unwrap();

        // Verify with matching content
        let result = manager.verify_snapshot("test_snapshot", r#"{"key": "value"}"#).await.unwrap();
        assert!(result);

        // Verify with non-matching content
        let result = manager.verify_snapshot("test_snapshot", r#"{"key": "different"}"#).await.unwrap();
        assert!(!result);
    }

    #[tokio::test]
    async fn test_verify_snapshot_not_found() {
        let manager = SnapshotManager::new();
        
        let result = manager.verify_snapshot("nonexistent", "content").await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("not found"));
    }

    #[tokio::test]
    async fn test_get_snapshot() {
        let manager = SnapshotManager::new();
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "content".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        // Get snapshot
        let snapshot = manager.get_snapshot("test_snapshot").await.unwrap();
        assert_eq!(snapshot.name, "test_snapshot");
        assert_eq!(snapshot.data.content, "content");

        // Get non-existent snapshot
        let result = manager.get_snapshot("nonexistent").await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_get_all_snapshots() {
        let manager = SnapshotManager::new();
        
        // Capture multiple snapshots
        manager.capture_snapshot(
            "snapshot1".to_string(),
            "content1".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        manager.capture_snapshot(
            "snapshot2".to_string(),
            "content2".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        let snapshots = manager.get_all_snapshots().await;
        assert_eq!(snapshots.len(), 2);
    }

    #[tokio::test]
    async fn test_delete_snapshot() {
        let manager = SnapshotManager::new();
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "content".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        // Delete snapshot
        assert!(manager.delete_snapshot("test_snapshot").await.is_ok());

        // Try to get deleted snapshot
        let result = manager.get_snapshot("test_snapshot").await;
        assert!(result.is_err());

        // Try to delete non-existent snapshot
        let result = manager.delete_snapshot("nonexistent").await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_clear_snapshots() {
        let manager = SnapshotManager::new();
        
        // Capture multiple snapshots
        manager.capture_snapshot(
            "snapshot1".to_string(),
            "content1".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        manager.capture_snapshot(
            "snapshot2".to_string(),
            "content2".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        // Clear all snapshots
        assert!(manager.clear_snapshots().await.is_ok());

        // Verify snapshots are cleared
        let snapshots = manager.get_all_snapshots().await;
        assert_eq!(snapshots.len(), 0);
    }

    #[tokio::test]
    async fn test_get_snapshot_data() {
        let manager = SnapshotManager::new();
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "content".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        // Get snapshot data
        let data = manager.get_snapshot_data("test_snapshot").await.unwrap();
        assert_eq!(data.content, "content");
        assert_eq!(data.format, "text");
        assert_eq!(data.size_bytes, 7);
    }

    #[tokio::test]
    async fn test_generate_snapshot_report() {
        let manager = SnapshotManager::new();
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "content".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        // Generate report
        let report = manager.generate_snapshot_report().await.unwrap();
        assert_eq!(report.session_id, manager.session_id);
        assert_eq!(report.statistics.total_snapshots, 1);
        assert_eq!(report.snapshots.len(), 1);
    }

    #[tokio::test]
    async fn test_update_statistics() {
        let manager = SnapshotManager::new();
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "content".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        // Update statistics
        assert!(manager.update_statistics().await.is_ok());

        let stats = manager.get_statistics().await;
        assert_eq!(stats.total_snapshots, 1);
        assert_eq!(stats.total_data_size, 7);
        assert_eq!(stats.average_snapshot_size, 7.0);
    }

    #[tokio::test]
    async fn test_generate_recommendations() {
        let manager = SnapshotManager::new();
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "content".to_string(),
            "text".to_string(),
            SnapshotType::Text,
        ).await.unwrap();

        // Generate recommendations
        let recommendations = manager.generate_recommendations().await;
        assert!(recommendations.is_empty()); // No issues with single small snapshot
    }

    #[test]
    fn test_snapshot_statistics_creation() {
        let stats = SnapshotStatistics::new();
        assert_eq!(stats.total_snapshots, 0);
        assert_eq!(stats.validated_snapshots, 0);
        assert_eq!(stats.failed_validations, 0);
        assert_eq!(stats.total_data_size, 0);
        assert_eq!(stats.average_snapshot_size, 0.0);
        assert!(stats.snapshot_types.is_empty());
    }

    #[test]
    fn test_snapshot_statistics_add_snapshot() {
        let mut stats = SnapshotStatistics::new();
        let snapshot = Snapshot {
            id: Uuid::new_v4(),
            name: "test".to_string(),
            snapshot_type: SnapshotType::Json,
            data: SnapshotData {
                content: "content".to_string(),
                format: "json".to_string(),
                size_bytes: 7,
                checksum: "hash".to_string(),
            },
            created_at: SerializableInstant::from(Instant::now()),
            validation_status: SnapshotValidationStatus::NotValidated,
            hash: "hash".to_string(),
            metadata: HashMap::new(),
        };

        stats.add_snapshot(&snapshot);
        assert_eq!(stats.total_snapshots, 1);
        assert_eq!(stats.total_data_size, 7);
        assert_eq!(stats.average_snapshot_size, 7.0);
        assert_eq!(stats.snapshot_types.get("json"), Some(&1));
    }

    #[test]
    fn test_snapshot_summary_creation() {
        let summary = SnapshotSummary::new();
        assert_eq!(summary.total_snapshots, 0);
        assert_eq!(summary.validated_snapshots, 0);
        assert_eq!(summary.failed_validations, 0);
        assert_eq!(summary.total_data_size, 0);
        assert_eq!(summary.average_snapshot_size, 0.0);
        assert!(summary.most_common_type.is_none());
    }

    #[test]
    fn test_snapshot_summary_from_statistics() {
        let mut stats = SnapshotStatistics::new();
        stats.total_snapshots = 5;
        stats.validated_snapshots = 4;
        stats.failed_validations = 1;
        stats.total_data_size = 1000;
        stats.average_snapshot_size = 200.0;
        stats.snapshot_types.insert("json".to_string(), 3);
        stats.snapshot_types.insert("text".to_string(), 2);

        let summary = SnapshotSummary::from_statistics(&stats);
        assert_eq!(summary.total_snapshots, 5);
        assert_eq!(summary.validated_snapshots, 4);
        assert_eq!(summary.failed_validations, 1);
        assert_eq!(summary.total_data_size, 1000);
        assert_eq!(summary.average_snapshot_size, 200.0);
        assert_eq!(summary.most_common_type, Some("json".to_string()));
    }

    #[test]
    fn test_snapshot_type_serialization() {
        let types = vec![
            SnapshotType::Json,
            SnapshotType::Yaml,
            SnapshotType::Text,
            SnapshotType::Binary,
            SnapshotType::Html,
            SnapshotType::Xml,
            SnapshotType::Custom("custom".to_string()),
        ];

        for snapshot_type in types {
            let json = serde_json::to_string(&snapshot_type).unwrap();
            let deserialized: SnapshotType = serde_json::from_str(&json).unwrap();
            assert_eq!(snapshot_type, deserialized);
        }
    }

    #[test]
    fn test_snapshot_validation_status_serialization() {
        let statuses = vec![
            SnapshotValidationStatus::NotValidated,
            SnapshotValidationStatus::Validated,
            SnapshotValidationStatus::ValidationFailed,
            SnapshotValidationStatus::Validating,
        ];

        for status in statuses {
            let json = serde_json::to_string(&status).unwrap();
            let deserialized: SnapshotValidationStatus = serde_json::from_str(&json).unwrap();
            assert_eq!(status, deserialized);
        }
    }

    #[test]
    fn test_snapshot_serialization() {
        let snapshot = Snapshot {
            id: Uuid::new_v4(),
            name: "test_snapshot".to_string(),
            snapshot_type: SnapshotType::Json,
            data: SnapshotData {
                content: r#"{"key": "value"}"#.to_string(),
                format: "json".to_string(),
                size_bytes: 15,
                checksum: "hash123".to_string(),
            },
            created_at: SerializableInstant::from(Instant::now()),
            validation_status: SnapshotValidationStatus::Validated,
            hash: "hash456".to_string(),
            metadata: {
                let mut map = HashMap::new();
                map.insert("key1".to_string(), "value1".to_string());
                map
            },
        };

        let json = serde_json::to_string(&snapshot).unwrap();
        let deserialized: Snapshot = serde_json::from_str(&json).unwrap();
        
        assert_eq!(snapshot.id, deserialized.id);
        assert_eq!(snapshot.name, deserialized.name);
        assert_eq!(snapshot.data.content, deserialized.data.content);
        assert_eq!(snapshot.data.format, deserialized.data.format);
        assert_eq!(snapshot.data.size_bytes, deserialized.data.size_bytes);
        assert_eq!(snapshot.validation_status, deserialized.validation_status);
    }

    #[test]
    fn test_snapshot_data_serialization() {
        let data = SnapshotData {
            content: "test content".to_string(),
            format: "text".to_string(),
            size_bytes: 12,
            checksum: "abc123".to_string(),
        };

        let json = serde_json::to_string(&data).unwrap();
        let deserialized: SnapshotData = serde_json::from_str(&json).unwrap();
        
        assert_eq!(data.content, deserialized.content);
        assert_eq!(data.format, deserialized.format);
        assert_eq!(data.size_bytes, deserialized.size_bytes);
        assert_eq!(data.checksum, deserialized.checksum);
    }

    #[test]
    fn test_snapshot_report_serialization() {
        let report = SnapshotReport {
            session_id: Uuid::new_v4(),
            statistics: SnapshotStatistics::new(),
            snapshots: vec![],
            generated_at: SerializableInstant::from(Instant::now()),
        };

        let json = serde_json::to_string(&report).unwrap();
        let deserialized: SnapshotReport = serde_json::from_str(&json).unwrap();
        
        assert_eq!(report.session_id, deserialized.session_id);
        assert_eq!(report.statistics.total_snapshots, deserialized.statistics.total_snapshots);
        assert_eq!(report.snapshots.len(), deserialized.snapshots.len());
    }
}
