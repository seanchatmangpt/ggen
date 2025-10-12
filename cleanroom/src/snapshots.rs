//! Snapshot testing for cleanroom testing
//!
//! This module provides snapshot testing following core team best practices:
//! - Snapshot capture and comparison
//! - Snapshot management
//! - Snapshot validation
//! - Snapshot reporting

use crate::error::{Result, CleanroomError};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Snapshot manager for cleanroom testing
#[derive(Debug)]
pub struct SnapshotManager {
    /// Session ID
    session_id: Uuid,
    /// Snapshot data
    snapshot_data: Arc<RwLock<SnapshotData>>,
    /// Enabled flag
    enabled: bool,
}

/// Snapshot data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotData {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Snapshots
    pub snapshots: HashMap<String, Snapshot>,
    /// Snapshot statistics
    pub statistics: SnapshotStatistics,
}

/// Snapshot structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Snapshot {
    /// Snapshot name
    pub name: String,
    /// Snapshot content
    pub content: String,
    /// Snapshot hash
    pub hash: String,
    /// Creation time
    pub creation_time: Instant,
    /// Snapshot type
    pub snapshot_type: SnapshotType,
    /// Metadata
    pub metadata: HashMap<String, String>,
    /// Validation status
    pub validation_status: SnapshotValidationStatus,
}

/// Snapshot type enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SnapshotType {
    /// Text snapshot
    Text,
    /// JSON snapshot
    Json,
    /// XML snapshot
    Xml,
    /// Binary snapshot
    Binary,
    /// HTML snapshot
    Html,
    /// Custom snapshot
    Custom(String),
}

/// Snapshot validation status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SnapshotValidationStatus {
    /// Snapshot is valid
    Valid,
    /// Snapshot is invalid
    Invalid(String),
    /// Snapshot is pending validation
    Pending,
    /// Snapshot is new
    New,
}

/// Snapshot statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotStatistics {
    /// Total snapshots
    pub total_snapshots: u32,
    /// Valid snapshots
    pub valid_snapshots: u32,
    /// Invalid snapshots
    pub invalid_snapshots: u32,
    /// New snapshots
    pub new_snapshots: u32,
    /// Pending snapshots
    pub pending_snapshots: u32,
    /// Total size in bytes
    pub total_size_bytes: u64,
    /// Average size in bytes
    pub average_size_bytes: u64,
}

impl SnapshotManager {
    /// Create a new snapshot manager
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            snapshot_data: Arc::new(RwLock::new(SnapshotData::new(session_id))),
            enabled: true,
        }
    }
    
    /// Create a disabled snapshot manager
    pub fn disabled() -> Self {
        Self {
            session_id: Uuid::new_v4(),
            snapshot_data: Arc::new(RwLock::new(SnapshotData::new(Uuid::new_v4()))),
            enabled: false,
        }
    }
    
    /// Check if snapshot testing is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
    
    /// Capture a snapshot
    pub async fn capture_snapshot(
        &self,
        name: String,
        content: String,
        snapshot_type: SnapshotType,
        metadata: HashMap<String, String>,
    ) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let hash = self.calculate_hash(&content);
        let snapshot = Snapshot {
            name: name.clone(),
            content,
            hash,
            creation_time: Instant::now(),
            snapshot_type,
            metadata,
            validation_status: SnapshotValidationStatus::New,
        };
        
        let mut data = self.snapshot_data.write().await;
        data.snapshots.insert(name, snapshot);
        self.update_statistics(&mut data).await;
        
        Ok(())
    }
    
    /// Validate a snapshot
    pub async fn validate_snapshot(&self, name: &str, expected_content: &str) -> Result<bool> {
        if !self.enabled {
            return Ok(true);
        }
        
        let mut data = self.snapshot_data.write().await;
        if let Some(snapshot) = data.snapshots.get_mut(name) {
            let expected_hash = self.calculate_hash(expected_content);
            
            if snapshot.hash == expected_hash {
                snapshot.validation_status = SnapshotValidationStatus::Valid;
                self.update_statistics(&mut data).await;
                Ok(true)
            } else {
                let error_message = format!("Snapshot '{}' validation failed: hash mismatch", name);
                snapshot.validation_status = SnapshotValidationStatus::Invalid(error_message.clone());
                self.update_statistics(&mut data).await;
                Err(CleanroomError::snapshot_error(error_message))
            }
        } else {
            Err(CleanroomError::snapshot_error(format!("Snapshot '{}' not found", name)))
        }
    }
    
    /// Get a snapshot
    pub async fn get_snapshot(&self, name: &str) -> Result<Option<Snapshot>> {
        if !self.enabled {
            return Ok(None);
        }
        
        let data = self.snapshot_data.read().await;
        Ok(data.snapshots.get(name).cloned())
    }
    
    /// Get all snapshots
    pub async fn get_all_snapshots(&self) -> Result<HashMap<String, Snapshot>> {
        if !self.enabled {
            return Ok(HashMap::new());
        }
        
        let data = self.snapshot_data.read().await;
        Ok(data.snapshots.clone())
    }
    
    /// Delete a snapshot
    pub async fn delete_snapshot(&self, name: &str) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut data = self.snapshot_data.write().await;
        if data.snapshots.remove(name).is_some() {
            self.update_statistics(&mut data).await;
            Ok(())
        } else {
            Err(CleanroomError::snapshot_error(format!("Snapshot '{}' not found", name)))
        }
    }
    
    /// Clear all snapshots
    pub async fn clear_snapshots(&self) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut data = self.snapshot_data.write().await;
        data.snapshots.clear();
        self.update_statistics(&mut data).await;
        Ok(())
    }
    
    /// Get snapshot data
    pub async fn get_snapshot_data(&self) -> SnapshotData {
        let data = self.snapshot_data.read().await;
        data.clone()
    }
    
    /// Generate snapshot report
    pub async fn generate_snapshot_report(&self) -> Result<SnapshotReport> {
        if !self.enabled {
            return Err(CleanroomError::snapshot_error("Snapshot testing is disabled"));
        }
        
        let data = self.snapshot_data.read().await;
        
        let mut report = SnapshotReport {
            session_id: data.session_id,
            start_time: data.start_time,
            end_time: data.end_time,
            statistics: data.statistics.clone(),
            snapshots: Vec::new(),
            recommendations: Vec::new(),
        };
        
        // Generate snapshot summaries
        for (name, snapshot) in &data.snapshots {
            report.snapshots.push(SnapshotSummary {
                name: name.clone(),
                snapshot_type: snapshot.snapshot_type.clone(),
                hash: snapshot.hash.clone(),
                creation_time: snapshot.creation_time,
                validation_status: snapshot.validation_status.clone(),
                size_bytes: snapshot.content.len() as u64,
                metadata: snapshot.metadata.clone(),
            });
        }
        
        // Generate recommendations
        self.generate_recommendations(&data, &mut report).await;
        
        Ok(report)
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
    async fn update_statistics(&self, data: &mut SnapshotData) {
        let mut stats = SnapshotStatistics {
            total_snapshots: 0,
            valid_snapshots: 0,
            invalid_snapshots: 0,
            new_snapshots: 0,
            pending_snapshots: 0,
            total_size_bytes: 0,
            average_size_bytes: 0,
        };
        
        for snapshot in data.snapshots.values() {
            stats.total_snapshots += 1;
            stats.total_size_bytes += snapshot.content.len() as u64;
            
            match snapshot.validation_status {
                SnapshotValidationStatus::Valid => stats.valid_snapshots += 1,
                SnapshotValidationStatus::Invalid(_) => stats.invalid_snapshots += 1,
                SnapshotValidationStatus::New => stats.new_snapshots += 1,
                SnapshotValidationStatus::Pending => stats.pending_snapshots += 1,
            }
        }
        
        if stats.total_snapshots > 0 {
            stats.average_size_bytes = stats.total_size_bytes / stats.total_snapshots as u64;
        }
        
        data.statistics = stats;
    }
    
    /// Generate recommendations
    async fn generate_recommendations(&self, data: &SnapshotData, report: &mut SnapshotReport) {
        // Invalid snapshots recommendation
        if data.statistics.invalid_snapshots > 0 {
            report.recommendations.push(format!(
                "{} snapshots are invalid, review and update them",
                data.statistics.invalid_snapshots
            ));
        }
        
        // New snapshots recommendation
        if data.statistics.new_snapshots > 0 {
            report.recommendations.push(format!(
                "{} new snapshots detected, consider reviewing them",
                data.statistics.new_snapshots
            ));
        }
        
        // Pending snapshots recommendation
        if data.statistics.pending_snapshots > 0 {
            report.recommendations.push(format!(
                "{} snapshots are pending validation, complete validation",
                data.statistics.pending_snapshots
            ));
        }
        
        // Large snapshots recommendation
        if data.statistics.average_size_bytes > 1024 * 1024 { // 1MB
            report.recommendations.push(format!(
                "Average snapshot size is {} bytes, consider optimizing large snapshots",
                data.statistics.average_size_bytes
            ));
        }
    }
}

impl SnapshotData {
    /// Create new snapshot data
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            start_time: Instant::now(),
            end_time: None,
            snapshots: HashMap::new(),
            statistics: SnapshotStatistics {
                total_snapshots: 0,
                valid_snapshots: 0,
                invalid_snapshots: 0,
                new_snapshots: 0,
                pending_snapshots: 0,
                total_size_bytes: 0,
                average_size_bytes: 0,
            },
        }
    }
}

/// Snapshot report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotReport {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Statistics
    pub statistics: SnapshotStatistics,
    /// Snapshots
    pub snapshots: Vec<SnapshotSummary>,
    /// Recommendations
    pub recommendations: Vec<String>,
}

/// Snapshot summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotSummary {
    /// Snapshot name
    pub name: String,
    /// Snapshot type
    pub snapshot_type: SnapshotType,
    /// Snapshot hash
    pub hash: String,
    /// Creation time
    pub creation_time: Instant,
    /// Validation status
    pub validation_status: SnapshotValidationStatus,
    /// Size in bytes
    pub size_bytes: u64,
    /// Metadata
    pub metadata: HashMap<String, String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_snapshot_manager_creation() {
        let session_id = Uuid::new_v4();
        let manager = SnapshotManager::new(session_id);
        assert!(manager.is_enabled());
        assert_eq!(manager.session_id, session_id);
    }
    
    #[tokio::test]
    async fn test_snapshot_manager_disabled() {
        let manager = SnapshotManager::disabled();
        assert!(!manager.is_enabled());
    }
    
    #[tokio::test]
    async fn test_capture_snapshot() {
        let session_id = Uuid::new_v4();
        let manager = SnapshotManager::new(session_id);
        
        let mut metadata = HashMap::new();
        metadata.insert("test".to_string(), "value".to_string());
        
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "test content".to_string(),
            SnapshotType::Text,
            metadata,
        ).await.unwrap();
        
        let data = manager.get_snapshot_data().await;
        assert_eq!(data.snapshots.len(), 1);
        assert!(data.snapshots.contains_key("test_snapshot"));
        
        let snapshot = &data.snapshots["test_snapshot"];
        assert_eq!(snapshot.name, "test_snapshot");
        assert_eq!(snapshot.content, "test content");
        assert!(matches!(snapshot.validation_status, SnapshotValidationStatus::New));
    }
    
    #[tokio::test]
    async fn test_validate_snapshot() {
        let session_id = Uuid::new_v4();
        let manager = SnapshotManager::new(session_id);
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "test content".to_string(),
            SnapshotType::Text,
            HashMap::new(),
        ).await.unwrap();
        
        // Validate with same content
        let result = manager.validate_snapshot("test_snapshot", "test content").await;
        assert!(result.is_ok());
        assert!(result.unwrap());
        
        // Validate with different content
        let result = manager.validate_snapshot("test_snapshot", "different content").await;
        assert!(result.is_err());
    }
    
    #[tokio::test]
    async fn test_get_snapshot() {
        let session_id = Uuid::new_v4();
        let manager = SnapshotManager::new(session_id);
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "test content".to_string(),
            SnapshotType::Text,
            HashMap::new(),
        ).await.unwrap();
        
        // Get snapshot
        let snapshot = manager.get_snapshot("test_snapshot").await.unwrap();
        assert!(snapshot.is_some());
        
        let snapshot = snapshot.unwrap();
        assert_eq!(snapshot.name, "test_snapshot");
        assert_eq!(snapshot.content, "test content");
    }
    
    #[tokio::test]
    async fn test_delete_snapshot() {
        let session_id = Uuid::new_v4();
        let manager = SnapshotManager::new(session_id);
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "test content".to_string(),
            SnapshotType::Text,
            HashMap::new(),
        ).await.unwrap();
        
        // Delete snapshot
        manager.delete_snapshot("test_snapshot").await.unwrap();
        
        // Verify deletion
        let snapshot = manager.get_snapshot("test_snapshot").await.unwrap();
        assert!(snapshot.is_none());
    }
    
    #[tokio::test]
    async fn test_snapshot_report() {
        let session_id = Uuid::new_v4();
        let manager = SnapshotManager::new(session_id);
        
        // Capture snapshot
        manager.capture_snapshot(
            "test_snapshot".to_string(),
            "test content".to_string(),
            SnapshotType::Text,
            HashMap::new(),
        ).await.unwrap();
        
        // Generate report
        let report = manager.generate_snapshot_report().await.unwrap();
        assert_eq!(report.session_id, session_id);
        assert_eq!(report.snapshots.len(), 1);
        assert_eq!(report.snapshots[0].name, "test_snapshot");
    }
}