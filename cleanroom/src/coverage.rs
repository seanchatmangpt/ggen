//! Coverage collection for cleanroom testing

use crate::error::Result;
use crate::report::CoverageData;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use uuid::Uuid;

/// Container coverage data
#[derive(Debug, Clone)]
pub struct ContainerCoverageData {
    /// Path to coverage file
    pub path: PathBuf,
    /// Container ID
    pub container_id: String,
}

/// Coverage collector for cleanroom testing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageCollector {
    /// Session ID
    session_id: Uuid,
    /// Coverage data
    coverage_data: Option<CoverageData>,
}

impl CoverageCollector {
    /// Create a new coverage collector
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            coverage_data: None,
        }
    }

    /// Create a new coverage collector with auto-generated session ID
    pub fn new_default() -> Self {
        Self {
            session_id: Uuid::new_v4(),
            coverage_data: None,
        }
    }

    /// Set session ID
    pub fn with_session_id(mut self, session_id: Uuid) -> Self {
        self.session_id = session_id;
        self
    }

    /// Start coverage collection
    pub fn start_collection(&mut self) -> Result<()> {
        // Initialize coverage data
        self.coverage_data = Some(CoverageData::new());
        Ok(())
    }

    /// Stop coverage collection
    pub fn stop_collection(&mut self) -> Result<CoverageData> {
        if let Some(data) = self.coverage_data.take() {
            Ok(data)
        } else {
            Err(crate::error::CleanroomError::new(
                crate::error::ErrorKind::InternalError,
                "Coverage collection was not started".to_string(),
            ))
        }
    }

    /// Get current coverage data
    pub fn get_coverage_data(&self) -> Option<&CoverageData> {
        self.coverage_data.as_ref()
    }

    /// Update coverage data
    pub fn update_coverage_data<F>(&mut self, updater: F) -> Result<()>
    where
        F: FnOnce(&mut CoverageData),
    {
        if let Some(data) = &mut self.coverage_data {
            updater(data);
            Ok(())
        } else {
            Err(crate::error::CleanroomError::new(
                crate::error::ErrorKind::InternalError,
                "Coverage collection was not started".to_string(),
            ))
        }
    }

    /// Get session ID
    pub fn session_id(&self) -> Uuid {
        self.session_id
    }

    /// Check if collection is active
    pub fn is_collecting(&self) -> bool {
        self.coverage_data.is_some()
    }

    /// Collect coverage data from a container
    pub fn collect_from_container(&self, container_id: &str) -> Result<ContainerCoverageData> {
        // In a real implementation, this would extract coverage data from the container
        // For now, we'll create a mock implementation
        let temp_path = std::env::temp_dir().join(format!("coverage_{}.profraw", container_id));
        
        // Create a dummy coverage file
        std::fs::write(&temp_path, b"dummy coverage data").map_err(|e| {
            crate::error::CleanroomError::new(
                crate::error::ErrorKind::IoError,
                format!("Failed to create coverage file: {}", e)
            )
        })?;

        Ok(ContainerCoverageData {
            path: temp_path,
            container_id: container_id.to_string(),
        })
    }
}

impl Default for CoverageCollector {
    fn default() -> Self {
        Self::new_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_coverage_collector_creation() {
        let session_id = Uuid::new_v4();
        let collector = CoverageCollector::new(session_id);
        assert_eq!(collector.session_id(), session_id);
        assert!(!collector.is_collecting());
    }

    #[test]
    fn test_start_collection() {
        let session_id = Uuid::new_v4();
        let mut collector = CoverageCollector::new(session_id);
        
        assert!(collector.start_collection().is_ok());
        assert!(collector.is_collecting());
        assert!(collector.get_coverage_data().is_some());
    }

    #[test]
    fn test_stop_collection() {
        let session_id = Uuid::new_v4();
        let mut collector = CoverageCollector::new(session_id);
        
        collector.start_collection().unwrap();
        let data = collector.stop_collection().unwrap();
        assert!(!collector.is_collecting());
        assert_eq!(data.overall_coverage_percentage, 0.0);
    }

    #[test]
    fn test_update_coverage_data() {
        let session_id = Uuid::new_v4();
        let mut collector = CoverageCollector::new(session_id);
        
        collector.start_collection().unwrap();
        collector.update_coverage_data(|data| {
            data.overall_coverage_percentage = 85.5;
        }).unwrap();
        
        let data = collector.get_coverage_data().unwrap();
        assert_eq!(data.overall_coverage_percentage, 85.5);
    }

    #[test]
    fn test_coverage_data_new() {
        let session_id = Uuid::new_v4();
        let data = CoverageData::new();
        
        assert_eq!(data.session_id, session_id);
        assert_eq!(data.overall_coverage_percentage, 0.0);
        assert_eq!(data.line_coverage_percentage, 0.0);
        assert_eq!(data.branch_coverage_percentage, 0.0);
        assert_eq!(data.function_coverage_percentage, 0.0);
        assert!(data.coverage_files.is_empty());
        assert!(data.uncovered_lines.is_empty());
        assert!(data.uncovered_branches.is_empty());
        assert!(data.uncovered_functions.is_empty());
    }

    #[test]
    fn test_coverage_data_serialization() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new();
        data.overall_coverage_percentage = 75.5;
        data.line_coverage_percentage = 80.0;
        data.branch_coverage_percentage = 70.0;
        data.function_coverage_percentage = 85.0;
        
        let json = serde_json::to_string(&data).unwrap();
        let deserialized: CoverageData = serde_json::from_str(&json).unwrap();
        
        assert_eq!(data.session_id, deserialized.session_id);
        assert_eq!(data.overall_coverage_percentage, deserialized.overall_coverage_percentage);
        assert_eq!(data.line_coverage_percentage, deserialized.line_coverage_percentage);
        assert_eq!(data.branch_coverage_percentage, deserialized.branch_coverage_percentage);
        assert_eq!(data.function_coverage_percentage, deserialized.function_coverage_percentage);
    }

    #[test]
    fn test_coverage_data_add_file() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new();
        
        data.add_file("src/main.rs".to_string(), 100, 80);
        data.add_file("src/lib.rs".to_string(), 200, 160);
        
        assert_eq!(data.coverage_files.len(), 2);
        assert_eq!(data.coverage_files.get("src/main.rs"), Some(&(100, 80)));
        assert_eq!(data.coverage_files.get("src/lib.rs"), Some(&(200, 160)));
    }

    #[test]
    fn test_coverage_data_add_uncovered_line() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new();
        
        data.add_uncovered_line("src/main.rs".to_string(), 42);
        data.add_uncovered_line("src/main.rs".to_string(), 45);
        data.add_uncovered_line("src/lib.rs".to_string(), 10);
        
        assert_eq!(data.uncovered_lines.len(), 2);
        assert_eq!(data.uncovered_lines.get("src/main.rs"), Some(&vec![42, 45]));
        assert_eq!(data.uncovered_lines.get("src/lib.rs"), Some(&vec![10]));
    }

    #[test]
    fn test_coverage_data_add_uncovered_branch() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new();
        
        data.add_uncovered_branch("src/main.rs".to_string(), "if condition".to_string());
        data.add_uncovered_branch("src/lib.rs".to_string(), "match arm".to_string());
        
        assert_eq!(data.uncovered_branches.len(), 2);
        assert_eq!(data.uncovered_branches.get("src/main.rs"), Some(&vec!["if condition".to_string()]));
        assert_eq!(data.uncovered_branches.get("src/lib.rs"), Some(&vec!["match arm".to_string()]));
    }

    #[test]
    fn test_coverage_data_add_uncovered_function() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new();
        
        data.add_uncovered_function("src/main.rs".to_string(), "test_function".to_string());
        data.add_uncovered_function("src/lib.rs".to_string(), "helper_function".to_string());
        
        assert_eq!(data.uncovered_functions.len(), 2);
        assert_eq!(data.uncovered_functions.get("src/main.rs"), Some(&vec!["test_function".to_string()]));
        assert_eq!(data.uncovered_functions.get("src/lib.rs"), Some(&vec!["helper_function".to_string()]));
    }

    #[test]
    fn test_coverage_data_calculate_overall_coverage() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new();
        
        data.add_file("src/main.rs".to_string(), 100, 80); // 80% coverage
        data.add_file("src/lib.rs".to_string(), 200, 100); // 50% coverage
        
        data.calculate_overall_coverage();
        
        // Overall coverage should be weighted average: (80 + 100) / (100 + 200) = 60%
        assert_eq!(data.overall_coverage_percentage, 60.0);
    }

    #[test]
    fn test_coverage_data_get_summary() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new();
        
        data.overall_coverage_percentage = 85.5;
        data.line_coverage_percentage = 90.0;
        data.branch_coverage_percentage = 80.0;
        data.function_coverage_percentage = 95.0;
        
        let summary = data.get_summary();
        
        assert!(summary.contains("Coverage Summary"));
        assert!(summary.contains("85.5%"));
        assert!(summary.contains("90.0%"));
        assert!(summary.contains("80.0%"));
        assert!(summary.contains("95.0%"));
    }

    #[test]
    fn test_coverage_data_get_summary_with_files() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new();
        
        data.add_file("src/main.rs".to_string(), 100, 80);
        data.add_file("src/lib.rs".to_string(), 200, 160);
        data.add_uncovered_line("src/main.rs".to_string(), 42);
        data.add_uncovered_branch("src/lib.rs".to_string(), "if condition".to_string());
        data.add_uncovered_function("src/main.rs".to_string(), "test_function".to_string());
        
        let summary = data.get_summary();
        
        assert!(summary.contains("Coverage Summary"));
        assert!(summary.contains("src/main.rs"));
        assert!(summary.contains("src/lib.rs"));
        assert!(summary.contains("Uncovered Lines"));
        assert!(summary.contains("Uncovered Branches"));
        assert!(summary.contains("Uncovered Functions"));
    }

    #[test]
    fn test_coverage_collector_new() {
        let session_id = Uuid::new_v4();
        let collector = CoverageCollector::new(session_id);
        
        assert_eq!(collector.session_id, session_id);
        assert!(!collector.is_collecting());
        assert!(collector.get_coverage_data().is_none()); // Should fail when not collecting
    }

    #[test]
    fn test_coverage_collector_start_collection_already_collecting() {
        let session_id = Uuid::new_v4();
        let mut collector = CoverageCollector::new(session_id);
        
        collector.start_collection().unwrap();
        let result = collector.start_collection();
        
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(e.message.contains("already collecting"));
        }
    }

    #[test]
    fn test_coverage_collector_stop_collection_not_collecting() {
        let session_id = Uuid::new_v4();
        let mut collector = CoverageCollector::new(session_id);
        
        let result = collector.stop_collection();
        
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(e.message.contains("not collecting"));
        }
    }

    #[test]
    fn test_coverage_collector_update_coverage_data_not_collecting() {
        let session_id = Uuid::new_v4();
        let mut collector = CoverageCollector::new(session_id);
        
        let result = collector.update_coverage_data(|_| {});
        
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(e.message.contains("not collecting"));
        }
    }

    #[test]
    fn test_coverage_collector_get_coverage_data_not_collecting() {
        let session_id = Uuid::new_v4();
        let collector = CoverageCollector::new(session_id);
        
        let result = collector.get_coverage_data();
        
        assert!(result.is_none());
    }

    #[test]
    fn test_coverage_collector_multiple_updates() {
        let session_id = Uuid::new_v4();
        let mut collector = CoverageCollector::new(session_id);
        
        collector.start_collection().unwrap();
        
        // First update
        collector.update_coverage_data(|data| {
            data.overall_coverage_percentage = 50.0;
            data.add_file("src/main.rs".to_string(), 100, 50);
        }).unwrap();
        
        // Second update
        collector.update_coverage_data(|data| {
            data.overall_coverage_percentage = 75.0;
            data.add_file("src/lib.rs".to_string(), 200, 150);
        }).unwrap();
        
        let data = collector.get_coverage_data().unwrap();
        assert_eq!(data.overall_coverage_percentage, 75.0);
        assert_eq!(data.coverage_files.len(), 2);
        assert_eq!(data.coverage_files.get("src/main.rs"), Some(&(100, 50)));
        assert_eq!(data.coverage_files.get("src/lib.rs"), Some(&(200, 150)));
    }

    #[test]
    fn test_coverage_collector_serialization() {
        let session_id = Uuid::new_v4();
        let mut collector = CoverageCollector::new(session_id);
        
        collector.start_collection().unwrap();
        collector.update_coverage_data(|data| {
            data.overall_coverage_percentage = 85.5;
            data.line_coverage_percentage = 90.0;
        }).unwrap();
        
        let json = serde_json::to_string(&collector).unwrap();
        let deserialized: CoverageCollector = serde_json::from_str(&json).unwrap();
        
        assert_eq!(collector.session_id, deserialized.session_id);
        assert_eq!(collector.is_collecting(), deserialized.is_collecting());
    }

    #[test]
    fn test_coverage_collector_debug() {
        let session_id = Uuid::new_v4();
        let collector = CoverageCollector::new(session_id);
        let debug_str = format!("{:?}", collector);
        
        assert!(debug_str.contains("CoverageCollector"));
        assert!(debug_str.contains("session_id"));
    }

    #[test]
    fn test_coverage_data_debug() {
        let session_id = Uuid::new_v4();
        let data = CoverageData::new();
        let debug_str = format!("{:?}", data);
        
        assert!(debug_str.contains("CoverageData"));
        assert!(debug_str.contains("session_id"));
    }

    #[test]
    fn test_coverage_data_clone() {
        let session_id = Uuid::new_v4();
        let mut data1 = CoverageData::new();
        data1.overall_coverage_percentage = 75.5;
        data1.add_file("src/main.rs".to_string(), 100, 80);
        
        let data2 = data1.clone();
        
        assert_eq!(data1.session_id, data2.session_id);
        assert_eq!(data1.overall_coverage_percentage, data2.overall_coverage_percentage);
        assert_eq!(data1.coverage_files, data2.coverage_files);
    }

    #[test]
    fn test_coverage_collector_clone() {
        let session_id = Uuid::new_v4();
        let collector1 = CoverageCollector::new(session_id);
        let collector2 = collector1.clone();
        
        assert_eq!(collector1.session_id, collector2.session_id);
        assert_eq!(collector1.is_collecting(), collector2.is_collecting());
    }
}