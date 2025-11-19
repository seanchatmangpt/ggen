//! Genchi Genbutsu - Reality verification through audit trails
//!
//! This module implements comprehensive audit trails and change tracking,
//! following the TPS principle of "go and see" (verify the actual situation).

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Type of audit action
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AuditAction {
    /// Code generation
    Generate {
        /// Whether this was a dry run
        dry_run: bool,
    },
    /// Regeneration after delta
    Regenerate {
        /// Number of templates regenerated
        template_count: usize,
    },
    /// Rollback to previous state
    Rollback {
        /// Checkpoint ID
        checkpoint_id: String,
    },
    /// Validation
    Validate {
        /// Number of issues found
        issue_count: usize,
    },
}

/// Execution status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExecutionStatus {
    /// Completed successfully
    Success,
    /// Failed with error
    Failed {
        /// Error message
        error: String,
    },
    /// Partially successful (some warnings)
    PartialSuccess {
        /// Warning count
        warning_count: usize,
    },
}

/// A single file change record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileChange {
    /// Path to the changed file
    pub path: PathBuf,
    /// Type of change
    pub change_type: FileChangeType,
    /// Lines added
    pub lines_added: usize,
    /// Lines removed
    pub lines_removed: usize,
}

/// Type of file change
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FileChangeType {
    /// File created
    Created,
    /// File modified
    Modified,
    /// File deleted
    Deleted,
}

/// An audit log entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEntry {
    /// Unique entry ID
    pub id: String,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Actor (username or service)
    pub actor: String,
    /// Action performed
    pub action: AuditAction,
    /// Ontology version
    pub ontology_version: String,
    /// Templates used
    pub template_set: Vec<String>,
    /// Execution status
    pub status: ExecutionStatus,
    /// Files changed
    pub changes: Vec<FileChange>,
}

impl AuditEntry {
    /// Create a new audit entry
    pub fn new(
        actor: String,
        action: AuditAction,
        ontology_version: String,
        template_set: Vec<String>,
        status: ExecutionStatus,
        changes: Vec<FileChange>,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            timestamp: chrono::Utc::now(),
            actor,
            action,
            ontology_version,
            template_set,
            status,
            changes,
        }
    }

    /// Check if this entry represents a failure
    pub fn is_failure(&self) -> bool {
        matches!(self.status, ExecutionStatus::Failed { .. })
    }

    /// Get total lines changed
    pub fn total_lines_changed(&self) -> usize {
        self.changes
            .iter()
            .map(|c| c.lines_added + c.lines_removed)
            .sum()
    }
}

/// Audit log manager
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditLog {
    /// All audit entries
    entries: Vec<AuditEntry>,
    /// Retention period in days
    retention_days: u32,
}

impl AuditLog {
    /// Create a new audit log
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            retention_days: 90,
        }
    }

    /// Load from JSON file
    pub fn load(path: &PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::new(&format!("Failed to read audit log: {}", e)))?;
        serde_json::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse audit log: {}", e)))
    }

    /// Save to JSON file
    pub fn save(&self, path: &PathBuf) -> Result<()> {
        let content = serde_json::to_string_pretty(self)
            .map_err(|e| Error::new(&format!("Failed to serialize audit log: {}", e)))?;

        // Create parent directory if it doesn't exist
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| Error::new(&format!("Failed to create audit log directory: {}", e)))?;
        }

        std::fs::write(path, content)
            .map_err(|e| Error::new(&format!("Failed to write audit log: {}", e)))
    }

    /// Append a new entry
    pub fn append(&mut self, entry: AuditEntry) {
        self.entries.push(entry);
        self.cleanup_old_entries();
    }

    /// Query entries by actor
    pub fn by_actor(&self, actor: &str) -> Vec<&AuditEntry> {
        self.entries
            .iter()
            .filter(|e| e.actor == actor)
            .collect()
    }

    /// Query entries by date range
    pub fn by_date_range(
        &self,
        start: chrono::DateTime<chrono::Utc>,
        end: chrono::DateTime<chrono::Utc>,
    ) -> Vec<&AuditEntry> {
        self.entries
            .iter()
            .filter(|e| e.timestamp >= start && e.timestamp <= end)
            .collect()
    }

    /// Get all failures
    pub fn failures(&self) -> Vec<&AuditEntry> {
        self.entries.iter().filter(|e| e.is_failure()).collect()
    }

    /// Clean up entries older than retention period
    fn cleanup_old_entries(&mut self) {
        let cutoff = chrono::Utc::now() - chrono::Duration::days(self.retention_days as i64);
        self.entries.retain(|e| e.timestamp >= cutoff);
    }

    /// Get statistics
    pub fn stats(&self) -> AuditStats {
        let total = self.entries.len();
        let failures = self.failures().len();
        let total_lines: usize = self.entries.iter().map(|e| e.total_lines_changed()).sum();

        AuditStats {
            total_entries: total,
            failure_count: failures,
            success_rate: if total > 0 {
                ((total - failures) as f64 / total as f64) * 100.0
            } else {
                100.0
            },
            total_lines_changed: total_lines,
        }
    }
}

impl Default for AuditLog {
    fn default() -> Self {
        Self::new()
    }
}

/// Audit log statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditStats {
    /// Total number of entries
    pub total_entries: usize,
    /// Number of failures
    pub failure_count: usize,
    /// Success rate percentage
    pub success_rate: f64,
    /// Total lines changed across all entries
    pub total_lines_changed: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    test!(test_audit_entry_creation, {
        let entry = AuditEntry::new(
            "alice@example.com".to_string(),
            AuditAction::Generate { dry_run: false },
            "1.0.0".to_string(),
            vec!["template1.rs".to_string()],
            ExecutionStatus::Success,
            vec![FileChange {
                path: PathBuf::from("src/main.rs"),
                change_type: FileChangeType::Modified,
                lines_added: 10,
                lines_removed: 5,
            }],
        );

        assert_eq!(entry.actor, "alice@example.com");
        assert!(!entry.is_failure());
        assert_eq!(entry.total_lines_changed(), 15);

        Ok(())
    });

    test!(test_audit_log, {
        let mut log = AuditLog::new();

        let entry = AuditEntry::new(
            "bob@example.com".to_string(),
            AuditAction::Generate { dry_run: false },
            "1.0.0".to_string(),
            vec![],
            ExecutionStatus::Success,
            vec![],
        );

        log.append(entry);

        assert_eq!(log.entries.len(), 1);

        let by_actor = log.by_actor("bob@example.com");
        assert_eq!(by_actor.len(), 1);

        Ok(())
    });

    test!(test_audit_stats, {
        let mut log = AuditLog::new();

        // Add successful entry
        log.append(AuditEntry::new(
            "user1".to_string(),
            AuditAction::Generate { dry_run: false },
            "1.0".to_string(),
            vec![],
            ExecutionStatus::Success,
            vec![],
        ));

        // Add failed entry
        log.append(AuditEntry::new(
            "user2".to_string(),
            AuditAction::Generate { dry_run: false },
            "1.0".to_string(),
            vec![],
            ExecutionStatus::Failed {
                error: "test error".to_string(),
            },
            vec![],
        ));

        let stats = log.stats();
        assert_eq!(stats.total_entries, 2);
        assert_eq!(stats.failure_count, 1);
        assert_eq!(stats.success_rate, 50.0);

        Ok(())
    });
}
