//! State Persistence
//!
//! Save and load system state for recovery

use serde::{Deserialize, Serialize};
use std::path::Path;

/// System state snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateSnapshot {
    pub timestamp: String,
    pub domain_statuses: serde_json::Value,
    pub improvements: Vec<crate::improvement::ActionOutcome>,
}

impl StateSnapshot {
    /// Create new state snapshot
    pub fn new(
        domain_statuses: serde_json::Value,
        improvements: Vec<crate::improvement::ActionOutcome>,
    ) -> Self {
        Self {
            timestamp: chrono::Utc::now().to_rfc3339(),
            domain_statuses,
            improvements,
        }
    }

    /// Save snapshot to file
    pub fn save<P: AsRef<Path>>(&self, path: P) -> anyhow::Result<()> {
        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(path, json)?;
        Ok(())
    }

    /// Load snapshot from file
    pub fn load<P: AsRef<Path>>(path: P) -> anyhow::Result<Self> {
        let json = std::fs::read_to_string(path)?;
        let snapshot = serde_json::from_str(&json)?;
        Ok(snapshot)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_snapshot_creation() {
        let statuses = serde_json::json!({"health": 0.8});
        let snapshot = StateSnapshot::new(statuses, vec![]);
        assert!(!snapshot.timestamp.is_empty());
    }

    #[test]
    fn test_snapshot_save_load() {
        let temp_dir = tempfile::tempdir().unwrap();
        let path = temp_dir.path().join("snapshot.json");
        
        let statuses = serde_json::json!({"health": 0.8});
        let snapshot = StateSnapshot::new(statuses.clone(), vec![]);
        
        snapshot.save(&path).unwrap();
        let loaded = StateSnapshot::load(&path).unwrap();
        
        assert_eq!(snapshot.domain_statuses, loaded.domain_statuses);
    }
}
