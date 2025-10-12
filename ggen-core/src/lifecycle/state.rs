//! Lifecycle state management and persistence

use super::error::{LifecycleError, Result};
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Persistent state for lifecycle execution
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LifecycleState {
    /// Last executed phase
    pub last_phase: Option<String>,

    /// History of phase executions
    pub phase_history: Vec<RunRecord>,

    /// Generated files tracked
    pub generated: Vec<GeneratedFile>,

    /// Cache keys for deterministic builds
    pub cache_keys: Vec<CacheKey>,
}

/// Record of a single phase execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunRecord {
    pub phase: String,
    pub started_ms: u128,
    pub duration_ms: u128,
    pub success: bool,
}

/// Tracked generated file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratedFile {
    pub path: String,
    pub hash: String,
}

/// Cache key for a phase
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheKey {
    pub phase: String,
    pub key: String,
}

/// Load lifecycle state from .ggen/state.json
///
/// Returns an error if the file exists but cannot be read or parsed.
/// Returns a default state if the file doesn't exist (first run).
pub fn load_state<P: AsRef<Path>>(path: P) -> Result<LifecycleState> {
    let path_ref = path.as_ref();

    if !path_ref.exists() {
        // First run - no state file yet, return default
        return Ok(LifecycleState::default());
    }

    let content = std::fs::read_to_string(path_ref)
        .map_err(|e| LifecycleError::state_load(path_ref, e))?;

    let state: LifecycleState = serde_json::from_str(&content)
        .map_err(|e| LifecycleError::state_parse(path_ref, e))?;

    Ok(state)
}

/// Save lifecycle state to .ggen/state.json
pub fn save_state<P: AsRef<Path>>(path: P, state: &LifecycleState) -> Result<()> {
    let path_ref = path.as_ref();

    // Ensure directory exists
    if let Some(parent) = path_ref.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| LifecycleError::DirectoryCreate {
                path: parent.to_path_buf(),
                source: e,
            })?;
    }

    let json = serde_json::to_string_pretty(state)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
        .map_err(|e| LifecycleError::state_save(path_ref, e))?;

    std::fs::write(path_ref, json)
        .map_err(|e| LifecycleError::state_save(path_ref, e))?;

    Ok(())
}

impl LifecycleState {
    /// Add a new run record
    pub fn record_run(&mut self, phase: String, started_ms: u128, duration_ms: u128, success: bool) {
        self.phase_history.push(RunRecord {
            phase: phase.clone(),
            started_ms,
            duration_ms,
            success,
        });
        self.last_phase = Some(phase);
    }

    /// Add a cache key
    pub fn add_cache_key(&mut self, phase: String, key: String) {
        self.cache_keys.push(CacheKey { phase, key });
    }

    /// Get last run record for a phase
    pub fn last_run(&self, phase: &str) -> Option<&RunRecord> {
        self.phase_history.iter().rev().find(|r| r.phase == phase)
    }

    /// Get cache key for a phase
    pub fn get_cache_key(&self, phase: &str) -> Option<&str> {
        self.cache_keys.iter().rev().find(|k| k.phase == phase).map(|k| k.key.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_record() {
        let mut state = LifecycleState::default();
        state.record_run("build".to_string(), 1000, 500, true);

        assert_eq!(state.last_phase, Some("build".to_string()));
        assert_eq!(state.phase_history.len(), 1);
    }
}
