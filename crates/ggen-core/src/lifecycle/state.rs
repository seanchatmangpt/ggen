//! Lifecycle state management and persistence
//!
//! This module provides state management for lifecycle execution, tracking
//! phase history, generated files, and cache keys for deterministic builds.
//!
//! ## Features
//!
//! - **State persistence**: Save and load lifecycle state from JSON files
//! - **Phase history**: Track all executed phases with timestamps
//! - **Generated file tracking**: Record files created during generation
//! - **Cache key storage**: Store deterministic cache keys for phases
//! - **State queries**: Query state for phase execution history
//!
//! ## Examples
//!
//! ### Loading and Saving State
//!
//! ```rust,no_run
//! use ggen_core::lifecycle::state::{load_state, save_state, LifecycleState};
//! use ggen_core::lifecycle::Result;
//! use std::path::PathBuf;
//!
//! # fn main() -> Result<()> {
//! let state_path = PathBuf::from(".ggen/state.json");
//!
//! // Load existing state or create new
//! let mut state = load_state(&state_path).unwrap_or_default();
//!
//! // Update state
//! state.last_phase = Some("build".to_string());
//!
//! // Save state
//! save_state(&state_path, &state)?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Tracking Phase Execution
//!
//! ```rust,no_run
//! use ggen_core::lifecycle::state::{LifecycleState, RunRecord};
//! use ggen_core::lifecycle::Result;
//! use chrono::Utc;
//!
//! # fn main() -> Result<()> {
//! let mut state = LifecycleState::default();
//!
//! // Record phase execution
//! use std::time::{SystemTime, UNIX_EPOCH};
//! let started_ms = SystemTime::now()
//!     .duration_since(UNIX_EPOCH)
//!     .map_err(|_| ggen_utils::error::Error::new("Time went backwards"))?
//!     .as_millis();
//! state.phase_history.push(RunRecord {
//!     phase: "test".to_string(),
//!     started_ms,
//!     duration_ms: 1500,
//!     success: true,
//! });
//!
//! state.last_phase = Some("test".to_string());
//! # Ok(())
//! # }
//! ```

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

    let content =
        std::fs::read_to_string(path_ref).map_err(|e| LifecycleError::state_load(path_ref, e))?;

    let state: LifecycleState =
        serde_json::from_str(&content).map_err(|e| LifecycleError::state_parse(path_ref, e))?;

    Ok(state)
}

/// Save lifecycle state to .ggen/state.json with atomic write
///
/// PRODUCTION FIX: Uses atomic write pattern (write temp, rename) to prevent corruption
/// in parallel workspace execution
pub fn save_state<P: AsRef<Path>>(path: P, state: &LifecycleState) -> Result<()> {
    let path_ref = path.as_ref();

    // Ensure directory exists
    if let Some(parent) = path_ref.parent() {
        std::fs::create_dir_all(parent).map_err(|e| LifecycleError::DirectoryCreate {
            path: parent.to_path_buf(),
            source: e,
        })?;
    }

    let json = serde_json::to_string_pretty(state)
        .map_err(std::io::Error::other)
        .map_err(|e| LifecycleError::state_save(path_ref, e))?;

    // PRODUCTION FIX: Atomic write pattern prevents corruption from parallel writes
    // Write to temp file, then rename (rename is atomic on POSIX and Windows)
    let temp_path = path_ref.with_extension("json.tmp");
    std::fs::write(&temp_path, json).map_err(|e| LifecycleError::state_save(path_ref, e))?;

    std::fs::rename(&temp_path, path_ref).map_err(|e| LifecycleError::state_save(path_ref, e))?;

    Ok(())
}

impl LifecycleState {
    /// Add a new run record
    pub fn record_run(
        &mut self, phase: String, started_ms: u128, duration_ms: u128, success: bool,
    ) {
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
        self.cache_keys
            .iter()
            .rev()
            .find(|k| k.phase == phase)
            .map(|k| k.key.as_str())
    }

    /// Check if a phase has been completed successfully
    pub fn has_completed_phase(&self, phase: &str) -> bool {
        self.phase_history
            .iter()
            .any(|r| r.phase == phase && r.success)
    }
}

// Unit tests removed - covered by integration_test.rs:
// - test_state_record_run (comprehensive state recording)
// - test_state_add_cache_key (cache key tracking)
// And by behavior_tests.rs state persistence contracts
