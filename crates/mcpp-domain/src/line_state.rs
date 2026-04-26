//! Line state file — typed model for `.chatmangpt/state.yaml`.
//!
//! Promoted from absorbed source `/Users/sac/chatmangpt/mcpp/.chatmangpt/state.yaml`
//! (portfolio-obl-0002, promotion id `line-state-file`).
//!
//! The line state file records the runtime state of the manufacturing line:
//! active obligation, last command, and current operational status.

use mcpp_core::Envelope;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use thiserror::Error;

/// Stable schema identifier for the line state file.
pub const LINE_STATE_SCHEMA: &str = "chatmangpt.mcpp.line-state.v1";

/// Class string emitted in a failure envelope when line state is missing,
/// malformed, or violates a structural invariant.
pub const LINE_STATE_DEFECT_CLASS: &str = "LINE_STATE_DEFECT";

/// Operational status of the manufacturing line.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum LineStatus {
    /// The line is actively manufacturing an obligation.
    Running,
    /// The line has been halted (e.g. by andon activation).
    Stopped,
    /// The line is between obligations — no active work.
    Idle,
}

/// Typed errors for line state operations.
#[derive(Debug, Error)]
pub enum LineStateError {
    #[error("line state file missing at {path}")]
    Missing { path: String },
    #[error("line state at {path} is malformed: {source}")]
    Malformed {
        path: String,
        #[source]
        source: serde_yaml::Error,
    },
    #[error("line state violates invariant: {0}")]
    Invariant(String),
    #[error("line state I/O error at {path}: {source}")]
    Io {
        path: String,
        #[source]
        source: std::io::Error,
    },
}

impl LineStateError {
    /// Convert to a typed JSON failure envelope. Never panics.
    pub fn to_envelope(&self, command: &str, target: &str) -> Envelope {
        Envelope::fail(command, target, LINE_STATE_DEFECT_CLASS, &self.to_string())
    }
}

/// The canonical line state document stored at `.chatmangpt/state.yaml`.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct LineState {
    /// Must equal [`LINE_STATE_SCHEMA`].
    pub schema: String,
    /// Current operational status of the line.
    pub status: LineStatus,
    /// The obligation ID currently being manufactured. Required when
    /// `status == Running`.
    #[serde(default)]
    pub active_obligation: Option<String>,
    /// The last CLI command dispatched on this line.
    #[serde(default)]
    pub last_command: Option<String>,
    /// ISO-8601 timestamp of the last state update, or empty string.
    #[serde(default)]
    pub last_updated: String,
}

impl LineState {
    /// Construct an idle line state (no active obligation).
    pub fn idle() -> Self {
        Self {
            schema: LINE_STATE_SCHEMA.into(),
            status: LineStatus::Idle,
            active_obligation: None,
            last_command: None,
            last_updated: String::new(),
        }
    }

    /// Construct a running line state for the given obligation.
    pub fn running(obligation: &str) -> Self {
        Self {
            schema: LINE_STATE_SCHEMA.into(),
            status: LineStatus::Running,
            active_obligation: Some(obligation.into()),
            last_command: None,
            last_updated: String::new(),
        }
    }

    /// Construct a stopped line state (e.g. after andon activation).
    pub fn stopped() -> Self {
        Self {
            schema: LINE_STATE_SCHEMA.into(),
            status: LineStatus::Stopped,
            active_obligation: None,
            last_command: None,
            last_updated: String::new(),
        }
    }

    /// Load and validate a line state from a YAML file.
    pub fn load(path: &Path) -> Result<Self, LineStateError> {
        if !path.exists() {
            return Err(LineStateError::Missing {
                path: path.display().to_string(),
            });
        }
        let bytes = fs::read(path).map_err(|e| LineStateError::Io {
            path: path.display().to_string(),
            source: e,
        })?;
        let state: LineState =
            serde_yaml::from_slice(&bytes).map_err(|e| LineStateError::Malformed {
                path: path.display().to_string(),
                source: e,
            })?;
        state.validate()?;
        Ok(state)
    }

    /// Save the line state as YAML, creating parent directories as needed.
    pub fn save(&self, path: &Path) -> Result<(), LineStateError> {
        self.validate()?;
        let yaml = serde_yaml::to_string(self).map_err(|e| LineStateError::Malformed {
            path: path.display().to_string(),
            source: e,
        })?;
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).map_err(|e| LineStateError::Io {
                path: parent.display().to_string(),
                source: e,
            })?;
        }
        fs::write(path, yaml).map_err(|e| LineStateError::Io {
            path: path.display().to_string(),
            source: e,
        })
    }

    /// Validate structural invariants. Returns `Err` on first violation.
    ///
    /// Rules:
    /// - `schema` must equal [`LINE_STATE_SCHEMA`]
    /// - if `status == Running`, `active_obligation` must be `Some`
    pub fn validate(&self) -> Result<(), LineStateError> {
        if self.schema != LINE_STATE_SCHEMA {
            return Err(LineStateError::Invariant(format!(
                "schema must be {LINE_STATE_SCHEMA}, got {}",
                self.schema
            )));
        }
        if self.status == LineStatus::Running && self.active_obligation.is_none() {
            return Err(LineStateError::Invariant(
                "status=running requires active_obligation to be set".into(),
            ));
        }
        Ok(())
    }

    /// Default line state path relative to a project root.
    ///
    /// Returns `<root>/.chatmangpt/state.yaml`, matching the absorbed v2 cell.
    pub fn default_path(root: &Path) -> PathBuf {
        root.join(".chatmangpt").join("state.yaml")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn tmp(name: &str) -> PathBuf {
        let mut p = std::env::temp_dir();
        p.push(format!("mcpp-line-state-{}-{}", std::process::id(), name));
        p
    }

    #[test]
    fn idle_state_passes_validation() {
        let state = LineState::idle();
        state.validate().expect("idle must validate");
        assert_eq!(state.status, LineStatus::Idle);
        assert!(state.active_obligation.is_none());
    }

    #[test]
    fn running_with_obligation_passes_validation() {
        let state = LineState::running("portfolio-obl-0002");
        state
            .validate()
            .expect("running with obligation must validate");
        assert_eq!(state.status, LineStatus::Running);
        assert_eq!(
            state.active_obligation.as_deref(),
            Some("portfolio-obl-0002")
        );
    }

    #[test]
    fn running_without_obligation_fails_validation() {
        let state = LineState {
            schema: LINE_STATE_SCHEMA.into(),
            status: LineStatus::Running,
            active_obligation: None,
            last_command: None,
            last_updated: String::new(),
        };
        let err = state
            .validate()
            .expect_err("running without obligation must fail");
        assert!(err.to_string().contains("active_obligation"));
    }

    #[test]
    fn stopped_state_passes_validation() {
        let state = LineState::stopped();
        state.validate().expect("stopped must validate");
        assert_eq!(state.status, LineStatus::Stopped);
    }

    #[test]
    fn wrong_schema_fails_validation() {
        let state = LineState {
            schema: "chatmangpt.mcpp.line-state.v0".into(),
            status: LineStatus::Idle,
            active_obligation: None,
            last_command: None,
            last_updated: String::new(),
        };
        let err = state.validate().expect_err("wrong schema must fail");
        assert!(err.to_string().contains("schema must be"));
    }

    #[test]
    fn round_trip_save_load_idle() {
        let path = tmp("round-trip-idle.yaml");
        let state = LineState::idle();
        state.save(&path).expect("save");
        let loaded = LineState::load(&path).expect("load");
        assert_eq!(state, loaded);
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn round_trip_save_load_running() {
        let path = tmp("round-trip-running.yaml");
        let state = LineState::running("portfolio-obl-0002-promote-v2-constraints");
        state.save(&path).expect("save");
        let loaded = LineState::load(&path).expect("load");
        assert_eq!(state, loaded);
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn missing_file_returns_typed_error() {
        let path = tmp("does-not-exist.yaml");
        let err = LineState::load(&path).expect_err("missing must error");
        assert!(err.to_string().contains("missing"));
        // to_envelope must not panic
        let env = err.to_envelope("mcpp.linestate.load", "mcpp");
        let v: serde_json::Value = serde_json::from_str(&env.to_json()).unwrap();
        assert_eq!(v["errors"][0]["class"], "LINE_STATE_DEFECT");
    }

    #[test]
    fn malformed_yaml_returns_typed_error() {
        let path = tmp("malformed.yaml");
        fs::write(&path, "this: : : is: not: yaml").unwrap();
        let err = LineState::load(&path).expect_err("malformed must error");
        assert!(err.to_string().contains("malformed"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn default_path_produces_expected_suffix() {
        let root = PathBuf::from("/some/project");
        let p = LineState::default_path(&root);
        assert!(p.ends_with(".chatmangpt/state.yaml"));
    }
}
