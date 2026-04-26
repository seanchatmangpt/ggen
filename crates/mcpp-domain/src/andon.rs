//! Andon state file — typed model for `.chatmangpt/andon.yaml`.
//!
//! Promoted from absorbed source `/Users/sac/chatmangpt/mcpp/.chatmangpt/andon.yaml`
//! (portfolio-obl-0002, promotion id `andon-state-file`).
//!
//! The andon state file records whether the manufacturing line is stopped
//! due to an abnormality. When active, at least one entry with a canonical
//! class must be present. Reading this file detects line-stopped state.

use mcpp_core::Envelope;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use thiserror::Error;

/// Stable schema identifier for the andon state file.
pub const ANDON_STATE_SCHEMA: &str = "chatmangpt.mcpp.andon.v1";

/// Class string emitted in a failure envelope when andon state is missing,
/// malformed, or violates a structural invariant.
pub const ANDON_DEFECT_CLASS: &str = "ANDON_DEFECT";

/// Active abnormality classes (subset of canonical andon classes).
/// These are the only valid `class` values for [`AndonEntry`].
pub const ANDON_ACTIVE_CLASSES: &[&str] = &[
    "SPEC_DEFECT",
    "PLAN_DEFECT",
    "TASK_DEFECT",
    "IMPLEMENTATION_DEFECT",
    "PRESET_DEFECT",
    "INVOCATION_DEFECT",
    "RECEIPT_DEFECT",
    "AGENT_ROUTING_DEFECT",
    "POLICY_DEFECT",
    "ENVIRONMENT_DEFECT",
];

/// Typed errors for andon state operations.
#[derive(Debug, Error)]
pub enum AndonError {
    #[error("andon state file missing at {path}")]
    Missing { path: String },
    #[error("andon state at {path} is malformed: {source}")]
    Malformed {
        path: String,
        #[source]
        source: serde_yaml::Error,
    },
    #[error("andon state violates invariant: {0}")]
    Invariant(String),
    #[error("andon state I/O error at {path}: {source}")]
    Io {
        path: String,
        #[source]
        source: std::io::Error,
    },
}

impl AndonError {
    /// Convert to a typed JSON failure envelope. Never panics.
    pub fn to_envelope(&self, command: &str, target: &str) -> Envelope {
        Envelope::fail(command, target, ANDON_DEFECT_CLASS, &self.to_string())
    }
}

/// A single abnormality entry recorded in the andon state.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct AndonEntry {
    /// Must be one of [`ANDON_ACTIVE_CLASSES`].
    pub class: String,
    /// Human-readable description of the abnormality.
    pub message: String,
    /// ISO-8601 timestamp or `"unknown"` when not available.
    pub raised_at: String,
    /// Optional: identifier for the component that raised the andon.
    #[serde(default)]
    pub source: String,
}

/// The canonical andon state document stored at `.chatmangpt/andon.yaml`.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct AndonState {
    /// Must equal [`ANDON_STATE_SCHEMA`].
    pub schema: String,
    /// `true` when the line is stopped due to an abnormality.
    pub active: bool,
    /// Non-empty when `active == true`.
    #[serde(default)]
    pub entries: Vec<AndonEntry>,
}

impl AndonState {
    /// Construct an inactive (all-clear) andon state.
    pub fn inactive() -> Self {
        Self {
            schema: ANDON_STATE_SCHEMA.into(),
            active: false,
            entries: vec![],
        }
    }

    /// Construct an active andon state with a single entry.
    ///
    /// `class` should be one of [`ANDON_ACTIVE_CLASSES`]; if not,
    /// [`validate()`](Self::validate) will reject it.
    pub fn raise_andon(class: &str, message: &str) -> Self {
        Self {
            schema: ANDON_STATE_SCHEMA.into(),
            active: true,
            entries: vec![AndonEntry {
                class: class.into(),
                message: message.into(),
                raised_at: "unknown".into(),
                source: String::new(),
            }],
        }
    }

    /// Load and validate an andon state from a YAML file.
    pub fn load(path: &Path) -> Result<Self, AndonError> {
        if !path.exists() {
            return Err(AndonError::Missing {
                path: path.display().to_string(),
            });
        }
        let bytes = fs::read(path).map_err(|e| AndonError::Io {
            path: path.display().to_string(),
            source: e,
        })?;
        let state: AndonState =
            serde_yaml::from_slice(&bytes).map_err(|e| AndonError::Malformed {
                path: path.display().to_string(),
                source: e,
            })?;
        state.validate()?;
        Ok(state)
    }

    /// Save the andon state as YAML, creating parent directories as needed.
    pub fn save(&self, path: &Path) -> Result<(), AndonError> {
        self.validate()?;
        let yaml = serde_yaml::to_string(self).map_err(|e| AndonError::Malformed {
            path: path.display().to_string(),
            source: e,
        })?;
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).map_err(|e| AndonError::Io {
                path: parent.display().to_string(),
                source: e,
            })?;
        }
        fs::write(path, yaml).map_err(|e| AndonError::Io {
            path: path.display().to_string(),
            source: e,
        })
    }

    /// Validate structural invariants. Returns `Err` on first violation.
    ///
    /// Rules:
    /// - `schema` must equal [`ANDON_STATE_SCHEMA`]
    /// - if `active == true`, `entries` must be non-empty
    /// - every entry `class` must be in [`ANDON_ACTIVE_CLASSES`]
    pub fn validate(&self) -> Result<(), AndonError> {
        if self.schema != ANDON_STATE_SCHEMA {
            return Err(AndonError::Invariant(format!(
                "schema must be {ANDON_STATE_SCHEMA}, got {}",
                self.schema
            )));
        }
        if self.active && self.entries.is_empty() {
            return Err(AndonError::Invariant(
                "active andon state must have at least one entry".into(),
            ));
        }
        for entry in &self.entries {
            if !ANDON_ACTIVE_CLASSES.contains(&entry.class.as_str()) {
                return Err(AndonError::Invariant(format!(
                    "entry class '{}' is not a canonical andon class",
                    entry.class
                )));
            }
        }
        Ok(())
    }

    /// Default andon state path relative to a project root.
    ///
    /// Returns `<root>/.chatmangpt/andon.yaml`, matching the absorbed v2 cell.
    pub fn default_path(root: &Path) -> PathBuf {
        root.join(".chatmangpt").join("andon.yaml")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn tmp(name: &str) -> PathBuf {
        let mut p = std::env::temp_dir();
        p.push(format!("mcpp-andon-{}-{}", std::process::id(), name));
        p
    }

    #[test]
    fn inactive_state_passes_validation() {
        let state = AndonState::inactive();
        state.validate().expect("inactive must validate");
        assert!(!state.active);
        assert!(state.entries.is_empty());
    }

    #[test]
    fn active_state_with_valid_class_passes_validation() {
        let state = AndonState::raise_andon("SPEC_DEFECT", "spec is missing acceptance criteria");
        state.validate().expect("valid active andon must validate");
        assert!(state.active);
        assert_eq!(state.entries.len(), 1);
        assert_eq!(state.entries[0].class, "SPEC_DEFECT");
    }

    #[test]
    fn active_state_with_empty_entries_fails_validation() {
        let state = AndonState {
            schema: ANDON_STATE_SCHEMA.into(),
            active: true,
            entries: vec![],
        };
        let err = state
            .validate()
            .expect_err("active with no entries must fail");
        assert!(err.to_string().contains("at least one entry"));
    }

    #[test]
    fn invalid_class_fails_validation() {
        let state = AndonState {
            schema: ANDON_STATE_SCHEMA.into(),
            active: true,
            entries: vec![AndonEntry {
                class: "UNKNOWN_DEFECT".into(),
                message: "test".into(),
                raised_at: "unknown".into(),
                source: String::new(),
            }],
        };
        let err = state.validate().expect_err("unknown class must fail");
        assert!(err.to_string().contains("UNKNOWN_DEFECT"));
    }

    #[test]
    fn wrong_schema_fails_validation() {
        let state = AndonState {
            schema: "chatmangpt.mcpp.andon.v0".into(),
            active: false,
            entries: vec![],
        };
        let err = state.validate().expect_err("wrong schema must fail");
        assert!(err.to_string().contains("schema must be"));
    }

    #[test]
    fn round_trip_save_load_inactive() {
        let path = tmp("round-trip-inactive.yaml");
        let state = AndonState::inactive();
        state.save(&path).expect("save");
        let loaded = AndonState::load(&path).expect("load");
        assert_eq!(state, loaded);
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn round_trip_save_load_active() {
        let path = tmp("round-trip-active.yaml");
        let state = AndonState::raise_andon("RECEIPT_DEFECT", "receipt hash mismatch");
        state.save(&path).expect("save");
        let loaded = AndonState::load(&path).expect("load");
        assert_eq!(state, loaded);
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn missing_file_returns_typed_error() {
        let path = tmp("does-not-exist.yaml");
        let err = AndonState::load(&path).expect_err("missing must error");
        assert!(err.to_string().contains("missing"));
        // to_envelope must not panic
        let env = err.to_envelope("mcpp.andon.load", "mcpp");
        let v: serde_json::Value = serde_json::from_str(&env.to_json()).unwrap();
        assert_eq!(v["errors"][0]["class"], "ANDON_DEFECT");
    }

    #[test]
    fn malformed_yaml_returns_typed_error() {
        let path = tmp("malformed.yaml");
        fs::write(&path, "this: : : is: not: yaml").unwrap();
        let err = AndonState::load(&path).expect_err("malformed must error");
        assert!(err.to_string().contains("malformed"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn default_path_produces_expected_suffix() {
        let root = PathBuf::from("/some/project");
        let p = AndonState::default_path(&root);
        assert!(p.ends_with(".chatmangpt/andon.yaml"));
    }

    #[test]
    fn all_andon_active_classes_are_valid() {
        for class in ANDON_ACTIVE_CLASSES {
            let state = AndonState::raise_andon(class, "test");
            state
                .validate()
                .unwrap_or_else(|e| panic!("class {class} failed: {e}"));
        }
    }
}
