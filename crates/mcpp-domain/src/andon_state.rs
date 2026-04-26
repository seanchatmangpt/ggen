//! Andon abnormality state — promoted from
//! `chatmangpt-mcpp-v2-cell/.chatmangpt/andon.yaml` per `portfolio-obl-0002`.
//!
//! When the line stops, `current_event` carries the active andon and prior
//! events accumulate in `history`. `clear` requires either operator override
//! or a verified receipt — same policy as v2.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub struct AndonState {
    /// `"active" | "inactive"`.
    pub status: String,
    pub current_event: Option<AndonEvent>,
    #[serde(default)]
    pub history: Vec<AndonEvent>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AndonEvent {
    pub class: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub gate: Option<String>,
    pub message: String,
}

impl AndonState {
    pub fn inactive() -> Self {
        Self {
            status: "inactive".into(),
            current_event: None,
            history: Vec::new(),
        }
    }

    pub fn read(path: &Path) -> Result<Self> {
        if !path.exists() {
            return Ok(Self::inactive());
        }
        let raw =
            std::fs::read_to_string(path).with_context(|| format!("read andon at {path:?}"))?;
        let parsed: Self = serde_yaml::from_str(&raw).context("parse andon.yaml")?;
        Ok(parsed)
    }

    pub fn write(&self, path: &Path) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).ok();
        }
        std::fs::write(path, serde_yaml::to_string(self)?).context("write andon.yaml")?;
        Ok(())
    }

    /// Push a new event: archive the previous `current_event` into history,
    /// then mark the line stopped with the new event.
    pub fn raise(&mut self, class: impl Into<String>, message: impl Into<String>) {
        if let Some(prev) = self.current_event.take() {
            self.history.push(prev);
        }
        self.status = "active".into();
        self.current_event = Some(AndonEvent {
            class: class.into(),
            gate: None,
            message: message.into(),
        });
    }

    /// Clear the line. If `require_verified_receipt` is true, the caller is
    /// asserting a separate verification has succeeded; otherwise only an
    /// explicit operator override is allowed.
    pub fn clear(&mut self, require_verified_receipt: bool, receipt_verified: bool) -> Result<()> {
        if require_verified_receipt && !receipt_verified {
            anyhow::bail!("andon clear requires receipt.status == \"verified\"");
        }
        if let Some(evt) = self.current_event.take() {
            self.history.push(evt);
        }
        self.status = "inactive".into();
        Ok(())
    }

    pub fn is_active(&self) -> bool {
        self.status == "active"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn missing_file_returns_inactive() {
        let td = tempdir().unwrap();
        let p = td.path().join("missing.yaml");
        let s = AndonState::read(&p).unwrap();
        assert_eq!(s, AndonState::inactive());
    }

    #[test]
    fn raise_then_clear_archives_history() {
        let mut s = AndonState::inactive();
        s.raise("RECEIPT_DEFECT", "missing receipt");
        assert!(s.is_active());
        assert!(s.current_event.is_some());
        s.clear(false, false).expect("operator override");
        assert!(!s.is_active());
        assert_eq!(s.history.len(), 1);
        assert_eq!(s.history[0].class, "RECEIPT_DEFECT");
    }

    #[test]
    fn clear_with_required_verify_rejects_unverified() {
        let mut s = AndonState::inactive();
        s.raise("RECEIPT_DEFECT", "x");
        assert!(s.clear(true, false).is_err());
        assert!(s.is_active());
    }

    #[test]
    fn clear_with_required_verify_accepts_verified() {
        let mut s = AndonState::inactive();
        s.raise("RECEIPT_DEFECT", "x");
        s.clear(true, true).expect("verified receipt clears");
        assert!(!s.is_active());
    }

    #[test]
    fn round_trip_through_yaml() {
        let td = tempdir().unwrap();
        let p = td.path().join("andon.yaml");
        let mut s = AndonState::inactive();
        s.raise("INVOCATION_DEFECT", "bad classifier");
        s.write(&p).unwrap();
        let loaded = AndonState::read(&p).unwrap();
        assert_eq!(loaded, s);
    }
}
