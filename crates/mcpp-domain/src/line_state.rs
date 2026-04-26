//! Runtime line state — promoted from
//! `chatmangpt-mcpp-v2-cell/.chatmangpt/state.yaml` per
//! `portfolio-obl-0002`.
//!
//! Mirrors the YAML shape so v2 state files load directly into canonical
//! MCPP without translation.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LineState {
    pub target: String,
    pub phase: String,
    pub line_status: String,
    pub work_unit: String,
    pub wip: Wip,
    pub active_delta: String,
    pub expected_receipt: String,
    pub agents: AgentRoles,
    #[serde(default)]
    pub verify_passed: bool,
    #[serde(default)]
    pub state_advanced: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Wip {
    pub active: u32,
    pub limit: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AgentRoles {
    pub explore: String,
    pub exploit: String,
}

impl LineState {
    /// Default seed for canonical MCPP — used when bootstrapping a fresh
    /// `.mcpp/state.yaml`. Matches v2 default values.
    pub fn seed(target: impl Into<String>, work_unit: impl Into<String>) -> Self {
        Self {
            target: target.into(),
            phase: "implement".into(),
            line_status: "running".into(),
            work_unit: work_unit.into(),
            wip: Wip {
                active: 1,
                limit: 1,
            },
            active_delta: ".mcpp/accepted-delta.yaml".into(),
            expected_receipt: ".mcpp/receipt.yaml".into(),
            agents: AgentRoles {
                explore: "gemini".into(),
                exploit: "claude-code".into(),
            },
            verify_passed: false,
            state_advanced: false,
        }
    }

    pub fn read(path: &Path) -> Result<Self> {
        let raw = std::fs::read_to_string(path)
            .with_context(|| format!("read line state at {path:?}"))?;
        let parsed: Self = serde_yaml::from_str(&raw).context("parse state.yaml")?;
        Ok(parsed)
    }

    pub fn write(&self, path: &Path) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).ok();
        }
        std::fs::write(path, serde_yaml::to_string(self)?).context("write state.yaml")?;
        Ok(())
    }

    pub fn is_running(&self) -> bool {
        self.line_status == "running"
    }
    pub fn is_stopped(&self) -> bool {
        self.line_status == "stopped"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn round_trip_through_yaml() {
        let td = tempdir().unwrap();
        let p = td.path().join("state.yaml");
        let s = LineState::seed("ggen-mcpp", "obl-0002-promote");
        s.write(&p).unwrap();
        let loaded = LineState::read(&p).unwrap();
        assert_eq!(loaded, s);
    }

    #[test]
    fn parses_v2_yaml_shape() {
        // Verbatim v2 state.yaml shape — promotion contract requires this
        // to parse without translation.
        let yaml = "\
target: mcpp
phase: implement
line_status: running
work_unit: mcpp-first-control-loop
wip:
  active: 1
  limit: 1
active_delta: .chatmangpt/accepted-delta.yaml
expected_receipt: .chatmangpt/receipt.yaml
agents:
  explore: gemini
  exploit: claude-code
";
        let s: LineState = serde_yaml::from_str(yaml).expect("v2 shape must load");
        assert_eq!(s.target, "mcpp");
        assert!(s.is_running());
        assert!(!s.is_stopped());
    }

    #[test]
    fn line_status_helpers() {
        let mut s = LineState::seed("t", "w");
        assert!(s.is_running());
        s.line_status = "stopped".into();
        assert!(!s.is_running());
        assert!(s.is_stopped());
    }
}
