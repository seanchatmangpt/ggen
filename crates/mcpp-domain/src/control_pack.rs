//! `control-pack.yaml` loader — promoted from
//! `chatmangpt-mcpp-v2-cell/control-pack.yaml` per `portfolio-obl-0002`.
//!
//! The control pack is the executable contract manifest: every gate the
//! line evaluates and every andon class the line may raise live here.
//! Canonical MCPP loads either an existing v2-shape file or generates the
//! default one.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

pub const CONTROL_PACK_SCHEMA: &str = "chatmangpt.control-pack.v0.1";

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ControlPack {
    pub schema: String,
    pub naming: Naming,
    pub agents: Agents,
    pub truth_hierarchy: TruthHierarchy,
    pub gates: BTreeMap<String, Gate>,
    pub andon_classes: Vec<String>,
    pub forbidden_language: Vec<String>,
    #[serde(default)]
    pub extensions: ExtensionPolicy,
    #[serde(default)]
    pub completion_contract: Option<CompletionContract>,
    #[serde(default)]
    pub exit_codes: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Naming {
    pub product_name: String,
    pub code_name: String,
    pub cli_binary: String,
    pub target_id: String,
    pub forbidden_in_code: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Agents {
    pub gemini: AgentRole,
    pub claude_code: AgentRole,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AgentRole {
    pub role: String,
    #[serde(default)]
    pub may_write_truth: bool,
    #[serde(default)]
    pub requires_accepted_delta: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TruthHierarchy {
    pub order: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Gate {
    pub description: String,
    pub enforced_by: Vec<String>,
    pub failure_class: String,
    #[serde(default)]
    pub blocks_completion: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub struct ExtensionPolicy {
    pub default: String,
    #[serde(default)]
    pub allowed: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CompletionContract {
    pub done_requires: Vec<String>,
    pub invalid_completion_signals: Vec<String>,
}

impl ControlPack {
    /// Default control-pack matching the v2 cell — used when bootstrapping
    /// a fresh canonical workspace via `ControlPack::default_v2_compatible()`.
    pub fn default_v2_compatible() -> Self {
        let mut gates = BTreeMap::new();
        gates.insert(
            "invocation_split".into(),
            Gate {
                description:
                    "Slash commands (/speckit.*) are agent slash commands, NOT Specify CLI commands"
                        .into(),
                enforced_by: vec![
                    "sr verify".into(),
                    "speckit-ralph/tests/core_contracts.rs".into(),
                ],
                failure_class: "INVOCATION_DEFECT".into(),
                blocks_completion: true,
            },
        );
        gates.insert(
            "accepted_delta_required".into(),
            Gate {
                description: "Implementation requires a valid AcceptedDelta".into(),
                enforced_by: vec![
                    "sr implement".into(),
                    "speckit-ralph/tests/core_contracts.rs".into(),
                ],
                failure_class: "AGENT_ROUTING_DEFECT".into(),
                blocks_completion: true,
            },
        );
        gates.insert(
            "no_false_completion".into(),
            Gate {
                description: "Tool success and tests passing alone do not constitute completion"
                    .into(),
                enforced_by: vec![
                    "sr receipt verify".into(),
                    "mcp-plus/tests/control_loop.rs".into(),
                ],
                failure_class: "RECEIPT_DEFECT".into(),
                blocks_completion: true,
            },
        );
        gates.insert(
            "receipt_required".into(),
            Gate {
                description: "Receipt verification is the only valid done signal".into(),
                enforced_by: vec![
                    "sr receipt verify".into(),
                    "mcp-plus/tests/control_loop.rs".into(),
                ],
                failure_class: "RECEIPT_DEFECT".into(),
                blocks_completion: true,
            },
        );
        gates.insert(
            "extension_policy_default_deny".into(),
            Gate {
                description: "Extensions are deny-by-default; only deterministic stations admitted"
                    .into(),
                enforced_by: vec!["sr extension audit".into()],
                failure_class: "POLICY_DEFECT".into(),
                blocks_completion: true,
            },
        );

        Self {
            schema: CONTROL_PACK_SCHEMA.into(),
            naming: Naming {
                product_name: "MCP Plus".into(),
                code_name: "MCPP".into(),
                cli_binary: "mcpp".into(),
                target_id: "ggen-mcpp".into(),
                forbidden_in_code: vec!["mcp-plus".into()],
            },
            agents: Agents {
                gemini: AgentRole {
                    role: "explore".into(),
                    may_write_truth: false,
                    requires_accepted_delta: false,
                },
                claude_code: AgentRole {
                    role: "exploit".into(),
                    may_write_truth: false,
                    requires_accepted_delta: true,
                },
            },
            truth_hierarchy: TruthHierarchy {
                order: vec![
                    "PUBLIC-ONTOLOGIES.ttl".into(),
                    ".specify/memory/constitution.md".into(),
                    "control-pack.yaml".into(),
                    ".mcpp/state.yaml".into(),
                    ".mcpp/accepted-delta.yaml".into(),
                    "implementation".into(),
                    ".mcpp/receipt.yaml".into(),
                ],
            },
            gates,
            andon_classes: vec![
                "SPEC_DEFECT".into(),
                "PLAN_DEFECT".into(),
                "TASK_DEFECT".into(),
                "IMPLEMENTATION_DEFECT".into(),
                "PRESET_DEFECT".into(),
                "INVOCATION_DEFECT".into(),
                "RECEIPT_DEFECT".into(),
                "AGENT_ROUTING_DEFECT".into(),
                "ONTOLOGY_DEFECT".into(),
                "POLICY_DEFECT".into(),
                "ENVIRONMENT_DEFECT".into(),
            ],
            forbidden_language: vec![
                "MVP".into(),
                "prototype".into(),
                "scaffold".into(),
                "skeleton".into(),
                "phase 2 hardening".into(),
                "interesting extension".into(),
                "tool success is done".into(),
                "generic MCP server".into(),
            ],
            extensions: ExtensionPolicy {
                default: "deny".into(),
                allowed: Vec::new(),
            },
            completion_contract: Some(CompletionContract {
                done_requires: vec![
                    "accepted_delta_valid".into(),
                    "verify_passed".into(),
                    "receipt_emitted".into(),
                    "receipt_verified".into(),
                    "state_advanced".into(),
                    "andon_inactive".into(),
                ],
                invalid_completion_signals: vec![
                    "agent_said_done".into(),
                    "tool_returned_success".into(),
                    "tasks_checked".into(),
                    "tests_passed_alone".into(),
                ],
            }),
            exit_codes: BTreeMap::new(),
        }
    }

    pub fn read(path: &Path) -> Result<Self> {
        let raw = std::fs::read_to_string(path)
            .with_context(|| format!("read control-pack at {path:?}"))?;
        let parsed: Self = serde_yaml::from_str(&raw).context("parse control-pack.yaml")?;
        Ok(parsed)
    }

    pub fn read_or_default(path: &Path) -> Self {
        Self::read(path).unwrap_or_else(|_| Self::default_v2_compatible())
    }

    pub fn write(&self, path: &Path) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).ok();
        }
        std::fs::write(path, serde_yaml::to_string(self)?).context("write control-pack.yaml")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn default_has_five_v2_gates() {
        let cp = ControlPack::default_v2_compatible();
        for k in [
            "invocation_split",
            "accepted_delta_required",
            "no_false_completion",
            "receipt_required",
            "extension_policy_default_deny",
        ] {
            assert!(cp.gates.contains_key(k), "missing gate {k}");
        }
    }

    #[test]
    fn default_has_eleven_andon_classes() {
        let cp = ControlPack::default_v2_compatible();
        assert_eq!(cp.andon_classes.len(), 11);
        assert!(cp.andon_classes.contains(&"RECEIPT_DEFECT".to_string()));
    }

    #[test]
    fn default_extensions_default_deny() {
        let cp = ControlPack::default_v2_compatible();
        assert_eq!(cp.extensions.default, "deny");
        assert!(cp.extensions.allowed.is_empty());
    }

    #[test]
    fn round_trip_through_yaml() {
        let td = tempdir().unwrap();
        let p = td.path().join("control-pack.yaml");
        let cp = ControlPack::default_v2_compatible();
        cp.write(&p).unwrap();
        let loaded = ControlPack::read(&p).unwrap();
        assert_eq!(loaded, cp);
    }

    #[test]
    fn read_or_default_falls_back_when_missing() {
        let cp = ControlPack::read_or_default(Path::new("/nonexistent/control-pack.yaml"));
        assert_eq!(cp.schema, CONTROL_PACK_SCHEMA);
    }

    #[test]
    fn loads_v2_yaml_subset() {
        // Minimal v2-shape yaml: required keys only. The loader must parse
        // a real v2 control-pack.yaml without translation.
        let yaml = "\
schema: chatmangpt.control-pack.v0.1
naming:
  product_name: MCP Plus
  code_name: MCPP
  cli_binary: mcpp
  target_id: mcpp
  forbidden_in_code: ['mcp-plus']
agents:
  gemini:
    role: explore
    may_write_truth: false
  claude_code:
    role: exploit
    requires_accepted_delta: true
truth_hierarchy:
  order: ['PUBLIC-ONTOLOGIES.ttl', '.chatmangpt/state.yaml']
gates:
  no_false_completion:
    description: 'Tool success and tests passing alone do not constitute completion'
    enforced_by: ['sr receipt verify']
    failure_class: RECEIPT_DEFECT
    blocks_completion: true
andon_classes: ['RECEIPT_DEFECT', 'INVOCATION_DEFECT']
forbidden_language: ['MVP']
";
        let cp: ControlPack = serde_yaml::from_str(yaml).expect("v2 subset must parse");
        assert_eq!(cp.naming.cli_binary, "mcpp");
        assert!(cp.gates.contains_key("no_false_completion"));
    }
}
