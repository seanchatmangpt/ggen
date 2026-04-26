//! Canonical control-pack manifest.
//!
//! Promoted from absorbed source `/Users/sac/chatmangpt/mcpp/control-pack.yaml`
//! (portfolio-obl-0002, promotion id `control-pack-manifest`).
//!
//! The control pack is the executable contract manifest for canonical
//! MCPP. It declares: naming, doctrine, truth hierarchy, agent roles,
//! extension policy, forbidden language, named gates, andon classes,
//! CLI protocol exit codes, and the Definition-of-Done.
//!
//! Doctrine:
//! - The control pack is data, not narrative. It is loaded, validated,
//!   and consulted at runtime.
//! - Missing or malformed manifests must return a typed
//!   `CONTROL_PACK_DEFECT` envelope, never panic.
//! - Read/write is YAML round-trip-stable.
//! - Gate names align with the absorbed v2 manifest so downstream
//!   consumers (verify, receipt) see the same vocabulary.

use mcpp_core::Envelope;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use thiserror::Error;

/// Stable schema identifier for the canonical control pack.
pub const CONTROL_PACK_SCHEMA: &str = "chatmangpt.mcpp.control-pack.v1";

/// Class string emitted in a `CONTROL_PACK_DEFECT` failure envelope
/// when the manifest is missing, malformed, or violates a structural
/// invariant.
pub const CONTROL_PACK_DEFECT_CLASS: &str = "CONTROL_PACK_DEFECT";

/// The canonical exit-code names enforced by every MCPP CLI command.
///
/// These are the same names emitted by the absorbed v2 cell and form
/// part of the contract: downstream consumers map exit codes to these
/// names without ambiguity.
pub const CANONICAL_EXIT_CODES: &[(u8, &str)] = &[
    (0, "success"),
    (1, "command_failed"),
    (2, "invalid_usage"),
    (3, "gate_failed"),
    (4, "line_stopped"),
    (5, "missing_required_state"),
    (6, "policy_violation"),
    (7, "receipt_invalid"),
    (8, "external_tool_failure"),
    (9, "invocation_defect"),
];

/// The canonical gate names produced by `mcpp verify`. Aligned with
/// the absorbed v2 manifest.
pub const CANONICAL_GATES: &[&str] = &[
    "invocation_split",
    "accepted_delta_required",
    "no_false_completion",
    "receipt_required",
    "extension_policy",
    "forbidden_language",
];

/// The canonical andon classes.
pub const CANONICAL_ANDON_CLASSES: &[&str] = &[
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

#[derive(Debug, Error)]
pub enum ControlPackError {
    #[error("control pack file missing at {path}")]
    Missing { path: String },
    #[error("control pack at {path} is malformed: {source}")]
    Malformed {
        path: String,
        #[source]
        source: serde_yaml::Error,
    },
    #[error("control pack violates structural invariant: {0}")]
    Invariant(String),
    #[error("control pack I/O error at {path}: {source}")]
    Io {
        path: String,
        #[source]
        source: std::io::Error,
    },
}

impl ControlPackError {
    /// Convert to a typed JSON failure envelope. Never panics.
    pub fn to_envelope(&self, command: &str, target: &str) -> Envelope {
        Envelope::fail(
            command,
            target,
            CONTROL_PACK_DEFECT_CLASS,
            &self.to_string(),
        )
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct Naming {
    pub product_name: String,
    pub code_name: String,
    pub cli_binary: String,
    pub target_id: String,
    #[serde(default)]
    pub forbidden_in_code: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct Law {
    pub equation: String,
    #[serde(default)]
    pub doctrine: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct Gate {
    pub description: String,
    pub required: bool,
    #[serde(default)]
    pub enforced_by: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct ExtensionPolicy {
    pub default: String,
    #[serde(default)]
    pub allowed: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct CliProtocol {
    pub default_output: String,
    pub envelope_schema: String,
    pub exit_codes: BTreeMap<String, String>,
}

/// The canonical control pack.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct ControlPack {
    pub schema: String,
    pub version: String,
    pub naming: Naming,
    pub law: Law,
    pub extensions: ExtensionPolicy,
    #[serde(default)]
    pub forbidden_language: Vec<String>,
    #[serde(default)]
    pub gates: BTreeMap<String, Gate>,
    #[serde(default)]
    pub andon_classes: Vec<String>,
    pub cli_protocol: CliProtocol,
    #[serde(default)]
    pub dod_v2: Vec<String>,
    #[serde(default)]
    pub invalid_completion_signals: Vec<String>,
}

impl ControlPack {
    /// Build the v2-compatible default control pack. Mirrors the
    /// absorbed `/Users/sac/chatmangpt/mcpp/control-pack.yaml` shape.
    pub fn default_v2_compatible() -> Self {
        let mut gates = BTreeMap::new();
        for name in CANONICAL_GATES {
            gates.insert(
                (*name).into(),
                Gate {
                    description: format!("Canonical gate: {name}"),
                    required: true,
                    enforced_by: vec![],
                },
            );
        }
        let mut exit_codes = BTreeMap::new();
        for (code, label) in CANONICAL_EXIT_CODES {
            exit_codes.insert(code.to_string(), (*label).into());
        }
        ControlPack {
            schema: CONTROL_PACK_SCHEMA.into(),
            version: "0.2.0".into(),
            naming: Naming {
                product_name: "MCP Plus".into(),
                code_name: "MCPP".into(),
                cli_binary: "mcpp".into(),
                target_id: "mcpp".into(),
                forbidden_in_code: vec!["mcp-plus".into(), "MCP Plus".into()],
            },
            law: Law {
                equation: "A = μ(O*)".into(),
                doctrine: vec![
                    "Spec Kit is the line.".into(),
                    "SR operates the line.".into(),
                    "MCP Plus controls SR.".into(),
                    "AcceptedDelta bounds work.".into(),
                    "Receipt verification proves completion.".into(),
                    "Command success is not completion.".into(),
                    "Tool success is not completion.".into(),
                    "No AcceptedDelta, no implementation.".into(),
                    "No receipt verification, no done.".into(),
                    "Public ontology first.".into(),
                    "Interesting is rejected.".into(),
                ],
            },
            extensions: ExtensionPolicy {
                default: "deny".into(),
                allowed: vec![],
            },
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
            gates,
            andon_classes: CANONICAL_ANDON_CLASSES
                .iter()
                .map(|s| (*s).into())
                .collect(),
            cli_protocol: CliProtocol {
                default_output: "json".into(),
                envelope_schema: "chatmangpt.mcpp.result.v1".into(),
                exit_codes,
            },
            dod_v2: vec![
                "mcpp doctor exits 0".into(),
                "mcpp telco next returns selected obligation".into(),
                "mcpp verify passes all required gates".into(),
                "mcpp receipt emit produces evidence".into(),
                "mcpp receipt verify validates evidence-backed receipt".into(),
            ],
            invalid_completion_signals: vec![
                "agent_said_done".into(),
                "tool_returned_success".into(),
                "tasks_checked".into(),
                "tests_passed_without_receipt".into(),
                "command_exited_zero".into(),
            ],
        }
    }

    /// Load and validate a control pack from a YAML file.
    pub fn load(path: &Path) -> Result<Self, ControlPackError> {
        if !path.exists() {
            return Err(ControlPackError::Missing {
                path: path.display().to_string(),
            });
        }
        let bytes = fs::read(path).map_err(|e| ControlPackError::Io {
            path: path.display().to_string(),
            source: e,
        })?;
        let pack: ControlPack =
            serde_yaml::from_slice(&bytes).map_err(|e| ControlPackError::Malformed {
                path: path.display().to_string(),
                source: e,
            })?;
        pack.validate()?;
        Ok(pack)
    }

    /// Save the control pack as YAML.
    pub fn save(&self, path: &Path) -> Result<(), ControlPackError> {
        self.validate()?;
        let yaml = serde_yaml::to_string(self).map_err(|e| ControlPackError::Malformed {
            path: path.display().to_string(),
            source: e,
        })?;
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).map_err(|e| ControlPackError::Io {
                path: parent.display().to_string(),
                source: e,
            })?;
        }
        fs::write(path, yaml).map_err(|e| ControlPackError::Io {
            path: path.display().to_string(),
            source: e,
        })
    }

    /// Validate structural invariants. Returns Err on first violation.
    pub fn validate(&self) -> Result<(), ControlPackError> {
        if self.schema != CONTROL_PACK_SCHEMA {
            return Err(ControlPackError::Invariant(format!(
                "schema must be {CONTROL_PACK_SCHEMA}, got {}",
                self.schema
            )));
        }
        if self.naming.cli_binary != "mcpp" {
            return Err(ControlPackError::Invariant(
                "naming.cli_binary must be 'mcpp'".into(),
            ));
        }
        if self.cli_protocol.default_output != "json" {
            return Err(ControlPackError::Invariant(
                "cli_protocol.default_output must be 'json'".into(),
            ));
        }
        if self.cli_protocol.envelope_schema != "chatmangpt.mcpp.result.v1" {
            return Err(ControlPackError::Invariant(
                "cli_protocol.envelope_schema must align with envelope contract".into(),
            ));
        }
        if self.extensions.default != "deny" {
            return Err(ControlPackError::Invariant(
                "extensions.default must be 'deny' (interesting is rejected)".into(),
            ));
        }
        for required_gate in CANONICAL_GATES {
            if !self.gates.contains_key(*required_gate) {
                return Err(ControlPackError::Invariant(format!(
                    "missing canonical gate: {required_gate}"
                )));
            }
        }
        for required_class in CANONICAL_ANDON_CLASSES {
            if !self.andon_classes.iter().any(|c| c == required_class) {
                return Err(ControlPackError::Invariant(format!(
                    "missing canonical andon class: {required_class}"
                )));
            }
        }
        for (code, label) in CANONICAL_EXIT_CODES {
            match self.cli_protocol.exit_codes.get(&code.to_string()) {
                Some(actual) if actual == label => {}
                Some(actual) => {
                    return Err(ControlPackError::Invariant(format!(
                        "exit_code {code} must be '{label}', got '{actual}'"
                    )))
                }
                None => {
                    return Err(ControlPackError::Invariant(format!(
                        "missing exit_code {code} ({label})"
                    )))
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    fn tmp(name: &str) -> std::path::PathBuf {
        let mut p = std::env::temp_dir();
        p.push(format!("mcpp-control-pack-{}-{}", std::process::id(), name));
        p
    }

    #[test]
    fn schema_constants_are_stable() {
        assert_eq!(CONTROL_PACK_SCHEMA, "chatmangpt.mcpp.control-pack.v1");
        assert_eq!(CONTROL_PACK_DEFECT_CLASS, "CONTROL_PACK_DEFECT");
    }

    #[test]
    fn default_pack_passes_validation() {
        let p = ControlPack::default_v2_compatible();
        p.validate().expect("default pack must validate");
        assert_eq!(p.naming.cli_binary, "mcpp");
        assert_eq!(p.cli_protocol.default_output, "json");
    }

    #[test]
    fn default_pack_carries_all_canonical_gates() {
        let p = ControlPack::default_v2_compatible();
        for g in CANONICAL_GATES {
            assert!(p.gates.contains_key(*g), "missing gate {g}");
        }
    }

    #[test]
    fn default_pack_carries_all_canonical_andon_classes() {
        let p = ControlPack::default_v2_compatible();
        for c in CANONICAL_ANDON_CLASSES {
            assert!(
                p.andon_classes.iter().any(|x| x == c),
                "missing andon class {c}"
            );
        }
    }

    #[test]
    fn default_pack_carries_all_canonical_exit_codes() {
        let p = ControlPack::default_v2_compatible();
        for (code, label) in CANONICAL_EXIT_CODES {
            assert_eq!(
                p.cli_protocol
                    .exit_codes
                    .get(&code.to_string())
                    .map(|s| s.as_str()),
                Some(*label),
                "exit code {code} mismatch"
            );
        }
    }

    #[test]
    fn save_and_load_round_trips() {
        let path = tmp("round-trip.yaml");
        let p = ControlPack::default_v2_compatible();
        p.save(&path).expect("save");
        let back = ControlPack::load(&path).expect("load");
        assert_eq!(p, back);
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn missing_pack_returns_typed_envelope_not_panic() {
        let path = tmp("does-not-exist.yaml");
        let err = ControlPack::load(&path).expect_err("must error");
        let env = err.to_envelope("mcpp.controlpack.load", "mcpp");
        let s = env.to_json();
        let v: Value = serde_json::from_str(&s).expect("must be valid JSON");
        assert_eq!(v["status"], "fail");
        assert_eq!(v["errors"][0]["class"], "CONTROL_PACK_DEFECT");
        assert!(v["errors"][0]["message"]
            .as_str()
            .unwrap()
            .contains("missing"));
    }

    #[test]
    fn malformed_pack_returns_typed_defect() {
        let path = tmp("malformed.yaml");
        fs::write(&path, "this is: : : not valid yaml: -- - :").unwrap();
        let err = ControlPack::load(&path).expect_err("must error");
        let env = err.to_envelope("mcpp.controlpack.load", "mcpp");
        let v: Value = serde_json::from_str(&env.to_json()).unwrap();
        assert_eq!(v["errors"][0]["class"], "CONTROL_PACK_DEFECT");
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn invariant_violation_returns_typed_defect() {
        let mut p = ControlPack::default_v2_compatible();
        p.naming.cli_binary = "not-mcpp".into();
        let err = p.validate().expect_err("must error on bad cli_binary");
        let env = err.to_envelope("mcpp.controlpack.validate", "mcpp");
        let v: Value = serde_json::from_str(&env.to_json()).unwrap();
        assert_eq!(v["errors"][0]["class"], "CONTROL_PACK_DEFECT");
        assert!(v["errors"][0]["message"]
            .as_str()
            .unwrap()
            .contains("cli_binary"));
    }

    #[test]
    fn extension_policy_must_be_deny_by_default() {
        let mut p = ControlPack::default_v2_compatible();
        p.extensions.default = "allow".into();
        let err = p.validate().expect_err("must reject allow-by-default");
        assert!(err.to_string().contains("deny"));
    }

    #[test]
    fn missing_canonical_gate_is_an_invariant_violation() {
        let mut p = ControlPack::default_v2_compatible();
        p.gates.remove("receipt_required");
        let err = p.validate().expect_err("must catch missing gate");
        assert!(err.to_string().contains("receipt_required"));
    }
}
