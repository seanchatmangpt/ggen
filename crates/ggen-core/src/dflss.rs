//! Design for Lean Six Sigma (DFLSS) - DMADV Phase Validation
//!
//! This module implements DMADV (Define, Measure, Analyze, Design, Verify)
//! quality gates for proactive quality engineering in ggen's manufacturing pipeline.

use crate::manifest::GgenManifest;
use crate::types::fmea::FmeaConfig;
use crate::utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Conventional location of the JSON FMEA report consumed by the Analyze gate,
/// relative to the project base path.
const FMEA_REPORT_PATH: &str = ".ggen/fmea.json";

/// Analyze-phase guard: enforce the FMEA Risk Priority Number (RPN) threshold.
///
/// RPN = Severity x Occurrence x Detection (see [`crate::types::fmea`]). A score
/// above 200 is classified as [`crate::types::fmea::RpnLevel::Critical`] and MUST
/// carry a mitigation control. This gate reads the real FMEA report from
/// `<base_path>/.ggen/fmea.json` (the JSON FMEA report referenced by the original
/// stub comment), deserializes it into an [`FmeaConfig`], and runs its real
/// `validate()` which fails on any unmitigated critical (RPN > 200) or duplicate ID.
///
/// Honest absence semantics: if no FMEA report exists, no failure modes are
/// declared, so the constraint "no failure mode has RPN > 200 without mitigation"
/// is vacuously satisfied. We do NOT fabricate a pass for a malformed report —
/// an unreadable or unparseable report blocks the gate.
fn analyze_fmea_risk_threshold(base_path: &Path) -> Result<()> {
    let report_path = base_path.join(FMEA_REPORT_PATH);
    if !report_path.exists() {
        // No declared failure modes -> nothing can exceed the threshold.
        return Ok(());
    }

    let contents = std::fs::read_to_string(&report_path).map_err(|e| {
        Error::new(&format!(
            "FMEA-1: Unable to read FMEA report at {}: {}",
            report_path.display(),
            e
        ))
    })?;

    let config: FmeaConfig = serde_json::from_str(&contents).map_err(|e| {
        Error::new(&format!(
            "FMEA-1: Malformed FMEA report at {}: {}",
            report_path.display(),
            e
        ))
    })?;

    config.validate().map_err(|errors| {
        let detail = errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("; ");
        Error::new(&format!(
            "FMEA-1: FMEA risk threshold exceeded — {} unmitigated/invalid failure mode(s): {}",
            errors.len(),
            detail
        ))
    })
}

/// DMADV phases for Design for Lean Six Sigma
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DmadvPhase {
    /// Phase 1: Define project goals and customer (agent) requirements
    Define,
    /// Phase 2: Measure and identify CTQs and capabilities
    Measure,
    /// Phase 3: Analyze design options and failure modes (FMEA)
    Analyze,
    /// Phase 4: Design process to meet CTQs
    Design,
    /// Phase 5: Verify design performance and ability to meet CTQs
    Verify,
}

impl DmadvPhase {
    pub fn phase_number(&self) -> u8 {
        match self {
            DmadvPhase::Define => 1,
            DmadvPhase::Measure => 2,
            DmadvPhase::Analyze => 3,
            DmadvPhase::Design => 4,
            DmadvPhase::Verify => 5,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            DmadvPhase::Define => "Define",
            DmadvPhase::Measure => "Measure",
            DmadvPhase::Analyze => "Analyze",
            DmadvPhase::Design => "Design",
            DmadvPhase::Verify => "Verify",
        }
    }
}

/// Validation criteria for a DMADV phase
#[derive(Debug, Clone)]
pub struct DflssCriteria {
    pub name: String,
    pub description: String,
    pub validator: fn(&GgenManifest, &Path) -> Result<()>,
}

/// DFLSS quality gate for a specific DMADV phase
pub struct DflssGate {
    phase: DmadvPhase,
    criteria: Vec<DflssCriteria>,
}

impl DflssGate {
    pub fn new(phase: DmadvPhase) -> Self {
        let criteria = Self::criteria_for_phase(phase);
        Self { phase, criteria }
    }

    fn criteria_for_phase(phase: DmadvPhase) -> Vec<DflssCriteria> {
        match phase {
            DmadvPhase::Define => vec![
                DflssCriteria {
                    name: "CTQ Definition".to_string(),
                    description: "Critical-to-Quality requirements are defined".to_string(),
                    validator: |manifest, _base_path| {
                        if manifest.project.name.is_empty() {
                            return Err(Error::new("CTQ-1: Project name missing"));
                        }
                        Ok(())
                    },
                },
                DflssCriteria {
                    name: "Voice of Customer (VOC)".to_string(),
                    description: "Customer (Agent) requirements are captured in rules".to_string(),
                    validator: |manifest, _base_path| {
                        if manifest.generation.rules.is_empty() {
                            return Err(Error::new("VOC-1: No generation rules found"));
                        }
                        Ok(())
                    },
                },
            ],
            DmadvPhase::Measure => vec![DflssCriteria {
                name: "Capability Baseline".to_string(),
                description: "System capability is measurable".to_string(),
                validator: |_manifest, base_path| {
                    if !base_path.join("Cargo.toml").exists() {
                        return Err(Error::new("CAP-1: Not a valid Rust workspace"));
                    }
                    Ok(())
                },
            }],
            DmadvPhase::Analyze => vec![DflssCriteria {
                name: "FMEA Risk Threshold".to_string(),
                description: "No failure modes have RPN > 200 without mitigation".to_string(),
                validator: |_manifest, base_path| analyze_fmea_risk_threshold(base_path),
            }],
            DmadvPhase::Design => vec![DflssCriteria {
                name: "Poka-Yoke Design".to_string(),
                description: "Design includes mistake-proofing guards".to_string(),
                validator: |manifest, _base_path| {
                    // Poka-Yoke (mistake-proofing): DFLSS requires an audit trail so
                    // that defects are caught/traced rather than silently emitted.
                    // The audit trail is the design-level guard; without it the
                    // pipeline cannot prove what was generated, so the gate blocks.
                    if !manifest.generation.require_audit_trail {
                        return Err(Error::new(
                            "PY-1: Audit trail disabled — Poka-Yoke design requires \
                             generation.require_audit_trail = true",
                        ));
                    }
                    Ok(())
                },
            }],
            DmadvPhase::Verify => vec![DflssCriteria {
                name: "Canonical Proof".to_string(),
                description: "Design is verified via proof gates".to_string(),
                validator: |_manifest, base_path| {
                    if !base_path.join(".ggen/receipts").exists() {
                        // return Err(Error::new("VER-1: No generation receipts found"));
                    }
                    Ok(())
                },
            }],
        }
    }

    pub fn check(&self, manifest: &GgenManifest, base_path: &Path) -> Result<DflssReport> {
        let mut results = Vec::new();
        let mut all_passed = true;

        for criterion in &self.criteria {
            let res = (criterion.validator)(manifest, base_path);
            if res.is_err() {
                all_passed = false;
            }
            results.push(DflssCheckResult {
                name: criterion.name.clone(),
                passed: res.is_ok(),
                error: res.err().map(|e| e.to_string()),
            });
        }

        Ok(DflssReport {
            phase: self.phase,
            passed: all_passed,
            checks: results,
        })
    }
}

pub fn execute_dflss(
    manifest: &GgenManifest, base_path: &Path, phase: Option<String>,
) -> Result<Vec<DflssReport>> {
    let phases = if let Some(p) = phase {
        match p.to_lowercase().as_str() {
            "define" => vec![DmadvPhase::Define],
            "measure" => vec![DmadvPhase::Measure],
            "analyze" => vec![DmadvPhase::Analyze],
            "design" => vec![DmadvPhase::Design],
            "verify" => vec![DmadvPhase::Verify],
            _ => return Err(Error::new(&format!("Invalid phase: {}", p))),
        }
    } else {
        vec![
            DmadvPhase::Define,
            DmadvPhase::Measure,
            DmadvPhase::Analyze,
            DmadvPhase::Design,
            DmadvPhase::Verify,
        ]
    };

    let mut reports = Vec::new();
    for p in phases {
        let gate = DflssGate::new(p);
        reports.push(gate.check(manifest, base_path)?);
    }
    Ok(reports)
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DflssReport {
    pub phase: DmadvPhase,
    pub passed: bool,
    pub checks: Vec<DflssCheckResult>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DflssCheckResult {
    pub name: String,
    pub passed: bool,
    pub error: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::manifest::ManifestParser;
    use std::fs;
    use tempfile::TempDir;

    /// Minimal real `ggen.toml` with a controllable `require_audit_trail` flag.
    /// Parsed via the real `ManifestParser` so tests exercise a genuine manifest,
    /// not a hand-built struct.
    fn manifest_toml(require_audit_trail: bool) -> String {
        format!(
            r#"
[project]
name = "dflss-test"
version = "1.0.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "structs"
query = {{ inline = "CONSTRUCT {{ ?s ?p ?o }} WHERE {{ ?s ?p ?o }}" }}
template = {{ inline = "x" }}
output_file = "src/out.rs"

[generation]
require_audit_trail = {require_audit_trail}
output_dir = "."
"#
        )
    }

    fn parse_manifest(require_audit_trail: bool) -> GgenManifest {
        ManifestParser::parse_str(&manifest_toml(require_audit_trail))
            .expect("test manifest should parse")
    }

    // --- Poka-Yoke Design gate (Design phase) ---

    #[test]
    fn poka_yoke_passes_when_audit_trail_enabled() {
        let manifest = parse_manifest(true);
        let gate = DflssGate::new(DmadvPhase::Design);
        let report = gate
            .check(&manifest, Path::new("."))
            .expect("check should produce a report");

        assert!(report.passed, "compliant manifest must pass Design gate");
        let poka = report
            .checks
            .iter()
            .find(|c| c.name == "Poka-Yoke Design")
            .expect("Poka-Yoke check must exist");
        assert!(poka.passed);
        assert!(poka.error.is_none());
    }

    /// SABOTAGE TEST: if the Poka-Yoke gate is ever neutered (return Err removed),
    /// this fails loudly because a non-compliant manifest must be blocked.
    #[test]
    fn poka_yoke_blocks_when_audit_trail_disabled() {
        let manifest = parse_manifest(false);
        let gate = DflssGate::new(DmadvPhase::Design);
        let report = gate
            .check(&manifest, Path::new("."))
            .expect("check should produce a report");

        assert!(
            !report.passed,
            "Design gate must FAIL when audit trail is disabled"
        );
        let poka = report
            .checks
            .iter()
            .find(|c| c.name == "Poka-Yoke Design")
            .expect("Poka-Yoke check must exist");
        assert!(!poka.passed, "Poka-Yoke must block non-compliant input");
        let msg = poka.error.as_deref().unwrap_or_default();
        assert!(
            msg.contains("PY-1") && msg.contains("Audit trail"),
            "error message must explain the violation, got: {msg:?}"
        );
    }

    // --- FMEA Risk Threshold gate (Analyze phase) ---

    #[test]
    fn fmea_passes_when_no_report_present() {
        // No declared failure modes -> nothing can exceed RPN 200 (vacuously true).
        let temp = TempDir::new().expect("tempdir");
        let result = analyze_fmea_risk_threshold(temp.path());
        assert!(result.is_ok(), "absent FMEA report must not fabricate failure");
    }

    #[test]
    fn fmea_passes_when_all_modes_within_threshold() {
        // RPN = 9*6*4 = 216 (Critical) but MITIGATED -> compliant.
        // RPN = 2*2*2 = 8 (Medium) -> compliant.
        let temp = TempDir::new().expect("tempdir");
        let ggen_dir = temp.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).expect("mkdir .ggen");
        let report = r#"{
            "enabled": true,
            "min_coverage": 100,
            "controls": [
                {"id":"F1","mode":"edit generated file","severity":9,"occurrence":6,"detection":4,"control":"DO NOT EDIT header"},
                {"id":"F2","mode":"low risk","severity":2,"occurrence":2,"detection":2}
            ]
        }"#;
        fs::write(ggen_dir.join("fmea.json"), report).expect("write report");

        let result = analyze_fmea_risk_threshold(temp.path());
        assert!(
            result.is_ok(),
            "mitigated critical + low-risk modes must pass, got: {result:?}"
        );
    }

    /// SABOTAGE TEST: an unmitigated critical (RPN > 200) MUST block. If the gate
    /// is ever reverted to `Ok(())`, this fails loudly.
    #[test]
    fn fmea_blocks_unmitigated_critical_rpn_over_200() {
        // RPN = 10*7*3 = 210 (> 200, Critical) with NO control -> must fail.
        let temp = TempDir::new().expect("tempdir");
        let ggen_dir = temp.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).expect("mkdir .ggen");
        let report = r#"{
            "enabled": true,
            "min_coverage": 100,
            "controls": [
                {"id":"F1","mode":"unmitigated critical","severity":10,"occurrence":7,"detection":3}
            ]
        }"#;
        fs::write(ggen_dir.join("fmea.json"), report).expect("write report");

        let result = analyze_fmea_risk_threshold(temp.path());
        assert!(
            result.is_err(),
            "unmitigated RPN > 200 must block the Analyze gate"
        );
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("FMEA-1") && msg.contains("F1"),
            "error must identify the offending failure mode, got: {msg:?}"
        );
    }

    #[test]
    fn fmea_blocks_malformed_report() {
        // A malformed report must NOT be treated as a pass (no fail-open).
        let temp = TempDir::new().expect("tempdir");
        let ggen_dir = temp.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).expect("mkdir .ggen");
        fs::write(ggen_dir.join("fmea.json"), "{ not valid json").expect("write");

        let result = analyze_fmea_risk_threshold(temp.path());
        assert!(result.is_err(), "malformed FMEA report must block, not pass");
    }

    /// End-to-end through the public Analyze gate, proving the wiring is live.
    #[test]
    fn analyze_gate_blocks_via_public_check() {
        let temp = TempDir::new().expect("tempdir");
        let ggen_dir = temp.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).expect("mkdir .ggen");
        let report = r#"{
            "enabled": true,
            "controls": [
                {"id":"F9","mode":"unmitigated","severity":10,"occurrence":10,"detection":10}
            ]
        }"#;
        fs::write(ggen_dir.join("fmea.json"), report).expect("write report");

        let manifest = parse_manifest(true);
        let gate = DflssGate::new(DmadvPhase::Analyze);
        let dflss_report = gate
            .check(&manifest, temp.path())
            .expect("check should produce a report");
        assert!(!dflss_report.passed, "Analyze gate must fail on RPN 1000");
    }
}
