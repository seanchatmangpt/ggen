//! Architecture doctor diagnostics across governance, lifecycle, and capacity.

use std::{collections::BTreeMap, fmt::Write as _};

use serde::{Deserialize, Serialize};

use crate::{
    capacity::{CapacityEnvelope, CapacityLevel},
    error::Result,
    model::{AssetKind, LifecycleState, Severity, Standing},
    receipt::deterministic_hash,
    state::ArchitectureState,
};

/// Aggregate doctor standing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DoctorStatus {
    /// No warning or refusal findings.
    Healthy,
    /// Operation remains admitted, but remediation is recommended.
    Warning,
    /// Promotion or operation should be refused.
    Refused,
}

/// One actionable doctor finding.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DoctorFinding {
    /// Stable finding code.
    pub code: String,
    /// Severity.
    pub severity: Severity,
    /// Primary subject.
    pub subject: String,
    /// Explanation.
    pub message: String,
    /// Bounded remediation.
    pub remediation: String,
}

/// Complete, receipted architecture health report.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DoctorReport {
    /// Aggregate standing.
    pub status: DoctorStatus,
    /// Ordered findings.
    pub findings: Vec<DoctorFinding>,
    /// Deterministic summary metrics.
    pub metrics: BTreeMap<String, u64>,
    /// BLAKE3 receipt over status, findings, and metrics.
    pub receipt_hash: String,
}

#[derive(Serialize)]
struct DoctorReceiptBody<'a> {
    status: DoctorStatus,
    findings: &'a [DoctorFinding],
    metrics: &'a BTreeMap<String, u64>,
}

impl DoctorReport {
    /// Evaluate a complete enterprise architecture state.
    pub fn analyze(state: &ArchitectureState) -> Result<Self> {
        let mut findings = state
            .registry
            .validate()
            .into_iter()
            .map(|violation| DoctorFinding {
                code: violation.code,
                severity: violation.severity,
                subject: violation.asset_id,
                message: violation.message,
                remediation: "repair registry closure before promotion".to_string(),
            })
            .collect::<Vec<_>>();

        for asset in state.registry.assets.values() {
            if asset.lifecycle == LifecycleState::Active && asset.owner.is_none() {
                findings.push(DoctorFinding {
                    code: "EA-GOV-001".to_string(),
                    severity: Severity::Warning,
                    subject: asset.id.clone(),
                    message: "active architecture asset has no accountable owner".to_string(),
                    remediation: "assign an owner or steward in the architecture registry"
                        .to_string(),
                });
            }

            if matches!(asset.kind, AssetKind::Ontology | AssetKind::Pack)
                && asset.lifecycle == LifecycleState::Active
                && asset.version.is_none()
            {
                findings.push(DoctorFinding {
                    code: "EA-LIFE-001".to_string(),
                    severity: Severity::Warning,
                    subject: asset.id.clone(),
                    message: "active ontology or pack has no declared version".to_string(),
                    remediation: "assign a distributable identity and compatibility version"
                        .to_string(),
                });
            }

            if asset.lifecycle == LifecycleState::Deprecated {
                let has_successor = state
                    .registry
                    .assets
                    .values()
                    .any(|candidate| candidate.replaces.contains(&asset.id));
                if !has_successor {
                    findings.push(DoctorFinding {
                        code: "EA-LIFE-002".to_string(),
                        severity: Severity::Warning,
                        subject: asset.id.clone(),
                        message: "deprecated asset has no registered successor".to_string(),
                        remediation: "register a replacement and migration work package"
                            .to_string(),
                    });
                }
            }

            let standing_severity = match asset.standing {
                Standing::Unknown if asset.lifecycle == LifecycleState::Active => {
                    Some((Severity::Warning, "active asset has UNKNOWN standing"))
                }
                Standing::Blocked => Some((Severity::Error, "asset standing is BLOCKED")),
                Standing::BuildBroken => {
                    Some((Severity::Error, "asset standing is BUILD_BROKEN"))
                }
                Standing::Unsupported if asset.lifecycle == LifecycleState::Active => {
                    Some((Severity::Error, "active asset standing is UNSUPPORTED"))
                }
                Standing::Retired if asset.lifecycle == LifecycleState::Active => {
                    Some((Severity::Error, "active asset has RETIRED standing"))
                }
                _ => None,
            };
            if let Some((severity, message)) = standing_severity {
                findings.push(DoctorFinding {
                    code: "EA-STAND-001".to_string(),
                    severity,
                    subject: asset.id.clone(),
                    message: message.to_string(),
                    remediation: "run the asset's Gall checkpoint and attach its evidence receipt"
                        .to_string(),
                });
            }
        }

        let envelope = CapacityEnvelope::analyze(&state.capacity_samples, &state.capacity_policy);
        if state.capacity_samples.is_empty() {
            findings.push(DoctorFinding {
                code: "EA-CAP-000".to_string(),
                severity: Severity::Warning,
                subject: state.name.clone(),
                message: "no architecture capacity observations are registered".to_string(),
                remediation: "run a count × density × rules × projections stress ramp"
                    .to_string(),
            });
        } else if let Some(latest) = envelope.samples.last() {
            findings.extend(
                state
                    .capacity_policy
                    .evaluate(latest)
                    .into_iter()
                    .map(|finding| DoctorFinding {
                        code: finding.code,
                        severity: finding.severity,
                        subject: latest.label.clone(),
                        message: finding.message,
                        remediation: finding.remediation,
                    }),
            );
        }

        if let Some(knee) = &envelope.first_knee {
            findings.push(DoctorFinding {
                code: "EA-CAP-007".to_string(),
                severity: Severity::Warning,
                subject: knee.clone(),
                message: "first observed nonlinear capacity knee".to_string(),
                remediation: "profile the dominant phase and establish a profile-selection rule"
                    .to_string(),
            });
        }

        if state.autonomic_policy.direct_actuation_allowed {
            findings.push(DoctorFinding {
                code: "EA-AUTO-001".to_string(),
                severity: Severity::Critical,
                subject: state.name.clone(),
                message: "autonomic policy permits direct actuation".to_string(),
                remediation: "set direct_actuation_allowed=false; submit intents through BRCE"
                    .to_string(),
            });
        }

        if !state.autonomic_policy.enabled {
            findings.push(DoctorFinding {
                code: "EA-AUTO-002".to_string(),
                severity: Severity::Info,
                subject: state.name.clone(),
                message: "autonomic observation and planning are disabled".to_string(),
                remediation: "enable cycles when operating evidence is available".to_string(),
            });
        }

        findings.sort_by(|left, right| {
            right
                .severity
                .cmp(&left.severity)
                .then(left.code.cmp(&right.code))
                .then(left.subject.cmp(&right.subject))
        });

        let status = if findings
            .iter()
            .any(|finding| finding.severity >= Severity::Error)
        {
            DoctorStatus::Refused
        } else if findings
            .iter()
            .any(|finding| finding.severity == Severity::Warning)
        {
            DoctorStatus::Warning
        } else {
            DoctorStatus::Healthy
        };

        let mut metrics = BTreeMap::new();
        metrics.insert("assets".to_string(), state.registry.assets.len() as u64);
        metrics.insert(
            "active_assets".to_string(),
            state
                .registry
                .assets
                .values()
                .filter(|asset| asset.lifecycle == LifecycleState::Active)
                .count() as u64,
        );
        metrics.insert("capacity_samples".to_string(), envelope.samples.len() as u64);
        metrics.insert("max_observed_units".to_string(), envelope.max_observed_units);
        metrics.insert(
            "capacity_latest_level".to_string(),
            match envelope.latest_level {
                CapacityLevel::Healthy => 0,
                CapacityLevel::Warning => 1,
                CapacityLevel::Refuse => 2,
            },
        );
        metrics.insert("findings".to_string(), findings.len() as u64);

        let receipt_hash = deterministic_hash(
            "architecture_doctor",
            &DoctorReceiptBody {
                status,
                findings: &findings,
                metrics: &metrics,
            },
        )?;

        Ok(Self {
            status,
            findings,
            metrics,
            receipt_hash,
        })
    }

    /// Render a stable human-readable report.
    #[must_use]
    pub fn render_text(&self) -> String {
        let mut output = String::new();
        let _ = writeln!(output, "ggen architecture doctor: {:?}", self.status);
        for finding in &self.findings {
            let _ = writeln!(
                output,
                "- [{:?}] {} {}: {}\n  remediation: {}",
                finding.severity,
                finding.code,
                finding.subject,
                finding.message,
                finding.remediation
            );
        }
        let _ = writeln!(output, "receipt: {}", self.receipt_hash);
        output
    }
}
