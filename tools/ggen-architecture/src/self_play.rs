//! CLI facade over the canonical `self_play` engine (deterministic, non-LLM
//! bounded-role move selection — see `ggen-architecture-kernel`'s own doc
//! comment on `self_play`).
//!
//! This module fills a real gap: `crates/ggen-architecture/src/self_play.rs`
//! is real, tested library code (`run_scenario`/`run_suite`/`verify_report`/
//! `verify_suite`), but had no CLI entry point anywhere in the repo — its own
//! smoke test (`crates/ggen-cli/tests/self_play_smoke_test.rs`) was disabled
//! (`#![cfg(any())]`) precisely because `crates/ggen-cli/src/cmds/self_play.rs`
//! never existed. Rather than force this deterministic kernel into the main
//! `ggen` binary's noun-verb macro system, it lands here — the CLI facade
//! this crate (`ggen-architecture-cli`) already exists for, alongside
//! `doctor`/`fortune5`/`capacity`, following the same conventions (a
//! receipted report struct, `render_text()`, `--json`).
//!
//! `demo_scenario()` is a real, hand-authored two-actor scenario (not lifted
//! from a fixture that doesn't exist elsewhere in the repo) exercising one
//! SAFe use case end to end. `doctor()` is this module's own addition beyond
//! what the disabled smoke test expected: it runs the scenario, independently
//! re-verifies it (proving replay determinism, not just single-run success),
//! and reports findings in the same `DoctorReport`-style shape the rest of
//! this CLI already uses, so `self-play doctor` reads like every other
//! `doctor`-style surface here instead of inventing a new report shape.

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write as _,
};

use serde::{Deserialize, Serialize};

use crate::{error::Result, model::Severity, receipt::deterministic_hash};

pub use ggen_architecture_kernel::{
    run_scenario, run_suite, verify_report, verify_suite, ActionSpec, ActorPolicy, ActorRole,
    Comparison, GameState, Metric, MetricConstraint, MetricEffect, MoveReceipt, SelfPlayReport,
    SelfPlayScenario, SelfPlayStanding, SelfPlayViolation, UseCaseKind,
};

/// A real, minimal, legal two-actor scenario: a `SystemArchitect` proposes
/// spending architecture runway to raise integration; a `RiskOfficer` must
/// independently verify evidence coverage before the scenario can reach its
/// goal. Two rounds are enough to reach a fixed point.
#[must_use]
pub fn demo_scenario() -> SelfPlayScenario {
    let architect_action = ActionSpec {
        id: "extend_integration_runway".to_string(),
        actor: ActorRole::SystemArchitect,
        choice_group: None,
        guards: vec![MetricConstraint {
            metric: Metric::ArchitectureRunway,
            comparison: Comparison::AtLeast,
            threshold: 1,
        }],
        effects: vec![
            MetricEffect {
                metric: Metric::Integration,
                delta: 3,
            },
            MetricEffect {
                metric: Metric::ArchitectureRunway,
                delta: -1,
            },
        ],
        required_authorities: BTreeSet::default(),
        evidence: ["integration_delta_receipt".to_string()]
            .into_iter()
            .collect(),
        broker_intent: false,
    };

    let verifier_action = ActionSpec {
        id: "verify_integration_evidence".to_string(),
        actor: ActorRole::EvidenceVerifier,
        choice_group: None,
        guards: vec![MetricConstraint {
            metric: Metric::Integration,
            comparison: Comparison::AtLeast,
            threshold: 3,
        }],
        effects: vec![MetricEffect {
            metric: Metric::EvidenceCoverage,
            delta: 5,
        }],
        required_authorities: BTreeSet::default(),
        evidence: ["evidence_coverage_receipt".to_string()]
            .into_iter()
            .collect(),
        broker_intent: false,
    };

    let mut initial_metrics = BTreeMap::new();
    initial_metrics.insert(Metric::ArchitectureRunway, 2);
    initial_metrics.insert(Metric::Integration, 0);
    initial_metrics.insert(Metric::EvidenceCoverage, 0);

    SelfPlayScenario {
        id: "demo-integration-runway".to_string(),
        use_cases: [UseCaseKind::SafeArchitectureRunway].into_iter().collect(),
        max_rounds: 5,
        initial_metrics,
        policies: vec![
            ActorPolicy {
                actor: ActorRole::SystemArchitect,
                weights: [(Metric::Integration, 1)].into_iter().collect(),
                authorities: BTreeSet::default(),
                minimum_utility: 1,
            },
            ActorPolicy {
                actor: ActorRole::EvidenceVerifier,
                weights: [(Metric::EvidenceCoverage, 1)].into_iter().collect(),
                authorities: BTreeSet::default(),
                minimum_utility: 1,
            },
        ],
        actions: vec![architect_action, verifier_action],
        invariants: vec![MetricConstraint {
            metric: Metric::ArchitectureRunway,
            comparison: Comparison::AtLeast,
            threshold: 0,
        }],
        goals: vec![MetricConstraint {
            metric: Metric::EvidenceCoverage,
            comparison: Comparison::AtLeast,
            threshold: 5,
        }],
    }
}

/// One actionable self-play doctor finding — same shape as
/// [`crate::doctor::DoctorFinding`], kept local to avoid coupling this
/// engine's findings to the enterprise-architecture registry's.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SelfPlayDoctorFinding {
    /// Stable finding code.
    pub code: String,
    /// Severity.
    pub severity: Severity,
    /// Primary subject (usually the scenario id).
    pub subject: String,
    /// Explanation.
    pub message: String,
    /// Bounded remediation.
    pub remediation: String,
}

/// Aggregate doctor standing, matching [`crate::doctor::DoctorStatus`]'s
/// vocabulary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SelfPlayDoctorStatus {
    /// No warning or refusal findings.
    Healthy,
    /// The scenario ran, but remediation is recommended.
    Warning,
    /// The scenario should be refused or reworked.
    Refused,
}

/// Receipted self-play health report: runs the scenario, independently
/// re-verifies the report (real replay, not a single-shot success claim),
/// and surfaces findings.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SelfPlayDoctorReport {
    /// Aggregate standing.
    pub status: SelfPlayDoctorStatus,
    /// Ordered findings.
    pub findings: Vec<SelfPlayDoctorFinding>,
    /// The scenario this report was analyzed against.
    pub scenario_id: String,
    /// The scenario's own reported standing.
    pub standing: SelfPlayStanding,
    /// Rounds actually executed.
    pub rounds: u32,
    /// Move receipts produced.
    pub receipts: usize,
    /// Whether independent replay verification succeeded.
    pub replay_verified: bool,
    /// BLAKE3 receipt over status, findings, and standing.
    pub receipt_hash: String,
}

#[derive(Serialize)]
struct SelfPlayDoctorReceiptBody<'a> {
    status: SelfPlayDoctorStatus,
    findings: &'a [SelfPlayDoctorFinding],
    scenario_id: &'a str,
    standing: SelfPlayStanding,
    replay_verified: bool,
}

impl SelfPlayDoctorReport {
    /// Run `scenario`, then independently re-verify the resulting report —
    /// this is the real value beyond a bare `run_scenario` call: a
    /// `SelfPlayViolation::ReplayMismatch` here means the engine is not
    /// actually deterministic for this scenario, not merely that this one
    /// run failed.
    pub fn analyze(scenario: &SelfPlayScenario) -> Result<Self> {
        let mut findings = Vec::new();

        let report = run_scenario(scenario).map_err(|violation| {
            crate::error::ArchitectureError::Violation(violation.to_string())
        })?;

        let replay_verified = match verify_report(scenario, &report) {
            Ok(()) => true,
            Err(violation) => {
                findings.push(SelfPlayDoctorFinding {
                    code: "SP-REPLAY-001".to_string(),
                    severity: Severity::Critical,
                    subject: scenario.id.clone(),
                    message: format!("independent replay verification failed: {violation}"),
                    remediation: "the scenario's policies/actions are not deterministic under \
                        replay; check for any source of nondeterminism in guards or effects"
                        .to_string(),
                });
                false
            }
        };

        match report.standing {
            SelfPlayStanding::Alive => {}
            SelfPlayStanding::PartialAlive => findings.push(SelfPlayDoctorFinding {
                code: "SP-GOAL-001".to_string(),
                severity: Severity::Warning,
                subject: scenario.id.clone(),
                message: format!(
                    "scenario reached a bounded fixed point with unmet goals: {}",
                    report.unmet_goals.join(", ")
                ),
                remediation: "add actions/policies that admit progress toward the unmet goals, \
                    or relax the goal thresholds"
                    .to_string(),
            }),
            SelfPlayStanding::Blocked => findings.push(SelfPlayDoctorFinding {
                code: "SP-BLOCKED-001".to_string(),
                severity: Severity::Error,
                subject: scenario.id.clone(),
                message: "scenario admission or execution was refused (no receipts produced)"
                    .to_string(),
                remediation: "check initial invariants and that at least one policy's minimum \
                    utility is reachable from the initial state"
                    .to_string(),
            }),
        }

        if !report.fixed_point {
            findings.push(SelfPlayDoctorFinding {
                code: "SP-ROUNDS-001".to_string(),
                severity: Severity::Warning,
                subject: scenario.id.clone(),
                message: format!(
                    "scenario exhausted max_rounds ({}) without reaching a move fixed point",
                    scenario.max_rounds
                ),
                remediation: "raise max_rounds, or check for actors that can always find a move \
                    (a scenario that never reaches a fixed point cannot be scored as ALIVE)"
                    .to_string(),
            });
        }

        findings.sort_by(|left, right| {
            right
                .severity
                .cmp(&left.severity)
                .then(left.code.cmp(&right.code))
        });

        let status = if findings.iter().any(|f| f.severity >= Severity::Error) {
            SelfPlayDoctorStatus::Refused
        } else if findings.iter().any(|f| f.severity == Severity::Warning) {
            SelfPlayDoctorStatus::Warning
        } else {
            SelfPlayDoctorStatus::Healthy
        };

        let receipt_hash = deterministic_hash(
            "self_play_doctor",
            &SelfPlayDoctorReceiptBody {
                status,
                findings: &findings,
                scenario_id: &scenario.id,
                standing: report.standing,
                replay_verified,
            },
        )?;

        Ok(Self {
            status,
            findings,
            scenario_id: scenario.id.clone(),
            standing: report.standing,
            rounds: report.rounds,
            receipts: report.receipts.len(),
            replay_verified,
            receipt_hash,
        })
    }

    /// Render a stable human-readable report, same convention as
    /// [`crate::doctor::DoctorReport::render_text`].
    #[must_use]
    pub fn render_text(&self) -> String {
        let mut output = String::new();
        let _ = writeln!(output, "ggen self-play doctor: {:?}", self.status);
        let _ = writeln!(
            output,
            "scenario: {} standing={:?} rounds={} receipts={} replay_verified={}",
            self.scenario_id, self.standing, self.rounds, self.receipts, self.replay_verified
        );
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
