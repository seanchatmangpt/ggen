//! FIELD-STATUS-1 — a read-only process-mining cockpit over the accumulated field log.
//!
//! "Let the event log speak; refuse what it cannot prove." This folds the OCEL log +
//! promotion history + the existing [`compute_metrics`](super::metrics::compute_metrics)
//! verdict into one legible view: how much real evidence exists, which transports/
//! agents/sessions produced it, how many distinct variants, the conformance rate, the
//! mining cycles, and the EARNED-or-REFUSED verdict. It computes no new success
//! signal — the verdict is reused verbatim — and it makes insufficient evidence
//! visible *as* insufficient. Read-only; never writes, never fabricates.

use std::collections::{BTreeMap, BTreeSet};

use serde::Serialize;

use super::log::IntelLog;
use super::metrics::{closed, compute_metrics, group_episodes, Episode, MetricValue};

/// Honest field-operation state — refusal is first-class.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FieldReadiness {
    /// No field evidence captured yet.
    NoEvidence,
    /// Evidence is accumulating; the verdict is not yet earned.
    Accumulating,
    /// `compute_metrics` earned `improving` from the accumulated evidence.
    VerdictEarned,
}

/// The process-mining cockpit: what the field evidence shows, computed read-only.
#[derive(Debug, Clone, Serialize)]
pub struct FieldStatus {
    pub event_count: usize,
    pub episode_count: usize,
    /// Episodes per transport (`lsp` | `mcp` | `a2a` | `headless`).
    pub by_transport: BTreeMap<String, usize>,
    /// Episodes per acting agent.
    pub by_agent: BTreeMap<String, usize>,
    pub distinct_sessions: usize,
    /// Distinct episode activity-sequence variants (van der Aalst variant analysis).
    pub distinct_variants: usize,
    /// Conservative instability flag: many variants relative to episodes.
    pub variant_explosion: bool,
    /// Closed episodes (`DiagnosticRaised ≺ GatePassed`) / total.
    pub conformance_rate: MetricValue,
    /// Distinct mining cycles (from `compute_metrics`).
    pub cycles: usize,
    /// The earned verdict, reused verbatim from `compute_metrics`.
    pub verdict: String,
    /// Honest field-operation state.
    pub readiness: FieldReadiness,
    /// Why the verdict/readiness is what it is.
    pub reasons: Vec<String>,
}

/// First non-empty value of attribute `key` across an episode's events (all events
/// in a capture batch carry the same attribution).
fn episode_attr(ep: &Episode, key: &str) -> Option<String> {
    ep.events.iter().find_map(|e| e.attributes.get(key).cloned())
}

/// The timestamp-ordered activity sequence of an episode — its process variant.
fn episode_variant(ep: &Episode) -> String {
    let mut evs: Vec<_> = ep.events.iter().collect();
    evs.sort_by_key(|e| e.timestamp);
    evs.iter().map(|e| e.activity.as_str()).collect::<Vec<_>>().join(">")
}

/// Compute the read-only field-status cockpit for the project at `root`.
#[must_use]
pub fn field_status(root: &std::path::Path) -> FieldStatus {
    let log = IntelLog::at_root(root).read();
    let event_count = log.events.len();
    let episodes = group_episodes(&log);
    let episode_count = episodes.len();

    let mut by_transport: BTreeMap<String, usize> = BTreeMap::new();
    let mut by_agent: BTreeMap<String, usize> = BTreeMap::new();
    let mut sessions: BTreeSet<String> = BTreeSet::new();
    let mut variants: BTreeSet<String> = BTreeSet::new();
    let mut closed_count = 0usize;

    for ep in &episodes {
        if let Some(t) = episode_attr(ep, "transport") {
            *by_transport.entry(t).or_insert(0) += 1;
        }
        if let Some(a) = episode_attr(ep, "agent_id") {
            *by_agent.entry(a).or_insert(0) += 1;
        }
        if let Some(s) = episode_attr(ep, "session_id") {
            sessions.insert(s);
        }
        variants.insert(episode_variant(ep));
        if closed(ep) {
            closed_count += 1;
        }
    }

    let distinct_variants = variants.len();
    // Conservative van der Aalst instability flag: distinct variants dominating the
    // episode population signals process churn worth investigating.
    let variant_explosion = episode_count >= 4 && distinct_variants * 2 > episode_count;

    let conformance_rate = if episode_count == 0 {
        MetricValue::InsufficientEvidence
    } else {
        MetricValue::Value(closed_count as f64 / episode_count as f64)
    };

    // The verdict is reused verbatim — one source of truth, never a parallel metric.
    let metrics = compute_metrics(root);
    let cycles = metrics.cycles;
    let verdict = metrics.verdict.clone();

    let mut reasons = Vec::new();
    let readiness = if event_count == 0 {
        reasons.push("no field evidence captured yet — operate the pack".to_string());
        FieldReadiness::NoEvidence
    } else if verdict == "improving" {
        reasons.push("compute_metrics earned 'improving' from accumulated evidence".to_string());
        FieldReadiness::VerdictEarned
    } else {
        if cycles < 2 {
            reasons.push(format!("only {cycles} mining cycle(s); the verdict needs ≥2"));
        }
        reasons.push("verdict not yet earned — accumulating field evidence".to_string());
        FieldReadiness::Accumulating
    };

    FieldStatus {
        event_count,
        episode_count,
        by_transport,
        by_agent,
        distinct_sessions: sessions.len(),
        distinct_variants,
        variant_explosion,
        conformance_rate,
        cycles,
        verdict,
        readiness,
        reasons,
    }
}
