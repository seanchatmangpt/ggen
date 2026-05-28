//! IMPROVE-1 metrics — measure whether promotion improves authoring, and
//! **refuse** any metric whose backing events are absent.
//!
//! Hard gate: *no event → no metric.* Each metric declares the event(s) it
//! derives from; if those events are not present, it returns
//! [`MetricValue::InsufficientEvidence`] — never a fabricated number. The
//! `verdict` is `improving` only when the evidence earns it; otherwise
//! `insufficient_evidence` (the default — the claim is refused).

use std::collections::BTreeMap;

use ggen_graph::ocel::{OcelEvent, OcelLog};
use serde::Serialize;

use super::events::{activity, obj_type};
use super::history::{PromotionHistory, RouteStatus};
use super::log::IntelLog;

/// A metric value that is honest about missing evidence.
#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
#[serde(untagged)]
pub enum MetricValue {
    /// A computed value (its backing events were present).
    Value(f64),
    /// Backing events absent → the metric is not computable (not zero).
    #[serde(serialize_with = "ser_insufficient")]
    InsufficientEvidence,
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn ser_insufficient<S: serde::Serializer>(s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str("insufficient_evidence")
}

/// The IMPROVE-1 metric set + the earned verdict.
#[derive(Debug, Clone, Serialize)]
pub struct ImproveMetrics {
    pub route_hit_rate: MetricValue,
    pub promoted_route_hit_rate: MetricValue,
    pub seed_displacement_rate: MetricValue,
    pub repair_cycle_time_secs: MetricValue,
    pub gate_pass_rate_seed: MetricValue,
    pub gate_pass_rate_mined: MetricValue,
    pub repeat_failure_rate: MetricValue,
    pub promotion_survival_rate: MetricValue,
    pub promotion_churn: MetricValue,
    pub receipt_density: MetricValue,
    /// Distinct mining cycles observed in the promotion history.
    pub cycles: usize,
    /// `improving` | `insufficient_evidence` — earned, not assumed.
    pub verdict: String,
}

/// One reconstructed episode (events sharing an episode object), with its code.
struct Episode {
    code: String,
    events: Vec<OcelEvent>,
}

fn group_episodes(log: &OcelLog) -> Vec<Episode> {
    let mut map: BTreeMap<String, (String, Vec<OcelEvent>)> = BTreeMap::new();
    for ev in &log.events {
        let epid = ev
            .objects
            .iter()
            .find(|o| o.r#type == obj_type::EPISODE)
            .map(|o| o.id.clone());
        let code = ev
            .objects
            .iter()
            .find(|o| o.r#type == obj_type::DIAGNOSTIC_CODE)
            .map(|o| o.id.clone());
        if let (Some(epid), Some(code)) = (epid, code) {
            map.entry(epid).or_insert_with(|| (code, Vec::new())).1.push(ev.clone());
        }
    }
    map.into_values()
        .map(|(code, events)| Episode { code, events })
        .collect()
}

fn has_activity(ep: &Episode, act: &str) -> bool {
    ep.events.iter().any(|e| e.activity == act)
}

fn route_source(ep: &Episode) -> Option<String> {
    ep.events
        .iter()
        .find(|e| e.activity == activity::ROUTE_SELECTED)
        .and_then(|e| e.attributes.get("route_source").cloned())
}

fn rate(num: usize, den: usize) -> MetricValue {
    if den == 0 {
        MetricValue::InsufficientEvidence
    } else {
        MetricValue::Value(num as f64 / den as f64)
    }
}

/// Episode lawful closure observed directly from its events (DiagnosticRaised
/// before GatePassed). Cheap structural check (the SPARQL ASK is used in mining;
/// here we read the already-captured events).
fn closed(ep: &Episode) -> bool {
    let raised = ep
        .events
        .iter()
        .filter(|e| e.activity == activity::DIAGNOSTIC_RAISED)
        .map(|e| e.timestamp)
        .min();
    let passed = ep
        .events
        .iter()
        .filter(|e| e.activity == activity::GATE_PASSED)
        .map(|e| e.timestamp)
        .max();
    matches!((raised, passed), (Some(r), Some(p)) if r <= p)
}

/// Compute the IMPROVE-1 metrics for the project at `root`.
#[must_use]
pub fn compute_metrics(root: &std::path::Path) -> ImproveMetrics {
    let log = IntelLog::at_root(root).read();
    let episodes = group_episodes(&log);
    let history = PromotionHistory::at_root(root).read();

    // --- episode-derived metrics ---
    let total = episodes.len();
    let with_route = episodes.iter().filter(|e| has_activity(e, activity::ROUTE_SELECTED)).count();
    let mined_selected = episodes
        .iter()
        .filter(|e| route_source(e).as_deref() == Some("mined"))
        .count();
    let closed_eps: Vec<&Episode> = episodes.iter().filter(|e| closed(e)).collect();
    let with_receipt = closed_eps
        .iter()
        .filter(|e| has_activity(e, activity::RECEIPT_EMITTED))
        .count();

    let route_hit_rate = rate(with_route, total);
    let promoted_route_hit_rate = rate(mined_selected, with_route);
    let receipt_density = rate(with_receipt, closed_eps.len());

    // gate pass rate by source
    let pass_rate_for = |src: &str| {
        let group: Vec<&Episode> = episodes
            .iter()
            .filter(|e| route_source(e).as_deref() == Some(src))
            .collect();
        rate(group.iter().filter(|e| closed(e)).count(), group.len())
    };
    let gate_pass_rate_seed = pass_rate_for("seed");
    let gate_pass_rate_mined = pass_rate_for("mined");

    // repair_cycle_time: mean (GatePassed - DiagnosticRaised) seconds over closed eps
    let repair_cycle_time_secs = if closed_eps.is_empty() {
        MetricValue::InsufficientEvidence
    } else {
        let mut secs = Vec::new();
        for e in &closed_eps {
            let r = e.events.iter().filter(|x| x.activity == activity::DIAGNOSTIC_RAISED).map(|x| x.timestamp).min();
            let p = e.events.iter().filter(|x| x.activity == activity::GATE_PASSED).map(|x| x.timestamp).max();
            if let (Some(r), Some(p)) = (r, p) {
                secs.push((p - r).num_seconds() as f64);
            }
        }
        if secs.is_empty() {
            MetricValue::InsufficientEvidence
        } else {
            MetricValue::Value(secs.iter().sum::<f64>() / secs.len() as f64)
        }
    };

    // repeat_failure_rate: (file,code,span) families seen in >1 episode
    let repeat_failure_rate = if total == 0 {
        MetricValue::InsufficientEvidence
    } else {
        let mut seen: BTreeMap<String, usize> = BTreeMap::new();
        for e in &episodes {
            // key by code+span of the DiagnosticRaised event
            if let Some(d) = e.events.iter().find(|x| x.activity == activity::DIAGNOSTIC_RAISED) {
                let span = d.attributes.get("span").cloned().unwrap_or_default();
                *seen.entry(format!("{}|{}", e.code, span)).or_insert(0) += 1;
            }
        }
        let repeats = seen.values().filter(|&&c| c > 1).count();
        rate(repeats, seen.len())
    };

    // --- history-derived metrics ---
    // cycles = distinct promoted_at timestamps
    let cycles_set: std::collections::BTreeSet<&str> =
        history.iter().map(|r| r.promoted_at.as_str()).collect();
    let cycles = cycles_set.len();

    let promotion_churn = if history.is_empty() {
        MetricValue::InsufficientEvidence
    } else {
        // demoted/superseded in the latest cycle
        let latest = cycles_set.iter().next_back().copied();
        let churn = history
            .iter()
            .filter(|r| Some(r.promoted_at.as_str()) == latest)
            .filter(|r| matches!(r.status, RouteStatus::Demoted | RouteStatus::Superseded))
            .count();
        MetricValue::Value(churn as f64)
    };

    let promotion_survival_rate = if cycles < 2 {
        MetricValue::InsufficientEvidence
    } else {
        let latest_by = PromotionHistory::at_root(root).latest_by_route();
        let ever_active = history.iter().filter(|r| r.status == RouteStatus::Active).map(|r| r.route_id.clone()).collect::<std::collections::BTreeSet<_>>();
        let still_active = latest_by.values().filter(|r| r.status == RouteStatus::Active).count();
        rate(still_active, ever_active.len())
    };

    let seed_displacement_rate = if history.is_empty() {
        MetricValue::InsufficientEvidence
    } else {
        let families: std::collections::BTreeSet<&str> = history.iter().map(|r| r.family.as_str()).collect();
        let displaced: std::collections::BTreeSet<&str> = history
            .iter()
            .filter(|r| r.status == RouteStatus::Active)
            .map(|r| r.family.as_str())
            .collect();
        rate(displaced.len(), families.len())
    };

    // --- earned verdict ---
    let verdict = verdict(cycles, &history, gate_pass_rate_mined);

    ImproveMetrics {
        route_hit_rate,
        promoted_route_hit_rate,
        seed_displacement_rate,
        repair_cycle_time_secs,
        gate_pass_rate_seed,
        gate_pass_rate_mined,
        repeat_failure_rate,
        promotion_survival_rate,
        promotion_churn,
        receipt_density,
        cycles,
        verdict,
    }
}

/// `improving` only when ≥2 cycles exist AND the mean mined success_rate rose
/// from the first to the latest cycle (and mined routes are actually passing).
/// Otherwise `insufficient_evidence` — the claim is refused by default.
fn verdict(
    cycles: usize,
    history: &[super::history::RoutePromotionRecord],
    gate_pass_rate_mined: MetricValue,
) -> String {
    if cycles < 2 {
        return "insufficient_evidence".to_string();
    }
    // mean success_rate per cycle (ordered by promoted_at)
    let mut by_cycle: BTreeMap<String, (f64, usize)> = BTreeMap::new();
    for r in history {
        let e = by_cycle.entry(r.promoted_at.clone()).or_insert((0.0, 0));
        e.0 += f64::from(r.success_rate);
        e.1 += 1;
    }
    let means: Vec<f64> = by_cycle
        .values()
        .map(|(sum, n)| if *n == 0 { 0.0 } else { sum / *n as f64 })
        .collect();
    let rising = matches!((means.first(), means.last()), (Some(f), Some(l)) if l > f);
    let mined_passing = matches!(gate_pass_rate_mined, MetricValue::Value(v) if v > 0.0);
    if rising && mined_passing {
        "improving".to_string()
    } else {
        "insufficient_evidence".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intel::events::{diagnostic_raised, gate_result, new_run_id, receipt_emitted, route_selected};
    use crate::intel::IntelLog;
    use tempfile::TempDir;

    #[test]
    fn empty_project_refuses_every_metric_and_the_verdict() {
        let dir = TempDir::new().expect("tempdir");
        let m = compute_metrics(dir.path());
        assert_eq!(m.route_hit_rate, MetricValue::InsufficientEvidence);
        assert_eq!(m.repair_cycle_time_secs, MetricValue::InsufficientEvidence);
        assert_eq!(m.verdict, "insufficient_evidence", "no events ⇒ claim refused");
    }

    #[test]
    fn verdict_improving_only_when_success_rises_across_cycles() {
        use crate::intel::history::{RoutePromotionRecord, RouteStatus};
        let rec = |at: &str, sr: f32| RoutePromotionRecord {
            route_id: "mined.template-failure".into(),
            route_source: "mined".into(),
            diagnostic_code: "E0011".into(),
            family: "TemplateFailure".into(),
            support: 5,
            success_rate: sr,
            source_log_hash: "s".into(),
            routes_hash: "r".into(),
            promotion_receipt: Some("x".into()),
            promoted_at: at.into(),
            supersedes: None,
            status: RouteStatus::Active,
        };
        // Rising success across 2 cycles + mined routes passing → improving.
        let rising = vec![rec("2026-05-28T00:00:00+00:00", 0.5), rec("2026-05-28T01:00:00+00:00", 0.9)];
        assert_eq!(verdict(2, &rising, MetricValue::Value(0.9)), "improving");
        // Flat success → refused.
        let flat = vec![rec("2026-05-28T00:00:00+00:00", 0.9), rec("2026-05-28T01:00:00+00:00", 0.9)];
        assert_eq!(verdict(2, &flat, MetricValue::Value(0.9)), "insufficient_evidence");
        // One cycle → refused regardless.
        assert_eq!(verdict(1, &rising, MetricValue::Value(0.9)), "insufficient_evidence");
        // Rising but mined not passing → refused.
        assert_eq!(verdict(2, &rising, MetricValue::Value(0.0)), "insufficient_evidence");
    }

    #[test]
    fn route_hit_rate_measured_from_routeselected_events() {
        let dir = TempDir::new().expect("tempdir");
        let log = IntelLog::at_root(dir.path());
        let run = new_run_id();
        // One episode WITH a route selected + closed + receipt.
        log.append(&[
            diagnostic_raised("r.rq", "E0011", "warning", "0:0-0:1", &run, 1),
            route_selected("r.rq", "E0011", "mined.template-failure", "mined", &run, 2),
            gate_result("r.rq", "E0011", true, 0, &run, 3),
            receipt_emitted("r.rq", "E0011", "rcpt", &run, 4),
        ])
        .expect("append");
        let m = compute_metrics(dir.path());
        assert_eq!(m.route_hit_rate, MetricValue::Value(1.0), "1/1 episodes routed");
        assert_eq!(m.promoted_route_hit_rate, MetricValue::Value(1.0), "route was mined");
        assert_eq!(m.receipt_density, MetricValue::Value(1.0), "closed episode has a receipt");
    }
}
