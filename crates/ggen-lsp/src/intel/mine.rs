//! Offline failure-edge mining + route promotion (the "explore → promote" phase).
//!
//! Reads the OCEL log → projects to RDF → discovers the directly-follows graph
//! and per-family conformance via SPARQL → synthesizes **advisory** mined routes
//! carrying measured evidence (support + success_rate) → writes the promoted-route
//! artifact and a promotion receipt. Runs offline (`ggen lsp mine`), never on the
//! edit hot path. Promotion is conformance-gated downstream by the registry.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use ggen_graph::ocel::{EvidenceProjector, OCELEvent, OCEL};
use ggen_graph::{check_lifecycle_order, discover_dfg, DeterministicGraph, DfgEdge};

use super::events::{activity, obj_type, EPISODE_QUALIFIER};
use super::log::IntelLog;
use super::receipt::RepairReceipt;
use crate::route::{
    default_pack_routes_path, family_of_code, write_promoted, EditTemplate, PartialOrder,
    PromotedRoutes, Provenance, RepairFamily, RepairRoute, RepairStep, RouteId, StepId,
};

/// SPARQL predicate the episode case object is reached through.
fn episode_qualifier_iri() -> String {
    format!("http://www.ocel-standard.org/ns#qualifier_{EPISODE_QUALIFIER}")
}

/// Result of a mining run.
#[derive(Debug, Clone)]
pub struct MineReport {
    /// Number of events read from the log.
    pub event_count: usize,
    /// Ranked failure edges (highest impact first).
    pub failure_edges: Vec<DfgEdge>,
    /// All directly-follows edges discovered.
    pub all_edges: Vec<DfgEdge>,
    /// Path of the written markdown discovery report.
    pub report_path: PathBuf,
    /// Path of the written promoted-route artifact.
    pub promoted_path: PathBuf,
    /// Number of routes promoted (one advisory route per family with evidence).
    pub promoted_count: usize,
}

fn is_failure_target(activity_name: &str) -> bool {
    activity_name == activity::GATE_FAILED
        || activity_name == activity::REFUSAL_EMITTED
        || activity_name == activity::EDIT_APPLIED // rework back-edge
}

/// Per-family aggregated evidence.
struct FamilyEvidence {
    support: u32,
    success_rate: f32,
    first_seen: String,
    last_seen: String,
}

/// Group events into episodes and compute, per diagnostic family, the support
/// (episode count) and measured success_rate (fraction of episodes that lawfully
/// close: `DiagnosticRaised ≺ GatePassed`, via SPARQL on the episode sub-graph).
fn family_evidence(log: &OCEL) -> BTreeMap<String, FamilyEvidence> {
    // episode id -> (diagnostic code, events)
    let mut episodes: BTreeMap<String, (String, Vec<OCELEvent>)> = BTreeMap::new();
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
            let entry = episodes.entry(epid).or_insert_with(|| (code, Vec::new()));
            entry.1.push(ev.clone());
        }
    }

    // family (code) -> rolled-up evidence
    let mut acc: BTreeMap<String, (u32, u32, String, String)> = BTreeMap::new(); // support, closed, first, last
    for (_epid, (code, events)) in episodes {
        let closed = episode_closed(&events);
        let (min_ts, max_ts) = episode_time_bounds(&events);
        let e = acc
            .entry(code)
            .or_insert_with(|| (0, 0, min_ts.clone(), max_ts.clone()));
        e.0 += 1;
        if closed {
            e.1 += 1;
        }
        if min_ts < e.2 {
            e.2 = min_ts;
        }
        if max_ts > e.3 {
            e.3 = max_ts;
        }
    }

    acc.into_iter()
        .map(|(code, (support, closed, first, last))| {
            let success_rate = if support == 0 {
                0.0
            } else {
                closed as f32 / support as f32
            };
            (
                code,
                FamilyEvidence {
                    support,
                    success_rate,
                    first_seen: first,
                    last_seen: last,
                },
            )
        })
        .collect()
}

/// Measured lawful closure for one episode: project its events and ASK whether
/// `DiagnosticRaised ≺ GatePassed` holds (SPARQL conformance). Errors → false.
fn episode_closed(events: &[OCELEvent]) -> bool {
    let sub = OCEL {
        objects: vec![],
        events: events.to_vec(),
    };
    let Ok(graph) = DeterministicGraph::new() else {
        return false;
    };
    if EvidenceProjector::project_ocel(&graph, &sub).is_err() {
        return false;
    }
    check_lifecycle_order(
        &graph,
        &episode_qualifier_iri(),
        &[activity::DIAGNOSTIC_RAISED, activity::GATE_PASSED],
    )
    .unwrap_or(false)
}

fn episode_time_bounds(events: &[OCELEvent]) -> (String, String) {
    let mut ts: Vec<String> = events.iter().map(|e| e.timestamp.to_rfc3339()).collect();
    ts.sort();
    let first = ts.first().cloned().unwrap_or_default();
    let last = ts.last().cloned().unwrap_or_default();
    (first, last)
}

fn family_slug(family: RepairFamily) -> &'static str {
    match family {
        RepairFamily::ParseFailure => "parse-failure",
        RepairFamily::PublicVocabViolation => "public-vocab",
        RepairFamily::ShaclFailure => "shacl-failure",
        RepairFamily::TemplateFailure => "template-failure",
        RepairFamily::DanglingReference => "dangling-reference",
        RepairFamily::AdmissionFailure => "admission-failure",
        RepairFamily::LoadFailure => "load-failure",
        RepairFamily::ConfigValue => "config-value",
        RepairFamily::RuleFileMissing => "rule-file-missing",
    }
}

/// Synthesize one **advisory** mined route per family that has evidence.
fn synthesize_routes(
    evidence: &BTreeMap<String, FamilyEvidence>, source_report_hash: &str,
) -> Vec<RepairRoute> {
    let mut routes = Vec::new();
    for (code, ev) in evidence {
        let Some(family) = family_of_code(code) else {
            continue;
        };
        routes.push(RepairRoute {
            id: RouteId(format!("mined.{}", family_slug(family))),
            family,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: format!(
                        "Mined repair guidance for {} (support {}, success {:.0}%)",
                        code,
                        ev.support,
                        ev.success_rate * 100.0
                    ),
                    // Advisory only — promotion never carries a destructive edit.
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: format!("Evidence-promoted route for {code}"),
            provenance: Provenance::Mined {
                confidence: ev.success_rate,
                support: ev.support,
                success_rate: ev.success_rate,
                first_seen: ev.first_seen.clone(),
                last_seen: ev.last_seen.clone(),
                source_report_hash: source_report_hash.to_string(),
            },
            priority: 1,
        });
    }
    routes
}

/// Run the mining + promotion pipeline against the log under `root`.
///
/// # Errors
/// Returns a [`ggen_graph::GraphError`] if projection or the SPARQL discovery
/// query fails.
pub fn mine(root: &Path) -> Result<MineReport, ggen_graph::GraphError> {
    let intel = IntelLog::at_root(root);
    let log = intel.read();
    let event_count = log.events.len();
    let source_log_hash = hash_file(intel.path());

    let graph = DeterministicGraph::new()?;
    EvidenceProjector::project_ocel(&graph, &log)?;

    let all_edges = discover_dfg(&graph, &episode_qualifier_iri())?;
    let mut failure_edges: Vec<DfgEdge> = all_edges
        .iter()
        .filter(|e| is_failure_target(&e.target))
        .cloned()
        .collect();
    failure_edges.sort_by_key(|e| std::cmp::Reverse(e.frequency));

    // Discovery report (human).
    let report_path = root
        .join(".ggen")
        .join("ocel")
        .join("discovery")
        .join("error-edge-mining.md");
    write_report(&report_path, event_count, &failure_edges, &all_edges)?;

    // Measure evidence → synthesize advisory mined routes → write artifact.
    let evidence = family_evidence(&log);
    let routes = synthesize_routes(&evidence, &source_log_hash);
    let promoted_count = routes.len();
    let promoted = PromotedRoutes {
        version: PromotedRoutes::VERSION,
        source_log_hash: source_log_hash.clone(),
        routes,
    };
    let promoted_path = default_pack_routes_path(root);
    write_promoted(&promoted_path, &promoted)
        .map_err(|e| ggen_graph::GraphError::Other(e.to_string()))?;

    // Promotion receipt: bind (source log, promoted artifact). R over R→O*→μ.
    let receipt_hex = emit_promotion_receipt(root, &source_log_hash, &promoted);

    // Promotion history ledger: one record per route per cycle, with status set
    // by diffing the prior cycle (demotion when a previously-Active route is no
    // longer promotable).
    append_history(root, &promoted, &source_log_hash, receipt_hex.as_deref());

    Ok(MineReport {
        event_count,
        failure_edges,
        all_edges,
        report_path,
        promoted_path,
        promoted_count,
    })
}

fn append_history(
    root: &Path, promoted: &PromotedRoutes, source_log_hash: &str, receipt_hex: Option<&str>,
) {
    use crate::intel::history::{PromotionHistory, RoutePromotionRecord, RouteStatus};
    use crate::route::is_promotable;

    let history = PromotionHistory::at_root(root);
    let prior = history.latest_by_route();
    let routes_json = serde_json::to_string(promoted).unwrap_or_default();
    let routes_hash = blake3::hash(routes_json.as_bytes()).to_hex().to_string();
    let promoted_at = chrono::Utc::now().to_rfc3339();

    let records: Vec<RoutePromotionRecord> = promoted
        .routes
        .iter()
        .map(|r| {
            let promotable = is_promotable(&r.provenance);
            let was_active = matches!(
                prior.get(&r.id.0).map(|p| p.status),
                Some(RouteStatus::Active)
            );
            let status = if promotable {
                RouteStatus::Active
            } else if was_active {
                RouteStatus::Demoted // was promoted before, no longer qualifies
            } else {
                RouteStatus::Quarantined // present but never cleared the gate
            };
            let (support, success_rate) = match &r.provenance {
                crate::route::Provenance::Mined {
                    support,
                    success_rate,
                    ..
                } => (*support, *success_rate),
                crate::route::Provenance::Seeded => (0, 0.0),
            };
            RoutePromotionRecord {
                route_id: r.id.0.clone(),
                route_source: "mined".to_string(),
                diagnostic_code: r.description.clone(),
                family: format!("{:?}", r.family),
                support,
                success_rate,
                source_log_hash: source_log_hash.to_string(),
                routes_hash: routes_hash.clone(),
                promotion_receipt: receipt_hex.map(str::to_string),
                promoted_at: promoted_at.clone(),
                supersedes: None,
                status,
            }
        })
        .collect();
    let _ = history.append(&records);
}

fn hash_file(path: &Path) -> String {
    let bytes = std::fs::read(path).unwrap_or_default();
    blake3::hash(&bytes).to_hex().to_string()
}

/// Write the promotion receipt; return its signature hex (for the history ledger).
fn emit_promotion_receipt(
    root: &Path, source_log_hash: &str, promoted: &PromotedRoutes,
) -> Option<String> {
    let routes_json = serde_json::to_string(promoted).unwrap_or_default();
    let pre: [u8; 32] = blake3::hash(source_log_hash.as_bytes()).into();
    let post: [u8; 32] = blake3::hash(routes_json.as_bytes()).into();
    let receipt = RepairReceipt::new(
        "promotion".to_string(),
        "PROMOTE-1".to_string(),
        pre,
        post,
        true,
    );
    let sig = receipt.signature_hex();
    let dir = root.join(".ggen").join("receipts");
    if std::fs::create_dir_all(&dir).is_ok() {
        let path = dir.join(format!("promotion-{}.json", &sig[..16]));
        if let Ok(json) = serde_json::to_string_pretty(&receipt) {
            let _ = std::fs::write(path, json);
        }
    }
    Some(sig)
}

fn write_report(
    path: &Path, event_count: usize, failure_edges: &[DfgEdge], all_edges: &[DfgEdge],
) -> Result<(), ggen_graph::GraphError> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| ggen_graph::GraphError::Other(e.to_string()))?;
    }
    let mut md = String::new();
    md.push_str("# Error-Edge Mining (ggen lsp mine)\n\n");
    md.push_str(&format!("Events analyzed: **{event_count}**\n\n"));
    md.push_str("## Top failure edges (80/20)\n\n");
    md.push_str("| source | → | target | frequency |\n|---|---|---|---|\n");
    if failure_edges.is_empty() {
        md.push_str("| _(none yet — insufficient evidence)_ | | | |\n");
    } else {
        for e in failure_edges {
            md.push_str(&format!(
                "| {} | → | {} | {} |\n",
                e.source, e.target, e.frequency
            ));
        }
    }
    md.push_str(&format!(
        "\n_Total directly-follows edges discovered: {}_\n",
        all_edges.len()
    ));
    std::fs::write(path, md).map_err(|e| ggen_graph::GraphError::Other(e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intel::events::{diagnostic_raised, gate_result, new_run_id, repair_suggested};
    use crate::route::{is_promotable, load_promoted};
    use tempfile::TempDir;

    #[test]
    fn mine_promotes_a_conformant_route_with_measured_success() {
        let dir = TempDir::new().expect("tempdir");
        let log = IntelLog::at_root(dir.path());
        // 3 episodes for E0024 that CLOSE (Raised → ... → GatePassed): high success.
        for i in 0..3 {
            let run = new_run_id();
            log.append(&[
                diagnostic_raised("t.tera", "E0024", "error", "0:0-0:1", &run, i * 3 + 1),
                repair_suggested("t.tera", "E0024", "template-failure", &run, i * 3 + 2),
                gate_result("t.tera", "E0024", true, 0, &run, i * 3 + 3),
            ])
            .expect("append");
        }

        let report = mine(dir.path()).expect("mine");
        assert!(report.promoted_count >= 1, "a route was promoted");
        assert!(report.promoted_path.is_file(), "repair-routes.json written");

        let promoted = load_promoted(&report.promoted_path).expect("load");
        let route = promoted
            .routes
            .iter()
            .find(|r| r.family == RepairFamily::TemplateFailure)
            .expect("template-failure route");
        // Measured success_rate should be high (all 3 episodes closed) → promotable.
        assert!(
            is_promotable(&route.provenance),
            "conformant route is promotable"
        );
    }

    #[test]
    fn mine_does_not_promote_failing_route() {
        let dir = TempDir::new().expect("tempdir");
        let log = IntelLog::at_root(dir.path());
        // 4 episodes that all FAIL the gate: success_rate 0 → not promotable.
        for i in 0..4 {
            let run = new_run_id();
            log.append(&[
                diagnostic_raised("t.tera", "E0024", "error", "0:0-0:1", &run, i * 2 + 1),
                gate_result("t.tera", "E0024", false, 1, &run, i * 2 + 2),
            ])
            .expect("append");
        }
        let report = mine(dir.path()).expect("mine");
        let promoted = load_promoted(&report.promoted_path).expect("load");
        let route = promoted
            .routes
            .iter()
            .find(|r| r.family == RepairFamily::TemplateFailure)
            .expect("route emitted (sub-threshold)");
        assert!(
            !is_promotable(&route.provenance),
            "never-closing route must NOT be promotable (success_rate 0)"
        );
    }

    #[test]
    fn promotion_emits_a_receipt() {
        let dir = TempDir::new().expect("tempdir");
        let log = IntelLog::at_root(dir.path());
        let run = new_run_id();
        log.append(&[diagnostic_raised(
            "t.tera", "E0024", "error", "0:0-0:1", &run, 1,
        )])
        .expect("append");
        mine(dir.path()).expect("mine");
        let receipts = std::fs::read_dir(dir.path().join(".ggen/receipts")).expect("dir");
        assert!(
            receipts
                .filter_map(Result::ok)
                .any(|e| e.file_name().to_string_lossy().starts_with("promotion-")),
            "a promotion receipt was written"
        );
    }
}
