//! Replay — reconstruct a case from logs + receipts + artifact.
//!
//! "If replay cannot reconstruct the claim, the claim is not done." Two surfaces:
//! - [`replay_case`] rebuilds one episode (events, route, source, gate outcome,
//!   conformance, receipt) from the OCEL log.
//! - [`verify_promotion`] re-derives the promotion binding (source-log-hash →
//!   routes-hash) and checks it against the stored promotion receipt; a mutated
//!   log or artifact ⇒ mismatch.

use std::path::Path;

use serde::Serialize;

use super::events::{activity, obj_type};
use super::log::IntelLog;
use crate::route::{default_pack_routes_path, PromotedRoutes};

/// Reconstruction of a single case (episode) from the event log.
#[derive(Debug, Clone, Serialize)]
pub struct CaseReplay {
    pub case_id: String,
    pub found: bool,
    pub diagnostic_code: Option<String>,
    pub route_id: Option<String>,
    pub route_source: Option<String>,
    pub gate_outcome: Option<String>,
    pub conformant: bool,
    pub receipt_id: Option<String>,
    pub event_count: usize,
}

/// Rebuild a case by its episode id from the OCEL log (deterministic).
#[must_use]
pub fn replay_case(root: &Path, case_id: &str) -> CaseReplay {
    let log = IntelLog::at_root(root).read();
    let events: Vec<_> = log
        .events
        .iter()
        .filter(|e| {
            e.objects
                .iter()
                .any(|o| o.r#type == obj_type::EPISODE && o.id == case_id)
        })
        .collect();

    if events.is_empty() {
        return CaseReplay {
            case_id: case_id.to_string(),
            found: false,
            diagnostic_code: None,
            route_id: None,
            route_source: None,
            gate_outcome: None,
            conformant: false,
            receipt_id: None,
            event_count: 0,
        };
    }

    let attr = |act: &str, key: &str| {
        events
            .iter()
            .find(|e| e.activity == act)
            .and_then(|e| e.attributes.get(key).cloned())
    };
    let diagnostic_code = events
        .iter()
        .find_map(|e| {
            e.objects
                .iter()
                .find(|o| o.r#type == obj_type::DIAGNOSTIC_CODE)
        })
        .map(|o| o.id.clone());
    let gate_outcome = if events.iter().any(|e| e.activity == activity::GATE_PASSED) {
        Some("GatePassed".to_string())
    } else if events.iter().any(|e| e.activity == activity::GATE_FAILED) {
        Some("GateFailed".to_string())
    } else {
        None
    };
    let conformant = {
        let r = events
            .iter()
            .filter(|e| e.activity == activity::DIAGNOSTIC_RAISED)
            .map(|e| e.timestamp)
            .min();
        let p = events
            .iter()
            .filter(|e| e.activity == activity::GATE_PASSED)
            .map(|e| e.timestamp)
            .max();
        matches!((r, p), (Some(r), Some(p)) if r <= p)
    };

    CaseReplay {
        case_id: case_id.to_string(),
        found: true,
        diagnostic_code,
        route_id: attr(activity::ROUTE_SELECTED, "route"),
        route_source: attr(activity::ROUTE_SELECTED, "route_source"),
        gate_outcome,
        conformant,
        receipt_id: attr(activity::RECEIPT_EMITTED, "receipt_id"),
        event_count: events.len(),
    }
}

/// Verification of the promotion binding against the stored receipt.
#[derive(Debug, Clone, Serialize)]
pub struct PromotionReplay {
    pub matches: bool,
    pub reason: String,
}

/// Re-derive `(source_log_hash → routes_hash)` from the current log + artifact
/// and check it against the latest promotion receipt.
///
/// A mutated log or artifact
/// breaks the match — the replay/tamper proof for the `R → O* → μ` step.
#[must_use]
pub fn verify_promotion(root: &Path) -> PromotionReplay {
    // Current source-log hash + artifact hash.
    let log_path = IntelLog::at_root(root);
    let src_bytes = std::fs::read(log_path.path()).unwrap_or_default();
    let source_log_hash = blake3::hash(&src_bytes).to_hex().to_string();

    let artifact_path = default_pack_routes_path(root);
    let Some(promoted) = crate::route::load_promoted(&artifact_path) else {
        return PromotionReplay {
            matches: false,
            reason: "no promoted-route artifact".to_string(),
        };
    };
    // The artifact records the source_log_hash it was mined from.
    if promoted.source_log_hash != source_log_hash {
        return PromotionReplay {
            matches: false,
            reason: format!(
                "log hash mismatch: artifact={} current={}",
                &promoted.source_log_hash[..8.min(promoted.source_log_hash.len())],
                &source_log_hash[..8.min(source_log_hash.len())]
            ),
        };
    }
    // Recompute the receipt binding and find a matching receipt.
    let routes_json = serde_json::to_string(&PromotedRoutes {
        version: promoted.version,
        source_log_hash: promoted.source_log_hash.clone(),
        routes: promoted.routes.clone(),
    })
    .unwrap_or_default();
    let pre: [u8; 32] = blake3::hash(source_log_hash.as_bytes()).into();
    let post: [u8; 32] = blake3::hash(routes_json.as_bytes()).into();
    let matches = find_promotion_receipt(root, pre, post);
    PromotionReplay {
        matches,
        reason: if matches {
            "promotion receipt reconstructs from current log + artifact".to_string()
        } else {
            "no receipt binds the current log+artifact (tampered or stale)".to_string()
        },
    }
}

fn find_promotion_receipt(root: &Path, pre: [u8; 32], post: [u8; 32]) -> bool {
    let dir = root.join(".ggen").join("receipts");
    let Ok(entries) = std::fs::read_dir(&dir) else {
        return false;
    };
    for entry in entries.flatten() {
        if !entry
            .file_name()
            .to_string_lossy()
            .starts_with("promotion-")
        {
            continue;
        }
        let Ok(content) = std::fs::read_to_string(entry.path()) else {
            continue;
        };
        if let Ok(r) = serde_json::from_str::<super::receipt::RepairReceipt>(&content) {
            if r.pre_state_hash == pre && r.post_state_hash == post && r.verify() {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intel::events::{
        diagnostic_raised, gate_result, new_run_id, receipt_emitted, route_selected,
    };
    use crate::intel::IntelLog;
    use tempfile::TempDir;

    #[test]
    fn replay_reconstructs_a_closed_case() {
        let dir = TempDir::new().expect("tempdir");
        let log = IntelLog::at_root(dir.path());
        let run = new_run_id();
        log.append(&[
            diagnostic_raised("r.rq", "E0011", "warning", "0:0-0:1", &run, 1),
            route_selected("r.rq", "E0011", "mined.template-failure", "mined", &run, 2),
            gate_result("r.rq", "E0011", true, 0, &run, 3),
            receipt_emitted("r.rq", "E0011", "rcpt1", &run, 4),
        ])
        .expect("append");
        // Episode id = blake3(file|code|run)[..16] — recompute the same way.
        let case_id = {
            let mut h = blake3::Hasher::new();
            h.update(b"r.rq");
            h.update(b"E0011");
            h.update(run.as_bytes());
            h.finalize().to_hex()[..16].to_string()
        };
        let rep = replay_case(dir.path(), &case_id);
        assert!(rep.found);
        assert_eq!(rep.route_id.as_deref(), Some("mined.template-failure"));
        assert_eq!(rep.route_source.as_deref(), Some("mined"));
        assert_eq!(rep.gate_outcome.as_deref(), Some("GatePassed"));
        assert!(rep.conformant);
        assert_eq!(rep.receipt_id.as_deref(), Some("rcpt1"));
    }

    #[test]
    fn promotion_verify_matches_then_tamper_breaks_it() {
        let dir = TempDir::new().expect("tempdir");
        let log = IntelLog::at_root(dir.path());
        let run = new_run_id();
        log.append(&[
            diagnostic_raised("t.tera", "E0024", "error", "0:0-0:1", &run, 1),
            gate_result("t.tera", "E0024", false, 1, &run, 2),
        ])
        .expect("append");
        crate::intel::mine(dir.path()).expect("mine");

        assert!(
            verify_promotion(dir.path()).matches,
            "fresh promotion replays"
        );

        // Tamper the OCEL log → source hash changes → binding no longer matches.
        use std::io::Write;
        let mut f = std::fs::OpenOptions::new()
            .append(true)
            .open(IntelLog::at_root(dir.path()).path())
            .expect("open");
        writeln!(f, "{{\"id\":\"x\",\"activity\":\"Tampered\",\"timestamp\":\"2026-05-28T00:00:00+00:00\",\"objects\":[],\"attributes\":{{}}}}").expect("tamper");
        assert!(
            !verify_promotion(dir.path()).matches,
            "tampered log breaks the replay"
        );
    }
}
