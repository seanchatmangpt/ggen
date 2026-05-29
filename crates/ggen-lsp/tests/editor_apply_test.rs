//! APPLY-1 — the editor observes *applied* repairs, not only proposed routes.
//!
//! Drives the real `ServerState` editor-observation path (the same methods the
//! `did_open`/`did_change` handlers call) with REAL analyzer diagnostics from
//! `check_content`. The "agent applied the fix" is modeled as the document's next
//! state having the diagnostic gone — exactly what `did_change` delivers. Proves
//! a `RepairApplied → GatePassed → ReceiptEmitted` rework closure is emitted and
//! becomes visible to the metrics layer.

use ggen_lsp::intel::events::activity;
use ggen_lsp::intel::MetricValue;
use ggen_lsp::state::ServerState;
use ggen_lsp::{check_content, compute_metrics, IntelLog};
use tempfile::TempDir;
use tower_lsp::lsp_types::Url;

#[tokio::test]
async fn editor_applied_repair_emits_repair_applied_and_closes_episode() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    let state = ServerState::with_root(root);

    let cfg_path = root.join("ggen.toml");
    let uri = Url::from_file_path(&cfg_path).expect("file url");

    // 1) Open broken: invalid enum `level = "verbose"` → E0023 (ConfigValue).
    //    A route is offered (advisory seed) and remembered as a pending repair.
    let broken =
        check_content(uri.path(), "[logging]\nlevel = \"verbose\"\n").expect("toml law surface");
    assert!(
        broken.diagnostics.iter().any(|d| matches!(
            &d.code,
            Some(tower_lsp::lsp_types::NumberOrString::String(c)) if c == "E0023"
        )),
        "broken config raises E0023"
    );
    state.observe_diagnostics(&uri, &broken.diagnostics).await;

    // 2) The agent applies the fix: `level = "info"` → E0023 disappears.
    let fixed =
        check_content(uri.path(), "[logging]\nlevel = \"info\"\n").expect("toml law surface");
    assert!(fixed.diagnostics.is_empty(), "fixed config is clean");
    state.observe_diagnostics(&uri, &fixed.diagnostics).await;

    // The OCEL log records the APPLIED repair, not merely the proposal.
    let log = IntelLog::at_root(root).read();
    let count = |a: &str| log.events.iter().filter(|e| e.activity == a).count();
    assert!(count(activity::DIAGNOSTIC_RAISED) >= 1, "diagnostic raised");
    assert!(count(activity::ROUTE_SELECTED) >= 1, "route offered");
    assert_eq!(
        count(activity::REPAIR_APPLIED),
        1,
        "applied repair observed once"
    );
    assert!(count(activity::GATE_PASSED) >= 1, "rework closed the gate");
    assert!(
        count(activity::RECEIPT_EMITTED) >= 1,
        "closed episode receipted"
    );

    // RepairApplied is bound to the right diagnostic code + route id.
    let applied = log
        .events
        .iter()
        .find(|e| e.activity == activity::REPAIR_APPLIED)
        .expect("a RepairApplied event");
    assert!(
        applied.objects.iter().any(|o| o.id == "E0023"),
        "RepairApplied bound to E0023"
    );
    assert_eq!(
        applied.attributes.get("route").map(String::as_str),
        Some("config.fix-enum-value"),
        "RepairApplied carries the offered route id"
    );

    // The editor rework episode is now visible to the metrics layer — an
    // apply-dependent metric is computable, no longer refused.
    let m = compute_metrics(root);
    assert_ne!(
        m.receipt_density,
        MetricValue::InsufficientEvidence,
        "editor rework produced computable apply-dependent evidence"
    );
}

#[tokio::test]
async fn unrepaired_diagnostic_emits_no_repair_applied() {
    // Negative control: a diagnostic that never disappears yields no RepairApplied.
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    let state = ServerState::with_root(root);
    let uri = Url::from_file_path(root.join("ggen.toml")).expect("file url");

    let broken =
        check_content(uri.path(), "[logging]\nlevel = \"verbose\"\n").expect("toml law surface");
    state.observe_diagnostics(&uri, &broken.diagnostics).await;
    // Re-publish the SAME broken diagnostics (no fix applied).
    state.observe_diagnostics(&uri, &broken.diagnostics).await;

    let log = IntelLog::at_root(root).read();
    assert_eq!(
        log.events
            .iter()
            .filter(|e| e.activity == activity::REPAIR_APPLIED)
            .count(),
        0,
        "no fix applied ⇒ no RepairApplied (no fabricated closure)"
    );
    // And it must not double-raise the persistent diagnostic.
    assert_eq!(
        log.events
            .iter()
            .filter(|e| e.activity == activity::DIAGNOSTIC_RAISED)
            .count(),
        1,
        "a persistent diagnostic is raised once, not on every publish"
    );
}
