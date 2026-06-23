#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! DEMOTE-1 — the system withdraws trust from a route that stops working.
//!
//! Real execution only. A ConfigValue route is genuinely promoted in cycle 1
//! (three editor rework closures → success 1.0 → Active), then in cycle 2 four
//! real headless failures drag its cumulative success below threshold. The next
//! mine marks it Demoted, churn rises, survival falls, and the registry resumes
//! the seed route. Promotion without demotion would be mythology; this proves the
//! lifecycle closes the loop.

use std::fs;
use std::path::{Path, PathBuf};

use ggen_lsp::intel::{MetricValue, PromotionHistory, RouteStatus};
use ggen_lsp::route::{default_pack_routes_path, RouteRegistry};
use ggen_lsp::state::ServerState;
use ggen_lsp::{check_content, check_files_in_root, compute_metrics, mine};
use lsp_max::lsp_types::Url;
use tempfile::TempDir;

fn url_from_path(path: impl AsRef<std::path::Path>) -> Url {
    url::Url::from_file_path(path.as_ref())
        .expect("absolute path")
        .to_string()
        .parse::<Url>()
        .expect("valid uri")
}

fn write(dir: &Path, name: &str, content: &str) -> PathBuf {
    let p = dir.join(name);
    fs::write(&p, content).expect("write");
    p
}

async fn editor_rework_config(state: &ServerState, root: &Path, name: &str) {
    let uri = url_from_path(root.join(name));
    let broken_diags = ggen_lsp::analyzers::build_analyzer(
        uri.path().as_str(),
        "[logging]\nlevel = \"verbose\"\n",
    )
    .map(|a| a.diagnostics())
    .unwrap_or_default();
    state.observe_diagnostics(&uri, &broken_diags).await;
    let fixed_diags =
        ggen_lsp::analyzers::build_analyzer(uri.path().as_str(), "[logging]\nlevel = \"info\"\n")
            .map(|a| a.diagnostics())
            .unwrap_or_default();
    state.observe_diagnostics(&uri, &fixed_diags).await;
}

#[tokio::test]
async fn promoted_route_demotes_when_it_stops_conforming() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // --- Cycle 1: the route WORKS — three editor fixes close E0023 (sr 1.0). ---
    let state = ServerState::with_root(root);
    for n in ["a", "b", "c"] {
        editor_rework_config(&state, root, &format!("c_{n}.toml")).await;
    }
    mine(root).expect("cycle 1 mine");

    let after1 = PromotionHistory::at_root(root).latest_by_route();
    let rec1 = after1
        .get("mined.config-value")
        .expect("config route promoted in cycle 1");
    assert_eq!(
        rec1.status,
        RouteStatus::Active,
        "a conformant route is Active"
    );
    let support1 = rec1.support;

    // --- Cycle 2: the route STOPS working — four real failures accumulate. ---
    let mut bad = Vec::new();
    for i in 0..4 {
        bad.push(write(
            root,
            &format!("bad{i}.toml"),
            "[logging]\nlevel = \"verbose\"\n",
        ));
    }
    check_files_in_root(root, &bad, true).capture(root);
    mine(root).expect("cycle 2 mine");

    // --- Trust is withdrawn. ---
    let after2 = PromotionHistory::at_root(root).latest_by_route();
    let rec2 = after2
        .get("mined.config-value")
        .expect("route still tracked after cycle 2");
    assert_eq!(
        rec2.status,
        RouteStatus::Demoted,
        "route is Demoted once cumulative success falls below threshold"
    );
    assert!(
        rec2.support > support1,
        "more episodes accumulated (support rose)"
    );

    let m = compute_metrics(root);
    assert!(
        matches!(m.promotion_churn, MetricValue::Value(v) if v > 0.0),
        "churn > 0 in the demoting cycle (got {:?})",
        m.promotion_churn
    );
    assert!(
        matches!(m.promotion_survival_rate, MetricValue::Value(v) if v < 1.0),
        "survival < 1 (got {:?})",
        m.promotion_survival_rate
    );

    // The registry stops selecting the demoted route — the seed resumes.
    let registry = RouteRegistry::seeded().with_pack_routes(&default_pack_routes_path(root));
    let probe = check_content(
        root.join("probe.toml").to_str().expect("utf8"),
        "[logging]\nlevel = \"verbose\"\n",
    )
    .expect("toml law surface");
    let chosen = registry
        .select_for_diagnostic(&probe.diagnostics[0])
        .expect("a route is selected");
    assert_eq!(
        chosen.id.0, "config.fix-enum-value",
        "the demoted mined route is no longer selected; the seed resumes"
    );
}
