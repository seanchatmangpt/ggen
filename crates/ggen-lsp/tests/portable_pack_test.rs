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
//! PORTABLE-PACK-1 — an admissibility (route) pack survives substitution into
//! another repo: the same failure family resolves to the same route, repo B emits
//! its OWN receipted proof, and the artifact carries no path-specific coupling.

use std::fs;
use std::path::Path;

use ggen_lsp::intel::events::activity;
use ggen_lsp::route::{default_pack_routes_path, load_promoted, RouteRegistry};
use ggen_lsp::{check_content, check_files_in_root, mine, IntelLog};
use tempfile::TempDir;

/// Drive 3 real warning episodes (E0011) → promote `mined.template-failure`.
fn promote_template_route(root: &Path) {
    let mut files = Vec::new();
    for i in 0..3 {
        let p = root.join(format!("q{i}.rq"));
        fs::write(&p, "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n").expect("write");
        files.push(p);
    }
    check_files_in_root(root, &files, true).capture(root);
    mine(root).expect("mine");
}

#[test]
fn pack_resolves_same_family_in_a_different_repo_with_independent_proof() {
    // --- Repo A: earn a promoted route. ---
    let repo_a = TempDir::new().expect("tempdir a");
    promote_template_route(repo_a.path());
    let pack_a = default_pack_routes_path(repo_a.path());
    let promoted_a = load_promoted(&pack_a).expect("A promoted a route");
    let route_a = promoted_a
        .routes
        .iter()
        .find(|r| r.id.0 == "mined.template-failure")
        .expect("A has the template-failure route");

    // The artifact must carry no absolute-path coupling to repo A.
    let artifact = fs::read_to_string(&pack_a).expect("read A artifact");
    let a_path = repo_a.path().to_string_lossy().to_string();
    assert!(
        !artifact.contains(&a_path),
        "promoted-route artifact must not embed repo A's path"
    );

    // --- Repo B: install A's pack (a plain file copy). ---
    let repo_b = TempDir::new().expect("tempdir b");
    let pack_b = default_pack_routes_path(repo_b.path());
    fs::create_dir_all(pack_b.parent().expect("parent")).expect("mkdir");
    fs::copy(&pack_a, &pack_b).expect("install pack into B");

    // Same failure family → same route id (no drift across repos).
    let registry_b = RouteRegistry::seeded().with_pack_routes(&pack_b);
    let diag = &check_content("probe.rq", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n")
        .expect("rq law surface")
        .diagnostics[0];
    let chosen = registry_b
        .select_for_diagnostic(diag)
        .expect("B selects a route");
    assert_eq!(
        chosen.id.0, route_a.id.0,
        "the same family resolves to the same route id in repo B"
    );

    // --- Repo B emits its OWN receipted proof using the installed route. ---
    let b_rq = {
        let p = repo_b.path().join("b.rq");
        fs::write(&p, "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n").expect("write");
        vec![p]
    };
    check_files_in_root(repo_b.path(), &b_rq, true).capture(repo_b.path());

    let log_b = IntelLog::at_root(repo_b.path()).read();
    let used_mined = log_b.events.iter().any(|e| {
        e.activity == activity::ROUTE_SELECTED
            && e.attributes.get("route").map(String::as_str) == Some("mined.template-failure")
            && e.attributes.get("route_source").map(String::as_str) == Some("mined")
    });
    assert!(used_mined, "repo B used the installed mined route");
    assert!(
        log_b
            .events
            .iter()
            .any(|e| e.activity == activity::RECEIPT_EMITTED),
        "repo B emitted its own independent receipt"
    );
    // B's proof lives under B's own root, distinct from A's pack.
    assert_ne!(pack_b, pack_a, "B has its own installed pack copy");
}
