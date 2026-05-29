//! REMOTE-PACK-1 — a pack advertises its movable route-law surface (manifest),
//! is version/canon-checked, binds its content hash, and carries no path coupling.

use std::fs;
use std::path::Path;

use ggen_lsp::{
    check_files_in_root, emit_pack, load_manifest, manifest_is_current, mine, pack_hash_at,
    PackOptions,
};
use tempfile::TempDir;

const E0011_SRC: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";

/// Promote a route so the pack has something to advertise.
fn promote_route(root: &Path) {
    let mut files = Vec::new();
    for i in 0..3 {
        let p = root.join(format!("q{i}.rq"));
        fs::write(&p, E0011_SRC).expect("write");
        files.push(p);
    }
    check_files_in_root(root, &files, true).capture(root);
    mine(root).expect("mine");
}

#[test]
fn manifest_advertises_routes_policies_and_binds_pack_hash() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    promote_route(root); // writes .agent-admissibility/powl/repair-routes.json

    let out = root.join(".agent-admissibility");
    let report = emit_pack(&PackOptions {
        agents: vec!["generic".to_string()],
        out_dir: out.clone(),
        scan_hash: None,
    })
    .expect("emit");

    // The advertised manifest exists and binds the pack hash.
    let manifest = load_manifest(&out).expect("manifest present");
    assert_eq!(
        manifest.pack_hash, report.pack_hash,
        "manifest binds the pack hash"
    );
    assert!(
        manifest_is_current(&manifest),
        "freshly emitted pack is current"
    );

    // It advertises the promoted route + the SHACL policies + law surfaces.
    assert!(
        manifest
            .routes
            .iter()
            .any(|r| r.route_id == "mined.template-failure"),
        "manifest advertises the promoted route"
    );
    assert!(
        !manifest.policies.is_empty(),
        "manifest advertises policies (hashed)"
    );
    assert!(manifest.policies.iter().all(|p| !p.hash.is_empty()));
    assert!(!manifest.law_surfaces.is_empty());

    // pack_hash_at(project_root) resolves the installed pack's hash (route binding).
    assert_eq!(
        pack_hash_at(root).as_deref(),
        Some(manifest.pack_hash.as_str())
    );

    // No absolute-path coupling — the manifest is remote-safe.
    let raw = fs::read_to_string(out.join("pack-manifest.json")).expect("read manifest");
    assert!(
        !raw.contains(root.to_str().expect("utf8")),
        "manifest must not embed the project's absolute path"
    );
}

#[test]
fn stale_or_future_manifest_is_rejected() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    let out = root.join(".agent-admissibility");
    emit_pack(&PackOptions {
        agents: vec!["generic".to_string()],
        out_dir: out.clone(),
        scan_hash: None,
    })
    .expect("emit");

    let mut manifest = load_manifest(&out).expect("manifest");
    assert!(manifest_is_current(&manifest));

    // A version bump (future pack) or canon drift fails the staleness guard.
    manifest.version = manifest.version.wrapping_add(1);
    assert!(
        !manifest_is_current(&manifest),
        "future schema version rejected"
    );

    let mut drifted = load_manifest(&out).expect("manifest");
    drifted.canon = "v0.0.0".to_string();
    assert!(!manifest_is_current(&drifted), "canon drift rejected");
}
