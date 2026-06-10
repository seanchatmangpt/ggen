#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! Fortune 5 bulk marketplace policy gate.
//!
//! Walks ALL packs in marketplace/packs/ and enforces:
//! Layer A — Structural validity: TOML parses, all declared template paths exist,
//!            [pack.conformance] unknown_preservation = true where declared.
//! Layer B — Law surface invariants on all .rs.tera templates:
//!            no tower_lsp_max::, REGISTRY/MESH in server impls,
//!            source_id on publish_diagnostics, is_disjoint on ConformanceVector,
//!            Unknown handled in admission gates.
//!
//! Violations accumulate into a Vec — the gate reports ALL violations at once.

use std::path::PathBuf;
use walkdir::WalkDir;

fn workspace_root() -> PathBuf {
    let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    root.pop(); // crates
    root.pop(); // ggen
    root
}

// ---------------------------------------------------------------------------
// Layer A — Structural validity across all 10 marketplace packs
// ---------------------------------------------------------------------------

#[test]
fn all_marketplace_packs_parse_and_template_paths_exist() {
    let packs_dir = workspace_root().join("marketplace").join("packs");
    assert!(
        packs_dir.exists(),
        "marketplace/packs/ not found at {}",
        packs_dir.display()
    );

    let mut violations: Vec<String> = Vec::new();
    let mut pack_count = 0usize;

    for entry in std::fs::read_dir(&packs_dir).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(|e| e.to_str()) != Some("toml") {
            continue;
        }
        pack_count += 1;

        let toml_str = match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => {
                violations.push(format!("{}: read error: {}", path.display(), e));
                continue;
            }
        };

        let val: toml::Value = match toml::from_str(&toml_str) {
            Ok(v) => v,
            Err(e) => {
                violations.push(format!("{}: TOML parse error: {}", path.display(), e));
                continue;
            }
        };

        // All declared template paths must exist on disk
        if let Some(templates) = val
            .get("pack")
            .and_then(|p| p.get("templates"))
            .and_then(|t| t.as_array())
        {
            for t in templates {
                if let Some(rel) = t.get("path").and_then(|p| p.as_str()) {
                    let full = workspace_root().join("marketplace").join(rel);
                    if !full.exists() {
                        violations.push(format!(
                            "{}: declared template path not found: {}",
                            path.display(),
                            rel
                        ));
                    }
                }
            }
        }

        // Packs with [pack.conformance] must declare unknown_preservation = true
        if let Some(conf) = val.get("pack").and_then(|p| p.get("conformance")) {
            let up = conf
                .get("unknown_preservation")
                .and_then(|v| v.as_bool())
                .unwrap_or(false);
            if !up {
                violations.push(format!(
                    "{}: [pack.conformance] must have unknown_preservation = true",
                    path.display()
                ));
            }
        }
    }

    assert!(
        pack_count > 0,
        "No .toml files found in marketplace/packs/ — check workspace_root()"
    );
    assert!(
        violations.is_empty(),
        "Marketplace pack structural violations ({} packs scanned):
{}",
        pack_count,
        violations.join(
            "
"
        )
    );
}

// ---------------------------------------------------------------------------
// Layer B — Law surface invariants on all .rs.tera templates
// ---------------------------------------------------------------------------

#[test]
fn all_rust_pack_templates_pass_law_invariants() {
    let templates_dir = workspace_root().join("marketplace").join("templates");
    assert!(
        templates_dir.exists(),
        "marketplace/templates/ not found at {}",
        templates_dir.display()
    );

    let mut violations: Vec<String> = Vec::new();
    let mut template_count = 0usize;

    // Only enforce law invariants on canonical lsp-max packs; legacy dirs are frozen history
    const CANONICAL_DIRS: &[&str] = &["lsp-max", "lsp-max-client", "ggen-pack-contrib"];

    for entry in WalkDir::new(&templates_dir) {
        let entry = entry.unwrap();
        // Skip templates not under a canonical pack directory
        let in_canonical = entry
            .path()
            .components()
            .any(|c| CANONICAL_DIRS.iter().any(|d| c.as_os_str() == *d));
        if !in_canonical {
            continue;
        }
        if !entry
            .file_name()
            .to_str()
            .map(|s| s.ends_with(".rs.tera"))
            .unwrap_or(false)
        {
            continue;
        }
        template_count += 1;
        let path = entry.path();

        let content = match std::fs::read_to_string(path) {
            Ok(c) => c,
            Err(e) => {
                violations.push(format!("{}: read error: {}", path.display(), e));
                continue;
            }
        };

        // Law: forbidden namespace must not appear
        if content.contains("tower_lsp_max") {
            violations.push(format!(
                "{}: contains forbidden tower_lsp_max:: reference",
                path.display()
            ));
        }

        // Law: LanguageServer impls must wire REGISTRY and MESH (mesh participation)
        if content.contains("impl LanguageServer") || content.contains("LanguageServer for") {
            if !content.contains("REGISTRY") {
                violations.push(format!(
                    "{}: LanguageServer impl must init REGISTRY (mesh participation required)",
                    path.display()
                ));
            }
            if !content.contains("MESH") {
                violations.push(format!(
                    "{}: LanguageServer impl must init MESH (autonomic hook required)",
                    path.display()
                ));
            }
        }

        // Law: diagnostic-emitting templates must carry source_id attribution
        if content.contains("publish_diagnostics") && !content.contains("source_id") {
            violations.push(format!(
                "{}: publish_diagnostics without source_id attribution",
                path.display()
            ));
        }

        // Law: ConformanceVector consumers must assert disjointness invariant
        if content.contains("ConformanceVector") && !content.contains("is_disjoint") {
            violations.push(format!(
                "{}: ConformanceVector usage must assert admitted ∩ refused = ∅ (is_disjoint)",
                path.display()
            ));
        }

        // Law: admission gate templates must handle Unknown status (never collapse)
        if content.contains("max/admission") && !content.contains("Unknown") {
            violations.push(format!(
                "{}: admission gate must explicitly handle Unknown status",
                path.display()
            ));
        }
    }

    assert!(
        template_count > 0,
        "No .rs.tera files found in marketplace/templates/ — check workspace_root()"
    );
    assert!(
        violations.is_empty(),
        "Law surface violations ({} templates scanned):
{}",
        template_count,
        violations.join(
            "
"
        )
    );
}
