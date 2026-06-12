#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
)]
//! Three-layer testing system for lsp-max marketplace packs.
//!
//! Layer 1 — Manifest validity: TOML loads, template paths exist on disk.
//! Layer 2 — Render correctness: templates render, no unrendered markers,
//!            key structural tokens present (lsp_max::, REGISTRY, #[verb], etc.).
//! Layer 3 — Compilation proof: rendered backend.rs + cli.rs scaffold a
//!            complete Cargo.toml in a tempdir and pass cargo check.
//!            (Ignored by default; run with --include-ignored.)

use std::path::PathBuf;
use tera::{Context, Tera};

// ---------------------------------------------------------------------------
// Paths
// ---------------------------------------------------------------------------

fn marketplace_root() -> PathBuf {
    // This file is at tests/integration/packs/ inside the ggen workspace root
    let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    root.push("marketplace");
    root
}

fn pack_path(name: &str) -> PathBuf {
    marketplace_root().join("packs").join(name)
}

fn template_path(name: &str) -> PathBuf {
    marketplace_root().join("templates").join(name)
}

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

fn render_template(path: &PathBuf, vars: &[(&str, &str)]) -> String {
    let content = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("cannot read {}: {}", path.display(), e));
    let mut tera = Tera::default();
    tera.add_raw_template("t", &content).unwrap();
    let mut ctx = Context::new();
    for (k, v) in vars {
        ctx.insert(*k, *v);
    }
    tera.render("t", &ctx)
        .unwrap_or_else(|e| panic!("tera render failed for {}: {}", path.display(), e))
}

// ---------------------------------------------------------------------------
// Layer 1 — Manifest validity
// ---------------------------------------------------------------------------

#[test]
fn test_lsp_max_pack_manifest_loads() {
    let path = pack_path("lsp-max.toml");
    assert!(path.exists(), "lsp-max.toml not found at {}", path.display());

    let toml_str = std::fs::read_to_string(&path).unwrap();
    let val: toml::Value = toml::from_str(&toml_str)
        .unwrap_or_else(|e| panic!("lsp-max.toml parse error: {}", e));

    let pack = &val["pack"];
    assert_eq!(pack["id"].as_str().unwrap(), "lsp-max");
    assert_eq!(pack["version"].as_str().unwrap(), "26.6.9");

    let templates = pack["templates"].as_array().unwrap();
    assert_eq!(templates.len(), 4, "expected 4 templates in lsp-max pack");

    for t in templates {
        let rel = t["path"].as_str().unwrap();
        let full = marketplace_root().join(rel);
        assert!(full.exists(), "declared template path not found: {}", rel);
    }
}

#[test]
fn test_lsp_max_client_pack_manifest_loads() {
    let path = pack_path("lsp-max-client.toml");
    assert!(path.exists(), "lsp-max-client.toml not found at {}", path.display());

    let toml_str = std::fs::read_to_string(&path).unwrap();
    let val: toml::Value = toml::from_str(&toml_str)
        .unwrap_or_else(|e| panic!("lsp-max-client.toml parse error: {}", e));

    let pack = &val["pack"];
    assert_eq!(pack["id"].as_str().unwrap(), "lsp-max-client");
    assert_eq!(pack["version"].as_str().unwrap(), "26.6.9");

    let templates = pack["templates"].as_array().unwrap();
    assert_eq!(templates.len(), 2, "expected 2 templates in lsp-max-client pack");

    for t in templates {
        let rel = t["path"].as_str().unwrap();
        let full = marketplace_root().join(rel);
        assert!(full.exists(), "declared template path not found: {}", rel);
    }
}

// ---------------------------------------------------------------------------
// Layer 2 — Render correctness (server pack templates)
// ---------------------------------------------------------------------------

#[test]
fn test_backend_template_renders_correctly() {
    let path = template_path("lsp-max/backend.rs.tera");
    let out = render_template(&path, &[
        ("server_name", "myserver"),
        ("diagnostic_source_id", "myserver_lsp_observer"),
    ]);
    assert!(!out.contains("{{"), "unrendered Tera markers remain");
    assert!(out.contains("lsp_max::"), "missing lsp_max:: import");
    assert!(out.contains("REGISTRY"), "missing REGISTRY init");
    assert!(out.contains("MESH"), "missing MESH init");
    assert!(out.contains("source_id"), "missing source_id attribution");
    assert!(!out.contains("tower_lsp_max"), "forbidden tower_lsp_max reference present");
    // server_name="myserver" capitalized → "Myserver"
    assert!(out.contains("MyserverBackend"), "struct name not derived from server_name");
}

#[test]
fn test_cli_template_renders_correctly() {
    let path = template_path("lsp-max/cli.rs.tera");
    let out = render_template(&path, &[("server_name", "myserver")]);
    assert!(!out.contains("{{"), "unrendered Tera markers remain");
    assert!(out.contains("#[verb("serve")]"), "missing #[verb(serve)] binding");
    assert!(out.contains("lsp_max::"), "missing lsp_max:: import");
    assert!(!out.contains("tower_lsp_max"), "forbidden tower_lsp_max reference");
    assert!(out.contains("MyserverBackend"), "struct name not derived from server_name");
}

#[test]
fn test_semantics_template_renders_correctly() {
    let path = template_path("lsp-max/semantics.rs.tera");
    let out = render_template(&path, &[
        ("server_name", "myserver"),
        ("rules", ""),
    ]);
    assert!(!out.contains("{{"), "unrendered Tera markers remain");
    assert!(out.contains("lsp_max::"), "missing lsp_max:: import");
    assert!(!out.contains("tower_lsp_max"), "forbidden tower_lsp_max reference");
}

#[test]
fn test_build_template_renders_correctly() {
    let path = template_path("lsp-max/build.rs.tera");
    let out = render_template(&path, &[("tree_sitter_crate", "tree_sitter_rust")]);
    assert!(!out.contains("{{"), "unrendered Tera markers remain");
    assert!(!out.contains("tower_lsp_max"), "forbidden tower_lsp_max reference");
    // build.rs must be minimal when tree-sitter codegen is commented out
    assert!(out.contains("fn main()"), "missing main() in build.rs output");
}

// ---------------------------------------------------------------------------
// Layer 2 — Render correctness (client pack templates)
// ---------------------------------------------------------------------------

#[test]
fn test_conformance_client_template_renders_correctly() {
    let path = template_path("lsp-max-client/conformance_client.rs.tera");
    let out = render_template(&path, &[("client_name", "myclient")]);
    assert!(!out.contains("{{"), "unrendered Tera markers remain");
    assert!(out.contains("ConformanceVector"), "missing ConformanceVector");
    assert!(out.contains("is_disjoint"), "ConformanceVector invariant assertion missing");
    assert!(out.contains("max/snapshot"), "missing max/snapshot call");
    assert!(out.contains("max/conformanceVector"), "missing max/conformanceVector call");
}

#[test]
fn test_admission_gate_template_renders_correctly() {
    let path = template_path("lsp-max-client/admission_gate.rs.tera");
    let out = render_template(&path, &[
        ("client_name", "myclient"),
        ("gate_axis", "Domain"),
    ]);
    assert!(!out.contains("{{"), "unrendered Tera markers remain");
    assert!(out.contains("Unknown"), "missing Unknown arm (strict-mode block)");
    assert!(out.contains("strict"), "missing strict parameter");
    assert!(out.contains("BLOCKED") || out.contains("REFUSED"),
        "missing gate status strings");
    assert!(out.contains("max/admission"), "missing max/admission call");
}

// ---------------------------------------------------------------------------
// Layer 3 — Compilation proof
// ---------------------------------------------------------------------------

#[test]
#[ignore = "requires crates.io network + cargo toolchain; run with --include-ignored"]
fn test_lsp_max_scaffold_compiles() {
    use tempfile::TempDir;
    let tmp = TempDir::new().unwrap();
    let src = tmp.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    let backend = render_template(
        &template_path("lsp-max/backend.rs.tera"),
        &[("server_name", "scaffold"), ("diagnostic_source_id", "scaffold_lsp_observer")],
    );
    let cli_src = render_template(
        &template_path("lsp-max/cli.rs.tera"),
        &[("server_name", "scaffold")],
    );

    std::fs::write(src.join("backend.rs"), &backend).unwrap();
    std::fs::write(src.join("cli.rs"), &cli_src).unwrap();
    std::fs::write(src.join("lib.rs"), "pub mod backend;
pub mod cli;
").unwrap();

    std::fs::write(tmp.path().join("Cargo.toml"), r#"
[package]
name = "lsp-max-pack-scaffold-test"
version = "0.1.0"
edition = "2021"

[dependencies]
lsp-max = "26.6.9"
lsp-max-protocol = "26.6.9"
clap-noun-verb = "26.6.2"
clap-noun-verb-macros = "26.6.2"
linkme = "0.3"
tokio = { version = "1", features = ["full"] }
serde_json = "1"
"#).unwrap();

    let output = std::process::Command::new("cargo")
        .args(["check", "--message-format=short"])
        .current_dir(tmp.path())
        .output()
        .expect("cargo check failed to spawn");

    assert!(
        output.status.success(),
        "scaffold cargo check REFUSED:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}
