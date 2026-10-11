#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Chicago tests for `ggen_capability_status` (star-toml migration +
//! additive `capabilities` key).
//!
//! Real fixtures only: every test writes a real `ggen.toml` (and, where
//! relevant, a real `<root>/packs/<name>/pack.toml`) into a `TempDir` and
//! calls the actual `capability_status` function. No mocks, no test
//! doubles. Assertions land on observable state: the serialized JSON
//! string (so `skip_serializing_if` is proven on the wire format, not the
//! struct) and typed error categories.

use ggen_mcp::error::ErrorCategory;
use ggen_mcp::tools::capability_status::{capability_status, CapabilityStatusParams};

fn write_file(path: &std::path::Path, content: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("create fixture parent dir");
    }
    std::fs::write(path, content).expect("write fixture file");
}

fn run(root: &std::path::Path) -> Result<String, ggen_mcp::error::McpError> {
    let result = capability_status(&CapabilityStatusParams {
        root: root.display().to_string(),
    })?;
    // Serialize the whole result so assertions see the real wire format.
    serde_json::to_string(&result).map_err(|e| {
        ggen_mcp::error::McpError::new(
            ggen_mcp::error::ErrorCategory::GraphLoadError,
            format!("test serialization failed: {e}"),
        )
    })
}

/// (1) star-toml migration: `${VAR}` in ggen.toml is expanded at parse
/// time, proven end-to-end -- the expanded pack name is what the tool
/// resolves against the fixture marketplace (`annotated_packs` names the
/// expanded name, not the literal `${...}` text).
#[test]
fn env_var_in_manifest_is_expanded_and_reaches_tool_output() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();

    write_file(
        &root.join("ggen.toml"),
        r#"
[[generation.rules]]
name = "expanding_rule"
[generation.rules.template]
pack = "${GGEN_MCP_CAPTEST_PACK}"
"#,
    );
    write_file(
        &root.join("packs").join("expack").join("pack.toml"),
        "[pack]\nname = \"expack\"\n[capabilities]\nprovides = [\"urn:test:prov\"]\n",
    );

    // SAFETY(test): single-threaded test body; unique var name avoids
    // cross-test contention on the process env.
    std::env::set_var("GGEN_MCP_CAPTEST_PACK", "expack");

    let json = run(root).expect("tool should succeed");
    assert!(
        json.contains("\"annotated_packs\":[\"expack\"]"),
        "expanded pack name should surface in annotated_packs, got: {json}"
    );
    assert!(
        json.contains("urn:test:prov"),
        "expanded pack's provides should reach capabilities, got: {json}"
    );
    std::env::remove_var("GGEN_MCP_CAPTEST_PACK");
}

/// (2) `capabilities` present exactly when a referenced pack carries
/// `[capabilities]`; absent (not null) otherwise -- `skip_serializing_if`
/// honored on the serialized JSON string.
#[test]
fn capabilities_key_present_with_annotation_absent_without() {
    // Project WITH an annotated pack -> key present.
    let with_dir = tempfile::TempDir::new().expect("tempdir");
    write_file(
        &with_dir.path().join("ggen.toml"),
        r#"
[[generation.rules]]
name = "r1"
[generation.rules.template]
pack = "annotated"
"#,
    );
    write_file(
        &with_dir
            .path()
            .join("packs")
            .join("annotated")
            .join("pack.toml"),
        "[pack]\nname = \"annotated\"\n[capabilities]\nprovides = [\"urn:test:a\"]\n",
    );
    let json = run(with_dir.path()).expect("with-annotation run");
    assert!(json.contains("\"capabilities\""), "got: {json}");

    // Project WITHOUT an annotated pack -> key absent from the wire bytes.
    let without_dir = tempfile::TempDir::new().expect("tempdir");
    write_file(
        &without_dir.path().join("ggen.toml"),
        r#"
[[generation.rules]]
name = "r1"
[generation.rules.template]
pack = "plain"
"#,
    );
    write_file(
        &without_dir
            .path()
            .join("packs")
            .join("plain")
            .join("pack.toml"),
        "[pack]\nname = \"plain\"\n",
    );
    let json = run(without_dir.path()).expect("without-annotation run");
    assert!(!json.contains("\"capabilities\""), "got: {json}");
    assert!(json.contains("\"ok\":true"), "got: {json}");
}

/// (3) A referenced pack whose `requires` URN no pack `provides` is
/// surfaced under `unsatisfied`.
#[test]
fn unsatisfied_requirement_is_surfaced() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    write_file(
        &root.join("ggen.toml"),
        r#"
[[generation.rules]]
name = "r1"
[generation.rules.template]
pack = "needy"
"#,
    );
    write_file(
        &root.join("packs").join("needy").join("pack.toml"),
        concat!(
            "[pack]\nname = \"needy\"\n",
            "[capabilities]\n",
            "provides = [\"urn:test:own\"]\n",
            "requires = [\"urn:test:nobody-provides-this\"]\n",
        ),
    );
    let json = run(root).expect("tool should succeed");
    assert!(
        json.contains("urn:test:nobody-provides-this"),
        "unsatisfied require should be listed, got: {json}"
    );
    assert!(
        json.contains("\"unsatisfied\":[\"urn:test:nobody-provides-this\"]"),
        "got: {json}"
    );
}

/// (4) `annotated_packs` lists only packs whose pack.toml actually carried
/// `[capabilities]`, and the project-local `packs/` dir wins over the
/// shared corpora.
#[test]
fn annotated_packs_reflect_fixture_marketplace_dir() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    write_file(
        &root.join("ggen.toml"),
        r#"
[[generation.rules]]
name = "r1"
[generation.rules.template]
pack = "capful"
[[generation.rules]]
name = "r2"
[generation.rules.template]
pack = "capless"
"#,
    );
    write_file(
        &root.join("packs").join("capful").join("pack.toml"),
        "[pack]\nname = \"capful\"\n[capabilities]\nprovides = [\"urn:test:c\"]\n",
    );
    write_file(
        &root.join("packs").join("capless").join("pack.toml"),
        "[pack]\nname = \"capless\"\n",
    );
    let json = run(root).expect("tool should succeed");
    assert!(
        json.contains("\"annotated_packs\":[\"capful\"]"),
        "only the annotated pack listed, got: {json}"
    );
    assert!(!json.contains("capless\"}"), "got: {json}");
}

/// (5) Malformed ggen.toml -> typed `ConfigError`, never a panic.
#[test]
fn malformed_manifest_yields_typed_config_error() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    write_file(&root.join("ggen.toml"), "not [ valid toml ===");

    let err = capability_status(&CapabilityStatusParams {
        root: root.display().to_string(),
    })
    .expect_err("malformed manifest must be a typed error, not a panic");
    assert!(matches!(err.category, ErrorCategory::ConfigError));
}

/// Missing ggen.toml -> typed `NotFound` (the tool reads a real file; a
/// vanished manifest is not silently treated as an empty project).
#[test]
fn missing_manifest_yields_typed_not_found() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let err = capability_status(&CapabilityStatusParams {
        root: dir.path().display().to_string(),
    })
    .expect_err("missing manifest must be a typed error");
    assert!(matches!(err.category, ErrorCategory::NotFound));
}
