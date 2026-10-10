//! Chicago tests for `generation.rules` TOML shape handling in
//! `ggen_capability_status`.
//!
//! Real fixtures only: each test writes a real `ggen.toml` into a
//! `TempDir` and calls the actual `capability_status` function,
//! asserting on the serialized wire format. No mocks.
//!
//! TOML has two valid spellings for nested data: array-of-tables
//! (`[[generation.rules]]`) and dotted tables (`[generation.rules.x]`).
//! The tool must not silently drop a dotted-table `generation.rules` --
//! a config shape the author believes declares rules would yield a
//! silent zero, the fail-open defect class. Fail-closed: refuse with a
//! typed ConfigError naming the key.

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
    serde_json::to_string(&result).map_err(|e| {
        ggen_mcp::error::McpError::new(
            ggen_mcp::error::ErrorCategory::GraphLoadError,
            format!("test serialization failed: {e}"),
        )
    })
}

/// (a) Canonical array-of-tables form: rules are counted; a rule whose
/// template references a pack is reported as `used_by_rules` and the
/// project is marked affected.
#[test]
fn array_of_tables_rules_are_counted() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    write_file(
        &root.join("ggen.toml"),
        r#"
[[generation.rules]]
name = "pack_rule"
[generation.rules.template]
pack = "some-pack"
"#,
    );
    let json = run(root).expect("tool should succeed");
    assert!(
        json.contains("\"used_by_rules\":[\"pack_rule\"]"),
        "array-of-tables rule should be counted, got: {json}"
    );
    assert!(
        json.contains("\"project_is_affected\":true"),
        "presence of a pack-referencing rule should mark the project affected, got: {json}"
    );
}

/// (b) Dotted-table form `[generation.rules]` (valid TOML, but a table,
/// not an array). Fail-closed contract: the tool refuses with
/// ConfigError naming `generation.rules` rather than silently yielding
/// zero rules.
#[test]
fn dotted_table_rules_are_refused_not_silently_ignored() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    write_file(
        &root.join("ggen.toml"),
        r#"
[generation.rules]
name = "dotted_rule"
[generation.rules.template]
pack = "some-pack"
"#,
    );
    let err = run(root).expect_err("dotted-table generation.rules must be refused, not ignored");
    assert_eq!(
        err.category,
        ErrorCategory::ConfigError,
        "expected ConfigError, got: {err:?}"
    );
    assert!(
        err.to_string().contains("generation.rules"),
        "error must name the offending key, got: {err}"
    );
}

/// (c) A rule entry missing a `template` is skipped per-entry (documented
/// tolerated shape), while well-formed siblings are still counted --
/// entry-level leniency, not whole-config silence.
#[test]
fn malformed_rule_entry_is_skipped_but_siblings_counted() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    write_file(
        &root.join("ggen.toml"),
        r#"
[[generation.rules]]
name = "no_template_rule"
[[generation.rules]]
name = "good_rule"
[generation.rules.template]
pack = "some-pack"
"#,
    );
    let json = run(root).expect("tool should succeed");
    assert!(
        !json.contains("no_template_rule"),
        "rule without template should not count toward any used_by_rules, got: {json}"
    );
    assert!(
        json.contains("\"used_by_rules\":[\"good_rule\"]"),
        "well-formed sibling should still be counted, got: {json}"
    );
}
