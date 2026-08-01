//! G6 evidence for the `E0011`/`E0013` law-surface diagnostic codes
//! documented in this repo's `CLAUDE.md` ("also independently re-implemented
//! as sync-time hard errors in `crates/ggen-config/src/manifest/validation.rs`").
//!
//! **Fixed (previously a real finding, not a fix):** `ggen sync run`'s
//! declarative-rules dispatch (`crate::schema_dispatch::load`,
//! `crates/ggen-engine/src/schema_dispatch.rs`) used to parse the manifest
//! via `ggen_config::manifest::ManifestParser::parse_str`, which does *not*
//! run semantic validation. It now calls
//! `ManifestParser::parse_and_validate`
//! (`crates/ggen-config/src/manifest/parser.rs:59-64`, which calls
//! `ManifestValidator::new(&manifest, base_path).validate()` — the method
//! that actually contains the `E0011`/`E0013` checks,
//! `crates/ggen-config/src/manifest/validation.rs:98`), so `[validation]
//! strict_mode = true` combined with an inline `SELECT`/`CONSTRUCT` lacking
//! `ORDER BY` now correctly fails `ggen sync run`, matching the CLAUDE.md
//! claim quoted above.
//!
//! The tests below are real, non-vacuous positive/negative pairs proving the
//! *current* (fixed) behavior: an unordered `SELECT`/`CONSTRUCT` under
//! `strict_mode = true` now blocks sync with an `E0011`/`E0013` error, while
//! the same rules with `ORDER BY` present still sync cleanly.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
ex:bob   ex:name "bob" .
"#;

fn write_manifest(root: &Path, body: &str) {
    let manifest = format!(
        "[project]\nname = \"diag-demo\"\nversion = \"0.1.0\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n{body}"
    );
    std::fs::write(root.join("ggen.toml"), manifest).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology.ttl");
}

/// Fixed behavior: `[[generation.rules]]` with an inline `SELECT` lacking
/// `ORDER BY`, under `[validation] strict_mode = true`, now fails to sync —
/// `ggen sync run`'s declarative-rules path reaches `ManifestValidator::validate`
/// (E0013's home) via `schema_dispatch::load`'s `parse_and_validate` call.
/// See the module doc comment for the exact call-site evidence.
#[test]
fn unordered_inline_select_under_strict_mode_now_blocks_sync_run() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[validation]\nstrict_mode = true\n\n\
         [[generation.rules]]\nname = \"names\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name }\" }\n\
         template = { inline = \"{% for row in results %}{{ row.name }};{% endfor %}\" }\n\
         output_file = \"out.txt\"\n",
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err(
        "fixed behavior: unordered SELECT + strict_mode=true must now fail sync \
         (E0013 is wired into schema_dispatch::load's parse_and_validate path)",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("E0013"),
        "expected E0013 in error message, got: {msg}"
    );
}

/// Control: the same rule with `ORDER BY` added still syncs cleanly —
/// proving the prior test's failure is specifically about missing `ORDER BY`,
/// not some unrelated breakage.
#[test]
fn ordered_inline_select_under_strict_mode_syncs_cleanly() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[validation]\nstrict_mode = true\n\n\
         [[generation.rules]]\nname = \"names\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { inline = \"{% for row in results %}{{ row.name }};{% endfor %}\" }\n\
         output_file = \"out.txt\"\n",
    );

    let report =
        sync(dir.path(), SyncOptions::default()).expect("ORDER BY present must sync cleanly");
    assert_eq!(report.written, vec![std::path::PathBuf::from("out.txt")]);
}

/// Fixed behavior: an `[[inference.rules]]` `CONSTRUCT` query lacking
/// `ORDER BY`, under `strict_mode = true`, now also blocks `sync()` — same
/// root cause fix as the SELECT case above (E0011 is E0013's sibling check
/// in the same now-reached `validate()` method).
#[test]
fn unordered_construct_under_strict_mode_now_blocks_sync_run() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[validation]\nstrict_mode = true\n\n\
         [[inference.rules]]\nname = \"infer-names\"\n\
         construct = \"CONSTRUCT { ?s <http://example.org/inferred> true } WHERE { ?s <http://example.org/name> ?name }\"\n\n\
         [[generation.rules]]\nname = \"names\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { inline = \"{% for row in results %}{{ row.name }};{% endfor %}\" }\n\
         output_file = \"out.txt\"\n",
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err(
        "fixed behavior: unordered CONSTRUCT + strict_mode=true must now fail sync \
         (E0011 is wired into schema_dispatch::load's parse_and_validate path)",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("E0011"),
        "expected E0011 in error message, got: {msg}"
    );
}

/// Control: the same inference rule with `ORDER BY` added on the
/// `CONSTRUCT` query still syncs cleanly under `strict_mode = true`.
#[test]
fn ordered_construct_under_strict_mode_syncs_cleanly() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[validation]\nstrict_mode = true\n\n\
         [[inference.rules]]\nname = \"infer-names\"\n\
         construct = \"CONSTRUCT { ?s <http://example.org/inferred> true } WHERE { ?s <http://example.org/name> ?name } ORDER BY ?s\"\n\n\
         [[generation.rules]]\nname = \"names\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { inline = \"{% for row in results %}{{ row.name }};{% endfor %}\" }\n\
         output_file = \"out.txt\"\n",
    );

    let report =
        sync(dir.path(), SyncOptions::default()).expect("ORDER BY present must sync cleanly");
    assert_eq!(report.written, vec![std::path::PathBuf::from("out.txt")]);
}

/// Negative falsifier proving the manifest is genuinely parsed and acted
/// upon (i.e. these tests are not vacuously succeeding because the rule
/// was silently skipped): a structurally invalid `[[generation.rules]]`
/// entry (missing the required `output_file`) still fails `sync()` today,
/// so the harness distinguishes "parsed and executed" from "silently
/// ignored".
#[test]
fn structurally_invalid_generation_rule_still_fails_sync() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"names\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { inline = \"{% for row in results %}{{ row.name }};{% endfor %}\" }\n",
    );

    sync(dir.path(), SyncOptions::default())
        .expect_err("a generation rule missing output_file must still fail to parse/sync");
}
