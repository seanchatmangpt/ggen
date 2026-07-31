//! G6 evidence for the `E0011`/`E0013` law-surface diagnostic codes
//! documented in this repo's `CLAUDE.md` ("also independently re-implemented
//! as sync-time hard errors in `crates/ggen-config/src/manifest/validation.rs`").
//!
//! **Real finding, not a fix (out of scope: `crates/ggen-engine/src/**` is a
//! read-only surface for this agent):** `ggen sync run`'s declarative-rules
//! dispatch (`crate::schema_dispatch::load`,
//! `crates/ggen-engine/src/schema_dispatch.rs:97`) parses the manifest via
//! `ggen_config::manifest::ManifestParser::parse_str`, which does *not* run
//! semantic validation. The validating entry point,
//! `ManifestParser::parse_and_validate`
//! (`crates/ggen-config/src/manifest/parser.rs:59-64`, which calls
//! `ManifestValidator::new(&manifest, base_path).validate()` — the method
//! that actually contains the `E0011`/`E0013` checks,
//! `crates/ggen-config/src/manifest/validation.rs:98`), is never reached
//! from any `ggen-engine` or `ggen-cli` call site (confirmed by exhaustive
//! search: `ManifestValidator`/`.validate()` construction appears only
//! inside `ggen-config` itself). Concretely: `[validation] strict_mode =
//! true` combined with an inline `SELECT`/`CONSTRUCT` lacking `ORDER BY`
//! does **not** fail `ggen sync run` today, contradicting the CLAUDE.md
//! claim quoted above. This is a Contract Drift finding per
//! `.claude/rules/coding-agent-mistakes.md` mistake class 5 — the doc
//! describes behavior the live sync path does not implement. Reported here,
//! not silently patched.
//!
//! The four tests below are real, non-vacuous positive/negative pairs
//! proving the *actual* current behavior (sync always succeeds on these
//! inputs regardless of `strict_mode` or `ORDER BY` presence), so this
//! evidence file fails loudly the moment someone wires
//! `parse_and_validate` into `schema_dispatch::load` and the gap closes —
//! at which point these tests should be revisited, not silently left green
//! by accident.

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

/// Current, verified behavior: `[[generation.rules]]` with an inline
/// `SELECT` lacking `ORDER BY`, under `[validation] strict_mode = true`,
/// still syncs successfully — `ggen sync run`'s declarative-rules path
/// never reaches `ManifestValidator::validate` (E0013's home), so
/// `strict_mode` has no observable effect on the live sync entry point.
/// See the module doc comment for the exact call-site evidence.
#[test]
fn unordered_inline_select_under_strict_mode_does_not_currently_block_sync_run() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[validation]\nstrict_mode = true\n\n\
         [[generation.rules]]\nname = \"names\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name }\" }\n\
         template = { inline = \"{% for row in results %}{{ row.name }};{% endfor %}\" }\n\
         output_file = \"out.txt\"\n",
    );

    let report = sync(dir.path(), SyncOptions::default()).expect(
        "current behavior: unordered SELECT + strict_mode=true still syncs \
         (E0013 is not wired into schema_dispatch::load's parse path)",
    );
    assert_eq!(report.written, vec![std::path::PathBuf::from("out.txt")]);
}

/// Control: the same rule with `ORDER BY` added also syncs cleanly —
/// proving the prior test's success is not itself broken/erroring for an
/// unrelated reason, just indifferent to `ORDER BY` presence.
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

/// Current, verified behavior: an `[[inference.rules]]` `CONSTRUCT` query
/// lacking `ORDER BY`, under `strict_mode = true`, also does not block
/// `sync()` today — same root cause as the SELECT case above (E0011 is
/// E0013's sibling check in the same unreached `validate()` method).
#[test]
fn unordered_construct_under_strict_mode_does_not_currently_block_sync_run() {
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

    let report = sync(dir.path(), SyncOptions::default()).expect(
        "current behavior: unordered CONSTRUCT + strict_mode=true still syncs \
         (E0011 is not wired into schema_dispatch::load's parse path)",
    );
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
