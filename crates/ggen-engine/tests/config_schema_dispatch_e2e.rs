//! Chicago-TDD end-to-end tests for the shared `ggen.toml` schema dispatch
//! (specs/014-ggen-core-replacement, correction 2 / Blocker A part 2):
//! `crate::schema_dispatch::load`, backed by the shared
//! `ggen_config::classify_ggen_toml` structural classifier, now repoints
//! every ggen.toml-reading call site in this crate (`sync::sync`,
//! `verbs::handlers::{handle_doctor, handle_graph_validate,
//! build_law_engine}`).
//!
//! Real filesystem (`tempfile::TempDir`), the real `ggen` binary
//! (`assert_cmd`) for `doctor`, real oxigraph-backed graph loading — no
//! mocks. These 8 fixtures are the `DoD` G1 table, one test per row:
//!
//! 1. authoritative (declarative-rules) schema, valid           -> Accepted
//! 2. compatible (frontmatter) schema, valid                    -> Accepted
//! 3. unsupported (matches neither schema)                      -> typed refusal
//! 4. ambiguous (matches both schemas)                           -> typed refusal
//! 5. malformed TOML                                             -> typed parse failure
//! 6. missing required field                                    -> field-specific typed error
//! 7. unknown field (rejected downstream of a clean classification) -> typed error
//! 8. `ggen doctor` on each supported schema                     -> correct diagnostic
//! 9. `ggen graph validate` on each supported schema             -> dispatches both
//! 10. `ggen graph validate` on ambiguous schema                 -> typed refusal
//! 11. `ggen law load` on each supported schema / ambiguous      -> dispatch + typed refusal

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn write(root: &Path, rel: &str, content: &str) {
    let path = root.join(rel);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdir parent");
    }
    std::fs::write(path, content).expect("write file");
}

/// Resolve the real `ggen` binary the same way `cli_boundary.rs::ggen_bin`
/// does: `CARGO_BIN_EXE_ggen` (set by `cargo test -p ggen-cli-lib`, never by
/// `-p ggen-engine` — the root package is `autobins = false`), then the
/// workspace `target/{debug,release}/ggen`, then `PATH`. Hard-failing on the
/// env var alone made these tests fail spuriously under
/// `cargo test -p ggen-engine`.
fn ggen_bin() -> std::path::PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = std::path::PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(std::path::PathBuf::from)
        .or_else(|| {
            let manifest_dir =
                std::env::var_os("CARGO_MANIFEST_DIR").map(std::path::PathBuf::from)?;
            let mut dir: &std::path::Path = manifest_dir.as_path();
            loop {
                if dir.join("Cargo.lock").exists() {
                    return Some(dir.join("target"));
                }
                match dir.parent() {
                    Some(p) => dir = p,
                    None => return None,
                }
            }
        });

    if let Some(target) = target_root {
        for profile in &["debug", "release"] {
            let candidate = target.join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
            let candidate_exe = target.join(profile).join("ggen.exe");
            if candidate_exe.is_file() {
                return candidate_exe;
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset and no \
         target/debug/ggen found; build it with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

// ---------------------------------------------------------------------------
// 1. Authoritative (declarative-rules) schema, valid -> Accepted.
// ---------------------------------------------------------------------------

/// A real, valid `[[generation.rules]]` project syncs successfully end to
/// end (real SPARQL execution, real Tera render, real write) -- proving
/// `crate::schema_dispatch::load`'s `DeclarativeRules` branch is wired all
/// the way through, not just reachable.
#[test]
fn authoritative_schema_valid_sync_is_accepted() {
    let dir = TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        r#"
[project]
name = "demo"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[[generation.rules]]
name = "names"
query = { inline = "SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name" }
template = { inline = "{% for row in results %}{{ row.name }}\n{% endfor %}" }
output_file = "out/names.txt"
"#,
    );
    write(
        dir.path(),
        "ontology.ttl",
        r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
"#,
    );

    let report =
        sync(dir.path(), SyncOptions::default()).expect("declarative-rules sync must succeed");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/names.txt")]
    );
    let content = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("read output");
    assert_eq!(content, "alice\n");
}

// ---------------------------------------------------------------------------
// 2. Compatible (frontmatter) schema, valid -> Accepted (unaffected by the
//    classifier repointing).
// ---------------------------------------------------------------------------

/// A real, valid frontmatter-per-template-file project syncs successfully
/// end to end, exactly as before the classifier repointing -- proving the
/// `Frontmatter` branch falls through to the unchanged Stage 1 path.
#[test]
fn compatible_schema_valid_sync_is_accepted() {
    let dir = TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#,
    );
    write(
        dir.path(),
        "ontology.ttl",
        r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
"#,
    );
    write(
        dir.path(),
        "templates/one.tmpl",
        "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}",
    );

    let report = sync(dir.path(), SyncOptions::default()).expect("frontmatter sync must succeed");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/names.txt")]
    );
    let content = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("read output");
    assert_eq!(content, "alice\n");
}

// ---------------------------------------------------------------------------
// 3. Unsupported (matches neither schema) -> typed refusal.
// ---------------------------------------------------------------------------

/// A syntactically valid `ggen.toml` that resembles neither recognized
/// schema fails closed with the classifier's own
/// `ggen_config::CONFIG_SCHEMA_UNSUPPORTED` code embedded in the message --
/// never a bare/generic TOML deserialization error.
#[test]
fn unsupported_schema_sync_is_typed_refusal() {
    let dir = TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        "[some_other_tool]\nkey = \"value\"\n",
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must be refused");
    assert!(
        err.to_string()
            .contains(ggen_config::CONFIG_SCHEMA_UNSUPPORTED),
        "{err}"
    );
}

// ---------------------------------------------------------------------------
// 4. Ambiguous (matches both schemas) -> typed refusal.
// ---------------------------------------------------------------------------

/// A `ggen.toml` carrying structural markers from both schemas at once
/// (frontmatter-shaped `[project]` with no `version`, plus a
/// declarative-rules-only `[ai]` table) fails closed with
/// `ggen_config::CONFIG_SCHEMA_AMBIGUOUS`, naming the conflicting markers --
/// never silently picking one schema.
#[test]
fn ambiguous_schema_sync_is_typed_refusal() {
    let dir = TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[ai]
provider = "openai"
"#,
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must be refused");
    let msg = err.to_string();
    assert!(msg.contains(ggen_config::CONFIG_SCHEMA_AMBIGUOUS), "{msg}");
}

// ---------------------------------------------------------------------------
// 5. Malformed TOML -> typed parse failure.
// ---------------------------------------------------------------------------

/// Syntactically invalid TOML fails closed with
/// `ggen_config::CONFIG_PARSE_FAILED` embedded in the message.
#[test]
fn malformed_toml_sync_is_typed_parse_failure() {
    let dir = TempDir::new().expect("tempdir");
    write(dir.path(), "ggen.toml", "not [ valid toml");

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must fail");
    assert!(
        err.to_string().contains(ggen_config::CONFIG_PARSE_FAILED),
        "{err}"
    );
}

// ---------------------------------------------------------------------------
// 6. Missing required field -> field-specific typed error.
// ---------------------------------------------------------------------------

/// A document whose *shape* is unambiguously declarative-rules
/// (`project.version` present, `[generation]` table present) but is missing
/// `[ontology]` (a required, non-`Option` `GgenManifest` field) fails with a
/// message naming the actual missing field -- not a generic "TOML deserialize
/// failed" message and not misrouted to the frontmatter parser.
#[test]
fn missing_required_field_is_field_specific_typed_error() {
    let dir = TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\nversion = \"1.0.0\"\n\n[[generation.rules]]\nname = \"x\"\nquery = { inline = \"SELECT * WHERE { ?s ?p ?o }\" }\ntemplate = { inline = \"hi\" }\noutput_file = \"out.txt\"\n",
    );

    let err =
        sync(dir.path(), SyncOptions::default()).expect_err("must fail: ontology is required");
    let msg = err.to_string().to_lowercase();
    assert!(msg.contains("ontology"), "{msg}");
}

// ---------------------------------------------------------------------------
// 7. Unknown field (rejected downstream of a clean classification) -> typed
//    error.
// ---------------------------------------------------------------------------

/// An unrecognized top-level table does not perturb shape classification
/// (the classifier's own documented scope boundary -- see
/// `ggen_config::config_schema`'s `unrecognized_extra_table_does_not_change_shape_classification`
/// test), but the real typed parser it dispatches to still rejects it via
/// `#[serde(deny_unknown_fields)]` -- proving classify-then-parse catches
/// what classification alone does not.
#[test]
fn unknown_field_is_rejected_under_strict_parsing() {
    let dir = TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[bogus_table]
whatever = 1
"#,
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must fail: unknown field");
    let msg = err.to_string();
    assert!(msg.contains("FM-CONFIG-002"), "{msg}");
}

// ---------------------------------------------------------------------------
// 8. `ggen doctor` on each supported schema -> correct diagnostic.
// ---------------------------------------------------------------------------

/// `ggen doctor run` succeeds against BOTH supported schemas, each with the
/// correctly-typed diagnostic for that schema: the frontmatter schema
/// (pack/template resolution implemented) reports `"pass"` for
/// `lockfile_drift`; the declarative-rules schema (pack/template resolution
/// not implemented yet -- see `crate::generation_rules`'s own documented
/// scope gap) reports an explicit `"skip"`, never a fabricated `"pass"` and
/// never the pre-repointing generic TOML schema-mismatch crash (BUG-005).
#[test]
fn doctor_succeeds_with_correct_diagnostic_on_each_supported_schema() {
    // -- Frontmatter --
    let frontmatter_dir = TempDir::new().expect("tempdir");
    write(
        frontmatter_dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    );
    write(
        frontmatter_dir.path(),
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:a ex:name \"a\" .\n",
    );
    std::fs::create_dir_all(frontmatter_dir.path().join("templates")).expect("mkdir templates");

    let frontmatter_assert = assert_cmd::Command::new(ggen_bin())
        .current_dir(frontmatter_dir.path())
        .args(["doctor", "run"])
        .assert()
        .success();
    let frontmatter_json: serde_json::Value =
        serde_json::from_slice(&frontmatter_assert.get_output().stdout)
            .expect("doctor stdout is JSON");
    assert_eq!(frontmatter_json["healthy"], true, "{frontmatter_json}");
    assert_eq!(
        frontmatter_json["checks"]["lockfile_drift"]["status"], "pass",
        "{frontmatter_json}"
    );

    // -- Declarative-rules (this is the exact schema/shape BUG-005 crashed
    //    on before the classifier repointing) --
    let declarative_dir = TempDir::new().expect("tempdir");
    write(
        declarative_dir.path(),
        "ggen.toml",
        // ORDER BY added 2026-08-03 (TECH-DEBT-003 fix): this fixture's SELECT used to
        // lack ORDER BY, which `strict_mode` (now defaults true) correctly refuses with
        // E0013 -- collateral damage from that default flip (same root cause as task #30's
        // schema_dispatch.rs fixture and crates/ggen-cli/tests/performance.rs's fixture),
        // not a real bug in `doctor run`'s declarative-rules diagnostic path this test
        // actually verifies.
        "[project]\nname = \"demo\"\nversion = \"1.0.0\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[[generation.rules]]\nname = \"x\"\nquery = { inline = \"SELECT * WHERE { ?s ?p ?o } ORDER BY ?s ?p ?o\" }\ntemplate = { inline = \"hi\" }\noutput_file = \"out.txt\"\n",
    );
    write(
        declarative_dir.path(),
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:a ex:name \"a\" .\n",
    );

    let declarative_assert = assert_cmd::Command::new(ggen_bin())
        .current_dir(declarative_dir.path())
        .args(["doctor", "run"])
        .assert()
        .success();
    let declarative_json: serde_json::Value =
        serde_json::from_slice(&declarative_assert.get_output().stdout)
            .expect("doctor stdout is JSON");
    assert_eq!(declarative_json["healthy"], true, "{declarative_json}");
    assert_eq!(
        declarative_json["checks"]["lockfile_drift"]["status"], "skip",
        "{declarative_json}"
    );
    assert_eq!(
        declarative_json["checks"]["orphaned_artifacts"]["status"], "pass",
        "{declarative_json}"
    );
    assert_eq!(
        declarative_json["checks"]["receipt_staleness"]["status"], "pass",
        "{declarative_json}"
    );
}

// ---------------------------------------------------------------------------
// 9. `ggen graph validate` on each supported schema -> dispatches both.
// ---------------------------------------------------------------------------

/// `ggen graph validate` (project mode, no `files`) dispatches through
/// `crate::schema_dispatch::load` for BOTH supported schemas: each valid
/// fixture validates cleanly (real oxigraph parse of the real ontology),
/// proving neither branch hardcodes the other schema.
#[test]
fn graph_validate_dispatches_on_each_supported_schema() {
    // -- Frontmatter --
    let frontmatter_dir = TempDir::new().expect("tempdir");
    write(
        frontmatter_dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    );
    write(
        frontmatter_dir.path(),
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:a ex:name \"a\" .\n",
    );
    std::fs::create_dir_all(frontmatter_dir.path().join("templates")).expect("mkdir templates");

    assert_cmd::Command::new(ggen_bin())
        .current_dir(frontmatter_dir.path())
        .args(["graph", "validate"])
        .assert()
        .success();

    // -- Declarative-rules --
    let declarative_dir = TempDir::new().expect("tempdir");
    write(
        declarative_dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\nversion = \"1.0.0\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[[generation.rules]]\nname = \"x\"\nquery = { inline = \"SELECT * WHERE { ?s ?p ?o } ORDER BY ?s ?p ?o\" }\ntemplate = { inline = \"hi\" }\noutput_file = \"out.txt\"\n",
    );
    write(
        declarative_dir.path(),
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:a ex:name \"a\" .\n",
    );

    assert_cmd::Command::new(ggen_bin())
        .current_dir(declarative_dir.path())
        .args(["graph", "validate"])
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// 10. `ggen graph validate` on ambiguous schema -> typed refusal.
// ---------------------------------------------------------------------------

/// An ambiguous `ggen.toml` (both schemas' markers) is refused by
/// `ggen graph validate` with `ggen_config::CONFIG_SCHEMA_AMBIGUOUS` in the
/// stderr -- the classifier, not a schema-specific parser, is the gate.
#[test]
fn graph_validate_refuses_ambiguous_schema_with_typed_code() {
    let dir = TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n\n[ai]\nprovider = \"openai\"\n",
    );

    let assert = assert_cmd::Command::new(ggen_bin())
        .current_dir(dir.path())
        .args(["graph", "validate"])
        .assert()
        .failure();
    let stderr = String::from_utf8_lossy(&assert.get_output().stderr);
    assert!(
        stderr.contains(ggen_config::CONFIG_SCHEMA_AMBIGUOUS),
        "{stderr}"
    );
}

// ---------------------------------------------------------------------------
// 11. `ggen law load` on each supported schema / ambiguous -> dispatch +
//     typed refusal.
// ---------------------------------------------------------------------------

/// `ggen law load` dispatches through `build_law_engine`'s
/// `schema_dispatch::load` for BOTH supported schemas (real ontology parse,
/// zero `[law].rules` -> empty per-file map), and refuses an ambiguous
/// schema with `ggen_config::CONFIG_SCHEMA_AMBIGUOUS` -- covering all five
/// `law *` verbs' shared dispatch point in one transport.
#[test]
fn law_load_dispatches_on_each_supported_schema_and_refuses_ambiguous() {
    // -- Frontmatter --
    let frontmatter_dir = TempDir::new().expect("tempdir");
    write(
        frontmatter_dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    );
    write(
        frontmatter_dir.path(),
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:a ex:name \"a\" .\n",
    );
    std::fs::create_dir_all(frontmatter_dir.path().join("templates")).expect("mkdir templates");

    let fm_assert = assert_cmd::Command::new(ggen_bin())
        .current_dir(frontmatter_dir.path())
        .args(["law", "load"])
        .assert()
        .success();
    let fm_json: serde_json::Value =
        serde_json::from_slice(&fm_assert.get_output().stdout).expect("law load stdout is JSON");
    assert_eq!(fm_json["rule_files"], 0, "{fm_json}");

    // -- Declarative-rules --
    let declarative_dir = TempDir::new().expect("tempdir");
    write(
        declarative_dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\nversion = \"1.0.0\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[[generation.rules]]\nname = \"x\"\nquery = { inline = \"SELECT * WHERE { ?s ?p ?o } ORDER BY ?s ?p ?o\" }\ntemplate = { inline = \"hi\" }\noutput_file = \"out.txt\"\n",
    );
    write(
        declarative_dir.path(),
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:a ex:name \"a\" .\n",
    );

    assert_cmd::Command::new(ggen_bin())
        .current_dir(declarative_dir.path())
        .args(["law", "load"])
        .assert()
        .success();

    // -- Ambiguous -> typed refusal --
    let ambiguous_dir = TempDir::new().expect("tempdir");
    write(
        ambiguous_dir.path(),
        "ggen.toml",
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n\n[ai]\nprovider = \"openai\"\n",
    );

    let ambiguous_assert = assert_cmd::Command::new(ggen_bin())
        .current_dir(ambiguous_dir.path())
        .args(["law", "load"])
        .assert()
        .failure();
    let stderr = String::from_utf8_lossy(&ambiguous_assert.get_output().stderr);
    assert!(
        stderr.contains(ggen_config::CONFIG_SCHEMA_AMBIGUOUS),
        "{stderr}"
    );
}
