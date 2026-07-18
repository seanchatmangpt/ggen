//! Chicago-TDD end-to-end tests for the G5A repair pass on the declarative
//! `[[generation.rules]]` sync path (`crate::generation_rules`): real
//! filesystem (`tempfile::TempDir`), a real graph-backed store, real SPARQL
//! evaluation, real Tera rendering — no mocks — driven exclusively through
//! the public [`ggen_engine::sync::sync`] entry point, same as
//! `tests/generation_rules_e2e.rs`.
//!
//! Covers, in order:
//! 1. The shared context-key fix (Cluster D): `sparql_results` is now an
//!    alias for `results`, live alongside it.
//! 2. The static-rule flatten fix (Cluster D, `llm-full-integration`): a
//!    static (non-per-row) rule now flattens the first row's columns to
//!    top-level context, matching the per-row branch's behavior.
//! 3. The `build_tera` glob error-isolation fix (Cluster A): an orphaned,
//!    syntactically-broken template file no longer aborts the whole sync,
//!    but a rule that actually references (via `{% include %}`) a broken
//!    file still fails loudly.
//! 4. The typed `[FM-GEN-008]` causal sub-classification: variable-missing,
//!    output-path-invalid, filter-unknown, and (Cluster B)
//!    schema-incompatible all report their own distinct tag.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const ONTOLOGY_ALICE_BOB: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
ex:bob   ex:name "bob" .
"#;

fn write_manifest(root: &Path, body: &str) {
    let manifest = format!(
        "[project]\nname = \"gendemo\"\nversion = \"0.1.0\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n{body}"
    );
    std::fs::write(root.join("ggen.toml"), manifest).expect("write ggen.toml");
}

fn write_ontology(root: &Path, ttl: &str) {
    std::fs::write(root.join("ontology.ttl"), ttl).expect("write ontology.ttl");
}

// ---------------------------------------------------------------------------
// 1. Cluster D — `sparql_results` alias
// ---------------------------------------------------------------------------

/// The exact failure mode 23/30 example projects hit: a template authored
/// against `sparql_results` (never actually supplied before this fix) must
/// now render real data — proving the alias is live, not merely
/// documented. `results` must keep working too (both names carry identical
/// row data).
#[test]
fn sparql_results_alias_is_available_alongside_results() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"names\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { inline = \"{% for row in sparql_results %}{{ row.name }};{% endfor %}|{% for row in results %}{{ row.name }};{% endfor %}\" }\n\
         output_file = \"out.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let report = sync(dir.path(), SyncOptions::default()).expect("sync must succeed");
    assert_eq!(report.written, vec![std::path::PathBuf::from("out.txt")]);
    let content = std::fs::read_to_string(dir.path().join("out.txt")).expect("read output");
    assert_eq!(
        content, "alice;bob;|alice;bob;",
        "`sparql_results` and `results` must both resolve to the identical real row data"
    );
}

// ---------------------------------------------------------------------------
// 2. Cluster D — static-rule first-row flatten
// ---------------------------------------------------------------------------

/// The `llm-full-integration` failure mode: a static (non-per-row) rule's
/// template references bare top-level variables (`{{ name }}`) rather than
/// `results`/`row`. Before this fix, only the per-row branch flattened a
/// row's columns onto the context — static rules never did, so this would
/// fail with "Variable `name` not found in context". Real SPARQL data must
/// flow through.
#[test]
fn static_rule_flattens_first_row_columns_to_top_level_context() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"greeting\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { inline = \"Hello, {{ name }}!\" }\n\
         output_file = \"greeting.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let report = sync(dir.path(), SyncOptions::default()).expect("sync must succeed");
    assert_eq!(report.written, vec![std::path::PathBuf::from("greeting.txt")]);
    let content = std::fs::read_to_string(dir.path().join("greeting.txt")).expect("read output");
    assert_eq!(
        content, "Hello, alice!",
        "static rule must flatten the FIRST row's real columns (`name`=\"alice\", \
         ordered first alphabetically) onto the top-level context"
    );
}

// ---------------------------------------------------------------------------
// 3. Cluster A — build_tera glob error isolation
// ---------------------------------------------------------------------------

/// The exact `advanced-rust-project`/`comprehensive-rust-showcase`/
/// `gcp-erlang-autonomics` failure mode: an orphaned template file under
/// `templates/` with a genuine Tera syntax error, referenced by NOTHING,
/// must not abort a sync whose active rule's own template is fine.
#[test]
fn orphaned_broken_template_file_does_not_abort_unrelated_rule() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"names\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { file = \"templates/valid.tmpl\" }\n\
         output_file = \"out.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(
        dir.path().join("templates/valid.tmpl"),
        "{% for row in results %}{{ row.name }};{% endfor %}",
    )
    .expect("write valid template");
    // Jinja2-style positional filter arg -- Tera has never accepted this
    // (requires `default(value=\"x\")`), the exact real-world defect from
    // `examples/advanced-rust-project/templates/documentation.tmpl`.
    // Nothing references this file.
    std::fs::write(
        dir.path().join("templates/broken.tmpl"),
        "{% set x = missing | default(\"fallback\") %}{{ x }}",
    )
    .expect("write broken orphaned template");

    let report = sync(dir.path(), SyncOptions::default())
        .expect("an orphaned, unreferenced broken template must not abort the sync");
    assert_eq!(report.written, vec![std::path::PathBuf::from("out.txt")]);
    let content = std::fs::read_to_string(dir.path().join("out.txt")).expect("read output");
    assert_eq!(content, "alice;bob;");
}

/// The other half of the same fix: a template that DOES reference the
/// broken file (via `{% include %}`) must still fail loudly, naming the
/// missing include target -- proving the lenient loader isolates failures
/// per-file rather than silently absorbing genuinely-used broken templates.
#[test]
fn included_broken_template_file_still_fails_loudly() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"names\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { file = \"templates/uses-broken.tmpl\" }\n\
         output_file = \"out.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(
        dir.path().join("templates/broken.tmpl"),
        "{% set x = missing | default(\"fallback\") %}{{ x }}",
    )
    .expect("write broken template");
    std::fs::write(
        dir.path().join("templates/uses-broken.tmpl"),
        "{% include \"broken.tmpl\" %}",
    )
    .expect("write template that includes the broken one");

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("a rule that actually uses the broken file must fail, not silently succeed");
    let msg = err.to_string();
    assert!(msg.contains("FM-GEN-008"), "{msg}");
    assert!(
        msg.contains("TEMPLATE_INCLUDE_NOT_FOUND"),
        "a broken, unregistered include target must classify as \
         TEMPLATE_INCLUDE_NOT_FOUND, not silently succeed: {msg}"
    );
}

// ---------------------------------------------------------------------------
// 4. Typed FM-GEN-008 causal sub-classification
// ---------------------------------------------------------------------------

/// A genuinely undefined context variable (unrelated to the `results`/
/// `sparql_results` naming fix) must classify as `TEMPLATE_VARIABLE_MISSING`
/// and report the example root, the template descriptor, and the rule name.
#[test]
fn missing_context_variable_is_classified_as_variable_missing() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"broken-var\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { inline = \"{{ this_var_does_not_exist_anywhere }}\" }\n\
         output_file = \"out.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must fail");
    let msg = err.to_string();
    assert!(msg.contains("[FM-GEN-008]"), "{msg}");
    assert!(msg.contains("[TEMPLATE_VARIABLE_MISSING]"), "{msg}");
    assert!(
        msg.contains(&format!("example=`{}`", dir.path().display())),
        "must report the example project root: {msg}"
    );
    assert!(msg.contains("template=`<inline>`"), "{msg}");
    assert!(msg.contains("rule=`broken-var`"), "{msg}");
}

/// A rule whose `output_file` pattern itself references an undefined
/// variable must classify as `TEMPLATE_OUTPUT_PATH_INVALID` -- distinct
/// from a body-render failure -- since the actionable fix (the `output_file`
/// pattern) is different from a body template fix.
#[test]
fn broken_output_file_pattern_is_classified_as_output_path_invalid() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"bad-path\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { inline = \"hi {{ row.name }}\" }\n\
         output_file = \"out/{{ this_path_var_does_not_exist }}.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must fail");
    let msg = err.to_string();
    assert!(msg.contains("[FM-GEN-008]"), "{msg}");
    assert!(msg.contains("[TEMPLATE_OUTPUT_PATH_INVALID]"), "{msg}");
    assert!(msg.contains("rule=`bad-path`"), "{msg}");
}

/// A filter name Tera has no registration for at all must classify as
/// `TEMPLATE_FILTER_UNKNOWN` -- distinct from a filter that exists but was
/// called with the wrong argument shape (`TEMPLATE_CONTEXT_INVALID`).
#[test]
fn unknown_filter_is_classified_as_filter_unknown() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"bad-filter\"\n\
         query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\n\
         template = { inline = \"{{ name | this_filter_is_not_registered_anywhere }}\" }\n\
         output_file = \"out.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must fail");
    let msg = err.to_string();
    assert!(msg.contains("[FM-GEN-008]"), "{msg}");
    assert!(msg.contains("[TEMPLATE_FILTER_UNKNOWN]"), "{msg}");
}

/// Cluster B: a YAML file-tree meta-spec (`structure:` + `foreach:`) --
/// the exact shape of `examples/clap-noun-verb-demo/cli-template.yaml` --
/// must classify as `TEMPLATE_SCHEMA_INCOMPATIBLE`, not
/// `TEMPLATE_VARIABLE_MISSING`. Without the structural pre-check, Tera
/// would happily parse this file's literal `{{ project.name }}` markers as
/// real expressions and only fail at render time with a generic
/// "Variable ... not found" message indistinguishable from Cluster D's
/// real bug.
#[test]
fn yaml_file_tree_meta_spec_is_classified_as_schema_incompatible() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"cli-code-gen\"\n\
         query = { inline = \"SELECT * WHERE { ?s ?p ?o }\" }\n\
         template = { inline = \"structure:\\n  - path: 'Cargo.toml'\\n    foreach: 'project.nouns'\\n\" }\n\
         output_file = \"generated/project.rs\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must fail");
    let msg = err.to_string();
    assert!(msg.contains("[FM-GEN-008]"), "{msg}");
    assert!(
        msg.contains("[TEMPLATE_SCHEMA_INCOMPATIBLE]"),
        "a YAML file-tree meta-spec must be classified structurally, not \
         misdiagnosed as a missing variable: {msg}"
    );
    assert!(
        !msg.contains("TEMPLATE_VARIABLE_MISSING"),
        "must not misclassify the Cluster B capability gap as Cluster D's \
         context-key bug: {msg}"
    );
    assert!(msg.contains("rule=`cli-code-gen`"), "{msg}");
}
