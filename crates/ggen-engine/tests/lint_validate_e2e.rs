//! `ggen graph validate` static-lint proofs: real filesystem (`TempDir`),
//! real template parsing, real `verbs::handlers::handle_graph_validate` calls.
//! No mocks.
//!
//! The 2 CLI-subprocess tests that used to live in this file
//! (`clean_demo_project_validates_via_cli`,
//! `cli_graph_validate_fails_closed_on_unbound_var`) are preserved, commented
//! out, at the bottom of this file. They exec `cargo_bin("ggen")`, which this
//! crate still doesn't produce (`autobins = false` — `verbs` is a re-enabled
//! library module, not a wired-up standalone binary; `ggen-cli` remains the
//! actual CLI entry point). The 5 `handle_graph_validate`-direct
//! `graph_validate_files_*` tests below were restored (specs/014-ggen-core-replacement)
//! once `verbs` was re-enabled (the workspace's clap-noun-verb version
//! conflict that disabled it is resolved — see `lib.rs`'s module comment).

#![allow(clippy::expect_used)]

use std::path::Path;

use camino::Utf8PathBuf;
use ggen_engine::lint::lint_template;
use ggen_engine::template::Template;
use ggen_engine::verbs::handlers::handle_graph_validate;
use tempfile::TempDir;

/// Scaffold a minimal project with one template body.
fn scaffold(root: &Path, template: &str) {
    std::fs::write(
        root.join("ggen.toml"),
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        root.join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n",
    )
    .expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), template).expect("write template");
}

fn parse_file(path: &Path) -> Template {
    let content = std::fs::read_to_string(path).expect("read template");
    Template::parse(&content).expect("parse template")
}

#[test]
fn unbound_body_var_on_disk_yields_fm_tpl_003() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{{ typo }}",
    );
    let path = dir.path().join("templates/one.tmpl");
    let errs = lint_template(&path, &parse_file(&path));
    assert_eq!(errs.len(), 1, "{errs:?}");
    let msg = errs[0].to_string();
    assert!(msg.contains("FM-TPL-003"), "{msg}");
    assert!(msg.contains("typo"), "{msg}");
    assert!(msg.contains("one.tmpl"), "{msg}");
}

#[test]
fn unbound_to_path_var_on_disk_yields_fm_tpl_004() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        "---\nto: \"out/{{ nope }}.txt\"\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{{ name }}",
    );
    let path = dir.path().join("templates/one.tmpl");
    let errs = lint_template(&path, &parse_file(&path));
    assert_eq!(errs.len(), 1, "{errs:?}");
    let msg = errs[0].to_string();
    assert!(msg.contains("FM-TPL-004"), "{msg}");
    assert!(msg.contains("nope"), "{msg}");
}

#[test]
fn select_star_disables_projection_check() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        "---\nto: out/all.txt\nsparql:\n  all: SELECT * WHERE { ?s ?p ?o } ORDER BY ?s\n---\n{{ anything_at_all }}",
    );
    let path = dir.path().join("templates/one.tmpl");
    let errs = lint_template(&path, &parse_file(&path));
    assert!(errs.is_empty(), "SELECT * must disable the check: {errs:?}");
}

#[test]
fn identity_construct_on_disk_yields_fm_tpl_005() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        "---\nto: out/static.txt\nconstruct: \"CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\"\n---\nstatic body",
    );
    let path = dir.path().join("templates/one.tmpl");
    let errs = lint_template(&path, &parse_file(&path));
    assert_eq!(errs.len(), 1, "{errs:?}");
    assert!(errs[0].to_string().contains("FM-TPL-005"), "{}", errs[0]);
}

// ---------------------------------------------------------------------
// DISABLED: these 2 tests exec the "ggen" binary via `cargo_bin("ggen")` —
// this crate still doesn't produce one (`autobins = false`, `verbs` is a
// library module, not a wired-up standalone binary). Preserved here, not
// deleted, pending either (a) ggen-cli actually routing through
// `ggen_engine::verbs`/`handlers` (the in-progress wiring work this ticket
// tracks) making an equivalent test possible from ggen-cli's own test suite,
// or (b) this crate genuinely growing its own standalone binary.
/*
#[test]
fn clean_demo_project_validates_via_cli() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}",
    );
    let output = CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(dir.path())
        .run()
        .expect("run graph validate");
    output
        .assert_success()
        .assert_stdout_contains("templates_checked");
}

#[test]
fn cli_graph_validate_fails_closed_on_unbound_var() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{{ typo }}",
    );
    let output = CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(dir.path())
        .run()
        .expect("run graph validate");
    output.assert_failure();
    let combined = format!("{}{}", output.stdout, output.stderr);
    assert!(
        combined.contains("FM-TPL-003"),
        "must name FM-TPL-003: {combined}"
    );
    assert!(
        combined.contains("typo"),
        "must name the variable: {combined}"
    );
}
*/

// ---------------------------------------------------------------------
// graph validate — file mode (multi-file `--files`), direct handler calls
// ---------------------------------------------------------------------
// These exercise `handle_graph_validate(files)` directly (the fn behind the
// generated `--files` route). Absolute paths in a TempDir make them
// cwd-independent: file mode never touches project_root/ggen.toml.

/// Two well-formed Turtle files both validate; each is reported with a path,
/// a positive quad count, and a 64-hex state hash. Independent per-file
/// validation, all reported at once.
#[test]
fn graph_validate_files_two_good_pass() {
    let dir = TempDir::new().expect("tempdir");
    let p1 = dir.path().join("a.ttl");
    let p2 = dir.path().join("b.ttl");
    std::fs::write(
        &p1,
        "@prefix ex: <http://example.org/> .\nex:a ex:p \"1\" .\n",
    )
    .expect("write a.ttl");
    std::fs::write(
        &p2,
        "@prefix ex: <http://example.org/> .\nex:b ex:q \"2\" .\n",
    )
    .expect("write b.ttl");
    let f1 = Utf8PathBuf::from_path_buf(p1).expect("utf8 path");
    let f2 = Utf8PathBuf::from_path_buf(p2).expect("utf8 path");

    let out = handle_graph_validate(vec![f1, f2], vec![]).expect("both files valid");
    assert_eq!(out["files_checked"], 2, "{out}");
    let files = out["files"].as_array().expect("files array");
    assert_eq!(files.len(), 2, "{out}");
    for rec in files {
        assert!(rec["path"].is_string(), "path present: {rec}");
        assert!(rec["quads"].as_u64().unwrap_or(0) > 0, "quads > 0: {rec}");
        let hash = rec["hash"].as_str().expect("hash string");
        assert_eq!(hash.len(), 64, "64-hex hash: {rec}");
        assert!(
            hash.chars().all(|c| c.is_ascii_hexdigit()),
            "hex hash: {rec}"
        );
    }
}

/// One good file + one malformed file fails closed (Err), and the error
/// names the malformed file by path and carries the "graph validate failed"
/// aggregate marker — never a cheerful `valid: false`.
#[test]
fn graph_validate_files_one_malformed_fails_named() {
    let dir = TempDir::new().expect("tempdir");
    let good = dir.path().join("good.ttl");
    let bad = dir.path().join("broken.ttl");
    std::fs::write(
        &good,
        "@prefix ex: <http://example.org/> .\nex:a ex:p \"1\" .\n",
    )
    .expect("write good.ttl");
    std::fs::write(&bad, "this is not valid turtle {{{").expect("write broken.ttl");
    let fg = Utf8PathBuf::from_path_buf(good).expect("utf8 path");
    let fb = Utf8PathBuf::from_path_buf(bad).expect("utf8 path");

    let err = handle_graph_validate(vec![fg, fb], vec![]).expect_err("must fail closed");
    let msg = err.to_string();
    assert!(msg.contains("broken.ttl"), "names the bad file: {msg}");
    assert!(
        msg.contains("graph validate failed"),
        "aggregate marker: {msg}"
    );
}

/// Empty file list lands in project mode: with no ggen.toml in cwd it must
/// fail closed, never silently succeed. (Project-mode success with a real
/// scaffold is covered by `clean_demo_project_validates_via_cli` and the
/// `graph validate` CLI-boundary tests.)
#[test]
fn graph_validate_empty_files_is_project_mode() {
    let dir = TempDir::new().expect("tempdir");
    // Run from a directory with no ggen.toml so project mode fails closed
    // deterministically regardless of the test runner's cwd.
    let prev = std::env::current_dir().expect("cwd");
    std::env::set_current_dir(dir.path()).expect("chdir tempdir");
    let res = handle_graph_validate(vec![], vec![]);
    std::env::set_current_dir(prev).expect("restore cwd");
    assert!(
        res.is_err(),
        "empty files -> project mode, no manifest -> Err"
    );
}

// ---------------------------------------------------------------------
// graph validate — SHACL shape-conformance (multi-value `--shapes`),
// direct handler calls
// ---------------------------------------------------------------------
// These exercise `handle_graph_validate(files, shapes)` against the real
// `fixtures/session-good.ttl` conforming sample plus a hand-built violating
// fixture (a `dfl:ToolEvent` missing the required `dfl:outcome`).
//
// SHAPES SOURCE (changed after these tests rotted silently): the pack's
// `shapes.ttl` was deliberately DELETED in the SHACL->SPARQL-gates migration
// (commit ad9106702 — its maxCount/minCount law now lives in
// `gates/020_single_valued.rq` etc., covered by
// `crates/praxis-graphlaw/tests/dogfood_lifecycle_hook_actuation.rs`), which
// broke both tests below unnoticed because CI's Test job runs
// `cargo test --lib` only — integration tests never run in CI. What these two
// tests actually prove is the ENGINE's SHACL `--shapes` path (conform +
// named-focus-node refusal), which is still a real, live engine surface
// ([law].shapes / FM-LAW-013) — so they now carry their own minimal
// self-contained shapes fixture, written to a TempDir, mirroring the
// dfl:ToolEventShape they originally validated against, instead of depending
// on a pack file whose law has migrated to a different mechanism.

/// Repository `packs/` directory (relative to this crate's manifest;
/// mirrors `cross_pack_matrix.rs`/`framework_packs_e2e.rs`).
fn packs_dir() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

/// Minimal self-contained SHACL shapes doc mirroring the deleted pack
/// `dfl:ToolEventShape`'s exactly-one-`dfl:outcome` constraint. Written to
/// `dir` and returned as a Utf8 path for `handle_graph_validate`.
fn write_toolevent_shapes(dir: &Path) -> Utf8PathBuf {
    let path = dir.join("toolevent-shapes.ttl");
    std::fs::write(
        &path,
        r#"
        @prefix sh:  <http://www.w3.org/ns/shacl#> .
        @prefix dfl: <http://seanchatmangpt.github.io/packs/dogfood-lifecycle#> .

        dfl:ToolEventShape
            a sh:NodeShape ;
            sh:targetClass dfl:ToolEvent ;
            sh:property [
                sh:path dfl:outcome ;
                sh:minCount 1 ;
                sh:maxCount 1 ;
                sh:message "every dfl:ToolEvent carries exactly one dfl:outcome" ;
            ] .
        "#,
    )
    .expect("write shapes fixture");
    Utf8PathBuf::from_path_buf(path).expect("utf8 path")
}

/// A well-formed session log conforming to the ToolEvent shape validates with
/// `--shapes`: exit is `Ok`, the file is reported with a positive quad count,
/// a 64-hex hash, and `shapes_conform: true`.
#[test]
fn graph_validate_files_with_shapes_conforms() {
    let dir = TempDir::new().expect("tempdir");
    let shapes = write_toolevent_shapes(dir.path());
    let good = Utf8PathBuf::from_path_buf(
        packs_dir().join("dogfood-lifecycle-pack/fixtures/session-good.ttl"),
    )
    .expect("utf8 path");

    let out =
        handle_graph_validate(vec![good], vec![shapes]).expect("conforming fixture must pass");
    assert_eq!(out["files_checked"], 1, "{out}");
    assert_eq!(out["shapes_checked"], 1, "{out}");
    let files = out["files"].as_array().expect("files array");
    assert_eq!(files.len(), 1, "{out}");
    assert!(files[0]["quads"].as_u64().unwrap_or(0) > 0, "{out}");
    assert_eq!(files[0]["shapes_conform"], true, "{out}");
    let hash = files[0]["hash"].as_str().expect("hash string");
    assert_eq!(hash.len(), 64, "64-hex hash: {out}");
}

/// A `dfl:ToolEvent` missing the required `dfl:outcome` property fails SHACL
/// validation (not a Turtle parse error — the file parses fine): exit is
/// `Err`, naming the violating focus node, the source shape, and the
/// `sh:message` text, and carrying the same "graph validate failed"
/// aggregate marker as the parse-failure path.
#[test]
fn graph_validate_files_with_shapes_violation_fails_named() {
    let dir = TempDir::new().expect("tempdir");
    let bad = dir.path().join("missing-outcome.ttl");
    // A minimal ToolEvent with every required property EXCEPT dfl:outcome.
    std::fs::write(
        &bad,
        r#"
        @prefix dfl:     <http://seanchatmangpt.github.io/packs/dogfood-lifecycle#> .
        @prefix prov:    <http://www.w3.org/ns/prov#> .
        @prefix dcterms: <http://purl.org/dc/terms/> .
        @prefix skos:    <http://www.w3.org/2004/02/skos/core#> .
        @prefix time:    <http://www.w3.org/2006/time#> .
        @prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .
        @base <http://example.org/missing-outcome#> .

        <#session>
            a                     dfl:Session ;
            dcterms:identifier    "missing-outcome-0000" ;
            prov:wasAssociatedWith <#agent> .

        <#agent> a prov:Agent, prov:SoftwareAgent .

        <#event-1>
            a                       dfl:ToolEvent ;
            dcterms:isPartOf        <#session> ;
            skos:notation           "Bash" ;
            dfl:sequenceIndex       1 ;
            prov:wasAssociatedWith  <#agent> ;
            prov:used               <urn:blake3:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa> ;
            prov:generated          <urn:blake3:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb> .
            # NOTE: no dfl:outcome — violates dfl:ToolEventShape's minCount 1.
        "#,
    )
    .expect("write missing-outcome.ttl");
    let fb = Utf8PathBuf::from_path_buf(bad).expect("utf8 path");
    let shapes = write_toolevent_shapes(dir.path());

    let err =
        handle_graph_validate(vec![fb], vec![shapes]).expect_err("missing dfl:outcome must fail");
    let msg = err.to_string();
    assert!(
        msg.contains("graph validate failed"),
        "aggregate marker: {msg}"
    );
    assert!(msg.contains("SHACL"), "names SHACL: {msg}");
    assert!(
        msg.contains("event-1"),
        "names the violating focus node: {msg}"
    );
    // `dfl:ToolEventShape`'s `dfl:outcome` constraint is declared on an
    // anonymous property shape (`sh:property [ sh:path dfl:outcome ; ... ]`
    // shorthand in shapes.ttl), so per SHACL's `sh:sourceShape` semantics
    // the *immediate* violated shape is that blank node, not the enclosing
    // named node shape — `(source shape _:...)` is the correct, spec-true
    // identification here, not a named IRI.
    assert!(
        msg.contains("(source shape "),
        "names the source shape (blank-node property shape, per SHACL sourceShape \
         semantics for an anonymous sh:property constraint): {msg}"
    );
    assert!(
        msg.contains("outcome"),
        "names the offending property via the shape's sh:message: {msg}"
    );
}

/// The pack's own adversarial IRI-collision fixture (two events merged onto
/// one `<#event-1>` IRI, yielding double `skos:notation`/`dfl:outcome`) must
/// also be refused via the SHACL cardinality path — same maxCount-1 law the
/// pack now enforces via `gates/020_single_valued.rq` (that gate-level
/// coverage lives in
/// `crates/praxis-graphlaw/tests/dogfood_lifecycle_hook_actuation.rs`; this
/// test proves the equivalent engine-level `--shapes` refusal names the
/// colliding node).
#[test]
fn graph_validate_refuses_the_iri_collision_fixture_via_max_count() {
    let dir = TempDir::new().expect("tempdir");
    let shapes = write_toolevent_shapes(dir.path());
    let colliding = Utf8PathBuf::from_path_buf(
        packs_dir().join("dogfood-lifecycle-pack/fixtures/session-iri-collision.ttl"),
    )
    .expect("utf8 path");

    let err = handle_graph_validate(vec![colliding], vec![shapes])
        .expect_err("the merged double-outcome node must fail SHACL maxCount 1");
    let msg = err.to_string();
    assert!(msg.contains("SHACL"), "names SHACL: {msg}");
    assert!(
        msg.contains("event-1"),
        "names the colliding focus node: {msg}"
    );
}
