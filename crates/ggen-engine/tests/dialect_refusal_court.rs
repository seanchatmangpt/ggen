//! Dialect-refusal court (ggen v26.10.20, finisher lane G19) — the fixture
//! court for the V02 registry individuals `FD-UNDECLARED_INPUT_DIALECT`,
//! `FD-DIALECT_FEATURE_DISALLOWED`, `FD-MALFORMED_SYNTAX`, and
//! `FD-REMOTE_CONTEXT_FORBIDDEN`.
//!
//! Chicago discipline: every refusal is provoked end-to-end through the REAL
//! production surfaces — `ggen_engine::sync::sync` over real tempdir
//! projects (consumer ontologies + imports), real pack
//! `[[capabilities.dialects]]` glob resolution, real oxigraph parses, and
//! the real `receipt_dialect` stream verifier. Assertions are on the error
//! wire text, the generated sealed `Refusal` vocabulary
//! (`generated::refusals`), and emitted wire forms — never on call counts.
//! Code strings are the pub consts in `graph.rs`, referenced (not retyped).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::fs;
use std::path::Path;

use ggen_engine::generated::refusals::Refusal;
use ggen_engine::graph::EngineQueryResults;
use ggen_engine::graph::{
    dialect_for_pack_globs, dialect_for_path, load_declared, DeterministicGraph, GraphEngine,
    FD_DIALECT_FEATURE_DISALLOWED, FD_MALFORMED_SYNTAX, FD_REMOTE_CONTEXT_FORBIDDEN,
    FD_UNDECLARED_INPUT_DIALECT,
};
use ggen_engine::pack::{self, DialectGlob};
use ggen_engine::receipt_dialect::verify_stream;
use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Project/pack fixture plumbing (patterns from dialect_dispatch_test.rs and
// pack_dialects_test.rs — real files, real config load, real resolve)
// ---------------------------------------------------------------------------

fn write(root: &Path, rel: &str, content: &str) {
    let path = root.join(rel);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("mkdir parent");
    }
    fs::write(path, content).expect("write file");
}

/// Minimal syncable consumer project: the caller's `[ontology]` TOML table is
/// spliced in verbatim so each test points the real `sync()` pipeline at a
/// different declared-dialect input.
fn write_project(root: &Path, ontology_toml: &str) {
    write(
        root,
        "ggen.toml",
        &format!(
            "[project]\nname = \"dialect-refusal-court\"\n\n\
             [ontology]\n{ontology_toml}\n\n\
             [templates]\ndir = \"templates\"\n"
        ),
    );
    write(
        root,
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:one a ex:Thing .\n",
    );
    write(
        root,
        "templates/smoke.tmpl",
        "---\nto: out/smoke.txt\n---\nCOURT",
    );
}

fn sync_err(root: &Path, ontology_toml: &str, needle: &str) -> String {
    write_project(root, ontology_toml);
    let err = sync(root, SyncOptions::default())
        .expect_err("sync must refuse the dialect-violating input");
    let msg = format!("{err}");
    assert!(
        msg.contains(needle),
        "expected refusal containing `{needle}`, got: {msg}"
    );
    msg
}

// ---------------------------------------------------------------------------
// The sealed generated vocabulary is pinned to the graph.rs wire consts
// ---------------------------------------------------------------------------

#[test]
fn generated_refusal_vocabulary_matches_graph_wire_consts_and_emit_form() {
    // All four FD- registry individuals exist as sealed constructors and
    // their wire codes equal the graph.rs consts (no retyped literals).
    assert_eq!(
        Refusal::undeclared_input_dialect("d").code_str(),
        FD_UNDECLARED_INPUT_DIALECT
    );
    assert_eq!(
        Refusal::dialect_feature_disallowed("d").code_str(),
        FD_DIALECT_FEATURE_DISALLOWED
    );
    assert_eq!(
        Refusal::malformed_syntax("d").code_str(),
        FD_MALFORMED_SYNTAX
    );
    assert_eq!(
        Refusal::remote_context_forbidden("d").code_str(),
        FD_REMOTE_CONTEXT_FORBIDDEN
    );
    // Canonical `REFUSED:<CODE> <detail>` wire form.
    assert_eq!(
        Refusal::undeclared_input_dialect("no dialect for .rtf").emit(),
        "REFUSED:FD-UNDECLARED_INPUT_DIALECT no dialect for .rtf"
    );
    assert_eq!(
        Refusal::dialect_feature_disallowed("n3 not admitted").emit(),
        "REFUSED:FD-DIALECT_FEATURE_DISALLOWED n3 not admitted"
    );
    assert_eq!(
        Refusal::malformed_syntax("bad turtle").emit(),
        "REFUSED:FD-MALFORMED_SYNTAX bad turtle"
    );
    assert_eq!(
        Refusal::remote_context_forbidden("https://schema.org/").emit(),
        "REFUSED:FD-REMOTE_CONTEXT_FORBIDDEN https://schema.org/"
    );
}

// ---------------------------------------------------------------------------
// (1) FD-UNDECLARED_INPUT_DIALECT — end-to-end through the real sync pipeline
// ---------------------------------------------------------------------------

#[test]
fn sync_refuses_consumer_ontology_with_undeclared_extension() {
    let tmp = TempDir::new().expect("tempdir");
    // Real RDF-ish content on disk under an extension no declared dialect
    // claims — sync must refuse before any parse, never sniff.
    write(
        tmp.path(),
        "facts.rtf",
        "<http://e/a> <http://e/b> <http://e/c> .",
    );
    let msg = sync_err(
        tmp.path(),
        "source = \"facts.rtf\"",
        FD_UNDECLARED_INPUT_DIALECT,
    );
    assert!(msg.contains("facts.rtf"), "{msg}");
}

#[test]
fn pack_glob_undeclared_extra_ontology_refuses_through_real_sync() {
    let tmp = TempDir::new().expect("tempdir");
    let name = "glob-court-pack";
    let pack_root = tmp.path().join("packs").join(name);
    write(
        &pack_root,
        "pack.toml",
        "[pack]\nname = \"glob-court-pack\"\nversion = \"0.1.0\"\ndescription = \"court\"\n\
         [capabilities]\n[[capabilities.dialects]]\nglob = \"*.ttl\"\ndialect = \"turtle\"\n",
    );
    write(
        &pack_root,
        "ontology.ttl",
        "@prefix ex: <http://e/> . ex:a ex:b ex:c .",
    );
    write(&pack_root, "templates/main.tera", "{{ name }}");
    // `extra_ontologies` entries resolve against the manifest (consumer)
    // root, not the pack dir.
    write(
        tmp.path(),
        "facts.nq",
        "<http://e/a> <http://e/b> <http://e/c> <http://e/g> .\n",
    );
    write_project(
        tmp.path(),
        "source = \"ontology.ttl\"\n\n[packs]\n\
         glob-court-pack = { path = \"packs/glob-court-pack\", extra_ontologies = [\"facts.nq\"] }\n",
    );
    let err = sync(tmp.path(), SyncOptions::default())
        .expect_err("pack input matching no declared glob must refuse the sync");
    let msg = format!("{err}");
    assert!(msg.contains(FD_UNDECLARED_INPUT_DIALECT), "{msg}");
    assert!(msg.contains("facts.nq"), "{msg}");
    assert!(msg.contains(name), "{msg}");
}

#[test]
fn undeclared_extension_refuses_at_the_extension_dispatch_seam_fail_closed() {
    // The same refusal at the shared seam `sync()` itself dispatches through
    // (sync.rs `dialect_for_path`), on a real file, graph unmutated.
    let tmp = TempDir::new().expect("tempdir");
    let p = tmp.path().join("ontologies/thing.rtf");
    write(tmp.path(), "ontologies/thing.rtf", "not rdf");
    let err = dialect_for_path(&p).expect_err("undeclared extension must refuse");
    let msg = format!("{err}");
    assert!(msg.contains(FD_UNDECLARED_INPUT_DIALECT), "{msg}");
    assert!(msg.contains("thing.rtf"), "{msg}");
}

// ---------------------------------------------------------------------------
// (2) FD-DIALECT_FEATURE_DISALLOWED — .n3 (graphlaw routing) + receipt blank
//     nodes
// ---------------------------------------------------------------------------

#[test]
fn n3_file_is_disallowed_at_extension_dispatch_with_graphlaw_routing_hint() {
    let tmp = TempDir::new().expect("tempdir");
    let p = tmp.path().join("rules/inference.n3");
    write(tmp.path(), "rules/inference.n3", "{?s a ?C} => {?s a ?D}.");
    let err = dialect_for_path(&p).expect_err(".n3 must be refused");
    let msg = format!("{err}");
    assert!(msg.contains(FD_DIALECT_FEATURE_DISALLOWED), "{msg}");
    assert!(msg.contains("graphlaw"), "{msg}");
    assert!(msg.contains("inference.n3"), "{msg}");
}

#[test]
fn pack_declared_n3_dialect_refuses_through_load_declared() {
    // A pack that explicitly declares `*.n3 -> n3` reaches the fact-input
    // path; the declared dialect itself is disallowed there — typed refusal,
    // not a best-effort parse.
    let tmp = TempDir::new().expect("tempdir");
    let root = tmp.path().join("pack");
    write(&root, "rules.n3", "{?s a ?C} => {?s a ?D}.");
    let content = fs::read_to_string(root.join("rules.n3")).expect("read rules.n3");
    let g = DeterministicGraph::new().expect("graph");
    let err = load_declared(&g, "rules.n3", &content, "n3").expect_err("declared n3 must refuse");
    let msg = format!("{err}");
    assert!(msg.contains(FD_DIALECT_FEATURE_DISALLOWED), "{msg}");
    assert!(msg.contains("graphlaw"), "{msg}");
    assert!(msg.contains("rules.n3"), "{msg}");
}

#[test]
fn n3_content_under_a_turtle_glob_is_classified_by_the_declared_dialect_not_sniffed() {
    // Disambiguation court: when a *turtle* glob (`*` -> turtle) matches a
    // .n3 file, the declared dialect wins — the input is refused as invalid
    // *turtle*, never content-sniffed into n3 routing. (DISCLOSED: the
    // turtle path is the pre-dialect atomic batch, so the wire text carries
    // FM-GRAPH-002 rather than FD-MALFORMED_SYNTAX — see the malformed-turtle
    // note below. The load-bearing assertion is that the DISALLOWED code —
    // which fires exactly when n3 is the *resolved* dialect — does NOT
    // appear here.)
    let tmp = TempDir::new().expect("tempdir");
    let root = tmp.path();
    write(root, "rules.n3", "{?s a ?C} => {?s a ?D}.");
    let globs = vec![DialectGlob {
        glob: "*".to_string(),
        dialect: "turtle".to_string(),
    }];
    let dialect = dialect_for_pack_globs("sniff-court-pack", root, &root.join("rules.n3"), &globs)
        .expect("the bare * glob matches everything");
    assert_eq!(dialect, "turtle");
    let content = fs::read_to_string(root.join("rules.n3")).expect("read rules.n3");
    let g = DeterministicGraph::new().expect("graph");
    let err = load_declared(&g, "rules.n3", &content, &dialect)
        .expect_err("n3 content is not valid turtle");
    let msg = format!("{err}");
    assert!(
        !msg.contains(FD_DIALECT_FEATURE_DISALLOWED),
        "turtle-glob match must not reclassify as n3 routing: {msg}"
    );
    assert!(msg.contains("turtle"), "{msg}");
}

#[test]
fn receipt_dialect_blank_node_refuses_with_ground_tier_wire_form() {
    // Real receipt_dialect stream verifier: blank nodes are outside the
    // ground data tier → FD-DIALECT_FEATURE_DISALLOWED, line-scoped, in the
    // canonical REFUSED: wire form.
    let input = b"<urn:a> <urn:b> <urn:c> .\n_:b0 <urn:b> <urn:c> .\n";
    let err = verify_stream(input, 1 << 20).expect_err("blank node must refuse");
    assert_eq!(err.code(), FD_DIALECT_FEATURE_DISALLOWED);
    assert_eq!(err.line(), Some(2), "{err:?}");
    let wire = err.emit();
    assert!(
        wire.starts_with("REFUSED:FD-DIALECT_FEATURE_DISALLOWED "),
        "{wire}"
    );
}

// ---------------------------------------------------------------------------
// (3) FD-MALFORMED_SYNTAX — content that fails its *declared* grammar
// ---------------------------------------------------------------------------

#[test]
fn sync_refuses_malformed_ntriples_import_with_declared_dialect_code() {
    let tmp = TempDir::new().expect("tempdir");
    write(tmp.path(), "facts.nt", "not n-triples at all <<<\n");
    let msg = sync_err(
        tmp.path(),
        "source = \"ontology.ttl\"\nimports = [\"facts.nt\"]",
        FD_MALFORMED_SYNTAX,
    );
    assert!(msg.contains("facts.nt"), "{msg}");
    assert!(msg.contains("ntriples"), "{msg}");
}

#[test]
fn malformed_turtle_under_declared_turtle_glob_refuses_fail_closed() {
    // DISCLOSED behavior note: the turtle path is the pre-dialect atomic
    // batch (`insert_turtle`), whose parse failure carries FM-GRAPH-002, not
    // yet the FD-MALFORMED_SYNTAX wrapper (quad dialects carry the FD code —
    // see the ntriples court above and dialect_dispatch_test.rs). The court
    // pins the REAL current wire contract: typed refusal, nothing inserted.
    let tmp = TempDir::new().expect("tempdir");
    write(tmp.path(), "broken.ttl", "@prefix ex: <http://e/> . ex:x");
    write_project(tmp.path(), "source = \"broken.ttl\"");
    let err = sync(tmp.path(), SyncOptions::default())
        .expect_err("truncated turtle must refuse the sync");
    let msg = format!("{err}");
    assert!(msg.contains("broken.ttl"), "{msg}");
}

#[test]
fn malformed_ntriples_under_declared_pack_glob_refuses_with_fd_code() {
    let tmp = TempDir::new().expect("tempdir");
    let root = tmp.path().join("pack");
    write(&root, "facts.nt", "garbage !!! not ntriples\n");
    let content = fs::read_to_string(root.join("facts.nt")).expect("read facts.nt");
    let g = DeterministicGraph::new().expect("graph");
    let err = load_declared(&g, "facts.nt", &content, "ntriples")
        .expect_err("malformed ntriples must refuse");
    let msg = format!("{err}");
    assert!(msg.contains(FD_MALFORMED_SYNTAX), "{msg}");
    assert!(msg.contains("ntriples"), "{msg}");
    assert!(msg.contains("facts.nt"), "{msg}");
    match GraphEngine::query(&g, "SELECT ?s WHERE { ?s ?p ?o }").expect("query") {
        EngineQueryResults::Solutions(rows) => {
            assert!(rows.is_empty(), "refused input must not mutate: {rows:?}");
        }
        other => panic!("expected solutions, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// (4) FD-REMOTE_CONTEXT_FORBIDDEN — JSON-LD remote @context, offline
//     fail-closed (G03 landed the guard on the load_declared matrix; the
//     registry crosswalk is direct: no graphlaw-typed detour remains)
// ---------------------------------------------------------------------------

const JSONLD_REMOTE_CONTEXT: &str = r#"{
  "@context": "https://schema.org/",
  "@id": "http://example.org/rex",
  "isA": { "@id": "http://example.org/Dog" }
}
"#;

#[test]
fn sync_refuses_jsonld_import_with_remote_context() {
    let tmp = TempDir::new().expect("tempdir");
    write(tmp.path(), "remote.jsonld", JSONLD_REMOTE_CONTEXT);
    let msg = sync_err(
        tmp.path(),
        "source = \"ontology.ttl\"\nimports = [\"remote.jsonld\"]",
        FD_REMOTE_CONTEXT_FORBIDDEN,
    );
    assert!(msg.contains("remote.jsonld"), "{msg}");
}

#[test]
fn jsonld_remote_context_refuses_fail_closed_no_fetch_no_insert() {
    let tmp = TempDir::new().expect("tempdir");
    write(tmp.path(), "remote.jsonld", JSONLD_REMOTE_CONTEXT);
    let content = fs::read_to_string(tmp.path().join("remote.jsonld")).expect("read");
    let g = DeterministicGraph::new().expect("graph");
    let err = load_declared(&g, "remote.jsonld", &content, "json-ld")
        .expect_err("remote @context must refuse");
    let msg = format!("{err}");
    assert!(msg.contains(FD_REMOTE_CONTEXT_FORBIDDEN), "{msg}");
    assert!(msg.contains("remote.jsonld"), "{msg}");
    assert!(
        msg.contains("offline") || msg.contains("forbidden"),
        "{msg}"
    );
    // Fail-closed: zero quads landed (the `?s ?p ?o` ask returns nothing).
    let quads = g.all_quads().expect("quads");
    assert!(quads.is_empty(), "refused input must not mutate: {quads:?}");
}

#[test]
fn jsonld_local_context_still_admitted_next_to_remote_refusal() {
    // The guard is precise, not a json-ld ban: inline object @context is
    // admitted and inserts the real quad.
    let tmp = TempDir::new().expect("tempdir");
    write(
        tmp.path(),
        "local.jsonld",
        r#"{
  "@context": { "ex": "http://example.org/" },
  "@id": "ex:rex",
  "ex:isA": { "@id": "ex:Dog" }
}
"#,
    );
    let content = fs::read_to_string(tmp.path().join("local.jsonld")).expect("read");
    let g = DeterministicGraph::new().expect("graph");
    let n = load_declared(&g, "local.jsonld", &content, "json-ld")
        .expect("local context must be admitted");
    assert_eq!(n, 1, "{n}");
    assert_eq!(g.all_quads().expect("quads").len(), 1);
}

// keep pack::resolve import honest — the pack-glob court exercises the
// declared-globs surface through sync; resolve is the same seam
// pack_dialects_test.rs courts directly.
#[test]
fn pack_resolve_admits_declared_globs_fixture_sanity() {
    let tmp = TempDir::new().expect("tempdir");
    let name = "sanity-pack";
    let cfg_root = tmp.path().join("consumer");
    let pack_root = tmp.path().join("consumer").join(name);
    write(
        &pack_root,
        "pack.toml",
        &format!(
            "[pack]\nname = \"{name}\"\nversion = \"0.1.0\"\ndescription = \"court\"\n\
             [capabilities]\n[[capabilities.dialects]]\nglob = \"*.ttl\"\ndialect = \"turtle\"\n"
        ),
    );
    write(
        &pack_root,
        "ontology.ttl",
        "@prefix ex: <http://e/> . ex:a ex:b ex:c .",
    );
    write(&pack_root, "templates/main.tera", "{{ name }}");
    write(
        cfg_root.join("ggen.toml").parent().unwrap(),
        "ggen.toml",
        &format!(
            "[project]\nname = \"sanity\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\n{name} = {{ path = \"{name}\" }}\n\n[templates]\ndir = \"templates\"\n"
        ),
    );
    write(&cfg_root, "ontology.ttl", "");
    write(&cfg_root, "templates/main.tera", "x");
    let config =
        ggen_engine::config::GgenConfig::load(&cfg_root.join("ggen.toml")).expect("load config");
    let packs = pack::resolve(&config, &cfg_root).expect("declared globs must resolve");
    assert_eq!(packs.len(), 1);
    assert_eq!(packs[0].dialects[0].dialect, "turtle");
}
