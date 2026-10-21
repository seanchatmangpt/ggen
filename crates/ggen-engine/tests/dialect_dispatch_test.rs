#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Chicago tests for the declared-dialect input path (v26.10.20 dialect
//! slice 1, lane V03): `graph::load_declared` + `graph::dialect_for_path`
//! dispatch, typed refusals, and real parse behavior — real oxigraph store,
//! real content, assertions on quads actually inserted (state, not calls).

#![allow(clippy::pedantic)]
use ggen_engine::graph::{
    dialect_for_path, load_declared, DeterministicGraph, EngineQueryResults, GraphEngine,
    FD_DIALECT_FEATURE_DISALLOWED, FD_MALFORMED_SYNTAX, FD_REMOTE_CONTEXT_FORBIDDEN,
    FD_UNDECLARED_INPUT_DIALECT,
};

const TURTLE_DOC: &str = r#"
    @prefix ex: <http://example.org/> .
    ex:rex ex:isA ex:Dog .
"#;

const NTRIPLES_DOC: &str =
    "<http://example.org/rex> <http://example.org/isA> <http://example.org/Dog> .\n";

const NQUADS_DOC: &str = "<http://example.org/rex> <http://example.org/isA> <http://example.org/Dog> <http://example.org/g1> .\n";

const TRIG_DOC: &str = r#"
    @prefix ex: <http://example.org/> .
    ex:g1 { ex:rex ex:isA ex:Dog . }
"#;

fn count_dog(graph: &DeterministicGraph) -> usize {
    match GraphEngine::query(
        graph,
        "SELECT ?s WHERE { ?s <http://example.org/isA> <http://example.org/Dog> }",
    )
    .expect("query")
    {
        EngineQueryResults::Solutions(rows) => rows.len(),
        other => panic!("expected solutions, got {other:?}"),
    }
}

#[test]
fn turtle_dialect_loads_through_existing_path() {
    let g = DeterministicGraph::new().expect("graph");
    let n = load_declared(&g, "urn:test:dog.ttl", TURTLE_DOC, "turtle").expect("turtle load");
    assert_eq!(n, 1, "one triple inserted: {n}");
    assert_eq!(count_dog(&g), 1, "fact queryable after turtle load");
}

#[test]
fn ntriples_dialect_parses_and_inserts_real_quads() {
    let g = DeterministicGraph::new().expect("graph");
    let n = load_declared(&g, "urn:test:dog.nt", NTRIPLES_DOC, "ntriples").expect("nt load");
    assert_eq!(n, 1, "one triple inserted: {n}");
    assert_eq!(count_dog(&g), 1, "fact queryable after ntriples load");
}

#[test]
fn nquads_dialect_parses_and_inserts_real_quads() {
    let g = DeterministicGraph::new().expect("graph");
    let n = load_declared(&g, "urn:test:dog.nq", NQUADS_DOC, "nquads").expect("nq load");
    assert_eq!(n, 1, "one quad inserted: {n}");
    // The quad lands in its declared named graph (`ex:g1`), so it is present
    // in the store but not in the default graph — assert on store state.
    let quads = g.all_quads().expect("quads");
    assert_eq!(quads.len(), 1, "{quads:?}");
}

#[test]
fn trig_dialect_parses_and_inserts_named_graph_quad() {
    let g = DeterministicGraph::new().expect("graph");
    let n = load_declared(&g, "urn:test:dog.trig", TRIG_DOC, "trig").expect("trig load");
    assert_eq!(n, 1, "one quad inserted: {n}");
    let quads = g.all_quads().expect("quads");
    assert_eq!(quads.len(), 1, "{quads:?}");
}

const RDF_XML_DOC: &str = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ex="http://example.org/">
  <rdf:Description rdf:about="http://example.org/rex">
    <ex:isA rdf:resource="http://example.org/Dog"/>
  </rdf:Description>
</rdf:RDF>
"#;

const JSONLD_EXPANDED_DOC: &str = r#"[
  {
    "@id": "http://example.org/rex",
    "http://example.org/isA": [{ "@id": "http://example.org/Dog" }]
  }
]
"#;

const JSONLD_REMOTE_CONTEXT_DOC: &str = r#"{
  "@context": "https://schema.org/",
  "@id": "http://example.org/rex",
  "isA": { "@id": "http://example.org/Dog" }
}
"#;

const JSONLD_LOCAL_CONTEXT_DOC: &str = r#"{
  "@context": { "ex": "http://example.org/" },
  "@id": "ex:rex",
  "ex:isA": { "@id": "ex:Dog" }
}
"#;

#[test]
fn rdf_xml_dialect_parses_and_inserts_real_quads() {
    // V04's probe (~/graphlaw tests/parser_capability_probe.rs) proved purrdf
    // parses rdf-xml; the pinned oxigraph oxrdfio carries RdfXml natively, so
    // the same quad-format seam admits it here — real parse, real state.
    let g = DeterministicGraph::new().expect("graph");
    let n = load_declared(&g, "urn:test:dog.rdf", RDF_XML_DOC, "rdf-xml").expect("rdf-xml load");
    assert_eq!(n, 1, "one triple inserted: {n}");
    assert_eq!(count_dog(&g), 1, "fact queryable after rdf-xml load");
}

#[test]
fn json_ld_expanded_dialect_parses_and_inserts_real_quads() {
    let g = DeterministicGraph::new().expect("graph");
    let n = load_declared(&g, "urn:test:dog.jsonld", JSONLD_EXPANDED_DOC, "json-ld")
        .expect("json-ld load");
    assert_eq!(n, 1, "one triple inserted: {n}");
    assert_eq!(count_dog(&g), 1, "fact queryable after json-ld load");
}

#[test]
fn json_ld_local_context_is_admitted() {
    // Inline object @context mappings are local by definition.
    let g = DeterministicGraph::new().expect("graph");
    load_declared(
        &g,
        "urn:test:local.jsonld",
        JSONLD_LOCAL_CONTEXT_DOC,
        "json-ld",
    )
    .expect("local-context json-ld must be admitted");
    assert_eq!(g.all_quads().expect("quads").len(), 1);
}

#[test]
fn json_ld_remote_context_is_typed_refusal_fail_closed() {
    // Top-level string @context containing `://` → FD-REMOTE_CONTEXT_FORBIDDEN
    // (guard copied from graphlaw::dialect::jsonld_remote_context); nothing
    // may be fetched, nothing inserted.
    let g = DeterministicGraph::new().expect("graph");
    let err = load_declared(
        &g,
        "urn:test:remote.jsonld",
        JSONLD_REMOTE_CONTEXT_DOC,
        "json-ld",
    )
    .expect_err("remote context must refuse");
    let msg = err.to_string();
    assert!(msg.contains(FD_REMOTE_CONTEXT_FORBIDDEN), "{msg}");
    assert!(msg.contains("urn:test:remote.jsonld"), "{msg}");
    assert_eq!(count_dog(&g), 0, "refused input must not mutate the graph");
}

#[test]
fn undeclared_dialect_string_is_typed_refusal() {
    let g = DeterministicGraph::new().expect("graph");
    let err = load_declared(&g, "urn:test:x.rtf", TURTLE_DOC, "rtf").expect_err("must refuse");
    let msg = err.to_string();
    assert!(msg.contains(FD_UNDECLARED_INPUT_DIALECT), "{msg}");
    assert!(msg.contains("rtf"), "{msg}");
    assert!(msg.contains("urn:test:x.rtf"), "{msg}");
    // Fail-closed: nothing was inserted.
    assert_eq!(count_dog(&g), 0, "refused input must not mutate the graph");
}

#[test]
fn undeclared_file_extension_is_typed_refusal() {
    let err = dialect_for_path(std::path::Path::new("ontologies/thing.rtf"))
        .expect_err("undeclared extension must refuse");
    let msg = err.to_string();
    assert!(msg.contains(FD_UNDECLARED_INPUT_DIALECT), "{msg}");
    assert!(msg.contains("thing.rtf"), "{msg}");
}

#[test]
fn n3_dialect_is_disallowed_with_graphlaw_routing_hint() {
    let g = DeterministicGraph::new().expect("graph");
    let err = load_declared(&g, "urn:test:rules.n3", "{?s a ?C} => {?s a ?D}.", "n3")
        .expect_err("n3 must be refused");
    let msg = err.to_string();
    assert!(msg.contains(FD_DIALECT_FEATURE_DISALLOWED), "{msg}");
    assert!(msg.contains("graphlaw"), "{msg}");
    assert_eq!(count_dog(&g), 0);
}

#[test]
fn n3_file_under_turtle_glob_is_disallowed_not_sniffed_as_turtle() {
    // The declared dialect wins over content sniffing: a .n3 rule document
    // declared (by extension dispatch) as `n3` is a typed disallow, never a
    // silent best-effort turtle parse.
    let err = dialect_for_path(std::path::Path::new("rules/inference.n3"))
        .expect_err(".n3 must be refused");
    let msg = err.to_string();
    assert!(msg.contains(FD_DIALECT_FEATURE_DISALLOWED), "{msg}");
    assert!(msg.contains("inference.n3"), "{msg}");
}

#[test]
fn malformed_content_under_declared_dialect_is_typed_refusal() {
    let g = DeterministicGraph::new().expect("graph");
    let err = load_declared(
        &g,
        "urn:test:broken.nt",
        "not n-triples at all <<<",
        "ntriples",
    )
    .expect_err("malformed ntriples must refuse");
    let msg = err.to_string();
    assert!(msg.contains(FD_MALFORMED_SYNTAX), "{msg}");
    assert!(msg.contains("ntriples"), "{msg}");
    assert!(msg.contains("urn:test:broken.nt"), "{msg}");
    assert_eq!(count_dog(&g), 0);
}

#[test]
fn malformed_turtle_under_declared_turtle_refuses_fail_closed() {
    let g = DeterministicGraph::new().expect("graph");
    let err = load_declared(
        &g,
        "urn:test:broken.ttl",
        "@prefix ex: <http://e/> . ex:x",
        "turtle",
    )
    .expect_err("truncated turtle must refuse");
    assert!(err.to_string().contains("FM-GRAPH-002"), "{err}");
    assert_eq!(count_dog(&g), 0);
}

#[test]
fn dialect_extension_table_resolves_first_match_per_extension() {
    let cases = [
        ("doc.ttl", "turtle"),
        ("doc.TTL", "turtle"),
        ("doc.nt", "ntriples"),
        ("doc.nq", "nquads"),
        ("doc.trig", "trig"),
        ("doc.rdf", "rdf-xml"),
        ("doc.owl", "rdf-xml"),
        ("doc.jsonld", "json-ld"),
    ];
    for (file, want) in cases {
        let got = dialect_for_path(std::path::Path::new(file)).unwrap_or_else(|e| {
            panic!("{file} must resolve to {want}, got {e}");
        });
        assert_eq!(got, want, "{file}");
    }
}

// -- pack `dialects` glob resolution (V01's DialectGlob surface) ------------

use ggen_engine::pack::{DialectGlob, Pack};

fn glob(pattern: &str, dialect: &str) -> DialectGlob {
    DialectGlob {
        glob: pattern.to_string(),
        dialect: dialect.to_string(),
    }
}

fn pack_with_globs(globs: Vec<DialectGlob>) -> (Pack, std::path::PathBuf) {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let pack = Pack {
        name: "glob-pack".to_string(),
        version: "1.0.0".to_string(),
        description: String::new(),
        dependencies: Default::default(),
        semantic_types: Default::default(),
        provides: Default::default(),
        requires: Default::default(),
        root: dir.path().to_path_buf(),
        ontology_path: dir.path().join("ontology.ttl"),
        extra_ontology_paths: Vec::new(),
        template_paths: Vec::new(),
        dialects: globs,
        lock: false,
    };
    (pack, dir.path().to_path_buf())
}

#[test]
fn pack_glob_first_match_wins() {
    let (pack, root) = pack_with_globs(vec![
        glob("shapes/*.trig", "trig"),
        glob("shapes/*", "turtle"),
        glob("*.ttl", "turtle"),
    ]);
    let got = ggen_engine::graph::dialect_for_pack_globs(
        &pack.name,
        &root,
        &root.join("shapes/graph.trig"),
        &pack.dialects,
    )
    .expect("must resolve");
    assert_eq!(got, "trig", "first declared matching glob wins");
    let got = ggen_engine::graph::dialect_for_pack_globs(
        &pack.name,
        &root,
        &root.join("ont.ttl"),
        &pack.dialects,
    )
    .expect("must resolve");
    assert_eq!(got, "turtle");
}

#[test]
fn pack_glob_default_ttl_glob_matches_nested_files_by_name() {
    // The synthesized default is `*.ttl -> turtle`; `*` does not cross `/`,
    // so a nested file matches via its bare name (declared "paths/names"
    // contract).
    let (pack, root) = pack_with_globs(vec![glob("*.ttl", "turtle")]);
    let got = ggen_engine::graph::dialect_for_pack_globs(
        &pack.name,
        &root,
        &root.join("sub/dir/ont.ttl"),
        &pack.dialects,
    )
    .expect("default *.ttl must match nested file by name");
    assert_eq!(got, "turtle");
}

#[test]
fn pack_file_matching_no_glob_is_undeclared_refusal() {
    let (pack, root) = pack_with_globs(vec![glob("*.ttl", "turtle")]);
    let err = ggen_engine::graph::dialect_for_pack_globs(
        &pack.name,
        &root,
        &root.join("facts.nq"),
        &pack.dialects,
    )
    .expect_err("undeclared pack input must refuse");
    let msg = err.to_string();
    assert!(msg.contains(FD_UNDECLARED_INPUT_DIALECT), "{msg}");
    assert!(msg.contains("glob-pack"), "{msg}");
    assert!(msg.contains("facts.nq"), "{msg}");
}

#[test]
fn glob_star_does_not_cross_slashes_double_star_does() {
    assert!(ggen_engine::graph::glob_matches("*.ttl", "ont.ttl"));
    assert!(!ggen_engine::graph::glob_matches("*.ttl", "sub/ont.ttl"));
    assert!(ggen_engine::graph::glob_matches("**/*.trig", "a/b/c.trig"));
    assert!(ggen_engine::graph::glob_matches(
        "shapes/*.trig",
        "shapes/g.trig"
    ));
    assert!(!ggen_engine::graph::glob_matches(
        "shapes/*.trig",
        "other/g.trig"
    ));
}

#[test]
fn end_to_end_nquads_pack_file_loads_through_load_declared() {
    // Chicago: real files on disk, real resolution, real store mutation.
    let dir = tempfile::TempDir::new().expect("tempdir");
    let nq = dir.path().join("facts.nq");
    std::fs::write(&nq, NQUADS_DOC).expect("write nq");
    let g = DeterministicGraph::new().expect("graph");
    let content = std::fs::read_to_string(&nq).expect("read nq");
    let dialect = dialect_for_path(&nq).expect("declared dialect");
    let n = load_declared(&g, "facts.nq", &content, dialect).expect("load");
    assert_eq!(n, 1);
    assert_eq!(g.all_quads().expect("quads").len(), 1);
}
