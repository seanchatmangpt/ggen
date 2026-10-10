//! Live smoke of every read-only tool handler against real repo/fixture
//! data. Chicago TDD: real TempDir projects, real ontology/templates, real
//! handler functions — no mocks. Each tool gets (a) a happy-path invocation
//! with one content assertion, (b) one hostile input asserted to return a
//! typed error (never panic), and (c) a reported (unasserted) latency.
//!
//! Note: `ggen_receipt_verify`'s clean-receipt path (real sync + real
//! receipt) is fully covered by `receipt_verify_test.rs`; here we smoke the
//! missing-receipt degraded path only, to keep this suite cheap.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests

mod common;

use std::time::Instant;

use common::{write_declarative_project, write_frontmatter_project};

use ggen_mcp::tools::{
    capability_status::{capability_status, CapabilityStatusParams},
    check_project::{check_project, CheckProjectParams},
    config_classify::{config_classify, ConfigClassifyParams},
    frontmatter_lint::{frontmatter_lint, FrontmatterLintParams},
    frontmatter_schema::{frontmatter_schema, FrontmatterSchemaParams},
    pack_capabilities::{pack_capabilities, PackCapabilitiesParams},
    pack_query::{pack_query, PackQueryParams},
    query_preview::{query_preview, QueryPreviewParams},
    receipt_verify::{receipt_verify, ReceiptVerifyParams},
    rule_graph::{rule_graph, RuleGraphParams},
    sync_dry_run::{sync_dry_run, SyncDryRunParams},
};

fn millis(label: &str, started: Instant) {
    println!("latency {label}: {} ms", started.elapsed().as_millis());
}

// ---------------------------------------------------------------------------
// ggen_query_preview — real SPARQL over a real ontology fixture
// ---------------------------------------------------------------------------

#[test]
fn query_preview_live_select_returns_real_rows() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let t = Instant::now();
    let got = query_preview(&QueryPreviewParams {
        root: dir.path().display().to_string(),
        sparql: "SELECT ?name WHERE { ?s <http://example.org/hasName> ?name } ORDER BY ?name"
            .to_string(),
        max_rows: None,
    })
    .expect("query_preview must succeed on a real project graph");
    millis("query_preview happy", t);

    assert!(got.ok);
    assert_eq!(
        got.row_count, 2,
        "fixture ontology has exactly two named people"
    );
    assert_eq!(
        got.rows[0].get("name").and_then(|v| v.as_str()),
        Some("alice"),
        "ORDER BY must surface alice first"
    );
}

#[test]
fn query_preview_hostile_empty_sparql_is_typed_error_not_panic() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let t = Instant::now();
    let got = query_preview(&QueryPreviewParams {
        root: dir.path().display().to_string(),
        sparql: String::new(),
        max_rows: None,
    });
    millis("query_preview hostile", t);
    assert!(got.is_err(), "empty SPARQL must be a typed error");
}

#[test]
fn query_preview_hostile_missing_root_is_typed_error_not_panic() {
    let got = query_preview(&QueryPreviewParams {
        root: "/nonexistent/ggen-mcp-live-smoke/root".to_string(),
        sparql: "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
        max_rows: None,
    });
    assert!(got.is_err(), "missing root must be a typed error");
}

// ---------------------------------------------------------------------------
// ggen_config_classify
// ---------------------------------------------------------------------------

#[test]
fn config_classify_live_frontmatter_project() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let t = Instant::now();
    let got = config_classify(&ConfigClassifyParams {
        root: dir.path().display().to_string(),
    })
    .expect("classify must succeed on a real project");
    millis("config_classify happy", t);

    assert!(got.ok);
    assert_eq!(got.schema, "frontmatter");
}

#[test]
fn config_classify_hostile_missing_root_is_typed_error_not_panic() {
    let t = Instant::now();
    let got = config_classify(&ConfigClassifyParams {
        root: "/nonexistent/ggen-mcp-live-smoke/root".to_string(),
    });
    millis("config_classify hostile", t);
    assert!(got.is_err(), "missing root must be a typed error");
}

// ---------------------------------------------------------------------------
// ggen_frontmatter_schema — derived live from the engine's own schema
// ---------------------------------------------------------------------------

#[test]
fn frontmatter_schema_live_lists_the_real_keys() {
    let t = Instant::now();
    let got = frontmatter_schema(&FrontmatterSchemaParams { key: None })
        .expect("frontmatter_schema must succeed with no args");
    millis("frontmatter_schema happy", t);

    assert!(got.ok);
    let names: Vec<&str> = got.keys.iter().map(|k| k.name.as_str()).collect();
    assert!(
        names.contains(&"to") && names.contains(&"sparql"),
        "the engine's own schema must include the keys the real fixture template uses, got {names:?}"
    );
}

// ---------------------------------------------------------------------------
// ggen_frontmatter_lint — real template from the fixture project
// ---------------------------------------------------------------------------

#[test]
fn frontmatter_lint_live_clean_template_reports_binding_sets() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let t = Instant::now();
    let got = frontmatter_lint(&FrontmatterLintParams {
        root: dir.path().display().to_string(),
        template_path: "templates/names.tmpl".to_string(),
    })
    .expect("lint must succeed on a real template");
    millis("frontmatter_lint happy", t);

    assert!(got.ok);
    assert!(
        got.projected_vars
            .as_ref()
            .is_some_and(|v| v.contains("name")),
        "the fixture SELECT projects ?name, got {:?}",
        got.projected_vars
    );
}

#[test]
fn frontmatter_lint_hostile_missing_template_is_typed_error_not_panic() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let t = Instant::now();
    let got = frontmatter_lint(&FrontmatterLintParams {
        root: dir.path().display().to_string(),
        template_path: "templates/does-not-exist.tmpl".to_string(),
    });
    millis("frontmatter_lint hostile", t);
    assert!(got.is_err(), "missing template must be a typed error");
}

// ---------------------------------------------------------------------------
// ggen_sync_dry_run — real pipeline, dry-run (writes nothing)
// ---------------------------------------------------------------------------

#[test]
fn sync_dry_run_live_plans_real_writes() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let t = Instant::now();
    let got = sync_dry_run(&SyncDryRunParams {
        root: dir.path().display().to_string(),
    })
    .expect("dry run must succeed on a real project");
    millis("sync_dry_run happy", t);

    assert!(got.ok);
    assert!(
        got.write_count >= 1,
        "the fixture project plans at least one output file"
    );
    assert!(
        !got.graph_hash.is_empty(),
        "graph_hash must identify the graph this plan was computed against"
    );
}

#[test]
fn sync_dry_run_hostile_missing_root_is_typed_error_not_panic() {
    let t = Instant::now();
    let got = sync_dry_run(&SyncDryRunParams {
        root: "/nonexistent/ggen-mcp-live-smoke/root".to_string(),
    });
    millis("sync_dry_run hostile", t);
    assert!(got.is_err(), "missing root must be a typed error");
}

// ---------------------------------------------------------------------------
// ggen_check_project
// ---------------------------------------------------------------------------

#[test]
fn check_project_live_over_fixture_project() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let t = Instant::now();
    let got = check_project(&CheckProjectParams {
        root: dir.path().display().to_string(),
        paths: None,
        with_routes: false,
    })
    .expect("check_project must succeed on a real project");
    millis("check_project happy", t);

    assert!(got.ok);
    assert!(
        got.files_checked >= 1,
        "the fixture project has real law surfaces to check"
    );
}

#[test]
fn check_project_hostile_missing_root_is_typed_error_not_panic() {
    let t = Instant::now();
    let got = check_project(&CheckProjectParams {
        root: "/nonexistent/ggen-mcp-live-smoke/root".to_string(),
        paths: None,
        with_routes: false,
    });
    millis("check_project hostile", t);
    assert!(got.is_err(), "missing root must be a typed error");
}

// ---------------------------------------------------------------------------
// ggen_rule_graph — declarative-rules fixture
// ---------------------------------------------------------------------------

#[test]
fn rule_graph_live_maps_the_two_fixture_rules() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_declarative_project(dir.path());
    let t = Instant::now();
    let got = rule_graph(&RuleGraphParams {
        root: dir.path().display().to_string(),
        rule_name: None,
        offset: None,
        limit: None,
    })
    .expect("rule_graph must succeed on a real declarative project");
    millis("rule_graph happy", t);

    assert!(got.ok);
    assert_eq!(got.total_rules, 2, "fixture declares exactly two rules");
    assert_eq!(got.returned, 2);
}

#[test]
fn rule_graph_hostile_missing_root_is_typed_error_not_panic() {
    let t = Instant::now();
    let got = rule_graph(&RuleGraphParams {
        root: "/nonexistent/ggen-mcp-live-smoke/root".to_string(),
        rule_name: None,
        offset: None,
        limit: None,
    });
    millis("rule_graph hostile", t);
    assert!(got.is_err(), "missing root must be a typed error");
}

// ---------------------------------------------------------------------------
// ggen_capability_status — declarative fixture uses inert template.pack
// ---------------------------------------------------------------------------

#[test]
fn capability_status_live_detects_the_inert_pack_source() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_declarative_project(dir.path());
    let t = Instant::now();
    let got = capability_status(&CapabilityStatusParams {
        root: dir.path().display().to_string(),
    })
    .expect("capability_status must succeed on a real project");
    millis("capability_status happy", t);

    assert!(got.ok);
    assert!(
        got.project_is_affected,
        "the fixture's second rule uses template.pack, which sync WILL refuse"
    );
}

#[test]
fn capability_status_hostile_missing_root_is_typed_error_not_panic() {
    let t = Instant::now();
    let got = capability_status(&CapabilityStatusParams {
        root: "/nonexistent/ggen-mcp-live-smoke/root".to_string(),
    });
    millis("capability_status hostile", t);
    assert!(got.is_err(), "missing root must be a typed error");
}

// ---------------------------------------------------------------------------
// ggen_pack_capabilities — real pack dir built in a TempDir
// ---------------------------------------------------------------------------

#[test]
fn pack_capabilities_live_introspects_a_real_pack_dir() {
    let dir = tempfile::tempdir().expect("tempdir");
    let pack = dir.path().join("demo-pack");
    std::fs::create_dir_all(pack.join("gates")).expect("mkdir gates");
    std::fs::write(
        pack.join("ontology.ttl"),
        r#"
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
ex:Widget a rdfs:Class .
ex:widgetA a ex:Widget .
"#,
    )
    .expect("write ontology.ttl");
    std::fs::write(
        pack.join("gates/admission.rq"),
        "# MESSAGE: every widget must have a name\nSELECT ?s WHERE { ?s a <http://example.org/Widget> }",
    )
    .expect("write gate");
    let t = Instant::now();
    let got = pack_capabilities(&PackCapabilitiesParams {
        pack_dir: pack.display().to_string(),
        contract_predicate_local_names: None,
    })
    .expect("pack_capabilities must succeed on a real pack dir");
    millis("pack_capabilities happy", t);

    assert!(got.ok);
    assert!(got.has_ontology);
    assert!(
        got.classes
            .iter()
            .any(|c| c.class_iri == "http://example.org/Widget"),
        "the fixture ontology declares ex:Widget, got {:?}",
        got.classes
    );
    assert!(got.has_gates, "the fixture ships one gates/*.rq file");
    assert!(
        got.gates
            .iter()
            .any(|g| g.message.as_deref() == Some("every widget must have a name")),
        "the gate's own # MESSAGE: header must be surfaced"
    );
}

#[test]
fn pack_capabilities_hostile_missing_dir_is_typed_error_not_panic() {
    let t = Instant::now();
    let got = pack_capabilities(&PackCapabilitiesParams {
        pack_dir: "/nonexistent/ggen-mcp-live-smoke/pack".to_string(),
        contract_predicate_local_names: None,
    });
    millis("pack_capabilities hostile", t);
    assert!(got.is_err(), "nonexistent pack dir must be a typed error");
}

/// A real directory with no `ontology.ttl` is a reported gap, not an error.
#[test]
fn pack_capabilities_live_empty_dir_reports_the_gap() {
    let dir = tempfile::tempdir().expect("tempdir");
    let t = Instant::now();
    let got = pack_capabilities(&PackCapabilitiesParams {
        pack_dir: dir.path().display().to_string(),
        contract_predicate_local_names: None,
    })
    .expect("a dir without ontology.ttl must be reported, not error");
    millis("pack_capabilities empty-dir", t);

    assert!(got.ok);
    assert!(!got.has_ontology);
    assert!(got.classes.is_empty());
}

// ---------------------------------------------------------------------------
// ggen_pack_query — real registry (the repo's own packs, when present)
// ---------------------------------------------------------------------------

#[test]
fn pack_query_live_select_over_the_local_registry() {
    let t = Instant::now();
    let got = pack_query(&PackQueryParams {
        sparql: "SELECT ?s WHERE { ?s ?p ?o } LIMIT 5".to_string(),
        pack_id: None,
    })
    .expect("pack_query must succeed over the real local registry");
    millis("pack_query happy", t);

    assert!(got.ok);
    assert_eq!(got.scope, "all-packs");
    assert!(!got.columns.is_empty(), "a SELECT must report its columns");
}

#[test]
fn pack_query_hostile_nonexistent_pack_is_typed_error_not_panic() {
    let t = Instant::now();
    let got = pack_query(&PackQueryParams {
        sparql: "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
        pack_id: Some("definitely-does-not-exist-live-smoke-pack".to_string()),
    });
    millis("pack_query hostile", t);
    assert!(got.is_err(), "nonexistent pack id must be a typed error");
}

// ---------------------------------------------------------------------------
// ggen_receipt_verify — degraded (missing-receipt) path; the clean path is
// covered by receipt_verify_test.rs with a real sync.
// ---------------------------------------------------------------------------

#[test]
fn receipt_verify_live_missing_receipt_reports_invalid_not_error() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let t = Instant::now();
    let got = receipt_verify(&ReceiptVerifyParams {
        root: dir.path().display().to_string(),
    })
    .expect("receipt_verify must never error on a missing receipt");
    millis("receipt_verify hostile", t);

    assert!(!got.valid, "no receipt on disk must report valid:false");
    assert!(
        got.error_message.is_some(),
        "the refusal message must be surfaced"
    );
}
