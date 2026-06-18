//! Honesty test for `ggen reverse templates` (ARM 3: discovered code → Tera candidates).
//!
//! Chicago TDD: scan real code → real discovered TTL → infer candidates →
//! verify the full code→template→code round-trip. Proves:
//! - the reverse GGEN-TPL-001 law: every emitted template binding is produced
//!   by the sibling SELECT (no orphan variables);
//! - three-way agreement: template terminals == query projection == bindings.json;
//! - the candidate actually renders against the real graph (enforced inside
//!   `infer_candidates`, which is fail-closed);
//! - teeth: a deliberately broken candidate is rejected by `check_bindings`;
//! - fail-loud: a missing discovered graph errors.

use std::path::PathBuf;

use ggen_core::reverse::{
    check_bindings, infer_candidates, scan_to_authority, ReverseReceipt,
};

fn temp_project() -> PathBuf {
    let root =
        std::env::temp_dir().join(format!("ggen_reverse_tmpl_{}", uuid::Uuid::new_v4()));
    std::fs::create_dir_all(root.join("src")).expect("create src dir");
    root
}

#[test]
fn templates_round_trip_from_scanned_code() {
    // Arrange: scan real code into a discovered authority graph.
    let proj = temp_project();
    std::fs::write(
        proj.join("src").join("lib.rs"),
        "pub struct Account { id: u64, balance: u64 }\n",
    )
    .expect("write source");
    let scan = scan_to_authority(&[proj.join("src")], &proj, "demo").expect("scan");

    // Act: infer template candidates from the discovered graph.
    let out_dir = proj.join("templates").join("candidates");
    let report =
        infer_candidates(&proj, &scan.authority_ttl, &out_dir).expect("infer candidates");

    // One candidate for the one discovered service.
    assert_eq!(report.candidates.len(), 1, "one candidate per service");
    let cand = &report.candidates[0];
    assert_eq!(cand.service, "Account");
    assert!(cand.template_path.exists());
    assert!(cand.query_path.exists());
    assert!(cand.bindings_path.exists());

    let template = std::fs::read_to_string(&cand.template_path).expect("read tmpl");
    let query = std::fs::read_to_string(&cand.query_path).expect("read query");
    assert!(template.contains("{{ name }}"), "parameterized by name");
    assert!(
        template.contains("{% for field in fields %}"),
        "iterates fields"
    );
    assert!(query.contains("SELECT ?name ?field_name ?field_type"));
    assert!(query.contains("disco:serviceName"));
    assert!(query.contains("FILTER(?name = \"Account\")"));
    // Strict-mode hygiene: explicit projection + ORDER BY (no SELECT *).
    assert!(!query.contains("SELECT *"), "must not use SELECT *");
    assert!(query.contains("ORDER BY"), "must be deterministically ordered");

    // The reverse GGEN-TPL-001 law holds for the EMITTED artifacts.
    check_bindings(&template, &query).expect("emitted template must have no orphan bindings");

    // Three-way agreement: bindings.json declares exactly name + fields.
    let raw = std::fs::read_to_string(&cand.bindings_path).expect("read bindings");
    let contract: serde_json::Value = serde_json::from_str(&raw).expect("parse bindings json");
    let bindings = contract["bindings"]
        .as_array()
        .expect("bindings array")
        .iter()
        .map(|v| v.as_str().unwrap_or_default().to_string())
        .collect::<Vec<_>>();
    assert_eq!(bindings, vec!["name".to_string(), "fields".to_string()]);

    // Honest receipt binds the discovered graph + lists the emitted artifacts.
    let receipt_raw = std::fs::read_to_string(&report.receipt_path).expect("read receipt");
    let receipt: ReverseReceipt = serde_json::from_str(&receipt_raw).expect("parse receipt");
    assert_eq!(receipt.operation, "reverse-templates");
    assert!(
        receipt.input_hashes.keys().any(|k| k.starts_with("discovered:")),
        "receipt binds the discovered graph: {:?}",
        receipt.input_hashes
    );
    assert!(
        receipt.output_hashes.contains_key("account.rs.tmpl"),
        "receipt lists the emitted template"
    );

    let _ = std::fs::remove_dir_all(&proj);
}

#[test]
fn orphan_binding_is_rejected() {
    // A template variable the query does not project must be rejected — proving
    // the round-trip checker has teeth (impossible-state injection).
    let broken_template = "pub struct {{ name }} { ghost: {{ ghost }} }";
    let query = "SELECT ?name WHERE { ?s ?p ?o } ORDER BY ?name";
    let result = check_bindings(broken_template, query);
    assert!(
        result.is_err(),
        "a template var (`ghost`) absent from the SELECT must be rejected"
    );
}

#[test]
fn missing_discovered_graph_fails_loudly() {
    let proj = temp_project();
    let result = infer_candidates(
        &proj,
        &proj.join("does-not-exist.ttl"),
        &proj.join("out"),
    );
    assert!(result.is_err(), "missing discovered graph must error");
    let _ = std::fs::remove_dir_all(&proj);
}

#[test]
fn field_less_service_is_coherent() {
    // A struct with no fields must still round-trip (coherent empty template),
    // never an orphan `{{ fields }}` with no query column.
    let proj = temp_project();
    std::fs::write(proj.join("src").join("lib.rs"), "pub struct Marker {}\n").expect("write");
    let scan = scan_to_authority(&[proj.join("src")], &proj, "demo").expect("scan");
    let report = infer_candidates(&proj, &scan.authority_ttl, &proj.join("out"))
        .expect("field-less service must still produce a coherent candidate");
    assert_eq!(report.candidates.len(), 1);
    assert_eq!(report.candidates[0].service, "Marker");
    let _ = std::fs::remove_dir_all(&proj);
}
