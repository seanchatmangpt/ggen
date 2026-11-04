//! Projection-surface courts (PG-2 Slice 6, ds-4). `generated.rs` is `src/`,
//! so its integration courts live here: real mini-pack fixtures on real
//! tempdirs, state-based assertions (Chicago), real Oxigraph SPARQL, real
//! guarded writes. No mocks.

use std::fs;
use std::path::{Path, PathBuf};

use ggen_sjira::generated::{
    project, ProjectionOpts, ProjectionRefusal, WriteOutcome,
};
use ggen_sjira::work_order::WorkOrder;
use tempfile::TempDir;

// ── fixture: a real mini-pack ────────────────────────────────────────────────

const ONTOLOGY: &str = r#"
@prefix sj: <urn:sj:> .
@prefix cap: <urn:cap:> .

cap:c1 a sj:Capability ;
    sj:id "c1" ;
    sj:name "capability one" .
"#;

const SCOPE_QUERY: &str = r#"
PREFIX sj: <urn:sj:>
PREFIX cap: <urn:cap:>
SELECT ?id ?name WHERE {
    ?cap a sj:Capability ;
        sj:id "c1" ;
        sj:name ?name .
    BIND("c1" AS ?id)
}
"#;

const GATE_QUERY: &str = r#"
PREFIX sj: <urn:sj:>
PREFIX cap: <urn:cap:>
SELECT ?cap WHERE {
    ?cap a sj:Capability .
}
"#;

const UNBOUND_QUERY: &str = r#"
PREFIX sj: <urn:sj:>
PREFIX cap: <urn:cap:>
SELECT ?cap WHERE {
    ?cap a sj:Capability ;
        sj:missing_property ?x .
}
"#;

const TEMPLATE: &str = r#"// capability: {{ capability_id }}
// rows: {{ rows | length }}
{% for r in rows %}// row: {{ r.id }} {{ r.name }}
{% endfor %}"#;

fn order() -> WorkOrder {
    serde_json::from_value(serde_json::json!({
        "identity": "EXEC-1",
        "title": "t",
        "description": "d",
        "subject": "src/x.rs",
        "repository": "owner/repo",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "UNVERIFIED",
        "promotion_rule": "courts",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": "https://example.test/auth/EXEC-1",
        "projections": ["jira"],
        "required_courts": ["c1"],
        "required_evidence": ["e1"],
        "acceptance": ["a1"],
        "falsifiers": []
    }))
    .expect("valid WorkOrder")
}

fn opts(capability: &str, target: &str) -> ProjectionOpts {
    ProjectionOpts {
        capability_id: capability.to_string(),
        target_path: target.to_string(),
        falsifiers: None,
        unless_exists: false,
        template: Some(TEMPLATE.to_string()),
    }
}

fn mini_pack(dir: &Path) -> PathBuf {
    let pack = dir.join("pack");
    fs::create_dir_all(pack.join("projections")).expect("projections dir");
    fs::create_dir_all(pack.join("gates")).expect("gates dir");
    fs::write(pack.join("ontology.ttl"), ONTOLOGY).expect("ontology");
    fs::write(pack.join("projections/c1.rq"), SCOPE_QUERY).expect("scope query");
    fs::write(pack.join("gates/rows-exist.rq"), GATE_QUERY).expect("gate");
    pack
}

fn staging_dirs() -> Vec<PathBuf> {
    let mut out = Vec::new();
    let tmp = std::env::temp_dir();
    let entries = fs::read_dir(&tmp).expect("temp dir readable");
    for entry in entries.flatten() {
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.starts_with("ggen_sjira_projection_staging_") {
            out.push(entry.path());
        }
    }
    out
}

// ── happy path: render + guarded write + gate census ─────────────────────────

#[test]
fn projects_capability_and_reports_written_receipt() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");

    let outcome = project(
        &order(),
        &pack,
        &target_dir,
        opts("c1", "src/cap_one.rs"),
    )
    .expect("projection must succeed");

    assert_eq!(outcome.order_id, "EXEC-1");
    assert_eq!(outcome.capability_id, "c1");
    assert_eq!(outcome.write_outcome, WriteOutcome::Written);
    assert!(outcome.pack_digest.starts_with("sha256:"));
    assert!(outcome.render_digest.starts_with("sha256:"));
    assert!(!outcome.gate_results.is_empty());
    assert!(outcome
        .gate_results
        .iter()
        .all(|g| g.status == "pass"));

    let written =
        fs::read_to_string(target_dir.join("src/cap_one.rs")).expect("written file");
    assert!(written.contains("capability: c1"));
    assert!(written.contains("rows: 1"));
    // Oxigraph term Display serializes RDF literals WITH quotes — the
    // disclosed row-serialization law this surface pins.
    assert!(written.contains("row: \"c1\" \"capability one\""), "actual bytes:\n{written}");

    // Digests are over real bytes: the render digest matches the file's.
    let content = fs::read_to_string(target_dir.join("src/cap_one.rs")).expect("content");
    assert_eq!(
        outcome.render_digest,
        ggen_sjira::work_order::digest_of(&serde_json::Value::String(content))
    );

    // Staging dir removed after the run — differential court: snapshot the
    // pre-run staging set around a fresh project call, assert no NEW staging
    // dir survives (absolute emptiness would flag pollution from earlier
    // aborted runs).
    let before = staging_dirs();
    let _ = project(&order(), &pack, &target_dir, opts("c1", "src/cap_one.rs"))
        .expect("second projection");
    let after = staging_dirs();
    let survivors: Vec<_> = after.iter().filter(|p| !before.contains(p)).collect();
    assert!(
        survivors.is_empty(),
        "staging dirs leaked: {survivors:?}"
    );
}

#[test]
fn rerun_is_unchanged_and_pack_digest_is_stable() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");

    let first = project(&order(), &pack, &target_dir, opts("c1", "out.rs"))
        .expect("first projection");
    assert_eq!(first.write_outcome, WriteOutcome::Written);

    let before = fs::read_to_string(target_dir.join("out.rs")).expect("file");
    let second = project(&order(), &pack, &target_dir, opts("c1", "out.rs"))
        .expect("second projection");
    assert_eq!(second.write_outcome, WriteOutcome::Unchanged);
    assert_eq!(
        fs::read_to_string(target_dir.join("out.rs")).expect("file"),
        before,
        "unchanged re-run must not rewrite the file"
    );
    assert_eq!(first.pack_digest, second.pack_digest);
}

#[test]
fn differing_target_content_refuses_silent_clobber() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");
    fs::write(target_dir.join("out.rs"), "hand-written content\n").expect("seed");

    let err = project(&order(), &pack, &target_dir, opts("c1", "out.rs"))
        .expect_err("differing content must refuse");
    assert!(
        matches!(err, ProjectionRefusal::GuardedWriteDiffers(_)),
        "{err}"
    );
    assert_eq!(
        fs::read_to_string(target_dir.join("out.rs")).expect("file"),
        "hand-written content\n",
        "refusal must not touch the existing file"
    );
}

#[test]
fn unless_exists_skips_existing_regardless_of_content() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");
    fs::write(target_dir.join("out.rs"), "keep\n").expect("seed");

    let mut o = opts("c1", "out.rs");
    o.unless_exists = true;
    let outcome = project(&order(), &pack, &target_dir, o).expect("projection");
    assert_eq!(outcome.write_outcome, WriteOutcome::SkippedExists);
    assert_eq!(
        fs::read_to_string(target_dir.join("out.rs")).expect("file"),
        "keep\n"
    );
}

// ── typed refusals ───────────────────────────────────────────────────────────

#[test]
fn unknown_capability_refuses() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");

    let err = project(&order(), &pack, &target_dir, opts("nope", "out.rs"))
        .expect_err("unknown capability must refuse");
    assert!(
        matches!(err, ProjectionRefusal::CapabilityUnknown(ref id) if id == "nope"),
        "{err}"
    );
    assert!(
        !target_dir.join("out.rs").exists(),
        "no byte may be written on refusal"
    );
}

#[test]
fn traversal_target_path_refuses_before_write() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");
    let escaped = dir.path().join("escaped.rs");

    let err = project(
        &order(),
        &pack,
        &target_dir,
        opts("c1", "../escaped.rs"),
    )
    .expect_err("traversal must refuse");
    assert!(matches!(err, ProjectionRefusal::BadOutputPath(_)), "{err}");
    assert!(!escaped.exists(), "the write must never land outside root");
}

#[test]
fn falsifiers_without_unbound_query_refuse() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");

    let mut o = opts("c1", "out.rs");
    o.falsifiers = Some(vec!["urn:semantic-jira:falsifier:EXEC-1:f1".to_string()]);
    let err = project(&order(), &pack, &target_dir, o)
        .expect_err("falsifiers with no falsified-state query must refuse");
    assert!(
        matches!(err, ProjectionRefusal::FalsifierGatesMissing(ref id) if id == "c1"),
        "{err}"
    );
}

#[test]
fn projected_falsifier_gates_ride_the_census() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    fs::write(pack.join("projections/c1.unbound.rq"), UNBOUND_QUERY).expect("unbound");
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");

    let mut o = opts("c1", "out.rs");
    o.falsifiers = Some(vec![
        "urn:semantic-jira:falsifier:EXEC-1:f1".to_string(),
        "urn:semantic-jira:falsifier:EXEC-1:f2".to_string(),
    ]);
    let outcome = project(&order(), &pack, &target_dir, o).expect("projection");
    let falsifier_gates: Vec<_> = outcome
        .gate_results
        .iter()
        .filter(|g| g.id.contains("falsifier"))
        .collect();
    assert_eq!(falsifier_gates.len(), 2);
    assert!(falsifier_gates.iter().all(|g| g.status == "pass"));
    // The ORIGINAL pack must not have grown a verify/ dir (staging law).
    assert!(
        !pack.join("verify").exists(),
        "projected gates must live in the staging copy, never the pinned pack"
    );
}

#[test]
fn failing_gate_census_refuses_verification_failed() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    // A gate that binds zero rows over this ontology -> census must fail.
    fs::write(
        pack.join("gates/impossible.rq"),
        r#"PREFIX sj: <urn:sj:>
SELECT ?x WHERE { ?x a sj:Nothing . }"#,
    )
    .expect("empty gate");
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");

    let err = project(&order(), &pack, &target_dir, opts("c1", "out.rs"))
        .expect_err("zero-row gate must fail the census");
    assert!(matches!(err, ProjectionRefusal::VerificationFailed(_)), "{err}");
    // Fail-closed honesty (projection.ex step 4): the actuated file holds on
    // disk but standing never advances -- here we pin the disk side.
    assert!(target_dir.join("out.rs").exists());
    let failed = format!("{err}");
    assert!(failed.contains("gates/impossible.rq"), "{failed}");
}

#[test]
fn pack_digest_changes_when_pack_bytes_change() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");

    let first = project(&order(), &pack, &target_dir, opts("c1", "out.rs"))
        .expect("first");
    fs::write(pack.join("ontology.ttl"), format!("{ONTOLOGY}\n# touched\n")).expect("mutate");
    let second = project(&order(), &pack, &target_dir, opts("c1", "out.rs"))
        .expect("second");
    assert_ne!(
        first.pack_digest, second.pack_digest,
        "pack byte change must move the pack digest"
    );
    // And a changed pack re-writes even though the render is identical? The
    // render is derived from ontology rows -- here unchanged -- so the write
    // is Unchanged; the DIGEST is the pinning surface, not the write.
    assert_eq!(second.write_outcome, WriteOutcome::Unchanged);
}

#[test]
fn original_pack_is_untouched_by_a_run() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    fs::write(pack.join("projections/c1.unbound.rq"), UNBOUND_QUERY).expect("unbound");
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");

    let mut o = opts("c1", "out.rs");
    o.falsifiers = Some(vec!["urn:semantic-jira:falsifier:EXEC-1:f1".to_string()]);
    let outcome = project(&order(), &pack, &target_dir, o).expect("projection");
    assert!(outcome.gate_results.iter().all(|g| g.status == "pass"));

    // File-state assert: the original pack tree still holds exactly the
    // seeded files -- the projection mutated only the staging copy.
    assert!(pack.join("ontology.ttl").exists());
    assert!(pack.join("projections/c1.rq").exists());
    assert!(pack.join("projections/c1.unbound.rq").exists());
    assert!(pack.join("gates/rows-exist.rq").exists());
    assert!(!pack.join("verify").exists());
    assert_eq!(
        fs::read_to_string(pack.join("projections/c1.unbound.rq")).expect("unbound"),
        UNBOUND_QUERY,
        "projected gate must be written into the staging copy only"
    );
}

#[test]
fn render_without_template_is_deterministic_json_rows() {
    let dir = TempDir::new().expect("tempdir");
    let pack = mini_pack(dir.path());
    let target_dir = dir.path().join("out");
    fs::create_dir_all(&target_dir).expect("target dir");

    let mut o = opts("c1", "out.rs");
    o.template = None;
    let outcome = project(&order(), &pack, &target_dir, o).expect("projection");
    let content = fs::read_to_string(target_dir.join("out.rs")).expect("file");
    let parsed: serde_json::Value =
        serde_json::from_str(&content).expect("rows are JSON");
    assert!(parsed.is_array());
    assert_eq!(outcome.render_digest, ggen_sjira::work_order::digest_of(&serde_json::Value::String(content)));
}
