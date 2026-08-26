use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use tempfile::TempDir;

const DIGEST_PLACEHOLDER: &str = "__OCEL_SHA256__";

fn packs_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn read(project: &Path, path: &str) -> String {
    std::fs::read_to_string(project.join(path)).unwrap_or_else(|error| {
        panic!("read {path}: {error}");
    })
}

fn read_json(project: &Path, path: &str) -> serde_json::Value {
    serde_json::from_str(&read(project, path)).unwrap_or_else(|error| {
        panic!("parse {path}: {error}");
    })
}

fn sha256(path: &Path) -> String {
    let bytes = std::fs::read(path).unwrap_or_else(|error| {
        panic!("read {} for digest: {error}", path.display());
    });
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn scaffold_pack_with_ontology(pack: &Path, ontology: &str) -> (TempDir, PathBuf) {
    let guard = tempfile::tempdir().expect("tempdir");
    let project = guard.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("templates");
    std::fs::copy(pack.join("ggen.toml"), project.join("ggen.toml")).expect("copy ggen.toml");
    for entry in std::fs::read_dir(pack.join("templates")).expect("read templates") {
        let entry = entry.expect("template entry");
        std::fs::copy(entry.path(), project.join("templates").join(entry.file_name()))
            .expect("copy template");
    }
    std::fs::write(project.join("ontology.ttl"), ontology).expect("write ontology");
    (guard, project)
}

fn assert_idempotent(project: &Path) {
    let before_ocel = read(project, "generated/ggen-ecosystem-ocel.json");
    let before_request = read(project, "generated/project2-ggen-ecosystem-ocel-request.json");
    ggen_engine::sync::sync(
        project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("idempotent replay");
    assert_eq!(read(project, "generated/ggen-ecosystem-ocel.json"), before_ocel);
    assert_eq!(
        read(project, "generated/project2-ggen-ecosystem-ocel-request.json"),
        before_request
    );
}

const CONSUMER: &str = r#"@prefix geocel: <https://ggen.dev/ontology/ecosystem-ocel#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

geocel:cell4-run-20260826 a geocel:ManufacturingRun ;
  geocel:runId "cell4-ggen-ocel-20260826" ;
  geocel:cell "CELL4" ;
  geocel:standing "PARTIAL_ALIVE" .

geocel:ocel-pack a geocel:ManufacturingObject ;
  geocel:objectId "ggen-marketplace:ggen-ecosystem-ocel-pack" ;
  geocel:objectType geocel:GgenPack .

geocel:project2-ocel-key a geocel:ManufacturingObject ;
  geocel:objectId "project2:ggen/ecosystem/ocel/current" ;
  geocel:objectType geocel:ProjectMemory .

geocel:r84-qualification a geocel:ManufacturingObject ;
  geocel:objectId "qualification:r84" ;
  geocel:objectType geocel:Qualification .

geocel:event-select a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:select" ;
  geocel:eventType geocel:Select ;
  geocel:eventTime "2026-08-26T17:44:00Z"^^xsd:dateTime ;
  geocel:sequence 1 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:ocel-pack ;
  geocel:standing "ALIVE" .

geocel:event-construct a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:construct" ;
  geocel:eventType geocel:Construct ;
  geocel:eventTime "2026-08-26T17:44:30Z"^^xsd:dateTime ;
  geocel:sequence 2 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:ocel-pack ;
  geocel:standing "ALIVE" .

geocel:event-qualify a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:qualify:r84" ;
  geocel:eventType geocel:Qualify ;
  geocel:eventTime "2026-08-26T17:45:00Z"^^xsd:dateTime ;
  geocel:sequence 3 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:r84-qualification ;
  geocel:standing "ALIVE" .

geocel:event-merge a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:merge:r84" ;
  geocel:eventType geocel:Merge ;
  geocel:eventTime "2026-08-26T17:45:30Z"^^xsd:dateTime ;
  geocel:sequence 4 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:r84-qualification ;
  geocel:standing "ALIVE" .

geocel:event-refuse a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:refuse:broad-ci-debt" ;
  geocel:eventType geocel:Refuse ;
  geocel:eventTime "2026-08-26T17:45:45Z"^^xsd:dateTime ;
  geocel:sequence 5 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:r84-qualification ;
  geocel:standing "REFUSED_INHERITED_BROAD_CI_DEBT" .

geocel:event-receipt a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:receipt:project2-memory-missing" ;
  geocel:eventType geocel:Receipt ;
  geocel:eventTime "2026-08-26T17:46:00Z"^^xsd:dateTime ;
  geocel:sequence 6 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:project2-ocel-key ;
  geocel:standing "REFUSED_MEMORY_NOT_FOUND" .

geocel:event-generate-ocel a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:generate:ocel" ;
  geocel:eventType geocel:Generate ;
  geocel:eventTime "2026-08-26T17:46:30Z"^^xsd:dateTime ;
  geocel:sequence 7 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:ocel-pack ;
  geocel:standing "ALIVE" .
"#;

#[test]
fn cell4_manufactures_digest_bound_ocel_and_project2_request() {
    let (_guard, project) = scaffold_pack_with_ontology(
        &packs_dir().join("ggen-ecosystem-ocel-pack"),
        CONSUMER,
    );

    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first real sync");

    let ocel_path = project.join("generated/ggen-ecosystem-ocel.json");
    let first_ocel = read(&project, "generated/ggen-ecosystem-ocel.json");
    let digest = sha256(&ocel_path);

    let ontology = read(&project, "ontology.ttl");
    assert!(ontology.contains(DIGEST_PLACEHOLDER));
    std::fs::write(
        project.join("ontology.ttl"),
        ontology.replace(DIGEST_PLACEHOLDER, &digest),
    )
    .expect("bind exact OCEL digest into RDF");

    // The first-pass Project2 request is intentionally provisional: it was
    // manufactured before the exact OCEL digest was rebound into the source
    // graph. Preserve GGEN's fail-closed no-clobber law by retiring only that
    // provisional generated consequence before the second lawful manufacture.
    // The OCEL itself remains in place and must replay byte-identically.
    std::fs::remove_file(project.join("generated/project2-ggen-ecosystem-ocel-request.json"))
        .expect("retire provisional Project2 request before digest-bound replay");

    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second real sync with digest-bound RDF");

    let second_ocel = read(&project, "generated/ggen-ecosystem-ocel.json");
    assert_eq!(second_ocel, first_ocel, "digest binding must not mutate OCEL projection");
    assert_eq!(sha256(&ocel_path), digest, "OCEL digest must be replay-stable");

    let request = read_json(
        &project,
        "generated/project2-ggen-ecosystem-ocel-request.json",
    );
    assert_eq!(request["operation"], "memory.upsert");
    assert_eq!(request["payload"]["record"]["key"], "ggen/ecosystem/ocel/current");
    assert_eq!(request["payload"]["record"]["metadata"]["ocel_digest"], digest);
    assert_eq!(request["payload"]["record"]["metadata"]["process_analysis_owner"], "wasm4pm");

    assert_idempotent(&project);

    if let Some(out) = std::env::var_os("CELL4_OCEL_OUT_DIR") {
        let out = PathBuf::from(out);
        std::fs::create_dir_all(&out).expect("create CELL4 output dir");
        std::fs::copy(&ocel_path, out.join("ggen-ecosystem-ocel.json")).expect("copy OCEL");
        std::fs::copy(
            project.join("generated/project2-ggen-ecosystem-ocel-request.json"),
            out.join("project2-ggen-ecosystem-ocel-request.json"),
        )
        .expect("copy Project2 request");
        std::fs::write(out.join("ocel.sha256"), format!("{digest}\n")).expect("write digest");
    }
}
