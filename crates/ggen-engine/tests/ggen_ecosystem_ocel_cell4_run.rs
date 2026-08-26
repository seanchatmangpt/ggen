//! Run-specific CELL4 execution receipt for the merged GGEN ecosystem OCEL pack.
//!
//! Real filesystem + real ggen sync. Generated JSON is never edited: the first
//! pass emits OCEL bytes, those bytes are SHA-256 bound back into RDF, and a
//! second real sync emits the canonical Project2 request with that exact digest.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};
use std::process::Command;
use support::{assert_idempotent, read, read_json, scaffold_pack_with_ontology};

const DIGEST_PLACEHOLDER: &str = "sha256:PENDING_CELL4_OCEL_DIGEST";

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn sha256(path: &Path) -> String {
    let output = Command::new("sha256sum")
        .arg(path)
        .output()
        .expect("sha256sum must execute");
    assert!(output.status.success(), "sha256sum failed: {output:?}");
    let stdout = String::from_utf8(output.stdout).expect("sha256sum utf8");
    let digest = stdout.split_whitespace().next().expect("sha256 token");
    assert_eq!(digest.len(), 64, "sha256 must be 64 hex chars");
    format!("sha256:{digest}")
}

const CONSUMER: &str = r#"
@prefix geocel: <https://ggen.dev/ontology/ggen-ecosystem-ocel#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

geocel:cell4-run-20260826 a geocel:ManufacturingRun ;
  geocel:runId "GGEN-OCEL-CELL4-20260826T174500Z" ;
  geocel:cell "CELL4" ;
  geocel:projectOwner "seanchatmangpt" ;
  geocel:projectNumber 2 ;
  geocel:projectMemoryKey "ggen/ecosystem/ocel/current" ;
  geocel:ocelDigest "sha256:PENDING_CELL4_OCEL_DIGEST" ;
  geocel:standing "PARTIAL_ALIVE" .

geocel:repo-ash-r2rml a geocel:ManufacturingObject, geocel:RepositoryObject ;
  geocel:objectId "repo:seanchatmangpt/ash_r2rml@e8de8bb28e17ac79aeb1e4982ff0e8a7067a433c" ;
  geocel:repository "seanchatmangpt/ash_r2rml" ;
  geocel:exactHead "e8de8bb28e17ac79aeb1e4982ff0e8a7067a433c" .

geocel:r84-pack a geocel:ManufacturingObject, geocel:GgenPrimitiveObject ;
  geocel:objectId "pack:ash-reactor-domain-error-contract-pack@6b7fb4af7b4ef3a6330ad61ca833b98902300332" ;
  geocel:primitive "ash-reactor-domain-error-contract-pack" ;
  geocel:exactHead "6b7fb4af7b4ef3a6330ad61ca833b98902300332" .

geocel:ocel-pack a geocel:ManufacturingObject, geocel:GgenPrimitiveObject ;
  geocel:objectId "pack:ggen-ecosystem-ocel-pack@4ad032c1488e4cd52c01b27121e38f835b2d61be" ;
  geocel:primitive "ggen-ecosystem-ocel-pack" ;
  geocel:exactHead "4ad032c1488e4cd52c01b27121e38f835b2d61be" .

geocel:r84-qualification a geocel:ManufacturingObject, geocel:QualificationObject ;
  geocel:objectId "qualification:ash_r2rml:r84:e8de8bb28e17ac79aeb1e4982ff0e8a7067a433c" ;
  geocel:qualification "exact-head R84 court + REUSE requalification" .

geocel:project2-ocel-key a geocel:ManufacturingObject, geocel:ProjectMemoryObject ;
  geocel:objectId "project2:ggen/ecosystem/ocel/current" .

geocel:event-select a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:select:ash_r2rml" ;
  geocel:eventType geocel:Select ;
  geocel:eventTime "2026-08-26T17:38:29Z"^^xsd:dateTime ;
  geocel:sequence 1 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:repo-ash-r2rml ;
  geocel:standing "ALIVE" .

geocel:event-generate a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:generate:r84" ;
  geocel:eventType geocel:Generate ;
  geocel:eventTime "2026-08-26T17:44:00Z"^^xsd:dateTime ;
  geocel:sequence 2 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:r84-pack ;
  geocel:standing "ALIVE" .

geocel:event-realize a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:realize:ash_r2rml" ;
  geocel:eventType geocel:Realize ;
  geocel:eventTime "2026-08-26T17:45:08Z"^^xsd:dateTime ;
  geocel:sequence 3 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:repo-ash-r2rml ;
  geocel:standing "PARTIAL_ALIVE" .

geocel:event-qualify a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:qualify:r84" ;
  geocel:eventType geocel:Qualify ;
  geocel:eventTime "2026-08-26T17:45:13Z"^^xsd:dateTime ;
  geocel:sequence 4 ;
  geocel:inRun geocel:cell4-run-20260826 ;
  geocel:relatesTo geocel:r84-qualification ;
  geocel:standing "PARTIAL_ALIVE" .

geocel:event-refuse a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:refuse:inherited-broad-ci" ;
  geocel:eventType geocel:Refuse ;
  geocel:eventTime "2026-08-26T17:45:20Z"^^xsd:dateTime ;
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
