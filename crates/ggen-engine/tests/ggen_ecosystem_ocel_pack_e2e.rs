//! Chicago-TDD e2e proof for `packs/ggen-ecosystem-ocel-pack`.
//! Real filesystem, real ggen sync, real JSON parsing, deterministic replay, no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;
use std::path::{Path, PathBuf};
use support::{
    assert_gate_refuses, assert_idempotent, read, read_json, scaffold_pack_with_ontology,
};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

const CONSUMER: &str = r#"
@prefix geocel: <https://ggen.dev/ontology/ggen-ecosystem-ocel#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

geocel:run-20260826 a geocel:ManufacturingRun ;
  geocel:runId "schedule-20260826T170200Z" ;
  geocel:cell "CELL1" ;
  geocel:projectOwner "seanchatmangpt" ;
  geocel:projectNumber 2 ;
  geocel:projectMemoryKey "ggen/ecosystem/ocel/current" ;
  geocel:ocelDigest "sha256:abc123" ;
  geocel:standing "ALIVE" .

geocel:repo-ggen a geocel:ManufacturingObject, geocel:RepositoryObject ;
  geocel:objectId "repo:seanchatmangpt/ggen" ;
  geocel:repository "seanchatmangpt/ggen" ;
  geocel:exactHead "ddfa602bfbab57b7ed5150f61b0acac7a41e3020" .

geocel:primitive-pack a geocel:ManufacturingObject, geocel:GgenPrimitiveObject ;
  geocel:objectId "pack:ggen-ecosystem-ocel-pack" ;
  geocel:primitive "ggen-ecosystem-ocel-pack" .

geocel:event-observe a geocel:ManufacturingEvent ;
  geocel:eventId "event:observe:ggen" ;
  geocel:eventType geocel:Observe ;
  geocel:eventTime "2026-08-26T17:02:00Z"^^xsd:dateTime ;
  geocel:sequence 1 ;
  geocel:inRun geocel:run-20260826 ;
  geocel:relatesTo geocel:repo-ggen ;
  geocel:standing "ALIVE" .

geocel:event-generate a geocel:ManufacturingEvent ;
  geocel:eventId "event:generate:pack" ;
  geocel:eventType geocel:Generate ;
  geocel:eventTime "2026-08-26T17:03:00Z"^^xsd:dateTime ;
  geocel:sequence 2 ;
  geocel:inRun geocel:run-20260826 ;
  geocel:relatesTo geocel:primitive-pack ;
  geocel:standing "ALIVE" .
"#;

#[test]
fn ggen_ecosystem_ocel_pack_generates_real_ocel_and_project2_request() {
    let (_dir, project) =
        scaffold_pack_with_ontology(&packs_dir().join("ggen-ecosystem-ocel-pack"), CONSUMER);

    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("real sync");

    let ocel = read_json(&project, "generated/ggen-ecosystem-ocel.json");
    assert_eq!(ocel["objects"].as_array().expect("objects").len(), 2);
    assert_eq!(ocel["events"].as_array().expect("events").len(), 2);
    assert_eq!(ocel["events"][0]["type"], "observe");
    assert_eq!(
        ocel["events"][0]["relationships"][0]["objectId"],
        "repo:seanchatmangpt/ggen"
    );
    assert_eq!(ocel["events"][1]["type"], "generate");

    let request = read_json(
        &project,
        "generated/project2-ggen-ecosystem-ocel-request.json",
    );
    assert_eq!(request["operation"], "memory.upsert");
    assert_eq!(request["project"]["owner"], "seanchatmangpt");
    assert_eq!(request["project"]["number"], 2);
    assert_eq!(
        request["payload"]["record"]["key"],
        "ggen/ecosystem/ocel/current"
    );
    assert_eq!(
        request["payload"]["record"]["metadata"]["process_analysis_owner"],
        "wasm4pm"
    );

    assert_idempotent(&project);
}

#[test]
fn ggen_ecosystem_ocel_pack_regenerates_owned_project2_request_when_digest_changes() {
    let (_dir, project) =
        scaffold_pack_with_ontology(&packs_dir().join("ggen-ecosystem-ocel-pack"), CONSUMER);

    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    let ontology = read(&project, "ontology.ttl");
    assert!(ontology.contains("sha256:abc123"));
    std::fs::write(
        project.join("ontology.ttl"),
        ontology.replace("sha256:abc123", "sha256:def456"),
    )
    .expect("change admitted digest input");

    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("owned generated Project2 request must regenerate");

    let request = read_json(
        &project,
        "generated/project2-ggen-ecosystem-ocel-request.json",
    );
    assert_eq!(
        request["payload"]["record"]["metadata"]["ocel_digest"],
        "sha256:def456"
    );
}

#[test]
fn ggen_ecosystem_ocel_pack_refuses_parallel_project2_truth() {
    let (_dir, project) =
        scaffold_pack_with_ontology(&packs_dir().join("ggen-ecosystem-ocel-pack"), CONSUMER);
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    assert_gate_refuses(
        &project,
        r#"@prefix geocel: <https://ggen.dev/ontology/ggen-ecosystem-ocel#> .
        geocel:bad-run a geocel:ManufacturingRun ;
          geocel:runId "bad-run" ;
          geocel:cell "CELL1" ;
          geocel:projectOwner "seanchatmangpt" ;
          geocel:projectNumber 2 ;
          geocel:projectMemoryKey "ocel/v2/revops/current" ;
          geocel:ocelDigest "sha256:deadbeef" ;
          geocel:standing "UNKNOWN" .
        "#,
        "030_canonical_project2_key",
    );
}
