//! Executable regression court for canonical OCEL negative witnesses.
//! Each test runs the real ggen sync path and proves one named gate refuses its canonical falsifier.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};
use support::{assert_gate_refuses, scaffold_pack_with_ontology};

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

fn assert_canonical_negative_witness_refuses(gate: &str) {
    let pack = packs_dir().join("ggen-ecosystem-ocel-pack");
    let witness = std::fs::read_to_string(
        pack.join("witnesses/fail").join(format!("{gate}.ttl")),
    )
    .expect("canonical negative witness");
    let (_dir, project) = scaffold_pack_with_ontology(&pack, CONSUMER);
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");
    assert_gate_refuses(&project, &witness, gate);
}

#[test]
fn gate_010_required_event_contract_rejects_canonical_negative_witness() {
    assert_canonical_negative_witness_refuses("010_required_event_contract");
}
