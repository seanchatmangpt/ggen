use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

mod support;
use support::{assert_idempotent, read, read_json, scaffold_pack_with_ontology};

const DIGEST_PLACEHOLDER: &str = "__OCEL_SHA256__";

fn packs_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn sha256(path: &Path) -> String {
    let bytes = std::fs::read(path).unwrap_or_else(|error| {
        panic!("read {} for digest: {error}", path.display());
    });
    format!("sha256:{:x}", Sha256::digest(bytes))
}

const CONSUMER: &str = r#"@prefix geocel: <https://ggen.dev/ontology/ggen-ecosystem-ocel#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

geocel:cell4-run-20260826T193631Z a geocel:ManufacturingRun ;
  geocel:runId "cell4-ggen-ocel-20260826T193631Z" ;
  geocel:cell "CELL4" ;
  geocel:projectOwner "seanchatmangpt" ;
  geocel:projectNumber 2 ;
  geocel:projectMemoryKey "ggen/ecosystem/ocel/current" ;
  geocel:ocelDigest "__OCEL_SHA256__" ;
  geocel:standing "PARTIAL_ALIVE" .

geocel:consumer-ash-r2rml a geocel:ManufacturingObject, geocel:RepositoryObject ;
  geocel:objectId "repo:seanchatmangpt/ash_r2rml@e8de8bb28e17ac79aeb1e4982ff0e8a7067a433c" ;
  geocel:repository "seanchatmangpt/ash_r2rml" ;
  geocel:exactHead "e8de8bb28e17ac79aeb1e4982ff0e8a7067a433c" .

geocel:constructor-ggen a geocel:ManufacturingObject, geocel:RepositoryObject ;
  geocel:objectId "repo:seanchatmangpt/ggen" ;
  geocel:repository "seanchatmangpt/ggen" .

geocel:ocel-pack a geocel:ManufacturingObject, geocel:GgenPrimitiveObject ;
  geocel:objectId "pack:ggen-ecosystem-ocel-pack" ;
  geocel:primitive "ggen-ecosystem-ocel-pack" .

geocel:project2-ocel-key a geocel:ManufacturingObject, geocel:ProjectMemoryObject ;
  geocel:objectId "project2:ggen/ecosystem/ocel/current" .

geocel:r84-qualification a geocel:ManufacturingObject, geocel:QualificationObject ;
  geocel:objectId "qualification:ash_r2rml:R84:32995950718" ;
  geocel:qualification "R84 Ash Reactor Domain Error Fanout" .

geocel:event-observe-missing-ocel a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:observe:canonical-ocel" ;
  geocel:eventType geocel:Observe ;
  geocel:eventTime "2026-08-26T19:38:19Z"^^xsd:dateTime ;
  geocel:sequence 1 ;
  geocel:inRun geocel:cell4-run-20260826T193631Z ;
  geocel:relatesTo geocel:project2-ocel-key ;
  geocel:standing "REFUSED_MEMORY_NOT_FOUND" .

geocel:event-receipt-missing-ocel a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:receipt:canonical-ocel-missing" ;
  geocel:eventType geocel:Receipt ;
  geocel:eventTime "2026-08-26T19:38:19Z"^^xsd:dateTime ;
  geocel:sequence 2 ;
  geocel:inRun geocel:cell4-run-20260826T193631Z ;
  geocel:relatesTo geocel:project2-ocel-key ;
  geocel:standing "REFUSED_MEMORY_NOT_FOUND" .

geocel:event-select a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:select:ash-r2rml" ;
  geocel:eventType geocel:Select ;
  geocel:eventTime "2026-08-26T19:38:31Z"^^xsd:dateTime ;
  geocel:sequence 3 ;
  geocel:inRun geocel:cell4-run-20260826T193631Z ;
  geocel:relatesTo geocel:consumer-ash-r2rml ;
  geocel:standing "ALIVE" .

geocel:event-realize a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:realize:ash-r2rml" ;
  geocel:eventType geocel:Realize ;
  geocel:eventTime "2026-08-26T19:38:31Z"^^xsd:dateTime ;
  geocel:sequence 4 ;
  geocel:inRun geocel:cell4-run-20260826T193631Z ;
  geocel:relatesTo geocel:consumer-ash-r2rml ;
  geocel:standing "ALIVE" .

geocel:event-qualify a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:qualify:r84" ;
  geocel:eventType geocel:Qualify ;
  geocel:eventTime "2026-08-26T19:38:31Z"^^xsd:dateTime ;
  geocel:sequence 5 ;
  geocel:inRun geocel:cell4-run-20260826T193631Z ;
  geocel:relatesTo geocel:r84-qualification ;
  geocel:standing "ALIVE" .

geocel:event-refuse a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:refuse:inherited-broad-ci" ;
  geocel:eventType geocel:Refuse ;
  geocel:eventTime "2026-08-26T19:38:31Z"^^xsd:dateTime ;
  geocel:sequence 6 ;
  geocel:inRun geocel:cell4-run-20260826T193631Z ;
  geocel:relatesTo geocel:r84-qualification ;
  geocel:standing "REFUSED_INHERITED_BROAD_CI_DEBT" .

geocel:event-generate-ocel a geocel:ManufacturingEvent ;
  geocel:eventId "event:cell4:generate:ocel" ;
  geocel:eventType geocel:Generate ;
  geocel:eventTime "2026-08-26T19:46:00Z"^^xsd:dateTime ;
  geocel:sequence 7 ;
  geocel:inRun geocel:cell4-run-20260826T193631Z ;
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

    let ocel = read_json(&project, "generated/ggen-ecosystem-ocel.json");
    assert_eq!(ocel["objects"].as_array().expect("objects").len(), 5);
    assert_eq!(ocel["events"].as_array().expect("events").len(), 7);
    assert!(!second_ocel.contains("\"type\":\"merge\""));

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
