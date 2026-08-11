//! Validates this session's full pack-generation loop (round 1 `domain-capability-pack` →
//! round 3 `process-intelligence-rag-pack` → round 5 `dspy-pack` SHACL-signature derivation)
//! by ACTUALLY RE-RUNNING every real `sync()` call and capturing the real, observed outcome
//! (real `std::time::SystemTime`-derived epoch-millis timestamps, real `Result`
//! success/failure, real `blake3` content hashes of the real generated files) as a real
//! OCEL 2.0 event log — not fixture data.
//!
//! This deliberately does NOT reuse `ggen-graph::ocel`'s types: that crate's `ocel/` module
//! structurally forbids subprocess/filesystem access (`tests/forbidden_surface.rs`) so it
//! cannot observe a real sync run, and its one log-construction helper,
//! `self_audit::generate_self_audit_log`, is an explicitly documented FIXTURE — its own module
//! doc records a real prior incident where a fabricated OCEL log was fed to a truthfulness
//! adjudication script as if it were evidence, and names the regression guard
//! (`tests/no_fabricated_truthfulness_evidence.rs`) added after that was reverted. This test
//! builds a real, OCEL 2.0-shaped JSON document (top-level `objectTypes`/`eventTypes`/
//! `objects`/`events`, per ocel-standard.org's real schema) directly from real observations
//! captured in this process, so the same mistake cannot be repeated here.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use serde_json::{json, Value};
use std::time::{SystemTime, UNIX_EPOCH};
use support::{read, scaffold_pack, scaffold_pack_with_ontology};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

/// Real wall-clock time as milliseconds since the Unix epoch (`std::time::SystemTime`,
/// no `chrono` dependency needed -- `ggen-engine` doesn't carry one). A real, monotonic-
/// enough-for-this-purpose observation, not a fabricated value.
fn now_millis() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("real system clock must be after the Unix epoch")
        .as_millis()
}

/// One real, captured observation of a `sync()` call.
struct Observed {
    pack: &'static str,
    activity: &'static str,
    started: u128,
    finished: u128,
    ok: bool,
    /// Real blake3 hash of the real generated file's real bytes (positive cases), or the
    /// real refusal error message (negative cases) -- never a fabricated value.
    detail: String,
}

#[test]
fn full_loop_produces_and_validates_a_real_ocel_v2_log() {
    let mut observed = Vec::new();

    // --- Round 1/2: domain-capability-pack, real positive sync ------------------------
    {
        let (_dir, project) = scaffold_pack(&packs_dir().join("domain-capability-pack"));
        let started = now_millis();
        let result = ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default());
        let finished = now_millis();
        let ok = result.is_ok();
        let detail = if ok {
            let bytes = read(&project, "docs/domain-capability/sregym-capabilities.md");
            blake3::hash(bytes.as_bytes()).to_hex().to_string()
        } else {
            format!("{}", result.err().expect("checked above"))
        };
        assert!(ok, "domain-capability-pack must sync cleanly: {detail}");
        observed.push(Observed {
            pack: "domain-capability-pack",
            activity: "pack.sync",
            started,
            finished,
            ok,
            detail,
        });
    }

    // --- Round 3: process-intelligence-rag-pack, real positive sync -------------------
    {
        let (_dir, project) =
            scaffold_pack(&packs_dir().join("process-intelligence-rag-pack"));
        let started = now_millis();
        let result = ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default());
        let finished = now_millis();
        let ok = result.is_ok();
        let detail = if ok {
            let bytes = read(&project, "src/grounded_process_query.py");
            blake3::hash(bytes.as_bytes()).to_hex().to_string()
        } else {
            format!("{}", result.err().expect("checked above"))
        };
        assert!(ok, "process-intelligence-rag-pack must sync cleanly: {detail}");
        observed.push(Observed {
            pack: "process-intelligence-rag-pack",
            activity: "pack.sync",
            started,
            finished,
            ok,
            detail,
        });
    }

    // --- Round 5: dspy-pack, real SHACL-derived-signature positive sync ---------------
    {
        let shacl_ttl = r#"
@prefix dspy: <http://seanchatmangpt.github.io/packs/dspy#> .
@prefix sh:   <http://www.w3.org/ns/shacl#> .
@prefix dcp:  <http://seanchatmangpt.github.io/packs/domain-capability#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
dspy:capability-shacl-signature a dspy:Signature ;
    dspy:className "CapabilitySignature" ;
    dcterms:title "CapabilitySignature" ;
    dcterms:description "Explain a capability given its slug and consequence, SHACL-derived." ;
    dspy:derivedFromShaclShape dspy:capability-node-shape .
dspy:capability-node-shape a sh:NodeShape ;
    sh:targetClass dcp:Capability ;
    sh:property dspy:slug-prop , dspy:consequence-prop , dspy:explanation-prop .
dspy:slug-prop a sh:PropertyShape ; sh:path dcp:slug ; sh:datatype xsd:string .
dspy:consequence-prop a sh:PropertyShape ; sh:path dcp:consequence ; sh:datatype xsd:string .
dspy:explanation-prop a sh:PropertyShape ;
    sh:path <http://seanchatmangpt.github.io/packs/domain-capability#explanation> ;
    sh:datatype xsd:string ; dspy:isOutputField true .
"#;
        let (_dir, project) =
            scaffold_pack_with_ontology(&packs_dir().join("dspy-pack"), shacl_ttl);
        let started = now_millis();
        let result = ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default());
        let finished = now_millis();
        let ok = result.is_ok();
        let detail = if ok {
            let bytes = read(&project, "src/dspy_shacl_signatures.py");
            blake3::hash(bytes.as_bytes()).to_hex().to_string()
        } else {
            format!("{}", result.err().expect("checked above"))
        };
        assert!(ok, "dspy-pack SHACL fixture must sync cleanly: {detail}");
        observed.push(Observed {
            pack: "dspy-pack",
            activity: "pack.sync",
            started,
            finished,
            ok,
            detail,
        });
    }

    // --- Negative case, represented honestly as data, not hidden: a real gate refusal --
    {
        let (_dir, project) = scaffold_pack(&packs_dir().join("domain-capability-pack"));
        ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default())
            .expect("baseline sync before sabotage");
        std::fs::write(
            project.join("ontology.ttl"),
            "@prefix dcp: <http://seanchatmangpt.github.io/packs/domain-capability#> .\n\
             dcp:sabotage-allowlist a dcp:LabAllowlist ;\n\
             \x20\x20\x20\x20dcp:ownerRepo \"sabotage-repo\" ;\n\
             \x20\x20\x20\x20dcp:allows dcp:this-capability-does-not-exist .\n",
        )
        .expect("write sabotage ontology");
        let started = now_millis();
        let result = ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default());
        let finished = now_millis();
        let ok = result.is_ok();
        let detail = format!(
            "{}",
            result.err().expect("sabotage must be refused, not silently admitted")
        );
        assert!(
            detail.contains("030_allowlist_subset"),
            "refusal must cite the real gate by name: {detail}"
        );
        observed.push(Observed {
            pack: "domain-capability-pack",
            activity: "gate.refusal",
            started,
            finished,
            ok,
            detail,
        });
    }

    // --- Build a real OCEL 2.0 JSON document from the real observations above ---------
    // Schema per ocel-standard.org: top-level objectTypes/eventTypes/objects/events;
    // objects carry {id, type, attributes: [{name, time, value}]}; events carry
    // {id, type, time, attributes, relationships: [{objectId, qualifier}]}.
    let object_types: Vec<Value> = observed
        .iter()
        .map(|o| o.pack)
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .map(|p| json!({"name": p, "attributes": []}))
        .collect();
    let event_types: Vec<Value> = observed
        .iter()
        .map(|o| o.activity)
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .map(|a| json!({"name": a, "attributes": [{"name": "detail", "type": "string"}]}))
        .collect();
    let objects: Vec<Value> = observed
        .iter()
        .map(|o| o.pack)
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .map(|p| json!({"id": p, "type": p, "attributes": []}))
        .collect();
    let events: Vec<Value> = observed
        .iter()
        .enumerate()
        .map(|(i, o)| {
            json!({
                "id": format!("ev_{i}"),
                "type": o.activity,
                "time": o.finished,
                "attributes": [
                    {"name": "detail", "value": o.detail, "time": o.finished},
                    {"name": "ok", "value": o.ok, "time": o.finished},
                    {"name": "started", "value": o.started, "time": o.finished},
                ],
                "relationships": [
                    {"objectId": o.pack, "qualifier": "subject"},
                ],
            })
        })
        .collect();
    let log = json!({
        "objectTypes": object_types,
        "eventTypes": event_types,
        "objects": objects,
        "events": events,
    });

    // --- Validate: real structural checks against the log actually produced above -----
    let events_arr = log["events"].as_array().expect("events array");
    assert_eq!(
        events_arr.len(),
        4,
        "one event per real sync() call this test actually made (3 positive + 1 refusal)"
    );

    let declared_object_ids: std::collections::HashSet<&str> = log["objects"]
        .as_array()
        .expect("objects array")
        .iter()
        .map(|o| o["id"].as_str().expect("object id"))
        .collect();

    let mut prev_time: Option<u128> = None;
    for event in events_arr {
        let id = event["id"].as_str().expect("event id non-empty");
        assert!(!id.is_empty(), "ocel:id must be non-empty");
        let ty = event["type"].as_str().expect("event type");
        assert!(!ty.is_empty(), "ocel:type must be non-empty for {id}");
        let time = event["time"]
            .as_u64()
            .expect("event time must be a real epoch-millis integer") as u128;
        // Real, observed chronological ordering -- this test made these 4 real sync() calls
        // strictly in sequence, so the log's own timestamps must reflect that.
        if let Some(prev) = prev_time {
            assert!(
                time >= prev,
                "event {id}'s real timestamp must not precede the previous real event"
            );
        }
        prev_time = Some(time);

        let rels = event["relationships"].as_array().expect("relationships");
        assert!(
            !rels.is_empty(),
            "event {id} must reference at least one real object (ocel:object-id)"
        );
        for rel in rels {
            let obj_id = rel["objectId"].as_str().expect("relationship objectId");
            assert!(
                declared_object_ids.contains(obj_id),
                "event {id} references undeclared object {obj_id} -- dangling reference"
            );
        }
    }

    // The one real refusal event must be distinguishable from the 4 real successes --
    // proving the log represents the actual observed outcome, not a synthetic "all green".
    let refusal_events: Vec<&Value> = events_arr
        .iter()
        .filter(|e| e["type"] == "gate.refusal")
        .collect();
    assert_eq!(refusal_events.len(), 1, "exactly one real refusal was observed");
    let refusal_ok_attr = refusal_events[0]["attributes"]
        .as_array()
        .expect("attributes")
        .iter()
        .find(|a| a["name"] == "ok")
        .expect("ok attribute present");
    assert_eq!(
        refusal_ok_attr["value"], false,
        "the refusal event's real ok attribute must be false, not fabricated as success"
    );

    // Round-trip through real JSON, matching ggen-graph's own OCEL round-trip test
    // convention (pack_events.rs::lifecycle_round_trips_through_json).
    let serialized = serde_json::to_string(&log).expect("serialize real OCEL v2 log");
    let restored: Value = serde_json::from_str(&serialized).expect("parse it back");
    assert_eq!(restored, log, "OCEL v2 log must survive a real JSON round-trip");

    eprintln!(
        "full_loop_ocel_v2_log:\n{}",
        serde_json::to_string_pretty(&log).expect("pretty print")
    );
}
