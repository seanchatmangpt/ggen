//! Chicago-TDD proofs for the receipt schema v1 -> v2 epoch migration
//! (`praxis_core::receipt_epoch`): dual-read/single-write, the admission
//! ledger's Red>Yellow>Green precedence, the closed 8-class equivalence map,
//! obligation accounting computed after admission processing, the ceiling
//! monotonicity rule, and the `M_1_to_2` migration receipt. Real sync() runs
//! on a real filesystem, real BLAKE3 chain hashing, no mocks.
//!
//! Exactly 12 tests, one `#[test]` each, matching the task's numbered list.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, RECEIPT_REL_PATH};
use praxis_core::{
    error::CoreError,
    law::Andon,
    receipt_epoch::{
        read_receipt_epoch, AdmissionDecision, AdmissionItem, AdmissionLedger, AndonLevel,
        CeilingLevel, ComponentLevels, EquivalenceMap, EquivalenceStatus, MigrationReceipt,
        ObligationCount, ObservedOutcome, ReceiptEpochV2, ReceiptEpochV2Builder,
        ReceiptRecordV1Legacy, MIGRATION_LAW_1_TO_2, SCHEMA_V1, SCHEMA_V2,
    },
    receipt_record::{ReceiptRecord, RECEIPT_RECORD_VERSION},
};
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Shared scaffolding (real filesystem, real sync())
// ---------------------------------------------------------------------------

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

fn scaffold(root: &Path, names: &[&str]) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    for name in names {
        ttl.push_str(&format!("ex:{name} ex:name \"{name}\" .\n"));
    }
    std::fs::write(root.join("ontology.ttl"), ttl).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

/// Build a real, chain-hash-valid v1-shaped record (schema `SCHEMA_V1`, no
/// `v2` payload, hardcoded `Andon::Green` -- exactly the pre-migration
/// shape).
fn v1_record(instruction_id: u64, prev_chain_hash_hex: &str) -> ReceiptRecord {
    let payload_hash_hex = format!("{instruction_id:02x}").repeat(32)[..64].to_string();
    let mut record = ReceiptRecord {
        version: RECEIPT_RECORD_VERSION,
        instruction_id,
        activity_idx: 0,
        activity: None,
        node_kind: 0,
        ts_ns: instruction_id * 1000,
        duration_ms: None,
        payload_hash_hex,
        prev_chain_hash_hex: prev_chain_hash_hex.to_string(),
        chain_hash_hex: String::new(),
        andon: Andon::Green,
        obligation_count: 0,
        object_ids: vec![format!("law:instr{instruction_id}")],
        signature_hex: None,
        schema: SCHEMA_V1.to_string(),
        v2: None,
    };
    let chain = record.recompute_chain_hash().expect("recompute v1 chain");
    record.chain_hash_hex = hex::encode(chain);
    record
}

/// Build a real, chain-hash-valid v2-shaped record chained onto
/// `prev_chain_hash_hex`, carrying `epoch` as its `v2` payload.
fn v2_record(instruction_id: u64, prev_chain_hash_hex: &str, epoch: ReceiptEpochV2) -> ReceiptRecord {
    let payload_hash_hex = format!("{instruction_id:02x}").repeat(32)[..64].to_string();
    let legacy_andon = match epoch.andon {
        AndonLevel::Green => Andon::Green,
        AndonLevel::Yellow => Andon::Overridden {
            by: "test".to_string(),
            reason: "quarantined".to_string(),
            at: 0,
        },
        AndonLevel::Red => Andon::Halted {
            unmet: vec![],
            refusals: vec![],
            at: 0,
        },
    };
    let mut record = ReceiptRecord {
        version: RECEIPT_RECORD_VERSION,
        instruction_id,
        activity_idx: 0,
        activity: None,
        node_kind: 0,
        ts_ns: instruction_id * 1000,
        duration_ms: None,
        payload_hash_hex,
        prev_chain_hash_hex: prev_chain_hash_hex.to_string(),
        chain_hash_hex: String::new(),
        andon: legacy_andon,
        obligation_count: epoch.obligation_count.remaining().unwrap_or(0),
        object_ids: vec![format!("law:instr{instruction_id}")],
        signature_hex: None,
        schema: SCHEMA_V2.to_string(),
        v2: Some(epoch),
    };
    let chain = record.recompute_chain_hash().expect("recompute v2 chain");
    record.chain_hash_hex = hex::encode(chain);
    record
}

// ---------------------------------------------------------------------------
// 1. old v1-only reader refuses to parse a v2 receipt with a real Err
// ---------------------------------------------------------------------------

#[test]
fn v1_only_reader_refuses_to_parse_a_v2_receipt() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    sync(dir.path(), SyncOptions {
        dry_run: false,
        ..Default::default()
    })
    .expect("real sync produces a v2 receipt");

    let raw = std::fs::read_to_string(dir.path().join(RECEIPT_REL_PATH)).expect("read receipt");
    let whole: serde_json::Value = serde_json::from_str(&raw).expect("parse json");
    let record_json = whole.get("record").expect("record field present").clone();

    // Sanity: this really is a v2 receipt (has `schema`/`v2` keys unknown to
    // the pre-migration wire shape).
    assert_eq!(record_json.get("schema").and_then(|v| v.as_str()), Some(SCHEMA_V2));
    assert!(record_json.get("v2").is_some());

    let result: Result<ReceiptRecordV1Legacy, _> = serde_json::from_value(record_json);
    assert!(
        result.is_err(),
        "an old v1-only (deny_unknown_fields) reader must refuse a real v2 receipt, got: {result:?}"
    );
}

// ---------------------------------------------------------------------------
// 2. a v1 receipt read by the v2-aware reader is treated as legacy-bounded
// ---------------------------------------------------------------------------

#[test]
fn v1_receipt_read_by_v2_aware_reader_is_legacy_bounded() {
    let record = v1_record(1, &"0".repeat(64));
    let epoch = read_receipt_epoch(&record).expect("v1 record reads as legacy-bounded");

    assert_eq!(epoch, ReceiptEpochV2::legacy_bounded());
    assert_eq!(epoch.admission, AdmissionLedger::LegacyUnrecorded);
    assert_eq!(epoch.standing_ceiling, CeilingLevel::LegacyObserved);
    assert_eq!(epoch.equivalence, EquivalenceMap::all_unknown());
    assert_eq!(epoch.obligation_count, ObligationCount::Unknown);
    assert!(!epoch.promotion_eligible);
}

// ---------------------------------------------------------------------------
// 3. a full v2 receipt round-trips (write then read) with all fields intact
// ---------------------------------------------------------------------------

#[test]
fn full_v2_receipt_round_trips_with_all_fields_intact() {
    // Rich payload: two admission items, non-trivial equivalence, real
    // obligation accounting.
    let components = ComponentLevels::uniform(AndonLevel::Yellow);
    let epoch = ReceiptEpochV2Builder::new(CeilingLevel::Green, components)
        .admission_item(AdmissionItem {
            evidence_id: "out/a.txt".to_string(),
            observed_outcome: ObservedOutcome::Pass,
            decision: AdmissionDecision::Admitted,
            reason: "written".to_string(),
            obligations_discharged: vec!["ob:1".to_string()],
            obligations_created: vec!["ob:1".to_string(), "ob:2".to_string()],
        })
        .admission_item(AdmissionItem {
            evidence_id: "out/b.txt".to_string(),
            observed_outcome: ObservedOutcome::Fail,
            decision: AdmissionDecision::Quarantined,
            reason: "pending review".to_string(),
            obligations_discharged: vec![],
            obligations_created: vec!["ob:3".to_string()],
        })
        .equivalence(EquivalenceMap {
            source: EquivalenceStatus::Divergent("template edited".to_string()),
            compiled_binary: EquivalenceStatus::Unknown,
            docs: EquivalenceStatus::Equivalent,
            tests: EquivalenceStatus::Unknown,
            receipts: EquivalenceStatus::Equivalent,
            evidence: EquivalenceStatus::Unknown,
            gates: EquivalenceStatus::Unknown,
            config: EquivalenceStatus::Equivalent,
        })
        .build()
        .expect("rich epoch builds");

    // Round-trip the payload alone.
    let json = serde_json::to_string(&epoch).expect("serialize epoch");
    let back: ReceiptEpochV2 = serde_json::from_str(&json).expect("deserialize epoch");
    assert_eq!(epoch, back);

    // Round-trip embedded in a full ReceiptRecord too.
    let record = v2_record(1, &"0".repeat(64), epoch.clone());
    let record_json = serde_json::to_string(&record).expect("serialize record");
    let record_back: ReceiptRecord =
        serde_json::from_str(&record_json).expect("deserialize record");
    assert_eq!(record, record_back);
    assert_eq!(record_back.schema, SCHEMA_V2);
    assert_eq!(record_back.v2, Some(epoch));
}

// ---------------------------------------------------------------------------
// 4. no code path can produce a derived Green andon from a v1 receipt's
//    hardcoded Green (tested directly against the reader function)
// ---------------------------------------------------------------------------

#[test]
fn v1_hardcoded_green_never_leaks_into_a_derived_green() {
    let record = v1_record(1, &"0".repeat(64));
    assert_eq!(record.andon, Andon::Green); // the hardcoded v1 fact

    let epoch = read_receipt_epoch(&record).expect("legacy-bounded reads");
    assert_ne!(
        epoch.andon,
        AndonLevel::Green,
        "a v1 receipt's hardcoded Green must never leak into a derived v2 Green"
    );
    assert_eq!(epoch.andon, AndonLevel::Yellow);
}

// ---------------------------------------------------------------------------
// 5. obligation_count of a receipt with zero admitted evidence items cannot
//    report 0 as if fully-discharged
// ---------------------------------------------------------------------------

#[test]
fn zero_admission_items_yields_explicit_zero_not_unknown() {
    let components = ComponentLevels::uniform(AndonLevel::Green);
    let epoch = ReceiptEpochV2Builder::new(CeilingLevel::Green, components)
        .build()
        .expect("empty-ledger epoch builds");

    assert_eq!(
        epoch.obligation_count,
        ObligationCount::Tracked {
            required: 0,
            discharged: 0
        }
    );
    assert_ne!(epoch.obligation_count, ObligationCount::Unknown);
    // Distinguishable at the API level too: `remaining()` is `Some(0)`, not
    // `None` (which is what `Unknown` reports).
    assert_eq!(epoch.obligation_count.remaining(), Some(0));
}

// ---------------------------------------------------------------------------
// 6. the migration receipt correctly links a real v1 hash to a real v2 hash
// ---------------------------------------------------------------------------

#[test]
fn migration_receipt_links_a_real_v1_hash_to_a_real_v2_hash() {
    let v1 = v1_record(1, &"0".repeat(64));

    let components = ComponentLevels::uniform(AndonLevel::Green);
    let epoch = ReceiptEpochV2Builder::new(CeilingLevel::LegacyObserved, components)
        .admission_item(AdmissionItem {
            evidence_id: "out/a.txt".to_string(),
            observed_outcome: ObservedOutcome::Pass,
            decision: AdmissionDecision::Admitted,
            reason: "written".to_string(),
            obligations_discharged: vec![],
            obligations_created: vec![],
        })
        .build()
        .expect("post-migration epoch builds");
    let v2 = v2_record(2, &v1.chain_hash_hex, epoch);

    // Real chain-hash continuity: the first v2 record chains directly onto
    // the final v1 record's chain hash.
    assert_eq!(v2.prev_chain_hash_hex, v1.chain_hash_hex);

    let migration = MigrationReceipt::new(v1.chain_hash_hex.clone(), v2.chain_hash_hex.clone());
    assert_eq!(migration.migration_law, MIGRATION_LAW_1_TO_2);
    assert_eq!(migration.from_schema, SCHEMA_V1);
    assert_eq!(migration.to_schema, SCHEMA_V2);
    assert_eq!(migration.final_v1_chain_hash_hex, v1.chain_hash_hex);
    assert_eq!(migration.first_v2_chain_hash_hex, v2.chain_hash_hex);
    assert_eq!(migration.resulting_ceiling, CeilingLevel::LegacyObserved);
    assert!(migration.becomes_unknown.contains(&"admission".to_string()));
    assert!(migration
        .carries_forward
        .contains(&"chain_hash_lineage".to_string()));
}

// ---------------------------------------------------------------------------
// 7. serializing a v2 receipt with an incomplete equivalence map (missing
//    one of the 8 classes) is a construction error, not silently accepted
// ---------------------------------------------------------------------------

#[test]
fn incomplete_equivalence_map_is_a_construction_error() {
    let components = ComponentLevels::uniform(AndonLevel::Green);
    let epoch = ReceiptEpochV2Builder::new(CeilingLevel::Green, components)
        .build()
        .expect("epoch builds");
    let record = v2_record(1, &"0".repeat(64), epoch);

    let mut record_json: serde_json::Value =
        serde_json::to_value(&record).expect("serialize record");
    let equivalence = record_json
        .get_mut("v2")
        .and_then(|v2| v2.get_mut("equivalence"))
        .and_then(|e| e.as_object_mut())
        .expect("equivalence object present");
    let removed = equivalence.remove("config");
    assert!(removed.is_some(), "precondition: config key was present");

    let result: Result<ReceiptRecord, _> = serde_json::from_value(record_json);
    assert!(
        result.is_err(),
        "a receipt whose equivalence map is missing one of the 8 classes must fail to \
         deserialize, not silently default the missing class, got: {result:?}"
    );
}

// ---------------------------------------------------------------------------
// 8. an Unknown equivalence value survives a serialize→deserialize round
//    trip unchanged
// ---------------------------------------------------------------------------

#[test]
fn unknown_equivalence_value_round_trips_unchanged() {
    let map = EquivalenceMap::all_unknown();
    let json = serde_json::to_string(&map).expect("serialize");
    let back: EquivalenceMap = serde_json::from_str(&json).expect("deserialize");
    assert_eq!(map, back);
    assert_eq!(back.config, EquivalenceStatus::Unknown);
    assert_eq!(back.source, EquivalenceStatus::Unknown);
}

// ---------------------------------------------------------------------------
// 9. an admission item marked as quarantined derives Yellow (not Green) and
//    sets promotion_eligible=false
// ---------------------------------------------------------------------------

#[test]
fn quarantined_admission_item_derives_yellow_and_blocks_promotion() {
    let components = ComponentLevels::uniform(AndonLevel::Yellow);
    let epoch = ReceiptEpochV2Builder::new(CeilingLevel::Green, components)
        .admission_item(AdmissionItem {
            evidence_id: "out/b.txt".to_string(),
            observed_outcome: ObservedOutcome::Unknown,
            decision: AdmissionDecision::Quarantined,
            reason: "pending review".to_string(),
            obligations_discharged: vec![],
            obligations_created: vec![],
        })
        .build()
        .expect("epoch with a quarantined item builds");

    assert_eq!(epoch.andon, AndonLevel::Yellow);
    assert_ne!(epoch.andon, AndonLevel::Green);
    assert!(!epoch.promotion_eligible);
}

// ---------------------------------------------------------------------------
// 10. an admission item marked refused derives Red
// ---------------------------------------------------------------------------

#[test]
fn refused_admission_item_derives_red() {
    let components = ComponentLevels::uniform(AndonLevel::Green);
    let epoch = ReceiptEpochV2Builder::new(CeilingLevel::Green, components)
        .admission_item(AdmissionItem {
            evidence_id: "out/c.txt".to_string(),
            observed_outcome: ObservedOutcome::Fail,
            decision: AdmissionDecision::Refused,
            reason: "SHACL violation".to_string(),
            obligations_discharged: vec![],
            obligations_created: vec![],
        })
        .admission_item(AdmissionItem {
            evidence_id: "out/d.txt".to_string(),
            observed_outcome: ObservedOutcome::Pass,
            decision: AdmissionDecision::Admitted,
            reason: "written".to_string(),
            obligations_discharged: vec![],
            obligations_created: vec![],
        })
        .build()
        .expect("epoch with a refused item builds");

    // Red beats Green even though the second item was cleanly admitted --
    // strict Red > Yellow > Green precedence.
    assert_eq!(epoch.andon, AndonLevel::Red);
    assert!(!epoch.promotion_eligible);
}

// ---------------------------------------------------------------------------
// 11. constructing a receipt with a ceiling above
//     meet(recoverable(prev), supported(new)) is refused
// ---------------------------------------------------------------------------

#[test]
fn ceiling_above_the_meet_is_refused_at_construction() {
    // supported(components) == Yellow, so meet(Green, Yellow) == Yellow.
    let components = ComponentLevels::uniform(AndonLevel::Yellow);
    let result = ReceiptEpochV2Builder::new(CeilingLevel::Green, components)
        .with_explicit_ceiling(CeilingLevel::Green) // above the allowed Yellow
        .build();

    match result {
        Err(CoreError::CeilingExceedsMeet { requested, allowed }) => {
            assert_eq!(requested, CeilingLevel::Green);
            assert_eq!(allowed, CeilingLevel::Yellow);
        }
        other => panic!("expected CeilingExceedsMeet, got {other:?}"),
    }

    // The same components, without forcing an above-meet ceiling, build
    // fine and land exactly on the allowed value.
    let components_ok = ComponentLevels::uniform(AndonLevel::Yellow);
    let ok = ReceiptEpochV2Builder::new(CeilingLevel::Green, components_ok)
        .with_explicit_ceiling(CeilingLevel::Yellow)
        .build()
        .expect("ceiling at the meet is accepted");
    assert_eq!(ok.standing_ceiling, CeilingLevel::Yellow);
}

// ---------------------------------------------------------------------------
// 12. replaying a chain of 3+ receipts (v1, migration, v2, v2) reproduces
//     the same final derived andon/ceiling aggregate deterministically on a
//     second replay
// ---------------------------------------------------------------------------

/// Independently walk a v1 record, a migration receipt, and one or more v2
/// records, verifying chain-hash continuity at every hop and returning the
/// final derived `(andon, ceiling)`. Pure and deterministic: no I/O, no
/// clocks, no randomness -- calling it twice on the same input must produce
/// identical output.
fn replay_chain(
    v1: &ReceiptRecord, migration: &MigrationReceipt, v2s: &[ReceiptRecord],
) -> (AndonLevel, CeilingLevel) {
    assert_eq!(migration.final_v1_chain_hash_hex, v1.chain_hash_hex, "migration must bind the real final v1 hash");
    let first_v2 = v2s.first().expect("at least one v2 record");
    assert_eq!(migration.first_v2_chain_hash_hex, first_v2.chain_hash_hex, "migration must bind the real first v2 hash");
    assert_eq!(first_v2.prev_chain_hash_hex, v1.chain_hash_hex, "chain hash continuity must not break across the v1/v2 boundary");

    let mut prev_chain_hash_hex = first_v2.chain_hash_hex.clone();
    let mut last_epoch = read_receipt_epoch(first_v2).expect("first v2 record readable");
    for record in &v2s[1..] {
        assert_eq!(record.prev_chain_hash_hex, prev_chain_hash_hex, "chain hash continuity within the v2 epoch");
        last_epoch = read_receipt_epoch(record).expect("v2 record readable");
        prev_chain_hash_hex = record.chain_hash_hex.clone();
    }
    (last_epoch.andon, last_epoch.standing_ceiling)
}

#[test]
fn replaying_a_mixed_chain_twice_is_deterministic() {
    let v1 = v1_record(1, &"0".repeat(64));

    let epoch1 = ReceiptEpochV2Builder::new(CeilingLevel::LegacyObserved, ComponentLevels::uniform(AndonLevel::Green))
        .admission_item(AdmissionItem {
            evidence_id: "out/a.txt".to_string(),
            observed_outcome: ObservedOutcome::Pass,
            decision: AdmissionDecision::Admitted,
            reason: "written".to_string(),
            obligations_discharged: vec![],
            obligations_created: vec![],
        })
        .build()
        .expect("epoch1 builds");
    let v2_a = v2_record(2, &v1.chain_hash_hex, epoch1);

    let migration = MigrationReceipt::new(v1.chain_hash_hex.clone(), v2_a.chain_hash_hex.clone());

    let epoch2 = ReceiptEpochV2Builder::new(
        // Chain onto the standing ceiling the first v2 record actually
        // carries -- LegacyObserved is a one-way cap (see
        // `receipt_epoch::recoverable`), so it stays capped even though
        // this generation's own evidence is clean.
        v2_a.v2.as_ref().expect("v2_a has a v2 payload").standing_ceiling,
        ComponentLevels::uniform(AndonLevel::Yellow),
    )
    .admission_item(AdmissionItem {
        evidence_id: "out/b.txt".to_string(),
        observed_outcome: ObservedOutcome::Unknown,
        decision: AdmissionDecision::Quarantined,
        reason: "pending review".to_string(),
        obligations_discharged: vec![],
        obligations_created: vec![],
    })
    .build()
    .expect("epoch2 builds");
    let v2_b = v2_record(3, &v2_a.chain_hash_hex, epoch2);

    let v2s = vec![v2_a, v2_b];

    let first = replay_chain(&v1, &migration, &v2s);
    let second = replay_chain(&v1, &migration, &v2s);
    assert_eq!(first, second, "replaying the same chain twice must be deterministic");

    // The final aggregate reflects the second (quarantined) generation:
    // Yellow andon, ceiling still capped at LegacyObserved (never promoted
    // to Green by inference, per the migration's necessarily-capped start).
    assert_eq!(first, (AndonLevel::Yellow, CeilingLevel::LegacyObserved));
}
