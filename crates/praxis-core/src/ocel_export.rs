//! `ocel_export` — convert persisted [`ReceiptRecord`]s into an OCEL 2.0 log
//! (`wasm4pm_compat::ocel::OCEL`), modeled on `bcinr-powl`'s
//! `OcelLog::to_ocel_2_0` pattern (`bcinr-powl/src/ocel.rs`).

use std::collections::BTreeSet;

use chrono::{DateTime, FixedOffset, TimeZone, Utc};
use wasm4pm_compat::ocel::{
    OCELEvent, OCELEventAttribute, OCELObject, OCELRelationship, OCELType, OCEL,
};

use crate::{law::Andon, receipt_record::ReceiptRecord};

/// Convert a nanosecond UNIX timestamp into an RFC3339 `DateTime` (UTC,
/// represented with a zero `FixedOffset`).
///
/// Out-of-range or otherwise unrepresentable `ts_ns` values fall back to the
/// UNIX epoch rather than panicking — this is a display/export concern, not
/// something that should ever abort a receipt export.
///
/// Shared by both this module's `to_ocel` and `crate::ocel`'s `ToOcelEvent`
/// impl, replacing the latter's previously hardcoded
/// `"2026-07-01T00:00:00Z"` literal with a real timestamp.
#[must_use]
pub fn ts_ns_to_rfc3339(ts_ns: u64) -> DateTime<FixedOffset> {
    let secs = (ts_ns / 1_000_000_000) as i64;
    let nanos = (ts_ns % 1_000_000_000) as u32;
    Utc.timestamp_opt(secs, nanos)
        .single()
        .unwrap_or_else(|| {
            Utc.timestamp_opt(0, 0)
                .single()
                .expect("epoch is always a valid timestamp")
        })
        .into()
}

/// Short, human-legible label for an [`Andon`] variant (used as an OCEL event
/// attribute; not the full serialized `Andon` value).
fn andon_label(andon: &Andon) -> &'static str {
    match andon {
        Andon::Green => "green",
        Andon::Halted { .. } => "halted",
        Andon::Overridden { .. } => "overridden",
    }
}

/// The chain-lineage object identifier for a record: the first 16 hex
/// characters of the chain hash this record was chained onto.
fn chain_object_id(record: &ReceiptRecord) -> String {
    let prefix_len = record.prev_chain_hash_hex.len().min(16);
    format!("chain:{}", &record.prev_chain_hash_hex[..prefix_len])
}

/// Convert a slice of [`ReceiptRecord`]s into an OCEL 2.0 log.
///
/// - `event_types`: a single `lifecycle:receipt` type.
/// - `object_types`: `LawObject` (one instance per unique `object_ids` entry
///   across all records) and `ReceiptChain` (one instance per distinct chain
///   lineage, identified by the first 16 hex characters of the record's
///   `prev_chain_hash_hex`).
/// - `events`: one `lifecycle:receipt` event per record, with id
///   `receipt:<chain_hash_hex>` (globally unique — chain hashes are BLAKE3
///   outputs), a real RFC3339 `time` derived from `ts_ns` (see
///   [`ts_ns_to_rfc3339`]), `instruction_id`/`payload_hash`/`chain_hash`/
///   `andon` attributes, and relationships to its `object_ids` (qualifier
///   `"governs"`) and its chain object (qualifier `"extends"`).
#[must_use]
pub fn to_ocel(records: &[ReceiptRecord]) -> OCEL {
    let event_types = vec![OCELType {
        name: "lifecycle:receipt".to_string(),
        attributes: vec![],
    }];
    let object_types = vec![
        OCELType {
            name: "LawObject".to_string(),
            attributes: vec![],
        },
        OCELType {
            name: "ReceiptChain".to_string(),
            attributes: vec![],
        },
    ];

    let mut law_object_ids: BTreeSet<String> = BTreeSet::new();
    let mut chain_ids: BTreeSet<String> = BTreeSet::new();
    for record in records {
        law_object_ids.extend(record.object_ids.iter().cloned());
        chain_ids.insert(chain_object_id(record));
    }

    let mut objects: Vec<OCELObject> = Vec::new();
    for id in &law_object_ids {
        objects.push(OCELObject::new(id.clone(), "LawObject"));
    }
    for id in &chain_ids {
        objects.push(OCELObject::new(id.clone(), "ReceiptChain"));
    }

    let mut events: Vec<OCELEvent> = Vec::new();
    for record in records {
        let mut event = OCELEvent::new(
            format!("receipt:{}", record.chain_hash_hex),
            "lifecycle:receipt",
        );
        event.time = ts_ns_to_rfc3339(record.ts_ns);
        event.attributes.push(OCELEventAttribute::integer(
            "instruction_id",
            record.instruction_id as i64,
        ));
        event.attributes.push(OCELEventAttribute::string(
            "payload_hash",
            record.payload_hash_hex.clone(),
        ));
        event.attributes.push(OCELEventAttribute::string(
            "chain_hash",
            record.chain_hash_hex.clone(),
        ));
        event.attributes.push(OCELEventAttribute::string(
            "andon",
            andon_label(&record.andon).to_string(),
        ));

        for object_id in &record.object_ids {
            event.relationships.push(OCELRelationship {
                object_id: object_id.clone(),
                qualifier: "governs".to_string(),
            });
        }
        event.relationships.push(OCELRelationship {
            object_id: chain_object_id(record),
            qualifier: "extends".to_string(),
        });

        events.push(event);
    }

    OCEL {
        event_types,
        object_types,
        events,
        objects,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_record(instruction_id: u64, ts_ns: u64) -> ReceiptRecord {
        ReceiptRecord {
            version: crate::receipt_record::RECEIPT_RECORD_VERSION,
            instruction_id,
            activity_idx: 0,
            activity: None,
            node_kind: 0,
            ts_ns,
            duration_ms: None,
            payload_hash_hex: "11".repeat(32),
            prev_chain_hash_hex: "0".repeat(64),
            chain_hash_hex: format!("{:064x}", instruction_id),
            andon: Andon::Green,
            obligation_count: 0,
            object_ids: vec![format!("law:instr{instruction_id}")],
            signature_hex: None,
        }
    }

    #[test]
    fn ts_ns_to_rfc3339_is_not_the_old_hardcoded_literal() {
        // ts_ns corresponding to 2024-01-01T00:00:00Z, distinct from the
        // previously-hardcoded "2026-07-01T00:00:00Z" bug.
        let ts_ns: u64 = 1_704_067_200_000_000_000;
        let rendered = ts_ns_to_rfc3339(ts_ns).to_rfc3339();
        assert!(rendered.starts_with("2024-01-01T00:00:00"));
        assert_ne!(rendered, "2026-07-01T00:00:00Z");
    }

    #[test]
    fn ts_ns_to_rfc3339_round_trips_through_parsing() {
        let ts_ns: u64 = 1_700_000_000_123_456_789;
        let rendered = ts_ns_to_rfc3339(ts_ns).to_rfc3339();
        let parsed: DateTime<FixedOffset> = rendered.parse().expect("must parse as RFC3339");
        assert_eq!(parsed, ts_ns_to_rfc3339(ts_ns));
    }

    #[test]
    fn to_ocel_has_expected_event_and_object_types() {
        let records = vec![sample_record(1, 1_000), sample_record(2, 2_000)];
        let ocel = to_ocel(&records);

        assert_eq!(ocel.event_types.len(), 1);
        assert_eq!(ocel.event_types[0].name, "lifecycle:receipt");
        let object_type_names: Vec<&str> =
            ocel.object_types.iter().map(|t| t.name.as_str()).collect();
        assert!(object_type_names.contains(&"LawObject"));
        assert!(object_type_names.contains(&"ReceiptChain"));

        assert_eq!(ocel.events.len(), 2);
        assert_eq!(ocel.objects.len(), 3); // 2 LawObjects + 1 shared ReceiptChain
    }

    #[test]
    fn to_ocel_events_link_to_object_ids_and_chain() {
        let records = vec![sample_record(1, 1_000)];
        let ocel = to_ocel(&records);
        let event = &ocel.events[0];
        let rel_ids: Vec<&str> = event
            .relationships
            .iter()
            .map(|r| r.object_id.as_str())
            .collect();
        assert!(rel_ids.contains(&"law:instr1"));
        assert!(rel_ids.iter().any(|id| id.starts_with("chain:")));
    }

    #[test]
    fn to_ocel_json_round_trips() {
        let records = vec![sample_record(1, 1_000)];
        let ocel = to_ocel(&records);
        let json = serde_json::to_string(&ocel).expect("serialize");
        let parsed: OCEL = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(parsed.events.len(), 1);
        assert_eq!(parsed.events[0].time, ocel.events[0].time);
    }
}
