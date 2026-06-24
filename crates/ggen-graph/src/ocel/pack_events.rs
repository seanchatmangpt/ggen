//! OCEL event emitters for the marketplace pack lifecycle.
//!
//! This module records pack state transitions (install, remove, verify, publish)
//! and the lockfile writes / receipts that accompany them as OCEL (Object-Centric
//! Event Log) events, so the L (event-log) pole can be replayed for
//! process-mining conformance checks (see
//! `.claude/rules/process-mining-chicago-tdd.md`).
//!
//! It reuses the existing OCEL vocabulary from [`crate::ocel::ocel_types`]
//! ([`OcelLog`], [`OcelObject`], [`OcelEvent`], [`OcelObjectRef`]) rather than
//! defining a parallel event model. The constructors here are thin, lawful
//! builders that guarantee every emitted event carries the four required OCEL
//! attributes: `ocel:activity` (the [`OcelEvent::activity`] field),
//! `ocel:timestamp` (the [`OcelEvent::timestamp`] field), and at least one
//! `ocel:object-id` / `ocel:object-type` pair (via [`OcelObjectRef`]).
//!
//! Object types tracked here: [`OBJ_TYPE_PACK`], [`OBJ_TYPE_LOCKFILE_ENTRY`],
//! [`OBJ_TYPE_RECEIPT`]. Activities: [`ACT_PACK_INSTALL`], [`ACT_PACK_REMOVE`],
//! [`ACT_PACK_VERIFY`], [`ACT_PACK_PUBLISH`], [`ACT_LOCKFILE_WRITE`].
//!
//! Lawful-ordering note: the verify/publish/remove emitters reference an
//! existing pack object id, so a discovered process can only conform if a
//! `pack.install` event for that pack precedes its `pack.verify` /
//! `pack.publish` / `pack.remove`.

use chrono::{DateTime, Utc};
use std::collections::HashMap;

use crate::ocel::{OcelEvent, OcelObject, OcelObjectRef};

/// OCEL object type for a marketplace pack.
pub const OBJ_TYPE_PACK: &str = "pack";
/// OCEL object type for a single entry in `.ggen/packs.lock`.
pub const OBJ_TYPE_LOCKFILE_ENTRY: &str = "lockfile-entry";
/// OCEL object type for a cryptographic transition receipt.
pub const OBJ_TYPE_RECEIPT: &str = "receipt";

/// Activity name for installing a pack.
pub const ACT_PACK_INSTALL: &str = "pack.install";
/// Activity name for removing a pack.
pub const ACT_PACK_REMOVE: &str = "pack.remove";
/// Activity name for verifying a pack's digest against its lockfile entry.
pub const ACT_PACK_VERIFY: &str = "pack.verify";
/// Activity name for publishing a pack to the marketplace.
pub const ACT_PACK_PUBLISH: &str = "pack.publish";
/// Activity name for writing the lockfile.
pub const ACT_LOCKFILE_WRITE: &str = "lockfile.write";

/// Event-to-object qualifier marking the primary subject of a lifecycle event.
pub const QUAL_SUBJECT: &str = "subject";
/// Event-to-object qualifier marking the lockfile entry written/consumed by an event.
pub const QUAL_LOCKED_BY: &str = "locked-by";
/// Event-to-object qualifier marking the receipt produced by an event.
pub const QUAL_PRODUCES_RECEIPT: &str = "produces-receipt";

/// Returns the stable OCEL object id for a pack, derived from its id and version.
///
/// IDs are deterministic so the same pack at the same version always maps to the
/// same OCEL object across separate event emissions (install, verify, publish).
#[must_use]
pub fn pack_object_id(pack_id: &str, version: &str) -> String {
    format!("pack:{pack_id}@{version}")
}

/// Returns the stable OCEL object id for a lockfile entry, derived from the pack
/// id and version it locks.
#[must_use]
pub fn lockfile_entry_object_id(pack_id: &str, version: &str) -> String {
    format!("lockfile-entry:{pack_id}@{version}")
}

/// Returns the stable OCEL object id for a receipt, derived from its operation id.
#[must_use]
pub fn receipt_object_id(operation_id: &str) -> String {
    format!("receipt:{operation_id}")
}

/// Builds the OCEL object describing a pack.
///
/// The `digest` (SHA-256 of the pack TOML at install time) is recorded as an
/// attribute so downstream conformance checks can detect drift.
#[must_use]
pub fn pack_object(pack_id: &str, version: &str, digest: &str) -> OcelObject {
    let mut attributes = HashMap::new();
    attributes.insert("pack_id".to_string(), pack_id.to_string());
    attributes.insert("version".to_string(), version.to_string());
    attributes.insert("digest".to_string(), digest.to_string());
    OcelObject {
        id: pack_object_id(pack_id, version),
        r#type: OBJ_TYPE_PACK.to_string(),
        attributes,
    }
}

/// Builds the OCEL object describing a lockfile entry for a pack.
#[must_use]
pub fn lockfile_entry_object(pack_id: &str, version: &str, digest: &str) -> OcelObject {
    let mut attributes = HashMap::new();
    attributes.insert("pack_id".to_string(), pack_id.to_string());
    attributes.insert("version".to_string(), version.to_string());
    attributes.insert("digest".to_string(), digest.to_string());
    OcelObject {
        id: lockfile_entry_object_id(pack_id, version),
        r#type: OBJ_TYPE_LOCKFILE_ENTRY.to_string(),
        attributes,
    }
}

/// Builds the OCEL object describing a transition receipt.
#[must_use]
pub fn receipt_object(operation_id: &str, signature: &str) -> OcelObject {
    let mut attributes = HashMap::new();
    attributes.insert("operation_id".to_string(), operation_id.to_string());
    attributes.insert("signature".to_string(), signature.to_string());
    OcelObject {
        id: receipt_object_id(operation_id),
        r#type: OBJ_TYPE_RECEIPT.to_string(),
        attributes,
    }
}

/// Internal helper: build an event whose only object reference is the pack
/// itself, marked as the subject of the activity.
fn pack_subject_event(
    event_id: &str, activity: &str, timestamp: DateTime<Utc>, pack_id: &str, version: &str,
) -> OcelEvent {
    OcelEvent {
        id: event_id.to_string(),
        activity: activity.to_string(),
        timestamp,
        objects: vec![OcelObjectRef {
            id: pack_object_id(pack_id, version),
            r#type: OBJ_TYPE_PACK.to_string(),
            qualifier: Some(QUAL_SUBJECT.to_string()),
        }],
        attributes: HashMap::new(),
    }
}

/// Emits a `pack.install` OCEL event.
///
/// The event references the pack object (as subject) and the lockfile entry it
/// creates (qualifier [`QUAL_LOCKED_BY`]), recording that an install both
/// materialises a pack and locks it.
#[must_use]
pub fn emit_pack_install(
    event_id: &str, timestamp: DateTime<Utc>, pack_id: &str, version: &str,
) -> OcelEvent {
    OcelEvent {
        id: event_id.to_string(),
        activity: ACT_PACK_INSTALL.to_string(),
        timestamp,
        objects: vec![
            OcelObjectRef {
                id: pack_object_id(pack_id, version),
                r#type: OBJ_TYPE_PACK.to_string(),
                qualifier: Some(QUAL_SUBJECT.to_string()),
            },
            OcelObjectRef {
                id: lockfile_entry_object_id(pack_id, version),
                r#type: OBJ_TYPE_LOCKFILE_ENTRY.to_string(),
                qualifier: Some(QUAL_LOCKED_BY.to_string()),
            },
        ],
        attributes: HashMap::new(),
    }
}

/// Emits a `pack.verify` OCEL event.
///
/// References the existing pack object (subject) and the lockfile entry whose
/// digest the pack is checked against. Because it reuses the pack object id from
/// install, a lawful log must contain the matching `pack.install` first.
#[must_use]
pub fn emit_pack_verify(
    event_id: &str, timestamp: DateTime<Utc>, pack_id: &str, version: &str,
) -> OcelEvent {
    OcelEvent {
        id: event_id.to_string(),
        activity: ACT_PACK_VERIFY.to_string(),
        timestamp,
        objects: vec![
            OcelObjectRef {
                id: pack_object_id(pack_id, version),
                r#type: OBJ_TYPE_PACK.to_string(),
                qualifier: Some(QUAL_SUBJECT.to_string()),
            },
            OcelObjectRef {
                id: lockfile_entry_object_id(pack_id, version),
                r#type: OBJ_TYPE_LOCKFILE_ENTRY.to_string(),
                qualifier: Some(QUAL_LOCKED_BY.to_string()),
            },
        ],
        attributes: HashMap::new(),
    }
}

/// Emits a `pack.publish` OCEL event.
///
/// References the existing pack object (subject) and the receipt produced by the
/// publish operation (qualifier [`QUAL_PRODUCES_RECEIPT`]).
#[must_use]
pub fn emit_pack_publish(
    event_id: &str, timestamp: DateTime<Utc>, pack_id: &str, version: &str, operation_id: &str,
) -> OcelEvent {
    OcelEvent {
        id: event_id.to_string(),
        activity: ACT_PACK_PUBLISH.to_string(),
        timestamp,
        objects: vec![
            OcelObjectRef {
                id: pack_object_id(pack_id, version),
                r#type: OBJ_TYPE_PACK.to_string(),
                qualifier: Some(QUAL_SUBJECT.to_string()),
            },
            OcelObjectRef {
                id: receipt_object_id(operation_id),
                r#type: OBJ_TYPE_RECEIPT.to_string(),
                qualifier: Some(QUAL_PRODUCES_RECEIPT.to_string()),
            },
        ],
        attributes: HashMap::new(),
    }
}

/// Emits a `pack.remove` OCEL event referencing the existing pack object.
#[must_use]
pub fn emit_pack_remove(
    event_id: &str, timestamp: DateTime<Utc>, pack_id: &str, version: &str,
) -> OcelEvent {
    pack_subject_event(event_id, ACT_PACK_REMOVE, timestamp, pack_id, version)
}

/// Emits a `lockfile.write` OCEL event referencing the lockfile entry written.
#[must_use]
pub fn emit_lockfile_write(
    event_id: &str, timestamp: DateTime<Utc>, pack_id: &str, version: &str,
) -> OcelEvent {
    OcelEvent {
        id: event_id.to_string(),
        activity: ACT_LOCKFILE_WRITE.to_string(),
        timestamp,
        objects: vec![OcelObjectRef {
            id: lockfile_entry_object_id(pack_id, version),
            r#type: OBJ_TYPE_LOCKFILE_ENTRY.to_string(),
            qualifier: Some(QUAL_SUBJECT.to_string()),
        }],
        attributes: HashMap::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ocel::{OcelLog, OcelObject};
    use chrono::{TimeZone, Utc};

    // Real, deterministic timestamps for a lawful install -> verify -> publish
    // sequence. No panics: `single()` + `unwrap_or_else` mirrors the
    // non-panicking pattern already used in `conformance.rs`.
    fn ts(secs: i64) -> DateTime<Utc> {
        Utc.timestamp_opt(secs, 0).single().unwrap_or_else(Utc::now)
    }

    const PACK_ID: &str = "acme/base";
    const VERSION: &str = "1.2.3";
    const DIGEST: &str = "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b";
    const OPERATION_ID: &str = "11111111-1111-4111-8111-111111111111";

    /// Build a lawful pack-lifecycle log: install (t=10) -> verify (t=20) ->
    /// publish (t=30), with the three required object types present.
    fn lawful_pack_log() -> OcelLog {
        let mut log = OcelLog::new();

        log.objects.push(pack_object(PACK_ID, VERSION, DIGEST));
        log.objects
            .push(lockfile_entry_object(PACK_ID, VERSION, DIGEST));
        log.objects.push(receipt_object(OPERATION_ID, "c2lnbmF0dXJl"));

        log.events
            .push(emit_pack_install("ev_install", ts(10), PACK_ID, VERSION));
        log.events
            .push(emit_pack_verify("ev_verify", ts(20), PACK_ID, VERSION));
        log.events.push(emit_pack_publish(
            "ev_publish",
            ts(30),
            PACK_ID,
            VERSION,
            OPERATION_ID,
        ));

        log
    }

    #[test]
    fn lifecycle_round_trips_through_json() -> Result<(), serde_json::Error> {
        // Arrange
        let log = lawful_pack_log();

        // Act: serialize to JSON and deserialize back.
        let json = serde_json::to_string(&log)?;
        let restored: OcelLog = serde_json::from_str(&json)?;

        // Assert: observable serialized state survives the round-trip intact.
        assert_eq!(restored, log, "OCEL log must survive a JSON round-trip");
        assert_eq!(restored.events.len(), 3, "three lifecycle events expected");
        assert_eq!(
            restored.objects.len(),
            3,
            "pack, lockfile-entry, and receipt objects expected"
        );
        Ok(())
    }

    #[test]
    fn required_ocel_attributes_are_present_and_non_empty() -> Result<(), serde_json::Error> {
        // Arrange + Act: round-trip through JSON so we assert on serialized state.
        let json = serde_json::to_string(&lawful_pack_log())?;
        let restored: OcelLog = serde_json::from_str(&json)?;

        // Assert on every event: ocel:activity, ocel:timestamp, and at least one
        // ocel:object-id / ocel:object-type pair are present and non-empty.
        for event in &restored.events {
            assert!(!event.id.is_empty(), "event id must be non-empty");
            assert!(
                !event.activity.is_empty(),
                "ocel:activity must be non-empty for {}",
                event.id
            );
            // A real chrono timestamp; verify it serializes to a non-empty RFC3339 string.
            let ts_json = serde_json::to_string(&event.timestamp)?;
            assert!(
                ts_json.len() > 2,
                "ocel:timestamp must serialize to a real value for {}",
                event.id
            );
            assert!(
                !event.objects.is_empty(),
                "event {} must reference at least one object",
                event.id
            );
            for obj_ref in &event.objects {
                assert!(
                    !obj_ref.id.is_empty(),
                    "ocel:object-id must be non-empty in {}",
                    event.id
                );
                assert!(
                    !obj_ref.r#type.is_empty(),
                    "ocel:object-type must be non-empty in {}",
                    event.id
                );
            }
        }
        Ok(())
    }

    #[test]
    fn timestamps_are_lawfully_ordered() -> Result<(), serde_json::Error> {
        // Arrange + Act
        let json = serde_json::to_string(&lawful_pack_log())?;
        let restored: OcelLog = serde_json::from_str(&json)?;

        // Locate each lifecycle event's timestamp by its activity.
        let find = |activity: &str| -> Option<DateTime<Utc>> {
            restored
                .events
                .iter()
                .find(|e| e.activity == activity)
                .map(|e| e.timestamp)
        };

        // Each lifecycle activity must be present (proves completeness), and the
        // timestamps must be strictly increasing (proves lawful ordering).
        let install = find(ACT_PACK_INSTALL);
        let verify = find(ACT_PACK_VERIFY);
        let publish = find(ACT_PACK_PUBLISH);

        assert!(install.is_some(), "pack.install event must be present");
        assert!(verify.is_some(), "pack.verify event must be present");
        assert!(publish.is_some(), "pack.publish event must be present");

        // Assert lawful ordering: install < verify < publish.
        assert!(
            install < verify,
            "pack.install must precede pack.verify (install={install:?}, verify={verify:?})"
        );
        assert!(
            verify < publish,
            "pack.verify must precede pack.publish (verify={verify:?}, publish={publish:?})"
        );
        Ok(())
    }

    #[test]
    fn pack_object_id_is_consistent_across_lifecycle_events() -> Result<(), serde_json::Error> {
        // Arrange + Act
        let json = serde_json::to_string(&lawful_pack_log())?;
        let restored: OcelLog = serde_json::from_str(&json)?;

        let expected_pack_id = pack_object_id(PACK_ID, VERSION);

        // The pack subject object id must be identical in install, verify, and publish.
        let subject_ids: Vec<String> = restored
            .events
            .iter()
            .filter_map(|e| {
                e.objects
                    .iter()
                    .find(|o| o.r#type == OBJ_TYPE_PACK)
                    .map(|o| o.id.clone())
            })
            .collect();

        assert_eq!(
            subject_ids.len(),
            3,
            "all three lifecycle events reference a pack object"
        );
        for id in &subject_ids {
            assert_eq!(
                id, &expected_pack_id,
                "pack object id must be stable across all lifecycle events"
            );
        }

        // The pack object id used by events must match the declared pack object.
        let declared_pack: Vec<&OcelObject> = restored
            .objects
            .iter()
            .filter(|o| o.r#type == OBJ_TYPE_PACK)
            .collect();
        assert_eq!(declared_pack.len(), 1, "exactly one pack object declared");
        assert_eq!(
            declared_pack[0].id, expected_pack_id,
            "declared pack object id must match the id referenced by events"
        );

        // The lockfile entry referenced by install/verify must also be consistent.
        let expected_lock_id = lockfile_entry_object_id(PACK_ID, VERSION);
        let lock_ids: Vec<String> = restored
            .events
            .iter()
            .flat_map(|e| e.objects.iter())
            .filter(|o| o.r#type == OBJ_TYPE_LOCKFILE_ENTRY)
            .map(|o| o.id.clone())
            .collect();
        assert_eq!(
            lock_ids.len(),
            2,
            "install and verify each reference the lockfile entry"
        );
        for id in &lock_ids {
            assert_eq!(
                id, &expected_lock_id,
                "lockfile entry object id must be stable across events"
            );
        }
        Ok(())
    }

    #[test]
    fn remove_and_lockfile_write_emitters_carry_required_attributes(
    ) -> Result<(), serde_json::Error> {
        // Arrange: the two emitters not exercised by the install->verify->publish path.
        let remove = emit_pack_remove("ev_remove", ts(40), PACK_ID, VERSION);
        let lock_write = emit_lockfile_write("ev_lock", ts(5), PACK_ID, VERSION);

        // Act: round-trip both individually.
        let remove_restored: OcelEvent = serde_json::from_str(&serde_json::to_string(&remove)?)?;
        let lock_restored: OcelEvent = serde_json::from_str(&serde_json::to_string(&lock_write)?)?;

        // Assert remove references the pack subject.
        assert_eq!(remove_restored.activity, ACT_PACK_REMOVE);
        assert_eq!(
            remove_restored.objects.first().map(|o| o.id.clone()),
            Some(pack_object_id(PACK_ID, VERSION))
        );

        // Assert lockfile.write references the lockfile entry subject.
        assert_eq!(lock_restored.activity, ACT_LOCKFILE_WRITE);
        assert_eq!(
            lock_restored.objects.first().map(|o| o.id.clone()),
            Some(lockfile_entry_object_id(PACK_ID, VERSION))
        );
        Ok(())
    }
}
