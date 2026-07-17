//! OCEL 2.0 event conversion for law objects (feature-gated).

use serde::{Deserialize, Serialize};

use crate::{law::LawObject, lifecycle::Receipted};

/// Reference to an OCEL object (with type and optional qualifier).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelObjectRef {
    /// Object identifier.
    pub id: String,
    /// Object type.
    pub r#type: String,
    /// Optional qualifier (e.g., "ROLE" or "ACTIVITY").
    pub qualifier: Option<String>,
}

/// OCEL 2.0 event representation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelEvent {
    /// Event identifier.
    pub id: String,
    /// Event type (e.g., "lifecycle:judgment", "lifecycle:receipt").
    pub r#type: String,
    /// ISO 8601 timestamp.
    pub time: String,
    /// Event attributes as JSON.
    pub attributes: serde_json::Value,
    /// Related objects.
    pub relationships: Vec<OcelObjectRef>,
}

/// Trait for converting a type into an OCEL event.
pub trait ToOcelEvent {
    /// Convert self into an OCEL event representation.
    fn to_ocel_event(&self) -> OcelEvent;
}

impl<Payload: Serialize, Law: std::fmt::Debug> ToOcelEvent for LawObject<Payload, Receipted, Law> {
    fn to_ocel_event(&self) -> OcelEvent {
        let payload_hash = blake3::hash(
            serde_json::to_vec(&self.payload)
                .unwrap_or_default()
                .as_slice(),
        );
        let payload_hex = payload_hash
            .as_bytes()
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect::<String>();
        let event_id = format!("receipt:{}", payload_hex);

        let chain_hash_hex = self
            .chain_hash
            .map(|h| h.iter().map(|b| format!("{:02x}", b)).collect::<String>());

        let attributes = serde_json::json!({
            "obligations": self.obligations,
            "andon": self.andon,
            "chain_hash": chain_hash_hex,
        });

        // `LawObject` does not persist the timestamp it was receipted at
        // (`ReceiptMeta.ts_ns` is consumed by `receipt()` to build the chain
        // frame, then discarded), so this uses the current wall-clock time
        // rather than a fabricated one. This replaces a previously
        // hardcoded `"2026-07-01T00:00:00Z"` literal, which produced the
        // same (wrong) timestamp for every event regardless of when it was
        // actually converted. Callers that need the exact receipt-time
        // timestamp should persist a `crate::receipt_record::ReceiptRecord`
        // via `receipt_with_record()` and export via
        // `crate::ocel_export::to_ocel`, which uses the record's real
        // `ts_ns` instead of "now".
        let now_ns = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as u64;
        let time = crate::ocel_export::ts_ns_to_rfc3339(now_ns).to_rfc3339();

        OcelEvent {
            id: event_id,
            r#type: "lifecycle:receipt".to_string(),
            time,
            attributes,
            relationships: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lifecycle::Raw, Admit, DefaultLaw, Judge};

    fn receipted(
        payload: serde_json::Value,
    ) -> LawObject<serde_json::Value, Receipted, DefaultLaw> {
        // `receipt()` signs the chain hash when `signed` is also enabled,
        // which needs a `PRAXIS_SIGNING_KEY`; see `signing::test_support`.
        #[cfg(feature = "signed")]
        let _guard = crate::signing::test_support::with_test_signing_key();

        // Drive the real Raw -> Validated -> Admitted -> Receipted lifecycle
        // through the public API rather than hand-rolling a `Receipted`
        // value (the stage phantom fields are private to `law.rs`).
        //
        // `LawObject` intentionally does not derive `Debug` (its phantom
        // stage/law markers are not all `Debug`), so unwrap via `match`
        // instead of `.expect()` (mirrors `default_law.rs`'s own tests).
        let raw = LawObject::<serde_json::Value, Raw, DefaultLaw>::new(payload, vec![]);
        let validated = match DefaultLaw::judge(raw) {
            Ok(v) => v,
            Err(_) => panic!("no obligations should always validate"),
        };
        let admitted = match DefaultLaw::admit(validated) {
            Ok(a) => a,
            Err(_) => panic!("green andon should always admit"),
        };
        match admitted.receipt(&[0u8; 32], crate::law::ReceiptMeta::default()) {
            Ok(r) => r,
            Err(e) => panic!("receipt should succeed: {e}"),
        }
    }

    #[test]
    fn to_ocel_event_time_is_not_the_old_hardcoded_literal() {
        let event = receipted(serde_json::json!({"a": 1})).to_ocel_event();
        assert_ne!(event.time, "2026-07-01T00:00:00Z");
        // Must parse as a real RFC3339 timestamp.
        assert!(event
            .time
            .parse::<chrono::DateTime<chrono::FixedOffset>>()
            .is_ok());
    }
}
