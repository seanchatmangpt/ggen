//! Fuzz the praxis-core admission boundaries with proptest.
//!
//! Genesis Day 3, phase 2 (fuzzing). This crate's job is to *refuse* untrusted
//! input safely, so the load-bearing property is **total absence of panics**:
//! no adversarial string, byte sequence, or malformed receipt field may ever
//! unwind through a boundary. Every refusal must additionally *carry a reason*
//! (a non-empty diagnostic), never a silent or empty error.
//!
//! Surfaces fuzzed here (the praxis-core half of the admission perimeter):
//!   * [`RiceQuarantine::admit`] — arbitrary strings, arbitrary bytes rendered
//!     lossily as UTF-8, and JSON-biased strings, against both a permissive
//!     `serde_json::Value` schema and a typed+predicate schema.
//!   * [`ReceiptRecord::recompute_chain_hash`] — arbitrary hex-ish field values
//!     (the hex-decode boundary must reject, never panic).
//!   * [`ReceiptValidator::validate`] — arbitrary receipt ledgers (the staged
//!     verifier must always produce a `Verdict`, never unwind, and `ok` must
//!     imply every stage passed or was skipped).
//!
//! The `ops`/config/PDDL/promote surfaces live in the root crate's
//! `tests/fuzz_ops.rs`; the receipt *chain* is mutation-tested in
//! `tests/mutation_chain.rs`.
//!
//! ## Case counts
//!
//! Every property below runs `FUZZ_CASES` (default **2048**) iterations — these
//! are all cheap pure-CPU boundaries, so a high count buys real coverage. Override
//! at runtime with `PROPTEST_CASES=<n>` (proptest reads it and it wins over the
//! `with_cases` default), e.g. `PROPTEST_CASES=50000 cargo test -p praxis-core --test fuzz_boundaries`.

use praxis_core::{
    law::Andon,
    receipt_record::{ReceiptRecord, RECEIPT_RECORD_VERSION},
    receipt_validator::{FixedClock, ReceiptValidator},
    JsonBoundarySchema, QuarantineError, RiceQuarantine,
};
use proptest::prelude::*;
use serde::{Deserialize, Serialize};

/// Default proptest iteration count for every property in this file. High,
/// because each case is a cheap pure-CPU boundary call. Override with the
/// `PROPTEST_CASES` environment variable.
const FUZZ_CASES: u32 = 2048;

fn cfg() -> ProptestConfig {
    ProptestConfig::with_cases(FUZZ_CASES)
}

/// A typed payload used to exercise the deserialize + predicate leg of the
/// quarantine (arbitrary JSON must never panic even when it must be coerced
/// into a concrete struct and run through a domain predicate).
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TypedPayload {
    id: String,
    action: String,
}

/// A refusal must always carry a non-empty reason — an empty diagnostic is a
/// bug even though it wouldn't panic. Checks both the `Display` text and the
/// structured `reason` field of every variant.
fn quarantine_error_has_reason(e: &QuarantineError) {
    assert!(
        !e.to_string().is_empty(),
        "QuarantineError Display must be non-empty: {e:?}"
    );
    let reason = match e {
        QuarantineError::ValidationFailed { reason }
        | QuarantineError::DeserializationFailed { reason }
        | QuarantineError::PredicateDenied { reason }
        | QuarantineError::Malformed { reason } => reason,
    };
    assert!(
        !reason.trim().is_empty(),
        "QuarantineError reason must be non-empty: {e:?}"
    );
}

proptest! {
    #![proptest_config(cfg())]

    /// Arbitrary UTF-8 strings into a permissive (`serde_json::Value`) schema:
    /// never panics; every `Err` carries a reason.
    #[test]
    fn quarantine_value_schema_never_panics(s in ".*") {
        let q = RiceQuarantine::new(JsonBoundarySchema::<serde_json::Value>::new());
        if let Err(e) = q.admit(&s) {
            quarantine_error_has_reason(&e);
        }
    }

    /// Arbitrary *bytes* rendered as lossy UTF-8 (covers embedded NULs, control
    /// bytes, invalid sequences replaced by U+FFFD): never panics.
    #[test]
    fn quarantine_never_panics_on_arbitrary_bytes(bytes in proptest::collection::vec(any::<u8>(), 0..512)) {
        let s = String::from_utf8_lossy(&bytes);
        let q = RiceQuarantine::new(JsonBoundarySchema::<serde_json::Value>::new());
        if let Err(e) = q.admit(&s) {
            quarantine_error_has_reason(&e);
        }
    }

    /// JSON-biased strings (structural punctuation + alnum) so proptest spends
    /// its budget near the parse boundary rather than on pure noise.
    #[test]
    fn quarantine_never_panics_on_json_like(s in r#"[{}\[\]"a-zA-Z0-9:,._ +-]{0,256}"#) {
        let q = RiceQuarantine::new(JsonBoundarySchema::<serde_json::Value>::new());
        if let Err(e) = q.admit(&s) {
            quarantine_error_has_reason(&e);
        }
    }

    /// Typed schema with a domain predicate: arbitrary strings must never panic
    /// through deserialize + coercion into `TypedPayload` + predicate eval.
    #[test]
    fn quarantine_typed_predicate_never_panics(s in r#"[{}\[\]"a-zA-Z0-9:,._ +-]{0,256}"#) {
        let schema = JsonBoundarySchema::<TypedPayload, _>::with_predicate(
            |p: &TypedPayload| !p.id.is_empty() && p.action != "forbidden",
        );
        let q = RiceQuarantine::new(schema);
        if let Err(e) = q.admit(&s) {
            quarantine_error_has_reason(&e);
        }
    }

    /// A well-formed JSON object that matches `TypedPayload` must round-trip
    /// through the typed quarantine to `Ok` (predicate accepts non-empty id /
    /// non-"forbidden" action) — proves the boundary isn't merely rejecting
    /// everything, so the never-panic properties above have real signal.
    #[test]
    fn quarantine_typed_admits_wellformed(
        id in "[a-zA-Z0-9]{1,16}",
        action in "[a-z]{1,16}",
    ) {
        prop_assume!(action != "forbidden");
        let schema = JsonBoundarySchema::<TypedPayload, _>::with_predicate(
            |p: &TypedPayload| !p.id.is_empty() && p.action != "forbidden",
        );
        let q = RiceQuarantine::new(schema);
        let obs = format!(r#"{{"id":"{id}","action":"{action}"}}"#);
        let admitted = q.admit(&obs);
        prop_assert!(admitted.is_ok(), "well-formed payload must admit: {admitted:?}");
    }

    /// Empty/whitespace observations are always a `Malformed` refusal, never a
    /// panic and never a silent accept.
    #[test]
    fn quarantine_rejects_blank(s in "[ \t\r\n]{0,32}") {
        let q = RiceQuarantine::new(JsonBoundarySchema::<serde_json::Value>::new());
        let r = q.admit(&s);
        prop_assert!(matches!(r, Err(QuarantineError::Malformed { .. })), "blank must be Malformed: {r:?}");
    }

    /// The receipt hex-decode boundary: arbitrary strings in every hex field
    /// must never panic — a non-64-hex value is an `Err`, never an unwind.
    #[test]
    fn recompute_chain_hash_never_panics_on_arbitrary_hex(
        payload_hex in ".{0,80}",
        prev_hex in ".{0,80}",
    ) {
        let record = ReceiptRecord {
            version: RECEIPT_RECORD_VERSION,
            instruction_id: 1,
            activity_idx: 0,
            activity: None,
            node_kind: 0,
            ts_ns: 1,
            duration_ms: None,
            payload_hash_hex: payload_hex,
            prev_chain_hash_hex: prev_hex,
            chain_hash_hex: String::new(),
            andon: Andon::Green,
            obligation_count: 0,
            object_ids: vec![],
            signature_hex: None,
        };
        // Must resolve, never unwind. (Ok only when both fields are valid 64-hex.)
        let _ = record.recompute_chain_hash();
    }

    /// The staged validator on an *arbitrary* ledger: never panics, always
    /// yields a `Verdict`, and `ok` implies every stage passed or was skipped
    /// (never `ok` with a `Fail` stage hiding in the report).
    #[test]
    fn validate_never_panics_on_arbitrary_records(
        records in proptest::collection::vec(arb_record(), 0..8),
    ) {
        let verdict = ReceiptValidator::validate(&records, &FixedClock(u64::MAX));
        prop_assert_eq!(verdict.records_checked, records.len());
        if verdict.ok {
            for stage in &verdict.stages {
                prop_assert!(
                    stage.outcome.is_pass()
                        || matches!(stage.outcome, praxis_core::receipt_validator::CheckOutcome::Skip(_)),
                    "ok verdict must not contain a Fail stage: {:?}", stage
                );
            }
        }
    }
}

/// An arbitrary, mostly-shaped-but-untrusted `ReceiptRecord`: version, ids and
/// timestamps range freely; hex fields are drawn from a hex-ish alphabet with a
/// loose length so both valid-64-hex and malformed values occur.
fn arb_record() -> impl Strategy<Value = ReceiptRecord> {
    (
        any::<u32>(),
        any::<u64>(),
        any::<u16>(),
        any::<u8>(),
        any::<u64>(),
        "[0-9a-fA-Fx]{0,72}",
        "[0-9a-fA-Fx]{0,72}",
        "[0-9a-fA-Fx]{0,72}",
        any::<u32>(),
    )
        .prop_map(
            |(version, instruction_id, activity_idx, node_kind, ts_ns, ph, pph, ch, obl)| {
                ReceiptRecord {
                    version,
                    instruction_id,
                    activity_idx,
                    activity: None,
                    node_kind,
                    ts_ns,
                    duration_ms: None,
                    payload_hash_hex: ph,
                    prev_chain_hash_hex: pph,
                    chain_hash_hex: ch,
                    andon: Andon::Green,
                    obligation_count: obl,
                    object_ids: vec![],
                    signature_hex: None,
                }
            },
        )
}
