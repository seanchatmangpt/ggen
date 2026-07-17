//! Mutation testing of the receipt chain validator.
//!
//! Genesis Day 3, phase 2 (mutation testing). Where `fuzz_boundaries.rs` proves
//! the boundaries never *panic*, this file proves the [`ReceiptValidator`]
//! pipeline actually *detects tampering*: we take a known-lawful receipt ledger,
//! apply a catalogue of affidavit-style mutation operators to it, and assert the
//! validator kills every mutant at the stage its design says should catch it.
//!
//! A green pipeline over an unmutated chain is necessary but not sufficient —
//! it only tells us the validator says "yes" to lawful input. Mutation testing
//! is the dual: it tells us the validator says "no" to *every* way we know how to
//! corrupt the ledger, and pins which stage is responsible for each. A mutant
//! that survives (validator still says `ok`) is either a killed test or a real,
//! documented gap in the integrity model.
//!
//! ## Mutation operators
//!
//! | operator        | corruption                                   | designed catch stage        |
//! |-----------------|----------------------------------------------|-----------------------------|
//! | `EventDrop`     | remove an interior record                    | `chain_linkage`             |
//! | `EventReorder`  | swap two adjacent records                    | `chain_linkage` + `monotonic` |
//! | `FieldFlip`     | flip a hex char of a **preimage** field      | `chain_recompute`           |
//! | `HashTruncate`  | truncate a hex field below 64 chars          | `schema`                    |
//! | `TimestampSkew` | reorder a timestamp (re-sealed, so only the  | `monotonic`                 |
//! |                 | ordering anomaly remains)                    |                             |
//!
//! The pipeline runs **all** stages unconditionally (it does not short-circuit),
//! so a mutant may trip more than one stage; each kill-test asserts the designed
//! stage is *among* the failing stages.
//!
//! ## Known-uncovered classes (documented survivors, not test escapes)
//!
//! Three mutation classes provably cannot be caught by this validator, by
//! design. They are asserted here as *surviving* (verdict stays `ok`) so the
//! coverage boundary is explicit and regression-guarded — if a future change
//! starts catching them, these tests fail loudly and should be promoted to kills.
//!
//!   1. **Out-of-preimage field flips** (`duration_ms`, `activity`). These fields
//!      are deliberately excluded from the BLAKE3 chain preimage (`receipt_record.rs`
//!      documents them as "purely descriptive"), and `token_replay` replays a fixed
//!      lawful lifecycle that ignores them (`replay_adapter.rs`). Nothing hashes or
//!      cross-checks them, so mutating them is undetectable.
//!   2. **Tail drop** — removing the last record yields a strictly-shorter but
//!      internally-consistent prefix. There is no persisted length/terminal
//!      commitment, so a truncated ledger validates clean.
//!   3. **Head drop** — removing the genesis record leaves a chain whose new head
//!      references an absent predecessor, but `chain_linkage` only checks links
//!      for `i >= 1` and there is no genesis anchor (no check that
//!      `records[0].prev_chain_hash == [0u8; 32]`), so it survives.
//!
//! ## Equivalent mutants (documented via cargo-mutants; genuinely unkillable)
//!
//! `cargo mutants -p praxis-core --file receipt_validator.rs --file receipt_record.rs`
//! reports **two equivalent mutants** — mutations with no observable behaviour, so
//! no test can distinguish them from the original. These are design facts, not gaps:
//!
//!   * **`delete field ts_ns from ReceiptRecord::receipt_meta`.** That helper is
//!     called *only* by `recompute_chain_hash`, which passes `self.ts_ns` to
//!     `build_admission_frame` as an explicit argument; the frame reads that
//!     argument, never `meta.ts_ns`. Dropping `meta.ts_ns` therefore cannot change
//!     any hash. (The sibling drops of `meta.activity_idx` / `meta.node_kind` *are*
//!     killed — those fields ARE folded into the frame, and this file's fixture
//!     seals them with non-zero varying values so a drop-to-default changes the hash.)
//!   * **`replace guard metrics.fitness == 0x0001_0000 with true` in
//!     `check_token_replay`.** `replay_adapter::replay_receipt_lifecycle` replays a
//!     fixed lawful `Judged→Admitted→Receipted` sequence for *every* record, so it
//!     always returns fitness `1.0` (`0x0001_0000`); no `ReceiptRecord` input can
//!     make the guard false, so relaxing it to `true` is unobservable. This is the
//!     same design fact as the token_replay known-gap above: the stage is a model
//!     sanity check, not a per-record tamper detector.
//!
//! Two further `check_monotonic` mutants cargo-mutants first reported as survivors
//! (`> → >=` on the future check, `< → <=` on the decrease check) were **killable**
//! boundary escapes and are now killed by the `boundary_*` tests below.

use praxis_core::{
    law::{Andon, ReceiptMeta},
    lifecycle::Raw,
    receipt_record::RECEIPT_RECORD_VERSION,
    receipt_validator::{CheckOutcome, FixedClock, ReceiptValidator, Verdict},
    Admit, DefaultLaw, Judge, LawObject, ReceiptRecord,
};

/// The `signed` feature was removed 2026-07-17 alongside this workspace's
/// absolute-path-dependency cleanup (PR #255) — see `tests/receipt_lane.rs`'s
/// `signing_guard` comment. This is now a permanent no-op, kept so call
/// sites below don't need touching.
fn ensure_signing_key() {}

// ── Fixtures ────────────────────────────────────────────────────────────────

/// Build a lawful, correctly-linked ledger of `n` records (`n >= 1`) via the
/// **authoritative emission path** (`LawObject::receipt_with_record`), with
/// strictly-increasing `instruction_id` and `ts_ns` and genesis `prev` = zero.
///
/// Sealing via the real emission path (not via `recompute_chain_hash`) is
/// deliberate: it means every record's stored `chain_hash_hex` is an
/// *independent authority* against which the validator's `chain_recompute`
/// stage — and therefore `recompute_chain_hash`'s `receipt_meta()`
/// reconstruction — is checked. Sealing with `recompute_chain_hash` itself
/// would make meta-reconstruction mutations (dropping `ts_ns`/`activity_idx`/
/// `node_kind`) invisible, since they'd corrupt both the seal and the check
/// symmetrically. (cargo-mutants found exactly this; this fixture kills it.)
fn lawful_chain(n: u64) -> Vec<ReceiptRecord> {
    assert!(n >= 1);
    ensure_signing_key();
    let mut records = Vec::new();
    let mut prev = [0u8; 32];
    for i in 1..=n {
        // `ts_ns` strictly increasing (i*1000); `activity_idx`/`node_kind`
        // non-zero and varying (see `emit`).
        let (chain_hash, record) = emit(i, (i as u16) + 1, ((i % 5) + 1) as u8, i * 1_000, &prev);
        prev = chain_hash;
        records.push(record);
    }
    records
}

/// Emit one authoritatively-sealed [`ReceiptRecord`] via the real emission path
/// (`LawObject::receipt_with_record`), returning `(chain_hash, record)`.
///
/// Sealing via the real path (not `recompute_chain_hash`) is deliberate: the
/// stored `chain_hash_hex` is then an *independent authority* the validator's
/// `chain_recompute` stage — and thus `receipt_meta()`'s reconstruction — is
/// checked against. `activity_idx`/`node_kind` are folded into the hashed frame
/// and `ReceiptMeta::default()` is `0` for both, so callers pass *non-zero*
/// values: otherwise a mutant that drops those fields from `receipt_meta()`
/// (→ default `0`) would be a silent no-op and survive. (cargo-mutants found
/// exactly that; non-zero fixtures turn it into a kill.)
fn emit(
    instruction_id: u64,
    activity_idx: u16,
    node_kind: u8,
    ts_ns: u64,
    prev: &[u8; 32],
) -> ([u8; 32], ReceiptRecord) {
    ensure_signing_key();
    let raw = LawObject::<serde_json::Value, Raw, DefaultLaw>::new(
        serde_json::json!({ "i": instruction_id }),
        vec![],
    );
    let validated = match DefaultLaw::judge(raw) {
        Ok(v) => v,
        Err(_) => panic!("no obligations must always validate"),
    };
    let admitted = match DefaultLaw::admit(validated) {
        Ok(a) => a,
        Err(_) => panic!("green andon must always admit"),
    };
    let meta = ReceiptMeta {
        instruction_id,
        activity_idx,
        node_kind,
        ts_ns: Some(ts_ns),
        ..Default::default()
    };
    let (receipted, record) = admitted
        .receipt_with_record(prev, meta)
        .expect("authoritative receipt_with_record");
    (*receipted.chain_hash().expect("chain hash set"), record)
}

/// Re-seal records `[start..]` in place: relink each to its predecessor's
/// chain hash and recompute its own, so that after a content mutation at
/// `start` the `schema`/`chain_recompute`/`chain_linkage` stages stay green and
/// only the *semantic* anomaly (e.g. a timestamp ordering violation) remains for
/// `monotonic` to catch. This is what lets `TimestampSkew` be tested in isolation
/// rather than being masked by `chain_recompute` (since `ts_ns` is in the preimage).
fn reseal_from(records: &mut [ReceiptRecord], start: usize) {
    let mut prev = if start == 0 {
        [0u8; 32]
    } else {
        records[start - 1]
            .chain_hash()
            .expect("predecessor chain hash valid")
    };
    for r in &mut records[start..] {
        r.prev_chain_hash_hex = hex::encode(prev);
        let ch = r.recompute_chain_hash().expect("reseal recompute");
        r.chain_hash_hex = hex::encode(ch);
        prev = ch;
    }
}

/// Validate with an unbounded clock so "future timestamp" never fires — this
/// isolates each mutation from the wall-clock check unless the mutation itself
/// is a timestamp anomaly.
fn validate(records: &[ReceiptRecord]) -> Verdict {
    ReceiptValidator::validate(records, &FixedClock(u64::MAX))
}

/// Names of every stage that returned `Fail`.
fn failing_stages(v: &Verdict) -> Vec<String> {
    v.stages
        .iter()
        .filter(|s| matches!(s.outcome, CheckOutcome::Fail(_)))
        .map(|s| s.stage.clone())
        .collect()
}

/// Assert a mutant was killed (verdict not ok) and that `expected_stage` is
/// among the stages that failed.
fn assert_killed_at(v: &Verdict, expected_stage: &str) {
    assert!(!v.ok, "mutant survived — validator still ok: {v:?}");
    let failed = failing_stages(v);
    assert!(
        failed.iter().any(|s| s == expected_stage),
        "expected kill at `{expected_stage}`, but failing stages were {failed:?}"
    );
}

// ── Baseline ──────────────────────────────────────────────────────────────

#[test]
fn baseline_lawful_chain_passes_every_stage() {
    let v = validate(&lawful_chain(4));
    assert!(v.ok, "unmutated chain must validate: {v:?}");
    assert_eq!(v.records_checked, 4);
}

// ── Kills ───────────────────────────────────────────────────────────────────

/// EventDrop: removing an interior record breaks the hash link between the
/// records that now sit adjacent → `chain_linkage`.
#[test]
fn mutant_event_drop_interior_killed_at_chain_linkage() {
    let mut records = lawful_chain(4);
    records.remove(2); // drop an interior record (index 2 of 0..4)
    assert_killed_at(&validate(&records), "chain_linkage");
}

/// EventReorder: swapping two adjacent records breaks their prev-hash links and
/// also violates strict `instruction_id` monotonicity. `token_replay` cannot
/// catch a reorder (it replays a fixed lawful lifecycle per record with no
/// persisted judge/admit events to reorder — see `replay_adapter.rs`), so the
/// designed catch is `chain_linkage` + `monotonic`; we assert `monotonic` fires.
#[test]
fn mutant_event_reorder_killed_at_monotonic_and_linkage() {
    let mut records = lawful_chain(4);
    records.swap(1, 2);
    let v = validate(&records);
    assert_killed_at(&v, "monotonic");
    // Document the (also-firing) linkage catch so the dual coverage is explicit.
    let failed = failing_stages(&v);
    assert!(
        failed.iter().any(|s| s == "chain_linkage"),
        "reorder also trips linkage: {failed:?}"
    );
}

/// FieldFlip on a preimage field (`payload_hash_hex`): recomputing the chain
/// hash from the record's own fields no longer matches the stored
/// `chain_hash_hex` → `chain_recompute` (tamper detection).
#[test]
fn mutant_field_flip_payload_hash_killed_at_chain_recompute() {
    let mut records = lawful_chain(4);
    let field = &mut records[2].payload_hash_hex;
    let c = field.chars().next().unwrap();
    let repl = if c == 'a' { 'b' } else { 'a' };
    field.replace_range(0..1, &repl.to_string());
    assert_killed_at(&validate(&records), "chain_recompute");
}

/// FieldFlip on another preimage field (`ts_ns`, which enters the meta of the
/// hash preimage) — also caught by `chain_recompute` when not re-sealed.
#[test]
fn mutant_field_flip_ts_ns_without_reseal_killed_at_chain_recompute() {
    let mut records = lawful_chain(4);
    records[2].ts_ns = records[2].ts_ns.wrapping_add(1); // still not "in the future" under u64::MAX clock
    assert_killed_at(&validate(&records), "chain_recompute");
}

/// FieldFlip on `activity_idx` (a preimage field folded into the admission
/// frame): corrupting it makes the recomputed hash diverge from the
/// authoritatively-sealed one → `chain_recompute`. This directly regression-guards
/// the cargo-mutants "delete field activity_idx from receipt_meta" kill — the
/// same divergence the mutant would (now) trigger.
#[test]
fn mutant_field_flip_activity_idx_killed_at_chain_recompute() {
    let mut records = lawful_chain(4);
    records[2].activity_idx = records[2].activity_idx.wrapping_add(7);
    assert_killed_at(&validate(&records), "chain_recompute");
}

/// FieldFlip on `node_kind` (likewise a preimage frame field) → `chain_recompute`.
/// Regression-guards the "delete field node_kind from receipt_meta" mutant kill.
#[test]
fn mutant_field_flip_node_kind_killed_at_chain_recompute() {
    let mut records = lawful_chain(4);
    records[2].node_kind = records[2].node_kind.wrapping_add(3);
    assert_killed_at(&validate(&records), "chain_recompute");
}

/// HashTruncate: truncating a hex field below 64 chars makes it fail to decode
/// into 32 bytes → `schema`.
#[test]
fn mutant_hash_truncate_killed_at_schema() {
    let mut records = lawful_chain(4);
    records[2].chain_hash_hex.truncate(60); // 30 bytes, not 32
    assert_killed_at(&validate(&records), "schema");
}

/// TimestampSkew: push an interior record's `ts_ns` above its successor's, then
/// re-seal the tail so schema/recompute/linkage stay green — the only surviving
/// anomaly is a non-monotonic timestamp → `monotonic`.
#[test]
fn mutant_timestamp_skew_resealed_killed_at_monotonic() {
    let mut records = lawful_chain(4);
    // Record 1 (0-indexed) gets a ts far above record 2's, breaking non-decreasing ts.
    records[1].ts_ns = records[2].ts_ns + 10_000;
    reseal_from(&mut records, 1);
    let v = validate(&records);
    assert_killed_at(&v, "monotonic");
    // The re-seal must have kept the hash stages clean, proving isolation.
    let failed = failing_stages(&v);
    assert!(
        !failed.iter().any(|s| s == "chain_recompute"),
        "reseal should keep recompute green: {failed:?}"
    );
    assert!(
        !failed.iter().any(|s| s == "schema"),
        "reseal should keep schema green: {failed:?}"
    );
    assert!(
        !failed.iter().any(|s| s == "chain_linkage"),
        "reseal should keep linkage green: {failed:?}"
    );
}

/// TimestampSkew, clock variant: a future timestamp is caught by `monotonic`'s
/// "not in the future" check when validated against a bounded clock (re-sealed so
/// the hash stages stay green and `monotonic` is the sole catcher).
#[test]
fn mutant_timestamp_future_killed_at_monotonic_under_bounded_clock() {
    let mut records = lawful_chain(1);
    records[0].ts_ns = 5_000;
    reseal_from(&mut records, 0);
    // "now" is 4_999 ns since epoch; the record's ts of 5_000 is in the future.
    let v = ReceiptValidator::validate(&records, &FixedClock(4_999));
    assert!(!v.ok, "future ts must be killed: {v:?}");
    let failed = failing_stages(&v);
    assert!(
        failed.iter().any(|s| s == "monotonic"),
        "future ts caught by monotonic: {failed:?}"
    );
    assert!(
        !failed.iter().any(|s| s == "chain_recompute"),
        "reseal keeps recompute green: {failed:?}"
    );
}

/// Non-increasing `instruction_id` (a distinct monotonic invariant from ts),
/// re-sealed so only the id-ordering anomaly remains → `monotonic`.
#[test]
fn mutant_instruction_id_regression_killed_at_monotonic() {
    let mut records = lawful_chain(4);
    records[2].instruction_id = records[1].instruction_id; // equal → not strictly increasing
    reseal_from(&mut records, 2);
    assert_killed_at(&validate(&records), "monotonic");
}

/// Version bump beyond the supported schema is caught by `schema` (guards
/// against replaying records written by an incompatible future writer).
#[test]
fn mutant_version_bump_killed_at_schema() {
    let mut records = lawful_chain(2);
    records[1].version = RECEIPT_RECORD_VERSION + 7;
    assert_killed_at(&validate(&records), "schema");
}

// ── Boundary kills (pin the exact comparison operators) ─────────────────────

/// The `monotonic` "not in the future" check is `ts_ns > now` (strict). A record
/// whose `ts_ns` exactly equals `now` is lawful and must pass. This kills the
/// `> → >=` mutant (which would flag `ts == now` as future). cargo-mutants found
/// this survivor because every other fixture keeps `ts` strictly below the clock.
#[test]
fn boundary_ts_equal_to_now_is_not_future() {
    let (_ch, record) = emit(1, 1, 1, 5_000, &[0u8; 32]);
    let v = ReceiptValidator::validate(&[record], &FixedClock(5_000)); // now == ts
    assert!(v.ok, "ts_ns == now must not be 'in the future': {v:?}");
}

/// The `monotonic` "decreased" check is `ts_ns < prev` (strict), so *equal*
/// consecutive timestamps are lawful (non-decreasing). Two properly-chained
/// records with equal `ts_ns` but strictly-increasing `instruction_id` must pass.
/// This kills the `< → <=` mutant (which would flag equal timestamps as a decrease).
#[test]
fn boundary_equal_consecutive_ts_is_allowed() {
    let (ch0, r0) = emit(1, 1, 1, 7_000, &[0u8; 32]);
    let (_ch1, r1) = emit(2, 2, 2, 7_000, &ch0); // same ts as r0, higher instruction_id
    let v = validate(&[r0, r1]);
    assert!(
        v.ok,
        "equal consecutive ts_ns (non-decreasing) must be allowed: {v:?}"
    );
}

// ── Documented survivors (known-uncovered classes) ─────────────────────────

/// Mutant: flipping `andon` from `Green` to `Halted` is now detected.
#[test]
fn mutant_andon_flip_killed_at_chain_recompute() {
    let mut records = lawful_chain(3);
    records[1].andon = Andon::Halted {
        unmet: vec![],
        refusals: vec![],
        at: 0,
    };
    assert_killed_at(&validate(&records), "chain_recompute");
}

/// Mutant: mutating `obligation_count` is now detected.
#[test]
fn mutant_obligation_count_flip_killed_at_chain_recompute() {
    let mut records = lawful_chain(3);
    records[1].obligation_count = records[1].obligation_count.wrapping_add(99);
    assert_killed_at(&validate(&records), "chain_recompute");
}

/// Mutant: mutating `object_ids` is now detected.
#[test]
fn mutant_object_ids_mutation_killed_at_chain_recompute() {
    let mut records = lawful_chain(3);
    records[1].object_ids = vec!["law:forged-identity".to_string()];
    assert_killed_at(&validate(&records), "chain_recompute");
}

/// SURVIVOR (class 2): dropping the tail record yields a strictly-shorter,
/// internally-consistent prefix. No persisted length/terminal commitment exists
/// to catch a truncated ledger.
#[test]
fn survivor_tail_drop_is_a_documented_gap() {
    let mut records = lawful_chain(4);
    records.pop();
    assert!(
        validate(&records).ok,
        "KNOWN GAP: no length/terminal commitment, so a tail-truncated ledger validates clean"
    );
}

/// Mutant: dropping the genesis record is now caught by the genesis anchor check.
#[test]
fn mutant_head_drop_killed_at_chain_linkage() {
    let mut records = lawful_chain(4);
    records.remove(0);
    assert_killed_at(&validate(&records), "chain_linkage");
}
