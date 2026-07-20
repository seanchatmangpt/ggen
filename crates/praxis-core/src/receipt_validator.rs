//! `ReceiptValidator` — staged, replayable verification of a receipt ledger.
//!
//! Runs a fixed pipeline of independent checks over a slice of
//! [`ReceiptRecord`]s and produces a [`Verdict`] with a per-stage
//! [`CheckOutcome`], rather than short-circuiting on the first failure — this
//! mirrors `crate::verify`'s "run every stage, report all of them" shape.
//!
//! Stages, in order: `schema`, `chain_recompute` (tamper detection),
//! `chain_linkage`, `monotonic` (uses an injectable [`Clock`] so "not in the
//! future" checks are deterministic in tests), and `token_replay` (POWL
//! lifecycle conformance via [`crate::replay_adapter`]).

use std::time::{SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};

use crate::{receipt_record::ReceiptRecord, replay_adapter};

/// Injectable wall-clock so "timestamp not in the future" checks are
/// deterministic in tests.
pub trait Clock: Sync {
    /// Current time in nanoseconds since the UNIX epoch.
    fn now_ns(&self) -> u64;
}

/// Real wall-clock via `SystemTime::now()`.
#[derive(Debug, Default, Clone, Copy)]
pub struct SystemClock;

impl Clock for SystemClock {
    fn now_ns(&self) -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as u64
    }
}

/// A fixed clock for deterministic tests: always reports the same `now_ns()`.
#[derive(Debug, Clone, Copy)]
pub struct FixedClock(pub u64);

impl Clock for FixedClock {
    fn now_ns(&self) -> u64 {
        self.0
    }
}

/// The outcome of a single validation stage.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CheckOutcome {
    /// The stage found no issues.
    Pass,
    /// The stage found a problem, described in the message.
    Fail(String),
    /// The stage was not applicable (e.g. no records to check).
    Skip(String),
}

impl CheckOutcome {
    /// `true` iff this outcome is [`CheckOutcome::Pass`].
    #[must_use]
    pub fn is_pass(&self) -> bool {
        matches!(self, CheckOutcome::Pass)
    }
}

/// One stage's name and outcome.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StageResult {
    /// Stage identifier (`"schema"`, `"chain_recompute"`, `"chain_linkage"`,
    /// `"monotonic"`, `"token_replay"`).
    pub stage: String,
    /// The stage's outcome.
    pub outcome: CheckOutcome,
}

impl StageResult {
    fn new(stage: &'static str, outcome: CheckOutcome) -> Self {
        Self {
            stage: stage.to_string(),
            outcome,
        }
    }
}

/// The full validation result: every stage's outcome plus an overall verdict.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Verdict {
    /// `true` iff every stage passed (or was skipped).
    pub ok: bool,
    /// Per-stage results, in pipeline order.
    pub stages: Vec<StageResult>,
    /// Number of records validated.
    pub records_checked: usize,
}

/// Runs the staged validation pipeline over a receipt ledger.
pub struct ReceiptValidator;

impl ReceiptValidator {
    /// Validate `records` against every stage, using `clock` for the
    /// `monotonic` stage's "not in the future" check.
    #[must_use]
    pub fn validate(records: &[ReceiptRecord], clock: &dyn Clock) -> Verdict {
        let (s1, s2, s3, s4, s5) = std::thread::scope(|s| {
            let t1 = s.spawn(|| Self::check_schema(records));
            let t2 = s.spawn(|| Self::check_chain_recompute(records));
            let t3 = s.spawn(|| Self::check_chain_linkage(records));
            let t4 = s.spawn(|| Self::check_monotonic(records, clock));
            let t5 = s.spawn(|| Self::check_token_replay(records));
            (
                t1.join().unwrap(),
                t2.join().unwrap(),
                t3.join().unwrap(),
                t4.join().unwrap(),
                t5.join().unwrap(),
            )
        });
        let stages = vec![s1, s2, s3, s4, s5];
        let ok = stages
            .iter()
            .all(|s| s.outcome.is_pass() || matches!(s.outcome, CheckOutcome::Skip(_)));
        Verdict {
            ok,
            stages,
            records_checked: records.len(),
        }
    }

    fn check_schema(records: &[ReceiptRecord]) -> StageResult {
        for (i, record) in records.iter().enumerate() {
            if record.version != crate::receipt_record::RECEIPT_RECORD_VERSION {
                return StageResult::new(
                    "schema",
                    CheckOutcome::Fail(format!(
                        "record {i}: unsupported schema version {} (expected {})",
                        record.version,
                        crate::receipt_record::RECEIPT_RECORD_VERSION
                    )),
                );
            }
            if let Err(e) = record.payload_hash() {
                return StageResult::new("schema", CheckOutcome::Fail(format!("record {i}: {e}")));
            }
            if let Err(e) = record.prev_chain_hash() {
                return StageResult::new("schema", CheckOutcome::Fail(format!("record {i}: {e}")));
            }
            if let Err(e) = record.chain_hash() {
                return StageResult::new("schema", CheckOutcome::Fail(format!("record {i}: {e}")));
            }
        }
        StageResult::new("schema", CheckOutcome::Pass)
    }

    fn check_chain_recompute(records: &[ReceiptRecord]) -> StageResult {
        for (i, record) in records.iter().enumerate() {
            let claimed = match record.chain_hash() {
                Ok(h) => h,
                Err(_) => continue, // already reported by `schema`
            };
            match record.recompute_chain_hash() {
                Ok(computed) if computed == claimed => {}
                Ok(_) => {
                    return StageResult::new(
                        "chain_recompute",
                        CheckOutcome::Fail(format!(
                            "record {i}: recomputed chain hash does not match stored chain_hash_hex (tamper detected)"
                        )),
                    );
                }
                Err(e) => {
                    return StageResult::new(
                        "chain_recompute",
                        CheckOutcome::Fail(format!("record {i}: {e}")),
                    );
                }
            }
        }
        StageResult::new("chain_recompute", CheckOutcome::Pass)
    }

    fn check_chain_linkage(records: &[ReceiptRecord]) -> StageResult {
        if !records.is_empty() {
            let genesis_hex = hex::encode(crate::receipt_store::GENESIS_CHAIN_HASH);
            if records[0].prev_chain_hash_hex != genesis_hex {
                return StageResult::new(
                    "chain_linkage",
                    CheckOutcome::Fail(format!(
                        "record 0: prev_chain_hash_hex ({}) does not match genesis anchor ({})",
                        records[0].prev_chain_hash_hex, genesis_hex
                    )),
                );
            }
        }
        for i in 1..records.len() {
            let expected_prev = &records[i - 1].chain_hash_hex;
            let actual_prev = &records[i].prev_chain_hash_hex;
            if expected_prev != actual_prev {
                return StageResult::new(
                    "chain_linkage",
                    CheckOutcome::Fail(format!(
                        "record {i}: prev_chain_hash_hex ({actual_prev}) != record {}'s chain_hash_hex ({expected_prev})",
                        i - 1
                    )),
                );
            }
        }
        StageResult::new("chain_linkage", CheckOutcome::Pass)
    }

    fn check_monotonic(records: &[ReceiptRecord], clock: &dyn Clock) -> StageResult {
        let now = clock.now_ns();
        let mut prev_instruction: Option<u64> = None;
        let mut prev_ts: Option<u64> = None;

        for (i, record) in records.iter().enumerate() {
            if record.ts_ns > now {
                return StageResult::new(
                    "monotonic",
                    CheckOutcome::Fail(format!(
                        "record {i}: ts_ns ({}) is in the future (now={now})",
                        record.ts_ns
                    )),
                );
            }
            if let Some(prev) = prev_instruction {
                if record.instruction_id <= prev {
                    return StageResult::new(
                        "monotonic",
                        CheckOutcome::Fail(format!(
                            "record {i}: instruction_id ({}) not strictly increasing after {prev}",
                            record.instruction_id
                        )),
                    );
                }
            }
            if let Some(prev) = prev_ts {
                if record.ts_ns < prev {
                    return StageResult::new(
                        "monotonic",
                        CheckOutcome::Fail(format!(
                            "record {i}: ts_ns ({}) decreased from previous record's {prev}",
                            record.ts_ns
                        )),
                    );
                }
            }
            prev_instruction = Some(record.instruction_id);
            prev_ts = Some(record.ts_ns);
        }
        StageResult::new("monotonic", CheckOutcome::Pass)
    }

    fn check_token_replay(records: &[ReceiptRecord]) -> StageResult {
        if records.is_empty() {
            return StageResult::new(
                "token_replay",
                CheckOutcome::Skip("no records to replay".to_string()),
            );
        }
        for (i, record) in records.iter().enumerate() {
            match replay_adapter::replay_receipt_lifecycle(record) {
                Ok(metrics) if metrics.fitness == 0x0001_0000 => {}
                Ok(metrics) => {
                    return StageResult::new(
                        "token_replay",
                        CheckOutcome::Fail(format!(
                            "record {i}: fitness {:#010x} != 1.0 (0x00010000)",
                            metrics.fitness
                        )),
                    );
                }
                Err(e) => {
                    return StageResult::new(
                        "token_replay",
                        CheckOutcome::Fail(format!("record {i}: replay violation {e:?}")),
                    );
                }
            }
        }
        StageResult::new("token_replay", CheckOutcome::Pass)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::law::Andon;

    fn chained_records(n: u64) -> Vec<ReceiptRecord> {
        let mut records = Vec::new();
        let mut prev = [0u8; 32];
        for i in 1..=n {
            let payload_hash_hex = format!("{i:02x}").repeat(32)[..64].to_string();
            let mut record = ReceiptRecord {
                version: crate::receipt_record::RECEIPT_RECORD_VERSION,
                instruction_id: i,
                activity_idx: 0,
                activity: None,
                node_kind: 0,
                ts_ns: i * 1000,
                duration_ms: None,
                payload_hash_hex,
                prev_chain_hash_hex: hex::encode(prev),
                chain_hash_hex: String::new(),
                andon: Andon::Green,
                obligation_count: 0,
                object_ids: vec![format!("law:instr{i}")],
                signature_hex: None,
                schema: crate::receipt_epoch::SCHEMA_V1.to_string(),
                v2: None,
            };
            let chain_hash = record.recompute_chain_hash().expect("recompute");
            record.chain_hash_hex = hex::encode(chain_hash);
            prev = chain_hash;
            records.push(record);
        }
        records
    }

    #[test]
    fn lawful_chain_passes_every_stage() {
        let records = chained_records(3);
        let verdict = ReceiptValidator::validate(&records, &FixedClock(u64::MAX));
        assert!(verdict.ok, "verdict: {verdict:?}");
        assert_eq!(verdict.records_checked, 3);
        for stage in &verdict.stages {
            assert!(stage.outcome.is_pass() || matches!(stage.outcome, CheckOutcome::Skip(_)));
        }
    }

    #[test]
    fn tampered_payload_hash_fails_chain_recompute() {
        let mut records = chained_records(3);
        // Flip one hex character of the middle record's payload hash.
        let c = records[1].payload_hash_hex.chars().next().unwrap();
        let replacement = if c == 'a' { 'b' } else { 'a' };
        records[1]
            .payload_hash_hex
            .replace_range(0..1, &replacement.to_string());

        let verdict = ReceiptValidator::validate(&records, &FixedClock(u64::MAX));
        assert!(!verdict.ok);
        let stage = verdict
            .stages
            .iter()
            .find(|s| s.stage == "chain_recompute")
            .unwrap();
        assert!(matches!(stage.outcome, CheckOutcome::Fail(_)));
    }

    #[test]
    fn broken_linkage_fails_chain_linkage() {
        let mut records = chained_records(3);
        records.swap(1, 2);
        let verdict = ReceiptValidator::validate(&records, &FixedClock(u64::MAX));
        assert!(!verdict.ok);
        let stage = verdict
            .stages
            .iter()
            .find(|s| s.stage == "chain_linkage")
            .unwrap();
        assert!(matches!(stage.outcome, CheckOutcome::Fail(_)));
    }

    #[test]
    fn non_increasing_instruction_id_fails_monotonic() {
        let mut records = chained_records(3);
        records[2].instruction_id = records[1].instruction_id;
        let verdict = ReceiptValidator::validate(&records, &FixedClock(u64::MAX));
        assert!(!verdict.ok);
        let stage = verdict
            .stages
            .iter()
            .find(|s| s.stage == "monotonic")
            .unwrap();
        assert!(matches!(stage.outcome, CheckOutcome::Fail(_)));
    }

    #[test]
    fn future_timestamp_fails_monotonic_under_fixed_clock() {
        let records = chained_records(1);
        // FixedClock(0) means "now" is the epoch; any record's ts_ns > 0 is "in the future".
        let verdict = ReceiptValidator::validate(&records, &FixedClock(0));
        assert!(!verdict.ok);
        let stage = verdict
            .stages
            .iter()
            .find(|s| s.stage == "monotonic")
            .unwrap();
        assert!(matches!(stage.outcome, CheckOutcome::Fail(_)));
    }

    #[test]
    fn empty_ledger_passes_with_skipped_token_replay() {
        let verdict = ReceiptValidator::validate(&[], &SystemClock);
        assert!(verdict.ok);
        let stage = verdict
            .stages
            .iter()
            .find(|s| s.stage == "token_replay")
            .unwrap();
        assert!(matches!(stage.outcome, CheckOutcome::Skip(_)));
    }

    /// Documents (rather than tightly asserts, to avoid CI flakiness) the
    /// CPHY_ROADMAP's <5ms-per-batch validation target. `benches/
    /// receipt_validate.rs` (root crate) is the authoritative Criterion
    /// measurement; this is a cheap `#[test]`-time sanity check that also
    /// runs wherever `cargo test -p praxis-core` runs (no root-crate/full
    /// -workspace build required), asserting a generous 50ms ceiling — two
    /// orders of magnitude above the 5ms target — so it only fails on an
    /// actual regression, not benchmark noise.
    #[test]
    fn validate_1000_records_documents_sub_5ms_target() {
        let records = chained_records(1000);
        let start = std::time::Instant::now();
        let verdict = ReceiptValidator::validate(&records, &SystemClock);
        let elapsed = start.elapsed();
        assert!(verdict.ok, "verdict: {verdict:?}");
        eprintln!(
            "[receipt_validator] validate() over {} records took {:?} (<5ms target; 50ms ceiling)",
            records.len(),
            elapsed
        );
        assert!(
            elapsed.as_millis() < 50,
            "validate() over 1000 records took {elapsed:?}, expected well under the 5ms target"
        );
    }
}
