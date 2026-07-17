//! `verify` pipeline metrics — RAII per-stage timing and pass/fail tracking.
//!
//! [`VerifyGuard`] is a RAII wrapper that records stage start/end timing and
//! pass/fail counts. On drop it finalizes the metrics into a [`VerifyMetrics`]
//! summary, which can be logged or returned to the caller.

use std::time::{Duration, Instant};

// ── Metrics types ─────────────────────────────────────────────────────────

/// Timing and outcome for a single verification stage.
#[derive(Debug, Clone)]
pub struct StageMetric {
    /// Stage name.
    pub name: String,
    /// Whether the stage passed.
    pub passed: bool,
    /// Wall-clock duration of the stage.
    pub duration: Duration,
}

/// Aggregate metrics for a complete verify run.
#[derive(Debug, Clone)]
pub struct VerifyMetrics {
    /// Per-stage breakdowns in pipeline order.
    pub stages: Vec<StageMetric>,
    /// Total wall-clock duration across all stages.
    pub total_duration: Duration,
    /// Number of stages that passed.
    pub passed_count: usize,
    /// Number of stages that failed.
    pub failed_count: usize,
}

impl VerifyMetrics {
    /// Returns the name of the first failing stage, if any.
    pub fn first_failure_stage(&self) -> Option<&str> {
        self.stages
            .iter()
            .find(|s| !s.passed)
            .map(|s| s.name.as_str())
    }

    /// One-line summary: `"7/7 passed in 1.23ms"` or `"REJECT at chain_integrity after 0.45ms"`.
    pub fn summary_line(&self) -> String {
        let total_ms = self.total_duration.as_secs_f64() * 1_000.0;
        if self.failed_count == 0 {
            let n = self.passed_count;
            format!("{n}/{n} passed in {total_ms:.2}ms")
        } else {
            let stage = self.first_failure_stage().unwrap_or("unknown");
            format!("REJECT at {stage} after {total_ms:.2}ms")
        }
    }
}

// ── RAII guard ────────────────────────────────────────────────────────────

/// RAII guard for a single verification stage.
///
/// Create via [`VerifyGuard::begin_stage`]; the stage is timed from
/// construction to the call to [`VerifyGuard::end_stage`].
///
/// # Example
///
/// ```rust,no_run
/// use praxis_core::verify::{VerifyGuard, VerifyMetrics};
///
/// let mut guard = VerifyGuard::new();
/// guard.begin_stage("decode");
/// // ... run stage logic ...
/// guard.end_stage(true);
/// guard.begin_stage("chain_integrity");
/// // ... run stage logic ...
/// guard.end_stage(true);
/// let metrics = guard.finish();
/// eprintln!("{}", metrics.summary_line());
/// ```
pub struct VerifyGuard {
    stages: Vec<StageMetric>,
    current_stage: Option<(String, Instant)>,
    overall_start: Instant,
}

impl VerifyGuard {
    /// Begin a new verify run.
    pub fn new() -> Self {
        VerifyGuard {
            stages: Vec::new(),
            current_stage: None,
            overall_start: Instant::now(),
        }
    }

    /// Begin timing a named stage. Panics if a stage is already in progress
    /// (caller must call `end_stage` before starting the next).
    pub fn begin_stage(&mut self, name: impl Into<String>) {
        assert!(
            self.current_stage.is_none(),
            "VerifyGuard: begin_stage called while a stage is already in progress"
        );
        self.current_stage = Some((name.into(), Instant::now()));
    }

    /// End the current stage with a pass/fail outcome.
    /// Returns the measured duration for this stage.
    pub fn end_stage(&mut self, passed: bool) -> Duration {
        let (name, start) = self
            .current_stage
            .take()
            .expect("VerifyGuard: end_stage called without a matching begin_stage");
        let duration = start.elapsed();
        self.stages.push(StageMetric {
            name,
            passed,
            duration,
        });
        duration
    }

    /// Consume the guard and return aggregated [`VerifyMetrics`].
    pub fn finish(mut self) -> VerifyMetrics {
        // If a stage was started but never ended, record it as failed.
        if let Some((name, start)) = self.current_stage.take() {
            self.stages.push(StageMetric {
                name,
                passed: false,
                duration: start.elapsed(),
            });
        }
        let total_duration = self.overall_start.elapsed();
        let passed_count = self.stages.iter().filter(|s| s.passed).count();
        let failed_count = self.stages.iter().filter(|s| !s.passed).count();
        VerifyMetrics {
            stages: self.stages,
            total_duration,
            passed_count,
            failed_count,
        }
    }
}

impl Default for VerifyGuard {
    fn default() -> Self {
        Self::new()
    }
}

// ── Affidavit-style verdict pipeline ────────────────────────────────────────
//
// Distinct from `crate::receipt_validator::ReceiptValidator`: that pipeline
// is concrete on `ReceiptRecord` and short-circuits nothing but doesn't time
// itself. This pipeline is generic over any [`ReceiptLike`] view of a
// receipt (so it never needs to know about `ReceiptRecord`'s concrete
// fields), is wrapped in [`VerifyGuard`] for per-stage timing, and produces a
// [`Verdict`] carrying a `profile` name — the shape the `verify` CLI verb
// returns as JSON.

use crate::error::CoreError;

/// Outcome of one verification stage (affidavit-style: recorded even when it
/// fails, never short-circuits the pipeline).
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CheckOutcome {
    /// Stage name (`"check_format"`, `"chain_integrity"`, `"continuity"`,
    /// `"verify_commitments"`, `"evaluate_profile"`).
    pub stage: String,
    /// Whether this stage passed.
    pub passed: bool,
    /// Human-readable detail: why it passed or failed.
    pub detail: String,
}

impl CheckOutcome {
    fn pass(stage: &str, detail: impl Into<String>) -> Self {
        CheckOutcome {
            stage: stage.to_string(),
            passed: true,
            detail: detail.into(),
        }
    }

    fn fail(stage: &str, detail: impl Into<String>) -> Self {
        CheckOutcome {
            stage: stage.to_string(),
            passed: false,
            detail: detail.into(),
        }
    }
}

/// Final verdict of the affidavit pipeline: every stage's outcome plus the
/// overall accept/reject call.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Verdict {
    /// `true` iff every stage passed.
    pub accepted: bool,
    /// The verification profile this verdict was evaluated against.
    pub profile: String,
    /// Every stage's outcome, in pipeline order. Never short-circuited: a
    /// failing stage still lets every later stage run so the verdict
    /// documents everything that was observed, not just the first problem.
    pub outcomes: Vec<CheckOutcome>,
    /// Detail of the first failing stage; `None` when `accepted`.
    pub reason: Option<String>,
}

/// A minimal, storage-agnostic view of a persisted receipt, decoupling this
/// pipeline from `crate::receipt_record::ReceiptRecord`'s concrete fields.
///
/// Implemented below for [`crate::receipt_record::ReceiptRecord`] (the
/// receipt-persistence lane's actual type) by delegating straight to its own
/// hex/hash accessors, so the two can never silently diverge.
pub trait ReceiptLike {
    /// Monotonically increasing step identity within a run.
    fn instruction_id(&self) -> u64;
    /// Wall-clock timestamp in nanoseconds.
    fn ts_ns(&self) -> u64;
    /// The claimed payload hash, as 64 lowercase hex characters.
    fn payload_hash_hex(&self) -> &str;
    /// The chain hash this record was chained onto, as 64 lowercase hex characters.
    fn prev_chain_hash_hex(&self) -> &str;
    /// The resulting chain hash after this record, as 64 lowercase hex characters.
    fn chain_hash_hex(&self) -> &str;
    /// Decode [`Self::prev_chain_hash_hex`] into raw bytes.
    fn prev_chain_hash(&self) -> Result<[u8; 32], CoreError>;
    /// Decode [`Self::chain_hash_hex`] into raw bytes.
    fn chain_hash(&self) -> Result<[u8; 32], CoreError>;
    /// Recompute the chain hash from this record's own fields, using the
    /// exact same construction the emission path used. A mismatch against
    /// [`Self::chain_hash`] means tampering (or an incompatible chain rule).
    fn recompute_chain_hash(&self) -> Result<[u8; 32], CoreError>;
}

impl ReceiptLike for crate::receipt_record::ReceiptRecord {
    fn instruction_id(&self) -> u64 {
        self.instruction_id
    }

    fn ts_ns(&self) -> u64 {
        self.ts_ns
    }

    fn payload_hash_hex(&self) -> &str {
        &self.payload_hash_hex
    }

    fn prev_chain_hash_hex(&self) -> &str {
        &self.prev_chain_hash_hex
    }

    fn chain_hash_hex(&self) -> &str {
        &self.chain_hash_hex
    }

    fn prev_chain_hash(&self) -> Result<[u8; 32], CoreError> {
        crate::receipt_record::ReceiptRecord::prev_chain_hash(self)
    }

    fn chain_hash(&self) -> Result<[u8; 32], CoreError> {
        crate::receipt_record::ReceiptRecord::chain_hash(self)
    }

    fn recompute_chain_hash(&self) -> Result<[u8; 32], CoreError> {
        crate::receipt_record::ReceiptRecord::recompute_chain_hash(self)
    }
}

/// `true` iff `s` is exactly 64 lowercase-or-digit hex characters.
fn is_hex64(s: &str) -> bool {
    s.len() == 64 && s.chars().all(|c| c.is_ascii_hexdigit())
}

/// Stage 1: every record's hex-encoded hash fields are present and
/// well-formed (64 hex characters). Purely structural — does not check
/// whether the hashes are internally consistent (that's later stages).
pub fn check_format<R: ReceiptLike>(records: &[R]) -> CheckOutcome {
    if records.is_empty() {
        return CheckOutcome::pass("check_format", "no records to check");
    }
    for (i, r) in records.iter().enumerate() {
        for (field, hex_str) in [
            ("payload_hash_hex", r.payload_hash_hex()),
            ("prev_chain_hash_hex", r.prev_chain_hash_hex()),
            ("chain_hash_hex", r.chain_hash_hex()),
        ] {
            if !is_hex64(hex_str) {
                return CheckOutcome::fail(
                    "check_format",
                    format!("record {i}: {field} is not 64 hex characters"),
                );
            }
        }
    }
    CheckOutcome::pass(
        "check_format",
        format!("{} record(s) well-formed", records.len()),
    )
}

/// Stage 2: recompute each record's chain hash from its own fields and
/// compare against the stored `chain_hash`. A mismatch is tamper detection.
pub fn chain_integrity<R: ReceiptLike>(records: &[R]) -> CheckOutcome {
    for (i, r) in records.iter().enumerate() {
        let claimed = match r.chain_hash() {
            Ok(h) => h,
            Err(e) => return CheckOutcome::fail("chain_integrity", format!("record {i}: {e}")),
        };
        match r.recompute_chain_hash() {
            Ok(computed) if computed == claimed => {}
            Ok(_) => {
                return CheckOutcome::fail(
                    "chain_integrity",
                    format!("record {i}: recomputed chain hash does not match stored chain_hash (tamper detected)"),
                );
            }
            Err(e) => {
                return CheckOutcome::fail("chain_integrity", format!("record {i}: {e}"));
            }
        }
    }
    CheckOutcome::pass(
        "chain_integrity",
        format!("{} record(s) recompute clean", records.len()),
    )
}

/// Stage 3: `instruction_id` strictly increasing and `ts_ns` monotonically
/// non-decreasing across the record sequence.
pub fn continuity<R: ReceiptLike>(records: &[R]) -> CheckOutcome {
    let mut prev_instruction: Option<u64> = None;
    let mut prev_ts: Option<u64> = None;
    for (i, r) in records.iter().enumerate() {
        if let Some(prev) = prev_instruction {
            if r.instruction_id() <= prev {
                return CheckOutcome::fail(
                    "continuity",
                    format!(
                        "record {i}: instruction_id ({}) not strictly increasing after {prev}",
                        r.instruction_id()
                    ),
                );
            }
        }
        if let Some(prev) = prev_ts {
            if r.ts_ns() < prev {
                return CheckOutcome::fail(
                    "continuity",
                    format!(
                        "record {i}: ts_ns ({}) decreased from previous record's {prev}",
                        r.ts_ns()
                    ),
                );
            }
        }
        prev_instruction = Some(r.instruction_id());
        prev_ts = Some(r.ts_ns());
    }
    CheckOutcome::pass(
        "continuity",
        format!("{} record(s) monotonic", records.len()),
    )
}

/// Stage 4: each record's `prev_chain_hash` commits to the previous record's
/// `chain_hash` — the ledger-linkage commitment. (`ReceiptRecord` persists
/// only the payload's *hash*, not its raw bytes, so recomputing
/// `payload_hash` from source bytes isn't possible from the ledger alone;
/// that half of "verify commitments" is covered by `chain_integrity`, which
/// recomputes the chain frame including `payload_hash`. This stage covers
/// the other half: that the chain of commitments is unbroken.)
pub fn verify_commitments<R: ReceiptLike>(records: &[R]) -> CheckOutcome {
    for i in 1..records.len() {
        let expected = records[i - 1].chain_hash_hex();
        let actual = records[i].prev_chain_hash_hex();
        if expected != actual {
            return CheckOutcome::fail(
                "verify_commitments",
                format!(
                    "record {i}: prev_chain_hash ({actual}) does not commit to record {}'s chain_hash ({expected})",
                    i - 1
                ),
            );
        }
    }
    CheckOutcome::pass(
        "verify_commitments",
        format!("{} record(s) linked", records.len()),
    )
}

/// Stage 5: extension point for named verification profiles. `"default"`
/// (and the empty string) requires every prior stage to have passed. Any
/// other profile name is rejected as unknown — add new profiles here as
/// they're needed rather than silently accepting unrecognized names.
pub fn evaluate_profile(profile: &str, prior: &[CheckOutcome]) -> CheckOutcome {
    match profile {
        "default" | "" => {
            if prior.iter().all(|o| o.passed) {
                CheckOutcome::pass(
                    "evaluate_profile",
                    format!(
                        "profile 'default': all {} prior stage(s) passed",
                        prior.len()
                    ),
                )
            } else {
                CheckOutcome::fail(
                    "evaluate_profile",
                    "profile 'default': one or more prior stages failed",
                )
            }
        }
        other => CheckOutcome::fail("evaluate_profile", format!("unknown profile '{other}'")),
    }
}

/// Run every stage of the affidavit pipeline over `records` against
/// `profile`, timing each stage with a [`VerifyGuard`]. No stage
/// short-circuits another: every stage always runs and contributes a
/// [`CheckOutcome`], even after an earlier stage has already failed, so the
/// returned [`Verdict`] documents everything the pipeline observed.
pub fn run_pipeline<R: ReceiptLike>(records: &[R], profile: &str) -> (Verdict, VerifyMetrics) {
    let mut guard = VerifyGuard::new();
    let mut outcomes = Vec::with_capacity(5);

    guard.begin_stage("check_format");
    let o = check_format(records);
    guard.end_stage(o.passed);
    outcomes.push(o);

    guard.begin_stage("chain_integrity");
    let o = chain_integrity(records);
    guard.end_stage(o.passed);
    outcomes.push(o);

    guard.begin_stage("continuity");
    let o = continuity(records);
    guard.end_stage(o.passed);
    outcomes.push(o);

    guard.begin_stage("verify_commitments");
    let o = verify_commitments(records);
    guard.end_stage(o.passed);
    outcomes.push(o);

    guard.begin_stage("evaluate_profile");
    let o = evaluate_profile(profile, &outcomes);
    guard.end_stage(o.passed);
    outcomes.push(o);

    let metrics = guard.finish();
    let accepted = outcomes.iter().all(|o| o.passed);
    let reason = if accepted {
        None
    } else {
        outcomes
            .iter()
            .find(|o| !o.passed)
            .map(|o| o.detail.clone())
    };
    let verdict = Verdict {
        accepted,
        profile: profile.to_string(),
        outcomes,
        reason,
    };
    (verdict, metrics)
}

// ── Tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_guard_all_pass() {
        let mut g = VerifyGuard::new();
        g.begin_stage("decode");
        g.end_stage(true);
        g.begin_stage("chain_integrity");
        g.end_stage(true);
        let m = g.finish();
        assert_eq!(m.passed_count, 2);
        assert_eq!(m.failed_count, 0);
        assert!(m.first_failure_stage().is_none());
        assert!(m.summary_line().starts_with("2/2 passed"));
    }

    #[test]
    fn verify_guard_first_failure_reported() {
        let mut g = VerifyGuard::new();
        g.begin_stage("decode");
        g.end_stage(true);
        g.begin_stage("chain_integrity");
        g.end_stage(false);
        g.begin_stage("continuity");
        g.end_stage(false);
        let m = g.finish();
        assert_eq!(m.first_failure_stage(), Some("chain_integrity"));
        assert!(m.summary_line().contains("chain_integrity"));
    }

    #[test]
    fn verify_guard_unclosed_stage_counts_as_fail() {
        let mut g = VerifyGuard::new();
        g.begin_stage("decode");
        g.end_stage(true);
        g.begin_stage("chain_integrity"); // never ended
        let m = g.finish();
        assert_eq!(m.failed_count, 1);
    }

    // ── run_pipeline / ReceiptLike ───────────────────────────────────────

    use crate::receipt_record::ReceiptRecord;

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
                andon: crate::law::Andon::Green,
                obligation_count: 0,
                object_ids: vec![format!("law:instr{i}")],
                signature_hex: None,
            };
            let chain_hash = record.recompute_chain_hash().expect("recompute");
            record.chain_hash_hex = hex::encode(chain_hash);
            prev = chain_hash;
            records.push(record);
        }
        records
    }

    #[test]
    fn run_pipeline_accepts_a_lawful_chain() {
        let records = chained_records(3);
        let (verdict, metrics) = run_pipeline(&records, "default");
        assert!(verdict.accepted, "verdict: {verdict:?}");
        assert_eq!(verdict.profile, "default");
        assert!(verdict.reason.is_none());
        assert_eq!(verdict.outcomes.len(), 5);
        assert_eq!(metrics.stages.len(), 5);
        assert_eq!(metrics.failed_count, 0);
    }

    #[test]
    fn run_pipeline_rejects_tampered_chain_hash_but_runs_every_stage() {
        let mut records = chained_records(3);
        // Tamper with the middle record's chain_hash directly (not via
        // recompute), so chain_integrity fails but linkage to record 2 also
        // breaks - both are independently detectable, and every stage still
        // runs (affidavit style: no short-circuiting).
        records[1].chain_hash_hex = "ff".repeat(32);
        let (verdict, _metrics) = run_pipeline(&records, "default");
        assert!(!verdict.accepted);
        assert_eq!(
            verdict.outcomes.len(),
            5,
            "every stage must still run after a failure"
        );
        let chain_integrity_outcome = verdict
            .outcomes
            .iter()
            .find(|o| o.stage == "chain_integrity")
            .unwrap();
        assert!(!chain_integrity_outcome.passed);
        assert!(verdict.reason.is_some());
    }

    #[test]
    fn run_pipeline_rejects_out_of_order_instruction_id() {
        let mut records = chained_records(3);
        records[2].instruction_id = records[1].instruction_id;
        let (verdict, _metrics) = run_pipeline(&records, "default");
        assert!(!verdict.accepted);
        let continuity_outcome = verdict
            .outcomes
            .iter()
            .find(|o| o.stage == "continuity")
            .unwrap();
        assert!(!continuity_outcome.passed);
    }

    #[test]
    fn run_pipeline_rejects_broken_linkage() {
        let mut records = chained_records(3);
        records.swap(1, 2);
        let (verdict, _metrics) = run_pipeline(&records, "default");
        assert!(!verdict.accepted);
        let linkage_outcome = verdict
            .outcomes
            .iter()
            .find(|o| o.stage == "verify_commitments")
            .unwrap();
        assert!(!linkage_outcome.passed);
    }

    #[test]
    fn run_pipeline_rejects_unknown_profile() {
        let records = chained_records(1);
        let (verdict, _metrics) = run_pipeline(&records, "quantum-certified");
        assert!(!verdict.accepted);
        assert_eq!(verdict.profile, "quantum-certified");
    }

    #[test]
    fn check_format_flags_malformed_hex() {
        let mut records = chained_records(1);
        records[0].payload_hash_hex = "not-hex".to_string();
        let outcome = check_format(&records);
        assert!(!outcome.passed);
    }

    #[test]
    fn empty_record_slice_passes_every_structural_stage() {
        let records: Vec<ReceiptRecord> = Vec::new();
        let (verdict, _metrics) = run_pipeline(&records, "default");
        assert!(verdict.accepted, "verdict: {verdict:?}");
    }
}
