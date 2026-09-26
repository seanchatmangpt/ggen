//! Hardening + benchmark for rule-aware receipt chain verification
//! (FM-CHAIN-009, ggen PR #752).
//!
//! Chicago style: real `ReceiptRecord`s sealed by the real praxis-core chain
//! rules, the real committed `examples/tcps-generated` ledger (66 pre-F1
//! records), the real `ReceiptValidator` and `verify::chain_integrity`
//! pipelines. No test doubles.
//!
//! Falsifiers covered here (each would make a test below fail):
//! - malformed discriminator (case / whitespace / empty / non-string /
//!   duplicate key) accepted or silently defaulted;
//! - a verifier (validator, `verify::chain_integrity`) rejecting the lawful
//!   pre-F1 committed ledger, or accepting a tampered one;
//! - chain-rule downgrade: a legacy base-rule record after a declared one
//!   verifying instead of being refused;
//! - `recompute_chain_hash_lawful` accepting a hash `verify_chain` refuses;
//! - replay mismatch: verification not deterministic across runs;
//! - reordering / duplicate delivery of committed records passing linkage;
//! - benchmark: rule-aware verification exceeding its recorded bound.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::PathBuf;
use std::time::{Duration, Instant};

use praxis_core::law::Andon;
use praxis_core::receipt_epoch::{
    AndonLevel, CeilingLevel, ComponentLevels, ReceiptEpochV2Builder, SCHEMA_V1, SCHEMA_V2,
};
use praxis_core::receipt_record::{
    ChainRule, ChainRuleMonotonicity, ChainStanding, ChainVerification, ReceiptRecord,
    CHAIN_RULE_BASE, CHAIN_RULE_V2_FOLD, RECEIPT_RECORD_VERSION,
};
use praxis_core::verify::chain_integrity;
use praxis_core::{ReceiptValidator, SystemClock};

// ---------------------------------------------------------------------------
// Fixtures: real records sealed by the real rules.
// ---------------------------------------------------------------------------

fn v1_record(instruction_id: u64, prev: &str) -> ReceiptRecord {
    ReceiptRecord {
        version: RECEIPT_RECORD_VERSION,
        instruction_id,
        activity_idx: 0,
        activity: None,
        node_kind: 0,
        ts_ns: 1_000 + instruction_id,
        duration_ms: None,
        origin: None,
        payload_hash_hex: format!("{:064x}", instruction_id + 7),
        prev_chain_hash_hex: prev.to_string(),
        chain_hash_hex: String::new(),
        andon: Andon::Green,
        obligation_count: 0,
        object_ids: vec!["law:1111111111111111".to_string()],
        signature_hex: None,
        schema: SCHEMA_V1.to_string(),
        v2: None,
        chain_rule: None,
    }
}

fn v2_record(instruction_id: u64, prev: &str) -> ReceiptRecord {
    let mut r = v1_record(instruction_id, prev);
    r.schema = SCHEMA_V2.to_string();
    r.v2 = Some(
        ReceiptEpochV2Builder::new(
            CeilingLevel::Green,
            ComponentLevels::uniform(AndonLevel::Green),
        )
        .build()
        .expect("epoch builds"),
    );
    r
}

fn seal(mut r: ReceiptRecord, rule: ChainRule, declare: bool) -> ReceiptRecord {
    r.chain_rule = None;
    r.chain_hash_hex = hex::encode(r.recompute_chain_hash_under(rule).expect("recompute"));
    if declare {
        r.chain_rule = Some(rule.as_str().to_string());
    }
    r
}

/// A linked ledger of `n` v2 records, each sealed by `rule_of(i)`.
fn ledger(n: u64, rule_of: impl Fn(u64) -> (ChainRule, bool)) -> Vec<ReceiptRecord> {
    let mut prev = "0".repeat(64);
    let mut out = Vec::new();
    for i in 1..=n {
        let (rule, declare) = rule_of(i);
        let r = seal(v2_record(i, &prev), rule, declare);
        prev.clone_from(&r.chain_hash_hex);
        out.push(r);
    }
    out
}

fn committed_tcps_log() -> Vec<ReceiptRecord> {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../examples/tcps-generated/.ggen-v2/receipt-log.jsonl");
    let raw = std::fs::read_to_string(&path).expect("read committed tcps receipt log");
    raw.lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            let v: serde_json::Value = serde_json::from_str(l).expect("parse log line");
            serde_json::from_value(v["record"].clone()).expect("record deserializes")
        })
        .collect()
}

fn stage<'a>(
    verdict: &'a praxis_core::Verdict, name: &str,
) -> &'a praxis_core::receipt_validator::CheckOutcome {
    &verdict
        .stages
        .iter()
        .find(|s| s.stage == name)
        .unwrap_or_else(|| panic!("stage {name} present"))
        .outcome
}

fn stage_passed(verdict: &praxis_core::Verdict, name: &str) -> bool {
    matches!(
        stage(verdict, name),
        praxis_core::receipt_validator::CheckOutcome::Pass
            | praxis_core::receipt_validator::CheckOutcome::Skip(_)
    )
}

fn stage_message(verdict: &praxis_core::Verdict, name: &str) -> String {
    format!("{:?}", stage(verdict, name))
}

// ---------------------------------------------------------------------------
// Malformed input.
// ---------------------------------------------------------------------------

#[test]
fn malformed_discriminators_are_refused_never_defaulted() {
    for bad in [
        "",
        " praxis-chain/v2-fold",
        "praxis-chain/v2-fold ",
        "PRAXIS-CHAIN/V2-FOLD",
        "praxis-chain/Base",
        "praxis-chain/v2",
        "praxis-chain/v2-fold\0",
        "praxis-chain/v2\u{2011}fold", // non-breaking hyphen homoglyph
    ] {
        let err = ChainRule::parse(bad).expect_err("malformed rule refused");
        assert_eq!(err.name(), "ReceiptChainRuleInvalid", "{bad:?}");
        let mut r = seal(v2_record(1, &"0".repeat(64)), ChainRule::V2Fold, false);
        r.chain_rule = Some(bad.to_string());
        assert_eq!(
            r.verify_chain().expect_err("verify refuses").name(),
            "ReceiptChainRuleInvalid",
            "{bad:?}"
        );
        assert_eq!(
            r.recompute_chain_hash_lawful()
                .expect_err("lawful recompute refuses")
                .name(),
            "ReceiptChainRuleInvalid",
            "{bad:?}"
        );
    }
}

#[test]
fn chain_rule_json_shapes_duplicate_key_and_wrong_type_are_refused() {
    let r = seal(v2_record(1, &"0".repeat(64)), ChainRule::V2Fold, true);
    let json = serde_json::to_string(&r).expect("serialize");
    // Duplicate key: a smuggled second declaration must not be resolved by
    // last-wins parsing.
    let dup = json.replacen(
        r#""chain_rule":"praxis-chain/v2-fold""#,
        r#""chain_rule":"praxis-chain/v2-fold","chain_rule":"praxis-chain/base""#,
        1,
    );
    assert_ne!(dup, json, "fixture must contain the declaration");
    assert!(serde_json::from_str::<ReceiptRecord>(&dup).is_err());
    // Wrong type.
    for bad in ["1", "true", r#"["praxis-chain/v2-fold"]"#, "{}"] {
        let wrong = json.replacen(
            r#""chain_rule":"praxis-chain/v2-fold""#,
            &format!(r#""chain_rule":{bad}"#),
            1,
        );
        assert!(
            serde_json::from_str::<ReceiptRecord>(&wrong).is_err(),
            "{bad}"
        );
    }
    // Explicit null is the undeclared shape (same as absent), and verifies
    // under the fold rule exactly like the stripped record.
    let null = json.replacen(
        r#""chain_rule":"praxis-chain/v2-fold""#,
        r#""chain_rule":null"#,
        1,
    );
    let back: ReceiptRecord = serde_json::from_str(&null).expect("null parses");
    assert_eq!(back.chain_rule, None);
    assert_eq!(
        back.verify_chain().expect("verify"),
        ChainVerification::Verified(ChainStanding::FullyBound)
    );
}

#[test]
fn declared_base_on_v2_is_refused_but_declared_base_on_v1_is_bound() {
    let mut v2 = seal(v2_record(1, &"0".repeat(64)), ChainRule::Base, false);
    v2.chain_rule = Some(CHAIN_RULE_BASE.to_string());
    assert_eq!(
        v2.verify_chain().expect_err("refused").name(),
        "ReceiptChainRuleInvalid"
    );
    let v1 = seal(v1_record(1, &"0".repeat(64)), ChainRule::Base, true);
    assert_eq!(
        v1.verify_chain().expect("verify"),
        ChainVerification::Verified(ChainStanding::FullyBound)
    );
    // Declaring fold on a v1 record is equally bound (fold is a no-op there).
    let mut v1_fold = v1.clone();
    v1_fold.chain_rule = Some(CHAIN_RULE_V2_FOLD.to_string());
    assert_eq!(
        v1_fold.verify_chain().expect("verify"),
        ChainVerification::Verified(ChainStanding::FullyBound)
    );
}

// ---------------------------------------------------------------------------
// Verifier consistency on the real committed ledger (was: every verifier
// but `receipt history`/`sync` reported the pre-F1 ledger as tampered).
// ---------------------------------------------------------------------------

#[test]
fn every_verifier_accepts_the_committed_pre_f1_ledger() {
    let log = committed_tcps_log();
    assert_eq!(log.len(), 66);
    assert!(log.iter().all(|r| r.chain_rule.is_none()));
    let legacy = log
        .iter()
        .filter(|r| {
            r.verify_chain().expect("verify")
                == ChainVerification::Verified(ChainStanding::LegacyV2Unbound)
        })
        .count();
    assert!(legacy > 0, "the committed ledger carries pre-F1 v2 records");

    let verdict = ReceiptValidator::validate(&log, &SystemClock);
    assert!(
        stage_passed(&verdict, "chain_recompute"),
        "{}",
        stage_message(&verdict, "chain_recompute")
    );
    assert!(
        stage_passed(&verdict, "chain_linkage"),
        "{}",
        stage_message(&verdict, "chain_linkage")
    );
    let integrity = chain_integrity(&log);
    assert!(integrity.passed, "{integrity:?}");
}

#[test]
fn every_verifier_refuses_a_tampered_committed_record() {
    let clean = committed_tcps_log();
    // Tamper each preimage class the base rule covers, at the head and in
    // the middle: payload hash, andon count, instruction id.
    for idx in [0usize, 33, 65] {
        for field in 0..3 {
            let mut log = clean.clone();
            match field {
                0 => log[idx].payload_hash_hex = "ab".repeat(32),
                1 => log[idx].obligation_count += 1,
                _ => log[idx].instruction_id ^= 1,
            }
            assert!(
                matches!(
                    log[idx].verify_chain().expect("verify"),
                    ChainVerification::Mismatch { .. }
                ),
                "idx {idx} field {field}"
            );
            let verdict = ReceiptValidator::validate(&log, &SystemClock);
            assert!(!verdict.ok, "idx {idx} field {field}");
            assert!(
                !stage_passed(&verdict, "chain_recompute"),
                "idx {idx} field {field}"
            );
            assert!(!chain_integrity(&log).passed, "idx {idx} field {field}");
        }
    }
}

#[test]
fn lawful_recompute_never_accepts_what_verify_chain_refuses() {
    let clean = committed_tcps_log();
    let mut mutants = Vec::new();
    for r in &clean {
        mutants.push(r.clone());
        let mut forged_ceiling = r.clone();
        if let Some(v2) = forged_ceiling.v2.as_mut() {
            v2.standing_ceiling = CeilingLevel::Red;
        }
        mutants.push(forged_ceiling);
        let mut declared_fold = r.clone();
        declared_fold.chain_rule = Some(CHAIN_RULE_V2_FOLD.to_string());
        mutants.push(declared_fold);
        let mut bad_payload = r.clone();
        bad_payload.payload_hash_hex = "cd".repeat(32);
        mutants.push(bad_payload);
    }
    for (i, m) in mutants.iter().enumerate() {
        let verified = matches!(m.verify_chain(), Ok(ChainVerification::Verified(_)));
        let lawful_matches = m
            .recompute_chain_hash_lawful()
            .map(|h| h == m.chain_hash().expect("hex"))
            .unwrap_or(false);
        assert_eq!(verified, lawful_matches, "mutant {i}");
    }
}

// ---------------------------------------------------------------------------
// Downgrade (unauthorized re-seal), reordering, duplicate delivery, replay.
// ---------------------------------------------------------------------------

#[test]
fn legacy_record_after_a_declared_one_is_a_refused_downgrade() {
    // Three declared fold records; the attacker re-seals the head under the
    // base rule, strips the declaration and forges its v2 payload.
    let mut log = ledger(3, |_| (ChainRule::V2Fold, true));
    let mut head = log.pop().expect("head");
    head.v2.as_mut().expect("v2").standing_ceiling = CeilingLevel::Green;
    head.v2.as_mut().expect("v2").promotion_eligible =
        !head.v2.as_ref().unwrap().promotion_eligible;
    let forged = seal(head, ChainRule::Base, false);
    assert_eq!(
        forged.verify_chain().expect("verify"),
        ChainVerification::Verified(ChainStanding::LegacyV2Unbound),
        "in isolation the forged head looks like a legacy record"
    );
    log.push(forged);

    let verdict = ReceiptValidator::validate(&log, &SystemClock);
    assert!(!verdict.ok);
    let msg = stage_message(&verdict, "chain_recompute");
    assert!(msg.contains("chain-rule downgrade"), "{msg}");
    assert!(msg.contains("record 2"), "{msg}");

    let mut mono = ChainRuleMonotonicity::new();
    mono.observe(0, &log[0], ChainStanding::FullyBound)
        .expect("declared");
    assert_eq!(mono.first_declared(), Some(0));
    mono.observe(1, &log[1], ChainStanding::FullyBound)
        .expect("declared");
    let err = mono
        .observe(2, &log[2], ChainStanding::LegacyV2Unbound)
        .expect_err("downgrade refused");
    assert_eq!(err.name(), "ReceiptChainRuleInvalid");
}

#[test]
fn legacy_prefix_then_declared_suffix_is_lawful() {
    // The shape `ggen sync` produces when it extends the committed tcps
    // ledger: undeclared base-sealed prefix, declared fold suffix.
    let log = ledger(6, |i| {
        if i <= 3 {
            (ChainRule::Base, false)
        } else {
            (ChainRule::V2Fold, true)
        }
    });
    let verdict = ReceiptValidator::validate(&log, &SystemClock);
    assert!(
        stage_passed(&verdict, "chain_recompute"),
        "{}",
        stage_message(&verdict, "chain_recompute")
    );
    assert!(chain_integrity(&log).passed);
    let mut mono = ChainRuleMonotonicity::new();
    for (i, r) in log.iter().enumerate() {
        let ChainVerification::Verified(s) = r.verify_chain().expect("verify") else {
            panic!("record {i} must verify");
        };
        mono.observe(i, r, s).expect("monotone");
    }
    assert_eq!(mono.first_declared(), Some(3));
    // Undeclared-but-fold records after a declared one (an older post-F1
    // writer) stay lawful: only legacy standing is a downgrade.
    let mut mixed = ledger(2, |_| (ChainRule::V2Fold, true));
    let prev = mixed[1].chain_hash_hex.clone();
    mixed.push(seal(v2_record(3, &prev), ChainRule::V2Fold, false));
    assert!(stage_passed(
        &ReceiptValidator::validate(&mixed, &SystemClock),
        "chain_recompute"
    ));
}

#[test]
fn reordering_or_duplicating_committed_records_breaks_linkage() {
    let clean = committed_tcps_log();
    let mut swapped = clean.clone();
    swapped.swap(10, 11);
    assert!(!stage_passed(
        &ReceiptValidator::validate(&swapped, &SystemClock),
        "chain_linkage"
    ));
    let mut duplicated = clean.clone();
    duplicated.insert(20, clean[19].clone());
    assert!(!stage_passed(
        &ReceiptValidator::validate(&duplicated, &SystemClock),
        "chain_linkage"
    ));
    // Every record still verifies individually: the per-record rule check is
    // not what catches reordering/duplication -- the linkage stage is.
    assert!(swapped
        .iter()
        .all(|r| matches!(r.verify_chain(), Ok(ChainVerification::Verified(_)))));
}

#[test]
fn verification_replays_deterministically() {
    let log = committed_tcps_log();
    let first: Vec<_> = log.iter().map(|r| r.verify_chain().expect("v")).collect();
    for _ in 0..3 {
        let again: Vec<_> = log.iter().map(|r| r.verify_chain().expect("v")).collect();
        assert_eq!(first, again);
    }
    // Serialization round-trip does not change the verdict (replay from disk).
    for r in &log {
        let back: ReceiptRecord =
            serde_json::from_str(&serde_json::to_string(r).expect("ser")).expect("de");
        assert_eq!(
            back.verify_chain().expect("v"),
            r.verify_chain().expect("v")
        );
    }
}

// ---------------------------------------------------------------------------
// Benchmark with a committed regression bound.
// ---------------------------------------------------------------------------

fn per_record(records: &[ReceiptRecord]) -> Duration {
    let start = Instant::now();
    for r in records {
        std::hint::black_box(r.verify_chain().expect("verify"));
    }
    start.elapsed() / u32::try_from(records.len()).expect("len")
}

/// Interleaved rounds (A, B, C, A, B, C, ...) so machine-wide noise hits
/// every path alike. Returns the per-path samples in round order.
fn interleaved(paths: &[&[ReceiptRecord]], rounds: usize) -> Vec<Vec<Duration>> {
    let mut samples: Vec<Vec<Duration>> = vec![Vec::new(); paths.len()];
    for _ in 0..rounds {
        for (i, p) in paths.iter().enumerate() {
            samples[i].push(per_record(p));
        }
    }
    samples
}

fn median(mut v: Vec<Duration>) -> Duration {
    v.sort();
    v[v.len() / 2]
}

/// Median over rounds of the same-round ratio `b / a`: adjacent samples
/// share the machine state, so drift cancels and outlier rounds drop out.
fn median_ratio(a: &[Duration], b: &[Duration]) -> f64 {
    let mut r: Vec<f64> = a
        .iter()
        .zip(b)
        .map(|(x, y)| y.as_secs_f64() / x.as_secs_f64())
        .collect();
    r.sort_by(f64::total_cmp);
    r[r.len() / 2]
}

/// Deterministic timing benchmark (the crate carries no criterion harness;
/// this mirrors `receipt_validator`'s documented-target pattern). Numbers
/// are printed for the bench receipt.
///
/// Measured 2026-09-26 on the PR #752 hardening head (Apple Silicon):
/// release -- fully-bound 2.9us/record, legacy 3.1us/record (synthetic
/// n=1000), committed tcps 28.6us/record (n=66, ~52 KB v2 payload each),
/// `ReceiptValidator::validate` over 1000 fold records 4.9ms; debug --
/// fully-bound ~146-170us, legacy ~128-156us, committed tcps ~2.8-3.0ms.
/// Before the shared-base optimisation in `verify_chain` the legacy path
/// recomputed the base frame twice (release 4.55us vs 3.04us fully-bound;
/// debug 239us vs 114us).
///
/// Ceilings (debug build, shared CI runners, test threads concurrent): 2ms
/// per small record on either path, 30ms per committed tcps record, and the
/// legacy path at most 1.25x the fully-bound path (median of 21 same-round
/// ratios, interleaved) -- the falsifier for a reintroduced second base
/// frame recompute (measured 1.4-2.0x) or any quadratic re-verification.
#[test]
fn bench_rule_aware_verification_stays_within_bound() {
    let fold = ledger(1000, |_| (ChainRule::V2Fold, true));
    let legacy = ledger(1000, |_| (ChainRule::Base, false));
    let tcps = committed_tcps_log();
    for r in legacy.iter().take(3) {
        assert_eq!(
            r.verify_chain().expect("v"),
            ChainVerification::Verified(ChainStanding::LegacyV2Unbound)
        );
    }

    let stats = interleaved(&[&fold, &legacy, &tcps], 21);
    let ratio = median_ratio(&stats[0], &stats[1]);
    let fold_med = median(stats[0].clone());
    let legacy_med = median(stats[1].clone());
    let tcps_med = median(stats[2].clone());

    let start = Instant::now();
    let verdict = ReceiptValidator::validate(&fold, &SystemClock);
    let validate_1000 = start.elapsed();
    assert!(stage_passed(&verdict, "chain_recompute"));

    eprintln!(
        "[bench chain_rule] verify_chain median per record over 21 interleaved rounds: \
         fully-bound(declared fold, n=1000) {fold_med:?}; legacy-v2-unbound(base, n=1000) \
         {legacy_med:?}; committed tcps (n={}) {tcps_med:?}; median same-round ratio \
         legacy/fully-bound {ratio:.3}; ReceiptValidator::validate(1000 fold) {validate_1000:?}",
        tcps.len()
    );

    let ceiling = Duration::from_millis(2);
    assert!(
        fold_med < ceiling,
        "fully-bound {fold_med:?} >= {ceiling:?}"
    );
    assert!(legacy_med < ceiling, "legacy {legacy_med:?} >= {ceiling:?}");
    let tcps_ceiling = Duration::from_millis(30);
    assert!(
        tcps_med < tcps_ceiling,
        "tcps {tcps_med:?} >= {tcps_ceiling:?}"
    );
    assert!(
        ratio <= 1.25,
        "legacy path costs {ratio:.3}x the fully-bound path (bound 1.25x; a second \
         base-frame recompute measures 1.4-2.0x)"
    );
}
