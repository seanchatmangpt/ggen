//! Golden court for the receipt chain (post praxis-core retirement).
//!
//! The pre-retirement differential court compared the seam against the
//! vendored `praxis-core` oracle; praxis-core is now retired (SJIRA-15,
//! 2026-10-09), so the proof of record is:
//!
//! 1. the committed TCPS golden fixture
//!    (`examples/tcps-generated/.ggen-v2/receipt.json`) — its `d04c6d08…`
//!    golden base head and `bebae299…` fold hash are asserted from the
//!    committed bytes (FM-CHAIN-009 golden), and
//! 2. graphlaw-side verification: every record sealed through the seam
//!    (`ggen_engine::receipt_chain_seam`, graphlaw-backed) is re-serialized
//!    to wire JSON and re-verified through `graphlaw::receipt_chain`
//!    directly — same bytes, same head, same verdict.
//!
//! Method (Chicago, real collaborators, no mocks): a seeded LCG generates a
//! deterministic matrix of receipt records (varied payloads, obligation
//! counts, andon outcomes, refusal-adjacent denial metadata, both
//! `ChainRule::Base` and `ChainRule::V2Fold`, declared and undeclared, valid
//! and tampered). Every record is built and sealed through the seam
//! directly, serialized to its wire JSON, and re-deserialized into
//! `graphlaw`'s `ReceiptRecord`, which must reproduce the seam's head hash
//! and verdict on the shared wire shape.

use graphlaw::receipt_chain::CHAIN_RULE_BASE;
use ggen_engine::receipt_chain_seam::epoch::{
    AdmissionDecision, AdmissionItem, AndonLevel, CeilingLevel, ComponentLevels,
    ObservedOutcome, ReceiptEpochV2Builder, SCHEMA_V2,
};
use ggen_engine::receipt_chain_seam::epoch as receipt_epoch;
use ggen_engine::receipt_chain_seam::{
    ChainRule, ChainStanding, ChainVerification, ReceiptRecord, CHAIN_RULE_V2_FOLD,
    RECEIPT_RECORD_VERSION, Andon,
};

/// Deterministic 64-bit LCG (numerical-recipes constants). Fixed seed, so
/// the whole case matrix is reproducible run-to-run.
struct Lcg(u64);

impl Lcg {
    fn next(&mut self) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        self.0
    }

    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }
}

/// What kind of case to generate.
#[derive(Debug, Clone, Copy, PartialEq)]
enum CaseKind {
    /// Valid, no tampering.
    Valid,
    /// Flip one byte of `payload_hash_hex`.
    TamperedPayload,
    /// Forge a `standing_ceiling` ratchet in the `v2` payload (the F1 attack).
    TamperedV2Ceiling,
    /// Valid fold-sealed record with its `chain_rule` declaration stripped.
    StrippedDeclaration,
    /// Malformed (non-hex) `payload_hash_hex`: the wire side must refuse with
    /// the same error classification as the seam side.
    MalformedHex,
}

/// One generated case.
struct Case {
    label: String,
    record: ReceiptRecord,
}

/// Build a fresh, untampered record from seeded parameters.
fn generate_record(rng: &mut Lcg, index: usize) -> ReceiptRecord {
    let instruction_id = rng.next();
    let activity_idx = rng.below(64) as u16;
    let node_kind = rng.below(8) as u8;
    let ts_ns = rng.next();
    let obligation_count = rng.below(12) as u32;
    let n_objects = (rng.below(4) + 1) as usize;

    // Varied andon outcomes, including the payload-bearing Overridden
    // variant (refusal-adjacent: Halted carries obligations we would have to
    // construct; Overridden exercises the same enum serde surface).
    let andon = if index % 3 == 0 {
        Andon::Overridden {
            by: format!("actor-{}", rng.below(1000)),
            reason: format!("override-reason-{}", rng.below(1000)),
            at: rng.next(),
        }
    } else {
        Andon::Green
    };

    let mut object_ids = Vec::with_capacity(n_objects);
    for _ in 0..n_objects {
        object_ids.push(format!("law:{:016x}", rng.next()));
    }

    let payload_hash_hex = hex::encode(rng.next().to_le_bytes()).repeat(4); // 64 hex chars

    let mut record = ReceiptRecord {
        version: RECEIPT_RECORD_VERSION,
        instruction_id,
        activity_idx,
        activity: if index % 2 == 0 {
            Some(format!("activity-{index}"))
        } else {
            None
        },
        node_kind,
        ts_ns,
        duration_ms: None,
        origin: None,
        payload_hash_hex,
        prev_chain_hash_hex: "0".repeat(64),
        chain_hash_hex: String::new(),
        andon,
        obligation_count,
        object_ids,
        signature_hex: None,
        schema: receipt_epoch::SCHEMA_V1.to_string(),
        v2: None,
        chain_rule: None,
    };

    // Half the v2 cases carry a full v2 epoch payload.
    let carries_v2 = index % 4 == 1;
    if carries_v2 {
        let ceiling = if index % 8 == 1 {
            CeilingLevel::LegacyObserved
        } else {
            CeilingLevel::Green
        };
        let mut builder = ReceiptEpochV2Builder::new(
            ceiling,
            ComponentLevels::uniform(if index % 8 == 5 {
                AndonLevel::Yellow
            } else {
                AndonLevel::Green
            }),
        );
        if index % 8 == 5 {
            builder = builder.admission_item(AdmissionItem {
                evidence_id: format!("out/evidence-{index}.txt"),
                observed_outcome: ObservedOutcome::Fail,
                decision: AdmissionDecision::Refused,
                reason: format!("refusal-reason-{index}"),
                obligations_discharged: vec![],
                obligations_created: vec![],
            });
        }
        record.schema = SCHEMA_V2.to_string();
        record.v2 = Some(builder.build().expect("epoch builds"));
    }

    record
}

/// Seal `record` under `rule`; declare the rule on the wire when `declare`.
fn seal(mut record: ReceiptRecord, rule: ChainRule, declare: bool) -> ReceiptRecord {
    record.chain_rule = None;
    let chain = record
        .recompute_chain_hash_under(rule)
        .expect("sealing recompute");
    record.chain_hash_hex = hex::encode(chain);
    record.chain_rule = if declare {
        Some(rule.as_str().to_string())
    } else {
        None
    };
    record
}

/// The generated case matrix: >= 20 deterministic cases covering both chain
/// rules, declared and undeclared, v1 and v2, valid and tampered.
fn case_matrix() -> Vec<Case> {
    let mut rng = Lcg(0xBADC0FFE_D15EA5E);
    let mut cases = Vec::new();

    for i in 0..24 {
        let record = generate_record(&mut rng, i);
        let carries_v2 = record.v2.is_some();

        // Cycle rule/declaration choices lawfully: base is only sealable on
        // records without a v2 payload.
        let (rule, declare) = match i % 4 {
            0 => (ChainRule::V2Fold, true),
            1 => (ChainRule::V2Fold, false),
            2 if !carries_v2 => (ChainRule::Base, true),
            2 => (ChainRule::V2Fold, true),
            _ if !carries_v2 => (ChainRule::Base, false),
            _ => (ChainRule::V2Fold, false),
        };
        let sealed = seal(record, rule, declare);

        let kind = match i % 6 {
            0 => CaseKind::Valid,
            1 => CaseKind::TamperedPayload,
            2 if carries_v2 => CaseKind::TamperedV2Ceiling,
            2 => CaseKind::Valid,
            3 => CaseKind::StrippedDeclaration,
            4 => CaseKind::Valid,
            _ => CaseKind::MalformedHex,
        };

        let mut case_record = sealed;
        match kind {
            CaseKind::Valid => {}
            CaseKind::TamperedPayload => {
                case_record.payload_hash_hex = format!("aa{}", &case_record.payload_hash_hex[2..]);
            }
            CaseKind::TamperedV2Ceiling => {
                let v2 = case_record.v2.as_mut().expect("v2 present");
                v2.standing_ceiling = CeilingLevel::Red;
            }
            CaseKind::StrippedDeclaration => {
                case_record.chain_rule = None;
            }
            CaseKind::MalformedHex => {
                case_record.payload_hash_hex = "not-hex-at-all".to_string();
            }
        }

        cases.push(Case {
            label: format!("gen-{i:02}/{kind:?}/rule={rule:?}/declared={declare}"),
            record: case_record,
        });
    }

    cases
}

/// One assertion: the seam's verdict on the record, and graphlaw's verdict on
/// the same record's wire bytes, must be the same string, and the recomputed
/// head must be non-empty.
fn assert_golden_agreement(label: &str, record: &ReceiptRecord) {
    let wire =
        serde_json::to_value(record).expect("record serializes to the shared wire shape");

    let recompute: Result<String, String> = record
        .recompute_chain_hash()
        .map(hex::encode)
        .map_err(|e| e.name().to_string());
    let verification: Result<String, String> = record
        .verify_chain()
        .map(|v| match v {
            ChainVerification::Verified(s) => format!("Verified({})", standing_str(s)),
            ChainVerification::Mismatch { rule, .. } => {
                format!("Mismatch({})", rule_str(rule))
            }
        })
        .map_err(|e| e.name().to_string());

    // graphlaw re-parses the wire bytes and must reproduce both outcomes.
    let g: graphlaw::receipt_chain::ReceiptRecord =
        serde_json::from_value(wire).expect("graphlaw deserializes the shared wire shape");
    let g_recompute: Result<String, String> = g
        .recompute_chain_hash()
        .map(hex::encode)
        .map_err(|e| e.name().to_string());
    let g_verification: Result<String, String> = g
        .verify_chain()
        .map(|v| match v {
            graphlaw::receipt_chain::ChainVerification::Verified(s) => format!(
                "Verified({})",
                match s {
                    graphlaw::receipt_chain::ChainStanding::FullyBound => "fully-bound",
                    graphlaw::receipt_chain::ChainStanding::LegacyV2Unbound => {
                        "legacy-v2-unbound"
                    }
                }
            ),
            graphlaw::receipt_chain::ChainVerification::Mismatch { rule, .. } => format!(
                "Mismatch({})",
                match rule {
                    graphlaw::receipt_chain::ChainRule::Base => CHAIN_RULE_BASE,
                    graphlaw::receipt_chain::ChainRule::V2Fold => CHAIN_RULE_V2_FOLD,
                }
            ),
        })
        .map_err(|e| e.name().to_string());

    assert_eq!(
        format!("{:?}", recompute),
        format!("{:?}", g_recompute),
        "{label}: graphlaw wire-side recompute diverged from the seam"
    );
    assert_eq!(
        format!("{:?}", verification),
        format!("{:?}", g_verification),
        "{label}: graphlaw wire-side verify verdict diverged from the seam"
    );
    if let Ok(hash) = &recompute {
        assert_ne!(hash, "", "{label}: empty recompute hash");
    }
}

fn standing_str(s: ChainStanding) -> String {
    match s {
        ChainStanding::FullyBound => "fully-bound".to_string(),
        ChainStanding::LegacyV2Unbound => "legacy-v2-unbound".to_string(),
    }
}

fn rule_str(r: ChainRule) -> String {
    match r {
        ChainRule::Base => CHAIN_RULE_BASE.to_string(),
        ChainRule::V2Fold => CHAIN_RULE_V2_FOLD.to_string(),
    }
}

/// Case 0: the committed TCPS golden fixture. The seam and graphlaw's wire
/// side must both accept it, reproduce its `d04c6d08…` head under the base
/// rule and its `bebae299…` fold recompute (FM-CHAIN-009 golden).
#[test]
fn case_0_committed_tcps_fixture_is_accepted_identically() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../examples/tcps-generated/.ggen-v2/receipt.json");
    let raw = std::fs::read_to_string(&path).expect("read committed tcps receipt.json");
    let value: serde_json::Value = serde_json::from_str(&raw).expect("parse receipt.json");
    let record_value = value["record"].clone();

    let record: ReceiptRecord =
        serde_json::from_value(record_value.clone()).expect("seam parses fixture record");

    // Real ground truth from the committed bytes (FM-CHAIN-009 golden).
    assert!(
        record.chain_hash_hex.starts_with("d04c6d08"),
        "fixture head hash drifted: {}",
        record.chain_hash_hex
    );
    assert!(record.v2.is_some());
    assert!(record.chain_rule.is_none());

    assert_golden_agreement("case-0/tcps-golden-fixture", &record);

    // The base rule must reproduce the committed head exactly.
    let base_hex = hex::encode(
        record
            .recompute_chain_hash_under(ChainRule::Base)
            .expect("base recompute"),
    );
    assert_eq!(base_hex, record.chain_hash_hex, "base rule reproduces head");
    assert!(base_hex.starts_with("d04c6d08"));

    // graphlaw's wire side must agree on both rules against the golden hashes.
    let g: graphlaw::receipt_chain::ReceiptRecord =
        serde_json::from_value(record_value).expect("graphlaw parses fixture record");
    let g_base = hex::encode(
        g.recompute_chain_hash_under(graphlaw::receipt_chain::ChainRule::Base)
            .expect("graphlaw base recompute"),
    );
    assert_eq!(g_base, base_hex, "graphlaw base-rule head diverged");
    let g_fold = hex::encode(
        g.recompute_chain_hash_under(graphlaw::receipt_chain::ChainRule::V2Fold)
            .expect("graphlaw fold recompute"),
    );
    assert!(
        g_fold.starts_with("bebae299"),
        "graphlaw fold recompute lost the FM-CHAIN-009 golden fold hash: {g_fold}"
    );
}

/// The full generated matrix: every case's seam outcome must be reproduced
/// by graphlaw from the wire bytes alone.
#[test]
fn generated_matrix_agrees_on_recompute_and_verification() {
    let cases = case_matrix();
    assert!(
        cases.len() >= 20,
        "case matrix must be at least 20 cases, got {}",
        cases.len()
    );
    for case in &cases {
        assert_golden_agreement(&case.label, &case.record);
    }
}

/// Rolling multi-record chains: a chain of N records where each record's
/// `prev_chain_hash_hex` is the previous record's chain hash. graphlaw's
/// wire side must reproduce the seam's head at every link, under both chain
/// rules, and the head must advance.
#[test]
fn rolling_chains_produce_identical_heads() {
    for &(rule, declare) in &[
        (ChainRule::V2Fold, true),
        (ChainRule::V2Fold, false),
        (ChainRule::Base, true),
    ] {
        let mut rng = Lcg(0x5EED_5EED_5EED_5EED + u64::from(rule == ChainRule::Base));
        let mut prev = "0".repeat(64);
        let mut seam_head = prev.clone();

        for t in 0..6 {
            let mut record = generate_record(&mut rng, t);
            record.prev_chain_hash_hex = prev.clone();
            // Keep chains uniform: v1-only for base-rule chains (base is
            // unlawful on v2), and skip v2 on those records too for
            // simplicity of the rolling assertion.
            record.v2 = None;
            record.schema = receipt_epoch::SCHEMA_V1.to_string();
            let sealed = seal(record, rule, declare);
            seam_head = hex::encode(sealed.recompute_chain_hash().expect("chain link recompute"));

            let wire = serde_json::to_value(&sealed).expect("serialize link");
            let g: graphlaw::receipt_chain::ReceiptRecord =
                serde_json::from_value(wire).expect("graphlaw parses link");
            let g_head =
                hex::encode(g.recompute_chain_hash().expect("graphlaw chain link recompute"));
            assert_eq!(
                seam_head, g_head,
                "chain rule={rule:?} declared={declare} link {t}: heads diverged"
            );

            prev = seam_head.clone();
        }

        assert_ne!(
            seam_head,
            "0".repeat(64),
            "chain of 6 must advance the head"
        );
    }
}

/// Tamper refusal on rolling chains: flipping a mid-chain payload must be
/// refused (mismatch verdict) identically by the seam and by graphlaw's
/// wire side.
#[test]
fn tampered_mid_chain_link_is_refused_identically() {
    let mut rng = Lcg(0xCAFE_BABE_0000_0001);
    let mut chain: Vec<ReceiptRecord> = Vec::new();
    let mut prev = "0".repeat(64);
    for t in 0..5 {
        let mut record = generate_record(&mut rng, t);
        record.v2 = None;
        record.schema = receipt_epoch::SCHEMA_V1.to_string();
        record.prev_chain_hash_hex = prev.clone();
        let sealed = seal(record, ChainRule::V2Fold, true);
        prev = hex::encode(sealed.recompute_chain_hash().expect("link recompute"));
        chain.push(sealed);
    }

    // Tamper link 2's payload hash.
    let mut tampered = chain[2].clone();
    tampered.payload_hash_hex = format!("bb{}", &tampered.payload_hash_hex[2..]);

    assert_golden_agreement("tampered-chain/link-2", &tampered);

    // Both sides must reject the stored (now stale) chain hash.
    let verdict: String = tampered
        .verify_chain()
        .map(|v| match v {
            ChainVerification::Verified(_) => "Verified".to_string(),
            ChainVerification::Mismatch { .. } => "Mismatch".to_string(),
        })
        .expect("tampered valid-shape record must not error");
    assert_eq!(verdict, "Mismatch", "expected tamper refusal, got {verdict}");
}

/// The error surface agrees: a malformed hex field and a base-rule
/// declaration on a v2 record are refused with the same error
/// classification on both sides.
#[test]
fn error_classification_agrees() {
    let mut cases = case_matrix();
    let malformed = cases
        .iter_mut()
        .find(|c| c.record.payload_hash_hex.starts_with("not-hex"))
        .expect("matrix contains a malformed-hex case");
    assert_golden_agreement(&malformed.label, &malformed.record);

    // Declaring the base rule on a v2 record: both sides must return the
    // same error name.
    let mut rng = Lcg(0xDEAD_BEEF_0000_0042);
    let mut record = generate_record(&mut rng, 1);
    record.schema = SCHEMA_V2.to_string();
    record.v2 = Some(
        ReceiptEpochV2Builder::new(
            CeilingLevel::Green,
            ComponentLevels::uniform(AndonLevel::Green),
        )
        .build()
        .expect("epoch builds"),
    );
    record.chain_rule = Some(CHAIN_RULE_BASE.to_string());

    let wire = serde_json::to_value(&record).expect("serialize");
    let p_err = record
        .verify_chain()
        .err()
        .expect("base-on-v2 must be refused")
        .name()
        .to_string();
    let g: graphlaw::receipt_chain::ReceiptRecord =
        serde_json::from_value(wire).expect("graphlaw parses");
    let g_err = g
        .verify_chain()
        .err()
        .expect("graphlaw must also refuse base-on-v2")
        .name()
        .to_string();
    assert_eq!(p_err, g_err, "base-on-v2 refusal classification diverged");
}
