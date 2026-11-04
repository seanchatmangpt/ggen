//! Chicago-TDD proofs for the DEF-K6-1 chain-rule compat window: the
//! repo-root ledger's records 199-214 declare the legacy
//! `praxis-chain/v2-fold` discriminator, which the engine's
//! graphlaw-backed verifier refused (FM-CHAIN-007/014) before Stage 2c
//! even ran. The fixture is the VERBATIM ledger bytes
//! (`tests/fixtures/receipt_chain_legacy_tail.jsonl`), so every assertion
//! below is a witness over real emitted records, not a synthetic
//! construction. No mocks.
//!
//! Law under test (see `receipt_chain_seam::CHAIN_RULE_V2_FOLD_LEGACY`):
//! read-side compat window only — legacy discriminator aliases to
//! [`ChainRule::V2Fold`] (same fold, renamed crate prefix), writes stay
//! `graphlaw-chain/v2-fold`, tamper still refuses.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::receipt_chain_seam::{
    verify_chain_compat, ChainStanding, ChainVerification, ReceiptRecord,
    CHAIN_RULE_V2_FOLD, CHAIN_RULE_V2_FOLD_LEGACY,
};
use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};

/// The verbatim repo-root ledger tail: records 199-214, all sealed under
/// the legacy discriminator, chained contiguously.
const LEGACY_TAIL: &str = include_str!("fixtures/receipt_chain_legacy_tail.jsonl");

/// The verbatim FULL repo-root ledger (records 1-214, 4.6 MB): the exact
/// DEF-K6-1 shape — 198 pre-F1 records with no declared rule, then the 16
/// legacy `praxis-chain/v2-fold` records. Used for the CLI-boundary
/// end-to-end because `receipt history` requires a genesis-rooted ledger
/// (a truncated tail is refused at index 0 — itself fail-closed behavior).
const FULL_LEDGER: &str = include_str!("fixtures/receipt_chain_full_ledger.jsonl");

fn legacy_records() -> Vec<ReceiptRecord> {
    LEGACY_TAIL
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            let receipt: SyncReceipt = serde_json::from_str(l).expect("parse fixture line");
            receipt.record
        })
        .collect()
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().fold(String::new(), |mut h, b| {
        use std::fmt::Write as _;
        let _ = write!(h, "{b:02x}");
        h
    })
}

/// The defect as witnessed by DEF-K6-1: the raw graphlaw-backed verifier
/// refuses the legacy discriminator outright — this is the refusal the
/// compat window exists to close, kept as a regression pin.
#[test]
fn legacy_discriminator_is_refused_by_the_raw_verifier() {
    for record in legacy_records() {
        assert_eq!(record.chain_rule.as_deref(), Some(CHAIN_RULE_V2_FOLD_LEGACY));
        let err = record
            .verify_chain()
            .expect_err("raw verifier must refuse the legacy discriminator");
        let msg = err.to_string();
        assert!(
            msg.contains("unrecognized chain rule") && msg.contains(CHAIN_RULE_V2_FOLD_LEGACY),
            "refusal must name the unrecognized rule, got: {msg}"
        );
    }
}

/// THE fold-identity witness: every legacy-rule record from the real
/// ledger recomputes its stored chain hash exactly under `V2Fold` once the
/// discriminator is aliased. If praxis's fold had differed
/// cryptographically from graphlaw's, this would Mismatch — the alias
/// would be unlawful and migration-forbidden instead.
#[test]
fn every_legacy_ledger_entry_recomputes_under_the_v2_fold() {
    for (idx, record) in legacy_records().into_iter().enumerate() {
        match verify_chain_compat(&record).expect("compat verify fixture line {idx}") {
            ChainVerification::Verified(standing) => {
                assert_eq!(
                    standing,
                    ChainStanding::FullyBound,
                    "legacy record {idx} carries a v2 payload folded into its chain hash"
                );
            }
            ChainVerification::Mismatch { rule, recomputed } => {
                panic!(
                    "legacy record {idx}: stored chain hash {} does not recompute under \
                     `{}` (recomputed {}): the folds DIFFER and the alias is unlawful",
                    record.chain_hash_hex,
                    rule.as_str(),
                    hex(&recomputed)
                );
            }
        }
    }
}

/// A tampered legacy entry still refuses: the compat window widens the
/// ACCEPTED DISCRIMINATOR, never the verification. Mutating a
/// hash-relevant field (a v2 admission reason) flips Verified → Mismatch.
#[test]
fn tampered_legacy_entry_refuses_under_compat() {
    let mut value: serde_json::Value =
        serde_json::from_str(LEGACY_TAIL.lines().next().expect("non-empty fixture")).expect("parse");
    // Tamper a field the v2-fold covers (inside the v2 epoch payload).
    let reason = value["record"]["v2"]["admission"]["Recorded"][0]["reason"]
        .as_str()
        .expect("fixture admission reason")
        .to_string();
    value["record"]["v2"]["admission"]["Recorded"][0]["reason"] =
        serde_json::Value::String(format!("{reason} TAMPERED"));
    let line = serde_json::to_string(&value).expect("serialize tampered");
    let receipt: SyncReceipt = serde_json::from_str(&line).expect("parse tampered");

    match verify_chain_compat(&receipt.record).expect("compat verify runs") {
        ChainVerification::Verified(_) => {
            panic!("tampered legacy entry must NOT verify under the compat window");
        }
        ChainVerification::Mismatch { .. } => {
            // fail-closed, exactly as required
        }
    }
}

/// End-to-end over the real write path: a project whose ledger tail is the
/// legacy fixture extends the chain (sync chains onto the legacy head
/// instead of refusing with FM-CHAIN-009), the NEW record declares the
/// current rule, and the whole ledger — legacy prefix + new records —
/// verifies.
#[test]
fn legacy_ledger_tail_extends_and_the_whole_chain_verifies() {
    use chicago_tdd_tools::cli_proof::CliHarness;
    use std::path::Path;

    fn ggen_bin() -> std::path::PathBuf {
        if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
            let p = std::path::PathBuf::from(path);
            if p.exists() {
                return p;
            }
        }
        let manifest_dir = std::path::PathBuf::from(
            std::env::var_os("CARGO_MANIFEST_DIR")
                .expect("CARGO_MANIFEST_DIR is set under cargo test"),
        );
        let mut dir: &std::path::Path = manifest_dir.as_path();
        loop {
            if dir.join("Cargo.lock").exists() {
                break;
            }
            match dir.parent() {
                Some(p) => dir = p,
                None => panic!("could not resolve the workspace target dir for the ggen binary"),
            }
        }
        for profile in &["debug", "release"] {
            let candidate = dir.join("target").join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
        }
        panic!(
            "could not resolve the `ggen` binary; build it with \
             `cargo build -p ggen-cli-lib --bin ggen`"
        );
    }

    fn scaffold(root: &Path) {
        std::fs::write(
            root.join("ggen.toml"),
            "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n\
             [templates]\ndir = \"templates\"\n",
        )
        .expect("write ggen.toml");
        std::fs::write(
            root.join("ontology.ttl"),
            "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n",
        )
        .expect("write ontology");
        std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
        std::fs::write(
            root.join("templates/one.tmpl"),
            "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { \
             ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n\
             {% for row in results %}{{ row.name }}\n{% endfor %}",
        )
        .expect("write template");
    }

    let dir = tempfile::TempDir::new().expect("tempdir");
    scaffold(dir.path());

    // Seed the ledger with the verbatim FULL ledger + matching head
    // pointer (the exact DEF-K6-1 repo-root shape).
    let ledger: Vec<&str> = FULL_LEDGER.lines().filter(|l| !l.trim().is_empty()).collect();
    let legacy_tail = ledger.last().expect("non-empty fixture");
    let v2dir = dir.path().join(".ggen-v2");
    std::fs::create_dir_all(&v2dir).expect("mkdir .ggen-v2");
    std::fs::write(v2dir.join("receipt-log.jsonl"), FULL_LEDGER).expect("seed full ledger");
    std::fs::write(v2dir.join("receipt.json"), legacy_tail).expect("seed legacy head");

    // Extend the chain over the legacy head — refused FM-CHAIN-009 before
    // the compat window.
    sync(
        dir.path(),
        SyncOptions {
            consumer_mode: Default::default(),
            dry_run: false,
            skip_ea_admission: true,
            ..Default::default()
        },
    )
    .expect("sync must extend the legacy head");

    let log: Vec<SyncReceipt> = std::fs::read_to_string(v2dir.join("receipt-log.jsonl"))
        .expect("read log")
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(l).expect("parse log line"))
        .collect();
    assert_eq!(
        log.len(),
        ledger.len() + 1,
        "exactly one new record appended"
    );

    // The new head chains onto the legacy tail…
    let new_rec = &log[ledger.len()].record;
    assert_eq!(
        new_rec.prev_chain_hash_hex,
        log[ledger.len() - 1].record.chain_hash_hex,
        "new record chains onto the legacy tail"
    );
    // …and WRITES the current rule, never the legacy form.
    assert_eq!(
        new_rec.chain_rule.as_deref(),
        Some(CHAIN_RULE_V2_FOLD),
        "new writes must use the current rule"
    );
    assert_ne!(new_rec.chain_rule.as_deref(), Some(CHAIN_RULE_V2_FOLD_LEGACY));

    // The whole ledger — legacy prefix + current-rule head — verifies
    // under the compat window.
    for (idx, receipt) in log.iter().enumerate() {
        match verify_chain_compat(&receipt.record).expect("compat verify") {
            ChainVerification::Verified(_) => {}
            ChainVerification::Mismatch { rule, recomputed } => panic!(
                "record {idx} mismatches under `{}` (recomputed {})",
                rule.as_str(),
                hex(&recomputed)
            ),
        }
    }

    // Adjacent links hold across the legacy → current boundary.
    for w in log.windows(2) {
        assert_eq!(
            w[0].record.chain_hash_hex, w[1].record.prev_chain_hash_hex,
            "chain link intact across the legacy → current boundary"
        );
    }

    // Full-history verification passes at the CLI boundary.
    let _ = CliHarness::from_path(ggen_bin())
        .args(["receipt", "history"])
        .current_dir(dir.path())
        .run()
        .expect("history run")
        .assert_success();

    // A fresh project's FIRST record also declares the current rule.
    let fresh = tempfile::TempDir::new().expect("tempdir");
    scaffold(fresh.path());
    sync(
        fresh.path(),
        SyncOptions {
            consumer_mode: Default::default(),
            dry_run: false,
            skip_ea_admission: true,
            ..Default::default()
        },
    )
    .expect("fresh sync");
    let fresh_head: SyncReceipt =
        serde_json::from_str(&std::fs::read_to_string(fresh.path().join(RECEIPT_REL_PATH)).expect("read head"))
            .expect("parse head");
    assert_eq!(
        fresh_head.record.chain_rule.as_deref(),
        Some(CHAIN_RULE_V2_FOLD),
        "fresh writes use the current rule"
    );
}
