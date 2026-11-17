//! Drift guard for the receipt chain-rule discriminator written into the repo's own
//! `.ggen-v2/` receipts.
//!
//! History: `graphlaw` 26.10.13 spells the v2-fold rule `graphlaw-chain/v2-fold` and does not
//! parse the legacy `praxis-chain/v2-fold` that ggen wrote before the praxis retirement. The
//! committed head + log therefore stopped verifying (`FM-CHAIN-014` / `FM-CHAIN-007`).
//! `.ggen-v2/chain-rule-migration.json` records the byte-exact rename that repaired it (chain
//! hashes unchanged). These tests pin the repaired state: every chain rule the committed
//! receipts declare is one the pinned `graphlaw` accepts, and the whole log verifies.
//!
//! Chicago TDD: real files, the real `graphlaw` parser, the real history verifier.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::receipt_chain_seam::ChainRule;
use ggen_engine::sync::{RECEIPT_LOG_REL_PATH, RECEIPT_REL_PATH};
use ggen_engine::verbs::handlers::handle_receipt_history_in;
use std::path::PathBuf;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// Every `record.chain_rule` declared by the head and by each log line.
fn declared_rules(root: &std::path::Path) -> Vec<String> {
    let mut texts = vec![std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).unwrap()];
    texts.extend(
        std::fs::read_to_string(root.join(RECEIPT_LOG_REL_PATH))
            .unwrap()
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(str::to_owned),
    );
    texts
        .iter()
        .filter_map(|t| {
            let v: serde_json::Value = serde_json::from_str(t).unwrap();
            v["record"]["chain_rule"].as_str().map(str::to_owned)
        })
        .collect()
}

#[test]
fn every_declared_chain_rule_in_committed_receipts_parses_under_pinned_graphlaw() {
    let rules = declared_rules(&repo_root());
    assert!(
        rules.len() >= 14,
        "expected the head plus at least the 13 migrated log records to declare a rule, found {}",
        rules.len()
    );
    for r in &rules {
        ChainRule::parse(r).unwrap_or_else(|e| {
            panic!("committed receipt declares chain rule `{r}` the pinned graphlaw rejects: {e}")
        });
    }
}

#[test]
fn committed_receipts_carry_no_legacy_praxis_chain_discriminator() {
    for rel in [RECEIPT_REL_PATH, RECEIPT_LOG_REL_PATH] {
        let text = std::fs::read_to_string(repo_root().join(rel)).unwrap();
        assert!(
            !text.contains("praxis-chain/"),
            "`{rel}` still declares a legacy `praxis-chain/*` rule; see \
             .ggen-v2/chain-rule-migration.json"
        );
    }
}

#[test]
fn committed_receipt_log_verifies_end_to_end_without_a_signing_key() {
    // History verification walks every record and link (no signature step), so it runs
    // anywhere. The head's Ed25519 signature needs the owner's key and is checked by
    // `ggen receipt verify`, not here.
    let out = handle_receipt_history_in(&repo_root()).expect("committed receipt log must verify");
    assert_eq!(out["valid"], true, "{out}");
    assert!(out["records"].as_u64().unwrap() >= 211, "{out}");
}

#[test]
fn negative_control_an_unrecognized_chain_rule_is_refused() {
    // Proves the checks above are live: corrupt ONE declared rule in a copy and the same
    // verifier must refuse with a typed FM-CHAIN code (not pass, not panic).
    let dir = tempfile::TempDir::new().unwrap();
    let root = dir.path();
    std::fs::create_dir_all(root.join(".ggen-v2")).unwrap();
    for rel in [RECEIPT_REL_PATH, RECEIPT_LOG_REL_PATH] {
        std::fs::copy(repo_root().join(rel), root.join(rel)).unwrap();
    }
    let log = root.join(RECEIPT_LOG_REL_PATH);
    let text = std::fs::read_to_string(&log).unwrap();
    assert!(text.contains("\"chain_rule\":\"graphlaw-chain/v2-fold\""));
    std::fs::write(
        &log,
        text.replacen("graphlaw-chain/v2-fold", "bogus-chain/v2-fold", 1),
    )
    .unwrap();
    let err = handle_receipt_history_in(root).expect_err("unknown chain rule must be refused");
    let msg = err.to_string();
    assert!(
        msg.contains("FM-CHAIN") && msg.contains("bogus-chain/v2-fold"),
        "expected a typed FM-CHAIN refusal naming the bad rule, got: {msg}"
    );
}
