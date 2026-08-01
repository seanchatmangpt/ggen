#![allow(clippy::unwrap_used, clippy::expect_used)]
//! G6 evidence for the "products" row: default-verb compatibility law.
//!
//! Mission brief asked for the pass/fail state of
//! `generated_commands::default_verb_tests::graph_and_unknown_nouns_are_not_rewritten`
//! on current `main`. That exact test name does not exist in this repo —
//! `crates/ggen-cli/src/generated_commands.rs`'s `default_verb_tests`
//! module has `unknown_nouns_are_not_rewritten` (no `graph` case) and 3
//! other tests; all 4 currently pass (verified: `cargo test -p ggen-cli-lib
//! --lib generated_commands::default_verb_tests`). The closest real
//! analog — a test asserting `graph` is deliberately left unrewritten — is
//! `inject_default_verbs_tests::graph_has_no_default_verb_mapping_and_is_untouched`
//! in `crates/ggen-cli/src/lib.rs`, and it also currently passes.
//!
//! While chasing that down this agent found a real, distinct bug: the
//! RDF source of truth for `generated_commands.rs`'s `DEFAULT_VERBS` table
//! (`.specify/cli-proof-tests.ttl`'s `cdf:doctor` individual) declared
//! `cdf:verb "check"`, disagreeing with `lib.rs`'s live, hand-written
//! `inject_default_verbs` (called at `lib.rs:202`) and the real
//! `ggen-engine` noun surface (`crates/ggen-engine/src/verbs/doctor.rs`'s
//! `#[verb("run")]`), which both say `run`. There is no `check` verb
//! anywhere in the doctor noun.
//!
//! **Fixed:** `cdf:doctor`'s `cdf:verb` fact was corrected from `"check"` to
//! `"run"` in `.specify/cli-proof-tests.ttl`, and `crates/ggen-cli/src/
//! generated_commands.rs` was regenerated via `ggen sync run` (never
//! hand-edited — it is a GENERATED file). `generated_commands::DEFAULT_VERBS`
//! now agrees with `lib.rs`'s live mapping for every noun. The tests below
//! are updated to assert the corrected (agreeing) behavior; Chicago TDD:
//! real function calls, real `ggen` binary subprocess elsewhere in this
//! crate's test suite — no mocks.

use ggen_cli_lib::generated_commands::{default_verb, inject_default_verbs, DEFAULT_VERBS};

fn strings(values: &[&str]) -> Vec<String> {
    values.iter().map(|value| (*value).to_string()).collect()
}

/// Positive witness: the (now-fixed) compatibility table declares
/// `doctor`'s default verb as `run`, agreeing with the live `ggen-engine`
/// noun surface.
#[test]
fn generated_commands_default_verbs_table_declares_doctor_run() {
    assert_eq!(
        default_verb("doctor"),
        Some("run"),
        "generated_commands::DEFAULT_VERBS must declare (\"doctor\", \"run\") -- \
         regenerated from the corrected .specify/cli-proof-tests.ttl cdf:doctor fact"
    );
    assert!(
        DEFAULT_VERBS.contains(&("doctor", "run")),
        "DEFAULT_VERBS must literally contain the (\"doctor\", \"run\") tuple"
    );
    assert!(
        !DEFAULT_VERBS.contains(&("doctor", "check")),
        "the dead (\"doctor\", \"check\") mapping must no longer be present"
    );
}

/// Positive witness: `generated_commands`'s `inject_default_verbs` now
/// rewrites a bare `ggen doctor` into `ggen doctor run` — a real verb in
/// the live `ggen-engine` doctor noun, matching `lib.rs`'s live mapping and
/// what `cli_surface_evidence_test.rs` proves at the real binary boundary.
#[test]
fn generated_commands_inject_default_verbs_produces_the_reachable_doctor_run_verb() {
    assert_eq!(
        inject_default_verbs(strings(&["ggen", "doctor"])),
        strings(&["ggen", "doctor", "run"]),
        "the generated_commands copy of inject_default_verbs must rewrite bare `ggen doctor` \
         into `ggen doctor run`, a verb present in ggen-engine's doctor noun"
    );
}

/// Negative falsifier / current-true-state control: `sync` and `receipt`
/// default verbs DO agree between the two independent implementations
/// (`generated_commands`'s dead copy and `lib.rs`'s live copy) — proving
/// the `doctor` mismatch above is a specific, isolated divergence, not a
/// symptom of the two tables being generally unrelated or the assertions
/// above being vacuously true for any noun.
#[test]
fn sync_and_receipt_default_verbs_agree_with_the_live_lib_rs_mapping() {
    assert_eq!(default_verb("sync"), Some("run"));
    assert_eq!(default_verb("receipt"), Some("verify"));
    assert_eq!(
        inject_default_verbs(strings(&["ggen", "sync"])),
        strings(&["ggen", "sync", "run"])
    );
    assert_eq!(
        inject_default_verbs(strings(&["ggen", "receipt"])),
        strings(&["ggen", "receipt", "verify"])
    );
}

/// Positive witness for the mission's literal ask: `graph` and an unknown
/// noun are both left unrewritten by the (dead) `generated_commands`
/// implementation too — confirming this half of the described behavior is
/// real and passing today, independent of the `doctor` finding above.
#[test]
fn graph_and_unknown_nouns_are_not_rewritten_reproduced_against_generated_commands() {
    for noun in ["graph", "unknown"] {
        let argv = strings(&["ggen", noun]);
        assert_eq!(
            inject_default_verbs(argv.clone()),
            argv,
            "noun `{noun}` must be left untouched (no declared default verb)"
        );
    }
}
