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
//! While chasing that down this agent found a real, distinct bug (not
//! silently fixed — `crates/ggen-cli/src/**` is out of scope for this
//! agent): `generated_commands.rs` defines its OWN, second
//! `inject_default_verbs`/`DEFAULT_VERBS` pair, independent of the one
//! `main` actually dispatches through (`lib.rs`'s `inject_default_verbs`,
//! called at `lib.rs:202`). The two disagree on `doctor`'s default verb:
//! `generated_commands::DEFAULT_VERBS` says `("doctor", "check")`;
//! `lib.rs`'s live mapping (and the real `ggen-engine` noun surface,
//! `crates/ggen-engine/src/verbs/doctor.rs`'s `#[verb("run")]`) says
//! `run`. There is no `check` verb anywhere in the doctor noun. This test
//! file proves both halves of that claim from the public, test-only
//! surface available to this agent (no source-file edits): the *unit*
//! function in `generated_commands` really does compute the
//! wrong/unreachable verb, while `cli_surface_evidence_test.rs` in this
//! same directory separately proves the real binary uses the *correct*
//! verb (`run`) and that `doctor check` genuinely fails at the CLI
//! boundary. Chicago TDD: real function calls, real `ggen` binary
//! subprocess elsewhere in this crate's test suite — no mocks.

use ggen_cli_lib::generated_commands::{default_verb, inject_default_verbs, DEFAULT_VERBS};

fn strings(values: &[&str]) -> Vec<String> {
    values.iter().map(|value| (*value).to_string()).collect()
}

/// Positive witness: the currently-declared (dead) compatibility table
/// really does claim `doctor`'s default verb is `check`.
#[test]
fn generated_commands_default_verbs_table_declares_doctor_check() {
    assert_eq!(
        default_verb("doctor"),
        Some("check"),
        "generated_commands::DEFAULT_VERBS currently declares (\"doctor\", \"check\") -- \
         if this assertion starts failing, the table has been corrected upstream and this \
         evidence file (and its doc comment) should be revisited, not just re-asserted"
    );
    assert!(
        DEFAULT_VERBS.contains(&("doctor", "check")),
        "DEFAULT_VERBS must literally contain the (\"doctor\", \"check\") tuple"
    );
}

/// Positive witness: the same (dead) function's `inject_default_verbs`
/// rewrites a bare `ggen doctor` into `ggen doctor check` — a verb that
/// does not exist in the live `ggen-engine` doctor noun (only `run`
/// does), and that `cli_surface_evidence_test.rs` proves fails at the
/// real binary boundary. This is the concrete, executable evidence behind
/// the "dead code with a wrong mapping" finding — not merely inferred
/// from reading the source.
#[test]
fn generated_commands_inject_default_verbs_produces_the_unreachable_doctor_check_verb() {
    assert_eq!(
        inject_default_verbs(strings(&["ggen", "doctor"])),
        strings(&["ggen", "doctor", "check"]),
        "the generated_commands copy of inject_default_verbs rewrites bare `ggen doctor` \
         into `ggen doctor check`, a verb absent from ggen-engine's doctor noun"
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
