//! Governance row evidence (v26.8.1 coverage matrix, `governance` subsystem): proves
//! the "change-control rules" / "Definition of Done" claim -- that `just pre-commit` is
//! the authoritative gate chain -- by parsing the REAL `justfile` recipe line, not by
//! trusting whatever count is written in prose.
//!
//! # Real finding this test documents
//!
//! Multiple governance docs (`CLAUDE.md`'s "Definition of Done" row,
//! `.claude/rules/andon/signals.md`, `.claude/rules/README.md`) assert `just pre-commit`
//! chains **10 gates**. Confirmed by reading `justfile`'s `pre-commit:` recipe line
//! directly (2026-07-31): it currently chains **11** dependencies --
//! `fmt-check check lint test-lib coherence-check guard-process-intelligence-boundary
//! guard-cheat-scan guard-claims-schema guard-pack-proofs guard-generation-hash-pin
//! guard-pack-count` -- `guard-pack-count` is an 11th gate not reflected in any of the
//! "10 gates" doc claims above (justfile's own inline comment on the line above the
//! recipe also still says "10 gates" -- the drift is in the source-of-truth file
//! itself, not just its downstream docs). This is real Contract Drift per
//! `.claude/rules/coding-agent-mistakes.md` mistake class 5 (a governance claim about a
//! proof-object gate chain no longer describes what actually runs) -- documented here,
//! not silently fixed, since correcting every doc that repeats the stale "10 gates"
//! figure is outside this pass's file ownership (governance/system rows only; those
//! docs are CLAUDE.md and `.claude/rules/*`, owned elsewhere per the mission brief).
//!
//! Chicago TDD: no mocks -- reads the real `justfile` off disk and parses the real
//! `pre-commit:` recipe line with plain text scanning.

use std::path::PathBuf;

fn justfile_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("justfile")
}

/// Return the whitespace-separated list of recipe names `pre-commit` depends on, parsed
/// from the real `pre-commit: <dep> <dep> ...` line in `justfile`. Panics if no such
/// line is found -- that would mean `just pre-commit` itself no longer exists, a much
/// bigger governance problem than a stale count.
fn pre_commit_gate_names() -> Vec<String> {
    let text = std::fs::read_to_string(justfile_path())
        .unwrap_or_else(|e| panic!("failed to read justfile: {e}"));
    for line in text.lines() {
        if let Some(rest) = line.strip_prefix("pre-commit:") {
            return rest.split_whitespace().map(str::to_string).collect();
        }
    }
    panic!("no `pre-commit:` recipe line found in justfile -- Definition of Done gate is missing");
}

#[test]
fn pre_commit_recipe_exists_and_is_non_empty() {
    // Positive witness: the parser finds a real, non-trivial dependency chain, not an
    // empty match from a broken prefix check.
    let gates = pre_commit_gate_names();
    assert!(
        gates.len() >= 5,
        "pre-commit recipe has suspiciously few gates ({}): {gates:?} -- parser bug or \
         real gate removal, investigate before trusting this count",
        gates.len()
    );
    assert!(
        gates.contains(&"fmt-check".to_string()),
        "expected fmt-check as a pre-commit gate, got: {gates:?}"
    );
}

#[test]
fn pre_commit_gate_count_is_eleven_not_ten() {
    // This is the real, current count -- confirmed by the positive-witness test above
    // finding the exact same recipe line. If this test starts failing because the
    // count changed again, that is real information: update this constant AND every
    // governance doc that repeats a gate count (CLAUDE.md, .claude/rules/andon/signals.md,
    // .claude/rules/README.md, justfile's own inline comment above the recipe) in the
    // same change -- do not just bump the number here and call it done.
    const CURRENT_REAL_GATE_COUNT: usize = 11;
    let gates = pre_commit_gate_names();
    assert_eq!(
        gates.len(),
        CURRENT_REAL_GATE_COUNT,
        "just pre-commit's real gate count changed from {CURRENT_REAL_GATE_COUNT} to {} \
         ({gates:?}) -- this is exactly the silent drift `.claude/rules/coding-agent-mistakes.md` \
         class 5 (Contract Drift) warns about for proof-object descriptions; update every \
         governance doc claiming a gate count, not just this constant",
        gates.len()
    );
    // Documents this repo's own multi-doc drift for the day this test's own comment
    // above goes stale: "10 gates" is what CLAUDE.md / andon/signals.md / README.md
    // still say as of 2026-07-31; it is provably wrong against the real recipe line.
    assert_ne!(
        gates.len(),
        10,
        "if this ever passes, the widely-repeated '10 gates' doc claim would be correct \
         again -- re-verify by hand, this assertion exists to make that moment visible"
    );
}
