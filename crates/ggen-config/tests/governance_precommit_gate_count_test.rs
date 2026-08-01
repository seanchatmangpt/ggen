//! Governance row evidence (v26.8.1 coverage matrix, `governance` subsystem): proves
//! the "change-control rules" / "Definition of Done" claim -- that `just pre-commit` is
//! the authoritative gate chain -- by parsing the REAL `justfile` recipe line, not by
//! trusting whatever count is written in prose.
//!
//! # Real finding this test documents
//!
//! Multiple governance docs (`CLAUDE.md`'s "Definition of Done" row,
//! `.claude/rules/andon/signals.md`, `.claude/rules/README.md`, `justfile`'s own comment
//! above the recipe, `.specify/repo-facts.ttl`) previously hardcoded the gate count as
//! prose -- first "10 gates", corrected to "11", corrected again to "12" the same day a
//! 12th gate was added. Each correction required remembering to bump every doc in sync;
//! this is Contract Drift (`.claude/rules/coding-agent-mistakes.md` mistake class 5) by
//! construction, not by mistake -- a hardcoded count of something that legitimately grows
//! will always eventually go stale, no matter how carefully it's kept in sync by hand.
//!
//! 2026-08-01: removed the hardcoded count from every governance doc entirely (they now
//! point to this justfile recipe line as the sole source of truth, with no digit to go
//! stale) and from this test file (no `usize` literal count constant exists below).
//! `pre_commit_recipe_exists_and_is_non_empty` is the real evidence: it proves the parser
//! finds a real, non-trivial gate chain by reading `justfile` directly, never a count
//! someone had to remember to update.
//!
//! Chicago TDD: no mocks -- reads the real `justfile` and governance docs off disk with
//! plain text scanning.

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

/// Doc paths (relative to repo root) that must never again restate the pre-commit gate
/// count as a literal number in prose -- the recurring failure mode this test guards
/// against, having now happened three times (10 -> 11 -> 12) across these exact files.
const GOVERNANCE_DOCS_MUST_NOT_HARDCODE_COUNT: &[&str] = &[
    "CLAUDE.md",
    ".claude/rules/andon/signals.md",
    ".claude/rules/README.md",
];

/// Trailing phrases this repo has actually used to state a hardcoded gate count -- not an
/// exhaustive grammar, just the exact wording that already caused drift three times
/// (10 -> 11 -> 12). A regression guard only needs to catch a repeat of the same mistake.
const HARDCODED_COUNT_PHRASES: &[&str] = &["gates", "real checks", "chained checks"];

/// The exact, backtick-quoted anchor distinguishing a claim about `just pre-commit`'s own
/// gate count from an unrelated "N gates" claim about something else entirely -- e.g.
/// CLAUDE.md separately (and correctly) documents `.git/hooks/pre-commit` (2 gates) and
/// `.git/hooks/pre-push` (4 gates), which are different scripts with their own real,
/// independently-accurate counts and are not part of this drift class.
const JUST_PRE_COMMIT_ANCHOR: &str = "`just pre-commit`";

/// Finds every `<digits> <phrase>` occurrence (e.g. "12 gates") on the SAME LINE as a
/// `` `just pre-commit` `` mention, for any phrase in `HARDCODED_COUNT_PHRASES`. Bounded
/// to one line (not a fixed character window) so a claim about a different subject in a
/// nearby paragraph -- e.g. `.git/hooks/pre-commit` (2 gates), a different script entirely
/// -- can never leak into the window just because it happens to be close by. Plain string
/// scanning, no regex dependency needed for a check this narrow.
fn find_hardcoded_count_claims(text: &str) -> Vec<String> {
    let mut hits = Vec::new();
    let mut anchor_from = 0;
    while let Some(rel) = text[anchor_from..].find(JUST_PRE_COMMIT_ANCHOR) {
        let anchor_end = anchor_from + rel + JUST_PRE_COMMIT_ANCHOR.len();
        let line_end = text[anchor_end..]
            .find('\n')
            .map(|i| anchor_end + i)
            .unwrap_or(text.len());
        let window = &text[anchor_end..line_end];

        for phrase in HARDCODED_COUNT_PHRASES {
            let mut search_from = 0;
            while let Some(prel) = window[search_from..].find(phrase) {
                let phrase_start = search_from + prel;
                let before = &window[..phrase_start];
                let digits_end = before.trim_end().len();
                let digits_start = before[..digits_end]
                    .rfind(|c: char| !c.is_ascii_digit())
                    .map(|i| i + 1)
                    .unwrap_or(0);
                let digits = &before[digits_start..digits_end];
                let gap_is_just_whitespace = before[digits_end..].trim().is_empty();
                if !digits.is_empty() && gap_is_just_whitespace {
                    hits.push(format!("{digits} {phrase}"));
                }
                search_from = phrase_start + phrase.len();
            }
        }
        anchor_from = anchor_end;
    }
    hits
}

#[test]
fn no_governance_doc_hardcodes_a_pre_commit_gate_count() {
    // Real, current gate count -- read once, used only to report what the doc SHOULD say
    // if it must mention gates at all, never asserted to equal a literal in this file.
    let real_count = pre_commit_gate_names().len();
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");

    let mut violations = Vec::new();
    for doc in GOVERNANCE_DOCS_MUST_NOT_HARDCODE_COUNT {
        let path = repo_root.join(doc);
        let text = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {doc}: {e}"));
        for hit in find_hardcoded_count_claims(&text) {
            violations.push(format!("{doc}: {hit:?}"));
        }
    }

    assert!(
        violations.is_empty(),
        "governance doc(s) restated a hardcoded pre-commit gate count -- this is exactly \
         the drift class that already happened three times (10 -> 11 -> 12) in these same \
         files: {violations:?}. The real, current count is {real_count} (from justfile's \
         `pre-commit:` line), but do not \"fix\" this by writing {real_count} into the doc -- \
         it will go stale again the next time a gate is added. Instead, point the doc at \
         justfile's `pre-commit:` recipe as the sole source of truth, with no digit."
    );
}
