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
//! # Crate-count drift (added 2026-08-03)
//!
//! A second, independent instance of the same Contract Drift class: `Cargo.toml`'s
//! `[workspace] members = [...]` array, `CLAUDE.md`'s prose (both the top-line "N-crate
//! workspace" summary and the GENERATED "Crate Map (N workspace crates)" header), and
//! `.claude/rules/architecture.md`'s "Crate Map (N workspace members)" header disagreed --
//! variously 17, 18, or 19 depending which doc you read -- because `ggen-mcp` had been a
//! real `Cargo.toml` member for a while before `.specify/repo-facts.ttl` (the RDF source
//! both docs are generated from) grew a matching `rf:Crate` individual, and because
//! `crates/ggen-architecture` (an independent nested Cargo workspace -- it declares its
//! own `[workspace]` table -- living in neither `Cargo.toml`'s `members` nor `exclude`)
//! made "how many crates are in this workspace" ambiguous to answer by directory listing
//! alone. Unlike the gate-count fix above, this test does NOT ban hardcoding the number in
//! prose (crate count is meaningful, reader-facing context, not just an internal gate
//! chain) -- it cross-checks every hardcoded occurrence against the real, freshly-parsed
//! `Cargo.toml` count and fails loudly the moment any doc drifts again.
//!
//! Chicago TDD: no mocks -- reads the real `justfile`, `Cargo.toml`, and governance docs
//! off disk with plain text scanning.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::PathBuf;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn justfile_path() -> PathBuf {
    repo_root().join("justfile")
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
        let text =
            std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("failed to read {doc}: {e}"));
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

// ---------------------------------------------------------------------------
// Crate-count drift (2026-08-03): see the module-level doc comment above.
// ---------------------------------------------------------------------------

/// Absolute path to the real workspace root `Cargo.toml`.
fn cargo_toml_path() -> PathBuf {
    repo_root().join("Cargo.toml")
}

/// Count of `"crates/<name>",` entries inside `Cargo.toml`'s `[workspace] members = [...]`
/// array. Deliberately independent of `system_crate_map_parity_test.rs`'s own parser (a
/// different test binary, no shared helper crate to import from) but the same parsing
/// strategy: line-oriented scanning between the `members = [` and matching `]` lines,
/// counting only literal `"crates/` entries so a commented-out line (`# "examples/..."`)
/// is correctly not counted.
fn cargo_toml_workspace_member_count() -> usize {
    let text = std::fs::read_to_string(cargo_toml_path())
        .unwrap_or_else(|e| panic!("failed to read root Cargo.toml: {e}"));
    let mut in_workspace_members = false;
    let mut count = 0usize;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed == "members = [" {
            in_workspace_members = true;
            continue;
        }
        if in_workspace_members {
            if trimmed.starts_with(']') {
                break;
            }
            if trimmed.starts_with("\"crates/") {
                count += 1;
            }
        }
    }
    count
}

/// The single real "how many crates are in this workspace" number every doc claim below
/// must agree with: every `[workspace] members` entry, plus the root `ggen` package
/// (defined by root `Cargo.toml`'s own `[package]` table, not a `members` entry) -- the
/// same "members + 1" convention this repo's docs have used since the crate map was first
/// written (see `.claude/rules/architecture.md`'s own history notes). `ggen-architecture`
/// is correctly excluded from this count: it declares its own `[workspace]` table, making
/// it an independent nested Cargo workspace rather than a member of this one, and it lives
/// in `Cargo.toml`'s `[workspace] exclude`, not `members`.
fn real_workspace_crate_count() -> usize {
    cargo_toml_workspace_member_count() + 1
}

/// Governance docs that state the workspace crate count in prose. Unlike
/// `GOVERNANCE_DOCS_MUST_NOT_HARDCODE_COUNT` above, a hardcoded number here is fine --
/// crate count is reader-facing orientation, not an internal gate chain -- as long as it
/// agrees with the real count. `README.md` was checked (2026-08-03) and makes no
/// crate-count claim of its own (no `<digits>-crate`/`<digits> workspace crates`/`<digits>
/// workspace members` phrase anywhere in it); it is deliberately left off this list rather
/// than added with nothing for the scanner to find, since the sanity test below (which
/// asserts real matches were found in every guarded doc) would then fail against it for
/// the wrong reason. Add it here the day it ever states a crate count.
const CRATE_COUNT_DOCS: &[&str] = &["CLAUDE.md", ".claude/rules/architecture.md"];

/// Trailing phrases this repo has actually used to state the workspace crate count --
/// not an exhaustive grammar, just the exact wordings already seen to drift (17 / 18 / 19
/// depending which doc and which day). `-crate workspace` binds its number with a hyphen,
/// directly adjacent (no space); the other two are space-separated -- `find_crate_count_
/// claims` handles both by trimming trailing whitespace (zero or more) before reading
/// digits off the end, rather than assuming a fixed gap.
const CRATE_COUNT_PHRASES: &[&str] = &["-crate workspace", "workspace crates", "workspace members"];

/// Finds every `<digits><phrase>` occurrence (e.g. `19-crate workspace`, `(19 workspace
/// crates`) anywhere in `text`, for any phrase in `CRATE_COUNT_PHRASES`. No anchor is
/// needed here (unlike `find_hardcoded_count_claims` above, which is scoped to lines
/// mentioning `` `just pre-commit` ``): a crate-count claim is not confusable with an
/// unrelated "N workspace crates/members" claim about something else, so a plain
/// whole-document scan is precise enough. A phrase preceded by non-digit text (e.g. "all
/// workspace crates") correctly yields no match, because the digit-extraction step reads
/// zero digits and the result is discarded.
fn find_crate_count_claims(text: &str) -> Vec<(usize, String)> {
    let mut hits = Vec::new();
    for phrase in CRATE_COUNT_PHRASES {
        let mut search_from = 0;
        while let Some(rel) = text[search_from..].find(phrase) {
            let phrase_start = search_from + rel;
            let before = &text[..phrase_start];
            let trimmed_before = before.trim_end();
            let gap = &before[trimmed_before.len()..];
            if gap.chars().all(char::is_whitespace) {
                let digits_start = trimmed_before
                    .rfind(|c: char| !c.is_ascii_digit())
                    .map(|i| i + 1)
                    .unwrap_or(0);
                let digits = &trimmed_before[digits_start..];
                if !digits.is_empty() {
                    if let Ok(n) = digits.parse::<usize>() {
                        hits.push((n, format!("{digits}{phrase}")));
                    }
                }
            }
            search_from = phrase_start + phrase.len();
        }
    }
    hits
}

#[test]
fn real_workspace_crate_count_is_sane() {
    // Positive witness: the parser finds real, known members (not zero from a parsing
    // bug) before its count is trusted as the ground truth the docs are checked against.
    let count = real_workspace_crate_count();
    assert!(
        count > 10,
        "real_workspace_crate_count() computed a suspiciously small number ({count}) -- \
         parser bug (members array format changed?) or a real, drastic crate removal; \
         investigate before trusting this count"
    );
}

#[test]
fn no_governance_doc_disagrees_with_real_workspace_crate_count() {
    let real_count = real_workspace_crate_count();
    let root = repo_root();

    let mut violations = Vec::new();
    let mut total_claims_found = 0usize;
    for doc in CRATE_COUNT_DOCS {
        let path = root.join(doc);
        let text =
            std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("failed to read {doc}: {e}"));
        let claims = find_crate_count_claims(&text);
        total_claims_found += claims.len();
        for (n, phrase) in claims {
            if n != real_count {
                violations.push(format!(
                    "{doc}: claims {phrase:?} but real count is {real_count}"
                ));
            }
        }
    }

    // Positive witness for the scanner itself: if this ever drops to 0, the phrase list
    // stopped matching real prose (a parser regression), not proof every doc went quiet.
    assert!(
        total_claims_found > 0,
        "found zero crate-count claims across {CRATE_COUNT_DOCS:?} -- scanner regression, \
         not evidence the docs stopped mentioning a crate count"
    );

    assert!(
        violations.is_empty(),
        "governance doc(s) disagree with the real workspace crate count ({real_count}, from \
         Cargo.toml's [workspace] members array + the root `ggen` package) -- this is the \
         same Contract Drift class as the pre-commit gate count above, previously seen as a \
         real 17 vs 18 vs 19 disagreement across CLAUDE.md and .claude/rules/architecture.md: \
         {violations:?}"
    );
}
