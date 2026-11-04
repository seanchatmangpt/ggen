# ZD-66: doc-clippy + EOF whitespace sweep (ZB-13 items 1-2)

Date: 2026-11-04
Repo: /Users/sac/ggen @ v26-11-3-wave (58a3b4108+)
Lane: own only — CLAUDE.md (EOF strip), zd-16-eof-sweep.md:22,
crates/ggen-abb-sbb/src/depgraph.rs:345, this receipt.

## Changes

1. **CLAUDE.md EOF strip**: removed the single blank line at EOF (file now
   ends after the E0014 table row, `equivalent) |\n`). No trailing-space
   change was needed in this file — the trailing-space instance
   (`clean in git): `) was in zd-16, not CLAUDE.md.
2. **zd-16-eof-sweep.md:22**: stripped one trailing space after
   `clean in git): `.
3. **depgraph.rs:345** — `clippy::too_long_first_doc_paragraph` on
   `closure_fixpoint`: restructured so the first paragraph is a single-line
   summary (`The bounded transitive-closure fixpoint.`), blank `///` line,
   then the detail paragraph. Note: an initial attempt inserting only a
   blank `///` after the 3-line first paragraph did NOT clear the lint —
   the lint demands a single-line first paragraph, not merely a short
   first paragraph.

## Verification

- `git diff --check CLAUDE.md docs/jira/v26.11.3/receipts/zd-16-eof-sweep.md`
  → clean (exit 0).
- `cargo clippy --locked --manifest-path crates/ggen-abb-sbb/Cargo.toml
  --all-targets -- -D warnings` (matches `.github/workflows/abb-sbb-manufacture.yml:40`,
  the kernel job's invocation): the :345 lint is GONE. One lint remains at
  **depgraph.rs:38** (`ConsumerEdges` doc) — that is a DIFFERENT first-doc
  paragraph outside this lane's boundary (:345 ONLY) and is left for its
  owner.
- `cargo check --locked -p ggen-abb-sbb` → exit 0.

## Standing

ALIVE for the :345 fix and the two whitespace fixes; the crate-level clippy
gate is PARTIAL (blocked solely on :38, out of lane).
