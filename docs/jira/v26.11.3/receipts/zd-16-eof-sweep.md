# ZD-16: Repo-wide EOF whitespace sweep (tracked .md)

Standing: ALIVE

Date: 2026-11-04. Lane: ZD-16. Branch: v26-11-3-wave.

## Scope

Tracked markdown only (`git ls-files '*.md'`). Mechanical EOF normalization:
strip trailing blank lines and trailing whitespace so every file ends with
exactly one trailing newline (POSIX style). No content edits, no non-md files,
no commit (per lane contract).

## Method

1. Candidates: tracked .md files whose bytes end with (a) no trailing newline,
   (b) more than one trailing newline, or (c) trailing whitespace on the final
   line. Byte-level check in Python (`rstrip(b'\n')` length delta + final-line
   rstrip compare).
2. Exclusions applied before fix:
   - Files with uncommitted non-whitespace changes (git diff -w non-quiet):
     `book/src/CAPABILITY_MAP.md`, `docs/jira/v26.11.3/closure/descope-ledger.md`,
     `examples/zero-code-inventoryctl/docs/command-reference.md` — SKIP-CONTENT.
   - Recent-mtime guard (modified within 30 min, clean in git):
     `examples/zero-code-inventoryctl/docs/SCHEMA_ADMITTED.md` — SKIP-RECENT
     (possible in-flight other-lane edit).
3. Fix: rewrite last-line/EOF bytes only; interior lines untouched.

## Receipt

- Files fixed: 280
- Diff classes (numstat): 0/N = trailing blank lines removed; 1/1 = missing
  final newline added (git shows last line as changed with
  `\ No newline at end of file`); 1/2 = both classes in one file.
- Non-md files touched: 0

## Verification (actual output)

- `bash /tmp/zd16_sweep.sh` re-run after fix: 0 candidates
  (`wc -l /tmp/zd16_rerun.txt` = 0).
- `git diff --check -- '*.md'`: clean for all fixed files; sole remaining
  complaint is `CLAUDE.md:70: new blank line at EOF` — that file is excluded
  (uncommitted non-whitespace change in flight by another lane, the
  GENERATED/MANUAL marker removal) and its diff shows 4 deletions only, none
  mine.
- Whitespace-only audit: for each fixed file, every +/- diff line is either
  the final line re-emitted with a newline (content byte-identical) or a
  removed blank/trailing-whitespace line. Sample:
  `git diff -- docs/README.md` ends
  `-...PLANNING (proposal) \ No newline at end of file` /
  `+...PLANNING (proposal)` — no content change.
- Non-fixed files in the working tree (README.md, RECEIPTS.md,
  CAPABILITY_MAP.md, CLAUDE.md, ontology.ttl, workflows, gap-closure.toml,
  receipt-log.jsonl) carry other lanes' in-flight content changes; confirmed
  none appear in the fix list (`grep -E 'docs/jira/v26.11.3/(README|RECEIPTS)'` /tmp/zd16_fix.txt`
  → no match).

## Standing

ALIVE on the verified surface: 280 tracked .md files, EOF normalized, sweep
re-run returns 0 candidates, diff verified whitespace-only. Not committed.
