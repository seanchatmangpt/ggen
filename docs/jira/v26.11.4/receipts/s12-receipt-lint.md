# S-12 — Receipt-Lint Standing Vocabulary (Gate 3)

Standing: PARTIAL_ALIVE

## Scope note (dispatch/spec ID conflict, flagged)

Lane dispatch names this lane S-12 = Gate 3 (receipt_lint.py); the spec's SEED
table labels Gate 3 as S-13 and S-12 as the index-disclosure hook. Executed the
dispatch: Gate 3, `scripts/receipt_lint.py`.

## Files owned

- `scripts/receipt_lint.py` (pre-existing from SD-21/O3-3, Nov 4; this lane
  extended it — see deltas)
- `justfile` (added `lint-receipts` alias recipe, line ~163)
- `receipts/receipt-standing-map.json` (regenerated with pins)
- `docs/jira/v26.11.4/receipts/s12-receipt-lint.md` (this receipt)

## Deltas this lane

1. `UNVERIFIED` added to `STANDING_VOCAB` (receipt-standing-law canonical
   value; was missing — a `Standing: UNVERIFIED` receipt previously FAILed).
2. Standing-map entries now carry a `pin` (mtime + sha256) per file, per the
   Gate 3 census-extension spec: the census re-flags only files whose standing
   line changed since the last map write, never files that already parse.
3. `just lint-receipts` recipe wired (alias of `receipt-lint`).

## Falsification (all commands run this session)

- Fixture corpus `/tmp/s12fix/` (7 files):
  - `pass-canonical.md` (Standing: ALIVE) — PASS
  - `pass-synonym.md` (Status: PASS) — PASS (synonym accepted)
  - `pass-annotation.md` (verdict: VERIFIED(9/9)) — PASS (parens stripped)
  - `pass-unverified.md` (Standing: UNVERIFIED) — PASS (new vocab value)
  - `fail-missing.md` (no standing line) — FAIL, exit 1
  - `fail-badvocab.md` (Standing: mostly-works) — FAIL with accepted list, exit 1
  - `backfillable.md` (bare `ALIVE` in first 5 lines) — backfill proposal
    emitted as diff; sha256 byte-identical before/after → never writes.
- Exit-code synonym: `Exit-code: N` derives `Exit-code-N` (spec family).

## Real-corpus anti-vacuity run

`just lint-receipts` (2026-11-05): **163 files, 23 parse OK, 140 FAIL,
exit 1.** The gate is red on the live corpus — the ~140 receipts missing
canonical standing lines are the same class as the E-y2/P-p ~60-receipt
finding (mostly `docs/jira/v26.11.3/receipts/`, plus 6 in v26.11.4). Backfill
of those receipts is a separate lane (D108's backfill ran in-session, external
to this repo). Map hash pinned: `e9ccd7059db40182`.

## Not done / boundary

- No git commit (per lane contract).
- 140 standing-line FAILs left as-is — repairing the corpus was not this
  lane's gate; the gate is exit-coded and loud.
- `/tmp/s12fix/` fixtures are scratch, not repo surface.
