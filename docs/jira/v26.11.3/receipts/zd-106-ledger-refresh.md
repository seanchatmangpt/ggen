# ZD-106: Wave-ledger refresh (v26.11.3) — rebuild in place, all six repos re-frozen

Standing: PASS

## Scope

Rebuilt
`docs/jira/v26.11.3/closure/wave-ledger-v26.11.3.md` in place from its own
mechanical steps (STEP 0/1/2), because the 72b743d23-era build was stale: HEAD
had moved in 5 of 6 repos and receipt counts grew in 5 of 6.

## STEP 0 deltas vs previous build (72b743d23 era)

| repo | prev HEAD | new HEAD | prev→new receipts |
|---|---|---|---|
| ggen (v26-11-3-wave) | ed3f1dfb | 9c1cf48b (58a3b410 at first capture, +1 mid-write) | 109 → 135 |
| cgap (main) | 37721e2f | b6d68517 | 63 → 77 |
| arcus (main) | 1c08fd14 | 70477e93 | 29 → 31 |
| lsp-max (master) | 7278ae77 | e23629b8 | 13 → 16 |
| claude-code-config-lsp (main) | 6a041243 (unchanged) | 6a041243 | 70 → 70 |
| ggen-marketplace (main) | 0e94adbb | 808c621c | 4 → 12 |

Tags: no repo carries a `*26.11.3*` tag (unchanged).

## Step-1 totals (new build)

- ggen 122 tokens over 135 files (13 token-less); cgap 77/77 exact;
  arcus 35/31; lsp-max 13/16 (3 token-less); claude-code-config-lsp 73/70;
  ggen-marketplace 14/12.

## Findings

1. **Mid-write HEAD move, falsifier 1 fired and was refreshed**: ggen advanced
   58a3b410 → 9c1cf48b during the build (runbook §6 PR-#824-merge commit,
   `git diff` on the receipts dir empty, STEP 1 identical 91/6/8/13/1/3, file
   count 135 before and after). The Step-0 ggen cell was refreshed to 9c1cf48b
   and the as-of block records the move. Same drift class as the previous
   build's mid-write catch — recurring, not one-off.
2. **Dirty-tree provenance**: ggen's tree was dirty pre-sweep — 244
   modified/untracked paths as-of-disk at capture. Step-1/Step-2 ggen counts
   are as-of-disk, not as-of-HEAD; falsifier 7 (new) pins this.
3. **RECEIPTS.md index set changed**: ggen-marketplace now carries
   `docs/jira/v26.11.3/RECEIPTS.md` (previous build recorded it ABSENT);
   cgap (77 receipts) is the largest remaining gap.
4. **Sum-vs-filecount finding sharpened**: the imbalance now runs both ways —
   ggen 122/135 and lsp-max 13/16 are token-deficient (13 and 3 token-less
   receipts, visible as NONE rows in Step 2), while arcus 35/31,
   claude-code-config-lsp 73/70 and ggen-marketplace 14/12 are token-surplus
   (double-token first lines). Falsifier 4 rewritten to record both directions.
5. **Self-check diff EMPTY (PASS)**: fresh STEP 0 re-run after the refresh
   diffs empty against the written Step-0 table (evidence:
   `/tmp/zd106-step0.txt` vs `/tmp/zd106-ledger.txt`, diff exit 0).
   Re-passed after the ledger was refreshed to include this receipt itself
   (ggen 135 → 136 files; the receipt's landing invalidated its own frozen
   count — refresh, not edit-stale).
6. **Rebuild determinism PASS**: STEP 2 re-extraction reproduces all six
   ledger tables byte-for-byte (evidence: `/tmp/zd106-step2.txt` vs
   `/tmp/zd106-step2-ledger.txt`, diff exit 0) — re-passed post-refresh.

## Verification ladder

- STEP 0 freeze re-run 3× (initial capture, mid-write re-run, post-write
  self-check) — final diff empty.
- STEP 1 uniq -c re-run for ggen post-move — identical.
- STEP 2 extraction re-run — byte-identical.
- Ledger is uncommitted (per instructions); working tree only.

## Standing

ALIVE — the ledger's own falsifiers 1 and 5 executed against it and passed;
falsifier 1 additionally caught a real mid-write move this build, proving the
gate non-vacuous.
