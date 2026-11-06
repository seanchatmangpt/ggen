# ZB-75 Receipt — v26.11.4 PRD materialization (ZP-23 plan)

Standing: UNSUPPORTED

Date: 2026-11-04. Lane: ZB-75 (docs lane). Repo: /Users/sac/ggen,
branch v26-11-3-wave. Never ran destructive git; one pathspec-scoped commit +
push.

## Subject

- docs/jira/v26.11.4/PRD.md (NEW) — the v26.11.4 PRD with corrections C1-C6
  baked in.
- docs/jira/v26.11.3/receipts/zb-75-prd-materialize.md (NEW) — this receipt.

## Corrections baked in

- **C1**: receipt figure re-surfaced to "chained BLAKE3 receipt 456.3 ns
  (AoqChain::append, ab-7 law)" (af-35); the bare-hash 167.0 ns median
  (af-22) kept as a LABELED appendix row only, per af-35's
  no-landed-surface ruling.
- **C2**: CUJ-3's "sub-10 µs ceiling" replaced by the honest af-37 matrix
  (I2 MEETS 1.25-3.875 µs, I3 MEETS 167-291 ns, I4 MEETS 583-584 ns with
  parity fence 0.93-1.00 ≤ 1.5x, I1 UNMET 26,250-281,667 ns vs <850 ns with
  the digest-floor root cause ~4.5 µs of the 64B figure, I5 1.27-5.10 ms
  e2e below-band); CUJ-1's 167.0 ns corrected likewise (C1).
- **C3**: gate ladder uses REAL names only — CBMQS G1-G7 + the 8th
  annotation row (`gates.len()==8`, dod-009), RFC-0089 §6 G1-G7 scorecard
  (RX-3), cgap dod-001..012 (adopt-and-extend per ZB-70; wholesale
  replacement would silently drop dod-001/007/008/009/010). Proposed new
  gates separated into §3.4: Gate 09 (court-to-add, ZP-20), Gate 10
  (UNSUPPORTED at 25.5202 µs/link pending ZP-17), plus CODEC-BUDGET /
  SLAB-INGESTION / HEAP-CEILING / TRANSPORT-UDS / PROCESS-CHURN /
  AUDIT-CHAIN citing the ZB-70 map (3 renames, 3 partials, 6 new).
- **C4**: Gate-09 carried as PROPOSED court-to-add per ZP-20 (3-run rerun
  decides in-place tighten vs new isolated court; "streams"→"requests"
  wording fix; quiet-machine poll `pgrep -fl lsp-echo-server`, the
  `pgrep -f "cargo test"` self-match lesson from zd-8).
- **C5**: ARD labeling nits: digest-cover-span vs invariant-header
  distinguished; metadata-tail pad vs body-tail pad distinguished (ZB-41).
- **C6**: Gate 10 carried UNSUPPORTED at 25.5202 µs/link (ab-14: 50k span
  1276.01 ms / 50,000, 2.08% over) pending the ZP-17 settling experiment;
  AB-14 receipt immutable, 3 §6 consumers move atomically.
  > ERRATA 2026-11-04: superseded by ZP-17 branch-(a) MEASURED-MET
  > 1.26394 µs/link (fenced release min-of-7, POST-FOLD) —
  > `cgap/docs/jira/v26.11.4/receipts/zp-17-gate10-settling.md`; v4 ledger
  > (`ggen/docs/jira/v26.11.4/receipts/v4-corrected-claims-ledger.md` row 5).

## Receipts verified on disk before citing

All of the following were read/grepped on disk this session (not cited from
session memory):

- /Users/sac/cgap/docs/jira/v26.11.3/receipts/ab-7-chain-session.md
- /Users/sac/cgap/docs/jira/v26.11.3/receipts/ab-12-blake3-stress.md (cited
  inside af-37's digest-floor paragraph; file listed in the cgap receipts
  dir listing this session)
- ab-12's 1768 MiB/s figure re-read from af-22's coherence section on disk
- all paths in PRD §5 provenance table verified with `ls`/`find` this
  session:
  ab-7, ab-12, ab-14 (ggen), ab-17 (lsp-max), ab-19, af-10, af-22, af-25,
  af-26, af-35, af-37, zd-8, zd-36 — all exist at the exact PRD §5 paths.

## Falsifier sweeps (real output, run at materialization)

Filters encode the label law; the sweep section's own self-description lines
are excluded (`Sweep|grep -`).

```
$ grep -n '167\.0' docs/jira/v26.11.4/PRD.md | \
    grep -v 'appendix\|af-22\|bare\|CONTRADICTED\|CUJ-1\|Sweep\|grep -'
```
→ zero hits.

```
$ grep -ni 'sub-10' docs/jira/v26.11.4/PRD.md | \
    grep -v 'CONTRADICTED\|C2\|Sweep\|grep -'
```
→ zero hits.

```
$ grep -n 'Gate-09\|Gate 09' docs/jira/v26.11.4/PRD.md | \
    grep -v 'court-to-add\|PROPOSED\|C4\|ZP-20\|Sweep\|grep -'
```
→ zero hits.

## Standing

ALIVE (2026-11-04): PRD.md written and sweep-verified this session; both
files committed pathspec-scoped to v26-11-3-wave and pushed. The PRD itself
is a requirements document — its figures are as-receipted, its proposed
gates are PROPOSED, and the wave plans remain UNVERIFIED until executed
(PLANS.md header law).
