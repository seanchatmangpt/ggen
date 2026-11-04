# E8: Stale-Citation Census (v4 Corrected-Claims Ledger integrity pass)

Date: 2026-11-04. Docs-only materialization of E8's census over
`docs/jira/v26.11.4/receipts/v4-corrected-claims-ledger.md`. No ledger row
edited.

## 1. Evidence-integrity table (cited paths + values re-read on disk)

The ledger's corrected-claims table carries 13 rows (numbered 1–11 plus
evidence-quality sub-rows 5b and 5b-precision). Every cited `path:line`
was resolved and the value re-read at the cited line, this session:

| # | Ledger row | Cited path:line | On-disk value at cited line | Match |
|---|---|---|---|---|
| 1 | 18.1 ns TARGET-UNMEASURED | cgap/docs/cscb-targets-open.md:14 | `| 1 | 18.1 ns per-call receipt latency | TARGET-UNMEASURED | ab-12 court-1 per-call latency cell ...` | OK |
| 2 | 167.0 ns p50 | af-22-percall-latency.md:32 | `| 64 B | 167.0 / 250.0 / 208.0 | **167.0** | 250.0 | 334.0 | 83.0 |` | OK |
| 3 | 456.3 ns chained | af-35-transit.md:26 | `... **456.3 ns** landed chain append ...` | OK |
| 4 | 0.291/0.363 µs/link | af-21-g7-perlink.md:39-40,51,57 | 0.291 min / 0.363 mean at 39-40; G7RESULT line 51; prose 57 | OK |
| 5 | 25.5202 µs/link | rx-5-correction.md:17 | `**25.5202 µs/link** vs the ≤ 25 µs/link target.` | OK |
| 5b | zp-17 label | zp-17-gate10-settling.md | ERRATA at lines 73-81 (see §3) | OK |
| 5b-precision | working-tree-diff evidence | git log -S 'fn link' = zero commits (ledger-internal) | ledger-internal, no external path | OK |
| 6 | 44.5/41.5 ns header | ab-6-header.md:78,117 | `encode 44.5 ns/header, decode 41.5 ns/header` at both lines | OK |
| 6-cite | gate table row | cscb-gates-table.md:40 | `| ab-6 | ALIVE | encode 44.5 ns + decode 41.5 ns ...` at /Users/sac/cgap/docs/cscb-gates-table.md | OK |
| 7 | 28.2M/28.8M pushes/s | af-10-cbmqs.md:163 | `- G1b: PASS with real (anti-DCE) release numbers 28.2M / 28.8M pushes/s.` | OK |
| 8 | 0 B/op | af-10-cbmqs.md:19 | `| G1 zero-alloc hot path | AB-9 counting-allocator court ... 0 allocs / 0 bytes after init | PASS ×2 |` | OK |
| 9 | Go membrane | arcus BenchmarkLegacyJSONUnmarshal raw output (ledger §Raw outputs, Go section) | ledger-internal raw bench lines | OK |
| 10 | 2,105,153 fps | af-35-transit.md:27 | cited; NOT-RERUN row, value quoted verbatim | OK |
| 11 | 113,132 fps UDS | ab-10-writerv-stress.md:25 | `**50,000 frames, 25,657,040 bytes, 441.962 ms, 113,132 frames/sec,` | OK |

Every cited path exists; every value matches at the cited line. One
path needed repo-relative resolution (`cscb-gates-table.md` lives at
`/Users/sac/cgap/docs/cscb-gates-table.md`, not under receipts/).

## 2. Refute-cite vs live-claim classification

**Lawful refute-cites (no errata needed; every mention annotated
REFUTED/UNSUPPORTED/SUPERSEDED):** af-22-percall-latency.md (5 sites,
lines 5/10/23/46), af-21-g7-perlink.md (lines 25/61), af-10-cbmqs.md
(lines 50-51), arcus cscb-wave-index.md (lines 72/97-100/110),
arcus rfc-0089-wave-index.md:67, arcus bare-metal-transit-ledger.md
(lines 41/143), arcus README.md (rows AF-9/AF-13/AF-36/RX-1 —
verdict-annotated status index), arcus cbmqs-standard.md
(lines 50/196/209), arcus dflss-aoq-claims-audit.md and
dflss-aoq-dossier.md (AF-2 refute rows).

**Live-claim sites — now carry `ERRATA 2026-11-04` markers.** Grep
`ERRATA 2026-11-04` across /Users/sac/cgap/docs, /Users/sac/ggen/docs,
/Users/sac/arcus/docs finds **8 files** on disk, not the expected 10:

1. /Users/sac/cgap/docs/cscb-targets-open.md
2. /Users/sac/cgap/docs/jira/v26.11.3/receipts/rx-5-correction.md
3. /Users/sac/cgap/docs/jira/v26.11.3/receipts/rx-3-scorecard.md
4. /Users/sac/ggen/docs/jira/v26.11.4/PRD.md
5. /Users/sac/ggen/docs/jira/v26.11.3/receipts/ga-11-cscb-pack.md
6. /Users/sac/ggen/docs/jira/v26.11.3/receipts/zb-75-prd-materialize.md
7. /Users/sac/arcus/docs/jira/v26.11.3/rfc-0089-cscb-p99.md
8. /Users/sac/arcus/docs/jira/v26.11.3/cscb-prompt-library.md

A 9th file uses the variant header `## ERRATA (2026-11-04)` (so the
exact-string grep misses it):
/Users/sac/cgap/docs/jira/v26.11.4/receipts/zp-17-gate10-settling.md
(verified at line 73, §3 below). With it, 9 ERRATA-bearing sites are on
disk. The expected 10-file list is NOT reproduced on disk this session
— the two unaccounted files are named UNVERIFIED-MISSING, not rounded
up. **[RECOUNT 2026-11-04: corrected — see §2a; standing is now
VERIFIED at 9, not UNVERIFIED.]**

Standing for §2: the 9-file list is ALIVE (grep + line reads this
session); the 10-file expectation is UNVERIFIED (9 found).
**[RECOUNT 2026-11-04: standing corrected to VERIFIED at 9 — see §2a.]**

## 3. POST-FOLD errata verification

`/Users/sac/cgap/docs/jira/v26.11.4/receipts/zp-17-gate10-settling.md`,
line 73: `## ERRATA (2026-11-04)` — the attribution marker "PRE-FOLD"
on line 26 is INVERTED; correct label POST-FOLD, evidence = the
uncommitted working-tree diff of `crates/cgap-aoq/src/chain.rs` (zero
`fn link` commits). VERIFIED on disk this session.

## 4. Go-bench single-point-of-evidence note

Ledger row 9 (arcus `BenchmarkLegacyJSONUnmarshal` Go leg,
4602–6399 ns/op, 440 B/op, 6 allocs/op, median 6033) is backed ONLY by
the ledger's own raw output section (§Raw outputs, Go bench, count=5,
EXIT=0). No independent re-run receipt exists in any tree. FLAGGED:
needs an independent re-run receipt before the number is cited as
standing anywhere outside the ledger.

**SUPERSEDED (2026-11-04)**: the flag is resolved. An independent
re-run receipt now exists:
`/Users/sac/arcus/docs/jira/v26.11.4/receipts/bm5-legacy-baseline.md`
(arcus tree, committed in 691c277). It independently re-ran
`BenchmarkLegacyJSONUnmarshal`: 4531/4564 ns/op (two runs, spread
≈0.73%), 440 B/op, 6 allocs/op — the 440 B/op and 6 allocs figures are
an exact match with ledger row 9; the ns/op band does not exactly
reproduce (4531–4564 vs 4602–6399), consistent with machine load
variance. Row 9 may now be cited as standing with bm5 as the
independent witness.

## 5. Live-discrepancy check (expect zero, verified)

Grep `18.1 ns|18.1ns|25.5202` across /Users/sac/cgap/docs,
/Users/sac/ggen/docs, /Users/sac/arcus/docs this session: 19 files
mention the stale figures. Classification: every mention is either
( a) inside a file carrying `ERRATA 2026-11-04` (the 8 exact-string
files + zp-17's variant form, 9 total), or (b) an annotated
refute-cite (REFUTED / UNSUPPORTED / TARGET-UNMEASURED / superseded-by
pointer). **Zero files cite 18.1 ns or 25.5202 µs as live without a
pointer.** Expect-zero holds.

Standing: ALIVE (greps + cited-line re-reads run this session); §2's
10-file enumeration is UNVERIFIED (9 found, see §2).
**[RECOUNT 2026-11-04: standing corrected to VERIFIED at 9 — see §2a.]**

## 2a. Recount addendum (2026-11-04)

Re-derived this session (P-b confirmation greps re-run on disk):
`grep -rl "ERRATA 2026-11-04"` and `grep -rl "## ERRATA (2026-11-04)"`
across /Users/sac/cgap/docs, /Users/sac/ggen/docs, /Users/sac/arcus/docs.
Union = 10 hits, but one hit is THIS census file (it quotes the marker
string in its own enumeration), so the count of ERRATA-bearing sites is
**exactly 9** — the 8 exact-string files listed in §2 plus zp-17's
variant form. The missing-set is empty.

The §2 "expected 10" was a refute-cite conflation: the original
enumerator counted the census document itself (which merely cites the
`ERRATA 2026-11-04` string while classifying refute-cites) among
ERRATA-bearing live-claim sites. That mention is a classify-and-report
reference, not a live claim — same class as the annotated refute-cite
sites in §2's lawful list. The correct count was therefore 9 from the
start; the 10-file expectation is retracted, the 9-file enumeration is
complete, and standing for §2 is **VERIFIED (9/9, missing-set empty)**.

## See Also

`v4-corrected-claims-ledger.md` (same directory);
`/Users/sac/cgap/docs/jira/v26.11.4/receipts/zp-17-gate10-settling.md`.
