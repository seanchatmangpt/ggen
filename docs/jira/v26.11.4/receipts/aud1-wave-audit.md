# AUD-1 Wave Audit — skeptic re-verification of v26.11.4 lane receipts

Standing: PASS

- **Lane**: AUD-1 (skeptic audit — grade, never build)
- **Date**: 2026-11-05
- **Repos audited (read-only + this receipt)**: /Users/sac/ggen @ 3c2ee02e74415023cb5dd1036ec7b4a0e7ad0fb5, /Users/sac/ggen-marketplace @ 71d411a90b23db55d5d4b48207f7ad3c263006f1, /Users/sac/gymact
- **Method**: no receipt claim trusted; every cheap check re-run in-session. No fixes, no git commits.

## Verdict table

| receipt | verdict | re-run command | exit | result |
|---|---|---|---|---|
| gg1-wip-sweep.md | CONFIRMED | `git status --porcelain` in ggen | 0 | tree carries only other-lane modified files; no untracked foreign artifacts. HEAD matches the pinned 3c2ee02e. |
| s11-lane-registry.md | CONFIRMED | `GGEN_LANE_ID=S-11 sh scripts/hooks/lane-lease-check` with synthetic staged foreign path `crates/ggen-cli/src/foreign_probe.rs` (GIT_INDEX_FILE temp index, real index untouched — `git status` grep foreign rc=1 after) | 1 | exact refusal text: `REFUSED — lane 'S-11' staged paths outside its lease: crates/ggen-cli/src/foreign_probe.rs (owner: UNOWNED)`; T2 (unset GGEN_LANE_ID) also exit 1 with `REFUSED — no lane lease in environment; commits are coordinator-only (set GGEN_LANE_ID)`. Matches T1/T2 rows. |
| s12-receipt-lint.md | CONFIRMED (superseded counts, as disclosed) | `just lint-receipts` | 0 | 167 files, 167 parse OK, 0 FAIL, `PASS (0 missing)`. S-12's 140-FAIL red state is the honest pre-backfill state; SB-1 closed it. Map hash e9ccd7059db40182 matches the receipt's pin. |
| sb1-standing-backfill.md | CONFIRMED | `just lint-receipts` (0 FAIL, exit 0) + `grep -rh '^Standing:' docs/jira/*/receipts \| awk '{print $2}' \| sort \| uniq -c` | 0 | Recount: ALIVE 85, PASS 29, UNVERIFIED 15, PARTIAL_ALIVE 8, PARTIAL 6, REFUSED 4, UNKNOWN 3, CLOSED 3, BLOCKED 3, UNSUPPORTED 2, FAIL 2, OPEN 1, DECIDED 1 (163 total). Receipt's distribution matches on every bucket within ±1 on ALIVE/PASS — +1 each attributable to receipts landed after SB-1's write (this audit receipt included). One outlier line `Standing: 4 FIXED...` in a pre-v26.11.3 receipt falls outside the receipt's claimed corpus. |
| mk1-wip-finish.md | CONFIRMED | `python3 packs/kafka-wire-pack/qualification/verify.py` | 0 | verdict CONFORMANT, 9 gates, 9 flips, all FLIP-OK. Matches receipt exactly. |
| gy1-wip-finish.md | CONFIRMED | python re-derivation of gate math from `docs/sjira/v26.10.8/doc-hdit.receipts.jsonl` | 0 | 1 line, parses; S_coverage 0.97895 >= 0.9 PASS; Phi_halluc 0.0 <= 0.001 PASS; Q_density 1.0 >= 0.65 PASS; verdict ACCEPTED lawful; parent "" genesis; hash 64 hex (BLAKE3-consistent). Cited surfaces exist (`docs/reference/generated/reference.md`; pack README.md:40 path convention line confirmed). |

## Per-receipt notes

- **gg1**: receipt says "porcelain empty" at its write time; current tree carries other lanes'
  in-flight edits (expected mid-wave drift, not a gg1 defect — gg1's claim is time-scoped).
- **s11**: environment note (direct shebang exec hangs in sandbox) reproduced implicitly —
  audit ran the hook via `sh scripts/hooks/lane-lease-check`, same argv contract. Real git
  index verified untouched after the synthetic-index court. Only T1/T2 re-run (the audit
  contract's foreign-path court); T3–T7 not re-derived.
- **s12**: 140-FAIL state honestly disclosed and subsequently closed by SB-1; gate now green.
  `UNVERIFIED` vocab delta and standing-map pins are live in `scripts/receipt_lint.py` and
  `receipts/receipt-standing-map.json` (map written by the audit run itself).
- **sb1**: distribution recount drift limited to +1 ALIVE / +1 PASS from post-write receipts
  — within the receipt's own disclosed census-moves discipline. 140-file backfill diff is
  visible in `git status` (v26.11.3 + v26.11.4 receipts dirs).
- **mk1**: 050_version_matrix_closure FLIP-OK on the corrected servedMax corpus — the
  fail witness still flips, so the court is non-vacuous.
- **gy1**: timestamp in receipt reads 16:58:34 (local) vs 23:58:34Z from the epoch — same
  instant, timezone presentation only. Gate math independently re-derived, all three gates
  pass against embedded thresholds. Orphan disclosure (unreferenced+untracked) verified as
  accurate framing.

## Commands + exits (this audit)

1. `git rev-parse HEAD` (ggen) → 0, 3c2ee02e74415023cb5dd1036ec7b4a0e7ad0fb5
2. `just lint-receipts` → 0: 167 files, 167 OK, 0 FAIL
3. `python3 -c "json.load(...)"` on lane-registry.json → 0, 23 lane rows parse, S-11 row present with 3 owned paths
4. foreign-path lane-lease court → exit 1 with owner-refusal text; no-lease court → exit 1
5. `grep -rh '^Standing:' ...` recount → 163 lines, distribution as tabled
6. `python3 packs/kafka-wire-pack/qualification/verify.py` (ggen-marketplace) → 0, CONFORMANT 9/9
7. gymact JSONL re-derivation → 0, verdict ACCEPTED lawful, all gates pass

## Falsifier / replay

Any checkout at the pinned SHAs: re-run commands 2/4/6/7 above; a green gate flipping red
without a substrate change, or the lane-lease hook passing on the foreign probe path,
refutes this audit.

Standing re-read at close: PASS (every gate executed this session).
