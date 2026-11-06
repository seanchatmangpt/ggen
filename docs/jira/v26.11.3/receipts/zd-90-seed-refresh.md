# ZD-90 — v26.11.4 SEED refresh (per-item re-verification against disk receipts)

Standing: CLOSED

- **Date**: 2026-11-04
- **Repo**: /Users/sac/ggen (branch v26-11-3-wave)
- **Owned files**: docs/jira/v26.11.4/SEED.md (update only),
  this receipt. No commit made (coordinator owns git).
- **Method**: every SEED row re-verified by reading the named receipt on disk;
  no count or standing carried from the prior SEED. Receipt-cited-counts law
  applied at use time.

## Per-item dispositions (receipts read this session)

| Item | New standing | Receipt (disk-verified) |
|---|---|---|
| CARRY-01 | CLOSED | ggen docs/jira/v26.11.3/receipts/zd-50-wo-drift.md (WO standing reconciliation sweep, exit 0) |
| CARRY-02 | CLOSED | zd-45-receipt-seam.md (allow(dead_code) removed; check 0 warnings; pin court 4/0) |
| CARRY-04 | CLOSED | zd-44-skip-flag.md (flag already wired CLI→gate; bypass-marked receipt field + fail-closed court added) |
| CARRY-05 | CLOSED | zd-59-carry05.md (FM-LAW-015 row RESOLVED-DISCLOSED; sync_surface 6/0 ×2) |
| CARRY-07 | CLOSED | zd-46-zero-verb-audit.md (phantom "mod" noun; 23 verbs rewired in 3 dirs) |
| CARRY-08 | CLOSED | zd-47-relay-probe.md (probe in lsp-max; resolve_child_bin with typed SKIP) |
| CARRY-09 | CLOSED | zd-48-fm-codes.md (52→45 none; 8 courts green ×2; 45 typed rulings U/P/T) |
| CARRY-12 | R80-12 CLOSED / R80-11 PARTIAL_ALIVE / R80-6 open L | zd-51-r80.md |
| CARRY-13/14 | UNCHANGED | parked / pin-gated |
| MKT-01 | CLOSED | ggen-marketplace docs/jira/v26.11.3/receipts/zd-62-mkt01-adoption.md (ambient 26.11.3 adoption + disclosed opt-out) |
| MKT-02 | CLOSED | zd-49-gac-registry.md + zd-28-regression-packs.md + zd-65-smalt-gates.md (347/347 qualified, zero REFUSED, full-corpus exit 0) |
| MKT-03 | CLOSED (1 repair residue) | ggen-marketplace zd-52-warn-typing.md (10 WARN + 1 SKIPPED typed; 1 REPAIR-LANE row survives) |
| W2-D | CLOSED | arcus docs/jira/v26.11.3/receipts/zd-42-w2d-admission.md (ALIVE, 6 courts) |
| ZD-56 rung-8 | CLOSED | cgap docs/jira/v26.11.3/receipts/zd-56-sidecar-e2e.md (sequential serve + E2E court) |
| W2-B | ALIVE | arcus zd-29-w2b-twin.md (file present; standing word re-read at wave open) |
| CARRY-06 | IN-FLIGHT/UNRECEIPTED | **no zd-60 receipt on disk** (searched ggen/cgap/arcus/ggen-marketplace docs/jira; `grep -rln ZD-60` over all four docs trees: 0 hits) |
| E-2 / ZB-21 insight | carried, UNVERIFIED | **no zb-21 receipt on disk** (same search: 0 hits); insight recorded in SEED with explicit UNVERIFIED flag |

## Not found on disk (typed)

- `zd-60*` — no file, no textual mention, any repo's docs tree.
- `zb-21*` — no file, no textual mention, any repo's docs tree.
- The C21-class insight ("--check must compare against the observation commit,
  not literal HEAD") is carried in SEED §E-2 attributed to the coordinator
  dispatch, marked UNVERIFIED until a receipt lands.

## Cross-repo receipt note

Marketplace and arcus/cgap receipts live in their own repos' docs/jira/v26.11.3/
receipts/ — the SEED now names the owning repo per cite so every cite resolves
unambiguously. All cites in the rewritten SEED resolve on disk except the two
typed-missing above (flagged in place).

## Search commands (receipt)

- `find /Users/sac/ggen /Users/sac/cgap /Users/sac/arcus
  /Users/sac/ggen-marketplace -name 'zd-*'` (per-repo, plus `-name 'zb-21*'`
  and `-name 'zd-60*'` sweeps: no matches)
- `grep -rln 'ZB-21'` and `grep -rln 'ZD-60'` over the four docs trees: 0 hits
