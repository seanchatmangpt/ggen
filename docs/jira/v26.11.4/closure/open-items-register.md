# Open-Items Register — v26.11.4 pre-drain

as-of 2026-11-04T21:41Z. Every row's evidence read live on disk this
session (paths absolute); a row whose evidence contradicted P-g's draft is
marked CORRECTED with the delta disclosed inline. Lines <=100 chars.

| id | title | status | owner | evidence | falsifier |
|---|---|---|---|---|---|
| OI-1 | ZP-16 publish automation | PARTIAL | operator+lane GAP-CLOSE | /Users/sac/ggen/docs/jira/v26.11.4/receipts/zp-16-publish-automation.md | a tag push refusal at pre-fix-gate with engine pins already at 26.11.4 |
| OI-2 | ZP-24 tera whitespace census | PARTIAL | F2 lane | /Users/sac/ggen/crates/ggen-engine/tests/tera_whitespace_census.rs | dual-engine render-diff court still passing after tera 2.4.0 becomes a real ggen-engine dev-dep |
| OI-3 | ML-DSA-65 signer (ZP-22) | PLANNED | wave-3 | /Users/sac/ggen/docs/jira/v26.11.4/PLANS.md:112-118 | no FIPS-204 witness exists in any tree; sign-loop court flipping on a faulted candidate count |
| OI-4 | rung-7 stress/chaos (zd-79) | BLOCKED | unassigned | /Users/sac/ggen/docs/jira/v26.11.3/receipts/zb-24-final-index.md:46-48 | a zd-79 receipt file lands in docs/jira/v26.11.3/receipts/ |
| OI-5 | Q6 closure wave | IN-FLIGHT | coordinator | /Users/sac/ggen/docs/jira/v26.11.4/closure/ (this directory) | wave-closing-receipt.md promoted DRAFT-UNTIL-DRAIN to filled counts |
| OI-6 | ZP-21 vs ZP-14 home dispute | OPEN | coordinator | /Users/sac/ggen/docs/jira/v26.11.4/PLANS.md:87-88 | coordinator names one home; losing plan retires its surface |
| OI-7 | cgap workspace still 26.11.3 | OPEN | cgap lanes | /Users/sac/cgap/Cargo.toml:51 `version = "26.11.3"` | cgap workspace version flips to 26.11.4 |
| OI-8 | `_impl14` provenance | OPEN | session ledger | no on-disk referent found this session | reconstruct in-writing at drain or demote |
| OI-9 | /tmp lane leases | OPEN | coordinator | /tmp (723 entries incl. arcus-sidecar-* and *-msg.txt) | reconcile-then-delete: each lease named in a receipt, then removed |

## Corrections vs P-g's draft

- OI-1 CORRECTED: the zp-16 receipt (written earlier 2026-11-04) still says
  engine pins are 26.10.11/26.10.12 and the gate is BLOCKED-by-design.
  Live read of /Users/sac/ggen/crates/ggen-engine/Cargo.toml:118,121 shows
  `=26.11.4` on both edges — the PRE-FIX bump has landed SINCE the
  receipt, at =26.11.4 (not the planned 26.11.3). The gate may now pass;
  a tag-push rehearsal is the falsifier, not the receipt's BLOCKED claim.
- OI-4: no zd-79 receipt exists on disk (v26.11.3 zb-24-final-index.md
  rows it NOT-on-disk); BLOCKED stands with absence as the evidence.
- OI-6: PLANS.md:87-88 (ZP-14 `[[bin]]` in cbmqs-cli vs ZP-21 feature-gated
  `[[bin]]` inside cgap-aoq — coordinator decides one home). Verified text.
