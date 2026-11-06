# Open-Items Register — v26.11.4 pre-drain

as-of 2026-11-04T23:05Z (refresh pass). Every row's evidence re-read live
on disk this session (paths absolute); statuses flipped only with a live
command/output receipt. Lines <=100 chars.

| id | title | status | owner | evidence | falsifier |
|---|---|---|---|---|---|
| OI-1 | ZP-16 publish automation | CLOSED | operator+lane GAP-CLOSE | /Users/sac/ggen/docs/jira/v26.11.4/receipts/zp-16-publish-automation.md; PLANS.md:13,28 (3afe33b2d+8d5ff1167): ZP-16 VERIFIED-LANDED, NO-OP retracted; live read crates/ggen-engine/Cargo.toml:118,121 pins `=26.11.4` on both edges | falsifier discharged: pre-fix-gate version derives from workspace, not a hardcoded pin — tag-push rehearsal may proceed |
| OI-2 | ZP-24 tera whitespace census | PARTIAL | F2 lane | /Users/sac/ggen/crates/ggen-engine/tests/tera_whitespace_census.rs | dual-engine render-diff court still passing after tera 2.4.0 becomes a real ggen-engine dev-dep |
| OI-3 | ML-DSA-65 signer (ZP-22) | PLANNED | wave-3 | PLANS.md:135-136 (re-read as-of 2026-11-04T23:05Z): 5-phase build, wave-3 gate; PLANS.md:50 ZP-22 UNVERIFIED (no receipt); no FIPS-204 witness in v26.11.4 docs (repo grep) | no FIPS-204 witness exists in any tree; sign-loop court flipping on a faulted candidate count |
| OI-4 | rung-7 stress/chaos (zd-79) | BLOCKED | unassigned | /Users/sac/ggen/docs/jira/v26.11.3/receipts/zb-24-final-index.md:46-48 | a zd-79 receipt file lands in docs/jira/v26.11.3/receipts/ |
| OI-5 | Q6 closure wave | CLOSED | coordinator | cgap fbc0d9f3 + arcus 9fec512 both pushed (ls-remote verified: cgap origin/main=601e0274 contains fbc0d9f3; arcus origin/main=3904ac2 contains 9fec512); 30/30 suites + 7 Go courts GREEN; purity gate REGISTERED[13] (D-wave consolidated receipt 498d28ef8, arcus row); E-t independently verified coherent | wave-closing-receipt.md counts unfilled, or a Q6 SHA unpushed — both disproven live 2026-11-04 |
| OI-6 | ZP-21 vs ZP-14 home dispute | OPEN | coordinator | /Users/sac/ggen/docs/jira/v26.11.4/PLANS.md:87-88 | coordinator names one home; losing plan retires its surface |
| OI-7 | cgap workspace still 26.11.3 | CLOSED | cgap lanes | SD-4 landed: /Users/sac/cgap commit cd1d8740 `chore: workspace version 26.11.5 (SD-4) + manifest sync` (on cgap origin/main, ls-remote verified 2026-11-04); cargo metadata 26/26 members at 26.11.5 | cgap workspace version flips to 26.11.4 (SD-4 targets 26.11.5; superseded — retargeted straight to 26.11.5); falsifier disproven: Cargo.toml:51 now `version = "26.11.5"` |
| OI-8 | `_impl14` provenance | CLOSED | session ledger | reconstructed in-writing (see below); removal merged 434321b0 | `git log --diff-filter=A -- '_impl14/**' \| head` shows entry commit; lesson recorded: build artifacts must never be git-added (lane law) |

### OI-8 reconstruction (written 2026-11-04, from session ledger)

`_impl14` was a stray Elixir implementation-lane build tree (mix compile
output: `test/lib/ash/ebin/*.beam`, `.so` NIF binaries incl.
`libwasmex-v0.15.1`, `.mix/compile` caches) committed as tracked files in
an earlier smalt wave — the "committed build artifacts" anti-pattern.
Discovered 2026-11-04 by the E4/E6 ERRC sweeps (259 git-tracked files,
~350MB). Import-check passed (zero refs in `mix.exs`, `lib/`, `test/`,
`scripts/`, `justfile.d`). C6's U3 landed the removal on branch
`chore/rm-impl14-artifacts` (0acc51b0, 259 deletions); D63 merged it
--no-ff to main (434321b0, pushed). The ~1.47GB side-lane build roots
`_build-eplaw`/`_build-expl11r3` were also deleted untracked. Status
CLOSED — this row plus merge 434321b0 is the evidence path; no residual
in-writing gap remains.
| OI-9 | /tmp lane leases | CLOSED | coordinator | D72 deleted 27 lease entries gated-verified (coordinator session ledger); live check 2026-11-04T23:05Z: `ls /tmp \| grep -cE 'bm7-head\|lane2-\|cgap-mut-\|arcus-bm8'` = 0 — all four named lease families gone. Two superseded intermediates disclosed in D72's receipt and not re-created since. Note: /tmp still holds arcus-sidecar-* + *-msg.txt entries (820 total) — outside D72's lease scope, retained | reconcile-then-delete: each lease named in a receipt, then removed — discharged for the named families; sidecar/msg entries are a new open item if any are claimed as leases |
| OI-10 | lint-fence wider-scope clippy debt | CLOSED | pending D90 decision | D90 landed 97c2148d1 (46 files) and its cascade covered D92's 4 named files (ggen-sjira/src/ledger.rs, ggen-scip/tests/scip_ontology_test.rs, ggen-sjira/src/reconcile.rs, ggen-engine/src/verbs/{sync,handlers}.rs). Fresh gate 2026-11-04 at 6d6346c8a: `just lint` (= `cargo clippy --workspace --all-targets --keep-going -- -D warnings -A unexpected_cfgs`, justfile lint: recipe — covers the test targets D92 flagged) exit 0, and `cargo clippy --workspace -- -D warnings` exit 0. No fixes were needed beyond D90's landing | none — D90's cascade already fixed all 4 files; both gates re-witnessed green at HEAD |

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

## OI-6 resolution (coordinator, 2026-11-05 crash-recovery wave)

ZP-14 home wins: `crates/cbmqs-cli` as a separate workspace member (receipt
`cgap/docs/jira/v26.11.4/receipts/zp-14-cbmqs-cli.md`, courts C7 tamper /
C8 SKIP-fail-closed / C9 bin-parity). It preserves cgap-aoq's empty-deps
embedded claim, which a feature-gated `[[bin]]` inside cgap-aoq would
regress. ZP-21's colliding surface (feature-gated `[[bin]]` inside
cgap-aoq) RETIRES; ZP-21's non-colliding substance migrates into
cbmqs-cli: `canonical_form: "fixed-declaration-order-v1"` named inside the
sealed artifact, disk-tamper court anti-vacuity legs (delete/corrupt
sidecar -> exit 2), RunSeqCollision law, timestamp-NONE grounds. Falsifier
for the migration: two `cbmqs-audit` homes never ship in one tree —
discharged by this decision; the landing diff must not add a bin target to
crates/cgap-aoq.
