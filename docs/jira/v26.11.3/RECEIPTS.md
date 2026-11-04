# RECEIPTS

Register of truth for v26.11.3 lane receipts. Every row cites a receipt
file on disk in `receipts/`; status re-read from receipts at each update
(never from memory). Standing vocabulary:
ALIVE / PARTIAL / BLOCKED / UNVERIFIED / REFUSED. ALIVE requires a green
gate re-run THIS session (2026-11-03, lane-close run).

## Landed lane receipts (WO-01 through WO-07)

| WO | Receipt(s) | Standing | Gate re-run this session (exit 0) |
|---|---|---|---|
| WO-01 | g1-stage2c, k8-cli-flag | ALIVE | engine cli_boundary 32/0; stage2c wiring 7/0 |
| WO-02 | g8-cli.md | ALIVE | `-p ggen-cli-lib --test ea_verbs_test` 7/0 |
| WO-03 | g4-composer.md | ALIVE | `-p ggen-cli-lib --test pack_compose_test` 6/0 |
| WO-03 | g4-composer.md | ALIVE | `-p ggen-marketplace --lib packs_registry::composer` 5/0 |
| WO-04 | g7-schema.md | ALIVE | `-p ggen-config --test ea_graph_schema_parity_test` 4/0 |
| WO-04 | x3-projection.md | ALIVE | `-p ggen-abb-sbb --test ea_projection_test` 5/0 |
| WO-05 | g5-refusal-fences.md | ALIVE | `-p ggen-abb-sbb` 129+3dt, 0 failed (crosswalk 3/3) |
| WO-06 | i5-wo06-reconcile.md | ALIVE | `-p ggen-engine --lib envelope` 8/0 |
| WO-07 | g6-depgraph.md, k1-refusal-flip.md | ALIVE | `-p ggen-abb-sbb` 129+3dt (hardening 15/15) |

## Orphan-recovery lanes (CL-P / CL-R)

Standing words re-read from each receipt body on disk (2026-11-03, cl-close
lane). Exit record law: A gate is witnessed by an exit record: either (a) the process exit status, or (b) cargo's "N passed; 0 failed" summary line. A passed-count without a failed-count is not an exit record.

| Lane | Receipt | Standing | Exit record |
|---|---|---|---|
| CL-P | cl-p-g2-recovery.md | VERIFY-ALREADY-LANDED (no code change; G2's own receipt g2-typed-refusals.md reads ALIVE; `cargo test -p ggen-engine` every binary `test result: ok`, lib 235 passed / 0 failed) | 0 failed |
| CL-R | cl-r-transactional-recovery.md | ALIVE | transactional_edits_test 17 passed; 0 failed; `cargo test -p ggen-engine --lib` 235 passed; 0 failed |

WO-01 blocker note: g1's PARTIAL standing (9 red CLI tests) was closed by
the k8 receipt (`--skip-ea-admission` flag + fail-closed Stage 2c block 1);
k8's court list includes the flip witness (SHIM-COURT, restore byte-exact).

## cl-close self-row + new-wave register pointer (cl-close-2, 2026-11-03)

| Lane | Receipt | Standing | Exit record |
|---|---|---|---|
| CL-CLOSE | cl-close.md | ALIVE (contracted scope) | exit gate PASS exit 0 over the five index surfaces (receipt §Standing) |

Full new-wave table (63-receipt orphan census; standing words re-read from
receipt bodies on disk; in-flight-expected list): see README.md's
"New-wave receipts (cl-close-2)" section.

## Open work orders (NOT closed by this lane)

| WO | Receipts | Standing |
|---|---|---|
| WO-08 | ea-run-transcript.md names the WO-08 interim route; arcus-side receipts + close record at /Users/sac/arcus/docs/jira/v26.11.3/closure/waves-wo08-wo09.md read PARTIAL-ALIVE | landed on disk — PARTIAL-ALIVE per the close record; closure decision not made by this index lane |
| WO-09 | x4-consumer-guide.md (ALIVE), ea-run-transcript (ALIVE) | landed on disk — receipts' own rows read ALIVE; closure decision not made by this index lane |

WO-09 receipts landed mid-close (13:59, during this lane's run) and their
own standing rows read ALIVE; the arcus-side close record
(closure/waves-wo08-wo09.md) re-ran all gates green. Receipt citations above
are as-of-listing; re-read before use.

## Post-close receipts (xd-17 transcribe, 2026-11-04)

The 12 ggen receipts XD-3 found landed after this file's last write
(2026-11-03 18:27) carried rows only in the cl-close-2.md addendum; they are
transcribed here and into README.md ("Post-close receipts" section), which
also carries xd-8 and xd-10 (landed after XD-3). Standing words re-read from
each receipt body on disk, never from memory; UNSTATED where the body carries
none. Count reconciliation at this lane's run: `ls docs/jira/v26.11.3/receipts/
| wc -l` = 92 before this lane's receipt, 93 with xd-17-index-transcribe.md on
disk; xd-13-fmt-sweep.md, xd-11-fmlaw015.md and xd-15-tcps-lock.md each
landed mid-pass during this lane's verification runs (96 total at final
re-census) and are absorbed with their own rows. This file's
per-lane tables above (15 rows) do not aim at 1 row per
receipt — the full per-receipt register is README.md's tables; this section
adds the post-close rows so no landed receipt is indexed only in an addendum.

| Lane | Receipt | Standing | Exit record |
|---|---|---|---|
| SX-1 | receipts/sx-1-reds.md | UNSTATED in body (green tables carry the evidence) | manifest_contract_test ×2 exit 0 (2/2); performance ×2 exit 0 (10 passed, 6 ignored); bench_gates.sh exit 0 ALL BENCH GATES PASS; G3 fail branch exit 1 typed EA-BIN-MISSING |
| AF-7 | receipts/af-7-path-reconcile.md | UNSTATED in body | 5 byte-exact copies (4156/4036/3562/4761/7823); post-edit sweep 9 ab-* canonical |
| AF-5 | receipts/af-5-sync-surface.md | ALIVE | sync_surface_test 6 passed; 0 failed (exit 0); performance 10 passed; 0 failed; 6 ignored |
| AB-14 | receipts/ab-14-chain-scale.md | ALIVE (Status section) | receipt_chain_scale_stress 2 passed ×3 (42.83s/61.89s/56.25s); pre-existing receipt_verb_e2e failure disclosed, root-caused not session-introduced |
| GA-11 | receipts/ga-11-cscb-pack.md | ALIVE (cscb-pack) | witness court exit 0 (4/4 FLIP); cgap sync ×2 exit 0 `written: []` replay; shim-court 4/4 + byte-exact restore |
| AB-13 | receipts/ab-13-kernel-stress.md | UNSTATED in body | kernel_stress --release ×2 5 passed; 0 failed (254.4s/245.8s); full ggen-sjira --release 205 passed / 0 failed |
| GA-12 | receipts/ga-12-tort-pack.md | ALIVE (tort-pack; tort-1/3/4 TARGET-UNMEASURED, pending AF-37) | witness court exit 0 (4/4 FLIP); cgap sync ×2; marketplace validate exit 0 |
| AF-42 | receipts/af-42-marketplace-fixes.md | ALIVE | marketplace.py validate exit 0 (packs=347 manifests=347); check_gate_witness_courts.py exit 0, 50 courts ALIVE; baseline.json 313 entries |
| GD-2 | receipts/gd-2-ggen-dod.md | ALIVE | ggen sync run #1 exit 0 (2 outputs, 4 gates, receipt valid/signed); #2 exit 0 `written: []`; shim courts flipped + byte-exact restores |
| XC-8 | receipts/xc-8-cve-bump.md | UNSTATED in body (registered divergence carried) | cargo check --workspace exit 0; cargo audit → 2 vulnerabilities (from 18); quick-xml blocked upstream |
| XC-7 | receipts/xc-7-ci-unblock.md | UNSTATED in body (post-push CI re-run pending, disclosed) | cargo check --workspace exit 0; fmt --check -p ggen-sjira exit 0; sjira 12+4 passed 0 failed |
| XD-2 | receipts/xd-2-key-perms.md | ALIVE (chmod + verify path); code check = FOLLOW-UP (UNVERIFIED by design) | ls -l .ggen/keys/ 0600/0644 exit 0; ggen receipt verify exit 0 signature_valid true; FM-KEY-013 seam file:line, not landed |
| XD-8 | receipts/xd-8-key-mode-gate.md | ALIVE (code check + negative-path refusal) | cargo test -p ggen-engine --lib keys 19 passed; 0 failed ×2 exit 0; focused new test 1/0 exit 0; cargo check --workspace exit 0 |
| XD-10 | receipts/xd-10-publish-blockers.md | ALIVE (three owned files) | cargo check -p ggen-engine exit 0; cargo build -p ggen-engine exit 0 (both v26.11.3) |
| XD-13 | receipts/xd-13-fmt-sweep.md | ALIVE (landed mid-pass, absorbed at re-census) | cargo fmt --all exit 0 (83 files); fmt --all -- --check exit 0; git diff --check exit 0; cargo check --workspace exit 0 |
| DS-8 | receipts/ds-8.md | UNSTATED in body (gates green; full DS-8 lane receipt, distinct from ds-8-cli-skeleton.md, landed post-index, found uncovered by this lane's coverage check) | cli_sjira_test exit 0, 17 passed / 0 failed; clippy 0 findings; admit valid exit 0, malformed exit 1 typed, epoch_check exit 5 Andon |
| XD-11 | receipts/xd-11-fmlaw015.md | PASS standing — sync_surface_test PASS green ×2 + full suite exit 0; PUB-1 blocking row resolvable to closed; (landed mid-pass, absorbed at re-census) | sync_surface_test ×2 exit 0 (6/6); full ggen-cli-lib 604 passed / 0 failed (62 binaries) |
| XD-15 | receipts/xd-15-tcps-lock.md | PARTIAL_ALIVE (artifact produced, uncommitted; landed mid-pass, absorbed at re-census) | tcps_chicago_tdd_8pow4 17/0; tcps_chicago_tdd_1000x 16/0; ggen sync run --skip-ea-admission exit 0 wrote ggen.lock 1288 bytes (8 packs) |

### Cross-repo (pointer-only; canonical register lives in the host repo)

| Receipt | Host | Standing | Exit record |
|---|---|---|---|
| cgap receipts/bk-1-takeover.md | /Users/sac/cgap/docs/jira/v26.11.3/receipts/ | AB-4 ALIVE, AB-5 ALIVE, corridor join ALIVE (AF1-KD-1 CLOSED) | cgap-crypto 75 passed / 0 failed ×2; riscv32imc + wasm32 check exit 0 |
| cgap receipts/af-37-p99-torture.md | same dir | ALIVE (pre-existing doctest failure disclosed) | macro_p99_torture ×4 exit 0; cgap-aoq 22/22 binaries ok |
| cgap receipts/rx-2-conformance.md | same dir | ALIVE | rfc0089_conformance ×4 exit 0 (4 passed; 0 failed; 1 ignored); lib courts 14/0; CBMQS 1/0 |
| cgap receipts/gd-1-dod-pack.md | same dir | ALIVE pack surface; PARTIAL overall (dod-006 TARGET-UNMEASURED) | sync ×2 determinism + witness court + shim-court flip, green this session |
| arcus receipts/xd-4-govulncheck.md | /Users/sac/arcus/docs/jira/v26.11.3/receipts/ | ALIVE (clean at go1.26.10; 40 called vulns at go1.23.4) | govulncheck exit 3 default / exit 0 pinned go1.26.10; wrapper exit 0 |
