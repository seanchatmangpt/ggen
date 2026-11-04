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
re-census) and are absorbed with their own rows. Re-census at the zd-23
refresh (2026-11-04): 103 receipt .md files on disk before zd-23's own
receipt, 104 with it — the 7 receipts landed after XD-17's transcribe are
rowed in the zd-23 section below (zd-21-shacl-syntax.md landed mid-pass,
then zd-15-template-fmt.md and zd-24-claude-markers.md landed during the
final coverage re-run — 107 total at final re-census — each absorbed with
its own row). Re-census at the zd-58 refresh (2026-11-04): 119 receipt .md
files on disk before zd-58's own receipt, 120 with it — the 12 receipts
landed after zd-23's census are rowed in the zd-58 section below. Re-census
at the zb-24 final pass (2026-11-04): 130 receipt .md files on disk before
zb-24's own receipt, 131 with it — the 10 stems the mention-level coverage
gate found uncovered (zd-44, zd-48, zd-59, zd-66, zd-67, zd-68, zd-69,
zd-71, zd-86, zd-87) are rowed in the zb-24 section below; gate now exits
UNCOVERED []. This file's
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

## Post-close receipts (zd-23 index refresh, 2026-11-04)

Seven receipts landed after XD-17's transcribe pass with no row in this file
or README.md; they are rowed here and into README.md's post-close section.
Standing words re-read from each receipt body on disk, never from memory;
UNSTATED where the body carries none. zd-15/zd-16/zd-17/zd-18 were NOT on
disk at this lane's run (`ls docs/jira/v26.11.3/receipts/ | wc -l` = 103
before this lane's receipt, 104 with zd-23-index-refresh2.md).

| Lane | Receipt | Standing | Exit record |
|---|---|---|---|
| AB-20 | receipts/ab-20-tokens-stress.md | UNSTATED in body (gate tables carry the evidence; disclosures: release profile, one out-of-lane marketplace.json version sync, test-authoring accounting bug fixed in-lane) | `cargo test -p ggen-lsp --release --test tokens_delta_stress` ×2 exit 0 (4 passed; 0 failed; soak 2378.87s/2327.41s); full ggen-lsp suite (4 stress tests skipped by name) exit 0 |
| XD-14 | receipts/xd-14-ea-graph-ci.md | ALIVE for 3 of 4 checks (zero-code, part-passport, root dogfood sync + receipt verify); PARTIAL for ggen-self-host (FM-LAW-015 satisfied, blocked downstream on the observation gate, closed since by zd-9) | `validate_ea_graph` ×4 graphs exit 0, all 8 SBBs ADMITTED; root `ggen sync run` + `receipt verify` exit 0; inventoryctl + part-passport `ggen sync run` exit 0; self-host exit 1 on pre-existing FM-PACK-013 (other lane's scope) |
| XD-18 | receipts/xd-18-ci-residuals.md | UNSTATED in body (verification ladder carries the evidence; concurrency disclosure: other lanes' uncommitted tree edits left untouched) | `validate_gap_closure.py` exit 0 (25/25, errors=0); YAML parse ×3 exit 0; `bash -n` on 23 extracted run: blocks exit 0 ×23; rustfmt --check on 2 owned files exit 0 |
| ZD-5 | receipts/zd-5-sccache.md | FIXED (uncommitted, per body Status); CI job execution NOT run — first push-triggered run named the real gate, so the CI-path fix itself is UNVERIFIED by design | YAML `safe_load` exit 0; pinned SHA identical to setup-ggen-build STEP 2a (diff-verified); full job execution NOT run (no local runner), disclosed |
| ZD-9 | receipts/zd-9-self-host-observation.md | ALIVE | `observe_self_host.py` exit 0 (13-min census); `verify_observation.py` exit 0 GGEN_SELF_HOST_OBSERVATION_VERIFIED; self-host `ggen sync run` exit 0 (26.10.11 and 26.11.3 binaries); root dogfood sync exit 0 under repo-built v26.11.3; ambient 26.10.11 root sync exit 1 FM-CHAIN-003 = pre-existing binary/log schema skew, disclosed |
| ZD-11 | receipts/zd-11-sync431.md | RESOLVED-DISCLOSED (pinned-by-law, no code change); ALIVE for the ruling (code + kernel gate read, history traced, tests + dogfood this session) | `cargo test -p ggen-engine --lib` 236 passed / 0 failed; `cargo build -p ggen-cli-lib --bin ggen` exit 0; `ggen sync run` exit 0; `ggen receipt verify` exit 0 valid/signed; one receipt-chain wall-clock SLO test failure on the shared uncommitted tree, root-caused pre-existing not session-introduced |
| ZD-14 | receipts/zd-14-ci-final.md | Fixes 1-3, 5, 7 ALIVE (gates this session); Fix 4 ALIVE-as-verified (already fixed at HEAD, coordinator brief stale); Fix 6 PARTIAL (owned docs fixed, repo-wide EOF-blank-line class flagged not fixed) | `cargo fmt --all -- --check` exit 0; `rustfmt --check refusals.rs` exit 0; YAML parse ×3 + bash -n ×2 exit 0; census prints exactly 3 authored files, 5 generated paths tracked; `validate_ea_graph` on examples/affidavit-verify exit 0 ADMITTED + `ggen sync run` exit 0; `validate_gap_closure.py` exit 0 errors=0; book Chicago suite 2 fixed, 1 pre-existing industry-TTL failure disclosed out of scope |
| ZD-21 | receipts/zd-21-shacl-syntax.md | ALIVE (landed mid-pass during the zd-23 verification run, absorbed at re-census; closes the industry-TTL failure ZD-14 disclosed out of scope) | template fix in ggen-marketplace (generated file regenerated, not hand-patched); `ggen sync run` in examples/industry/manufacturing-dflss exit 0; rdflib parse OK 21 triples; failing book test green ×2 (5.03s/4.84s); file suite 4 passed, 1 skipped (pre-existing skip) |
| ZD-15 | receipts/zd-15-template-fmt.md | ALIVE (landed during the zd-23 final coverage re-run, absorbed; closes ZD-14's disclosed fmt-gate flap — sync+fmt now a fixed point) | `ggen sync run` ×2 exit 0 (refusals.rs written run 1); `rustfmt --check refusals.rs` exit 0 ×2; `cargo fmt --all -- --check` exit 0; sha256 byte-identical across runs 1→2 (793fddd9…7913227); `cargo test -p ggen-abb-sbb` exit 0 (16+3 crosswalk+3 doc-tests green) |
| ZD-24 | receipts/zd-24-claude-markers.md | RESOLVED (uncommitted); ALIVE for the owned surface; the repo-facts.ttl gap (ggen-scip/ggen-sjira missing rf:Crate individuals, commit 301849a9c) is a pre-existing HEAD defect, BLOCKED for this lane, filed for the coordinator | `grep -c` markers → 0 matches (exit 1 ×2); `wc -l CLAUDE.md` = 70; `cargo test -p ggen-config --test system_crate_map_parity_test` 1 passed / 1 FAILED — failure pre-existing (repo-facts.ttl ↔ Cargo.toml, not CLAUDE.md, both files match HEAD), disclosed not session-introduced |

## Post-close receipts (zd-58 index refresh, 2026-11-04)

Twelve receipts landed after zd-23's 107-census with no row in this file
or README.md; they are rowed here and into README.md's post-close section.
Standing words re-read from each receipt body on disk, never from memory;
UNSTATED where the body carries none. Brief candidates zd-29 / zd-32 /
zd-35 / zd-36 / zd-41 / zd-42 / zd-43 / zd-44 / zd-48 / zd-49 / zd-52..57
were NOT on disk at this lane's run (`ls docs/jira/v26.11.3/receipts/
| wc -l` = 119 before this lane's receipt, 120 with zd-58-index-refresh3.md).
zd-32's seal-reverify doc lives in `closure/`, not `receipts/` — closure
docs are outside this index's receipts/ convention, disclosed, not rowed.

| Lane | Receipt | Standing | Exit record |
|---|---|---|---|
| ZD-16 | receipts/zd-16-eof-sweep.md | ALIVE on the verified surface (280 tracked .md files EOF-normalized; uncommitted) | sweep re-run after fix: 0 candidates; `git diff --check -- '*.md'` clean for all fixed files (sole residual CLAUDE.md:70 complaint is another lane's in-flight edit); whitespace-only audit — content byte-identical |
| ZD-17 | receipts/zd-17-ambient-ggen.md | ALIVE (exact subject `~/.local/bin/ggen` observed at 26.11.3; backup + rollback path disclosed) | `ggen --version` → 26.11.3 exit 0; `ggen sync run --help` exit 0; `ggen receipt verify` exit 0 valid/signed; self-host `ggen sync run` exit 0 (previously FM-CHAIN-003-refusing) |
| ZD-27 | receipts/zd-27-repo-facts.md | UNSTATED in body (Standing/residual-drift section carries only disclosed ttl-intro drift, not fixed) | `system_crate_map_parity_test` ×2 exit 0 (2 passed; 0 failed); `governance_precommit_gate_count_test` exit 0 (4/0); `grep -c '^  "crates/' Cargo.toml` = 15 |
| ZD-31 | receipts/zd-31-runbook-refresh.md | ALIVE for the verification ladder; PARTIAL for publish readiness itself (PR #824 unmerged, tags unminted, dirty trees outstanding) | live ladder all run this session: `git rev-parse HEAD` 72b743d23; PR #824 rollup via `gh pr view` (13 failing, 3 in-progress); cgap/marketplace/lsp-max/arcus state re-verified per runbook §5 |
| ZD-38 | receipts/zd-38-advisory-ignores.md | UNSTATED in body (before/after gates table carries the evidence; registered divergence carried for RUSTSEC-2026-0194/0195 per xc-8, close condition on quick-xml upstream) | `cargo audit` exit 1 → exit 0 after; `cargo deny check advisories` exit 1 (5 errors) → exit 0 `advisories ok`; `cargo check --workspace` exit 0 post tokio bump (1.53.1→1.54.1, disclosed out-of-owner-file fix) |
| ZD-39 | receipts/zd-39-polyglot-sccache.md | FIXED (uncommitted, per body Status); full CI job NOT run — first push-triggered run is the real gate, CI-path fix UNVERIFIED by design (supersedes ZD-5's fix for cyberpunk-tv-platform.yml) | YAML `safe_load` OK; `bash -n` on 9 embedded `run:` blocks exit 0; job env `{RUSTC_WRAPPER: '', SCCACHE_DISABLE: '1'}` confirmed on disk |
| ZD-40 | receipts/zd-40-lsp-contract-graph.md | ALIVE for `self-host/lsp-contract` (FM-LAW-015 satisfied with a kernel-admitted SBB, no --skip-ea-admission); full ggen-lsp compile/test chain out of scope | pre-fix repro exit 1 FM-LAW-015; `validate_ea_graph` exit 0 ADMITTED (pre-pinning run refused typed SBB_DIGEST_MISMATCH — non-vacuity); `ggen sync run` exit 0, 5 stages, files_generated=3 |
| ZD-45 | receipts/zd-45-receipt-seam.md | ALIVE | `cargo check --workspace` exit 0, dead_code warnings 0; `cargo test -p ggen-engine --lib receipt` 6 passed / 0 failed; seam pin court `receipt_chain_seam_test` 4 passed / 0 failed |
| ZD-46 | receipts/zd-46-zero-verb-audit.md | ALIVE for the noun-surface claim (23 dead verbs rewired, phantom noun `mod` removed; attribute-only diff) | `cargo build -p ggen-cli-lib` exit 0; `ggen sbb/maximalism/vision2030 --help` list 6/7/10 verbs; `ggen mod --help` exit 2 unrecognized; `cargo test -p ggen-cli-lib` 61× `test result: ok` (one pre-existing flake re-run green) |
| ZD-47 | receipts/zd-47-relay-probe.md | ADMITTED (per body — all branches executed this session) | `diagnostic_relay_probe` ×3 exit 0 (2 passed; 0 failed; relay round trip 14 ms); shim court: binary moved away → loud typed SKIP, restore sha256-verified; env-override branch witnessed 2/0 |
| ZD-50 | receipts/zd-50-wo-drift.md | ALIVE for the landed surface (WO standing-word sweep exits clean ×1; WO-08 stays PARTIAL-ALIVE, not rounded up) | sweep ×1 exit 0 over six WOs — standing word in each WO == cited receipt body / close record word; `grep -c UNVERIFIED` over the six WO Standing lines → 0 matches |
| ZD-51 | receipts/zd-51-r80.md | ALIVE (R80-12 CLOSED — wiring + production caller already landed; R80-11 promoted UNKNOWN → PARTIAL_ALIVE, not rounded up; R80-6 scope-check only, L-effort recorded for CARRY-03) | `diagnostics_rules_test` 15 passed / 0 failed exit 0; `edits_bridge_test` 11 passed / 0 failed; `--lib edits_bridge` 4/0; `cargo check --workspace` exit 0 (1m34s) |

## Post-close receipts (zd-116 final coverage pass, 2026-11-04)

Eight receipt stems landed after zb-24's 131-census with no row in this
file or README.md (zb-21, zd-60, zd-88, zd-90, zd-101, zd-106, zd-108,
zd-110); they are rowed here and into README.md's post-close section,
with zb-24's own self-row in this file. Standing words re-read from
each receipt body on disk, never from memory; UNSTATED where the body
carries none. Census at this lane's run: `ls docs/jira/v26.11.3/receipts/
| wc -l` = 139 before this lane's own receipt, 140 with
zd-116-index-final.md on disk. Mention-level coverage gate after these
rows: UNCOVERED [] (exit 0). zd-112 and zb-26/zb-27 are NOT on disk —
nothing to row.

| Lane | Receipt | Standing | Exit record |
|---|---|---|---|
| ZB-24 | receipts/zb-24-final-index.md | ALIVE (contracted scope, self-row; the gate UNCOVERED [] is the witness) | mention-level coverage gate exit 0 over 131 receipt stems (10 uncovered at lane start, 0 after its rows landed); census 130-before / 131-with own receipt |
| ZB-21 | receipts/zb-21-observation-drift.md | PARTIAL — census and re-pin plan only; the re-pin itself runs at terminal HEAD per the sweep sequence | `observe_self_host.py --check` ×2 → exit 2 typed GGEN_SELF_HOST_DRIFT (deterministic); ontology.ttl sha256 identical before/after; committed observation revision ed3f1dfb1 vs HEAD 58a3b4108 (3 behind, content not stale) |
| ZD-60 | receipts/zd-60-ds7-tighten.md | ALIVE (verification ladder "all ALIVE" this session; closes DS-7-D1/D2/D3; DS-7-D1's original guard proven UNREACHABLE, disclosed) | tightened suites green ×2 (`execute_test` 8/0, `lease_epoch_test` 23/0, exit 0 both); full `cargo test -p ggen-sjira` exit 0, 219 passed / 0 failed; mutation battery 20/20 FLIPPED, gate PASS exit 0, 0 restore failures |
| ZD-88 | receipts/zd-88-pre-landing.md | Verdict UNSAFE — sweep must not land red (3 of 6 gates RED; every red classified file:line; UNSTATED as a standing-vocabulary word) | `cargo fmt --all -- --check` exit 1 (flap class confirmed); clippy ggen-abb-sbb exit 101 (settled lint drift, retry-after-60s did not clear); `git diff --check` exit 2 (6 files blank-at-EOF); check/gap-closure/config/keys/sync-surface gates exit 0 |
| ZD-90 | receipts/zd-90-seed-refresh.md | UNSTATED in body (per-item disposition table carries the standing; every SEED row re-verified from its disk receipt, counts never carried from the prior SEED) | typed-missing check: no zd-60 or zb-21 receipt on disk at that lane's run (both since landed and rowed above); `find` + `grep -rln ZD-60/ZB-21` over four repos' docs trees → 0 hits |
| ZD-101 | receipts/zd-101-sync-verifies.md | UNSTATED in body (results table carries the evidence; read-only lane, no edits, no git state changes) | root dogfood `sync run` exit 0 (25 unchanged, 4 written) + `receipt verify` exit 0 valid/signed; self-host + 3 example roots' `sync run` exit 0 (unchanged/tracked); zero FM-LAW-015 refusals across all 5 syncs |
| ZD-106 | receipts/zd-106-ledger-refresh.md | ALIVE (Standing section; the ledger's falsifiers 1 and 5 executed against it and passed — falsifier 1 caught a real mid-write HEAD move, proving the gate non-vacuous) | STEP 0 freeze re-run 3× — final diff empty (exit 0); STEP 1 uniq -c identical post-move; STEP 2 extraction byte-identical; self-check re-passed after the receipt's landing refreshed ggen 135→136 |
| ZD-108 | receipts/zd-108-soak-evidence.md | ALIVE (Standing section: evidence-complete — AB-20's tokens_delta_stress now carries two independent green witnesses; /tmp log not repo-pinned, this receipt is the durable record) | detached run pid 28463 finished: SOAK1_EXIT=0, SOAK2_EXIT=0, FULL_EXIT=0; final suite `test result: ok. 4 passed; 0 failed` (1320.78s) |
| ZD-110 | receipts/zd-110-clippy-zero.md | UNSTATED in body (Gates table carries the evidence; Chicago-TDD allow header covers 53 of 56 lints, 3 real fixes in kernel_highs_courts.rs) | clippy `--all-targets -D warnings` (kernel-job invocation) exit 0 (56→0); `cargo test --all-targets` exit 0, 110 passed / 0 failed; doc tests 3 passed / 0 failed |

## Post-close receipts (zb-24 final coverage pass, 2026-11-04)

Ten receipt stems on disk carried no row in this file or README.md at the
terminal coverage gate (mention-level, stems vs both index files): two
landed in the zd-58 window without rows (zd-44, zd-48) and eight landed
after zd-58's 120-census (zd-59, zd-66, zd-67, zd-68, zd-69, zd-71, zd-86,
zd-87). They are rowed here and into README.md's post-close section.
Standing words re-read from each receipt body on disk, never from memory;
UNSTATED where the body carries none. Census at this lane's run:
`ls docs/jira/v26.11.3/receipts/ | wc -l` = 130 before this lane's own
receipt, 131 with zb-24-final-index.md on disk. Coverage gate after these
rows: UNCOVERED [] (exit 0). zd-60 / zd-70 / zd-76 / zd-77 / zd-79 / zd-85 /
zd-88 / zd-90 / zd-91 / zd-96 are NOT on disk — nothing to row.

| Lane | Receipt | Standing | Exit record |
|---|---|---|---|
| ZD-44 | receipts/zd-44-skip-flag.md | CARRY-04 CLOSED — resolved state (a): flag already wired CLI→gate; gap closed = bypass-marked receipt law landed with fail-closed court | `sync_run_skip` court 2 passed; 0 failed; `cargo test -p ggen-engine --lib` 236/0 (2 ignored); full `--test cli_boundary` 33/0; scratch consumer: no flag → FM-LAW-015 exit 1, `--skip-ea-admission` → exit 0 with receipt bypass mark; 3 failing tests in the full run classified pre-existing, none session-introduced |
| ZD-48 | receipts/zd-48-fm-codes.md | UNSTATED in body (courts table carries the evidence; census stale-premise corrected: 52 `none` codes at start, not 37; 45 of 119 at end) | 8 FM courts ×2 → 8 passed / 0 failed across 5 files (run 2 on a lane-local target root after a concurrent lane swept the shared `target/debug`, disclosed); parity court red pre-existing on HEAD → 3 passed / 0 failed after the FM-KEY-013 registry row; 45 remaining `none` codes typed 12 U / 21 P / 12 T |
| ZD-59 | receipts/zd-59-carry05.md | CARRY-05 RESOLVED-DISCLOSED; test surface ALIVE under ambient ggen 26.11.3 | `sync_surface_test` ×2 → 6 passed; 0 failed, exit 0 (14.84s/14.33s); closing fixture commit e9829bead verified; descope-ledger rows closed only after reading their landing receipts (zd-45/zd-46/zd-47); rows without landed closing receipts deliberately left open |
| ZD-66 | receipts/zd-66-doc-clippy.md | ALIVE for the depgraph.rs:345 fix + two whitespace fixes; crate-level clippy gate PARTIAL (blocked solely on :38, out of lane) | `git diff --check` on both fixed files clean exit 0; clippy `--all-targets -D warnings` (the abb-sbb-manufacture.yml:40 invocation): :345 lint gone; `cargo check --locked -p ggen-abb-sbb` exit 0 |
| ZD-67 | receipts/zd-67-ea-batch2.md | ALIVE (all commands run this session, exits cited inline) | `validate_ea_graph` over all four graphs → 4 ADMITTED lines, zero REFUSED; per-root sync exits tai 0, cyberpunk 0 ×2 idempotent, inventoryctl 0, lsp-contract 0; pre-pinning admission refused SBB_DIGEST_MISMATCH (non-vacuity); FM-PACK-008 re-lock executed; CI fix identified as commit-surface (untracked ea.graph.json), no workflow edit |
| ZD-68 | receipts/zd-68-premise-rot.md | UNSTATED in body (verification ladder carries the evidence; premise-rot law applied per the testing-discipline SDK-parity register) | repair-constructor embedded python ×2 exit 0 idempotent green; self-host seed `sync run` ×2 exit 0 (10m11s run 1) + receipt verify; tpot2 rotted pin observed failing before the applied-markers fix (drift-refusal path reachable — non-vacuity); book consumer test re-derived per-consumer (graph present → real admission; absent → per-consumer opt-out, never a global bypass) |
| ZD-69 | receipts/zd-69-enospc-lean.md | FIXED (uncommitted, working tree, per body Status); real gate is the next PR-#824 push rerun — UNVERIFIED by design | YAML `safe_load` OK ×4; disk-hygiene blocks added to examples-core-team-constructor.yml + gall-001-replay.yml and ci.yml's integration_deep reclaim upgraded (with `df -h /` before/after); lean-action step `build: true` → `build: false` (manifest generated in-job via the existing `lake update` step); committing lake-manifest.json preferred but none exists in-tree, disclosed |
| ZD-71 | receipts/zd-71-runbook-refresh2.md | PARTIAL — documentation refresh; every value from a live command this session; no gates executed, no git transitions | live ladder: `git fetch` + rev-parse/merge-base per repo (6 SHAs cat-file verified `commit`); `gh pr view 824` live (14 failing / 0 pending, fail-set composition shifted); cgap dirty 23→2; marketplace dirty 1→28; baseline.json 347 packs (ALIVE 326 / WARN 13 / REFUSED 7 / SKIPPED 1); dry-run ladder 7/20 exit 0 |
| ZD-86 | receipts/zd-86-tai-evidence.md | Workflow-level ALIVE (ZB-19's two suspected defects do not exist — both misattributions from echoed script text); whitespace unblocks fixed, working tree verified exit 0; ea.graph.json staged + kernel-validated; full TAI job green on runners UNVERIFIED (needs next push) | `gh run view` on both runs → real fail steps named ("Manufacture TAI-REBUILD-001 twice" FM-LAW-015 on the untracked ea.graph.json; docs_deep `git diff --check` exit 2 on 2 whitespace defects); `git diff --check` working tree exit 0 after; `validate_ea_graph` exit 0 ADMITTED; yaml.safe_load ×3 workflows OK; SLA minimal-unblock disclosures: CLAUDE.md EOF + zd-16:22 trailing space (other lanes' files) |
| ZD-87 | receipts/zd-87-fmkey-ttl.md | UNSTATED in body (three-way cross-check + court evidence carry it; closes the pre-existing `fm_diagnostic_codes_ttl_matches_real_source` failure ZD-44 classified) | `fm_diagnostic_codes_ttl_matches_real_source` ×2 → 1 passed; 0 failed; full product_mirror_conformance 3 passed / 0 failed; `prod:fm_KEY_13` declared grounded in the file's existing DiagnosticCode individuals |
