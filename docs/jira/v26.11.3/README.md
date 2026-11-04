# v26.11.3 Wave Index

ggen v26.11.3 — ABB/SBB hardening so `/Users/sac/arcus` can consume the kernel:
composer+kernel as complementary gates, typed refusal boundary, ea.graph.json
fail-loud contract, Turtle+SHACL authoring projection, digest-bound qualification.
Key sources: `crates/ggen-abb-sbb/`, `ggen-engine` sync.rs Stage 2c (~1007-1095),
`docs/rfc/v26.9.26/abb-sbb-implementation.md`.
Canonical checkout: `/Users/sac/ggen`. No worktrees; coordinator owns git;
no lane runs git commands.

Standing vocabulary (law): ALIVE / PARTIAL / BLOCKED / UNVERIFIED / REFUSED.
Never round up. Every lane status below is cited to a receipt and was re-read
from the receipt body on disk at index time (2026-11-03, cl-close-2 lane);
this file is not scaffold state — the lane, R80, E80 and new-wave tables are
populated from receipts on disk.

Exit record law: A gate is witnessed by an exit record: either (a) the process exit status, or (b) cargo's "N passed; 0 failed" summary line. A passed-count without a failed-count is not an exit record.

## Wave documents

| Doc | Role |
|---|---|
| [_LANES.md](_LANES.md) | Lane partition: 6 lanes (L01-L06), sole-writer ownership, off-limits |
| [RESOLUTIONS.md](RESOLUTIONS.md) | Decision log seeded with hardening resolutions (R1-R5) |
| [WO-08](WO-08-arcus-ea-graph.md) | arcus EA graph + pack, closed PARTIAL-ALIVE |
| [WO-09](WO-09-working-example.md) | first real consumer run + guide, closed ALIVE |
| [abb-sbb-consumer-guide](../../../abb-sbb-consumer-guide.md) | WO-09 deliverable: consumer guide |

## Lane completion table

Status re-read from receipts/ at 2026-11-03 reconciliation (cl-o); every
ALIVE row had its gate re-run green at lane-close (exits in RECEIPTS.md and
closure/waves-wo08-wo09.md). WO-08/09 were left IN-PROGRESS by LC1 pending
the coordinator call; LC2's closure record
(/Users/sac/arcus/docs/jira/v26.11.3/closure/waves-wo08-wo09.md) re-ran all
gates green and closes them — reconciled to that record.

| WO | Scope | Standing | Receipt(s) |
|---|---|---|---|
| WO-01 | Stage 2c wiring + CLI flag + fail-closed depgraph | ALIVE | g1-stage2c.md, k8-cli-flag-depgraph.md |
| WO-02 | `ggen ea` CLI surface (plan/admit/manufacture/replay/verify) | ALIVE | g8-cli |
| WO-03 | Compose plan persistence + topological order | ALIVE | g4-composer |
| WO-04 | EA graph schema + parity court + projection | ALIVE | g7-schema, x3-projection |
| WO-05 | Typed boundary-refusal fences (generated registry) | ALIVE | g5-refusal-fences |
| WO-06 | Envelope court reconcile (R7/R8/R9, R10→R11) | ALIVE | i5-wo06-reconcile |
| WO-07 | Depgraph hardening + typed-refusal flip | ALIVE | g6-depgraph, k1-refusal-flip |
| WO-08 | arcus EA graph consumer | PARTIAL-ALIVE | i1/a2/k5/k7/x6 (arcus receipts/); closure at /Users/sac/arcus/docs/jira/v26.11.3/closure/waves-wo08-wo09.md |
| WO-09 | Working example end-to-end | ALIVE | x4-consumer-guide, k6-self-dogfood |

## Receipts

Per-lane receipts land in this directory as work completes; the lane table
above is re-read from them at every update (stale-by-default rule applies to
any count recalled from memory). This section is the register of truth for
lane status.

## Late E80 landing (post-plan, law 5)

| Lane | Receipt | Standing | Gate (exit record) |
|---|---|---|---|
| E80-04 | receipts/e80-04.md | ALIVE | SCIP-backed SymbolGraph for the blast-radius gate (REDUCE); blast_radius_scip_test green, exit 0; scope note: SCIP radii are callers*-only until the scip emitter encodes subtype relations |

## New-wave receipts (cl-close-2, 2026-11-03)

Rows below cover every receipt on disk in `receipts/` that had no index row
before this pass (63-receipt orphan census, SE-1b refinement absorbed). Standing
words re-read from each receipt body on disk, never from memory; gate evidence
is an exit-status or "N passed; 0 failed" line quoted from the receipt. Where a
receipt body carries no standing word, the cell reads UNSTATED — never rounded
up to ALIVE.

| Lane | Receipt | Standing | Gate (exit record) |
|---|---|---|---|
| CL-CLOSE | receipts/cl-close.md | ALIVE (contracted scope) | index-only close receipt; exit gate PASS exit 0 over the five index surfaces (receipt §Standing) |
| CL-CLOSE-2 | receipts/cl-close-2.md | ALIVE (contracted scope) | full new-wave census close: 176-receipt gate PASS (all four checks green) over the exact subjects on disk; this receipt is self-subject (landed after its own gate run, disclosed ordering in receipt) |
| CL-D | receipts/cl-d-fmlaw-fixtures.md | PARTIAL_ALIVE | "218 passed / 0 failed" across 30 owned/related targets (affected-target rerun 39 passed / 0 failed) |
| CL-I | receipts/cl-i-lib-wiring.md | ALIVE | lib_wiring_test "4 passed, 0 failed"; tokens_delta+moniker+diagnostics_rules "32 passed, 0 failed" |
| CL-O | receipts/cl-o-readme-reconcile.md | PARTIAL-ALIVE | docs lane, no owned code; cited receipts read, not re-run |
| CL-Q | receipts/cl-q-k2-recovery.md | ALIVE | "155 passed / 0 failed / 22 targets ok", exit 0 (`cargo test -p ggen-engine --no-fail-fast`, 22 owned targets) |
| DS-1 | receipts/ds-1.md | UNSTATED (gates green) | reconcile_test "13 passed; 0 failed"; full crate "197 passed; 0 failed (19 binaries)" |
| DS-2 | receipts/ds-2.md | UNSTATED (gates green) | complete_test "8 passed; 0 failed"; full suite 19 binaries all ok, exit 0 |
| SJ-COMPLETE | receipts/complete-receipt.md | ALIVE | complete_test "8 passed, 0 failed, exit 0"; court pinned this session (landed mid-pass, absorbed at re-census) |
| G3 | receipts/g3-kernel-highs.md | ALIVE (owned-scope) | "10 test binaries, 106 tests passed, 0 failed, 0 ignored" — kernel-highs court; this receipt predates the lane tables and had no row until the cl-close-2 census caught it (gate-falsified gap, now rowed) |
| DS-3 | receipts/ds-3.md | ALIVE | "all gates executed this session, exit 0" |
| DS-4 | receipts/ds-4.md | UNSTATED (gates green) | projection_test "12 passed; 0 failed"; census 211 passed / 0 failed |
| DS-5 | receipts/ds-5-baseline.md | ALIVE | "124 passed / 0 failed" across 12 compiling targets, individually |
| DS-6 | receipts/ds-6.md | PARTIAL_ALIVE | verification ladder in receipt; wiring recorded, not applied (justfile untouched per charter) |
| DS-7 | receipts/ds-7-mutation.md | UNSTATED (battery receipt; landed mid-pass, absorbed at re-census 2026-11-03) | mutation battery: 16/18 criteria FLIPPED with witnessed runs; gate FAILED overall by design (3 criteria did not flip); 3 unpinned-surface defects recorded (DS-7-D1..D3), all seven shimmed files restored sha256-exact |
| DS-8 | receipts/ds-8-cli-skeleton.md | UNSTATED (descope receipt; landed during gate run, absorbed at final re-census) | CLI skeleton real invocations: `sjira admit` valid payload exit 0; missing-field exit 1 `REFUSED: Deserialization`; nonexistent file exit 2; unknown subcommand exit 2 typed; no-args exit 2 usage |
| DS-7-ART | receipts/sjira-mutation-battery.md | UNSTATED (battery artifact receipt; script exit 1 = gate FAIL by design) | combined flip-set record, 21 criteria: 17 FLIPPED; 3 non-flip divergences registered (LEAS-C08 VACUOUS + EXEC-C03 class); script exit 1 — the gate failing is the battery's designed outcome, not a defect |
| DS-9 | receipts/ds-9.md | PARTIAL_ALIVE | xaas_loop_test "4 passed, 0 failed"; full gate 211 passed / 0 failed across 19 binaries |
| DS-10 | receipts/ds-10.md | UNSTATED (gates green) | execute_lease_test "8 passed, 0 failed"; full census 165 passed / 1 failed (the 1 pre-existing cross-lane staging-dir leak, not session-introduced) |
| E80-01 | receipts/e80-01.md | UNSTATED (gates green) | `cargo test -p ggen-cli-lib` PASS — "260 passed / 0 failed / 9 ignored" |
| E80-03 | receipts/e80-03.md | PARTIAL_ALIVE | edits_bridge_test "11 passed; 0 failed"; full crate "0 failed across 11 test binaries" |
| E80-05 | receipts/e80-05.md | ALIVE | handler_wiring_test "3 passed, 0 failed"; full ggen-lsp suite 31 `test result: ok` groups, 0 failures (one pre-existing env failure disclosed) |
| E80-08 | receipts/e80-08.md | ALIVE | execute_test "6/6 passed, 0 failed"; full suite "125 passed, 0 failed" across 10 targets |
| E80-09 | receipts/e80-09.md | ALIVE | lease_epoch_test "22 passed; 0 failed"; full crate "125 passed; 0 failed" |
| E80-10 | receipts/e80-10.md | ALIVE | composition_plan_feed_test "6 passed / 0 failed"; cli_boundary "32 passed / 0 failed" unregressed; shim-court flip witnessed |
| F1 | receipts/f1-chain-rule.md | ALIVE | receipt_chain_rule_test "4 passed; 0 failed" |
| F2 | receipts/f2-declarative-admission.md | PARTIAL_ALIVE | owned court green; ~100 pre-existing declarative fixtures fallout recorded, not owned |
| K2 | receipts/k2-test-fixes.md | LANDED (gates green) | per-target all `test result: ok` (sync_e2e 11/0, gall_checkpoint_001_replay_e2e 11/0, envelope_caps_e2e 4/0) |
| P11 | receipts/p11-fmgen007.md | ALIVE (capability) / PARTIAL-BLOCKED (fixture debt) | TemplateSource::Pack court "3 passed; 0 failed"; FM-GEN-007 Git/Package variants UNSUPPORTED by design |
| GL-1 | receipts/gl-1.md | ALIVE | "31 test suites `test result: ok`"; full `cargo test -p ggen-lsp` exit 0 cargo code; 1 suite FAILED disclosed as pre-existing |
| SJ-1 | receipts/sj-1.md | ALIVE (landed slices; 3,4,5,6 DEFERRED) | final gate "146 passed, 0 failed" |
| SKILL-UP | receipts/skill-up-rewggen.md | ALIVE | "bash ~/.claude/skills/run-ggen/smoke.sh → exit 0, SMOKE TEST PASSED" |
| X1 | receipts/x1-sjira-kernel.md | ALIVE (core slice; deferred slices UNBUILT) | ggen-sjira "17 total, 0 failed"; `cargo check -p ggen-cli-lib` green |
| SA-6 | receipts/sa-6.md | ALIVE | `ggen sync run` exit 0 with rendered output verified; dry-run exit 0 |
| SA-7 | receipts/sa-7.md | ALIVE | all four contract gates exit 0 (dry-run, sync ×2 idempotent, `cargo check -p ggen-cli-lib` exit 0, --tests exit 0) |
| SA-8 | receipts/sa-8.md | PASS (53/54; 1 BLOCKED) | "53/54 exit 0" over 54 live example manifests; the 1 non-pass is BLOCKED (typed, outside-ownership fix named) |
| SA-12 | receipts/sa-12.md | CONFORMANT post-fix (audit) | "Gates (real outputs)" section in receipt; ggen/ conformance to run-ggen verified live |
| SA-13 | receipts/sa-13.md | ALIVE | "all commands and exits above are from this session … shim-court witnessed RED-then-GREEN with byte-exact restore" |
| SA-14 | receipts/sa-14.md | UNSTATED (no-op verified) | "Statuses untouched: ALIVE 292, REFUSED 8, WARN 9, SKIPPED 1"; JSON parse exit 0, 310 packs, entry count unchanged |
| SA-15 | receipts/sa-15.md | UNSTATED (audit receipt) | no standing word in body; shim-court flip proof + reconciliation recorded in receipt |
| SA-16 | receipts/sa-16.md | ALIVE | fm_registry_parity_test "3 passed; 0 failed" re-run after restore; shim-court flip witnessed |
| SA-17 | receipts/sa-17-census.md | ALIVE (census) / PARTIAL (wave) | census commands + outputs in receipt; "1 of 17 lane receipts on disk" at census time — superseded by this table |
| GA-1 | receipts/ga-1-aoq-ontology.md | ALIVE | ontology parsed + asserted this session; frozen-band assert passed |
| GA-3 | receipts/ga-3-aoq-templates.md | ALIVE | real `ggen sync run` renders executed on the exact subject; `cc -std=c99 -ffreestanding` exit 0 |
| GA-7 | receipts/ga-7-control-wiring.md | ALIVE | both recipes executed with exits above; falsifier `just sjira-control-court` exit 1 = flip lost |
| GA-9 | receipts/ga-9-receiptctl.md | ALIVE | examples/receiptctl dry-run + real sync green; "168 tests, 0 failed (15 suites)" |

### In-flight-expected (law 5)

- DS-8 — no receipt on disk at the initial pass; ds-8-cli-skeleton.md landed
  during the exit-gate run and was absorbed with its own row above (final
  re-census 2026-11-03).
- GA-2, GA-6 — no receipts on disk; GA lane receipts landed as ga-1/3/7/9
  (ggen), ga-4/8 (arcus), ga-5 (cgap).
- PG-A lanes — no pg-a* receipts landed on disk in any tree.

## R80 reverse-80/20 lanes (status as of 2026-11-03 close)

Receipt census reconciliation (cl-close-2): each R80-N row above/below cites
its landed receipt `receipts/r80-01.md` … `receipts/r80-14.md` by stem —
R80-1→r80-01, R80-2→r80-02, R80-3→r80-03, R80-4→r80-04, R80-5→r80-05,
R80-6→r80-06, R80-7→r80-07, R80-8→r80-08, R80-9→r80-09, R80-10→r80-10,
R80-11→r80-11, R80-12→r80-12, R80-13→r80-13 (hosted in
/Users/sac/claude-code-config-lsp/docs/jira/v26.11.3/receipts/), R80-14→r80-14.

Status cited to receipts in `receipts/` and gate re-runs in
[closure/r80-wave-close.md](closure/r80-wave-close.md) (all gates re-run at
close, exit 0). L01-L06 table above is unchanged by this section.

| Lane | Surface | Status |
|---|---|---|
| R80-1 | Tree-sitter position-awareness (ggen-lsp) | CLOSED (ALIVE as delivered module, 6/6 tokens_delta_test; server-handler wiring deferred to integration lanes per r80-01.md — PARTIAL at server level, not rounded up) |
| R80-2 | textDocument/moniker | CLOSED (ALIVE, 11/11) |
| R80-3 | ggen-scip SCIP index | CLOSED (ALIVE, 13/13) |
| R80-4 | SCIP ontology layer (scip.ttl + SHACL) | CLOSED (PARTIAL_ALIVE, 7/7) |
| R80-5 | Transactional mutations + rollback journal | CLOSED (ALIVE, 17/17) |
| R80-6 | Blast-radius gate | CLOSED (gate ALIVE 6/6; SCIP + write-path wiring open follow-ups) |
| R80-7 | Cryptographic transition receipts | CLOSED (ALIVE, 4/4) |
| R80-8 | Frontier slice | CLOSED (ALIVE, 10/10) |
| R80-9 | File-backed ledger | CLOSED (ALIVE, 15/15) |
| R80-10 | Authority law port | CLOSED (ALIVE, 20/20) |
| R80-11 | Edits→WorkOrder bridge | CLOSED (PARTIAL — tests 9/9; receipt standing UNKNOWN, not rounded up) |
| R80-12 | Diagnostic rules GGEN-DIAG-NNN | CLOSED (ALIVE 15/15 as tests; lib.rs wiring one-liner open) |
| R80-13 | Compositor child verification | CLOSED ALIVE (receipt VERIFIED at /Users/sac/claude-code-config-lsp/docs/jira/v26.11.3/receipts/r80-13.md — the reconciliation search missed the config-lsp receipts dir; individual pre-existed in compositor.ttl, build exit 0, stdio serving confirmed, 21/21 routing tests green; individual was already landed — verification lane, not creation) |
| R80-14 | Admission-controller synthesis | CLOSED (ALIVE, 10/10) |

Overall R80 wave standing: **PARTIAL_ALIVE** — 14 of 14 lanes closed (R80-11
closed without rounding up: receipt standing UNKNOWN). R80-1's row was
reconciled to its landed receipt `receipts/r80-01.md` at 2026-11-03 (the
closure record's surface table predates that landing). R80-13's row was
reconciled to its landed receipt at
`/Users/sac/claude-code-config-lsp/docs/jira/v26.11.3/receipts/r80-13.md`
(ADMITTED — verification lane, not creation; the reconciliation search that
flagged it absent missed the config-lsp receipts dir). The closure record's
§2 rows for R80-1/R80-13 ("receipt absent — do not close") are stale against
these later landings and are disclosed, not edited — the closure record is
not owned by this lane. §3/§4/§5 carry disclosed follow-ups; see
[closure/milestone-final-sweep.md](closure/milestone-final-sweep.md) for the
final lane-status table and the milestone close verdict.

## Post-close receipts (xd-17 transcribe, 2026-11-04)

XD-3 found these 12 receipts landed after the canonical indexes' last write
(2026-11-03 18:27-18:38) with rows only in the cl-close-2.md addendum; this
section is the transcription into the canonical index (XD-3 successor lane;
drift finding: receipts/xd-3-index-refresh.md).
Standing words re-read from each receipt body on disk, never from memory;
UNSTATED used where the body carries no standing word. xd-8 and xd-10 landed
after XD-3 and are rowed here first. xd-13/xd-14/xd-15 were not on disk at
this lane's run (`ls docs/jira/v26.11.3/receipts/ | wc -l` = 92 before this
lane's receipt, 93 with xd-17-index-transcribe.md on disk).
xd-13-fmt-sweep.md landed mid-pass during this lane's verification run and
is absorbed with its own row (disclosed in-row). xd-14 landed later and is
rowed in the zd-23 refresh rows below. Re-census at the zd-23 refresh
(2026-11-04): 103 receipt .md files on disk before zd-23's own receipt, 104
with it; the 7 receipts landed after XD-17 (ab-20, xd-14, xd-18, zd-5, zd-9,
zd-11, zd-14) are rowed at the end of this table. zd-15/zd-16/zd-17/zd-18
were not on disk at the zd-23 run. zd-21-shacl-syntax.md landed mid-pass
during the zd-23 verification run and zd-15-template-fmt.md +
zd-24-claude-markers.md landed during the final coverage re-run (107
receipt .md files at final re-census) — each absorbed with its own row.
zd-21 closes the industry-TTL failure ZD-14 disclosed out of scope; zd-15
closes ZD-14's disclosed fmt-gate flap (sync+fmt now a fixed point,
byte-identical across two sync runs). Re-census at the zd-58 refresh
(2026-11-04): 119 receipt .md files on disk before zd-58's own receipt,
120 with it; the 12 receipts landed after zd-23's census (zd-16, zd-17,
zd-27, zd-31, zd-38, zd-39, zd-40, zd-45, zd-46, zd-47, zd-50, zd-51) are
rowed at the end of this table. Re-census at the zb-24 final pass
(2026-11-04): 130 receipt .md files on disk before zb-24's own receipt,
131 with it; the 10 stems the terminal mention-level coverage gate found
uncovered (zd-44, zd-48, zd-59, zd-66, zd-67, zd-68, zd-69, zd-71, zd-86,
zd-87) are rowed at the end of this table — gate now exits UNCOVERED [].

| Lane | Receipt | Standing | Gate (exit record) |
|---|---|---|---|
| SX-1 | receipts/sx-1-reds.md | UNSTATED in body (green tables carry the evidence) | `cargo test -p ggen-lsp --test manifest_contract_test` ×2 exit 0 (2/2); `cargo test -p ggen-cli-lib --test performance` ×2 exit 0 (10 passed, 6 ignored); `zsh scripts/bench_gates.sh` exit 0 ALL BENCH GATES PASS; G3 fail branch exit 1 typed EA-BIN-MISSING |
| AF-7 | receipts/af-7-path-reconcile.md | UNSTATED in body | 5 byte-exact copies (sizes 4156/4036/3562/4761/7823), pointer `head -2` on all 5 originals, post-edit sweep 9 ab-* canonical |
| AF-5 | receipts/af-5-sync-surface.md | ALIVE | `cargo test -p ggen-cli-lib --test sync_surface_test` → 6 passed; 0 failed (exit 0); performance 10 passed; 0 failed; 6 ignored |
| AB-14 | receipts/ab-14-chain-scale.md | ALIVE (Status section) | ggen-engine `receipt_chain_scale_stress` 2 passed ×3 (42.83s/61.89s/56.25s); one pre-existing deterministic hex-prefix-lottery failure disclosed (receipt_verb_e2e), root-caused not session-introduced |
| GA-11 | receipts/ga-11-cscb-pack.md | ALIVE (cscb-pack) | witness court exit 0 (4/4 FLIP); cgap sync ×2 exit 0, `written: []` replay; shim-court 4/4 refusal + byte-exact restore |
| AB-13 | receipts/ab-13-kernel-stress.md | UNSTATED in body | `cargo test -p ggen-sjira --test kernel_stress --release` ×2 → 5 passed; 0 failed (254.4s/245.8s); full `cargo test -p ggen-sjira --release` 205 passed / 0 failed over 20 targets |
| GA-12 | receipts/ga-12-tort-pack.md | ALIVE (tort-pack; tort-1/3/4 ceilings TARGET-UNMEASURED, pending AF-37) | witness court exit 0 (4/4 FLIP); cgap sync ×2 (written 2, then `[]`); marketplace validate exit 0 |
| AF-42 | receipts/af-42-marketplace-fixes.md | ALIVE | `marketplace.py validate` exit 0 (packs=347 manifests=347); `check_gate_witness_courts.py` exit 0, 50 courts ALIVE; baseline.json 313 entries |
| GD-2 | receipts/gd-2-ggen-dod.md | ALIVE | `ggen sync run` #1 exit 0 (written 2 outputs, 4 gates passed, receipt valid/signed); #2 exit 0 `written: []`; both shim courts flipped w/ typed refusals + byte-exact restores |
| XC-8 | receipts/xc-8-cve-bump.md | UNSTATED in body (registered divergence carried) | `cargo check --workspace` exit 0; `cargo audit` → 2 vulnerabilities (from 18); quick-xml blocked upstream (process_mining pins ^0.37.4) |
| XC-7 | receipts/xc-7-ci-unblock.md | UNSTATED in body (post-push CI re-run pending, disclosed) | `cargo check --workspace` exit 0; `cargo fmt --check -p ggen-sjira` exit 0; sjira tests 12+4 passed 0 failed; CI checks re-run deferred to post-push |
| XD-2 | receipts/xd-2-key-perms.md | ALIVE (chmod + verify path); code check = FOLLOW-UP (UNVERIFIED by design) | `ls -l .ggen/keys/` 0600/0644 as intended, exit 0; `ggen receipt verify` exit 0, signature_valid true, chain_hash 4e0210e4…; FM-KEY-013 seam identified file:line, not landed |
| XD-8 | receipts/xd-8-key-mode-gate.md | ALIVE (code check + negative-path refusal) | `cargo test -p ggen-engine --lib keys` 19 passed; 0 failed ×2 exit 0; focused `overly_permissive_preexisting_signing_key_refuses` 1 passed; 0 failed exit 0; `cargo check --workspace` exit 0 |
| XD-10 | receipts/xd-10-publish-blockers.md | ALIVE (three owned files) | `cargo check -p ggen-engine` exit 0 (compiled as ggen-engine v26.11.3); `cargo build -p ggen-engine` exit 0 (v26.11.3) |
| XD-13 | receipts/xd-13-fmt-sweep.md | ALIVE (landed mid-pass, absorbed at verification re-census) | `cargo fmt --all` exit 0 (83 files reformatted); `cargo fmt --all -- --check` exit 0; `git diff --check` exit 0; `cargo check --workspace` exit 0 |
| DS-8 | receipts/ds-8.md | UNSTATED in body (gates green; distinct from the ds-8-cli-skeleton descope receipt — this is the full DS-8 lane receipt, landed post-index and found uncovered by this lane's coverage check) | `cargo test -p ggen-cli-lib --test cli_sjira_test` exit 0, 17 passed / 0 failed; clippy 0 findings; binary witnesses: admit valid exit 0, malformed exit 1 typed REFUSED:SEMANTIC_JIRA, epoch_check exit 5 Andon class |
| XD-11 | receipts/xd-11-fmlaw015.md | PASS standing — sync_surface_test PASS green ×2 + full suite exit 0; PUB-1 blocking row resolvable to CLOSED (landed mid-pass, absorbed at re-census) | `cargo test -p ggen-cli-lib --test sync_surface_test` ×2 exit 0 (6/6); full `cargo test -p ggen-cli-lib` exit 0 (604 passed, 0 failed, 62 binaries) |
| XD-15 | receipts/xd-15-tcps-lock.md | PARTIAL_ALIVE (artifact produced, uncommitted; landed mid-pass, absorbed at re-census) | tcps_chicago_tdd_8pow4 17 passed; 0 failed; tcps_chicago_tdd_1000x 16 passed; 0 failed; `ggen sync run --skip-ea-admission` exit 0 wrote ggen.lock (1288 bytes, 8 packs) |
| AB-20 | receipts/ab-20-tokens-stress.md | UNSTATED in body (gate tables carry the evidence; disclosures: release profile, one out-of-lane marketplace.json version sync, in-lane test-accounting fix) | `cargo test -p ggen-lsp --release --test tokens_delta_stress` ×2 exit 0 (4 passed; 0 failed; soak 2378.87s/2327.41s); full ggen-lsp suite (4 stress tests skipped by name) exit 0 |
| XD-14 | receipts/xd-14-ea-graph-ci.md | ALIVE for 3 of 4 checks (zero-code, part-passport, root dogfood sync + receipt verify); PARTIAL for ggen-self-host (FM-LAW-015 satisfied, blocked downstream on the observation gate, closed since by zd-9) | `validate_ea_graph` ×4 graphs exit 0, all 8 SBBs ADMITTED, zero REFUSED; root `ggen sync run` + `receipt verify` exit 0; inventoryctl + part-passport `ggen sync run` exit 0; self-host exit 1 on pre-existing FM-PACK-013 (other lane's scope, disclosed) |
| XD-18 | receipts/xd-18-ci-residuals.md | UNSTATED in body (verification ladder carries the evidence; concurrency disclosure: other lanes' uncommitted tree edits left untouched) | `python3 book/scripts/validate_gap_closure.py` exit 0 (obligations=25, errors=0); YAML parse ×3 exit 0; `bash -n` on 23 extracted `run:` blocks exit 0 ×23; rustfmt `--check` on 2 owned files exit 0 |
| ZD-5 | receipts/zd-5-sccache.md | FIXED (uncommitted, per body Status); CI job execution NOT run — first push-triggered run named the real gate, so the CI-path fix itself is UNVERIFIED by design | YAML `safe_load` exit 0; pinned install-action SHA diff-verified identical to setup-ggen-build STEP 2a; full job execution NOT run (no local runner), disclosed |
| ZD-9 | receipts/zd-9-self-host-observation.md | ALIVE | `observe_self_host.py` exit 0 (13-min census); `verify_observation.py` exit 0 GGEN_SELF_HOST_OBSERVATION_VERIFIED; self-host `ggen sync run` exit 0 (26.10.11 and 26.11.3 binaries); root dogfood sync exit 0 under repo-built v26.11.3; ambient 26.10.11 root sync exit 1 FM-CHAIN-003 = pre-existing binary/log schema skew, disclosed |
| ZD-11 | receipts/zd-11-sync431.md | RESOLVED-DISCLOSED (pinned-by-law hardcode kept; no code change); ALIVE for the ruling | `cargo test -p ggen-engine --lib` 236 passed / 0 failed; `cargo build -p ggen-cli-lib --bin ggen` exit 0; `ggen sync run` exit 0; `ggen receipt verify` exit 0 valid/signed; one receipt-chain wall-clock SLO test failure on the shared uncommitted tree root-caused pre-existing, not session-introduced |
| ZD-14 | receipts/zd-14-ci-final.md | Fixes 1-3, 5, 7 ALIVE (gates this session); Fix 4 ALIVE-as-verified (already fixed at HEAD, coordinator brief stale); Fix 6 PARTIAL (owned docs fixed; repo-wide EOF-blank-line class flagged not fixed) | `cargo fmt --all -- --check` + `rustfmt --check refusals.rs` exit 0; YAML parse ×3 + `bash -n` ×2 exit 0; census prints exactly 3 authored files, 5 generated paths tracked; `validate_ea_graph` on examples/affidavit-verify exit 0 ADMITTED + `ggen sync run` exit 0; `validate_gap_closure.py` exit 0 errors=0; book Chicago suite 2 fixed, 1 pre-existing industry-TTL failure disclosed out of scope |
| ZD-21 | receipts/zd-21-shacl-syntax.md | ALIVE (landed mid-pass during the zd-23 verification run, absorbed at re-census; closes the industry-TTL failure ZD-14 disclosed out of scope) | template IRI-delimiter fix in ggen-marketplace (generated file regenerated, not hand-patched); `ggen sync run` in examples/industry/manufacturing-dflss exit 0; rdflib parse OK 21 triples; failing book test green ×2 (5.03s/4.84s); file suite 4 passed, 1 skipped (pre-existing skip) |
| ZD-15 | receipts/zd-15-template-fmt.md | ALIVE (landed during the zd-23 final coverage re-run, absorbed; closes ZD-14's disclosed fmt-gate flap — sync+fmt now a fixed point) | `ggen sync run` ×2 exit 0 (refusals.rs written run 1); `rustfmt --check refusals.rs` exit 0 ×2; `cargo fmt --all -- --check` exit 0; sha256 byte-identical across runs 1→2 (793fddd9…7913227); `cargo test -p ggen-abb-sbb` exit 0 (16+3 crosswalk+3 doc-tests green) |
| ZD-24 | receipts/zd-24-claude-markers.md | RESOLVED (uncommitted); ALIVE for the owned surface; the repo-facts.ttl gap (ggen-scip/ggen-sjira missing rf:Crate individuals, commit 301849a9c) is a pre-existing HEAD defect, BLOCKED for this lane, filed for the coordinator | `grep -c` markers → 0 matches (exit 1 ×2); `wc -l CLAUDE.md` = 70; `cargo test -p ggen-config --test system_crate_map_parity_test` 1 passed / 1 FAILED — failure pre-existing (repo-facts.ttl ↔ Cargo.toml skew, both files match HEAD), disclosed not session-introduced |
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

### Cross-repo receipts (pointer-only)

Receipts on disk in other repos' receipts dirs; rowed pointer-only, matching
the pointer convention the cl-close-2 addendum used (Receipt | Host |
Standing | Exit record). Their canonical indexes live in the host repos.

| Receipt | Host | Standing (from body) | Exit record (from body) |
|---|---|---|---|
| cgap receipts/bk-1-takeover.md | /Users/sac/cgap/docs/jira/v26.11.3/receipts/ | AB-4 ALIVE, AB-5 ALIVE, corridor join ALIVE (AF1-KD-1 CLOSED) | `cargo test -p cgap-crypto` GREEN ×2 (75 passed / 0 failed); riscv32imc + wasm32 `cargo check` exit 0 |
| cgap receipts/af-37-p99-torture.md | same dir | ALIVE (one pre-existing profile_guard.rs doctest failure disclosed) | `macro_p99_torture` release serial ×2 exit 0, debug ×2 exit 0; `cargo test -p cgap-aoq` 22/22 integration binaries ok; doctest exit 1 pre-existing |
| cgap receipts/rx-2-conformance.md | same dir | ALIVE | rfc0089_conformance release ×2 exit 0 (4 passed; 0 failed; 1 ignored) + debug ×2 exit 0; lib courts 14/0 ×2; CBMQS 1/0 ×2 |
| cgap receipts/gd-1-dod-pack.md | same dir | ALIVE for pack surface; PARTIAL overall (dod-006 TARGET-UNMEASURED) | sync ×2 determinism + witness court + shim-court flip, green this session |
| arcus receipts/xd-4-govulncheck.md | /Users/sac/arcus/docs/jira/v26.11.3/receipts/ | ALIVE (scan executed; 0-vuln expectation refuted at go1.23.4, confirmed clean at go1.26.10) | govulncheck ./... exit 3 at default toolchain (40 called stdlib vulns), exit 0 pinned `GOTOOLCHAIN=go1.26.10`; `scripts/vulncheck.sh` wrapper exit 0 |
| ZD-44 | receipts/zd-44-skip-flag.md | CARRY-04 CLOSED — resolved state (a): flag already wired CLI→gate; gap closed = bypass-marked receipt law landed with fail-closed court | `sync_run_skip` court 2 passed; 0 failed; `cargo test -p ggen-engine --lib` 236/0 (2 ignored); full `--test cli_boundary` 33/0; scratch consumer: no flag → FM-LAW-015 exit 1, `--skip-ea-admission` → exit 0 with receipt bypass mark; 3 failing tests in the full run classified pre-existing, none session-introduced |
| ZD-48 | receipts/zd-48-fm-codes.md | UNSTATED in body (courts table carries the evidence; census stale-premise corrected: 52 `none` codes at start, not 37; 45 of 119 at end) | 8 FM courts ×2 → 8 passed / 0 failed across 5 files (run 2 on a lane-local target root after a concurrent lane swept the shared `target/debug`, disclosed); parity court red pre-existing on HEAD → 3 passed / 0 failed after the FM-KEY-013 registry row; 45 remaining `none` codes typed 12 U / 21 P / 12 T |
| ZD-59 | receipts/zd-59-carry05.md | CARRY-05 RESOLVED-DISCLOSED; test surface ALIVE under ambient ggen 26.11.3 | `sync_surface_test` ×2 → 6 passed; 0 failed, exit 0 (14.84s/14.33s); closing fixture commit e9829bead verified; descope-ledger rows closed only after reading their landing receipts (zd-45/zd-46/zd-47); rows without landed closing receipts deliberately left open |
| ZD-66 | receipts/zd-66-doc-clippy.md | ALIVE for the depgraph.rs:345 fix + two whitespace fixes; crate-level clippy gate PARTIAL (blocked solely on :38, out of lane) | `git diff --check` on both fixed files clean exit 0; clippy `--all-targets -D warnings` (the abb-sbb-manufacture.yml:40 invocation): :345 lint gone; `cargo check --locked -p ggen-abb-sbb` exit 0 |
| ZD-67 | receipts/zd-67-ea-batch2.md | ALIVE (all commands run this session, exits cited inline) | `validate_ea_graph` over all four graphs → 4 ADMITTED lines, zero REFUSED; per-root sync exits tai 0, cyberpunk 0 ×2 idempotent, inventoryctl 0, lsp-contract 0; pre-pinning admission refused SBB_DIGEST_MISMATCH (non-vacuity); FM-PACK-008 re-lock executed; CI fix identified as commit-surface (untracked ea.graph.json), no workflow edit |
| ZD-68 | receipts/zd-68-premise-rot.md | UNSTATED in body (verification ladder carries the evidence; premise-rot law applied per the testing-discipline SDK-parity register) | repair-constructor embedded python ×2 exit 0 idempotent green; self-host seed `sync run` ×2 exit 0 (10m11s run 1) + receipt verify; tpot2 rotted pin observed failing before the applied-markers fix (drift-refusal path reachable — non-vacuity); book consumer test re-derived per-consumer (graph present → real admission; absent → per-consumer opt-out, never a global bypass) |
| ZD-69 | receipts/zd-69-enospc-lean.md | FIXED (uncommitted, per body Status); real gate is the next PR-#824 push rerun — UNVERIFIED by design | YAML `safe_load` OK ×4; disk-hygiene blocks added to examples-core-team-constructor.yml + gall-001-replay.yml, ci.yml integration_deep reclaim upgraded (with `df -h /` before/after); lean-action step `build: true` → `build: false` (manifest generated in-job via existing `lake update` step); committing lake-manifest.json preferred but none exists in-tree, disclosed |
| ZD-71 | receipts/zd-71-runbook-refresh2.md | PARTIAL — documentation refresh; every value from a live command this session; no gates executed, no git transitions | live ladder: `git fetch` + rev-parse/merge-base per repo (6 SHAs cat-file verified `commit`); `gh pr view 824` live (14 failing / 0 pending, fail-set composition shifted); cgap dirty 23→2; marketplace dirty 1→28; baseline.json 347 packs (ALIVE 326 / WARN 13 / REFUSED 7 / SKIPPED 1); dry-run ladder 7/20 exit 0 |
| ZD-86 | receipts/zd-86-tai-evidence.md | Workflow-level ALIVE (ZB-19's two suspected defects do not exist — both misattributions from echoed script text); whitespace unblocks fixed, working tree exit 0; ea.graph.json staged + kernel-validated; full TAI job green on runners UNVERIFIED (needs next push) | `gh run view` on both runs → real fail steps named ("Manufacture TAI-REBUILD-001 twice" FM-LAW-015 on the untracked ea.graph.json; docs_deep `git diff --check` exit 2 on 2 whitespace defects); `git diff --check` working tree exit 0 after; `validate_ea_graph` exit 0 ADMITTED; yaml.safe_load ×3 workflows OK; SLA minimal-unblock disclosures: CLAUDE.md EOF + zd-16:22 trailing space (other lanes' files) |
| ZD-87 | receipts/zd-87-fmkey-ttl.md | UNSTATED in body (three-way cross-check + court evidence carry it; closes the pre-existing `fm_diagnostic_codes_ttl_matches_real_source` failure ZD-44 classified) | `fm_diagnostic_codes_ttl_matches_real_source` ×2 → 1 passed; 0 failed; full product_mirror_conformance 3 passed / 0 failed; `prod:fm_KEY_13` declared grounded in the file's existing DiagnosticCode individuals |
| ZB-24 | receipts/zb-24-final-index.md | ALIVE (contracted scope, self-row; gate UNCOVERED [] is the witness) | mention-level coverage gate exit 0 over 131 receipt stems (10 uncovered at lane start, 0 after rows landed); census 130-before / 131-with own receipt |
