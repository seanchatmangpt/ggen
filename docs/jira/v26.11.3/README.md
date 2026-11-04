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
