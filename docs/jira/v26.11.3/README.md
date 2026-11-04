# v26.11.3 Wave Index

ggen v26.11.3 — ABB/SBB hardening so `/Users/sac/arcus` can consume the kernel:
composer+kernel as complementary gates, typed refusal boundary, ea.graph.json
fail-loud contract, Turtle+SHACL authoring projection, digest-bound qualification.
Key sources: `crates/ggen-abb-sbb/`, `ggen-engine` sync.rs Stage 2c (~1007-1095),
`docs/rfc/v26.9.26/abb-sbb-implementation.md`.
Canonical checkout: `/Users/sac/ggen`. No worktrees; coordinator owns git;
no lane runs git commands.

Standing vocabulary (law): ALIVE / PARTIAL / BLOCKED / UNVERIFIED / REFUSED.
Never round up. Every lane status below is cited to a receipt; until receipts
land this file is scaffold state — all UNVERIFIED (2026-11-03).

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
| WO-01 | Stage 2c wiring + CLI flag + fail-closed depgraph | ALIVE | g1-stage2c, k8-cli-flag |
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

## R80 reverse-80/20 lanes (status as of 2026-11-03 close)

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
