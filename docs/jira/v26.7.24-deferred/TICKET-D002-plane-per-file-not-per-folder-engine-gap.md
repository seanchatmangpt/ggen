# TICKET-D002: lib/planes/index.ts emits one file, not one folder per plane (ggen engine capability gap)

## Status

DEFERRED — permanent structural limitation, engine capability gap not a coding gap

## Priority

P2 — code-organization/architecture-purity issue only; the single-file output is functionally complete and idempotent, just not decomposed per SPARQL row

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-012-app-layout-and-module-boundaries-generation.md`

## Source

- Research report: Disclosed Gaps Catalog, section (a) Permanent structural limitations
- Citation: "`templates/021_lib_planes_index.tmpl` -> `lib/planes/index.ts`: **one file, not one folder per plane.**... ggen's template model as observed does not support one-file-per-SPARQL-row from a single template... the 'iterate the plane query to also emit multiple files' half of this ticket is BLOCKED on that missing engine capability, not attempted via a side-channel script (would be Epistemic Bypass per `.claude/rules/coding-agent-mistakes.md`)." — `docs/jira/v26.7.23/TICKET-012-app-layout-and-module-boundaries-generation.md:141-149`
- Citation: "PARTIAL_ALIVE: layout generation and the plane-query projection are real and idempotent; true per-plane folder emission is BLOCKED (engine capability gap, not a coding gap), not silently dropped." — `docs/jira/v26.7.23/TICKET-012-app-layout-and-module-boundaries-generation.md:153-154`

## Objective

Determine whether ggen-engine should gain a one-file-per-SPARQL-row emission capability, and if so implement it in ggen-engine (not via a side-channel script in the pack), then re-project TICKET-012's plane layout as one folder per plane.

## Current state

`templates/021_lib_planes_index.tmpl` projects all planes into a single `lib/planes/index.ts` file. This is real, idempotent, and BLOCKED — not silently dropped — on a genuine ggen-engine limitation: the template model as observed does not support emitting multiple output files from one template driven by multiple SPARQL result rows.

## Target state

Either (a) ggen-engine gains a documented multi-file-per-template emission capability and TICKET-012 is re-run to emit one file per plane under `lib/planes/<plane>/`, or (b) this is explicitly accepted as a permanent architectural decision (single-file plane index) and TICKET-012's ticket text is updated to say so rather than calling it BLOCKED.

## Projection classification

- Template: N/A — this ticket is engine-capability research/design, not itself a projection
- Domain data: none — the plane data is already correctly projected, only file granularity is at issue
- Custom code: any engine change belongs in `ggen-engine`'s own crate, not a pack-local script (per this repo's Epistemic Bypass rule)

## Inputs

- `packs/wasm4pm-interview-assist-pack/templates/021_lib_planes_index.tmpl`
- ggen-engine's template/emission pipeline (`crates/ggen-engine/src/sync.rs`, `render`/`write` stages)

## Outputs

- either a new ggen-engine capability (multi-file emission per template) with its own tests, or a documented decision record accepting single-file emission as final

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

If pursued, the new capability is a reusable ggen-engine feature (multi-row-to-multi-file emission), not InterviewAssist-specific logic.

## Domain-data responsibility

None — this is purely an engine/tooling capability question, not a domain-data question.

## Custom-code boundary

None if the decision is to accept single-file emission; if pursued, the engine change itself is core `ggen-engine` code, explicitly not a workaround script in the pack (would be Epistemic Bypass per `.claude/rules/coding-agent-mistakes.md`).

## Exclusions

- no side-channel script in the pack that manually splits the single file into multiple files post-hoc — that would be Epistemic Bypass
- no fabricated claim that per-plane folders already exist

## Implementation steps

1. Confirm the exact limitation by re-reading ggen-engine's template/render/write pipeline (`crates/ggen-engine/src/sync.rs`, `src/template.rs`, `src/write.rs`) to identify precisely why one template cannot emit N files from N SPARQL rows.
2. Weigh the cost of adding multi-file emission against simply accepting single-file plane index as the permanent design (many single-file registries are a reasonable pattern).
3. If pursued: design and implement the capability in `ggen-engine` with its own unit tests (not InterviewAssist-specific).
4. If accepted: update TICKET-012's own ticket text to change 'BLOCKED (engine capability gap)' to a documented accepted-design decision, and remove any lingering ambiguity about whether this is still open work.

## Admission gates

- TICKET-012 (predecessor, must remain PARTIAL_ALIVE or reclassify to ALIVE once resolved)

## Acceptance criteria

Given this ticket is closed, when `lib/planes/` is inspected, then either (a) it contains one subfolder per plane individually generated, or (b) TICKET-012's own text is updated to state single-file emission is the accepted final design, not an open BLOCKED item.

## Negative tests

If a new multi-file capability is added to ggen-engine, run `ggen sync run` twice consecutively and confirm byte-identical output across both per-plane files (same idempotency bar TICKET-012 already applies to the single file).

## Verification ladder

- Unit: ggen-engine multi-file-emission unit test, if pursued
- Integration: `ggen sync run` against the interview-assist pack producing per-plane folders, if pursued
- End-to-end: N/A with reason — this is a tooling-capability ticket, not an application-behavior ticket
- Chaos: N/A with reason — not applicable to a template-emission capability
- Stress: N/A with reason — no volume target defined
- Benchmark: N/A with reason — no perf target defined
- Verifier report: either the new capability's test transcript, or the updated TICKET-012 decision text

## Receipts

- ggen-engine capability test transcript (if pursued)
- updated TICKET-012 ticket text (either path)

## Dependencies

- TICKET-012

## Falsifier

If this ticket is closed while TICKET-012 still says BLOCKED without either a working multi-file capability or an explicit accepted-design update, this ticket is not complete.

## Handoff

None — this is a terminal decision ticket for the plane-layout question.

## Definition of done

- decision made and documented (pursue capability vs. accept single-file design)
- if pursued, capability implemented with tests and idempotency re-verified
- TICKET-012's own text updated to reflect the final decision
