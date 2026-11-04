# WO-09: End-to-End Working Example — the ABB/SBB Consumer Guide

- **Date**: 2026-11-03
- **Repo**: /Users/sac/ggen
- **Lane**: WRITER (spec) → IMPLEMENT lane delivers `docs/abb-sbb-consumer-guide.md`
- **Standing**: UNVERIFIED (spec written from the task's ground-truth reads; no
  acceptance command has been run at spec time)

## Subject

The end-to-end consumer-loop gap: no document anywhere in the repo walks the full
ABB/SBB consumer loop — author → plan → admit → manufacture → verify → replay —
with commands and outputs. A reader must currently assemble the loop from four
fragmentary sources, none of which connects to the next.

## Ground truth (measured at spec time)

- The RFC (`docs/rfc/v26.9.26/abb-sbb-implementation.md`) is a 45-line DoD — a
  contract, not a walkthrough.
- The crate README (`crates/ggen-abb-sbb/README.md`) is API one-liners — no
  sequence, no outputs, no exits.
- `crates/ggen-abb-sbb/examples/admit_demo.rs` covers synthetic in-memory graphs
  only; it never exercises `parse_graph` from disk, so no runnable artifact
  demonstrates the file→loop path.
- Authoring an EA graph is undocumented: the only way to learn the format is to
  read the serde structs and the fixtures.
- The only real admitted SBB witness is the marketplace PR #506 fixture.

## Deliverable

`docs/abb-sbb-consumer-guide.md` (or a successor section replacing the RFC's
walkthrough slot — implementer's choice, recorded in the receipt). The guide
walks, in order, each section command-and-output complete:

1. **Author the EA graph.** Two admitted routes, both with copy-pasteable minimal
   examples: the Turtle route (per WO-04's schema) and the JSON exemplar route.
   State which route is canonical and when the other applies.
2. **Plan per ABB.** The `SELECT‖MANUFACTURE` decision law verbatim: the
   lowest-id admissible candidate wins; a `MANUFACTURE` selection carries the
   required/missing ports and EVERY per-candidate refusal — no refusal is
   dropped or summarized. Show a real plan output demonstrating both a selection
   and at least one refusal line.
3. **Admit at CONSTRUCT.** `expected_graph_digest` pinning plus `res:Envelope`
   capability caps (`EnvelopeCaps`/`max_artifacts`). Show the admit call and the
   typed refusal shape when the digest or caps mismatch.
4. **Manufacture → receipt.** The receipt schema
   `ggen.abb-sbb.manufacture-receipt.v1` as a field-by-field table, plus the
   provenance header anatomy (what each header field is, where it is minted,
   what it binds).
5. **verify_receipt + replay.** The 9-field comparison, and the
   `REPLAY_MISMATCH{field}` refusal with the offending field named.
6. **envelope_court exit codes.** The full contract: `0` = Pass, `1` =
   VacuousGate, `2` = `CAP_CEILING_MISMATCH`. A court that cannot flip is FAIL —
   say so in the guide, with a runnable vacuity example.
7. **Sync integration.** `ea.graph.json` at the repo root, the Stage 2c gates
   (per WO-01), and the `FM-LAW-015` / `FM-PACK-015` refusal shapes as the
   reader will actually see them.
8. **The full loop, executable.** Run every prior step against
   `fixtures/ea-graph.json` as the executable check: each command shown with its
   real exit code. This section is the guide's own acceptance harness — if the
   loop cannot be reproduced from the guide's own text, the guide is wrong.

## Constraints

- Every figure (digests, counts, outputs) is marked `[TARGET]` until actually
  run on the current tree; no invented outputs anywhere. A `[TARGET]` marker is
  removed only when the command has been executed with the recorded exit code.
- Every claim is command-cited (path + command + observed exit), or explicitly
  `[TARGET]`.
- No new code in `lib/`/`crates/*/src` — documentation work order. Defects
  discovered while running the loop are filed, not patched here (record them in
  the receipt; they route to their own orders).

## Acceptance

Falsifiable, command-cited, all from `/Users/sac/ggen`:

1. **Reproduction check.** A fresh reader following the guide's Section 8
   reproduces the full loop on `fixtures/ea-graph.json` byte-identically:
   every command in Section 8 is executed in sequence during this WO's own
   verification, each with its real exit code recorded in the receipt.
2. **No invented outputs.** `grep -n "TARGET" docs/abb-sbb-consumer-guide.md`
   after final verification lists zero unretired markers (every `[TARGET]`
   either replaced by real output or the section honestly FAIL-marked).
3. **Command-citation coverage.** Every code/output block in the guide is
   preceded by the exact command that produced it; a spot-check re-run of 3
   sampled blocks reproduces the printed output.
4. **Coverage of the 8 sections.** The guide carries all eight sections above;
   `grep -c "^## " docs/abb-sbb-consumer-guide.md` ≥ 9 (Date/subject plumbing
   plus the 8 walkthrough sections).
5. **Milestone README listing.** `grep -n "abb-sbb-consumer-guide"
   docs/jira/v26.11.3/README.md` exits 0 — the guide is listed in the
   milestone README.
6. **Receipt schema fidelity.** The `ggen.abb-sbb.manufacture-receipt.v1`
   field table matches the serde struct in the crate field-for-field (checked
   against source, not from memory), and the 9 replay-compared fields are
   enumerated in the verify_receipt section.
