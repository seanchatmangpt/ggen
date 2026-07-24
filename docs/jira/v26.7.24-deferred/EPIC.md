# EPIC: InterviewAssist v26.7.24-deferred (Deferred/Follow-On Backlog)

## Status

PLANNED

## Predecessor

`docs/jira/v26.7.23/EPIC.md` — the 57-ticket InterviewAssist manufacturing epic. This epic does
**not** supersede, retroactively judge, or block that epic. v26.7.23's own TICKET-057 (Final
Verifier Report) makes its own independent ALIVE/PARTIAL_ALIVE/BLOCKED determination; nothing in
this document changes that determination or is a precondition for it.

## Product objective

Catalog, as a receipted backlog rather than prose commentary, everything the v26.7.23 epic will
**not** finish — three distinct and non-overlapping categories, each grounded in a separate
read-only research pass over `docs/jira/v26.7.23/TICKET-*.md`, `packs/wasm4pm-interview-assist-pack/`,
and `examples/interview-assist/`:

1. **Disclosed structural/environment gaps** (workstream K, TICKET-D001..D014) — limitations the
   57-ticket epic's own ticket files already state in their own words, either as permanent
   architectural boundaries (an engine capability that does not exist, a domain-data coverage
   decision) or as environment-dependent blocks (no dev server exists yet to drive a browser
   harness against) that may resolve once the concurrently-running implementation workflow lands
   more of workstreams C/D/H.
2. **Production-readiness work never scoped by the 57-ticket epic** (workstream L,
   TICKET-D015..D025) — real hardening (container isolation, auth, rate limiting, CI/CD,
   deployment, observability, i18n, load testing) that a demo-scale manufacturing epic legitimately
   did not attempt, confirmed absent by direct grep/read against the corpus, not inferred.
3. **Verification that is real but narrower than the full risk surface** (workstream M,
   TICKET-D026..D031) — passing tests exist for a specific instance of a claim (one tamper case,
   one live Ollama call, SSR-string component checks) while the ticket's own prose or the
   epic's architecture decisions imply a broader property (fuzzing, statistical repetition,
   interaction testing, N-run/concurrent idempotency) that no artifact currently backs.

## Honest framing — this is not "the epic failed"

Every real production epic accumulates a deferred backlog; the absence of one is a red flag, not
a sign of completeness. This document exists because the v26.7.23 backlog's own ticket authors
were honest in their "Implementation notes," "Disclosed, unfixed risk," and "N/A with reason"
fields — the raw material for this epic is *disclosure the epic already made about itself*, not
a gap this epic discovered by catching v26.7.23 in an omission. Three explicit non-claims:

- This epic does **not** claim v26.7.23 should have scoped items in workstream L (TICKET-D015..D025)
  — a demo/proof-of-concept manufacturing epic reasonably excludes production hardening, and
  v26.7.23's own EPIC.md never claims to be production-ready.
- This epic does **not** claim workstream K's items (TICKET-D001..D014) are defects — several are
  genuine engine-capability limits (TICKET-012's one-file-per-plane gap) or timing artifacts of a
  concurrently-running workflow (TICKET-010's per-template idempotency re-verification schedule)
  that resolve on their own as that workflow proceeds.
- This epic does **not** claim workstream M's items (TICKET-D026..D031) mean the underlying tests
  are wrong — the Thin Verification report's own framing is precise: these tests are real and
  pass, they just cover one concrete instance of a broader claim, which is a normal, disclosed
  verification-maturity gap, not a fabrication.

## Source reports

Three independent, read-only research passes, each cited verbatim in this backlog's `## Source`
fields:

1. **Disclosed Gaps Catalog** — survey of `docs/jira/v26.7.23/TICKET-*.md`'s own "Current state,"
   "Implementation notes," and Status-header text for self-disclosed limitations, split into (a)
   permanent structural and (b) environment-dependent.
2. **Out-of-Scope Production Work Report** — full read of `EPIC.md`/`README.md`, cross-checked
   against all 57 tickets, the admitted ontology, `.github/workflows/`, and
   `examples/interview-assist/` source, using `grep -rniE` construction (not assumption) to
   confirm 11 production-readiness topics are ABSENT or SHALLOW.
3. **Thin Verification Report** — survey of `examples/interview-assist/tests/` and the 57 ticket
   files' verification-ladder rows, distinguishing real-but-narrow coverage from prose promises
   with no backing artifact at all.

## Numbering

This backlog uses the `TICKET-D0NN` prefix (`D` for "deferred") specifically to avoid colliding
with v26.7.23's `TICKET-0NN` (001-057) numbering. `TICKET-D0NN` numbers are sequential across all
three workstreams (K, L, M) in the order they appear in this document, not per-workstream.

## Major workstreams

| Workstream | Tickets | Source report | Purpose |
|---|---|---|---|
| K. Disclosed structural/environment gaps | D001-D014 | Disclosed Gaps Catalog | Self-disclosed limitations already in v26.7.23's own ticket text |
| L. Production-readiness work never scoped | D015-D025 | Out-of-Scope Production Work Report | Hardening topics confirmed absent from the 57-ticket epic |
| M. Narrower-than-target verification | D026-D031 | Thin Verification Report | Real, passing tests whose scope is narrower than the property their ticket's prose implies |

## Priority legend

- **P0** — blocks any real production use; specifically, anything that would let an untrusted or
  unauthenticated actor execute code, exhaust resources, or bypass a stated policy against a
  system exposed beyond a single trusted local operator.
- **P1** — real hardening or verification-completeness work needed before scaling past a
  single-operator demo (more users, a hosted deployment, a CI pipeline, external accessibility
  audiences).
- **P2** — nice-to-have completeness; does not block a demo or a single-operator deployment, and
  in several cases (TICKET-D012, D013) is expected to self-resolve as the concurrently-running
  v26.7.23 implementation workflow lands more tickets.

Every ticket states its priority and the concrete scenario that priority level protects against —
never an unqualified adjective.

## What this epic does not do

- Does not write, edit, or delete anything under `packs/wasm4pm-interview-assist-pack/`,
  `examples/interview-assist/`, or `docs/jira/v26.7.23/` — this backlog is planning artifacts only,
  written exclusively under `docs/jira/v26.7.24-deferred/`.
- Does not re-run or re-verify the v26.7.23 tickets' own evidence; every citation in this backlog's
  `## Source` fields quotes the three research reports (which themselves cite file:line against the
  live corpus at their own read time), not a fresh independent re-verification by this epic.
- Does not assign implementation owners or a schedule — this is a catalog, not a sprint plan.
- Does not mark any workstream-K item as blocking v26.7.23's TICKET-057. That determination is
  TICKET-057's alone.

## Completion state criteria

There is no single decisive-acceptance test for this backlog, unlike v26.7.23's TICKET-053 — each
`TICKET-D0NN` closes independently when its own `## Definition of done` is met. This epic itself
is "complete" in the sense that its cataloging work is finished once all `TICKET-D0NN` files exist
with grounded `## Source` citations, which is the state as of this document's authoring.
