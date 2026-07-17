# CLAUDE.md Refactor (First Principles)

Part of [00-OVERVIEW](00-OVERVIEW.md) — not phase-ordered like 00-12; this is a scoping
ticket for a separate, standing document (`/Users/sac/ggen/CLAUDE.md`, loaded into every
future session's context) rather than a step in the ggen-core migration itself. It is listed
here because most of its findings were surfaced *by* the migration research in tickets
00-12, and several of its claims will go further out of date once that migration executes.

**This ticket is scoping only — no edits to `/Users/sac/ggen/CLAUDE.md` have been made.**
Given the file is durable, project-wide instructions read by every future session, an actual
rewrite deserves explicit review against this scope before being applied, not a silent edit
bundled into the migration tickets.

## File reference table

| Path | LOC |
|---|---:|
| `/Users/sac/ggen/CLAUDE.md` | to be measured at rewrite time |
| `/Users/sac/ggen/.claude/rules/architecture.md` | crate map, likely needs the same treatment |
| `/Users/sac/ggen/.claude/rules/otel-validation.md` | OTEL span/attribute claims, cross-check against [04](04-RECEIPT-SIGNING-AND-OTEL.md)'s findings |
| `/Users/sac/ggen/.claude/rules/andon/signals.md` | Definition of Done gates, referenced accurately by [11](11-DELETION-AND-DEFINITION-OF-DONE.md) — no known issue |
| `/Users/sac/ggen/.claude/rules/coding-agent-mistakes.md` | 6-Question Patch Contract, referenced accurately by [11](11-DELETION-AND-DEFINITION-OF-DONE.md) — no known issue |

## Confirmed inaccuracies in CLAUDE.md today (independent of the migration)

These were found as a byproduct of the ggen-core replacement research (tickets 00-12) and
are true **right now**, before any migration work happens — CLAUDE.md is describing the
current `ggen-core` incorrectly, not just going stale in the future.

1. **Diagnostic Codes table conflates two crates.** CLAUDE.md's table presents
   `GGEN-TPL-001`, `GGEN-OUT-001`, `GGEN-YIELD-001`, `GGEN-RULE-001`, `GGEN-QUERY-002`,
   `E0011`, `E0013`, `E0015` as one flat surface without noting that 5 of 7 (`GGEN-*`) live
   entirely in `/Users/sac/ggen/crates/ggen-lsp/src/route/diagnostic_species.rs` (299
   lines), not in `ggen-core` at all — see
   [05-MANIFEST-CONFIG-PORT](05-MANIFEST-CONFIG-PORT.md). Only `E0011`/`E0013` are both
   claimed-and-confirmed live in `ggen-core`
   (`/Users/sac/ggen/crates/ggen-core/src/manifest/validation.rs:243,250`).
2. **`E0015` is documented as a live warning; it is reserved/inactive.**
   `ggen-core/tests/error_code_documentation_test.rs:45` lists `E0015` in
   `RESERVED_ERROR_CODES` explicitly as "should not appear in error messages, only in
   documentation." CLAUDE.md's table describes it as an active "WARNING: Identity CONSTRUCT
   detected" — directly contradicted by ggen-core's own test.
3. **Cryptographic Receipts section describes a mechanism that doesn't match the live code
   path.** CLAUDE.md: "Every `ggen sync` produces a signed BLAKE3 transition receipt at
   `.ggen/receipts/latest.json`," verified via `--public-key .ggen/keys/public.pem`. The
   actual live path (`/Users/sac/ggen/crates/ggen-cli/src/cmds/sync.rs`'s
   `emit_sync_receipt`) uses **SHA-256**, not BLAKE3, for content hashing, and writes the
   verifying key to `.ggen/keys/verifying.key`, not `.ggen/keys/public.pem` — confirmed by
   `/Users/sac/ggen/crates/ggen-cli/README.md:456` itself acknowledging the mismatch. Full
   detail in [04-RECEIPT-SIGNING-AND-OTEL](04-RECEIPT-SIGNING-AND-OTEL.md)'s file reference
   table.
4. **OTEL Validation checklist implies the pipeline spans currently work; one is silently
   broken.** `/Users/sac/ggen/crates/ggen-core/src/pipeline_engine/pipeline.rs`'s five
   `pipeline.*` spans never declare `pipeline.duration_ms` in their `tracing::info_span!`
   field lists, so every `.record("pipeline.duration_ms", ...)` call is a documented
   `tracing` no-op; `pipeline.files_generated` isn't implemented as a span field anywhere.
   Full detail and fix in [04-RECEIPT-SIGNING-AND-OTEL](04-RECEIPT-SIGNING-AND-OTEL.md).
5. **`AUDIT_DASHBOARD.md` (dated 2026-04-01), which CLAUDE.md points to for crate health, is
   itself measurably stale** — 2 of its 3 headline SHACL P0 claims no longer match the code
   (real SPARQL-based implementations now exist in
   `/Users/sac/ggen/crates/ggen-core/src/validation/{shacl,validator}.rs`); `v26.5.19/` and
   `config/hive_coordinator.rs`, both cited as current, have been deleted/renamed since. See
   [00-OVERVIEW](00-OVERVIEW.md)'s scale-reality-check section.
6. **`just slo-check`'s claimed guarantee doesn't match what the test actually checks.** The
   justfile comment above the `slo-check` recipe claims a `<5s` timing guarantee that
   `inverse_receipt_chain_test.rs` doesn't actually assert (zero
   `Duration`/`Instant`/`elapsed` matches in the file). This is a Decorative Completion
   already present today, not introduced by the migration — see
   [11-DELETION-AND-DEFINITION-OF-DONE](11-DELETION-AND-DEFINITION-OF-DONE.md).

## Claims that will go stale once the migration (tickets 00-12) executes

These are accurate today but will need updating once `ggen-core` is actually deleted —
listed here so the CLAUDE.md refactor can be sequenced correctly (either done in two passes:
fix-what's-wrong-now, then update-for-the-new-engine; or held until after
[11-DELETION-AND-DEFINITION-OF-DONE](11-DELETION-AND-DEFINITION-OF-DONE.md) lands).

- The 10-crate map (`.claude/rules/architecture.md` and CLAUDE.md's own crate table) loses
  `ggen-core` and gains whatever the new engine crate is named
  (see [01-PUBLISH-SAFETY-AND-CRATE-RENAME](01-PUBLISH-SAFETY-AND-CRATE-RENAME.md) for the
  naming decision).
- The "Process Intelligence Boundary" table's enforcement becomes real (a CI guard, not just
  a documented convention) once
  [02-CROSS-REPO-DEPENDENCY-RISKS](02-CROSS-REPO-DEPENDENCY-RISKS.md) item 2's guard script
  is wired in — CLAUDE.md should reference `scripts/ci/guard-process-intelligence-boundary.sh`
  once it exists, not just assert the rule in prose.
- The Cryptographic Receipts section should describe the corrected design from
  [04-RECEIPT-SIGNING-AND-OTEL](04-RECEIPT-SIGNING-AND-OTEL.md) (BLAKE3 chain +
  layered Ed25519 signature) once that lands, replacing the current inaccurate description
  rather than just fixing today's inaccuracy in place.
- `just slo-check`'s description should reference whatever it's retargeted to per
  [11-DELETION-AND-DEFINITION-OF-DONE](11-DELETION-AND-DEFINITION-OF-DONE.md), not the
  `ggen-core`-specific test binary names.

## Proposed first-principles approach

Rather than patching each inaccuracy above in place (which just produces the next round of
silent drift), restructure CLAUDE.md's factual claims into two categories:

1. **Cheap-to-keep-true inline facts** — things unlikely to change often (the Chicago TDD
   policy, the `just <task>` entry-point rule, the fix-forward git policy). Keep these
   as-is, in prose.
2. **Living-code claims** — anything describing a specific mechanism's current behavior
   (receipt hashing algorithm, diagnostic code ownership, OTEL span names, crate
   dependency graph) should cite the authoritative source file/module rather than assert the
   behavior inline, the same evidence-first discipline this ticket set itself follows. A
   reader (human or agent) who needs the exact current behavior follows the pointer instead
   of trusting a prose description that can drift the moment the code changes underneath it.

This mirrors the "Evidence-First Principle" CLAUDE.md already states for docs/examples/OTEL
traces — the file should hold itself to its own rule.

## Definition of done for this ticket

- This scoping document reviewed and confirmed against `/Users/sac/ggen/CLAUDE.md`'s current
  content by a human or a dedicated read-through (not assumed accurate from this session's
  research alone).
- Explicit decision made: fix the 6 confirmed-today inaccuracies immediately (independent of
  the migration), or bundle them with the migration-dependent updates into one pass after
  [11-DELETION-AND-DEFINITION-OF-DONE](11-DELETION-AND-DEFINITION-OF-DONE.md) lands.
- If proceeding: a follow-up plan-mode session scopes the actual `CLAUDE.md` edit, since
  editing durable, project-wide instructions is a higher-blast-radius action than editing a
  jira ticket and deserves its own explicit review pass rather than being folded silently
  into this batch.
