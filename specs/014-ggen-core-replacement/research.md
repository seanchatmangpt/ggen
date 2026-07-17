# Phase 0 Research: Retire ggen-core in favor of a first-principles engine

All findings below were established through direct code investigation across both
repositories (`/Users/sac/ggen` and `/Users/sac/praxis`) this session — ten parallel research
passes and three independent verification passes — and are fully detailed with file:line
citations in `docs/jira/v26.7.16/00-OVERVIEW.md` through `13-CLAUDE-MD-REFACTOR.md`. This
document consolidates those findings into the Decision/Rationale/Alternatives format for
planning purposes. No `[NEEDS CLARIFICATION]` markers remain in `spec.md`, so no new research
agents were dispatched — this phase reformats already-completed research rather than
re-deriving it.

## Decision: Vendor a physical copy of the replacement engine, don't consume it live

**Rationale**: The source crate (`/Users/sac/praxis/crates/ggen`) declares `name = "ggen"`,
identical to this repository's already-published root package. A live path dependency
cannot be made publish-safe (Cargo packages the real upstream name into the manifest on
`cargo publish`), so the only option that structurally prevents the collision is vendoring a
physical copy under a new package name with `publish = false` set explicitly.

**Alternatives considered**:
- *Git submodule of a renamed fork* — rejected: no submodule tooling precedent in this
  repo, and requires maintaining a rebasing fork indefinitely just for a name change.
- *Workspace-side path dependency with a `package =` rename* — rejected: compiles fine for
  local development but is unsafe the moment anything in its dependency chain is published,
  since the rename doesn't survive `cargo package`.

Full detail: [01-PUBLISH-SAFETY-AND-CRATE-RENAME](../../docs/jira/v26.7.16/01-PUBLISH-SAFETY-AND-CRATE-RENAME.md).

## Decision: Bridge the incoming RDF engine, don't consolidate the four stacks

**Rationale**: This repo already runs three independent oxigraph-based RDF stacks
(`ggen-core::rdf`, `ggen-graph`, `ggen-marketplace`). The replacement engine's law/SHACL/N3
evaluation (`praxis-graphlaw`) is built on a different library family (`oxrdf`/`spargebra`).
Rather than merging all four onto one engine in the same pass as retiring `ggen-core`, the
replacement engine exposes a narrow, engine-neutral `LawEngine` trait (facts and rules
in/out as plain N-Triples strings, no `oxrdf`/`oxigraph` model type crossing the boundary) —
a pattern the source crate itself already implements internally (`GraphLawStore`, bridging
`oxrdf` and `oxigraph` the same way) and has working tests proving it's load-bearing, not
decorative.

**Alternatives considered**:
- *Full consolidation onto one RDF engine* — rejected for this migration's scope: would
  require a parity spike (proving `praxis-graphlaw`'s SPARQL/SHACL/OWL-RL modules are a
  strict superset of `ggen-graph`'s oxigraph wrapper) that has not been performed, and
  multiplies the surface under change at once. Deferred as explicit future work.

Full detail: [03-RDF-ENGINE-BRIDGE-DESIGN](../../docs/jira/v26.7.16/03-RDF-ENGINE-BRIDGE-DESIGN.md).

## Decision: Layer Ed25519 signing onto the BLAKE3 chain, don't rebuild either from scratch

**Rationale**: The replacement engine's receipt chain (via `praxis-core::receipt_record`) is
a rigorously-tested BLAKE3 hash chain with no signature — provable integrity, not provable
authenticity. `ggen-core`'s one genuinely rigorous signed path (`agent/receipt.rs`) has
Ed25519 signing but a simpler, less-tested chain discipline. `praxis-core` already ships the
exact signing primitive needed (`sign_chain_hash_with_key`/`verify_chain_hash_with_key`,
taking a raw hash + hex key — the same format `ggen-core`'s existing keypair
generation/persistence already produces), so the two designs slot together directly: add one
field (`signature_hex`) to the existing `ReceiptRecord`, verify in two explicit steps (chain
integrity, then signature authenticity).

**Alternatives considered**:
- *Port `ggen-core`'s `Receipt`/`sign`/`verify` wholesale* — rejected: duplicates signing
  logic `praxis-core` already owns in a stronger, chain-hash-anchored form, and would create
  a third copy of an already-twice-forked `Receipt` implementation.
- *Adopt `praxis-core`'s fail-closed `PRAXIS_SIGNING_KEY` env-var model outright* — deferred
  as an explicit decision point, not silently chosen: it's a real behavior change from
  today's zero-config file-keypair default, so it needs its own sign-off rather than being
  bundled into this decision.

Full detail: [04-RECEIPT-SIGNING-AND-OTEL](../../docs/jira/v26.7.16/04-RECEIPT-SIGNING-AND-OTEL.md).

## Decision: Fix the OTEL span bug while porting, don't carry it forward

**Rationale**: `ggen-core`'s five pipeline spans never declare `pipeline.duration_ms` in
their `tracing::info_span!` field lists, so every `.record()` call against that field is a
documented `tracing` no-op — confirmed via direct code read, not inference.
`pipeline.files_generated` isn't implemented as a span field anywhere. Both are one-line
fixes (declare the field as `tracing::field::Empty` at span-creation time) that must be
applied during the port, since silently reproducing a known-broken telemetry path in the
replacement would be worse than the status quo (a proof of forward progress is one of this
repo's own gates, per `.claude/rules/otel-validation.md`).

**Alternatives considered**: None — this is a confirmed bug with a confirmed one-line fix;
there is no reasonable alternative to fixing it during the port.

## Decision: Port manifest parsing into `ggen-config`, not into the new engine crate

**Rationale**: `ggen-config` is already the literal home of `Receipt`/`ReceiptChain` and is
what `ggen_core::config_lib` re-exports verbatim today — it's the crate that already owns
"things about `ggen.toml`." `ggen-lsp` already depends on it directly. Landing the ported
`ManifestParser`/`GenerationRule`/etc. there (rather than in the new engine crate, which
`ggen-lsp` would otherwise need a new dependency edge to reach) keeps `ggen-lsp`'s dependency
graph unchanged in shape, just re-pointed to real code instead of a re-export chain through a
retired crate.

**Alternatives considered**:
- *Land it in the new engine crate* — rejected: would require `ggen-lsp` to take on a new,
  heavier dependency (the whole engine) just to parse a manifest file, when `ggen-config` is
  already positioned exactly for this and already a direct `ggen-lsp` dependency.

A related, pre-existing finding surfaced during this research (not introduced by this
migration): `ggen-config`'s own `config_lib::GgenConfig` already defines a second, partially
overlapping schema for the same `ggen.toml` file, silently ignored by the manifest parser via
untyped passthrough fields. This migration's port must reconcile the two schemas, not just
relocate the duplication. Full detail:
[05-MANIFEST-CONFIG-PORT](../../docs/jira/v26.7.16/05-MANIFEST-CONFIG-PORT.md).

## Decision: Merge pack-registry logic into `ggen-marketplace`, don't extract a new crate

**Rationale**: `ggen-marketplace` already has a *more* mature installer (caching, signature
verification, atomic/rollback) than `ggen-core`'s own pack-install code — the direction of
the merge should be ggen-core's breadth (search, capability registry, dependency graph, ~19
files, 5,899 lines excluding a confirmed-dead stub) flowing into ggen-marketplace's already-
better foundation, not the reverse. This also directly follows this repository's own
constitution: the "ggen-domain blocker" precedent it documents (an extraction blocked by a
circular dependency between infrastructure and domain code) is exactly the failure mode
avoided by merging into an already-independent crate instead of minting a new one.

**Alternatives considered**:
- *Extract `domain::packs::*` as its own new crate* — rejected per the constitution's own
  extraction criteria (a new crate needs to be used by ≥2 other crates or be a standalone
  library; this code is consumed by `ggen-cli` alone).

Full detail: [06-MARKETPLACE-PACK-REGISTRY-MERGE](../../docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md).

## Decision: Keep project scaffolding inside `ggen-cli`, don't extract a new crate

**Rationale**: Scaffolding (`cli_generator/` + `project_generator/`, 2,585 lines combined) is
presentation/UX logic invoked only by `ggen init`/`wizard` — a single caller. Per this
repository's constitution's own extraction criteria (≥5k LOC, used by ≥2 crates), this fails
both thresholds; first-principles design means not manufacturing a library abstraction for a
capability with exactly one consumer.

**Alternatives considered**:
- *Extract as a standalone crate for symmetry with the marketplace/manifest ports* —
  rejected: symmetry isn't a real design criterion, and the constitution's own guidance
  argues directly against it here.

Full detail: [07-PROJECT-SCAFFOLDING-PORT](../../docs/jira/v26.7.16/07-PROJECT-SCAFFOLDING-PORT.md).

## Decision: Migrate consumers in dependency order, sync/codegen pipelines last

**Rationale**: `ggen-cli`'s error-plumbing type (`utils::error`) appears in 11 of 31 files and
every domain-handler bucket depends on it compiling first; the sync/codegen pipeline files
(`cmds/sync.rs`, `cmds/inverse_sync.rs`, `cmds/wizard.rs`, `cmds/init.rs`) are the most
cross-cutting in the crate (four separate pipeline mechanisms combined) and transitively
depend on every other bucket already being re-pointed — migrating them first would mean
redoing the work once their dependencies change underneath them. The same reasoning applies
to `ggen-lsp` (manifest parsing must land in `ggen-config` before any `ggen-lsp` file can be
re-pointed) and to the root package + 82 test/example/bench files (batch at the end, since
`utils`/`domain` symbols dominate the reference tally and most files touch several
not-yet-migrated buckets at once).

**Alternatives considered**:
- *Migrate file-by-file in isolation, any order* — rejected: several files import from
  multiple as-yet-unmigrated buckets simultaneously (e.g. `cmds/policy.rs` needs both
  error-plumbing and marketplace), so an unordered approach would require touching the same
  file multiple times as its dependencies land piecemeal.

Full detail: [08-GGEN-CLI-MIGRATION](../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md),
[09-GGEN-LSP-MIGRATION](../../docs/jira/v26.7.16/09-GGEN-LSP-MIGRATION.md),
[10-ROOT-PACKAGE-TEST-MIGRATION](../../docs/jira/v26.7.16/10-ROOT-PACKAGE-TEST-MIGRATION.md).

## Decision: Treat "reachable but uninvoked" as acceptable for the Process Intelligence Boundary, backed by a new CI guard

**Rationale**: The incoming dependency graph makes a forbidden analysis module
(`praxis-graphlaw::chatman`, which imports conformance/fitness/causal-replay machinery this
repository's architecture reserves exclusively for `wasm4pm-compat`) transitively reachable,
but the replacement engine's own code never calls into it (confirmed: all 8 of its
`praxis_graphlaw::` call sites touch only `TripleStore`/`hooks`/`parser`). The boundary holds
today only by absence of a call, not by any structural guarantee — this migration adds an
automated grep-based CI check so a future violation is caught by tooling instead of being
discovered later.

**Alternatives considered**:
- *Vendor only a stripped-down subset of `praxis-graphlaw` excluding `chatman`* — not
  pursued in this pass: higher engineering cost (forking and maintaining a subset) for a
  module that's already provably uninvoked; revisit if the CI guard ever needs to fire.

Full detail: [02-CROSS-REPO-DEPENDENCY-RISKS](../../docs/jira/v26.7.16/02-CROSS-REPO-DEPENDENCY-RISKS.md).

## Decision: Reconcile license inconsistency by dropping the unfulfillable Apache-2.0 clause

**Rationale**: Two of the three newly-adopted crates claim `MIT OR Apache-2.0`, but no
`LICENSE-APACHE` file exists anywhere in the source repository to back that claim — it's
already a compliance gap independent of this migration. Dropping the clause (rather than
drafting new Apache-2.0 text for an option nobody intends to actually offer) also makes the
adopted subtree license-uniform with the already-MIT-only published root package.

**Alternatives considered**:
- *Add the missing `LICENSE-APACHE` text and keep the dual license* — rejected: introduces a
  real legal artifact (with patent-grant implications) for optionality nothing downstream
  will ever exercise.

Full detail: [02-CROSS-REPO-DEPENDENCY-RISKS](../../docs/jira/v26.7.16/02-CROSS-REPO-DEPENDENCY-RISKS.md).

## Decision: Use `just`, not `cargo make`, as the command-runner for all gates in this plan

**Rationale**: This repository's constitution (`.specify/memory/constitution.md`, Principle
V) mandates `cargo make` exclusively, but that is stale relative to the live codebase:
`CLAUDE.md` explicitly forbids it ("NEVER call `cargo make` or bare `cargo` directly"), a
real `justfile` exists and was exercised successfully multiple times this session
(`just check`, `just test`, `just lint`, `just slo-check`), and no `cargo make` target or
`Makefile.toml` entry was found anywhere in this session's work. Every ticket's Definition of
Done already specifies `just` commands.

**Alternatives considered**: None credible — following a stale constitution section over the
actual enforced tooling would produce commands that don't work against this repository as it
exists today.

**Follow-up**: this discrepancy (along with the stale `.specify/specs/` vs. `specs/` path
reference and the stale 6-crate architecture description in the same constitution file) is
tracked as a documentation-sync item, the same class of finding as
[13-CLAUDE-MD-REFACTOR](../../docs/jira/v26.7.16/13-CLAUDE-MD-REFACTOR.md).
