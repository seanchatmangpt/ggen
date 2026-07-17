# Implementation Plan: Retire ggen-core in favor of a first-principles engine

**Branch**: `2026-ggen-core-replacement` | **Date**: 2026-07-16 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/014-ggen-core-replacement/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See
`.specify/templates/plan-template.md` for the execution workflow.

## Summary

Retire `crates/ggen-core` (142,908 lines, actually seven former crates glued together with
three redundant pipeline implementations) and replace it with a vendored, renamed copy of
`~/praxis/crates/ggen` (8,488 lines) plus its `praxis-core`/`praxis-graphlaw` dependencies.
Capabilities present in `ggen-core` but absent from the replacement — pack-registry breadth,
project scaffolding, receipt signing, operational telemetry, and the manifest-parsing
surface `ggen-lsp`'s diagnostics depend on — are re-homed into the sibling crate that
already owns that problem domain (`ggen-marketplace`, `ggen-cli`, the new engine crate, and
`ggen-config` respectively), not rebuilt as a new standalone crate. The technical approach,
every file-level porting decision, and the phased sequencing are already fully scoped across
13 evidence-first tickets in `docs/jira/v26.7.16/`; this plan translates that scope into the
Spec-Kit planning artifacts (this document, `research.md`, `data-model.md`, `contracts/`,
`quickstart.md`) so `/speckit.tasks` can generate an execution-ready task list.

## Technical Context

**Language/Version**: Rust, nightly toolchain pinned via `rust-toolchain.toml`
(`nightly-2026-06-22`) — required transitively by `wasm4pm-compat` (`ggen-lsp` dependency)
and `cpmp`'s `rusqlite` "bundled" feature; the replacement engine's own code does not itself
require nightly features (confirmed no `#![feature(...)]` in
`/Users/sac/praxis/crates/{ggen,praxis-core,praxis-graphlaw}`).
**Primary Dependencies**: `oxigraph` (existing three RDF stacks: `ggen-core::rdf`,
`ggen-graph`, `ggen-marketplace`), `oxrdf`/`spargebra` (incoming via `praxis-graphlaw`,
bridged not merged — see [03-RDF-ENGINE-BRIDGE-DESIGN](../../docs/jira/v26.7.16/03-RDF-ENGINE-BRIDGE-DESIGN.md)),
`tera` (templating), `clap`/`clap-noun-verb` (CLI), `blake3`/`ed25519-dalek` (receipt
chain + signing).
**Storage**: Local filesystem only — `.ggen/receipts/`, `.ggen/keys/`, `.ggen/packs.lock`;
no database.
**Testing**: `cargo test` via `just test`, Chicago TDD (real collaborators, no mocks) per
this repo's constitution Principle IV and `.claude/rules/rust/testing.md`.
**Target Platform**: Cross-platform CLI + LSP server binary (macOS confirmed this session;
Linux/Windows not independently re-verified in this pass).
**Project Type**: Multi-crate Rust workspace (library + CLI + LSP server), not a single
project/library/web-service — see Project Structure below.
**Performance Goals**: Existing SLOs unchanged by this migration —
first build ≤15s, incremental build ≤2s (per CLAUDE.md); this migration additionally
requires OTEL per-stage span capture to actually work (a confirmed-broken no-op today, fixed
per [04-RECEIPT-SIGNING-AND-OTEL](../../docs/jira/v26.7.16/04-RECEIPT-SIGNING-AND-OTEL.md)).
**Constraints**: The published `ggen` crate on crates.io (root package,
`/Users/sac/ggen/Cargo.toml:9`) must never be put at risk of a publish collision — see
[01-PUBLISH-SAFETY-AND-CRATE-RENAME](../../docs/jira/v26.7.16/01-PUBLISH-SAFETY-AND-CRATE-RENAME.md).
No `ggen-core` compatibility shim may remain post-migration (Legacy Path Contamination, per
`.claude/rules/coding-agent-mistakes.md`).
**Scale/Scope**: 164 `ggen_core::` call sites across 31 files in `ggen-cli`, 17 call sites
across 6 files in `ggen-lsp`, 82 files in the root package's `tests/examples/benches` — full
enumeration in [08](../../docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md),
[09](../../docs/jira/v26.7.16/09-GGEN-LSP-MIGRATION.md),
[10](../../docs/jira/v26.7.16/10-ROOT-PACKAGE-TEST-MIGRATION.md).

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|---|---|---|
| I. Crate-First Architecture | **PASS** | New engine crate is self-contained; dependency-direction sanity check already performed ([06-MARKETPLACE-PACK-REGISTRY-MERGE](../../docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md)) confirms no circular dependency is introduced — `ggen-config`/`ggen-graph` remain leaves, `ggen-marketplace → ggen-config` only, the retiring `ggen-core`'s edge to `ggen-marketplace` is removed, a net reduction in coupling. |
| II. Infrastructure-Domain Separation | **PASS, and directly informed by this principle's own documented precedent.** This constitution's "Historical Examples" section records the exact failure mode this migration avoids: the `ggen-domain` extraction was blocked because infrastructure (`pack_resolver.rs`) depended on domain code (`packs/`), creating a cycle. This migration does not attempt to extract `domain::packs::*` as a new standalone crate — it merges it into the already-independent `ggen-marketplace` (which has zero dependency on the engine crate), the same pattern the constitution itself prescribes as the fix ("if infrastructure depends on domain, accept the module staying" — here, accept it moving into the crate that already safely owns that domain). |
| III. Deterministic RDF Projections | **PASS** | The replacement engine's `determinism: true` self-check (re-renders and diffs) and BLAKE3-chained receipts directly serve this; [04-RECEIPT-SIGNING-AND-OTEL](../../docs/jira/v26.7.16/04-RECEIPT-SIGNING-AND-OTEL.md) adds a signature layer without touching the deterministic-hash guarantee. |
| IV. Chicago TDD (Zero Tolerance) | **PASS** | The replacement engine was independently confirmed genuinely mock-free this session (grepped clean for `mockall`/`struct Mock`/behavior-verification patterns). |
| V. `cargo make` Protocol | **CONFLICT — constitution is stale, not the plan.** The constitution mandates `cargo make <target>` exclusively. This repo's actual tooling — `CLAUDE.md` ("ALWAYS `just <task>`. NEVER call `cargo make` or bare `cargo` directly"), the real `justfile` (`just check`, `just test`, `just lint`, `just slo-check`, all used and verified working this session), and every Definition-of-Done in the 13-ticket set — uses `just`, not `cargo make`. No `cargo make` targets were found or exercised anywhere in this session's extensive verification work. Resolution: follow `just`, the operationally correct and currently-enforced tool; flag the constitution itself as needing a sync pass (tracked as follow-up scope alongside [13-CLAUDE-MD-REFACTOR](../../docs/jira/v26.7.16/13-CLAUDE-MD-REFACTOR.md), since both documents were found stale relative to the live codebase in the same research effort). |
| VI. Type-First Thinking | **PASS** | No change in posture; both the retiring and replacement code are idiomatic Rust. |
| VII. Andon Signal Protocol | **PASS** | Directly mirrored by `.claude/rules/andon/signals.md`, already the basis for [11-DELETION-AND-DEFINITION-OF-DONE](../../docs/jira/v26.7.16/11-DELETION-AND-DEFINITION-OF-DONE.md)'s gate list. |
| VIII. Error Handling Standards | **NOT INDEPENDENTLY VERIFIED** | Whether the replacement engine crate is `unwrap()`/`expect()`-free in production code was not checked in this session's research; flag as a task-level check during implementation, not a plan-blocking gate. |
| IX. Concurrent Execution / Claude-Flow hooks | **N/A** | No `claude-flow` tooling was encountered anywhere in this session; this section of the constitution appears to describe tooling not currently present in the repository. Not a gate for this plan. |
| X. Lean Six Sigma Quality | **PASS (pending verification)** | `just lint` (clippy) is already part of every ticket's Definition of Done; `#![deny(warnings)]` posture not independently re-verified this session for the replacement engine specifically. |
| Crate Extraction & Consolidation Patterns | **PASS, and directly validates two plan decisions.** The stated extraction criteria (≥5k LOC, stable API, zero infra deps, used by ≥2 crates, no circular deps) argue *against* extracting project scaffolding (2,585 lines, used by exactly 1 crate — `ggen-cli`) as a new standalone crate — which is exactly [07-PROJECT-SCAFFOLDING-PORT](../../docs/jira/v26.7.16/07-PROJECT-SCAFFOLDING-PORT.md)'s decision (keep it `ggen-cli`-internal). The "micro-crate <2k LOC → consolidate" guidance likewise supports merging `domain::packs::*` (5,899 lines, tightly coupled to the pack-registry domain) into `ggen-marketplace` rather than standing up a new crate. |

**Documentation-path discrepancy noted, not a gate**: the constitution states spec artifacts
live under `.specify/specs/NNN-feature/`; the actual `setup-plan.sh`/`create-new-feature.sh`
tooling exercised this session creates them under `specs/NNN-feature/` (this very directory).
Another stale-documentation finding for the same follow-up as the `cargo make` conflict
above.

**Net gate result: PASS.** One real conflict found (`cargo make` vs. `just`) is a
documentation-staleness issue in the constitution itself, not a defect in this plan — `just`
is what's actually used, tested, and required by every ticket's Definition of Done, so this
plan proceeds on that basis rather than blocking on a stale gate.

**Post-Phase-1 re-check**: `data-model.md`, `contracts/`, and `quickstart.md` were reviewed
against the same principle table above. The one new interface introduced by the design
(`LawEngine` trait, `contracts/law-engine-trait.md`) reinforces Principle I (no circular
dependency — it's explicitly a one-directional, string-only seam) and Principle II
(infrastructure/domain separation — it keeps RDF-engine choice an internal detail of
whichever crate owns it). No new violations introduced; gate result unchanged: **PASS**.

## Project Structure

### Documentation (this feature)

```text
specs/014-ggen-core-replacement/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md         # Phase 1 output
├── quickstart.md         # Phase 1 output
├── contracts/            # Phase 1 output
└── tasks.md              # Phase 2 output (/speckit.tasks — not created by /speckit.plan)
```

### Source Code (repository root)

This is an existing multi-crate Rust workspace, not a new project — no new top-level
directories are created; existing crate directories are modified, one new crate directory is
added (the vendored, renamed engine), and one crate directory is deleted at the end.

```text
/Users/sac/ggen/
├── Cargo.toml                          # [workspace] members: drop ggen-core, add new engine crate
├── crates/
│   ├── ggen-core/                      # DELETED at end of migration (Phase 6)
│   ├── ggen-engine/                    # NEW — vendored + renamed from /Users/sac/praxis/crates/ggen
│   │                                   #   (final name decided in ticket 01; publish = false)
│   ├── ggen-cli/src/                   # 31 files re-pointed away from ggen_core:: (ticket 08)
│   ├── ggen-lsp/src/                   # 6 files re-pointed away from ggen_core:: (ticket 09)
│   ├── ggen-config/src/manifest/       # NEW module — ported manifest parsing (ticket 05)
│   ├── ggen-marketplace/src/           # gains packs_registry/, packs/lockfile.rs (ticket 06)
│   └── ggen-graph/                     # unchanged — bridged via LawEngine trait (ticket 03)
├── src/lib.rs                          # `pub use ggen_core as core;` re-pointed (ticket 10)
├── tests/ examples/ benches/           # 82 files re-pointed away from ggen_core:: (ticket 10)
└── scripts/ci/
    ├── guard-publish-target.sh         # NEW (ticket 01)
    └── guard-process-intelligence-boundary.sh  # NEW (ticket 02)
```

**Structure Decision**: Existing 10-crate workspace structure is preserved; this migration
swaps one crate (`ggen-core`) for one new crate (the vendored engine) plus targeted
extensions to three existing sibling crates (`ggen-config`, `ggen-marketplace`, `ggen-cli`).
No new architectural layer, no new top-level directory, no change to how the workspace is
organized — matching Constitution Principle I's "consolidation-ready design" guidance and
avoiding the "ggen-domain blocker" anti-pattern by extending existing crates rather than
minting new ones except where the source material (the vendored engine) genuinely has no
existing home.

## Complexity Tracking

> Fill ONLY if Constitution Check has violations that must be justified

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| New crate (`ggen-engine`, vendored copy rather than a live path dependency) | The source crate (`/Users/sac/praxis/crates/ggen`) shares its package name (`ggen`) with this repo's already-published root package; a live path dependency would either fail to publish or risk corrupting the published crate's version history on an accidental `cargo publish` (see [01-PUBLISH-SAFETY-AND-CRATE-RENAME](../../docs/jira/v26.7.16/01-PUBLISH-SAFETY-AND-CRATE-RENAME.md)) | A live cross-repo path dependency was rejected: Cargo packages the *real* upstream name into the manifest on `cargo publish`, so it cannot be made publish-safe without vendoring; a git submodule was also rejected since this repo has no submodule tooling precedent and it would require maintaining a rebasing fork just for a package rename. |
| Bridging four RDF/SPARQL stacks instead of consolidating to one | `praxis-graphlaw` uses a different library family (`oxrdf`/`spargebra`) than the three existing stacks (`oxigraph`); consolidating all four in the same pass as retiring `ggen-core` multiplies the surface under change at once | Full consolidation was rejected for *this* migration (deferred to [12-OPEN-QUESTIONS](../../docs/jira/v26.7.16/12-OPEN-QUESTIONS.md) item 1) because it violates this repo's fix-forward, one-authoritative-path-at-a-time posture — a parity spike would be needed first to prove zero functional gaps, which has not been performed. |
