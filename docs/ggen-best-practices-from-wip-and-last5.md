# ggen Best Practices from WIP and Last 5 Commits

## Scope

This guide is grounded in:
- Current WIP in:
  - `crates/ggen-a2a-mcp/src/ggen_server.rs`
  - `crates/ggen-a2a-mcp/tests/validation_e2e.rs`
  - `crates/ggen-core/src/codegen/pipeline.rs`
  - `crates/ggen-core/src/graph/cycle_fixer.rs`
  - `crates/ggen-core/src/poka_yoke/quality_gates.rs`
- Last 5 commits on `master`:
  - `dfb62563` fix(clippy)
  - `11d8df17` docs(generation): generated/ elimination record
  - `811a65db` refactor(generation): `output_dir="."` migration
  - `3b34648b` feat(generation): migration script for generated folders
  - `7da61829` merge baseline for the large snapshot PR

Each best practice includes an evidence marker:
- `Evidence(WIP:...)`
- `Evidence(Commit:...)`

---

## 1) Non-Negotiable Workflow Standards

### Rule 1.1: Use `cargo make` tasks only for build/test/lint workflows
- Use `cargo make check`, `cargo make test`, `cargo make lint`, `cargo make slo-check`.
- Do not run direct `cargo check/test/clippy/fmt` as default workflow commands.
- Why: keeps timeout wrappers, consistent workspace behavior, and reproducible CI parity.
- Evidence(Commit:`dfb62563`): clippy-focused fix commit highlights lint gate importance.

### Rule 1.2: Treat compiler/lint/test failures as stop-the-line events
- If any gate fails, stop feature work and fix root cause immediately.
- Never stack unrelated changes on top of red signals.
- Why: reduces defect propagation and rework cost.
- Evidence(WIP:`crates/ggen-core/src/poka_yoke/quality_gates.rs`): explicit Andon-style gate enforcement.

### Rule 1.3: Keep validation observable, not implicit
- Log tool execution and error categories with structured telemetry.
- Include operation names and key attributes in spans.
- Why: fast diagnosis for MCP and pipeline failures.
- Evidence(WIP:`crates/ggen-a2a-mcp/src/ggen_server.rs`): tool spans and typed error paths.

---

## 2) Generation and Output Layout Best Practices

### Rule 2.1: Standardize generation targets on `output_dir="."` unless a strong isolation reason exists
- Keep generated artifacts co-located with project structure where practical.
- Avoid drifting between mixed output conventions in examples.
- Why: lower path complexity and cleaner project ergonomics.
- Evidence(Commit:`811a65db`): repo-wide migration of example manifests.

### Rule 2.2: Eliminate `generated/` folder drift with scripted migration, not manual edits
- Use scripted migration for broad config rewrites.
- Verify post-migration consistency across examples and docs.
- Why: prevents partial migrations and one-off regressions.
- Evidence(Commit:`3b34648b`): dedicated migration script introduction.
- Evidence(Commit:`11d8df17`): docs tracking generated-folder elimination.

### Rule 2.3: Validate generated output before writing
- Enforce:
  - non-empty content,
  - size limits,
  - no traversal paths.
- Why: prevents corrupt, unsafe, or runaway outputs.
- Evidence(WIP:`crates/ggen-core/src/codegen/pipeline.rs`): output validation checks before file writes.

---

## 3) MCP/A2A Tool and Server Design Practices

### Rule 3.1: Define strict typed schemas for every tool parameter set
- Use explicit parameter structs with serde + schema derivations.
- Keep inputs minimal and defaults predictable.
- Why: safer tool contracts and better client compatibility.
- Evidence(WIP:`crates/ggen-a2a-mcp/src/ggen_server.rs`): typed params for generate/sync/query/validate/fix tools.

### Rule 3.2: Make tool outputs deterministic and machine-readable
- Return stable JSON structures for list/search/query-style tools.
- Clamp limits and normalize field naming.
- Why: improves automation reliability and diffability.
- Evidence(WIP:`crates/ggen-a2a-mcp/src/ggen_server.rs`): limit clamping, JSON responses, stable resource listing.

### Rule 3.3: Offload blocking workloads from async handlers
- Use `spawn_blocking` around heavy parsing/execution paths.
- Keep handlers responsive and failure-isolated.
- Why: prevents runtime starvation.
- Evidence(WIP:`crates/ggen-a2a-mcp/src/ggen_server.rs`): `spawn_blocking` for sync/manifest and gate execution paths.

### Rule 3.4: Keep example scaffolding safe by excluding build/cache directories
- Exclude `target`, `node_modules`, `.git`, and cache dirs on recursive copy.
- Why: avoids bloat and accidental leakage of local artifacts.
- Evidence(WIP:`crates/ggen-a2a-mcp/src/ggen_server.rs`): explicit exclusion list in copy helpers.

---

## 4) Quality Gates and Validation Flow

### Rule 4.1: Run gate sequence before generation, not after
- Validate manifest schema, ontology dependencies, SPARQL, templates, permissions, and rule references first.
- Why: fail fast before expensive generation work.
- Evidence(WIP:`crates/ggen-core/src/poka_yoke/quality_gates.rs`): ordered gate runner and hard fail behavior.

### Rule 4.2: Keep gate failure feedback actionable
- Return:
  - failing gate name,
  - specific context,
  - recovery steps and docs link.
- Why: shortens fix loop and improves onboarding.
- Evidence(WIP:`crates/ggen-core/src/poka_yoke/quality_gates.rs`): recovery suggestions and links.

### Rule 4.3: Cycle fixing must support preview and rollback posture
- Provide `dry_run` first, then apply strategy with backup creation.
- Why: avoids destructive ontology edits.
- Evidence(WIP:`crates/ggen-core/src/graph/cycle_fixer.rs`): dry-run report mode and backup creation path.

---

## 5) Testing Strategy (Chicago TDD + OTEL)

### Rule 5.1: Prefer integration tests with real collaborators for MCP tools
- Use real parser/runtime/file I/O and full tool calls.
- Avoid mock-only confidence for protocol or syntax validation paths.
- Why: integration behavior is the product surface.
- Evidence(WIP:`crates/ggen-a2a-mcp/tests/validation_e2e.rs`): real TempDir/files/tool invocations across validation paths.

### Rule 5.2: Validate behavior/state, not just “ok”
- Assert concrete fields, error states, and outputs.
- Include edge and performance assertions where relevant.
- Why: catches silent semantic regressions.
- Evidence(WIP:`crates/ggen-a2a-mcp/tests/validation_e2e.rs`): explicit assertions on failure modes and timing.

### Rule 5.3: OTEL verification is mandatory for external-service features
- Verify span presence and required attributes.
- Keep a repeatable check script and report.
- Why: proves real execution path vs synthetic pass.
- Evidence(Commit:`dfb62563`): OTEL span verification tests/report artifacts introduced with lint fixes.

---

## 6) Pipeline and Core Design Practices

### Rule 6.1: Keep generation pipeline transactional and auditable
- Stage writes in transaction, commit only when all rules pass.
- Track executed rules, generated files, and validation results.
- Why: preserves consistency and supports audit/debug workflows.
- Evidence(WIP:`crates/ggen-core/src/codegen/pipeline.rs`): file transaction usage and execution state model.

### Rule 6.2: Support conditional generation safely
- Gate rules with explicit `when` checks and skip semantics.
- Why: avoids side effects from inapplicable rules.
- Evidence(WIP:`crates/ggen-core/src/codegen/pipeline.rs`): ASK-based condition evaluation.

### Rule 6.3: Keep dependency boundaries explicit for LLM integration
- Inject LLM service via trait boundary instead of cross-crate cyclic deps.
- Provide deterministic fallback behavior when LLM is unavailable.
- Why: architecture stability and graceful degradation.
- Evidence(WIP:`crates/ggen-core/src/codegen/pipeline.rs`): `LlmService` injection + default TODO fallback.

---

## 7) Commit/PR Hygiene for Large WIP

### Rule 7.1: Use snapshot branch strategy for high-volume WIP
- Create dedicated branch from current tip, commit full snapshot, push, then iterate with focused follow-up commits.
- Why: prevents loss and keeps reviewable checkpoints.
- Evidence(Commit:`7da61829`): large merge baseline from snapshot branch workflow.

### Rule 7.2: Separate migration/refactor/docs concerns when possible
- Prefer sequence:
  1. automation script,
  2. config/data migration,
  3. docs update.
- Why: easier reverts and clearer reviewer mental model.
- Evidence(Commits:`3b34648b` -> `811a65db` -> `11d8df17`): this exact pattern.

### Rule 7.3: Keep lint debt out of feature commits
- Run lint fixes as explicit, scoped commits.
- Why: improves bisectability and blame clarity.
- Evidence(Commit:`dfb62563`): targeted clippy fix commit.

---

## 8) Anti-Patterns Seen in Current WIP

### Anti-pattern A: Backup/patch artifacts in repo working set
- Examples currently present in WIP:
  - `crates/ggen-a2a-mcp/src/ggen_server.rs.bak3`
  - `fix_ggen_server.patch`
- Preferred: keep transient artifacts outside tracked repo paths or under explicit temp workspace.
- Evidence(WIP:`git status` snapshot for this guide prep).

### Anti-pattern B: Root-level report/test artifact sprawl
- Reports and ad hoc files at repo root raise noise and review friction.
- Preferred: route reports into `analysis/` or `docs/validation-reports/` consistently.
- Evidence(Commit:`dfb62563`, `7da61829`): high volume of root report artifacts.

### Anti-pattern C: Generated binaries/scripts without clear lifecycle
- Keep generated binaries out of source control unless intentional and documented.
- Preferred: script source + reproducible build instruction.
- Evidence(Commit:`dfb62563` stats include binary artifact additions).

---

## 9) Ready-to-Use Checklists

## Pre-Commit Checklist
- [ ] `cargo make timeout-check`
- [ ] `cargo make check`
- [ ] `cargo make test`
- [ ] `cargo make lint`
- [ ] No `.bak`, `.patch`, or ad hoc scratch files in staged set
- [ ] New/changed MCP tools have typed params and deterministic response shape

## Pre-Push Checklist
- [ ] `cargo make check` clean (no errors/warnings)
- [ ] `cargo make test` passing
- [ ] `cargo make slo-check` passing
- [ ] For LLM/external paths: OTEL spans verified and evidence captured

## PR-Ready Checklist
- [ ] Commit sequence explains intent (script -> migration -> docs where applicable)
- [ ] Diff excludes transient artifacts
- [ ] Quality gate impacts documented if pipeline/manifest/template logic changed
- [ ] Test plan includes behavior assertions and failure-path coverage

## Post-Merge Stabilization Checklist
- [ ] Confirm `master` is clean and branch deletion policy applied
- [ ] Run smoke checks on representative examples after migration-heavy merges
- [ ] Update guide/docs if the process changed during merge resolution

---

## Quick Reference: Evidence Map

- Workflow and lint discipline: `dfb62563`
- Generation path policy and migration:
  - `3b34648b`
  - `811a65db`
  - `11d8df17`
- Large WIP integration baseline: `7da61829`
- MCP tool contract and observability:
  - `crates/ggen-a2a-mcp/src/ggen_server.rs`
  - `crates/ggen-a2a-mcp/tests/validation_e2e.rs`
- Pipeline safety and determinism:
  - `crates/ggen-core/src/codegen/pipeline.rs`
  - `crates/ggen-core/src/poka_yoke/quality_gates.rs`
  - `crates/ggen-core/src/graph/cycle_fixer.rs`
