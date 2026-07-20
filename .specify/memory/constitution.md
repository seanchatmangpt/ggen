<!--
═══════════════════════════════════════════════════════════════════════════════
SYNC IMPACT REPORT - ggen Constitution v2.0.0 (Physician, Heal Thyself)
═══════════════════════════════════════════════════════════════════════════════

Version Change: 1.1.0 → 2.0.0 (MAJOR: principle redefinition + removals)

Version Bump Rationale (MAJOR):
- Principle V REDEFINED: "cargo make Protocol" → "just Protocol" (cargo make is
  retired; Makefile.toml is historical reference only — backward-incompatible
  governance redefinition)
- Removed section: "Agent Coordination (Claude-Flow Hooks)" (no longer practice;
  orchestration is now ggen-generated — see packs/claude-code-pack)
- Stale architecture facts corrected throughout: 6-crate architecture → 17-crate
  workspace; ggen-core (deleted 2026-07-17, PR #259) references removed; SHACL
  admission gates → SPARQL gates (gates/*.rq, engine-independent)

Added Principles:
- XI. Admission Boundary & Standing (Law 14: self-reports are observations,
  not standing; receipts carry observational AND constitutional truth)
- XII. Evidence as Law (verification results are admitted graph facts enforced
  by refusal gates, not attention-dependent shell rituals)
- XIII. Canonical Vocabulary (原語保持 / 訳語非権威 / 変換保存 / 差異受領 / 意味停止)
- XIV. Self-Hosting Governance (staged authority migration, generation ladder,
  no same-generation constitutional mutation)

Modified Principles:
- V. just Protocol (was: cargo make Protocol)
- VII. Andon Signal Protocol: extended — andon in receipts MUST be derived from
  the admission decision, never asserted by event completion
- X. Lean Six Sigma Quality: pre-commit is 9 gates via `just pre-commit`

Removed Sections:
- Agent Coordination (Claude-Flow Hooks)

Templates Requiring Updates:
- ⚠ .specify/templates/plan-template.md: verify Constitution Check names just
  (not cargo make) — pending
- ⚠ .specify/templates/tasks-template.md: same — pending
- ✅ CLAUDE.md / README.md: generated regions already current (root ggen sync)

Follow-up TODOs:
- packs/ggen-constitution-pack (heal-thyself-r1) will carry these laws as
  admitted semantics (ccn:Law individuals with anchors + proving tests); this
  document remains the human-ratified prose authority until that pack reaches
  the standing to supersede it — authority migration per Principle XIV, staged.
═══════════════════════════════════════════════════════════════════════════════
-->

# ggen Constitution

## Core Principles

### I. Crate-First Architecture

Every feature MUST start as a standalone crate within the workspace. Crates MUST be:
- **Self-contained**: No circular dependencies between workspace crates (verified at compile time)
- **Independently testable**: Each crate has its own test suite achieving 80%+ coverage
- **Clearly scoped**: Single responsibility principle—domain logic separated from CLI, engine separated from utilities
- **Documented**: Public APIs MUST have rustdoc comments with examples
- **Consolidation-ready**: Design for future consolidation (see Crate Extraction & Consolidation Patterns)

**Circular Dependency Anti-Pattern**: If extracting a new crate creates a cycle, DO NOT extract. Instead:
1. Identify shared types/traits causing the cycle
2. Extract shared types to a separate types crate (if justified by 80/20 analysis)
3. Or accept the module staying in its host crate with clear documentation of scope

**Rationale**: Workspace structure enforces modularity, prevents tight coupling, and enables parallel
development. The current 17-crate workspace (see `.claude/rules/architecture.md`, itself generated from
`.specify/repo-facts.ttl`) demonstrates this principle. Circular dependencies block compilation; they are
architectural problems, not fixable by refactoring.

### II. Infrastructure-Domain Separation

Engine code (`ggen-engine` and its kernel crates) MUST maintain clear boundaries between
**infrastructure** and **domain**:

**Infrastructure** (reusable across crates): RDF graph management, the five-stage sync pipeline,
template resolution and rendering, write-decision law, receipt chain, error taxonomy.

**Domain** (business logic, extractable): pack management and resolution, capability registry,
validation policies, marketplace package management (feature-gated).

**Extraction Rule**: Domain modules MAY be extracted to separate crates IF they don't create circular
dependencies. If infrastructure code depends on the domain logic, the extraction is blocked.

**Process-Intelligence Boundary (CRITICAL)**: ggen EMITS process evidence (OCEL events); it does NOT
analyse it. Discovery, conformance, fitness, precision, and variants belong to wasm4pm/wasm4pm-compat.
Analysis code found in ggen is post-split residue and MUST be deleted. Enforced by
`scripts/ci/guard-process-intelligence-boundary.sh` in `just pre-commit`.

**Rationale**: Clear boundaries prevent architectural tangles. Infrastructure must be stable and
reusable; domain logic can evolve independently—IF it doesn't create cycles.

### III. Deterministic RDF Projections

Code generation MUST be deterministic and reproducible:
- **Same input → same output**: Identical RDF ontology + template MUST produce identical output across runs
- **Ordered queries**: Every template SPARQL block MUST carry `ORDER BY` (row order is otherwise
  engine-dependent; enforced as FM-TPL diagnostics)
- **Version-locked**: Cargo.lock is committed; pack content is hash-locked in `ggen.lock` (FM-PACK-008
  refuses drift)
- **Content-addressable**: Generated outputs are BLAKE3-hashed into the sync receipt
- **Idempotent operations**: A second sync MUST be byte-identical on content; only the receipt chain
  advances. Proven per consumer by sync-twice-diff (the `guard-pack-proofs` convention).

**Rationale**: ggen's core value proposition is deterministic manufacture from knowledge graphs.
Non-determinism breaks trust, breaks receipts, and makes fixed-point self-hosting impossible.

### IV. Chicago TDD (Zero Tolerance)

Test-Driven Development using Chicago School methodology is MANDATORY:
- **State-based testing**: Tests verify observable outputs, return values, and side effects
- **Real collaborators**: Actual engines, real files, real subprocess boundaries (`CliHarness`)—no
  mocks, no test doubles, no behavior verification
- **Red-Green-Refactor**: Tests written first, failing before implementation
- **80%+ coverage** on critical paths
- **Reference oracles over hand-written assertions**: where a real, independently-authored reference
  implementation exists, its own test suite copied byte-identical (checksummed, never retyped) and
  passing against generated output is the strongest admissible fidelity proof (the TCPS method)

**Exemption**: Test code MAY use `unwrap()`/`expect()` to fail fast on setup issues.

**Rationale**: Chicago TDD validates actual system behavior. A verbatim third-party suite passing
against generated code cannot be gamed by the generator adjusting its own tests.

### V. just Protocol

Direct `cargo` and `cargo make` invocations are PROHIBITED as entry points in development and CI.
Use `just` recipes exclusively:
- **Fast feedback**: `just check` (workspace check, 300s timeout)
- **Test execution**: `just test-lib` (fast loop), `just test` (full suite, escalating timeout)
- **Quality gates**: `just lint`, `just pre-commit` (9 gates — the Definition of Done)
- **Verification loops**: `just verify-tcps` (evidence emit → gate-enforced sync → receipt verify),
  `just docs-sync` (regenerate all generated docs + book, checkers must stay green)
- **Timeout enforcement**: recipes carry explicit timeouts; a timeout kill is a failure, never a pass

`Makefile.toml` is historical reference only. Bare `cargo` remains acceptable *inside* recipes and
for narrow local iteration, never as the gate.

**Rationale**: One entry point, enforced timeouts, and recipes as the single place where gate
composition is defined. The justfile's recipe list is itself a generated-doc fact
(`.specify/repo-facts.ttl`), so documentation cannot drift from it silently.

### VI. Type-First Thinking

Leverage Rust's type system for compile-time guarantees:
- **Express invariants in types**: newtypes, enums, typestates make invalid states unrepresentable
- **Zero-cost abstractions**: prefer generics and const generics over trait objects in hot paths
- **Explicit ownership**: Clone is deliberate; prefer references and lifetimes
- **API ergonomics**: public APIs MUST be type-safe AND easy to use

**Rationale**: Type-first thinking shifts errors from runtime to compile-time. If it compiles,
invariants are enforced. The TCPS reference (生産線<稼働中> halting on 品質判定::異常) is the canonical
in-repo demonstration.

### VII. Andon Signal Protocol

Stop the line immediately when quality signals appear:
- **RED (compiler error, test failure, gate refusal)**: STOP all work. Fix root cause before proceeding.
- **YELLOW (warning, lint, incomplete evidence)**: investigate before release.
- **GREEN (clean output, evidence admitted)**: safe to continue.

**Receipt andon MUST be derived, never asserted**: a receipt's `andon` field MUST be computed from
the admission decision over the evidence (all required evidence current and admitted → Green;
incomplete/unsupported → Yellow; red, stale, inconsistent, or refused → Red). A synchronization
event completing is an observation; it does not by itself produce Green. A successful sync with
unsupported evidence is Yellow or Red in the chain.

**Rationale**: Andon signals prevent defects from propagating downstream. An andon that is hardcoded
by event completion is a receipt lying about the control plane.

### VIII. Error Handling Standards

Production code MUST use `Result<T, E>` error handling:
- **NO `unwrap()`/`expect()` in production** (test code exempted)
- **Typed refusals**: every refusal carries a stable `[FM-<FAMILY>-NNN]` code with remediation text;
  the code inventory is product surface (mirrored in `.specify/`, drift-refused)
- **Unknown is not success**: unknown config keys, legacy law surfaces, unreadable governing inputs,
  and malformed packs refuse LOUDLY with migration guidance — never silently ignored
- **Fail-safe design**: errors propagate with `?` or are handled explicitly; `unwrap_or_default()`
  on a meaningful Result launders failure and is treated as a defect

**Rationale**: Production systems handle errors gracefully; governance systems refuse visibly.
A file that used to be law and is now silently ignored is the worst failure class.

### IX. Concurrent Execution

All operations MUST be batched in single messages:
- **"1 MESSAGE = ALL RELATED OPERATIONS"**: task tracking, agent spawning, file operations, commands
- **Parallel agent execution**: fan-out implementation batches concurrently; verification is a
  distinct sequential phase (see Principle XI — implementers' self-reports are inputs, not proof)
- **File organization**: NEVER save working files to the repo root—use `crates/*/src/`, `tests/`,
  `docs/`, `scripts/`, the session scratchpad for temporaries

**Rationale**: Concurrent execution is faster AND cleaner when the verify boundary is explicit.
The generated `implement-verify` workflow (packs/claude-code-pack) encodes this shape as law:
a workflow without a verify phase refuses to sync.

### X. Lean Six Sigma Quality

Enforce manufacturing-grade quality standards:
- **Poka-Yoke**: invalid states unrepresentable in types; gates refuse before write
- **9-gate pre-commit**: `just pre-commit` chains fmt-check, check, lint, test-lib, coherence-check,
  guard-process-intelligence-boundary, guard-cheat-scan, guard-claims-schema, guard-pack-proofs —
  all green or the commit does not happen; a partial pass is a failure
- **Cheat scanning**: `ggen-cheat-scanner` detects vacuous asserts, tautological checks,
  no-assertion tests, and mock imports across the workspace
- **Evidence over narration**: no completion claim without the command run and its exit code

**Rationale**: DfLSS prevents defects AND waste from the start. Prevention is cheaper than
detection, which is cheaper than correction.

### XI. Admission Boundary & Standing (Law 14)

**自己申告は観測であり、standing ではない。** Self-reports are observations, not standing.

- A claim alone cannot cross the admission boundary. Evidence alone cannot either. Evidence is
  evaluated under this constitution, and the durable consequence is receipted:
  observation → evidence → constitutional judgment → standing ceiling → next lawful generation.
- Receipts MUST carry two truths without confusing them: **observational truth** (which checks ran,
  exit codes, digests, graph identity, toolchain, evidence age) and **constitutional truth**
  (admission outcome, standing ceiling, open obligations, quarantine reasons, permitted and
  prohibited claims, the equivalence relation established per artifact class).
- Standing is a **ceiling, not a victory label**: the meet of lifecycle × scope × equivalence ×
  freshness — never the most impressive passing cell. A new generation cannot round a weaker prior
  standing upward.
- Maturity (L1–L5) is derived from named artifacts only. A self-reported level contradicted by its
  own evidence is refused. No evidence, no promotion. Contradictory evidence, refusal.
- Unknowns stay explicit: if binary reproducibility is unknown, the receipt says "unknown" —
  the fixed-point claim is bounded by the weakest unresolved artifact class. `Unknown` is a
  serialized value; an absent entry is malformed evidence, not Unknown.
- **Receipt epochs migrate by law, not by field addition**: a receipt-schema change is a
  constitutional migration — new epoch identity, a migration receipt binding the final old-epoch
  record to the first new-epoch record, dual-read/single-write compatibility (old readers refuse
  new receipts loudly; new readers accept old receipts only as legacy-bounded), and **no
  backfilling of knowledge that did not exist**: a legacy hardcoded Green never becomes admitted
  Green; a legacy zero obligation count never proves discharge; legacy standing recovers at most
  LegacyObserved unless actively reverified under the current constitution.

**Rationale**: The retrofit history showed agents reporting `reached_l5 = true` against contradicting
evidence; those claims were rejected by independent reconciliation. This principle makes that
rejection law rather than vigilance. **Standing crosses nowhere without a 受領証 — and no 受領証
crosses an epoch without a receipted migration.**

### XII. Evidence as Law

Verification results are admitted graph facts enforced by refusal gates — not attention-dependent
shell rituals:
- **Admission gates are SPARQL** (`gates/*.rq`: `# MESSAGE:` header + one ASK/SELECT; violation =
  typed FM-PACK-012/013 refusal), engine-independent by construction (proven identical under
  praxis-graphlaw and Oxigraph by `reasoner_independence_e2e`)
- **External check results enter the graph** through the same admission path as all law (evidence
  emitters → `ver:Check` facts → gates refuse missing, red, or stale evidence)
- **Four-process separation**: observation (real runs) / evidence transport (unaltered) /
  judgment (gates) / manufacture (only on admitted evidence). The verifier does not manufacture;
  the manufacturer does not fabricate evidence.
- **Generated output is never authoritative input unless explicitly readmitted** (reflexive receipts
  are opt-in admission; hand-edits to generated surfaces are drift, flagged at edit time)
- **Every reflexive layer bottoms out in external ground truth**: a real exit code, a byte-identity
  against a frozen reference, a second-engine agreement. A layer certifying another layer without
  touching ground is the prohibited failure mode.

**Rationale**: The measured effect is verification transferred from attention to law — marginal
human verification attention per unit of manufactured work approaches zero while machine
verification work may grow. The exit code is the digital 現物.

### XIII. Canonical Vocabulary (原語保持)

Canonical production vocabulary participates in the architecture; it is not localization.

- **原語保持**: canonical production concepts (自働化, 現地現物, 受領証, 異常停止, 標準作業, 改善,
  かんばん, ポカヨケ, ...) retain their original terms throughout the normative manufacturing surface
- **訳語非権威**: translations may aid comprehension but possess no independent authority; the gloss
  is commentary, the canonical term is authority
- **変換保存**: every projection MUST preserve canonical vocabulary through graph union,
  serialization, generation, compilation, packaging, and replay — proven by byte-fidelity oracles,
  not asserted
- **差異受領**: any unavoidable alteration is named, justified, bounded, and bound to a 受領証
- **意味停止**: when a transformation cannot preserve the concept, the process stops rather than
  emitting a convenient approximation

**Rationale**: The terms constrain the reachable state space. 自働化 cannot degrade into generic
automation; 受領証 cannot degrade into a receipt-shaped file; 異常停止 cannot degrade into an
ignorable exception. Naming is not downstream of system design — naming determines which system
can be designed.

### XIV. Self-Hosting Governance

ggen is not the privileged exception to the laws it imposes. It becomes a product manufactured by
its own packs, through **staged authority migration** — never in one leap:

1. Descriptive mirror → 2. Generated secondary surfaces → 3. Generated decision surfaces →
4. Generated execution surfaces → 5. Self-hosting candidate → 6. Fixed point → 7. Reference retirement.

- **The generation ladder**: G0 (trusted seed, authority = historical standing) manufactures G1;
  the candidate manufactures its successor; fixed point requires stable closure across successive
  clean generations, with the equivalence relation declared **per artifact class** (byte / semantic /
  behavioral), never as one global "equals"
- **No same-generation constitutional mutation**: Constitution N governs Generation N+1. A running
  generation MUST NOT weaken the rules governing its own run; proposed changes are admitted,
  reviewed under the current constitution, and activate only in the next generation
- **Independent rails**: generated proofs alone never judge the generator. An independent behavioral
  rail (real consumers, including ones unrelated to the packs' own lineage) and an external
  conformance rail (manifest digests, schema validation, second engines, the reference's own
  checkers) must remain distinct from the generated proof rail
- **Reference preservation**: the current trusted implementation is the initial reference oracle
  during transition — its tests applied unmodified to generated candidates; it retires only when
  successive generations no longer need it to manufacture or understand the product

**Rationale**: A self-hosting system cannot begin by declaring its own generated result correct.
Diverse verification keeps self-hosting from becoming self-belief. The current ladder position is
recorded honestly in the generation ledger — Stage claims above the evidence are refused per
Principle XI.

## Crate Extraction & Consolidation Patterns

### When to Extract a Crate

Extraction is justified when a module satisfies ALL of these (80/20 Pareto analysis):

1. **Size**: ≥5k LOC (small extractions create maintenance burden without benefit)
2. **Stability**: Module API is stable
3. **Independence**: Zero infrastructure dependencies (or a clear boundary)
4. **Reusability**: Used by ≥2 other crates or is a standalone library
5. **No circular deps**: Verify extraction doesn't create a cycle

### When to Consolidate Crates

Consolidation is justified when a crate satisfies ANY of these:

1. **Micro-crate**: <2k LOC with few dependents
2. **Tight coupling**: >90% of functionality depends on another crate
3. **Shared types only**: exists primarily to share types with a parent
4. **Stable and small**: <5k LOC, stable API, well-tested

The 2026-07 consolidation pass (see `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` and the generated
crate map in `.claude/rules/architecture.md`) is the historical record of this pattern applied.

### Feature-Gating Optional Crates

Optional functionality MUST be feature-gated (e.g., `ggen-lsp`'s `mcp`/`a2a` features,
marketplace-adjacent surfaces), keeping default builds lean and dependency boundaries explicit.

## Build & Quality Standards

### just Targets (MANDATORY)

- `just check` — workspace compilation check
- `just test-lib` / `just test` — test suites with enforced timeouts
- `just lint` — clippy (root package scope; the known `--workspace` gap is documented in the
  justfile itself and is accepted, not hidden)
- `just pre-commit` — the 9-gate Definition of Done
- `just verify-tcps` — the self-hosted verification loop (evidence → gates → receipt)
- `just docs-sync` — regenerate all generated docs/book; second run MUST be a content no-op
- `just slo-check` — the two real SLOs (CLI startup bench; receipt-chain wall-clock ≤180s)

### Quality Gates (Zero Defects)

**CANNOT commit code that**:
- has compiler errors or failing tests
- uses `unwrap()`/`expect()` in production code
- hand-edits generated surfaces (edit the ontology, re-sync — enforced by convention, hookify
  warning, and force-overwrite semantics)
- weakens an assertion to make a suite pass
- claims completion without the command run and its exit code

## Development Workflow

### Speckit Integration (Spec-First Development)

NO implementation without specification. RDF is the source of truth: edit `.specify/*.ttl`,
never generated `.md`. Feature flow: `/speckit.constitution` → `/speckit.specify` →
`/speckit.plan` → `/speckit.tasks` → `/speckit.implement`, with evidence in
`.specify/specs/NNN-feature/evidence/`.

### TDD Cycle (Red-Green-Refactor)

Chicago school, state-based, real collaborators; `just test-lib` for the fast loop; watch Andon
signals continuously; commit only on GREEN. Where a reference oracle exists, its unchanged tests
are the acceptance bar (Principle IV).

### File Organization Rules

**NEVER save to the repo root.** Source under `crates/*/src/`, tests under `crates/*/tests/` or
`tests/`, docs under `docs/`, scripts under `scripts/`, packs under `packs/`, examples under
`examples/`, specs under `.specify/`, temporaries in the session scratchpad.

## Governance

### Amendment Procedure

1. **Proposal**: amendment with rationale (issue, PR, or session directive)
2. **Impact Analysis**: effect on existing specs, templates, packs, generated surfaces
3. **Template Sync**: update `.specify/templates/` to reflect new principles
4. **Version Bump**: MAJOR (removals/redefinitions), MINOR (additions), PATCH (clarifications)
5. **Approval**: maintainer sign-off
6. **Temporal boundary (Principle XIV)**: an amendment ratified during a generation governs the
   NEXT generation; it cannot retroactively weaken the gates of the run that produced it

### Versioning Policy

- **Format**: MAJOR.MINOR.PATCH (Semantic Versioning)
- **Version in File**: single source of truth at the bottom of this document
- **Admitted-semantics mirror**: `packs/ggen-constitution-pack` (heal-thyself-r1) carries these
  laws as `ccn:Law` individuals with enforcement anchors and proving tests; where the pack and this
  prose disagree, the disagreement is itself a defect to resolve — neither silently wins

### Compliance Review

- **Pull Requests**: MUST verify compliance with all principles
- **CI Pipeline**: full quality gates; advisory lanes stay advisory but visible
- **Complexity Justification**: violations documented in plan.md with reasoning
- **Escalation**: non-compliance flagged in review; blocking for RED signals

### Relationship with CLAUDE.md

This constitution supersedes CLAUDE.md where principles overlap. CLAUDE.md provides runtime
development guidance (its fact-bearing regions are themselves generated from
`.specify/repo-facts.ttl`). Precedence: Constitution → CLAUDE.md → README.md.

**Version**: 2.0.0 | **Ratified**: 2025-12-11 | **Last Amended**: 2026-07-19
