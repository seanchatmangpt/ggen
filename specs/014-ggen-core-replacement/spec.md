# Feature Specification: Retire ggen-core in favor of a first-principles engine

**Feature Branch**: `2026-ggen-core-replacement`
**Created**: 2026-07-16
**Status**: Draft
**Input**: User description: "Retire ggen-core in favor of a first-principles engine built from praxis/crates/ggen, per the full 13-ticket scope in docs/jira/v26.7.16/ (00-OVERVIEW through 13-CLAUDE-MD-REFACTOR)"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Maintainers get a smaller, honest codebase without losing capability (Priority: P1)

As a ggen maintainer, I want the 142,908-line `ggen-core` crate — which is actually seven
former crates glued together with three redundant pipeline implementations — replaced by a
much smaller, already-tested engine, so that the workspace is easier to reason about and
change, while every capability current users depend on keeps working.

**Why this priority**: This is the entire point of the migration. Without it, nothing else
in this feature matters — it's the P1 because every other story depends on the replacement
actually happening without a functionality regression.

**Independent Test**: Can be fully tested by running the full test suite
(`just check && just lint && just test && just slo-check`) against the new engine and
confirming it passes, and by diffing the CLI/LSP command surface before and after to confirm
no command silently disappeared.

**Acceptance Scenarios**:

1. **Given** a clean checkout of the repository, **When** the workspace is built, **Then**
   `ggen-core` no longer exists as a crate and the build succeeds with no dangling
   references to it.
2. **Given** a project that used any existing `ggen` CLI command (`sync`, `pack`, `init`,
   `wizard`, `receipt`, `doctor`, `graph`, `ontology`, `policy`, `capability`, `agent`,
   `sigma`, `inverse-sync`), **When** that command is run against the replaced engine,
   **Then** it produces the same observable outcome as before the migration.
3. **Given** the LSP diagnostic checks a user's editor relies on
   (unbound-template-variable, output-path-escape, dangling-rule-binding, wildcard-SPARQL,
   missing-ORDER-BY, identity-CONSTRUCT), **When** a project with those exact defects is
   opened, **Then** every diagnostic still fires with the same code and severity as before.

---

### User Story 2 - The published crate on crates.io is never put at risk (Priority: P1)

As the owner of the `ggen` package already published on crates.io, I want the migration to
guarantee that package's identity and publish history are never disrupted by adopting code
from a differently-owned crate that happens to share the same name, so that existing
downstream users of `cargo install ggen` / `cargo add ggen` are never affected.

**Why this priority**: This is a hard constraint, not a nice-to-have — a mistake here
corrupts a real, already-public artifact that other people depend on and cannot be
undone by a later commit.

**Independent Test**: Can be fully tested by running `cargo publish --dry-run --package
ggen` before and after the migration and confirming it always targets the same package
identity, and by confirming no other crate in the workspace is ever named `ggen`.

**Acceptance Scenarios**:

1. **Given** the migration is complete, **When** `cargo publish --dry-run` is run from the
   workspace root, **Then** it only ever succeeds for the original root package and fails
   loudly for any other crate that might share its name.
2. **Given** the vendored replacement engine crate, **When** its manifest is inspected,
   **Then** it has a distinct package name and is explicitly marked non-publishable.

---

### User Story 3 - Provenance and auditability are preserved or improved (Priority: P2)

As a user of `ggen sync` who relies on its cryptographic receipts to prove what was
generated from what inputs, I want the replacement engine's receipts to be at least as
trustworthy as today's — provably unmodified (hash-chain integrity) *and* provably
attributable to a known signer (cryptographic signature) — so that I don't lose an
auditability guarantee I already depend on.

**Why this priority**: Important and specifically flagged by prior research as a real gap
(the replacement engine's receipt chain, as available today, has no signature at all), but
the migration can proceed in stages without this being finished first — hence P2, not P1.

**Independent Test**: Can be fully tested by running `ggen sync` twice, tampering with the
middle receipt, and confirming `ggen receipt verify`/`ggen receipt history` detects both the
tampering (chain integrity) and can distinguish a validly-signed receipt from an unsigned or
forged one (authenticity).

**Acceptance Scenarios**:

1. **Given** a receipt chain produced by three consecutive `ggen sync` runs, **When** any
   one receipt's stored content is altered, **Then** verification fails and names which
   receipt is broken.
2. **Given** a receipt signed with a known key, **When** verification is run with the
   matching public key, **Then** it reports the receipt as both chain-valid and
   signature-valid, distinctly from a chain-valid-but-unsigned receipt.

---

### User Story 4 - Architectural and compliance boundaries hold under the new dependency graph (Priority: P3)

As the owner of this project's architectural rules (the "process intelligence must only be
emitted, never analyzed inside ggen" boundary) and licensing posture, I want the new
dependency graph — which pulls in several new upstream crates — to be checked against those
rules automatically, so that a rule violation is caught by tooling rather than discovered
later by a person.

**Why this priority**: Real risk, but lower urgency than P1/P2 — the boundary is not
violated *today* (nothing calls the newly-reachable code), so this is about adding a
standing guarantee, not fixing an active breach.

**Independent Test**: Can be fully tested by running the new CI check against the current
codebase (should pass clean today) and against a deliberately-introduced violation (should
fail).

**Acceptance Scenarios**:

1. **Given** the full dependency graph after migration, **When** the boundary-check runs in
   CI, **Then** it passes on the current codebase and fails if any code path is added that
   crosses into the forbidden analysis module.
2. **Given** the licenses declared by every newly-adopted upstream crate, **When** they are
   reviewed, **Then** none claims a license option it cannot actually back with license text,
   and none conflicts with the already-published root package's license.

---

### Edge Cases

- What happens when a downstream project's `ggen.toml` mixes fields that used to be silently
  ignored by one config parser and silently ignored by another (the two independently-evolved
  schema definitions found during research)? The migration must decide which schema wins for
  each overlapping field, not leave the ambiguity in place.
- How does the system handle a build environment that is missing one of the new sibling
  dependencies' absolute filesystem paths (a machine without the extra local checkouts the
  new engine's dependencies expect)? The build must fail with a clear, actionable error, not
  a confusing downstream compile error.
- What happens to a receipt produced by the *old* engine when verified against the *new*
  engine's verification logic, and vice versa, during a transition period? This must be
  either fully forward/backward compatible or explicitly and clearly rejected, never silently
  misinterpreted.
- What happens when a project has never run `ggen sync` before (no receipt history yet) —
  does `ggen doctor`/`ggen receipt verify` handle the "nothing to verify yet" case
  gracefully?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST provide every command currently available in the CLI (`sync`,
  `pack`, `packs`, `init`, `wizard`, `receipt`, `doctor`, `graph`, `ontology`, `policy`,
  `capability`, `agent`, `sigma`, `inverse-sync`, `template`, `lsp`, `mcp`) with equivalent
  observable behavior after the retired engine is removed.
- **FR-002**: The system MUST continue to install, search, and manage packs (the "pack
  registry" capability) with at least the same feature set as today, including local and
  remote sources, dependency resolution, and a lockfile that detects drift.
- **FR-003**: The system MUST continue to scaffold new projects (`init`/`wizard`) across the
  same set of supported project types as today.
- **FR-004**: Every `ggen sync` run MUST produce a receipt that is both tamper-evident (a
  broken hash chain is detectable) and attributable (a valid cryptographic signature proves
  who produced it), improving on the current split where different code paths provide one
  guarantee but not both consistently.
- **FR-005**: The system MUST emit machine-readable operational telemetry for every stage of
  a sync run (load, extract/enrich, generate, validate, write) including per-stage duration
  and a count of files produced, and that telemetry MUST be verified to actually appear in
  captured output, not merely be present in source code that silently fails to record it.
- **FR-006**: The system MUST continue to surface every existing editor diagnostic (unbound
  template variables, escaping output paths, dangling rule bindings, wildcard SPARQL
  projections, missing deterministic ordering, no-op identity transforms) with the same
  identifying codes as today.
- **FR-007**: The system MUST NOT allow the replacement engine's source crate (which shares
  a name with the already-published root package) to be published to a public registry under
  that shared name, under any build or release path.
- **FR-008**: The system MUST verify, via an automated check, that no code path inside the
  project ever calls into the specific external analysis capability this project's
  architecture forbids importing directly (the process-conformance/fitness-analysis module
  reachable through one of the new dependencies) — the check MUST fail the build if such a
  call is ever introduced.
- **FR-009**: The system MUST resolve the two independently-evolved project-configuration
  schema definitions discovered during research into one authoritative schema per
  configuration section, eliminating the silent-ignore behavior found in the current code.
- **FR-010**: The system MUST declare a single, unambiguous license for every newly-adopted
  dependency, with backing license text present for every license option claimed.
- **FR-011**: The system MUST NOT leave a compatibility shim re-exporting the retired
  engine's API surface after the migration completes — every consumer must be re-pointed to
  the real replacement, not to a forwarding layer.
- **FR-012**: The performance/regression validation gate (`slo-check`) MUST test what its own
  description claims to test (e.g., a stated timing bound MUST have an actual timing
  assertion backing it, not just a correctness check mislabeled as a performance check).

### Key Entities

- **Engine**: The deterministic code-generation pipeline (RDF graph load, law/SHACL/N3
  evaluation, template rendering, file write) that replaces `ggen-core`'s role. Owns the
  receipt chain and its own operational telemetry.
- **Receipt**: A record produced by every sync run, binding the inputs consumed (ontology,
  queries, templates, manifest, installed packs) to the outputs produced, chained to prior
  receipts for tamper-evidence, and signed for attribution.
- **Pack**: An installable, versioned unit of templates/queries/rules that a project can
  depend on; tracked in a lockfile for reproducibility and drift detection.
- **Diagnostic**: A named, coded finding (e.g., an unbound template variable) surfaced by the
  editor-integration layer against a project's source files, independent of whether a sync
  has actually been run.
- **Configuration schema**: The authoritative definition of what a project's `ggen.toml` may
  contain, replacing today's two competing, partially-overlapping definitions.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of CLI commands and LSP diagnostics available before the migration remain
  available and behaviorally equivalent after it, verified by the existing automated test
  suite passing without modification to test expectations.
- **SC-002**: The published package's release process (dry-run publish check) succeeds
  every time it is run post-migration, with zero incidents of it targeting the wrong
  package identity.
- **SC-003**: 100% of sync runs produce a receipt that is verifiably both tamper-evident and
  attributable to a signer, versus a mix of guarantees today.
- **SC-004**: 100% of sync runs emit verifiable per-stage operational telemetry, versus a
  currently-broken telemetry path that silently records nothing for one key metric.
- **SC-005**: The core engine's source size is reduced by at least 90% (from ~143,000 lines
  to a low-tens-of-thousands-line footprint across the replacement and the sibling crates it
  extends), while capability parity (SC-001) still holds.
- **SC-006**: Zero references to the retired engine crate remain anywhere in the codebase
  after completion, confirmed by an automated search returning no results.
- **SC-007**: The automated architectural-boundary check runs on every change and has never
  been observed to pass a change that violates the boundary (validated by a deliberate
  negative test).

## Assumptions

- The replacement engine is assembled from the `praxis/crates/ggen` crate plus its
  `praxis-core` and `praxis-graphlaw` dependencies, vendored into this repository under a
  new, non-conflicting name — not consumed as a live cross-repository dependency, since the
  latter would either fail to publish or risk the name collision described in User Story 2.
- Capabilities present in the retired engine but absent from the replacement (pack registry
  breadth, project scaffolding, receipt signing, operational telemetry, SPARQL/RDF-related
  editor diagnostics' backing configuration parser) are re-homed into the most relevant
  existing component rather than rebuilt as a new standalone component, to avoid recreating
  the original problem (an oversized, hard-to-reason-about core) in a new location.
- Where the replacement engine's RDF/SPARQL/SHACL evaluation library differs from the three
  already-existing RDF stacks elsewhere in this codebase, those stacks are bridged (kept
  separate, interoperating through a narrow, engine-agnostic interface) rather than merged
  into one, for this migration. Full consolidation onto a single RDF engine is treated as
  future work, out of scope here.
- Receipt signing uses a locally-generated, auto-managed key pair by default (matching
  today's zero-configuration behavior for this specific workflow), rather than requiring an
  operator to configure a signing key via environment variable before first use. This
  default can be revisited without re-litigating the rest of this specification.
- A newly-introduced architectural-boundary check treats "reachable in the dependency graph
  but never actually invoked" as acceptable for this migration, provided the automated check
  from FR-008 exists to catch the moment that stops being true.
- Where two independently-evolved configuration schemas overlap on the same setting, the
  more complete, actively-consumed definition is treated as authoritative and the other is
  retired, rather than attempting to merge both into a superset that has never been
  validated against real projects.
