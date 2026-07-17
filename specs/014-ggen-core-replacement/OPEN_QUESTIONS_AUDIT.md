# Open Questions Audit

Findings from discovery-only audits of `ggen-core` modules not yet covered by
`specs/014-ggen-core-replacement/tasks.md`. Each section is written by a separate agent for its
assigned cluster. Do not edit another agent's section; append new sections under a new `##`
heading.

## security/pki/membrane cluster (~7,200 lines: security 5,405 + pki.rs 846 + membrane 917)

**Scope**: `/Users/sac/ggen/crates/ggen-core/src/security/` (12 files), `src/pki.rs` (846 lines,
single file), `src/membrane/` (5 files + mod.rs). Total confirmed via `wc -l`: 7,168 lines across
18 files (5,405 + 846 + 917 = 7,168, matches exactly).

**Method**: `rust-analyzer` is still down in this session — `LSP workspaceSymbol` on
`pki.rs:286:12` (the `PkiManager` struct definition) returned the identical error already
recorded in tasks.md's T068 entry: `Error performing workspaceSymbol: LSP server
'plugin:rust-analyzer-lsp:rust-analyzer' exceeded max crash recovery attempts (3)`. This confirms
the outage is session/environment-level, not something this pass could route around either — same
conclusion tasks.md already reached, re-verified independently. Fell back to exhaustive `grep`
sweeps per the documented fallback procedure: exact-symbol references, `ggen_core::<module>::`
path references, and internal (`crate::<module>::`) self-consumption inside `ggen-core`, run
separately for every public item each module exports.

### `pki.rs` (846 lines) — DEAD, but high-value; NOT a duplicate of the receipt-signing work

- Public surface (re-exported at `lib.rs:290`: `pub use pki::{verify_ed25519, KeyPurpose,
  PkiManager, TrustedKeyEntry, TrustedKeysConfig};`): a TOML-backed **trust store** for Ed25519
  public keys — add/remove/lookup-by-name/lookup-by-fingerprint/expire/purpose-filter, backed by
  `.ggen/trusted-keys.toml` (project-local) or a user-level config dir. 18 unit tests in-file
  (lines 524-846), all real Ed25519 keypair generation/sign/verify via `ed25519_dalek`, no mocks.
- **Zero external callers anywhere in the workspace.** `grep -rln "PkiManager\|TrustedKeysConfig\|
  TrustedKeyEntry\|KeyPurpose\b\|verify_ed25519"` across `crates/`, `src/`, `tests/`, `examples/`,
  `benches/` (excluding `crates/ggen-core/`) returns exactly one hit, and it is unrelated (see
  next finding). `grep -rn "ggen_core::pki"` workspace-wide: zero matches.
- **Zero internal (self-)consumers inside `ggen-core` either.** `grep -rn "crate::pki"` inside
  `crates/ggen-core/src` other than `pki.rs`'s own doc-comment example (`pki.rs:33`, itself inside
  a `//!` block, non-compiled) returns nothing.
- **Not the same coincidental-match pattern as `project_config_path()`** (tasks.md's T068 entry
  already noted `pki.rs`'s `project_config_path()` is an unrelated substring hit for the separate
  `utils::project_config` dead-code investigation — confirmed still accurate, that was about a
  *different* symbol in this same file, not about `pki.rs` as a whole).
- **Genuine architectural relevance, not just dead weight**: T063 (open, not yet done) reads
  "Implement `verify_receipt_record()` two-step verification... decide and document the
  key-management policy (zero-config file keypair vs. `PRAXIS_SIGNING_KEY` env var)." The already-
  ported receipt-signing code in `crates/ggen-config/src/receipt/{receipt_impl.rs,chain.rs,
  envelope.rs}` (T031, done) does raw Ed25519 sign/verify against a caller-supplied
  `SigningKey`/`VerifyingKey` — it has **no trust-store, fingerprint, expiry, or purpose-filter
  layer at all**. `pki.rs`'s `PkiManager` is exactly that missing layer, already implemented,
  already tested, and about to be deleted wholesale by T055 with no task in tasks.md currently
  proposing to port it. This is a real gap: T063's "key-management policy" question already has a
  working, tested answer sitting in code that's on the chopping block. Flagging as
  **UNCERTAIN-BUT-ACTIONABLE**: not classified DEAD-safe-to-drop without a decision, because
  deleting it forecloses the option T063 needs, silently.

### `crates/ggen-marketplace/src/marketplace/pki.rs` (455 lines) — DUPLICATE-OF (schema-incompatible), itself DEAD

- Discovered while checking for pki/receipt-signing overlap. This file exists, has its own
  `PkiManager`, `TrustedKeyEntry`, `TrustedKeysConfig` — **same class names as `ggen-core`'s
  `pki.rs`**, same nominal target file `.ggen/trusted-keys.toml`, same Ed25519 basis — but a
  **different, incompatible on-disk schema**: `key_id` + `public_key_hex` (hex, not base64) +
  `label` + a separate top-level `revoked_keys: Vec<String>` list, vs. `ggen-core`'s `name` +
  `public_key` (base64) + `fingerprint` + `purpose` + `expires_at` per-entry. Two independently-
  designed, field-incompatible schemas both claiming the same config file name.
- **This file is not even wired into its own crate.** `grep -rn "mod pki"` across
  `crates/ggen-marketplace/src` (including non-`pub mod` declarations): zero matches. No
  `mod.rs`/`lib.rs` in `ggen-marketplace` declares `pki` as a module at all. `grep -rn "pki" -i`
  across the whole crate outside the file itself: zero matches. The crate's real trust-tier logic
  lives in `marketplace/trust.rs` (228 lines, `TrustTier` enum — `EnterpriseCertified` /
  `EnterpriseApproved` / `CommunityReviewed` / etc., a completely different concept: pack trust
  classification, not key trust). Verdict: **DEAD, orphaned, not compiled** — `cargo build` never
  sees this file. Noting it because it directly answers the "does pki overlap with the
  receipt-signing work" question from the brief: the overlap isn't with the already-ported receipt
  code, it's with this second, unrelated, unbuilt draft that predates it and lost.

### `security/` (5,405 lines, 12 files) — splits cleanly into a small load-bearing core and a large dead subsystem

**Load-bearing (real callers, cite them):**

- `security/command.rs` (326 lines) — `SafeCommand`/`CommandExecutor`/`CommandError`. Already
  flagged as a known duplicate being handled by a concurrent workflow (T030's note: ported to a
  private `ggen-cli/src/scaffolding/project_generator/safe_command.rs`). **Additional internal
  caller not mentioned in tasks.md's T030 entry**: `crates/ggen-core/src/pipeline.rs:611`
  (`execute_shell_hook`, `use crate::security::SafeCommand;`) — this is the **top-level**
  `pipeline.rs` (869 lines, distinct from `pipeline_engine/pipeline.rs` and
  `codegen/pipeline.rs` — three separate files literally named `pipeline.rs`). Top-level
  `pipeline.rs`'s own `Pipeline`/`PipelineBuilder` types are themselves load-bearing externally
  (`tests/tracing.rs:1`, `tests/template_systems_tests.rs:12`, `tests/generator_core_tests.rs:23`
  all do `use ggen_core::pipeline::{Pipeline, PipelineBuilder}`) and already appear in tasks.md's
  T053 "50 files remain genuinely untouched" bucket under the `Pipeline`/`Generator`/`Template`
  heading — so this `SafeCommand` call site is live, not dead-calling-dead. Whoever ports
  `security::command` should know it needs a 3rd call site fixed (in addition to
  `project_generator/mod.rs`'s 3 documented sites), inside a file this ticket has not yet assigned
  a port destination for.
  - External: `tests/security/week4_security_hardening_tests.rs:12` (`use
    ggen_core::security::command::{SafeCommand, CommandExecutor};`) — already tracked in tasks.md
    (line 995-996, one of the 50 blocked/untouched files).
- `security/validation.rs` (326 lines) — of its 3 exported types, only `PathValidator` has a real
  consumer: `crates/ggen-core/src/template_types.rs:39,310,452` (`use
  crate::security::validation::PathValidator;` then
  `PathValidator::validate_within(&rdf_path_raw, template_dir)` twice). `template_types.rs` (965
  lines, a distinct top-level module — separate from `domain::template`/top-level `templates/`)
  is itself unexamined by anyone as far as tasks.md records; not in this cluster's scope, noted
  only to substantiate `PathValidator`'s load-bearing status. `EnvVarValidator` and
  `InputValidator` (also exported from this file) have **zero callers anywhere**, internal or
  external — confirmed by `grep`, only their own in-file unit tests reference them.
  - External: `week4_security_hardening_tests.rs:13` imports `PathValidator, EnvVarValidator,
    InputValidator` together — already tracked, same file as above.
  - **DUPLICATE-OF-SOMETHING-ELSE, three-way**: `security::validation::PathValidator` is a
    *third*, independent `PathValidator`-shaped type in `ggen-core`, distinct from both:
    (a) `utils::path_validator::PathValidator` (726 lines, instance-based —
    `PathValidator::new(workspace_root)`, doc'd at `utils/mod.rs:72` as "enterprise-grade
    validation with workspace bounds"), and (b) `validation::input::PathValidatorRule`
    (`validation/input.rs:545`, a rule-object in a `NegativeRule`/`OrRule`/`PositiveRule`-style
    composition framework, re-exported from `validation/mod.rs:86`). `security`'s version is
    static-method style (`PathValidator::validate_within(path, base_dir)`,
    `security/validation.rs:39-`) with its own `ValidationError` enum
    (`InvalidPath`/`PathTraversal`/`InvalidEnvVar`/`TooLong`/`InvalidCharacters`/`EmptyInput`,
    `security/validation.rs:13-31`) — no shared code with either of the other two. This directly
    extends the open "SafePath/SafeCommand duplicate" question tasks.md already flagged (T068's
    "still needing an LSP findReferences sweep" item) with concrete evidence: it's not a two-way
    duplicate, it's (at least) three-way, and the `security` one is the only one of the three with
    zero test-file-only usage (it has one real internal caller, `template_types.rs`, that the
    other two don't obviously share).
- `security/error.rs` (275 lines) — `ErrorSanitizer`/`SanitizedError`. **Zero internal `ggen-core`
  consumers** (confirmed: every match for `ErrorSanitizer`/`SanitizedError` outside
  `security/error.rs` and `security/tests.rs` is inside `security/error.rs`'s own doc-comments/
  tests). Only external consumer: `week4_security_hardening_tests.rs:14,358` (already tracked,
  same file as above).

**DEAD (zero external callers, zero internal `ggen-core` self-consumers, confirmed by grep):**

- `security/template_secure.rs` (784 lines) — `ContextEscaper`, `SecureTeraEnvironment`,
  `TemplateSandbox`, `TemplateSecurityError`, `TemplateValidator`, `MAX_TEMPLATE_SIZE`,
  `MAX_VARIABLE_NAME_LENGTH`. Every reference to each of these 7 symbols anywhere in
  `crates/ggen-core/src` resolves to `security/template_secure.rs` or `security/mod.rs`'s
  re-export line only.
- `security/alerting.rs` (601 lines) — `Alert`, `AlertHandler`, `AlertManager`, `AlertSeverity`.
- `security/audit_trail.rs` (562 lines) — `AuditEntry`, `AuditError`, `AuditTrail` (this one),
  `MerkleProof`. Merkle-tree tamper-proof audit log, per `security/mod.rs`'s doc comment.
- `security/events.rs` (532 lines) — `AttackPattern`, `EventCategory`, `SecurityEvent`,
  `SecuritySeverity`.
- `security/intrusion_detection.rs` (659 lines) — `DetectionError`, `IntrusionDetector`,
  `PatternMatcher`, `RateLimiter`.
- `security/logging.rs` (622 lines) — `LoggingError`, `SecurityLogger`, `SecurityLoggerConfig`.
  This is the facade that wires the previous 5 modules together (per `security/mod.rs`'s own
  usage-example doc comment: `SecurityLogger::new()`, `.log(event)`, `.analyze_input(...)`,
  `.get_metrics_for_last_hour()`), i.e. these 6 files (3,761 lines: 601+562+532+659+622+784, minus
  template_secure's own separate 784 = 2,977 for the logging/alerting/audit/events/
  intrusion/metrics sextet alone, or 3,761 including template_secure) form one internally-coherent
  "Week 10 Security Logging & Intrusion Detection" subsystem that is fully wired to *itself* but
  has **no caller anywhere else in the workspace or in `ggen-core` outside this subsystem**.
  Confirmed by a per-symbol grep sweep (`AlertManager`, `AlertHandler`, `AuditTrail`,
  `SecurityEvent`, `IntrusionDetector`, `RateLimiter`, `SecurityLogger`, `MetricsCollector`,
  `SecurityMetrics`) — every hit outside `security/` is a same-name-different-type collision (see
  below), never a real import.
- `security/metrics.rs` (583 lines) — `MetricsCollector`, `MetricsError`, `SecurityMetrics`,
  `TimeWindow`.
- `security/tests.rs` (37 lines) — `#[cfg(test)] mod tests` wrapper, internal only, expected.

**Naming collisions found while checking the above (not real dependencies — flagging so nobody
mistakes a grep hit for a live caller):**

- `AuditTrail` is defined **three separate times** in `ggen-core` alone, all unrelated: (1)
  `security::audit_trail::AuditTrail` (Merkle-tree tamper-proofing, the one covered above), (2)
  `codegen/audit.rs:15` `pub struct AuditTrail` (own `AuditTrailBuilder` at line 95, different
  shape), (3) top-level `audit/mod.rs:12` `pub struct AuditTrail` (distinct from `domain::audit`
  per this repo's own module-naming convention, and now also distinct from `codegen::audit` — a
  third independent one). None of the three import from either of the others.
- `MetricsCollector` is defined **twice**: `security::metrics::MetricsCollector` (security-event
  metrics/`TimeWindow` aggregation) vs. top-level `metrics.rs:656` `pub struct MetricsCollector`
  (re-exported from `lib.rs:233` alongside `CodeMetrics`/`DefectMetrics`/`FlowMetrics`/
  `KaizenMetrics`/`MetricsReport` — a Lean-Six-Sigma quality-metrics concept, unrelated to
  security events).
- `GitHubStewardshipMembrane` (`crates/ggen-core/src/stpnt/github.rs:7,12`) — matches a
  case-insensitive `grep -i membrane` sweep but is unrelated to `src/membrane/`; it's a
  `stpnt`-module struct name that merely contains the substring "Membrane" ("stewardship
  membrane for the body", a metaphor in a GitHub-issue-templating string, `stpnt/github.rs:29`).
  Same coincidental-match pattern as `pki.rs`'s `project_config_path()` already noted in tasks.md.
  Confirmed `ggen-core`'s own `stpnt` module (`lib.rs:196`, `pub mod stpnt;`, 122 lines — this is
  the crate-internal module, **not** the removed sibling crate of the same name from the 2026-07
  consolidation pass; root `Cargo.toml` has zero literal `stpnt` references, only
  `genesis-core-v2`, confirming the two are unrelated namesakes) has zero external callers either
  (`grep -rn "ggen_core::stpnt"` workspace-wide: no matches) — not deeply audited since it's
  outside this cluster's assigned scope (security/pki/membrane), noted only because the membrane
  grep sweep surfaced it.
- "Generated Genesis membrane adapter" (`crates/ggen-core/src/parts_foundry/adapter_generator.rs:
  48,86,149,176`) is a **string literal emitted into generated output code**, not a real import of
  `src/membrane/` either — confirmed by reading the file; it's a comment string the generator
  writes into scaffolded projects.

### `membrane/` (917 lines, 5 files + mod.rs) — DEAD, and explicitly checked against the Process Intelligence Boundary (clean)

- Public surface (`membrane/mod.rs`): `GgenMembrane`, `GenesisCore`, `BoundaryCrossing`,
  `InterchangeablePart`, `VectorClock` (`core.rs`, 217 lines); `OcelEvent`, `OcelLog`,
  `OcelObject`, `OcelValue` (`ocel.rs`, 176 lines); `ProvDocument`, `ProvRelation` (`prov.rs`, 257
  lines); `RdfMembraneProjector` (`rdf.rs`, 162 lines); `MembraneShaclValidator` (`shacl.rs`, 84
  lines).
- **Zero external callers anywhere.** `grep -rn "ggen_core::membrane"` across the whole workspace
  (excluding `ggen-core`): zero matches.
- **Zero internal `ggen-core` self-consumers** outside the module's own files. `grep -rn
  "crate::membrane\|super::core::\|super::membrane"` inside `crates/ggen-core/src` (excluding
  `membrane/`): zero matches.
- **Has its own internal test**: `crates/ggen-core/tests/membrane_bindings_test.rs` (140 lines,
  `use ggen_core::membrane::{...}`, `#[test] fn test_membrane_bindings_and_boundary_crossings`) —
  a real `ggen-core`-crate-level integration test, genuinely exercises the module, but it is
  `ggen-core`'s own test (disappears with T055's crate deletion), not an external caller. Not the
  same file as `parts_manufacturing_e2e_test.rs`, whose 2 "membrane" hits
  (`assert!(code.contains("Generated Genesis membrane adapter"))`, lines 91 and 107) are testing
  `parts_foundry::adapter_generator`'s output string, unrelated to this module (see collision note
  above).
- **What it actually is**: a self-contained simulation of an "outer membrane" wrapping a "Genesis
  embedded core" that hosts WASM/AtomVM "interchangeable parts." `GgenMembrane::invoke()`
  (`membrane/core.rs:149-203`) is explicitly a stub — its own comment says "For a production
  system, this executes WASM / AtomVM code. Here, we provide a deterministic projection layer
  output based on input" (`core.rs:166-167`) — it concatenates strings rather than executing
  anything. This is a component-execution sandbox concept, unrelated to process-mining.
- **Process Intelligence Boundary check (explicitly requested by the brief)**: `grep -rniE
  "praxis_graphlaw|chatman|bcinr_powl|wasm4pm"` across all of `security/`, `pki.rs`, and
  `membrane/` combined: **zero matches**. `membrane/ocel.rs` builds an OCEL-shaped log purely by
  serializing existing `GgenMembrane` state (`OcelLog::from_membrane`, `ocel.rs:66-170`) and
  serializing it to JSON (`to_json`, `ocel.rs:173-175`) — no discovery, no conformance/fitness
  scoring, no DFG construction, no variant extraction; it is pure emission, consistent with
  CLAUDE.md's "ggen EMITS process evidence, ggen does NOT analyse it" rule. **Does not cross the
  boundary in the "ggen must not analyze" sense.**
- **However — it IS a local redefinition of OCEL types**, which CLAUDE.md's boundary table lists
  as forbidden in its own right (`| OCEL types (authority) | wasm4pm-compat::ocel::{OCEL,
  OCELEvent, OCELObject} | Local redefinition |`, forbidden column). `membrane/ocel.rs` defines its
  own `OcelEvent`/`OcelLog`/`OcelObject`/`OcelValue` (lines 16-64), structurally similar to but
  independent of the authoritative `wasm4pm-compat::ocel::{OCEL, OCELEvent, OCELObject}` types.
  This predates today's real emission surface (`ggen-graph/ocel/pack_events.rs`, per CLAUDE.md)
  and is entirely dead code with no callers, so it is not an *active* violation today — but it is
  exactly the shape of thing the guard-script's authors were worried about, just not yet caught by
  `guard-process-intelligence-boundary.sh` (that script only greps for
  `praxis_graphlaw::chatman`/`bcinr_powl(_receipt)::`, not for local OCEL-type redefinition —
  worth flagging to whoever owns T065/T066 as a possible guard-script gap, separate from this
  ticket's own scope). Recommended disposition: since it's dead and about to be deleted with all
  of `ggen-core` (T055), no action is required beyond noting it — do **not** port this module.
- `membrane/shacl.rs` references a real, existing file:
  `/Users/sac/ggen/.specify/shapes/membrane-boundary-crossing.ttl` (confirmed present, 63 lines,
  defines `mem:BoundaryCrossingShape` SHACL constraints for this module's own
  `BoundaryCrossing`/`GgenMembrane` concept) — the "boundary" in this TTL's name is the Genesis
  execution-boundary-crossing concept from `membrane/core.rs`, unrelated to the "Process
  Intelligence Boundary" from CLAUDE.md despite the shared word. Confirmed by reading both; not a
  fabricated path.

### Summary classification table

| Path | Lines | Verdict |
|---|---|---|
| `security/command.rs` | 326 | LOAD-BEARING (internal: `pipeline.rs:611`, `project_generator/mod.rs` x3 already tracked; external: `week4_security_hardening_tests.rs`) — duplicate-handling already in progress elsewhere, new call site flagged |
| `security/validation.rs` (`PathValidator` only) | 326 (shared file) | LOAD-BEARING for `PathValidator` (`template_types.rs`) + DUPLICATE-OF (3-way, see above); `EnvVarValidator`/`InputValidator` in the same file are DEAD |
| `security/error.rs` | 275 | LOAD-BEARING, external-only (`week4_security_hardening_tests.rs`, already tracked) |
| `security/template_secure.rs` | 784 | DEAD |
| `security/alerting.rs` | 601 | DEAD |
| `security/audit_trail.rs` | 562 | DEAD (+ naming collision w/ 2 unrelated `AuditTrail` types) |
| `security/events.rs` | 532 | DEAD |
| `security/intrusion_detection.rs` | 659 | DEAD |
| `security/logging.rs` | 622 | DEAD (facade over the above; whole subsystem unused) |
| `security/metrics.rs` | 583 | DEAD (+ naming collision w/ top-level `metrics.rs::MetricsCollector`) |
| `security/tests.rs`, `security/mod.rs` | 37 + 98 | internal wiring, not independently classifiable |
| `pki.rs` | 846 | DEAD but UNCERTAIN-BUT-ACTIONABLE — fully-tested trust-store code that answers T063's open key-management question; deleting via T055 without a port decision forecloses that option silently |
| `crates/ggen-marketplace/src/marketplace/pki.rs` | 455 | DUPLICATE-OF ggen-core's `pki.rs` (schema-incompatible), itself DEAD/orphaned (not declared as a module anywhere in its own crate) |
| `membrane/` (all 5 files + mod.rs) | 917 | DEAD, zero external/internal callers; explicitly checked clean against the Process Intelligence Boundary's "no analysis" rule; flagged as a local-OCEL-type-redefinition instance (dead, no action needed, but noted as a guard-script blind spot) |

**Net for this cluster**: of 7,168 lines, roughly 927 lines have any real caller today
(`command.rs` 326 + `validation.rs`'s `PathValidator` portion + `error.rs` 275, all already
routed through the concurrent SafeCommand/duplicate-handling work or already tracked as blocked
test files) — a decision is still needed on `pki.rs` (846 lines) given its direct relevance to
open task T063 — and the remaining ~5,400 lines (`template_secure.rs` through `metrics.rs`, plus
all of `membrane/`, plus the orphaned `ggen-marketplace` `pki.rs`) are safe to drop with T055 with
no porting required.

## security/pki/membrane cluster — independent re-audit, verification addendum

A second discovery pass was run against this exact cluster (`security/`, `pki.rs`, `membrane/`)
with the same brief. The section above was found already present and was independently
re-verified rather than duplicated: every specific, checkable claim in it was re-run and
confirmed byte-for-byte accurate, including exact line numbers —

- `lib.rs:290` re-export text (`pub use pki::{verify_ed25519, KeyPurpose, PkiManager,
  TrustedKeyEntry, TrustedKeysConfig};`) — confirmed verbatim.
- `pki.rs:286` `pub struct PkiManager` — confirmed.
- `grep -rn "ggen_core::pki"` workspace-wide outside `ggen-core` — confirmed zero hits.
- `crates/ggen-marketplace/src/marketplace/pki.rs` — confirmed present, confirmed **455 lines**
  exactly (`wc -l`), confirmed no `mod pki` declaration anywhere in `ggen-marketplace/src`.
- `pipeline.rs:611` (`fn execute_shell_hook`, `use crate::security::SafeCommand;`) — confirmed
  verbatim, including the preceding doc comment about command-injection prevention.
- `grep -rn "ggen_core::membrane"` workspace-wide outside `ggen-core` — confirmed the only hit is
  `crates/ggen-core/tests/membrane_bindings_test.rs:27`, i.e. `ggen-core`'s own test, not a true
  external caller.
- `template_types.rs:39,310,452` `PathValidator` usage — confirmed verbatim, all three lines.
- `week4_security_hardening_tests.rs:12-14,358` imports — confirmed verbatim.
- Three independent `pub struct AuditTrail` definitions
  (`security/audit_trail.rs:48`, `codegen/audit.rs:15`, `audit/mod.rs:12`) — confirmed, all three
  exist exactly as described.
- Two independent `pub struct MetricsCollector` definitions (`metrics.rs:656`,
  `security/metrics.rs:104`) — confirmed.
- Per-file line counts for the "DEAD sextet" (`alerting.rs` 601, `audit_trail.rs` 562, `events.rs`
  532, `intrusion_detection.rs` 659, `logging.rs` 622, `metrics.rs` 583 — 3,559 total) and for
  `command.rs`/`validation.rs`/`error.rs`/`tests.rs`/`mod.rs` (326/326/275/37/98) — all confirmed
  exact via independent `wc -l`.
- `guard-process-intelligence-boundary.sh` — confirmed it only greps for
  `praxis_graphlaw::chatman` and `bcinr_powl(_receipt)::` (lines 23, 27 of the script), i.e. it
  genuinely does not check for local OCEL-type redefinition — the "guard-script blind spot" claim
  about `membrane/ocel.rs` is accurate, not speculative.
- `.specify/shapes/membrane-boundary-crossing.ttl` — confirmed present, matches the description.
- Open task `T063` (`specs/014-ggen-core-replacement/tasks.md:1132`) — confirmed still `[ ]`
  (open), confirmed text matches the quoted "key-management policy" language.

**One genuine gap found and worth recording**: the original section's DEAD verdict for the
"logging/alerting/audit/events/intrusion/metrics sextet" (and separately for
`template_secure.rs`) states these have "no caller anywhere else in the workspace or in
`ggen-core` outside this subsystem" and correctly treats `membrane/`'s DEAD verdict as
compatible with `membrane_bindings_test.rs` existing (an internal `ggen-core`-crate-own
integration test, not a true external caller, disappearing with T055). The same pattern exists
for the "DEAD" security subsystem but was not called out:

- `crates/ggen-core/tests/security_logging_integration_tests.rs` (519 lines) —
  `use ggen_core::security::{alerting::{AlertManager, AlertSeverity, ConsoleAlertHandler,
  MemoryAlertHandler}, audit_trail::AuditTrail, events::{AttackPattern, EventCategory,
  SecurityEvent, SecuritySeverity}, intrusion_detection::IntrusionDetector,
  logging::{SecurityFeatures, SecurityLogger, SecurityLoggerConfig},
  metrics::{MetricsCollector, TimeWindow}};` (lines 42-49) — exercises every symbol in the
  "DEAD sextet" that the original section lists.
- `crates/ggen-core/tests/template_security_tests.rs` (613 lines) —
  `use ggen_core::security::{ContextEscaper, SecureTeraEnvironment, TemplateSandbox,
  TemplateValidator, MAX_TEMPLATE_SIZE};` (lines 50-52) — exercises every symbol in
  `template_secure.rs` that the original section lists as DEAD.

Neither file changes the DEAD verdict — both are `ggen-core`'s own crate-level integration tests
(under `crates/ggen-core/tests/`, not any other crate), both disappear along with the rest of
`ggen-core` under T055, and neither has any bearing on whether `ggen-cli`/`ggen-lsp`/root/
`ggen-engine` need this code ported. But omitting them from the DEAD writeup is inconsistent with
how the same category of evidence was already handled for `membrane_bindings_test.rs` one section
up — for completeness, and in case whoever executes T055 wants a full list of test files that die
with the crate (there are at least 3, not 1, for this cluster: `membrane_bindings_test.rs`,
`security_logging_integration_tests.rs`, `template_security_tests.rs`).

**No other discrepancies found.** No new load-bearing callers, no new duplicates, and no new
Process Intelligence Boundary concerns were discovered beyond what the original section already
reported. The original section's classifications, summary table, and net-line-count math all
stand as written.

## Sync-pipeline cluster: `codegen`/`codegen_lib`/`pipeline`/`pipeline_engine`/`sync` +
`register`/`registry`/`resolver`/`pack_resolver` (~26,000 lines)

**Scope**: the modules `/Users/sac/ggen/crates/ggen-cli/src/cmds/sync.rs`'s `SyncExecutor`/
`low_level_sync` calls need — `codegen` (12,164), `codegen_lib` (422), top-level `pipeline.rs`
(869), `pipeline_engine` (7,677), `sync` (1,372) = 22,504 lines — plus `register`/`registry`/
`resolver`/`pack_resolver` (1,131 + 976 + 277 + 1,082 = 3,466 lines), checked because
`pack_resolver.rs`'s own doc comment calls itself "μ₀: Pack Resolution Stage". Grand total
25,970 lines.

**Method**: `LSP workspaceSymbol` re-confirmed down this session (`GenerationPipeline` lookup on
`codegen/pipeline.rs:1:1` → identical `exceeded max crash recovery attempts (3)` error already
recorded elsewhere in this file). Fell back to exhaustive `grep`/`wc -l` sweeps: exact-symbol
references, `ggen_core::<module>::`/`crate::<module>::` path references, `Cargo.toml` dependency
edges, and direct reads of every file named in the brief plus their real call graphs (not
assumed from doc comments).

### 0. The critical finding, re-verified and sharpened

`ggen-cli/src/cmds/sync.rs:46-47` still reads:
```rust
use ggen_core::codegen::{OutputFormat, SyncExecutor, SyncOptions, SyncResult};
use ggen_core::sync::{sync as low_level_sync, SyncConfig, SyncLanguage};
```
`SyncExecutor::new(options).execute()` is called at line 437 (the `ggen.toml`-driven path,
`run_manifest_pipeline`); `low_level_sync(config)` is called at line 691 (the `--queries`-driven
path, `run_low_level_pipeline`, bypasses `ggen.toml` entirely). `ggen_core::domain::
sync_profile::validate_sync_preconditions` is also called at line 392 for `--profile`/`--locked`
(this one is `domain/` territory owned by the concurrent workflow; noted here only because it's a
third, real `sync.rs` dependency, not investigated further).

**Sharper than tasks.md's existing note**: `ggen-cli/Cargo.toml` has **zero** mention of
`ggen-engine` (`grep -n "ggen-engine" crates/ggen-cli/Cargo.toml` → no match) and
`grep -rl "ggen_engine" crates/ggen-cli/src` → no match, zero files. This isn't "the re-point
hasn't happened yet" — the dependency **edge itself does not exist**. Even a fully-complete,
fully-tested `ggen-engine::sync::sync()` sitting in the workspace today changes zero observable
behavior of the `ggen sync` command until (a) `ggen-engine` is added to `ggen-cli/Cargo.toml`'s
`[dependencies]` and (b) `cmds/sync.rs` is rewritten to call it. Workspace-wide,
`ggen_engine`/`ggen-engine` has real consumers in exactly one place: its own crate (`src/main.rs`,
`src/repl.rs`, its own tests) and root `/Users/sac/ggen/src/lib.rs`'s `pub use ggen_engine as
core;` (T051) — which T051 itself already confirmed has zero internal consumers in root `src/`
either. **`ggen-engine` is, today, dead code from every CLI entry point a user can actually run.**

### 1. What `ggen-core`'s real sync pipeline does (the two-pipeline structure)

`sync.rs` doesn't call one pipeline — it calls **two structurally unrelated ones**, selected by
whether `--queries` is supplied:

**Pipeline A — manifest-driven (`SyncExecutor::execute` → `codegen::pipeline::
GenerationPipeline::run`, the default/primary path, 2,076-line `codegen/pipeline.rs` +
1,248-line `codegen/executor.rs`)**. Real stage order (`pipeline.rs:1457` `run()`):
1. `load_ontology()` — reads `[ontology].source` + `.imports` as Turtle into an `oxigraph`-backed
   `Graph`.
2. `execute_inference_rules()` — `[[inference.rules]]`, sorted by `.order`, each a CONSTRUCT
   query materialized via `ConstructExecutor`; supports a `when:` SPARQL-ASK guard per rule;
   `strict_mode` turns a zero-triples CONSTRUCT into a hard error (`GGEN-INFER-001`), otherwise a
   warning.
3. `execute_shacl_validation()` — `[validation].shacl` shape files, `Error`-severity violations
   abort before any file is written.
4. `execute_validation_rules()` — `[[validation.rules]]` custom SPARQL ASK/SELECT, same
   abort-before-write semantics.
5. `execute_generation_rules()` (`pipeline.rs:726-1270`, rayon-parallelized across rules) — per
   rule: `when:` guard → load `QuerySource::{File,Inline,Pack}` → run SELECT → (unless
   `skip_empty`) load `TemplateSource::{File,Inline,Git,Package,Pack}` (Git clones a temp repo
   with `git clone --depth 1 [--branch]`; Package resolves `~/.ggen/packs/<pkg>/<version|latest>`;
   Pack resolves a `[[packs]]`-declared local path) → parse Hygen-style frontmatter
   (`template_types::Template::parse`, supports a `to:` override that itself is Tera-rendered) →
   render once (static `output_file`) or once per SPARQL row (dynamic `output_file`, itself
   Tera-rendered so `{{ name | lower }}`-style filters work in the path) → apply
   `GenerationMode::{Create,Overwrite,Merge}` (`Merge` uses `codegen::merge::merge_sections`'s
   `<<<<<<< GENERATED / ======= / >>>>>>> MANUAL` marker algorithm) → `validate_generated_output`
   (path-traversal + size-limit checks) → optional `check_no_unsafe` (`[validation].no_unsafe`) →
   write through `codegen::transaction::FileTransaction` (all rules render into the *same*
   transaction; one `tx.commit()` at the end — a mid-run failure rolls back every write from that
   run, not just the failing rule's).

Around this core, `SyncExecutor::execute`/`execute_full_sync` add machinery `ggen-engine` has
**none** of (see §3): `PreFlightValidator`, `DependencyValidator` (import-cycle detection),
`QualityGateRunner`/`AndonSignal` (typed, machine-parsable recovery-step errors), a 160-point
`MarketplaceValidator` FMEA pre-flight, `IncrementalCache` (`.ggen/cache`), `DriftDetector`
(non-blocking staleness warning against a `.ggen` state file), `ProofCarrier` (determinism
execution proof), and `codegen::audit::AuditTrailBuilder` (`--audit` → `audit.json` with real
per-input/per-output hashes and a `pipeline.*` OTEL-adjacent step log).

**Pipeline B — ontology-first, `--queries`-driven (`ggen_core::sync::sync`, 821-line
`sync/mod.rs`, entirely bypasses `ggen.toml`)**. A completely different generator: Stage 1 loads
one `.ttl` file directly (no manifest); Stage 2 runs every `*.rq` file in a directory in sorted
order with a 30s-per-query wall-clock timeout (`SPARQL_TIMEOUT_SECS`); Stage 3 dispatches to a
**native, hand-written, non-Tera** per-language code generator selected by `SyncLanguage::
{Go,Elixir,Rust,TypeScript,Python,Auto}` (`codegen/{go,elixir,python,typescript}.rs`, 524+376+
1164+320 = 2,384 lines of hardcoded struct/handler emission — not user-authored templates); Stage
4 runs WvdA soundness gates (`check_deadlock_freedom`/`check_liveness`/`check_boundedness`) on the
generated source text; Stage 4.5 is a **FATAL** gate — `ggen_graph::{CoherenceChecker,
CoherenceReport}`'s O≅A≅L three-pole isomorphism check blocks Stage 5 (file write) entirely on
failure; Stage 5 writes files and computes a `sha256(ontology || sorted file contents)` receipt;
Stage 5b writes `.ggen/receipts/coherence-latest.json`. This is reachable today from a real CLI
flag: `ggen sync --ontology <t> --queries <dir> --language go` (`sync.rs:660`'s own doc example).

`ggen-engine` has **zero** equivalent of Pipeline B anywhere: `grep -rl "SyncLanguage\|
generate_go\|generate_elixir\|generate_python\|CoherenceGate\|CoherenceChecker" crates/ggen-engine/
src/` → no matches. No `--queries` flag, no native per-language struct generators, no soundness
gates, no three-pole coherence check exists in the replacement engine at all.

### 2. What `ggen-engine`'s sync provides today (including in-flight, uncommitted work)

`crates/ggen-engine/src/sync.rs` (1,074 lines) implements a five-stage
Resolve→Enrich→Extract→Render→Write pipeline for a **frontmatter-per-template-file** convention
(`[templates].dir`, each `*.tmpl` carries its own `to:`/`sparql:`/`construct:`/`when:` block) —
structurally closer to Hygen than to `ggen.toml`'s `[[generation.rules]]` list. It has real
capabilities `ggen-core`'s pipeline lacks (pack-ontology `extra_ontologies` union, an N3/Datalog
`[law]` materialize+denial-check stage via `praxis-graphlaw`, a `determinism: true` per-template
self-check that actually re-renders and diffs rather than just asserting, a praxis-core
BLAKE3/chain `ReceiptRecord` written on every non-dry-run sync by default rather than opt-in).

**Observed live, mid-session**: as of this investigation, `crates/ggen-engine/src/
generation_rules.rs` (736 lines) exists **uncommitted** (`git status --porcelain` shows
`?? crates/ggen-engine/` — this whole crate is untracked in this branch) and is wired into
`sync.rs:139-167`'s `sync()` entry point via a Stage-0 schema dispatch: raw `ggen.toml` text is
sniffed for a non-empty `[[generation.rules]]` array before any typed parse; if present, `sync()`
delegates entirely to `generation_rules::run()` instead of the frontmatter path. This is
independent, concurrent work (the task list visible to this session shows a teammate actively on
"T068: Wire `[[generation.rules]]` into ggen-engine sync") — reported here as observed state, not
as this cluster's own output. Its own module doc comment is candid about scope: implements
`QuerySource::{File,Inline}` + `TemplateSource::{File,Inline}`, `when`/`skip_empty`,
per-row/static rendering, and `GenerationMode::{Create,Overwrite,Merge}` (the `Merge` marker
algorithm ported verbatim, unit-tested at `generation_rules.rs:669-698`); it explicitly, loudly
refuses (typed `[FM-GEN-6]`/`[FM-GEN-7]` errors naming the rule) rather than silently no-ops on
`QuerySource::Pack` and `TemplateSource::{Pack,Git,Package}`; and it explicitly does **not**
execute `[[inference.rules]]` at all (parses them as part of `GgenManifest` but never runs them),
nor `manifest.validation`/`manifest.law` for declarative-rules projects — all three documented as
deliberate, bounded gaps in the module's own header, not silent ones.

**Concrete, this-repo-real consequence of the inference-rules gap**: this repository's own
`/Users/sac/ggen/ggen.toml` (the file `ggen sync` run inside `/Users/sac/ggen` itself would read)
has exactly `1` `[[inference.rules]]` entry (`command-projection`, a CONSTRUCT materializing
`rdfs:comment` onto `?cmd a cli:Command` nodes) and `2` `[[generation.rules]]` entries
(`cli-commands-reference` → `crates/ggen-cli/src/generated_commands.rs`, `cli-proof-tests` →
`crates/ggen-cli/tests/generated/cli_proof_tests.rs`, both real, currently-compiled files in this
workspace). If `ggen-engine`'s `generation_rules::run()` were wired into `ggen-cli` today (it is
not — see §0) and pointed at this repo's own manifest, it would silently run only 2 of the 3
declared rules — the `command-projection` inference step never executes, so the two generation
SELECTs would run against the graph's raw asserted triples only, not the CONSTRUCT-enriched one.
Whether that changes the two generated files' actual bytes depends on whether the ontology's
`?cmd`/`rdfs:comment` triples are already asserted directly elsewhere (not verified in this
pass — this is a description of a real, checkable exposure, not a claim that output is currently
wrong; no `ggen sync` run was executed as part of this discovery-only audit).

### 3. Field-by-field `SyncOptions` capability gap

`ggen_core::codegen::executor::SyncOptions` (`executor.rs:103-139`) carries: `manifest_path`,
`output_dir`, `cache_dir`, `use_cache`, `flags: SyncFlags{ mode: ModeFlags{validate_only,
dry_run, watch}, behavior: BehaviorFlags{verbose, force, audit} }`, `output_format`,
`selected_rules` (the `--rule` filter), `a2a_stage`, `ontology_path` (override), `llm_service`
(DI trait for skill-implementation autogeneration), `timeout_ms` — 12 independently-settable
concerns.

`ggen_engine::sync::SyncOptions` (`sync.rs:56-63`) carries exactly: `dry_run: bool`, `engine:
EngineKind`. That is the **entire** options surface, for both the frontmatter path and (via the
same struct, unchanged) the new `generation_rules` path.

Concretely absent in `ggen-engine`, confirmed by grep (`grep -rln "AndonSignal\|
QualityGateRunner\|PreFlightValidator\|DependencyValidator\|MarketplaceValidator\|ProofCarrier\|
IncrementalCache\|DriftDetector\|AuditTrailBuilder\|FileTransaction" crates/ggen-engine/src/` →
no matches, all ten): no `--force` (both `Create`-mode-exists-skip and `Overwrite`-mode behavior
are unconditional on `GenerationMode`, not a global force flag), no `--audit` (the praxis-core
receipt is always-on and shaped differently — no `audit.json` with `pipeline.*` steps), no
`--verbose`, no `--watch` wired through `SyncOptions` (a *separate* `crate::watch::watch(root,
dry_run)` function exists, `watch.rs:50`, but is not reachable via `SyncOptions.mode.watch` and
is not exposed to `ggen-cli` at all — no caller anywhere outside `ggen-engine`'s own crate), no
`--rule`/`selected_rules` filter, no `--stage`/`a2a_stage`, no `--ontology` path override
independent of `[ontology].source`, no LLM-service injection point, no `--timeout`, and no
`--output-dir` override (`sync(root: &Path, ...)` takes only the project root; output paths are
always relative to it). `--format json` also has no equivalent — `SyncReport`/`SyncReceipt` are
`Serialize` but there's no `OutputFormat` selector or text/JSON dual-mode rendering the way
`codegen::executor::OutputFormat` provides.

### 4. Tera filter/function registry gap (confirms `register.rs` is the real sync-adjacent hit)

`crate::register::register_all(&mut tera_base)` is called directly at `codegen/pipeline.rs:750`
inside `execute_generation_rules` — **every** template `codegen::pipeline::GenerationPipeline`
renders gets this registry. Real external callers beyond the pipeline itself: 16
`ggen-core/tests/*.rs` files (`tera_filters_and_prefix_test.rs`, `pipeline_e2e_test.rs`, etc.),
top-level `ggen-core/src/pipeline.rs` (see §5), `template_types.rs`, `tera_env.rs`, and
`ggen-cli/tests/mcp_command_test.rs`.

Its filter/function set (`register.rs:103-217`), all confirmed present:
- Case filters: `camel`, `pascal`, `snake`, `kebab`, `class`, `title`, `sentence`, `train`,
  `shouty_snake`, `shouty_kebab`, `titlecase`, `param`, `constant`, `upper`, `lower`, `lcfirst`,
  `ucfirst`.
- Inflection filters: `pluralize`, `singularize`, `deconstantize`, `demodulize`, `ordinalize`,
  `deordinalize`, `foreign_key`.
- Misc: `pad_right`.
- SPARQL helper functions: `sparql_column`, `sparql_row`, `sparql_first`, `sparql_values`,
  `sparql_empty`, `sparql_count`.
- Native-code-emission filters: `schema_to_rust`, `schema_to_go`, `schema_to_elixir`,
  `schema_to_java`, `schema_to_typescript`.
- `bless_context()` — auto-derives a `Name` (PascalCase of `name`) and a `locals` alias for
  Hygen-template compatibility. **Not called from `codegen/pipeline.rs`'s own generation-rules
  path** (only from the separate top-level `pipeline.rs`, §5) — noted so as not to overclaim it
  as part of the `[[generation.rules]]` pipeline's actual behavior today.

`ggen_engine::template::build_tera` (`template.rs:218-244`) registers 9 filters (`snake_case`,
`pascal_case`, `camel_case`, `kebab_case`, `shouty_snake_case`, `title_case`, `pluralize`,
`singularize`, `hex_to_u64`) and 6 functions (`sparql`, `local`, `sparql_first`,
`sparql_values`, `sparql_empty`, `sparql_count`) — reused as-is by `generation_rules.rs` (`use
crate::template::build_tera`), so the new declarative-rules path inherits this same, narrower
set. Missing relative to `register.rs`: `class`, `sentence`, `train`, `shouty_kebab`, `titlecase`,
`param`, `constant`, `upper`, `lower`, `lcfirst`, `ucfirst`, `deconstantize`, `demodulize`,
`ordinalize`, `deordinalize`, `foreign_key`, `pad_right`, `sparql_column`, `sparql_row`, all 5
`schema_to_*` filters, and `bless_context`'s `Name`/`locals` auto-vars. A `ggen.toml` whose
templates use e.g. `{{ name | class }}` or `{{ result | schema_to_go }}` — filters real enough to
have their own doc-comment section in `register.rs` — would fail to render under `ggen-engine`
today with a Tera "filter not found" error, not a silent behavior change.

### 5. `register`/`registry`/`resolver`/`pack_resolver` classification

- **`register.rs` (1,131 lines) — LOAD-BEARING**, real caller `codegen/pipeline.rs:750` (see §4)
  plus the other citations above. This is the one module of the four the task brief's "sounds
  sync-pipeline-adjacent" hypothesis confirms.
- **`pack_resolver.rs` (1,082 lines, "μ₀: Pack Resolution Stage" per its own header) — DEAD from
  the real `ggen sync` CLI path.** `grep -rln "pack_resolver::" crates/ggen-core/src` → exactly 3
  files, all inside `pipeline_engine/` itself (`pipeline_engine/pipeline.rs`, `pipeline_engine/
  passes/{emission,extraction}.rs`) — **zero** references from `codegen/executor.rs`,
  `codegen/pipeline.rs`, or `sync/mod.rs`, the two real pipelines `sync.rs` actually calls. Its
  μ₀ "pack resolution before μ₁" concept is real but is only exercised by the `pipeline_engine`
  crate-internal implementation (§7), never by the pipeline `SyncExecutor` runs.
- **`registry.rs` (976 lines, `RegistryClient`/`RegistryIndex`/`ResolvedPack`) — DEAD from the
  real sync path.** Zero references from `codegen/`, `sync/mod.rs`, or `pipeline_engine/`. Its
  only outside-`ggen-core` callers are root `tests/cli.rs` and `tests/transport/
  registry_client_tests.rs`, both already flagged by tasks.md's T053 as gated behind
  `required-features = ["integration"]` (not compiled by default `cargo test`). Not a duplicate
  of `ggen-marketplace`'s registry concepts either — genuinely orphaned, not superseded.
- **`resolver.rs` (277 lines) — DEAD from the real sync path.** Its only references outside its
  own file are `pipeline_engine/passes/emission.rs` and one `ggen-core` test
  (`pack_template_integration_test.rs`). Not called from `codegen/` or `sync/mod.rs`.

None of `pack_resolver`/`registry`/`resolver` needs porting for `ggen sync` capability parity —
their only real consumer, `pipeline_engine`, is itself unreachable from the CLI (§7).

### 6. Two more dead/duplicate pipelines found while tracing this cluster (not in the brief's list)

- **`codegen_lib` (422 lines) — DEAD, confirmed with zero ambiguity.**
  `grep -rln "codegen_lib::" crates/ggen-core/src` (excluding its own directory) → **no matches**;
  same grep against `ggen-cli`/`ggen-lsp`/root → **no matches**. Not self-consumed, not
  externally consumed. Its `GenerationMode` (`codegen_lib/generation_mode.rs`) is a
  **DUPLICATE-OF** `manifest::GenerationMode` — the real pipeline uses the latter (`codegen/
  pipeline.rs:727`'s `use crate::manifest::{GenerationMode, QuerySource, TemplateSource};`), not
  `codegen_lib`'s copy. `Queryable`/`Renderable`/`Rule`/`GeneratedFile`/its own `Error` type are
  all likewise unused anywhere.
- **Top-level `pipeline.rs` (869 lines, `Pipeline`/`PipelineBuilder`) — DEAD from the real sync
  CLI path; a THIRD, independent pipeline implementation.** This is neither `codegen::pipeline::
  GenerationPipeline` nor `pipeline_engine::pipeline` — a separate Hygen-style
  parse-frontmatter→render→graph→body→plan-execution pipeline with its own SPARQL integration,
  prefix management, file injection, dry-run, and shell hooks, structurally overlapping (in
  ambition, not in code) with both of the other two. Its only real external callers are exactly
  the 3 files tasks.md's T053 section already grouped under the `Generator`/`Pipeline`/`Template`
  bucket and classified as blocked-with-no-destination and not gating `just test`:
  `tests/generator_core_tests.rs` (`use ggen_core::pipeline::Pipeline;`), `tests/
  template_systems_tests.rs` (same), `tests/tracing.rs` (`use ggen_core::pipeline::
  PipelineBuilder;`). Confirms, rather than adds to, that existing finding — flagged here because
  it is unambiguously part of "what would need porting for sync capability parity" (it wouldn't;
  it's dead) and the brief's module list didn't separately call it out from `codegen::pipeline`.

### 7. `pipeline_engine` (7,677 lines) — DEAD from the real `ggen sync` CLI path; test-only and
one narrow slice reachable elsewhere

`ggen-cli/src/cmds/sync.rs` never mentions `pipeline_engine` in any form, directly or
transitively (`SyncExecutor`/`GenerationPipeline`/`low_level_sync` call graphs traced above touch
none of it). Its real reachability, exhaustively:
- `crates/ggen-core/src/domain/utils/doctor.rs:311` — `use crate::pipeline_engine::vocabulary::
  VocabularyRegistry;`, one narrow submodule (381 of the 7,677 lines) consumed by `ggen doctor`
  (domain/utils territory, owned by the concurrent workflow — not investigated further here
  beyond confirming this one call site exists and is genuinely load-bearing for that command).
- `tests/contract/pipeline.rs` (root, part of the compiled `contract` test binary per tasks.md's
  T053 evidence — `mod pipeline;` is declared in `tests/contract/mod.rs`, confirmed) —
  `pipeline_engine::pipeline::{PipelineConfig, StagedPipeline}`, but only a construction smoke
  test (`StagedPipeline::new(config)`, asserts `Ok`, never calls `.run()` or equivalent) — real
  and currently passing, but proves initialization only, not the pipeline's actual behavior.
- 7 dedicated `ggen-core/tests/*.rs` files (`pack_sync_pipeline_e2e_test.rs`,
  `pipeline_embedded_ontologies_test.rs`, `pack_template_integration_test.rs`,
  `governance_e2e_test.rs`, `normalization_shacl_tests.rs`, `emission_determinism_test.rs`,
  `pipeline/canonicalization_tests.rs`) — real, deeper exercises of `pipeline_engine`'s passes
  (`normalization`/`extraction`/`emission`/`canonicalization`/`receipt_gen`), but these are
  `ggen-core`'s own crate-level tests, gone under T055 regardless, and none of them are invoked
  by anything `ggen-cli`/`ggen-lsp`/root call at runtime.

**Verdict**: `pipeline_engine` is a large (7,677-line), apparently-complete, independently
test-covered *second* implementation of the same "μ-stage code-generation pipeline" concept
`codegen::pipeline::GenerationPipeline` already implements — receipt generation
(`passes/receipt_gen.rs`), canonicalization, normalization, emission, guard/proof-gate concepts
of its own (`guard.rs`, `proof_gate.rs`, `epoch.rs`) — but it was never wired to the command any
user actually runs. It is DEAD for the purposes of "what does `ggen sync` need ported", with one
narrow, real exception (`vocabulary.rs` via `ggen doctor`) that belongs to the concurrent
workflow's `domain/` cluster, not this one.

### 8. Summary classification table

| Module | Lines | Verdict | Real caller (if any) |
|---|---|---|---|
| `codegen/executor.rs` + `codegen/pipeline.rs` | 1,248 + 2,076 | **LOAD-BEARING** | `ggen-cli/src/cmds/sync.rs:437` (`SyncExecutor`), the entire manifest-driven `ggen sync` |
| `codegen/{go,elixir,python,typescript}.rs` | 2,384 | **LOAD-BEARING** | `ggen_core::sync::sync`'s Stage 3, reachable via `ggen sync --queries` |
| `codegen/{merge,transaction,audit,merge_incremental_cache,...}.rs` (remaining ~6,500 lines of the 12,164) | ~6,500 | **LOAD-BEARING** (supporting the above) | same |
| `codegen_lib/` | 422 | **DEAD** | none; `GenerationMode` is DUPLICATE-OF `manifest::GenerationMode` |
| top-level `pipeline.rs` | 869 | **DEAD** (from CLI path); THIRD independent pipeline impl | 3 already-blocked root test files only (T053) |
| `pipeline_engine/` | 7,677 | **DEAD** (from CLI path) except `vocabulary.rs` (381 lines) via `ggen doctor` | test-only + `domain/utils/doctor.rs` |
| `sync/mod.rs` (+ `coherence_gate.rs`) | 821 + 265 | **LOAD-BEARING** | `ggen-cli/src/cmds/sync.rs:691` (`low_level_sync`), the `--queries` path |
| `register.rs` | 1,131 | **LOAD-BEARING** | `codegen/pipeline.rs:750`, every real generation-rule template render |
| `registry.rs` | 976 | **DEAD** (from sync path) | feature-gated (`integration`), off by default, root tests only |
| `resolver.rs` | 277 | **DEAD** (from sync path) | `pipeline_engine` internals + 1 ggen-core test |
| `pack_resolver.rs` | 1,082 | **DEAD** (from sync path) | `pipeline_engine` internals only |
| `ggen-engine` (whole crate, incl. in-flight `generation_rules.rs`) | 8,147 | **UNREACHABLE** from any live CLI command today | its own crate + unused root `pub use ... as core;` — zero `ggen-cli` consumers, no `Cargo.toml` dependency edge exists |

**Bottom line for whoever picks up T043's flagged gap**: `ggen-engine`'s sync — even accounting
for the in-flight `generation_rules.rs` work — is not a drop-in replacement for either of
`ggen-core`'s two real sync pipelines. Closing the gap requires, at minimum: (1) adding the
`ggen-cli → ggen-engine` dependency edge and rewriting `cmds/sync.rs` to call it (currently zero
lines of that exist); (2) deciding the fate of Pipeline B (`--queries`/`SyncLanguage`/native
per-language codegen/coherence-gate) — port it, or make it a documented, deliberate capability
drop; (3) closing the `SyncOptions` field gap (§3) or deciding which of the 12 fields are
deliberately dropped; (4) either porting `register.rs`'s missing 20 filters/functions into
`ggen_engine::template::build_tera` or accepting real templates using them will break; (5)
deciding whether `[[inference.rules]]`/`manifest.validation`/`manifest.law` execution is in scope
for the declarative-rules path, given this repo's own `ggen.toml` already declares an inference
rule that path does not run.

## Template-system cluster: `templates/`, `template_cache`, `template_types`, `tera_env`, `preprocessor`, `inject`, `streaming_generator`, `parallel_generator`, `generator`, `gpack`, `lockfile` (~9,318 lines)

**Scope** (line counts verified via `wc -l`, sum matches the ~9,300 estimate in the brief):
`crates/ggen-core/src/templates/` (3,990, 6 files + mod.rs), `template_cache.rs` (534),
`template_types.rs` (965), `tera_env.rs` (159), `preprocessor.rs` (558), `inject.rs` (485),
`streaming_generator.rs` (573), `parallel_generator.rs` (231), `generator.rs` (748, top-level —
distinct from `templates/generator.rs`), `gpack.rs` (567), `lockfile.rs` (508, top-level —
distinct from `packs/lockfile.rs`). 3990+534+965+159+558+485+573+231+748+567+508 = 9,318.

**Method**: `LSP workspaceSymbol` re-confirmed down this session (`Error performing
workspaceSymbol: LSP server 'plugin:rust-analyzer-lsp:rust-analyzer' exceeded max crash recovery
attempts (3)`, same failure already on record from the security/pki/membrane cluster and T068).
Fell back to exhaustive `grep`: per-symbol external references across `crates/{ggen-cli,ggen-lsp,
ggen-engine,ggen-config,ggen-marketplace,ggen-graph}/src`, root `{src,tests,examples,benches}`,
internal `crate::<module>::` self-consumption inside `ggen-core`, and cross-checked every
apparent "external caller" test file against `cargo metadata --format-version=1`'s real
`[[test]]`/`[[bench]]` target list (44 test binaries, 16 bench binaries) plus explicit `mod`
inclusion, since T053 already established most nested `tests/<dir>/*.rs` files are not compiled.
This distinction (real external reference vs. reference-from-a-file-that-never-compiles) drove
several classifications below — do not skip it when re-verifying.

### 1. `templates/` (3,990 lines, `mod.rs` + 6 files) — mostly DEAD, one load-bearing exception

`templates/mod.rs` re-exports: `BusinessLogicSeparator` (business_logic.rs), `TemplateContext`
(context.rs), `FileTreeTemplate`/`TemplateParser` (file_tree_generator.rs), `FileTreeNode`/
`NodeType`/`TemplateFormat` (format.rs), `FrozenMerger`/`FrozenParser`/`FrozenSection`
(frozen.rs), `generate_file_tree`/`FileTreeGenerator`/`GenerationResult`
(`templates::generator`, itself distinct from the load-bearing top-level `generator.rs` in item
9 below — two separate files both named `generator.rs` in this cluster alone).

- **`frozen.rs` (649 lines) — LOAD-BEARING.** `FrozenMerger::{has_frozen_sections,
  merge_with_frozen}` are called from top-level `generator.rs:408,410`
  (`if FrozenMerger::has_frozen_sections(&existing_content) { ... FrozenMerger::merge_with_frozen(...) }`),
  and `merge_with_frozen` itself internally calls `FrozenParser::extract_frozen_map`
  (`frozen.rs:364`), so `FrozenParser`/`FrozenSection` are transitively load-bearing too, not
  just re-exported dead weight. External: `tests/template_systems_tests.rs:14` — `use
  ggen_core::templates::frozen::FrozenMerger;` — confirmed a **real compiled test target**
  (`cargo metadata` lists `template_systems_tests
  /Users/sac/ggen/tests/template_systems_tests.rs` as a `[[test]]` binary). Mechanism: Tera-tag
  markers `{% frozen id="x" %}...{% endfrozen %}`, parsed then regex-substituted back into
  freshly-rendered output after the fact — preserves named sections across regeneration.
- **`business_logic.rs` (521), `context.rs` (672), `file_tree_generator.rs` (753), `format.rs`
  (709), `generator.rs` (618, i.e. `templates::generator`) — all DEAD.** Zero internal
  `ggen-core` self-consumers outside `templates/`'s own doc comments (`grep -rln
  "templates::(business_logic|context|file_tree_generator|format)"` across
  `crates/ggen-core/src` excluding `templates/` itself: zero hits for all four). The one
  internal consumer of `templates::generator::generate_file_tree` is
  `crates/ggen-core/src/domain/template/generate_tree.rs:46`
  (`crate::templates::generate_file_tree(template, context, output_dir)`) — but that file's own
  public re-exports (`domain/template/mod.rs:28,41`: `generate_file_tree`, `GenerateTreeInput`,
  `GenerateTreeOutput`, `generate_tree_run`) have **zero external callers anywhere**: `grep -rn
  "generate_tree_run\|GenerateTreeInput\|GenerateTreeOutput\|generate_file_tree"` across
  `crates/ggen-cli/src`, `crates/ggen-lsp/src`, `crates/ggen-engine/src` returns nothing. So the
  chain `templates::generator → domain::template::generate_tree → (nothing)` is dead
  end-to-end. (`domain::template` itself is the concurrent workflow's scope — flagging this
  specific dead-end for them, not re-auditing the rest of that module.)
  - The only apparent "external" references to this dead subsystem's types
    (`FileTreeGenerator`, `TemplateParser`, `TemplateContext`, `FileTreeTemplate`,
    `GenerationResult`) are not real: (a) `tests/integration/code_generation_tests.rs:204-223`
    references `FileTreeGenerator`/`TemplateParser` only inside `//` comments explicitly
    reading "DISABLED: FileTreeGenerator API not available" / "DISABLED: Use Template::parse()
    instead of TemplateParser" — dead code inside a real compiled test file (confirmed
    `code_generation_tests` is a genuine `[[test]]` target), but the references themselves are
    commented out, not live imports; (b) `tests/integration/template_tests/{mod.rs,
    test_template_generate_tree.rs,test_template_new.rs,test_template_list.rs,
    test_template_regenerate.rs}` reference `TemplateContext`/`FileTreeTemplate` in real
    (uncommented) code, but this whole directory is **not compiled**: `mod.rs` declares `mod
    test_template_generate_tree;` etc., but nothing anywhere (`grep -rln "mod template_tests"`
    across `tests/`) includes `tests/integration/template_tests/mod.rs` itself, and there is no
    `[[test]]` Cargo.toml entry for it (absent from `cargo metadata`'s 44-target list) — same
    "nested dir, not auto-discovered" pattern T053 already documented for 38 other files, just
    not individually named there. `(c)` `crates/ggen-cli/src/cmds/framework.rs:37`'s
    `TemplateContext` is its own unrelated locally-defined `struct TemplateContext { ... }`, not
    an import — a same-name collision, not a caller.

### 2. `template_cache.rs` (534 lines) — LOAD-BEARING, but only via a real *test-code* path, not the CLI

- Public surface: `TemplateCache` (struct, line 54), `CacheStats` (struct, line 394).
- **Internal caller**: `streaming_generator.rs:12` (`use crate::template_cache::TemplateCache;`)
  and `:293` (`pub fn cache_stats(&self) -> Result<crate::template_cache::CacheStats>`) — real
  usage, not just an unused import.
- `streaming_generator.rs` is itself heavily load-bearing (see item 7), so `template_cache.rs`
  is load-bearing transitively, entirely through test code — no CLI/production code path
  reaches it (confirmed: zero hits for `TemplateCache`/`CacheStats` in
  `crates/ggen-cli/src`, `crates/ggen-lsp/src`, `crates/ggen-engine/src`).
- The one apparent *direct* external caller, `tests/security/week4_security_hardening_tests.rs`
  (`use ggen_core::template_cache::TemplateCache;` at lines 25,36,48,64,76,90,447), is **not
  actually compiled** — absent from `cargo metadata`'s 44 `[[test]]` targets, no Cargo.toml
  entry for `week4_security_hardening_tests`, nested under `tests/security/` (not
  auto-discovered). This file is also where the security/pki/membrane cluster's audit already
  found `SafeCommand`/`PathValidator`/`ErrorSanitizer` "external callers" — worth noting to
  whoever reads that section: none of week4's imports are live compiled code today.
- **Naming-collision false positives, not real dependencies**: `CacheStats` also appears in
  `crates/ggen-marketplace/src/{marketplace/registry.rs,marketplace/cache.rs,
  marketplace/rdf/control.rs,packs_registry/cloud_distribution.rs,
  packs_registry/sparql_executor.rs}` — all independently-defined `CacheStats` types native to
  `ggen-marketplace`, not importing `ggen_core::template_cache`. Also
  `examples/factory-paas/world/world/src/routing.rs:140` defines its own local `struct
  CacheStats` (and `examples/factory-paas/templates/rust_routing_resolver.rs.tera:43,124-125,140`
  is a `.tera` *template* whose rendered *output* happens to emit a same-named
  struct/enum — not a real Rust import at all). `examples/factory-paas` is not a workspace
  member (absent from root `Cargo.toml`'s `[workspace] members`).

### 3. `template_types.rs` (965 lines) — LOAD-BEARING (multiply), and a near field-for-field DUPLICATE-OF `ggen-engine::template::{Frontmatter,Template}`

- Public surface: `FrontmatterFlags`, `Frontmatter`, `Template` (with `Template::parse`,
  `FromStr` impl).
- **Direct internal caller**: top-level `generator.rs:75,309` — `use
  crate::template_types::Template;` then `Template::parse(&input)?` — the real entry point of
  `Generator::generate()`'s pipeline (see item 9; `generator.rs` is load-bearing via 2 real
  compiled external test targets).
- **Second, independent load path via a facade module technically outside this cluster's
  assigned file list**: `crates/ggen-core/src/template/mod.rs` (131 lines, singular
  `template`, distinct from both `template_types.rs` and the plural `templates/` — flagged as
  "unexamined, distinct" in the brief's own module inventory) is nothing but `pub use
  crate::template_types::{Frontmatter, Template};` plus an unrelated `template_validation`
  submodule. Two **real compiled external test targets** consume it directly: `tests/tracing.rs:
  59,213` (`ggen_core::template::Frontmatter::default()`) and
  `tests/template_systems_tests.rs:13` (`use ggen_core::template::Template;`) — both confirmed
  present in `cargo metadata`'s `[[test]]` list (`tracing`, `template_systems_tests`). Zero other
  workspace-wide hits for `ggen_core::template::` (singular) exist beyond these two files.
  Flagging this facade module explicitly since it is the thing that makes `template_types.rs`
  provably load-bearing through *two* independent real paths, not one — whoever owns porting
  `template_types.rs` needs to also account for this 131-line re-export shim, which nothing in
  `tasks.md` currently assigns to either this cluster or the concurrent `domain/template`
  cluster.
- **Depends on** (own field/type usage, not just doc-comment mentions): `security::validation::
  PathValidator` (`template_types.rs:39,310,452` — already flagged load-bearing by the
  security/pki/membrane cluster's audit for exactly this call site) and `crate::preprocessor::
  {FreezePolicy, FreezeStage, PrepCtx, Preprocessor}` (item 4 below, real usage at lines
  201,204,218,221,225,232,851, not unused imports).
- **DUPLICATE-OF `ggen-engine::template::{Frontmatter, Template}` — near field-for-field.**
  `ggen-engine/src/template.rs`'s `Frontmatter` (its own doc comment: "produces a Tera
  environment with a `sparql(query="…")` function... `[Frontmatter]` uses
  `deny_unknown_fields`") has: `to`, `sparql: BTreeMap<String,String>`, `construct`, `inject`,
  `before`, `after`, `at_line`, `skip_if`, `unless_exists`, `force`, `when`, `skip_empty`,
  `from`, `sh_before`(`alias="sh"`), `sh_after`, `backup`, `shape: Vec<String>`, `determinism:
  Option<bool>`, `freeze_policy: Option<FreezePolicy>`, `freeze_slots_dir`. `ggen-core`'s
  `template_types::Frontmatter` has: `to`, `from`, `flags: FrontmatterFlags` (force,
  unless_exists, inject, prepend, append, eof_last, idempotent), `before`, `after`, `at_line`,
  `skip_if`, `sh_before` (`alias="sh"`), `sh_after`, `base`, `prefixes`, `rdf_inline`, `rdf`,
  `sparql: BTreeMap<String,String>`, `backup: Option<bool>`, `shape: Vec<String>`,
  `determinism: Option<serde_yaml::Value>`, `freeze_policy: Option<String>`,
  `freeze_slots_dir`, `sparql_results` (populated, `#[serde(skip)]`). 13 of `ggen-engine`'s 19
  fields have an exact-name counterpart in `ggen-core`'s 20 fields
  (`to,from,before,after,at_line,skip_if,sh_before,sh_after,backup,shape,determinism,
  freeze_policy,freeze_slots_dir,sparql`) — this is evidence of a deliberate rewrite, not
  coincidence. Real differences: `ggen-engine` adds `construct`/`when`/`skip_empty` (not present
  in `ggen-core`'s version) and types `freeze_policy` as a real enum vs. `ggen-core`'s untyped
  `Option<String>`; `ggen-core` additionally carries `rdf_inline`/`rdf`/`base`/`prefixes` (RDF
  loadable from the frontmatter itself) and `FrontmatterFlags`'s `prepend`/`append`/`eof_last`/
  `idempotent`, none of which exist in `ggen-engine::template::Frontmatter` at all — a real
  capability gap if `template_types.rs` is deleted without checking whether any live template
  actually uses those 4 RDF fields or those 4 extra flags.

### 4. `preprocessor.rs` (558 lines) — LOAD-BEARING internal-only; third independent "freeze" mechanism in this codebase

- Public surface: `PrepCtx`, `Stage` (trait), `FreezePolicy` (enum: `Always`/`Checksum`/
  `Never`), `FreezeStage`, `IncludeStage`, `Preprocessor`.
- **Sole internal consumer**: `template_types.rs` (see item 3) — real calls to
  `Preprocessor::with_default_stages()` (`:201`), `Preprocessor::new().with(FreezeStage{...})`
  (`:221-225`), `PrepCtx{...}` construction (`:204,232`), and `FreezePolicy::Always` (`:851`,
  inside a test). **Zero external callers anywhere** for `Preprocessor`/`PrepCtx`/`FreezeStage`
  workspace-wide.
- **Naming/design collision — flag for whoever reconciles the freeze story**: this file defines
  its *own* `FreezePolicy` enum with the *same three variant names*
  (`Always`/`Checksum`/`Never`) as `ggen-engine::template::FreezePolicy` (item 3), but it is a
  **structurally and semantically different type** with a different mechanism: `preprocessor`'s
  `FreezeStage` is a *pre-render pipeline stage* that reads cached slot content from a
  `slots_dir` and processes `{% startfreeze %}...{% endfreeze %}` blocks (different tag names
  from `templates::frozen`'s `{% frozen id=".." %}...{% endfrozen %}`, item 1) — three
  independently-implemented "freeze" concepts coexist in scope for this audit alone:
  (a) `templates::frozen::FrozenMerger` (Tera-tag, post-render regex substitution, load-bearing,
  item 1), (b) `preprocessor::FreezeStage`/`FreezePolicy` (Tera-tag, pre-render cache-slot read,
  load-bearing-but-internal-only, this item), (c) `ggen-engine::template::FreezePolicy`
  (whole-file, post-write BLAKE3 checksum, the new system). None of the three share code or a
  common trait; deleting `ggen-core` silently drops (a) and (b) in favor of (c)'s different
  semantics unless someone verifies no real `.tmpl`/`gpack` template in this repo or its
  templates directory actually relies on `{% frozen %}` or `{% startfreeze %}` syntax.

### 5. `tera_env.rs` (159 lines) — DEAD, chain confirmed dead end-to-end

- Public surface: `build_tera_with_glob`, `build_tera_minimal`. Not re-exported at `lib.rs` top
  level (`pub mod tera_env;` only, no `pub use`).
- Zero external callers anywhere (`build_tera_with_glob`/`build_tera_minimal`: zero hits outside
  `ggen-core`). Sole internal caller: `crates/ggen-core/src/domain/template/render_with_rdf.rs`
  (concurrent workflow's scope — not re-audited here beyond this one dependency check).
  `render_with_rdf`'s own re-export (`domain/template/mod.rs:37`, `pub use
  render_with_rdf::*;`) has exactly 2 external references, `tests/chicago_tdd/
  ontology_driven_e2e.rs` and `tests/integration/clap_noun_verb_ontology_test.rs` — and T053's
  own accounting already names `tests/integration/clap_noun_verb_ontology_test.rs` explicitly as
  one of the "39 total" files confirmed **not gating any build** (T053: "the other 38, plus
  tests/integration/clap_noun_verb_ontology_test.rs (39 total)... not referenced by any `[[test]]`
  path, not auto-discovered"). `tests/chicago_tdd/ontology_driven_e2e.rs` is likewise absent from
  `cargo metadata`'s 44-target list (nested under `tests/chicago_tdd/`, no explicit entry). So
  `tera_env → render_with_rdf → {2 uncompiled test files}` is dead at every hop — a clean,
  fully-verified DEAD verdict, not just "no caller found yet."

### 6. `inject.rs` (485 lines) — LOAD-BEARING internal-only; partial (not exact) functional overlap with `ggen-engine::write.rs`

- Public surface: `EolNormalizer`, `SkipIfGenerator`.
- **Sole internal caller**: top-level `pipeline.rs:432` — `use crate::inject::{EolNormalizer,
  SkipIfGenerator};` inside `PipelineBuilder::apply_injection()`, the real branch taken when
  `frontmatter.flags.inject` is set (`pipeline.rs:421-423`). `pipeline.rs`'s own
  `Pipeline`/`PipelineBuilder` types are load-bearing via 3 confirmed-real compiled test targets
  (`tests/tracing.rs`, `tests/template_systems_tests.rs`, `tests/generator_core_tests.rs`, all
  present in `cargo metadata`'s list; this reconfirms, from this cluster's angle, the same
  `pipeline.rs` load-bearing finding the security/pki/membrane cluster's audit made from
  `SafeCommand`'s angle). Zero direct external callers of `ggen_core::inject` anywhere.
- **Not an exact duplicate of `ggen-engine::write.rs`'s injection logic**: `write.rs::
  inject_into` (`write.rs:330-357`) covers `before`/`after`/`at_line`/append-by-default marker
  placement — the same marker semantics `inject.rs`'s caller (`pipeline.rs`) also implements
  itself inline (not via `inject.rs`). What `inject.rs` specifically contributes that
  `write.rs` does not have: `EolNormalizer::{detect_eol, normalize_to_match_file}` (preserve a
  target file's existing CRLF/LF style across injection) and
  `SkipIfGenerator::generate_exact_match` (regex-pattern generation for idempotency checks,
  distinct from `ggen-engine::template::Frontmatter.skip_if`'s plain substring check per its own
  doc comment: "Skip the write when the existing file already contains this substring"). A real,
  narrow capability gap if ported naively as "already covered."

### 7. `streaming_generator.rs` (573 lines) — LOAD-BEARING, the strongest-evidenced module in this cluster

- Public surface: `StreamingGenerator`, `GenerationResult`.
- **`tests/generator_core_tests.rs`** (confirmed real `[[test]]` target via `cargo metadata`)
  constructs `StreamingGenerator::new(...)`/`StreamingGenerator::with_cache_capacity(...)` at
  **25 separate call sites** (lines 574,590,617,654,682,714,761,780,854,872,901,934,967,998,
  1031,1067,1098,1145,1174,1274,1368,1430,1533,1591) and imports `GenerationResult` alongside
  it. This is by far the most heavily test-exercised symbol found in this cluster. Zero
  production (`ggen-cli`/`ggen-lsp`/`ggen-engine`) callers, same pattern as the rest of this
  cluster — this is real, substantial, currently-passing test coverage of code with no CLI path
  to it today.
- Depends on `template_cache::{TemplateCache, CacheStats}` (item 2), making that module's
  load-bearing status derive entirely from this one test file.

### 8. `parallel_generator.rs` (231 lines) — DEAD, cleanest verdict in this cluster

- Public surface: `ParallelGenerator` (unit struct, `generate_all` per its own doc example).
  `grep -rln "ParallelGenerator" .` (excluding `/target/`) across the **entire workspace**
  returns exactly one file: `crates/ggen-core/src/parallel_generator.rs` itself. Not re-exported
  at `lib.rs` (`pub mod parallel_generator;` only). No internal callers, no external callers, no
  test references anywhere, not even a dead/disabled one.

### 9. `generator.rs` (748 lines, top-level) — LOAD-BEARING, already tracked at bucket level by T053; this pass adds the specific in-cluster dependency chain

- Public surface: `GenContext`, `Generator`.
- External: `tests/generator_core_tests.rs:22` (`use ggen_core::generator::{GenContext,
  Generator};`) and `tests/integration/code_generation_tests.rs:24` (`use ggen_core::{GenContext,
  Generator, Pipeline, Template};`) — both confirmed real `[[test]]` targets. (`tests/
  integration/test_determinism.rs` and `tests/integration/packs/pack_e2e_workflows_test.rs` also
  reference `GenContext`/`Generator`/`Pipeline` but are **not** in `cargo metadata`'s 44-target
  list — consistent with T053's "50 blocked, mostly uncompiled" finding; not counted as live
  evidence here.)
- T053 already tracks this symbol bucket generically ("`GenContext`/`Generator`/`Pipeline`/
  `Template`/`Templates`/`pipeline`"); this pass's specific addition: `generator.rs`'s own real
  dependencies *within this cluster* are `templates::frozen::FrozenMerger` (item 1, frozen-
  section merge on regeneration, `generator.rs:408-410`) and `template_types::Template::parse`
  (item 3, `generator.rs:75,309`) — anyone porting `Generator` needs both, not just the type
  itself.

### 10. `gpack.rs` (567 lines) — DEAD, confirmed all the way down; directly answers the brief's question 2

- Public surface: `PackConventions`, `GpackManifest`, `GpackMetadata`, `TemplatesConfig`,
  `MacrosConfig`, `RdfConfig`, `QueriesConfig`, `ShapesConfig`, `PresetConfig` — the *old*
  `gpack.toml` manifest parser + file-discovery-convention system (`templates/**/*.tmpl`,
  `templates/**/graphs/*.ttl`, etc.), unrelated to the newer `domain::packs`/`packs::lockfile`
  pack-installation system already ported via T024-T026.
- **Internal callers (2, both themselves dead)**: `cache.rs:94` (`pub manifest:
  Option<crate::gpack::GpackManifest>` on `CachedPack`) and `ontology_pack.rs:41` (`pub base:
  crate::gpack::GpackMetadata` on an `OntologyPackMetadata` struct). Checked both transitively:
  `cache::{CacheManager, CachedPack}` — zero external callers anywhere (`grep -rn
  "ggen_core::cache"` workspace-wide: 0 hits). `ontology_pack::{OntologyPackMetadata,
  OntologyConfig, ...}` — zero external callers; the only grep hits for `OntologyConfig` outside
  `ggen-core` all resolve to `ggen-config`'s own, completely independent, already-ported
  `OntologyConfig` type (`crates/ggen-config/src/{config/ontology_config.rs:17,
  manifest/types.rs:279}` — a naming collision, not a shared type, same pattern as the
  security cluster's multiple `AuditTrail`/`PathValidator` collisions). So the chain `gpack.rs →
  {cache.rs, ontology_pack.rs} → (nothing)` terminates dead at both branches.
- **Directly answers the brief's question**: "does any blocked test actually need `gpack`?" T053
  named 7 files under the `gpack` bucket: `tests/unit/packs/{gpack_manifest_test,
  pack_edge_cases_test,pack_validation_test}.rs`, `tests/integration/packs/{
  pack_cli_integration_test,pack_e2e_workflows_test}.rs`, `tests/performance/packs/
  pack_benchmarks.rs`, `tests/performance/packs_performance_test.rs`. **None of the 7 are
  compiled today**: zero Cargo.toml `[[test]]`/`[[bench]]` entries for any of the 7 names
  (checked individually); `tests/unit/packs/mod.rs` and `tests/integration/packs/mod.rs` both
  declare their respective files as `mod` children, but nothing anywhere references
  `tests/unit/packs/mod.rs` or `tests/integration/packs/mod.rs` as a module from a compiled
  entry point either (`grep -rln "mod packs"` under `tests/unit/`, `tests/integration/` outside
  their own `mod.rs`: 0 hits); `tests/performance/` has no `[[bench]]`/`[[test]]` entry at all.
  **Answer: no, no blocked test actually needs `gpack.rs` in a live sense — all 7 are dead code
  themselves**, same as the 38-uncompiled-files pattern T053 already established generally, now
  confirmed specifically for this bucket.
- **Adjacent, orphaned on-disk artifact, not a code dependency**: real `gpack.toml` files exist
  at `templates/cleanroom/gpack.toml`, `templates/cli/subcommand/gpack.toml`,
  `templates/cli/noun-verb-cli/gpack.toml`, and 3 more under `tests/fixtures/packs/*/gpack.toml`
  — but `grep -rln "gpack.toml"` across `crates/ggen-marketplace/src`, `crates/ggen-cli/src`,
  `crates/ggen-engine/src` returns zero hits, meaning **no live code path reads these files
  either** — they are orphaned fixtures/templates, not evidence of a hidden `gpack.rs` consumer.

### 11. `lockfile.rs` (508 lines, top-level) — DEAD; DUPLICATE-OF `packs::lockfile::PackLockfile` (schema-incompatible, same target file); plus a dead unification attempt and two more unrelated same-named types

Directly answers the brief's question 1 ("is top-level `lockfile` a DUPLICATE of
`packs::lockfile`?"): **yes, conceptually — same target file, incompatible schema — but unlike
`packs::lockfile` (already ported, T026), top-level `lockfile.rs` has zero real callers today.**

- Public surface: `Lockfile` (line 108), `LockfileEntry` (131), `BundleExpansion` (158),
  `RegistrySource` (169), `ProfileRef` (182). Its own doc comment describes the JSON schema it
  targets: `.ggen/packs.lock` as `{version, packs: [{pack_id, version, source, digest,
  signature, trust_tier, dependencies}], bundles: [{bundle_id, expanded_to}], profile:
  {profile_id, runtime_constraints, trust_requirement}, digest, signature}` — **array-of-entries**
  keyed by `pack_id`, with per-pack Ed25519 `signature` + SHA-256 `digest` + `trust_tier` +
  bundle-expansion tracking + whole-lockfile digest/signature.
- **`packs/lockfile.rs`** (788 lines, already ported to `crates/ggen-marketplace/src/packs/
  lockfile.rs` per tasks.md T026) targets the **same file**, `.ggen/packs.lock`, but as
  `PackLockfile { packs: BTreeMap<String, LockedPack>, updated_at, ggen_version, profile }` —
  **map-keyed-by-pack-id**, with a single `integrity` checksum string per pack (no
  digest/signature split, no bundle-expansion concept, no per-pack trust tier). Two
  independently-designed, field-incompatible schemas targeting the identical on-disk path — the
  same "two schemas, one filename" pattern the security/pki/membrane cluster's audit already
  documented for `pki.rs` vs. `ggen-marketplace/src/marketplace/pki.rs`.
- **Zero real callers of the top-level version, internal or external, confirmed multiple ways**:
  - `lib.rs:282` re-exports `pub use lockfile::Lockfile;`, but `grep -rn "ggen_core::Lockfile\b\|
    ggen_core::lockfile::"` across the entire workspace outside `ggen-core`: **0 hits**.
  - Internal: `grep -rn "crate::lockfile::"` across `crates/ggen-core/src` outside
    `lockfile.rs`/`packs/lockfile.rs` itself returns exactly one file, `lockfile_unified/
    entry.rs:259-260` (`impl From<crate::lockfile::LockEntry> for UnifiedLockEntry`) — but this
    reference **does not even resolve**: `grep -n "LockEntry" crates/ggen-core/src/lockfile.rs`
    returns nothing — `lockfile.rs` has no `LockEntry` type at all (only `Lockfile`,
    `LockfileEntry`, `BundleExpansion`, `RegistrySource`, `ProfileRef`). This code would not
    compile if it were ever wired in.
  - And it never is: **`lockfile_unified/` (1,696 lines across 6 files — `mod.rs` (63),
    `cache.rs` (435), `entry.rs` (364), `format.rs` (174), `traits.rs` (164),
    `validation.rs` (496)) is not declared as a module anywhere `lib.rs` or any other reachable
    file can see it** — `grep -rn "lockfile_unified"` across all of `crates/ggen-core/src`
    outside its own directory: **0 hits**, no `pub mod lockfile_unified;`, no `mod
    lockfile_unified;`, no `#[path]` redirect. `cargo build` never compiles this directory at
    all — confirmed structurally (module-declaration absence is definitive in Rust; no build
    run was needed to establish this). Its own `mod.rs` doc comment self-describes as
    "unifying the three existing lockfile implementations into a coherent trait-based
    architecture" with `PqcSignable`/post-quantum-signature support and a `UnifiedLockfileManager`
    — a whole abandoned architecture-unification effort, dead on arrival, containing a **fourth**
    same-named thing (`traits::Lockfile`, a trait this time, re-exported at
    `lockfile_unified/mod.rs:62`) that is also never reachable.
- **A third, structurally unrelated `Lockfile` exists in `ggen-marketplace`**:
  `crates/ggen-marketplace/src/marketplace/install.rs:1302` — `pub struct Lockfile { version:
  u32, manifest_id: uuid::Uuid, packages: indexmap::IndexMap<PackageId, PackageVersion>,
  created_at }`, with its own `Lockfile::from_manifest` constructor, used internally at
  `install.rs:921,1740`. No `pack_id`/`digest`/`signature`/`trust_tier`/`bundles`/`profile`
  fields — shares nothing with either `ggen-core` schema beyond the name. Also,
  `BundleExpansion` is independently redefined a second time at
  `crates/ggen-marketplace/src/marketplace/composition_receipt.rs:39` (`pub struct
  BundleExpansion { ... }`, used by that file's own `CompositionReceipt`/
  `add_bundle_expansion`) — unrelated to `lockfile.rs:158`'s `BundleExpansion` beyond the name.
  Net: **"Lockfile" and "BundleExpansion" are each independently defined at least 3-4 times**
  across `ggen-core` + `ggen-marketplace` combined (top-level `lockfile.rs`, `packs::lockfile`
  [ported], the dead `lockfile_unified` trait, and `ggen-marketplace::marketplace::install`'s
  own native struct) — whoever finishes T055 (delete `ggen-core`) should not assume "Lockfile
  exists in ggen-marketplace already" settles which schema a given call site needs; it must be
  checked per call site.

### Ontology-adjacent finding relevant to `template_types.rs` and `ggen-engine::template`: RDF-in-frontmatter fields have no confirmed replacement

`template_types::Frontmatter`'s `rdf_inline: Vec<String>` and `rdf: Vec<String>` fields (item 3)
let a template embed or reference RDF triples directly in its own YAML frontmatter.
`ggen-engine::template::Frontmatter` (item 3's comparison) has no equivalent field at all — its
RDF story is entirely `sparql`/`construct` queries against a graph assembled elsewhere (per
`ggen-engine/src/template.rs`'s own doc comment, "a `sparql(query="…")` function bound to a
`DeterministicGraph`"). This wasn't in this cluster's assigned scope to resolve (RDF/graph
plumbing is the concurrent workflow's territory), but since it surfaced directly while comparing
Frontmatter shapes, flagging it here: if any real `.tmpl`/gpack template in this repo uses
`rdf_inline:`/`rdf:` in its frontmatter, porting away from `template_types.rs` silently drops
that capability rather than replacing it. Did not search the full templates tree for real usage
of these two fields — out of this pass's time budget, noting as a gap for whoever owns the
final cutover.

### Summary classification table

| Path | Lines | Verdict |
|---|---|---|
| `templates/frozen.rs` | 649 | LOAD-BEARING — `generator.rs:408-410` (internal) + `tests/template_systems_tests.rs` (external, real compiled target) |
| `templates/{business_logic,context,file_tree_generator,format,generator}.rs` | 521+672+753+709+618=3,273 | DEAD — reachable only via a dead internal chain through `domain::template::generate_tree` (zero external callers); apparent external hits are commented-out code or uncompiled test files |
| `templates/mod.rs` | 68 | wiring only |
| `template_cache.rs` | 534 | LOAD-BEARING internal-only, via `streaming_generator.rs` → real compiled `generator_core_tests.rs`; its one apparent direct external caller (`week4_security_hardening_tests.rs`) is itself uncompiled |
| `template_types.rs` | 965 | LOAD-BEARING (2 independent real paths: `generator.rs` directly, and the `template/` facade module via `tests/tracing.rs`+`tests/template_systems_tests.rs`) — also DUPLICATE-OF `ggen-engine::template::{Frontmatter,Template}` (13/19-20 fields match by name), with a real capability gap (`rdf_inline`/`rdf`/2 extra flags) not present in the new version |
| `preprocessor.rs` | 558 | LOAD-BEARING internal-only (`template_types.rs`'s real usage) — third independently-shaped "freeze" mechanism alongside `templates::frozen` and `ggen-engine::template::FreezePolicy` |
| `tera_env.rs` | 159 | DEAD — chain confirmed dead end-to-end through `domain::template::render_with_rdf` (both its external callers are uncompiled, one already named in T053's own 39-file list) |
| `inject.rs` | 485 | LOAD-BEARING internal-only (`pipeline.rs:432`, real production branch) — partial, not exact, overlap with `ggen-engine::write.rs`'s injection (missing EOL normalization + regex skip-if generation) |
| `streaming_generator.rs` | 573 | LOAD-BEARING — 25 real call sites in `tests/generator_core_tests.rs`, the single strongest-evidenced module in this cluster |
| `parallel_generator.rs` | 231 | DEAD — zero references anywhere in the entire workspace outside its own file |
| `generator.rs` (top-level) | 748 | LOAD-BEARING — already tracked at bucket level by T053; this pass adds its real in-cluster dependencies (`templates::frozen::FrozenMerger`, `template_types::Template::parse`) |
| `gpack.rs` | 567 | DEAD, confirmed all the way down (2 internal consumers, both dead; all 7 of T053's "gpack-blocked" test files confirmed uncompiled) |
| `lockfile.rs` (top-level) | 508 | DEAD; DUPLICATE-OF `packs::lockfile::PackLockfile` (schema-incompatible, same target file, already ported via T026); its only apparent internal consumer, `lockfile_unified/` (1,696 lines, 6 files), is itself entirely unwired/uncompiled and references a nonexistent symbol |

**Net for this cluster**: of 9,318 lines, roughly 3,668 are genuinely load-bearing today
(`frozen.rs` 649 + `template_cache.rs` 534 + `template_types.rs` 965 + `preprocessor.rs` 558 +
`inject.rs` 485 + `streaming_generator.rs` 573, minus double counting — these six form one
connected live subgraph rooted at `generator.rs`/`streaming_generator.rs`/`pipeline.rs`, all
reachable **only from test code today, zero CLI/production call sites** for any of them) +
`generator.rs` itself (748, already tracked by T053) ≈ 4,416 lines with real callers; the
remaining ≈4,902 lines (`templates/` minus `frozen.rs` = 3,273, `tera_env.rs` 159,
`parallel_generator.rs` 231, `gpack.rs` 567, `lockfile.rs` 508, `templates/mod.rs` 68 wiring +
164 misc) are safe to drop with T055, no porting required. The load-bearing subgraph's complete
absence from any `ggen-cli`/`ggen-lsp`/`ggen-engine` call site is the same pattern the sync-
pipeline cluster's audit found for `codegen`/`pipeline_engine`/`sync` — real, currently-passing
test coverage with no production path, which T055 (delete `ggen-core`) will silently delete
along with the dead code unless someone decides whether `tests/{tracing,generator_core_tests,
template_systems_tests}.rs` and `tests/integration/code_generation_tests.rs` (4 real compiled
targets, ~30+ combined assertions) get ported or explicitly retired.


## Long-tail cluster: genesis/prompt_mfg/lean_six_sigma/dflss/poc/stpnt/semantic_bit/
manufacturing/drift/snapshot/stewardship/github/cache/delta/audit/codegen_lib/resolver/
tracing/simple_tracing/e2e_tests/parts_execution/parts_foundry/types/metrics (13,559 lines, 24 modules)

**Scope**: every top-level `ggen-core` module not covered by the concurrent `domain/`-focused
workflow, the security/pki/membrane cluster, or the sync-pipeline cluster (whose independent
audit — see the "Sync-pipeline cluster" section above — already covers `codegen_lib`,
`resolver.rs`, and top-level `pipeline.rs`/`pipeline_engine`, all confirmed here to agree).
`wc -l` verified per-module: genesis.rs 847, prompt_mfg/ 1350, lean_six_sigma.rs 785, dflss.rs
447, poc.rs 402, stpnt/ 122, semantic_bit/ 414, manufacturing/ 163, drift/ 776, snapshot.rs 690,
stewardship.rs 566, github.rs 559, cache.rs 630, delta.rs 922, audit/ 150, codegen_lib/ 422,
resolver.rs 277, tracing.rs 274, simple_tracing.rs 427, e2e_tests.rs 258, parts_execution.rs 233,
parts_foundry/ 643, types/ 1336, metrics.rs 866. Sum = 13,559 (matches the briefing's ~13,500
estimate).

**Method**: `LSP workspaceSymbol` re-confirmed down this session too (`GenesisEngine` lookup on
`genesis.rs:1:1` → same `exceeded max crash recovery attempts (3)` error every other cluster in
this file has independently hit). Fell back to exhaustive `grep` sweeps per module: exact-symbol
references, `ggen_core::<module>::` and crate-root re-exported-symbol references (checked
separately — several of these modules are re-exported at `lib.rs` crate root, so
`ggen_core::CacheManager` is a valid external path even where `ggen_core::cache::` never
appears), `crate::<module>::` self-consumption inside `ggen-core`, and `ggen-core`'s own
`tests/*.rs` (which exercise the module but disappear at T055, not real external callers).
Two surprising LOAD-BEARING findings were additionally verified with `cargo metadata
--no-deps --format-version=1` (target existence) and a real `cargo check --test <name>` run
(compiles clean today), not grep alone — see §3.

### 1. Two real, unexpected LOAD-BEARING findings the briefing didn't anticipate

**`dflss.rs` (447 lines) — LOAD-BEARING, already flagged by the briefing, re-confirmed.**
Sole external caller: `crates/ggen-cli/src/cmds/sigma.rs:7`,
`use ggen_core::dflss::{execute_dflss, DflssReport};`. Matches tasks.md T042's "verified no-op"
finding exactly (re-pointing blocked on `ggen_core::manifest::GgenManifest` type coupling, not an
oversight). No other external or internal callers found (`grep -rn "ggen_core::dflss\|dflss::"`
workspace-wide outside `ggen-core`: exactly this one hit).

**`types/` (1,336 lines) — LOAD-BEARING, transitively, via `dflss.rs` — not previously connected
to T042's flagged gap.** `crates/ggen-core/src/dflss.rs:7` also does
`use crate::types::fmea::FmeaConfig;`, and `dflss.rs:44` really deserializes into it
(`let config: FmeaConfig = serde_json::from_str(&contents)...`) inside the exact
`execute_dflss` function `ggen-cli/src/cmds/sigma.rs` calls — not just a doc-comment mention.
This means whoever eventually resolves T042's `dflss`/`GgenManifest` coupling gap will also need
`types/fmea.rs` (466 lines: `FailureModeEntry`, `FmeaConfig`, `FmeaValidationError`, `Occurrence`,
`RpnLevel`, `RpnScore`, `Severity`, `Detection`) ported alongside it — a dependency the ticket
breakdown (05-MANIFEST-CONFIG-PORT.md / T042's note) never mentions. The other 3 submodules of
`types/` (`codeowners.rs` 345, `enterprise.rs` 212, `path_protection.rs` 289 — `CodeownersGenerator`,
`EnterpriseConfig`, `PathProtectionConfig`, etc.) have **no confirmed external caller**: their only
consumers are `crates/ggen-core/src/domain/generation/{codeowners.rs,protection.rs}`, and
`grep -rn "ggen_core::domain::generation\|domain::generation::"` across `ggen-cli`/`ggen-lsp`/root
returns zero matches — so whether the rest of `types/` is load-bearing depends entirely on whether
`domain::generation` itself (the concurrent workflow's territory, per the brief) has a real
external caller nobody has traced yet. Flagging this cross-reference for that workflow rather than
resolving it here.

### 2. `genesis.rs` (847 lines) — DEAD as a live dependency, but a genuine, evidence-backed
DUPLICATE-OF an active sibling crate, answering the briefing's specific question

- **Zero external callers**: `grep -rn "ggen_core::genesis\|use.*genesis::"` across `ggen-cli`,
  `ggen-lsp`, `ggen-engine`, root `{src,tests,examples,benches}` (excluding false-positive matches
  on `genesis-types`/`genesis_types`/`genesis-core`/`genesis_core` sibling-crate names): zero real
  matches. Zero self-consumers inside `ggen-core` (`grep -rn "crate::genesis::"` outside the file
  itself: zero). Not re-exported at `lib.rs` crate root (`grep -n "pub use genesis" lib.rs`: zero).
  Only consumers are its own 5 crate-level integration tests
  (`crates/ggen-core/tests/genesis_{sabotage,determinism,primitives,page_split,domain_bounds}_test.rs`)
  — real, but internal to `ggen-core`, gone under T055.
- **Answering the briefing's direct question — genuinely connected, not a name-coincidence.**
  `crates/genesis-core-v2/src/primitives.rs` (428 lines, in the **active** `genesis-core-v2`
  workspace member) defines `Pair2`, `RefusalReason`, `Refusal`, `RelationPage<const CAP: usize>`,
  `Construct8`, `Receipt`, `ReplayCursor` — the **same struct names, same conceptual roles**
  (bounded relation-pair primitives, receipts, refusal/replay machinery) as `ggen-core::genesis`'s
  `Pair2`, `RefusalCode`, `Refusal`, `RelationPage`, `Construct8`, `Receipt`, `Replay`, just
  rewritten as a `#[repr(C)]`, const-generic, more `no_std`-idiomatic second generation. This isn't
  a guess: `/Users/sac/ggen/CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md:13` (the same doc CLAUDE.md
  cites for the "Removed in the 2026-07 consolidation pass" list) explicitly documents this lineage
  for a *third*, now-fully-deleted copy of the same primitives (the old `genesis-core` crate, 256
  lines) and recommends exactly this migration: "Recommend salvaging `Pair2`/`RelationPage`/
  `Construct8Packet` no_std primitives into `genesis-core-v2`... before deletion" — which appears
  to have already happened (`genesis-core-v2/src/primitives.rs` is the result). `ggen-core::genesis`
  (mine, 847 lines) is the same design's *second* independent copy, never migrated, never deleted.
- **Caveat, so as not to overclaim "duplicate of a live thing"**: `genesis-core-v2` itself is
  presently an orphaned workspace member post-`stpnt`-removal. `grep -rln "genesis-core-v2"
  --include=Cargo.toml crates/` matches only `genesis-core-v2`'s own `Cargo.toml`; `grep -rln
  "genesis_core_v2::" crates/ src/ tests/ examples/ benches/` matches only its own `README.md` and
  its own `tests/receipt_chain_test.rs`. `genesis-types-v2` similarly now has exactly one real
  consumer left (`genesis-core-v2`'s own `lib.rs`), down from two (`stpnt` was the other, per the
  same consolidation doc, and `stpnt` was itself removed in that pass without anyone removing
  `genesis-core-v2`/`genesis-types-v2` alongside it). So the accurate statement is: `ggen-core::
  genesis` is a dead, unmigrated duplicate of a design that was correctly identified for
  consolidation into `genesis-core-v2` — but `genesis-core-v2` is *also* currently unreferenced by
  any live consumer, a second, adjacent, orphaned-crate cleanup question this migration surfaces
  but does not need to resolve to retire `ggen-core::genesis` itself (deleting the dead copy is
  safe regardless of what happens to the other one).
- Recommend: **do not port** `genesis.rs` — it is dead weight whose one legitimate purpose
  (canonical no_std relation-page primitives) is already served, more completely, by
  `genesis-core-v2::primitives`. If `genesis-core-v2` itself later gets deleted as newly-orphaned,
  that is a separate decision outside this ticket's scope.

### 3. `simple_tracing.rs`/`tracing.rs` (top-level)/`resolver.rs` — real, currently-compiling
external callers verified with `cargo metadata` + `cargo check`, not grep alone; reconciles
with, and sharpens, the sync-pipeline cluster's independent findings above

The sync-pipeline cluster's §6 ("Two more dead/duplicate pipelines...") already found top-level
`pipeline.rs`'s only external callers are `tests/generator_core_tests.rs`,
`tests/template_systems_tests.rs`, `tests/tracing.rs`, and characterized them (borrowing tasks.md
T053's framing) as "already-blocked... not gating `just test`." That characterization needs one
correction, verified two independent ways rather than assumed from T053's prose summary:

```
$ cargo metadata --no-deps --format-version=1 | jq -r '.packages[] | select(.name=="ggen") |
  .targets[] | select(.kind==["test"]) | .name' | grep -E "^(tracing|generator_core_tests|
  template_systems_tests)$"
tracing
generator_core_tests
template_systems_tests

$ cargo check --test tracing --test generator_core_tests --test template_systems_tests \
  --test contract
   Checking ggen-cli-lib v26.7.4 (...)
   Checking ggen-lsp v26.7.4 (...)
   Checking ggen v26.7.4 (...)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 28.56s
```

(**Independently triangulated**: the template-system cluster's own audit, appended just above this section, reached the identical `cargo metadata` 44-target-list conclusion for the same 3 files from a completely different angle — `templates::frozen::FrozenMerger`/`template_types::Template`/`streaming_generator.rs` respectively — three separate audit passes now agree these binaries genuinely compile today, not a single-pass assumption.) All three are real, `cargo metadata`-confirmed, auto-discovered (top-level `tests/*.rs`, not
nested — Cargo's auto-discovery boundary) test targets, and all three — plus `contract` (which
pulls in `pipeline_engine` via `tests/contract/pipeline.rs`, per the sync-pipeline cluster's §7) —
compile clean today with **zero errors**. This means, concretely, for **my** three modules:

- **`simple_tracing.rs` (427 lines) — LOAD-BEARING.** `tests/tracing.rs` (a real, compiled,
  currently-passing test binary named `tracing`) does `use ggen_core::simple_tracing::
  SimpleTracer;` at line 2, plus `TraceLevel::{Debug,Error,Warn}` and `SimpleTimer::start(...)`
  at lines 67/140/147/152/238. This is a distinct, home-grown, `GGEN_TRACE`-env-var-driven tracer
  — not the same implementation as top-level `tracing.rs` (which wraps the real `tracing`/
  `tracing-subscriber`/OTEL crates) — so it is not simply a duplicate of the other; it's a second,
  independently-designed observability system, and it is the one this specific compiled test
  actually exercises.
- **top-level `tracing.rs` (274 lines) — LOAD-BEARING, transitively.** Zero direct external
  callers, but its sole internal consumer, `crates/ggen-core/src/pipeline.rs:72`
  (`use crate::tracing::PipelineTracer;`), is exactly the top-level `pipeline.rs` the sync-pipeline
  cluster confirmed has 3 real external callers (see above) — including the same `tests/
  tracing.rs` binary this section already re-verified compiles clean.
- **`resolver.rs` (277 lines) — reachability re-confirmed as real (agreeing with the
  sync-pipeline cluster's "DEAD from the sync path" verdict on the separate, narrower question of
  whether `ggen sync` needs it).** Its only consumer, `pipeline_engine/passes/emission.rs:15`
  (`use crate::resolver::TemplateResolver;`), sits inside the same `pipeline_engine` crate the
  sync-pipeline cluster's §7 already traced to a real, `cargo metadata`-confirmed, now
  `cargo check`-clean external caller: `tests/contract/pipeline.rs` (part of the compiled
  `contract` binary). One honest caveat, not present in the sync-pipeline cluster's note: that
  test only calls `StagedPipeline::new(config)` (construction), not `.run()` or any method that
  necessarily reaches `emission.rs`'s `TemplateResolver`-touching code path — so "compiles and the
  crate it's in has a real external caller" is proven; "this specific test exercises
  `resolver.rs`'s logic at runtime" is not. Net verdict, matching the sync-pipeline cluster's
  framing exactly: **not needed for `ggen sync` capability parity**, but not simply "zero real
  caller" either — it is part of a crate (`pipeline_engine`) with a genuine, compiled, external
  consumer, just not the CLI's actual sync path.

**Why this matters beyond terminology**: if `ggen-core` is deleted (T055) before `tests/tracing.rs`,
`tests/generator_core_tests.rs`, `tests/template_systems_tests.rs`, and `tests/contract/pipeline.rs`
are either ported or deliberately deleted, `just check`/`just test` (which the Definition of Done
in this repo's `.claude/rules/andon/signals.md` requires green) will break on 4 real, currently-
compiling test binaries — not a hypothetical, `cargo check`-verified today.

### 4. Everything else in this cluster — DEAD, confirmed by grep, no surprises

For each of the following, `grep -rn "ggen_core::<module>::"` (and, where the module's symbols are
re-exported at `lib.rs` crate root — noted per item — the bare re-exported symbol names too)
across `ggen-cli/src`, `ggen-lsp/src`, `ggen-engine/src`, and root `{src,tests,examples,benches}`
returns **zero** matches; `grep -rln "crate::<module>::"` inside `crates/ggen-core/src` (excluding
the module's own files) also returns zero, **except where individually noted**:

- **`prompt_mfg/` (1,350 lines)** — `PromptCompiler`, `CompiledPrompt`, `PromptError`. Zero
  external, zero self, zero `ggen-core` test references at all (not even its own crate-level
  integration test — the only test coverage is nothing).
- **`lean_six_sigma.rs` (785 lines)** — `LeanSixSigmaGate`, `DmaicPhase`, `Criteria`. Zero
  external. One internal consumer: `crates/ggen-core/src/poka_yoke/quality_gates.rs`. `poka_yoke`
  itself (2,294 lines, re-exported piecemeal at crate root as `lifecycle::{Closed, Counter,
  EmptyPathError, ...}` per `lib.rs:238-241`, but its own `poka_yoke::` module path has zero
  external references: `grep -rn "ggen_core::poka_yoke\|poka_yoke::"` outside `ggen-core`: zero) is
  outside both this cluster's and the concurrent workflow's explicit scope — flagging as a genuine
  unowned gap: nobody has yet confirmed whether `poka_yoke` overall is dead, and `lean_six_sigma`'s
  status is downstream of that unresolved question. Also note: `poka_yoke::quality_gates::
  QualityGate` is a **third** independent `QualityGate` type (`validation::gate::QualityGate` is a
  second, already re-exported and load-bearing per `validation/mod.rs:72` — not investigated
  further here, outside this cluster).
- **`poc.rs` (402 lines)** — `HygenFrontmatter`, `poc_hygen()`. Zero external code references (one
  hit in `tests/chicago_tdd/LIBRARY_TEST_VALIDATION_REPORT.md`, a markdown report documenting a
  past test failure — data, not a caller). Its only test coverage is a private, `#[cfg(test)]`-
  gated internal `mod tests` block inside the file itself.
- **`stpnt/` (122 lines: `mod.rs` 12, `github.rs` 42, `obligation.rs` 68)** — answering the
  briefing's specific check: **confirmed NOT the same as the removed sibling `stpnt` crate**, a
  coincidental namesake only. The removed crate (832 lines, per
  `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md:14`) was a "canonical checkpoint validation tool"
  depending on `genesis-core-v2`/`genesis-types-v2`/`ggen-core`. This module ("Stewards of the
  Pentecost") is `ggen-core`'s own internal 3-file GitHub-stewardship-obligation concept
  (`StewardshipObligation`, `GitHubStewardshipMembrane`) with **zero code coupling** to
  `genesis-core-v2`/`genesis-types-v2` (`grep -n "genesis" crates/ggen-core/src/stpnt/*.rs`: zero
  matches) — thematically adjacent to `stewardship.rs`/`parts_execution.rs` (below, "welcome",
  "consent", "Pentecost invitation" naming) but shares zero imports with either. Zero external
  callers; the one apparent hit (`tests/values_inline_enforcement_test.rs:224`) is a SPARQL
  `VALUES` string literal `"stpnt"`, not a module reference — verified by reading the line.
- **`semantic_bit/` (414 lines: `field.rs`, `law.rs`, `phase.rs`, `root.rs`, `lifecycle.rs`,
  `receipt.rs`, `machine.rs`, `compound.rs`)** — `Field8<L,P>` (typestate, `PhantomData`-based),
  `Law`, `TableLaw`, `Machine`, `Receipt`, `Replay`. Zero external, zero self, zero tests.
  Conceptually adjacent to `genesis.rs` (another "compact 8-bit semantic primitive" design, another
  independent `Receipt`/`Replay` pair) but **not a literal duplicate** — a typestate-driven design
  (`Field8<Law, Phase>`) rather than `genesis.rs`'s relation-page data model; noting the family
  resemblance (this is now the *third* independent "genesis-flavored 8-bit primitive" experiment
  found across this audit and the removed `genesis-core`, after `ggen-core::genesis` and
  `genesis-core-v2::primitives`) without overclaiming code-level duplication.
- **`manufacturing/` (163 lines: `gates.rs`, `operator.rs`)** — `ProofGate`, `GateResult`,
  `ManufacturingOperator`, `OperatorContext`. Zero external, zero self. One coincidental-name test
  hit: `crates/ggen-core/tests/parts_manufacturing_e2e_test.rs` — read in full, its only import is
  `use ggen_core::parts_foundry::{...}` (below); it never references `manufacturing::` at all
  despite the filename.
- **`drift/` (776 lines: `detector.rs`, `sync_state.rs`)** — `DriftDetector`, `DriftStatus`,
  `ChangeType`, `DriftChange`, `FileHashState`, `SyncState`, all re-exported at `lib.rs:247`.
  **LOAD-BEARING**: `examples/drift-detection-example.rs:28`,
  `use ggen_core::drift::{DriftDetector, DriftStatus};` — a real, auto-discovered (directly under
  `examples/`, no `[[example]]` entry needed) Cargo example target, confirmed compiled by `cargo
  metadata`. This is **not a new finding** — tasks.md's T053 already lists `drift-detection-
  example.rs (also drift)` in its "50 files remain genuinely untouched" bucket (line ~992 of
  `tasks.md`) — cited here only to confirm the earlier record is accurate and complete for this
  module, not to claim credit for a new discovery.
- **`snapshot.rs` (690 lines)** — `Snapshot`, `GraphSnapshot`, `FileSnapshot`, `TemplateSnapshot`,
  `Region`, `RegionType`, `SnapshotManager`, all re-exported at `lib.rs:299`. Zero *direct*
  external callers. Two internal consumers: `crates/ggen-core/src/merge/mod.rs` (top-level
  `merge/`, 1,913 lines, itself confirmed **zero** external and **zero** other internal callers —
  `grep -rln "crate::merge::\|use crate::merge\b" crates/ggen-core/src` outside `merge/` itself:
  zero — so this chain terminates in dead code) and
  `crates/ggen-core/src/domain/template/regenerate.rs` (glob-re-exported as part of `domain::
  template`'s public surface, `domain/template/mod.rs:36`, `pub use regenerate::*;`). The second
  chain looked live at first (`domain::template` is confirmed load-bearing overall, per tasks.md
  T037: `ggen-cli/src/cmds/template.rs` calls `domain::template::{show,new,list,lint,
  TemplateService}`) — but the **only** thing anywhere that actually imports `regenerate`'s items
  specifically, `tests/integration/template_tests/test_template_regenerate.rs:3`, does
  `use ggen_cli::domain::template::regenerate::{...}` — **`ggen_cli` is not this workspace's real
  external crate name** (`ggen-cli`'s library target is named `ggen_cli_lib`, per its own
  `Cargo.toml`; `find crates/ggen-cli/src -maxdepth 1 -iname "domain*"` finds nothing) — so this
  test cannot possibly compile regardless of import path, confirming it as pre-existing broken
  code (already independently in tasks.md T053's own "50 blocked" bucket under the same
  `Generator`/`Pipeline`/`Template` heading). **Verdict: DEAD** in practice — technically part of
  `ggen-core`'s public API surface via the glob re-export, but with no real, working caller
  anywhere in the workspace today.
- **`stewardship.rs` (566 lines)** — `WelcomeOneAnotherPart`, `ConsentGatePart`,
  `AssignStewardPart`, `FollowUpObligationPart`, `PentecostInvitationPart`, `ContinuityWatchPart`
  (each with paired `*Input`/`*Output` structs). Zero external, zero other internal consumers. Sole
  consumer of `parts_execution.rs` (below); this pair forms a small, internally-coherent,
  fully-dead 2-file subsystem.
- **`github.rs` (559 lines, top-level, distinct from `stpnt::github.rs` above)** —
  `GitHubClient`, `PagesConfig`, `RepoInfo`, `WorkflowRun`, `WorkflowRunsResponse`, all
  re-exported at `lib.rs:279`. Zero external, zero self, zero tests.
- **`cache.rs` (630 lines, top-level)** — `CacheManager`, `CachedPack`, re-exported at
  `lib.rs:244`. Zero external, zero self, zero real `ggen-core` test references (the one apparent
  hit, `tests/lifecycle_edge_cases.rs`'s `cache::cache_key(...)`, resolves to `ggen_core::
  lifecycle::*`'s own nested `cache` submodule via that file's glob import — confirmed by reading
  its `use` block — a different `cache` entirely, not this one). **Naming-collision note**:
  `ggen-core::cache::CachedPack` is a **third**, independent `CachedPack`-named type alongside
  `ggen_marketplace::marketplace::cache::CachedPack` (the live one, reachable via T036's
  `ggen_core::marketplace::cache::*` re-point) — same name, same general "cached pack metadata"
  role, structurally unrelated field sets (`ggen-core`'s targets its own `gpack::GpackManifest`
  concept; `ggen-marketplace`'s targets `PackageId`/`PackageVersion`). Not a functional duplicate
  worth reconciling (the `ggen-core` one is dead), but worth flagging in the same spirit as the
  security cluster's `PathValidator`/`PkiManager`/`AuditTrail`/`QualityGate`/`MetricsCollector`
  collision list — this makes at least 3 independently-named-but-colliding `cache`-flavored
  concepts in this workspace (`ggen-core::cache`, `ggen-core::lifecycle::cache`,
  `ggen_marketplace::marketplace::cache`).
- **`delta.rs` (922 lines, top-level)** — `DeltaType`, `GraphDelta`, `ImpactAnalyzer`,
  `TemplateImpact`, re-exported at `lib.rs:246`. Zero external, zero self, zero tests.
- **`audit/` (150 lines: `mod.rs` 104, `writer.rs` 46, top-level, distinct from both
  `domain::audit` and `codegen::audit`)** — `AuditTrail`, `ExecutionMetadata`,
  `AuditTrailWriter`. Zero external, zero self-consumers. Has its own crate-level integration test
  (`crates/ggen-core/tests/audit_trail_integration_tests.rs`, internal only, gone at T055).
  Independently confirms the security/pki/membrane cluster's collision note (that section's own
  words: "top-level `audit/mod.rs:12` `pub struct AuditTrail`... a third independent one") — this
  audit adds the missing piece that security cluster's finding didn't check: this third
  `AuditTrail` also has zero self-consumers inside `ggen-core`, so it is not just a naming
  collision but a fully dead one, unlike `codegen::audit`'s `AuditTrailBuilder` which the
  sync-pipeline cluster's §1 confirms **is** load-bearing (real per-input/per-output hash audit
  trail for `--audit`, called from `SyncExecutor::execute_full_sync`).
- **`codegen_lib/` (422 lines: `mod.rs`, `generated_file.rs`, `generation_mode.rs`, `queryable.rs`,
  `renderable.rs`, `rule.rs`)** — `GeneratedFile`, `GenerationMode`, `Queryable`, `Renderable`,
  `Rule`, `Error`. Zero external, zero self, zero tests. Independently re-confirmed by the
  sync-pipeline cluster's §6 (identical verdict, identical `GenerationMode`-duplicate finding —
  the real pipeline uses `manifest::GenerationMode`, `codegen/pipeline.rs:727`, not this copy).
  My own check found exactly the same 2 `enum GenerationMode` definitions in `ggen-core`
  (`manifest/types.rs:380` and `codegen_lib/generation_mode.rs:5`) with different variant sets
  (`Create/Overwrite/Merge` vs. `Overwrite/Append/SkipIfExists`) — not a third.
- **`e2e_tests.rs` (258 lines)** — not a library module in any practical sense: its entire content
  is a single private `#[cfg(test)] mod tests { ... }` block (confirmed:
  `grep -n "^#\[cfg\|^mod \|^pub mod\|^fn \|#\[test\]"` shows only `#[cfg(test)] mod tests` and 4
  `#[test]` functions, nothing `pub` at the module level beyond the outer `pub mod e2e_tests;`
  declaration itself). Under a non-test build this compiles to nothing. **No porting question
  applies** — it is `ggen-core`'s own internal test code, not a capability.
- **`parts_execution.rs` (233 lines)** — `VectorClock`, `ExecutionStatus`, `ExecutionPacket`,
  `LocalExecutionContext`, `PartExecutor` (trait), `RefusalEvidence`. Zero external. Sole internal
  consumer: `stewardship.rs` (above), itself dead. Zero tests.
- **`parts_foundry/` (643 lines: `part_compiler.rs`, `adapter_generator.rs`, `part_signer.rs`)** —
  zero external, zero self-consumers. One coincidentally-named-adjacent test,
  `parts_manufacturing_e2e_test.rs` (see `manufacturing/` above) — this one genuinely does
  `use ggen_core::parts_foundry::{...}`, a real `ggen-core` crate-level test, gone at T055.
  `adapter_generator.rs`'s "Generated Genesis membrane adapter" string (lines 48/86/149/176) is
  output-template text the generator emits into *scaffolded projects*, not a reference to
  `src/membrane/` — same coincidental-string-match pattern the security cluster's membrane section
  already flagged for this exact file, re-confirmed here.
- **`metrics.rs` (866 lines, top-level, distinct from `security::metrics`)** — `CodeMetrics`,
  `ProcessMetrics`, `DefectMetrics`, `WasteType`, `WasteMetrics`, `FlowMetrics`, `OEEMetrics`,
  `KaizenMetrics`, `MetricsReport`, `MetricsCollector`, all re-exported at `lib.rs:232-235`. Zero
  external, zero self-consumers, one internal `ggen-core` test
  (`metrics_integration_test.rs`, gone at T055). Independently confirms the security cluster's
  collision note from the other side: they found `security::metrics::MetricsCollector` dead; this
  audit confirms the top-level `metrics.rs::MetricsCollector` it collides with is **also** dead —
  both halves of that naming collision are confirmed inert, not just one.

### 5. Summary classification table

| Path | Lines | Verdict |
|---|---|---|
| `dflss.rs` | 447 | **LOAD-BEARING** — sole caller `ggen-cli/src/cmds/sigma.rs:7` (already known, T042) |
| `types/` (`fmea.rs` specifically; other 3 submodules UNCERTAIN) | 1,336 | **LOAD-BEARING** (`fmea.rs` only, transitively via `dflss.rs:44`) — new finding, not previously connected to T042; `codeowners.rs`/`enterprise.rs`/`path_protection.rs` are UNCERTAIN, depend on unresolved `domain::generation` reachability |
| `drift/` | 776 | **LOAD-BEARING** — `examples/drift-detection-example.rs`, real compiled example target; already tracked in tasks.md T053 |
| `simple_tracing.rs` | 427 | **LOAD-BEARING** — `tests/tracing.rs`, confirmed via `cargo metadata` + clean `cargo check` |
| `tracing.rs` (top-level) | 274 | **LOAD-BEARING**, transitively via `pipeline.rs:72` → the same 3 confirmed-compiling root tests |
| `resolver.rs` | 277 | Real compiled-crate reachability (`pipeline_engine` → `tests/contract/pipeline.rs`, confirmed clean `cargo check`) but **not needed for `ggen sync` capability parity** — agrees with the sync-pipeline cluster's independent verdict |
| `genesis.rs` | 847 | **DEAD** as a dependency; **DUPLICATE-OF** `genesis-core-v2::primitives.rs` (evidenced lineage via `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md`); do not port |
| `prompt_mfg/` | 1,350 | DEAD |
| `lean_six_sigma.rs` | 785 | DEAD externally; 1 internal consumer (`poka_yoke::quality_gates`, itself unowned/unresolved — flagged gap) |
| `poc.rs` | 402 | DEAD |
| `stpnt/` | 122 | DEAD; confirmed **not** the same as the removed sibling `stpnt` crate (coincidental name only) |
| `semantic_bit/` | 414 | DEAD; conceptually (not literally) adjacent to `genesis.rs`/`genesis-core-v2::primitives` |
| `manufacturing/` | 163 | DEAD; coincidental-filename-only test hit (`parts_manufacturing_e2e_test.rs` actually tests `parts_foundry`) |
| `snapshot.rs` | 690 | DEAD in practice — only reachable path is a pre-existing non-compiling test (`ggen_cli::domain::...`, wrong crate name) plus an already-dead `merge/` module |
| `stewardship.rs` | 566 | DEAD |
| `github.rs` (top-level) | 559 | DEAD |
| `cache.rs` (top-level) | 630 | DEAD; 3-way `CachedPack`/`cache` naming collision noted |
| `delta.rs` | 922 | DEAD |
| `audit/` (top-level) | 150 | DEAD; third independent `AuditTrail`, confirms + extends security cluster's finding |
| `codegen_lib/` | 422 | DEAD; independently confirmed by the sync-pipeline cluster too; `GenerationMode` is DUPLICATE-OF `manifest::GenerationMode` |
| `e2e_tests.rs` | 258 | N/A — pure `#[cfg(test)]`-internal test code, no library surface, nothing to port |
| `parts_execution.rs` | 233 | DEAD; sole consumer `stewardship.rs` (also dead) |
| `parts_foundry/` | 643 | DEAD |
| `metrics.rs` (top-level) | 866 | DEAD; confirms security cluster's `MetricsCollector` collision from the other side |

**Net for this cluster**: of 13,559 lines, roughly 1,890 lines (`dflss.rs` 447 + `types/fmea.rs`
466 + `drift/` 776 + a proportional share of `simple_tracing.rs`/`tracing.rs`, counted separately
below since their "capability" is observability, not sync output) have a real, confirmed external
caller and need a port decision before `ggen-core` can be safely deleted; `simple_tracing.rs`
(427) + `tracing.rs` (274) = 701 more lines are load-bearing for `just test` staying green (not
for CLI capability parity) via 3 `cargo check`-confirmed root test binaries; `resolver.rs` (277)
sits in between (real compiled reachability, no CLI-capability need). The remaining ~10,000 lines
are confirmed safe to drop with T055, no porting required.
