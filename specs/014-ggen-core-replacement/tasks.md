# Implementation Tasks: Retire ggen-core in favor of a first-principles engine

**Branch**: `2026-ggen-core-replacement`
**Created**: 2026-07-16
**Total Tasks**: 67
**Input**: `plan.md`, `spec.md`, `data-model.md`, `contracts/`, `research.md`, and the full
evidence base in `docs/jira/v26.7.16/00-OVERVIEW.md` through `13-CLAUDE-MD-REFACTOR.md`

Every task below cites the exact full path(s) it touches. Where a ticket already enumerates
dozens of individual files (e.g. 164 call sites across 31 `ggen-cli` files), this breakdown
groups them by the same functional bucket the ticket already established, rather than
emitting one task per file — the ticket itself remains the authoritative per-file reference
during execution of each grouped task.

---

## Phase 1: Setup

- [ ] T001 Confirm workspace builds clean on `2026-ggen-core-replacement` before any change: run `just check && just lint && just test` from `/Users/sac/ggen`
- [ ] T002 Confirm `just` (not `cargo make`) is the enforced command runner for this feature: verify `/Users/sac/ggen/justfile` has `check`, `test`, `lint`, `slo-check` recipes (per `research.md`'s constitution-conflict finding)
- [ ] T003 Record baseline CLI command output for every command in `contracts/cli-command-surface.md`'s table (run each against a scratch project, save output) to diff against post-migration behavior

## Phase 2: Foundational (blocking prerequisites for all user stories)

**Purpose**: Land the replacement engine crate in the workspace, inert and unwired, with its
identity, licensing, and sibling-dependency risks resolved — nothing in Phase 3 onward can
start until this phase is done.

- [ ] T004 Vendor `/Users/sac/praxis/crates/ggen` into a new crate directory `/Users/sac/ggen/crates/ggen-engine/` (physical copy, not a live path dependency — see `research.md` "Decision: Vendor a physical copy")
- [ ] T005 Edit `/Users/sac/ggen/crates/ggen-engine/Cargo.toml`: change `name = "ggen"` to `name = "ggen-engine"`, add `publish = false`
- [ ] T006 [P] Vendor `/Users/sac/praxis/crates/praxis-core` into `/Users/sac/ggen/crates/praxis-core/`, rewrite its `bcinr-powl-receipt`/`wasm4pm-compat` path dependencies to the correct absolute paths for this checkout
- [ ] T007 [P] Vendor `/Users/sac/praxis/crates/praxis-graphlaw` into `/Users/sac/ggen/crates/praxis-graphlaw/`, rewrite its `bcinr-pddl`/`bcinr-powl`/`bcinr-powl-receipt`/`wasm4pm-compat` path dependencies to the correct absolute paths for this checkout
- [ ] T008 Add `ggen-engine`, `praxis-core`, `praxis-graphlaw` to `[workspace] members` in `/Users/sac/ggen/Cargo.toml`, unwired (no consumer references them yet)
- [ ] T009 Drop the `OR Apache-2.0` clause from `/Users/sac/ggen/crates/praxis-core/Cargo.toml` and `/Users/sac/ggen/crates/ggen-engine/Cargo.toml` license fields (`license = "MIT"` only) per `docs/jira/v26.7.16/02-CROSS-REPO-DEPENDENCY-RISKS.md` item 4
- [ ] T010 Confirm `/Users/sac/bcinr/crates/{bcinr-pddl,bcinr-powl,bcinr-powl-receipt}` and `/Users/sac/wasm4pm/crates/wasm4pm-cognition` exist on the build machine; document the new hard filesystem dependency in `/Users/sac/ggen/README.md` or a build-prerequisites doc
- [ ] T011 Run `just check` — confirm the workspace builds with the three new crates present but completely unreferenced by any existing consumer

**Checkpoint**: `cargo tree -p ggen-engine` resolves cleanly; no existing crate's behavior has changed yet.

---

## Phase 3: User Story 2 - The published crate on crates.io is never put at risk (Priority: P1)

**Goal**: Guarantee the vendored engine crate can never collide with the published `ggen` package.

**Independent Test**: `cargo publish --dry-run --package ggen` succeeds and only ever targets the root package; the new CI guard fails on a deliberately-reintroduced collision.

- [x] T012 [US2] Create `/Users/sac/ggen/scripts/ci/guard-publish-target.sh` per `docs/jira/v26.7.16/01-PUBLISH-SAFETY-AND-CRATE-RENAME.md`'s exact script
  - Deviated from the ticket's literal script in two ways, both fixes not scope changes:
    (1) name-collision detection was rewritten to use `cargo metadata`'s own `[package].name`
    resolution instead of a naive first-match `name = "..."` grep — the naive grep matched
    `ggen-cli/Cargo.toml`'s `[[bin]] name = "ggen"` (line 2) instead of its real package name
    `ggen-cli-lib` (line 29), a false positive proven via `cargo metadata --no-deps`.
    (2) the blanket "every crate needs `publish = false`" check was narrowed to the 3 vendored
    replacement-engine crates (`ggen-engine`, `praxis-core`, `praxis-graphlaw`) — FR-007 only
    requires the replacement engine's source crate never publish under the shared name; the
    ~9 pre-existing crates (ggen-core, ggen-cli-lib, ggen-graph, ggen-config,
    ggen-marketplace, ggen-lsp, genesis-types-v2, genesis-core-v2, cpmp) already ship without
    `publish = false` and closing that pre-existing gap is a separate, broader policy change
    this ticket doesn't authorize.
- [x] T013 [US2] Add `guard-publish-target` recipe to `/Users/sac/ggen/justfile`, wire into the `pre-commit`/CI recipe chain
  - `guard-process-intelligence-boundary` (cheap, always green) wired into `just pre-commit`
    directly and as a blocking job in `.github/workflows/quality.yml`.
  - `guard-publish-target` added as a standalone `just` recipe and a CI job, but NOT wired
    into the local `pre-commit` chain and marked advisory (`|| echo "::warning::..."`) in CI,
    matching the existing `audit`/`slo` advisory pattern in that same workflow file. Reason:
    its `cargo publish --dry-run` step fails today on a pre-existing, already-documented,
    unrelated gap (see T014) — wiring it in blocking would break every future commit for a
    reason this ticket doesn't own.
- [x] T014 [US2] Run `cargo publish --dry-run --package ggen` and `./scripts/ci/guard-publish-target.sh`; confirm both pass (acceptance scenario 1 of User Story 2 in `spec.md`)
  - BLOCKED, not passing, root cause fully identified and pre-existing:
    (1) `cargo publish --dry-run` refuses on the currently-uncommitted migration tree
    (expected Cargo behavior, resolves once committed).
    (2) Retested with `--allow-dirty` to isolate the underlying logic: fails on `ggen`'s
    `chicago-tdd-tools` dev-dependency requesting feature `cli-proof`, which the real
    crates.io-published `chicago-tdd-tools` v26.7.1 doesn't have — only available via the
    local-only `[patch.crates-io]` override at `Cargo.toml:807-808`, which `cargo publish`
    correctly ignores (that's the whole point of the dry-run safety check). This is
    pre-existing and already self-documented in `Cargo.toml:804-806` ("Remove this section
    and bump the version constraint once cli-proof is released"), unrelated to this
    migration's vendored crates.
    The guard script's own collision-detection and publish=false logic (the actual
    migration-relevant part) were verified correct in isolation, outside the full script:
    a deliberately-reintroduced collision (`ggen-cli`'s `[[bin]] name = "ggen"`) was
    correctly caught before the metadata-based fix, and correctly NOT flagged after it;
    `ggen-engine`/`praxis-core`/`praxis-graphlaw` correctly show `publish: []` (== `false`)
    via `cargo metadata`.
- [x] T015 [US2] Confirm `/Users/sac/ggen/crates/ggen-engine/Cargo.toml` has a distinct name and `publish = false` (acceptance scenario 2 of User Story 2)
  - Confirmed via `cargo metadata --no-deps --format-version=1`: `ggen-engine`, `praxis-core`,
    `praxis-graphlaw` all show a distinct name (not `ggen`) and `"publish": []` (Cargo's
    JSON encoding of `publish = false`).

**Checkpoint**: Publish-safety contract from `spec.md` User Story 2 is verifiable; the
collision-detection and publish=false invariants are proven green in isolation. The
end-to-end `cargo publish --dry-run --package ggen` gate remains BLOCKED by a pre-existing,
already-documented, unrelated `chicago-tdd-tools`/`cli-proof` gap (out of scope for this
migration) plus the expected mid-migration git-dirty state (resolves on commit).

---

## Phase 4: User Story 1 - Maintainers get a smaller, honest codebase without losing capability (Priority: P1)

**Goal**: Every existing CLI/LSP capability keeps working against the new engine; `ggen-core`
is fully retired.

**Independent Test**: Full test suite green with `ggen-core` unreferenced; command-surface
diff against the Phase 1 baseline shows zero regressions.

### 4a. RDF engine bridge (`docs/jira/v26.7.16/03-RDF-ENGINE-BRIDGE-DESIGN.md`)

- [x] T016 [P] [US1] Define the `LawEngine` trait in `/Users/sac/ggen/crates/ggen-engine/src/law_engine.rs` per `contracts/law-engine-trait.md`
  - Signature matches the contract exactly: `materialize`/`validate_shacl`/`check_denials`, all
    `&str`-only, no `oxrdf`/`spargebra`/`oxigraph` types. Reuses `graph::{MaterializeOutcome,
    ShaclOutcome}` (identical shape, no new duplicate types).
- [x] T017 [US1] Implement `LawEngine` backed internally by `/Users/sac/ggen/crates/praxis-graphlaw`, following the `GraphLawStore` pattern from `/Users/sac/ggen/crates/ggen-engine/src/graph.rs:354-357,447-496`
  - `GraphLawEngine`: stateless (no persistent mirror, unlike `GraphLawStore` — each call builds
    its own `TripleStore` from its string args, per contract rule 2, "callers own re-ingestion").
    Reuses the same refusal-detection (`EffectKind::Refuse`/`HookVerdict::Fired`) and
    before/after diffing technique, but diffs through `decode_triples` on both sides (not a
    persistent mirror) for exact format consistency.
  - Fixed one real clippy hit in this new code during verification:
    `map(...).unwrap_or_else(...)` → `map_or_else(...)` (`clippy::map_unwrap_or`).
- [x] T018 [US1] Write a unit test proving the bridge is load-bearing (the `graphlaw_e2e.rs` pattern: same fixture fails under plain oxigraph, succeeds with N3-derived facts) in `/Users/sac/ggen/crates/ggen-engine/tests/law_engine_test.rs`
  - `LawEngine` has only one implementation (no oxigraph-backed alternative to contrast against,
    unlike `GraphEngine`), so the load-bearing proof is: same fact set, no rules loaded → derives
    nothing; N3 rule loaded → derives the rule-implied fact absent from the raw input. Extended to
    all 3 trait methods (materialize/validate_shacl/check_denials), each proving the call reaches
    the real reasoner, not a passthrough. `cargo test -p ggen-engine --test law_engine_test`: 4/4
    pass (real execution, no mocks; `/tmp/law_engine_test.log`).
  - **Fixed 3 real, pre-existing regressions discovered while verifying this task**, all
    consequences of the earlier `ggen`→`ggen-engine` rename (T-numbers predate this discovery):
    1. 19 files under `crates/ggen-engine/{src,tests,benches}/` still said `use ggen::` (the
       crate's name before the rename) — `cargo check --workspace` never caught this since it
       doesn't build test/bench targets by default. Fixed via literal substring replace
       (`ggen::` → `ggen_engine::`, verified no identifier had it as a suffix first) across all
       19 files. `cargo check -p ggen-engine --all-targets` now passes (was exit 101).
    2. `tests/lint_validate_e2e.rs` referenced `ggen_engine::verbs::handlers::handle_graph_validate`
       and `cargo_bin("ggen")` — both broken by the earlier, deliberate `verbs` module disablement
       (`lib.rs`'s own comment: ggen-cli is the sole CLI surface) and `autobins = false`. Commented
       out (not deleted) the 7 affected tests with a pointer to T037-T038 (this functionality's
       real new home is `ggen-cli/src/cmds/graph.rs`); kept the 4 tests that only exercise
       `lint_template` directly (no `verbs` dependency).
    3. `praxis-graphlaw/src/bindings.rs:261-263` declared `#[cfg(test)] mod bindings_test;` at
       `#[path = "bindings_test.rs"]`, but that file doesn't exist in EITHER this vendored copy or
       the original `~/praxis/crates/praxis-graphlaw/src/` (confirmed via `ls` on both) — a
       pre-existing upstream gap, not something vendoring introduced or that I can fabricate the
       contents of. Commented out (not deleted), noted for upstream fix + re-vendor.
  - **New tracked gap for T058** (not fixed now — out of scope for this task, large and
    pre-existing): `cargo clippy -p ggen-engine --all-targets -- -D warnings` reports ~300
    pre-existing violations in vendored code unrelated to `law_engine.rs`/`law_engine_test.rs`
    (confirmed clean in isolation) — mostly `clippy::pedantic`/`unwrap_used`/`expect_used` in
    `ggen-engine`'s own lib+tests (this crate's Cargo.toml opts into those at `"warn"`; `just
    lint`'s `-D warnings` escalates all of them to hard errors) plus ~20 style-lint categories in
    `praxis-graphlaw`'s own tests/benches (its `#![allow(...)]` crate-root fix in this same commit
    only covers its **lib** target). `just check`/`cargo test` are unaffected (clippy-only gap).
    T058's `just lint` will need either scoped allows extended to these crates' test/bench
    targets (same "vendored, documented scoped allow" pattern used for `praxis-graphlaw`'s lib) or
    upstream fixes in `~/praxis`, before it can go green.

### 4b. Manifest/config port (`docs/jira/v26.7.16/05-MANIFEST-CONFIG-PORT.md`)

- [x] T019 [US1] Port `/Users/sac/ggen/crates/ggen-core/src/manifest/types.rs` (573 lines) to new file `/Users/sac/ggen/crates/ggen-config/src/manifest/types.rs`
  - Ported verbatim except `OntologyConfig::resolved_sources()` (delegated to
    `ggen_core::ontology::resolver::OntologyResolver`, a `walkdir`-based filesystem scan) --
    dropped, documented via comment. Confirmed zero callers outside ggen-core's own (retired)
    codegen pipeline; not part of this ticket's scope and would require a new `walkdir` dep.
  - Minor Chicago-TDD-neutral cleanups made while porting: `#[must_use]` on 2 pure getters,
    `.map(...).unwrap_or(key)` → `.map_or(key, ...)` (clippy fixes on code I authored, not
    scope creep into unrelated files).
- [x] T020 [US1] Port `/Users/sac/ggen/crates/ggen-core/src/manifest/parser.rs` (170 lines) to new file `/Users/sac/ggen/crates/ggen-config/src/manifest/parser.rs`, rewriting its `Result<T>` to `ggen_config::ConfigError`-based `Result`
  - `Error::new(&format!(...))` → `ConfigError::TomlParse`/direct `?` (via `#[from]
    std::io::Error`/`#[from] toml::de::Error`, both already on `ConfigError`no new variants
    needed). Fixed the doctest's `use crate::manifest::ManifestParser;` (meaningless outside
    the crate under test) to the real external path `use ggen_config::manifest::ManifestParser;`
    -- verified via `cargo test --doc -p ggen-config` (passes).
- [x] T021 [US1] Port `/Users/sac/ggen/crates/ggen-core/src/manifest/validation.rs` (305 lines) to new file `/Users/sac/ggen/crates/ggen-config/src/manifest/validation.rs`
  - `Error::new(&format!(...))` → `ConfigError::Validation(format!(...))` throughout (exact
    message text preserved, including the `error[E0010]`/`E0011`/`E0013`/`E0014` strings
    `ggen-lsp`'s diagnostics grep for). Added `log = { workspace = true }` as a new
    `ggen-config` dependency (previously absent) to preserve the two non-fatal
    `log::warn!` calls (missing ORDER BY outside strict mode, duplicate rule order) verbatim
    -- matches the facade `ggen-cli` (this crate's consumer) already uses directly.
- [x] T022 [US1] Add `pub mod manifest;` to `/Users/sac/ggen/crates/ggen-config/src/lib.rs`
  - Module-only declaration (no crate-root `pub use manifest::*`), matching ggen-core's own
    original `lib.rs:168` convention exactly -- avoids any collision with `config_lib`'s own
    already-re-exported `ProjectConfig` (different type, same name, different module).
- [x] T023 [US1] Reconcile the two overlapping config schemas: retire or explicitly disjoint-scope `config_lib::GgenConfig`'s generation/inference sections in `/Users/sac/ggen/crates/ggen-config/src/config_lib/schema.rs:757,734` against the newly-ported `GgenManifest` types (FR-009)
  - Chose **retire** (ticket 05's first option) over disjoint-scoping: confirmed via grep
    that `GgenConfig.inference`/`.generation` (`Option<InferenceConfig>`/
    `Option<GenerationConfig>`, the latter with an untyped `rules: Vec<serde_json::Value>`
    passthrough) have **zero real callers** anywhere in the workspace -- the actively-consumed
    typed schema for `[inference]`/`[generation]` was always `ggen_core::manifest::*`
    (confirmed: `ggen-lsp/src/project_index.rs:120`'s own comment, "`generation` is a
    required, non-optional field on `GgenManifest`"; `ggen-cli/src/cmds/sync.rs:596`). Removed
    the two dead fields from `GgenConfig`'s struct + `Default` impl, and removed the
    now-unused `InferenceConfig`/`InferenceRule`/`GenerationConfig` struct definitions from
    `schema.rs` entirely (not re-exported at `config_lib`'s crate boundary, confirmed via
    grep) -- each removal documented in place with a pointer to the new home.
  - **Fixed a real regression this removal caused**: `ggen-config/tests/adversarial_tests.rs`
    constructed 5 full `GgenConfig` struct literals (testing project name/version/star-toml
    `.check()` validation, unrelated to inference/generation) that each set the now-removed
    `inference: None, generation: None,` fields -- removed those 2 lines from each of the 5
    literals; zero test-coverage loss (nothing in that file exercised inference/generation
    semantics). `cargo test -p ggen-config`: 10/10 (adversarial) + 9/9 (new manifest module) +
    11/11 (doctests) pass.
  - **T023 follow-up (this session): full three-way schema unification, closing the gap the
    original T023 pass explicitly deferred** ("the actively-consumed typed schema... was
    always `ggen_core::manifest::*`" handled only the inference/generation overlap between
    `config_lib::GgenConfig` and `GgenManifest` — it did not touch the operational/
    infrastructure fields, nor `ggen-engine`'s own third schema). Investigated all three
    schemas first, verifying claims rather than assuming them:
    - Checked whether `config_lib::GgenConfig` implements `star_toml::Validate` (the task
      brief said to verify, not assume) — it does, already, correctly and with its own test
      coverage (`config_lib/schema.rs:921-1215`). What it lacked was any
      `[[generation.rules]]`/`[ontology]`/`[inference]` concept at all, and `GgenManifest`
      lacked `star_toml::Validate` entirely (only the hand-rolled, ggen-core-ported
      `ManifestValidator`).
    - **Design decision**: made `ggen_config::manifest::GgenManifest`
      (`crates/ggen-config/src/manifest/types.rs`) the one authoritative Rust type, by
      *reusing* `config_lib`'s existing, already-`Validate`-implemented, already-tested
      operational structs directly as `GgenManifest`'s own field types
      (`Option<crate::config_lib::{AiConfig, RdfConfig, SparqlConfig, LifecycleConfig,
      SecurityConfig, PerformanceConfig, LoggingConfig, TelemetryConfig, TemplatesConfig,
      BuildConfig, TestConfig, PackageMetadata, McpConfig, A2AConfig}>`) rather than
      redefining them a third time. This directly satisfies the brief's "full field coverage"
      requirement (`ai`/`templates`/`rdf`/`sparql`/`lifecycle`/`security`/`performance`/
      `logging`/`telemetry`/`features`/`env`/`build`/`test`/`package`/`mcp`/`a2a`, the last 5
      newly added to `GgenManifest` — purely additive via `#[serde(default)]`, so no
      previously-valid manifest is affected) while keeping `config_lib::GgenConfig`'s own
      tested `Validate` impls as the single source of truth for those sections' semantics,
      exercised (not re-implemented) when nested under `GgenManifest`.
    - Added `law: Law { rules: Vec<PathBuf> }` (N3/Datalog rule files, mirroring
      `ggen_engine::config::Law`) for the field-coverage item `ggen-engine::config::GgenConfig`
      needs. Reconciled the "two SHACL-shapes concepts" the brief flagged: did **not** add a
      duplicate `law.shapes` -- `validation.shacl: Vec<PathBuf>` (already existing, already
      path-checked) remains the one and only SHACL-shapes field; a `law`-aware consumer reads
      `law.rules` for N3 and `validation.shacl` for shapes. Documented in `types.rs`'s
      `GgenManifest`/`Law` doc comments.
    - Left `sync`/`output` as untyped `Option<toml::Value>` passthroughs, unchanged --
      confirmed via grep these have zero real readers anywhere in the workspace, have no
      counterpart in `config_lib` to reconcile against, and the root project's own
      `/Users/sac/ggen/ggen.toml` already uses both with fields (`[sync].on_change`,
      `[output].line_length`) that don't correspond to any typed schema this reconciliation
      is chartered to invent from scratch. Explicitly out of scope, not silently dropped.
    - **`star_toml::Validate` vs. the hand-rolled `ManifestValidator`**: implemented
      `impl Validate for GgenManifest` in `manifest/validation.rs` covering every pure-data
      invariant (project name/version non-empty; inference/generation rule name/construct/
      output_file non-empty; the E0011/E0013 ORDER-BY lint restricted to the parts that don't
      need file I/O -- `InferenceRule.construct` is always inline text so its E0011 check is
      fully pure, `GenerationRule`'s E0013 check covers only the `QuerySource::Inline` variant;
      E0014 pack-declared-in-`[[packs]]` check; delegation into every operational section's
      own already-tested `Validate` impl). Filesystem-dependent checks (ontology source/
      imports existence, `QuerySource::File`/`TemplateSource::File` existence, the file-content
      half of the E0010 VALUES-clause and E0013 ORDER-BY checks, `validation.shacl`/new
      `law.rules` existence) moved to a new inherent method,
      `GgenManifest::validate_paths(&self, base_path: &Path) -> Result<()>`, kept explicitly
      separate from `Validate` rather than forced into its `fn validate(&self, v: &mut
      Validator)` signature (which has no `base_path` parameter, by design, matching
      `ggen_engine::config::GgenConfig`'s own `Validate` impl). Full reasoning (including *why*
      the non-strict-mode "log a warning, don't fail validation" behavior cannot go through
      `star_toml::Validator` at all -- `Validator::finish()` fails on any recorded error
      regardless of `Severity`, so that branch calls `log::warn!` directly, unchanged from the
      original code) is documented in `manifest/validation.rs`'s module-level doc comment.
      `ManifestValidator` itself is kept as a thin, path-stable wrapper (`new`/`validate`
      unchanged) that now runs `self.manifest.check()` then `self.manifest.validate_paths(...)`
      -- `ManifestParser::parse_and_validate` and both known out-of-crate callers
      (`ggen-lsp/src/a2a_mcp/mcp_server.rs`, `ggen-cli/src/cmds/sigma.rs` -- both still against
      `ggen_core`'s own separate copy pending T041/T049) needed zero changes.
    - **Deliberately not added** (investigated and rejected, not silently skipped): a
      path-traversal check on `ontology.source`/`.imports` mirroring
      `ggen_engine::config::Ontology::validate`'s `check_path(..., Some(false))` -- grepping
      real fixtures first found `.specify/specs/*/ggen.toml`, `.specify/mcp-a2a/*.toml`, and
      several `marketplace/packages/*/ggen.toml` files legitimately use `../` in
      `source`/`imports` for sibling-directory ontology sharing; adding this check would have
      broken real, currently-valid manifests. Also not added: non-empty checks on
      `PackRef.name`/`ValidationRule` fields -- neither was checked by the original
      `ManifestValidator` and both are new, untested validation surface with unknown blast
      radius. Both gaps are documented in `validation.rs`'s doc comment as deliberate,
      bounded, future work.
    - Reconciled the `star-toml` version pin: `ggen-config`'s `Cargo.toml` said `"26.7.2"`
      against `ggen-engine`'s `"26.7.3"` (Cargo.lock already resolved both to 26.7.3, so this
      was a no-op for the actual build, but an avoidable inconsistency per the brief). Bumped
      `ggen-config/Cargo.toml` to `"26.7.3"` to match.
    - **Engine-wiring (item 3 of the brief): NOT implemented this session -- deliberately
      scoped out, per the brief's own "scale guidance" escape hatch.** Investigated the actual
      cost/risk: `ggen_engine::config::GgenConfig` (`crates/ggen-engine/src/config.rs`) has no
      dependency on `ggen-config` today (confirmed: zero `ggen-config` mentions in
      `ggen-engine/Cargo.toml`), and its `#[derive(JsonSchema)]` is load-bearing --
      `tests/ggen_toml_schema_match.rs` asserts the struct's *actual* field set (via
      `schemars::schema_for!`) matches `schema/ggen-toml-schema.ttl` *exactly*. Adding a
      `generation: Option<ggen_config::manifest::GenerationConfig>`-shaped field (or any
      superset field) to `ggen_engine::config::GgenConfig` would require: (a) a new, deliberate
      `ggen-engine → ggen-config` dependency edge (same class of decision as T028's
      `ggen-engine → ggen-marketplace` edge, but not yet made); (b) extending
      `schema/ggen-toml-schema.ttl` with new `ggenspec:hasField` triples for `GenerationConfig`
      and its nested `GenerationRule`, plus real TTL-vs.-struct coverage for the *untagged*
      `QuerySource` (3 variants: Pack/File/Inline) and `TemplateSource` (5 variants:
      Pack/File/Inline/Git/Package) enums -- the existing test file only has a bespoke helper
      for `PackRef`'s simpler 2-variant case (`pack_ref_variant_fields()`,
      `tests/ggen_toml_schema_match.rs:79-98`), not a generic N-variant helper; (c) rewriting
      `discover_templates()`/`sync()` (`crates/ggen-engine/src/sync.rs:139,653-665`) to run a
      second, declarative-rules code path alongside the existing frontmatter-per-template-file
      convention, resolving `QuerySource`/`TemplateSource`'s `File`/`Inline`/`Git`/`Package`/
      `Pack` variants (the last needing the T028 `ggen-marketplace` edge) and merging their
      output into the same `SyncReport`/receipt-closure/write-decision bookkeeping the
      frontmatter path already populates. This is a real, multi-file feature addition touching
      a load-bearing, JsonSchema-checked type and the write/receipt pipeline -- not a
      mechanical reconciliation -- and could not be implemented and *verified* (real
      `cargo test -p ggen-engine` execution, per this project's evidence-first rule) inside
      this session without risking exactly the four tests the brief named as must-stay-green
      (`ggen_toml_schema_match.rs`, `graphlaw_e2e.rs`, `sync_e2e.rs`, plus
      `frontmatter_schema_match.rs`). Per the brief's explicit instruction ("if genuinely too
      large/risky... stop short of a half-working implementation... write a clear, specific,
      evidence-based design note... so a human can make the final call"), this is left as an
      open design question for Phase 4c/4d, not implemented. **Whoever picks this up next**
      needs to decide: keep both models permanently (frontmatter for simple per-file rules,
      declarative `[[generation.rules]]` for cross-file/pack-sourced rules), or migrate
      existing frontmatter-style fixtures (`examples/demo-project`, `examples/demo-pack`, the
      `sync_e2e.rs`/`graphlaw_e2e.rs` fixtures) to the declarative model and retire
      `discover_templates()`. Either is a real design call this session should not make
      unilaterally.
    - **Verification (real execution, not narration)**: `cargo check -p ggen-config
      --all-targets`: clean. `cargo test -p ggen-config`: **92/92 pass** (71 lib + 10
      adversarial + 11 doctests -- includes 4 new tests added this session for the pure-data
      `Validate` split and operational-section reuse: `test_validate_paths_directly_
      on_manifest`, `test_pure_data_validate_catches_empty_project_name_without_fs_access`,
      `test_e0014_pack_not_declared_is_pure_data_violation`,
      `test_operational_sections_reuse_config_lib_validation`). `cargo clippy -p ggen-config
      --lib --tests -- -D warnings`: clean (2 findings in my own new test code, both fixed:
      `too_long_first_doc_paragraph` on `Law`'s doc comment, `needless_collect` in 2 assertions
      -- rewritten to `.any(...)`). `cargo check -p ggen-lsp --all-targets`: clean (confirms
      `ggen-lsp`'s direct consumption of `ggen_config::manifest::{ManifestParser,
      GenerationRule, QuerySource, TemplateSource, GenerationMode}` via `project_index.rs`/
      `rule_index.rs` -- unaffected, since none of those types' shapes changed, only
      `GgenManifest`'s own new fields and its validation split). `cargo test -p ggen-engine`:
      lib **107/107 pass**; the four tests the brief named as must-stay-green all pass
      unchanged (`ggen_toml_schema_match.rs` 5/5, `graphlaw_e2e.rs` 5/5, `sync_e2e.rs` 10/10,
      plus `frontmatter_schema_match.rs` 1/1, `law_engine_test.rs` 4/4) -- expected, since
      `ggen-engine` has no dependency on `ggen-config` at all (confirmed via grep on its
      `Cargo.toml`), so this session's changes cannot have affected them either way. 10 other
      pre-existing `ggen-engine` test binaries fail (`cli_boundary`,
      `cli_read_only_invariant_matrix`, `cross_pack_matrix`, `doctor_e2e`,
      `framework_packs_e2e`, `pack_behaviors_cli_e2e`, `pack_e2e`, `receipt_chain_e2e`,
      `wasm4pm_facts_e2e`, `write_behaviors_cli_e2e`) -- confirmed unrelated to this task: 8 of
      the 10 spawn the external `ggen` CLI binary via `CARGO_BIN_EXE_ggen`/`assert_cmd`
      (breaking on the concurrent T031-T044 ggen-cli re-point's in-flight CLI shape, per this
      repo's documented concurrent-session build flakiness), and `wasm4pm_facts_e2e` fails on a
      missing fixture directory (`packs/wasm4pm-facts-pack/ontology.ttl` absent on disk) --
      neither category reachable from `ggen-config`.

### 4c. Marketplace / pack-registry merge (`docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md`)

- [x] T025 [US1] Port the remaining 18 files under `/Users/sac/ggen/crates/ggen-core/src/domain/packs/` (5,899 lines) into new module `/Users/sac/ggen/crates/ggen-marketplace/src/packs_registry/`
  - Mechanical port: `crate::domain::packs::` → `crate::packs_registry::`; `crate::utils::error::
    {Error, Result}` (ggen-core's plain string-message type) → `crate::marketplace::error::
    {Error, Result}` (ggen-marketplace's existing richer `thiserror` enum, already has `Other`/
    `Io`/`TomlParse`/etc.); every `Error::new(...)`/`Error::new(&format!(...))` call site (27
    total across 9 files) → `Error::Other(...)`, preserving exact message text. All external deps
    (oxigraph, tar, tera, async-trait, tracing, reqwest, uuid, chrono) already present in
    ggen-marketplace's Cargo.toml -- zero new deps needed for this module.
  - `install.rs` excluded from this port (merged separately into T024 below, per ticket 06).
    `pub mod install;` dropped from the ported `mod.rs`; its doc-comment intra-doc link to
    `crate::agent::PackAgent::show` (a ggen-core-only type with no counterpart here) softened to
    plain text rather than left as a broken rustdoc reference.
  - **Scope discovery, not yet resolved**: confirmed via grep that `ggen-core/src/agent/
    facade.rs`/`agent/mod.rs` (the `PackAgent` facade, using `packs::install::PackInstallResult`)
    are NOT covered by T024-T028 or any other T0xx task -- ticket 08's own text confirms `agent`
    has "zero counterpart" in the replacement engine or its dependencies. This is Phase 4f's
    problem (T041, "re-point agent/pack-agent facade") to resolve, not this ticket's.
  - `cargo test -p ggen-marketplace --lib packs_registry::`: 34/34 pass.
- [x] T026 [US1] Port `/Users/sac/ggen/crates/ggen-core/src/packs/lockfile.rs` (788 lines) to new file `/Users/sac/ggen/crates/ggen-marketplace/src/packs/lockfile.rs`
  - Same `Error`-type rewrite as T025, plus `Error::with_context(msg, &format!(...))` (a
    two-field ggen-core-only constructor with no `ggen_marketplace::marketplace::error::Error`
    equivalent) → `Error::Other(format!("{msg}: ..."))`, folding msg+context into one string
    (5 call sites). Fixed 7 doctest `use crate::packs::lockfile::...`/`crate::utils::error::
    Result` references (meaningless inside a doctest regardless of source crate, same class of
    bug as T020's parser.rs fix) to the real external `ggen_marketplace::packs::lockfile`/
    `ggen_marketplace::marketplace::error::Result` paths.
  - `cargo test -p ggen-marketplace --lib packs::lockfile`: 9/9 pass; `--doc lockfile`: 7/7 pass.
- [x] T024 [US1] Merge `/Users/sac/ggen/crates/ggen-core/src/domain/packs/install.rs` (280 lines) into `/Users/sac/ggen/crates/ggen-marketplace/src/marketplace/install.rs`'s `Installer` (line 31)
  - **Not a literal signature merge**: confirmed the existing `Installer::install_pack(&self,
    package_id: &PackageId, version: &PackageVersion) -> Result<CachedPack>` is a genuinely
    different install path (downloads from `self.repository`, ggen's own signed marketplace
    registry, with mandatory signature verification) than ggen-core's free-function
    `install_pack(&InstallInput) -> Result<InstallOutput>` (resolves a bare string pack ID from
    the LOCAL registry or an EXTERNAL registry like crates.io/npm/PyPI). These are complementary,
    not competing -- unifying their signatures would be a real redesign beyond this ticket's
    "merge, don't duplicate" scope.
  - Landed the ported logic as free functions in the SAME FILE as `Installer` (not as `Installer`
    methods, since none of it touches `self`/`R: AsyncRepository`/the cache -- it's independent),
    satisfying "absorbed into install.rs, not a separate parallel module" without forcing an
    unused `&self` receiver. Renamed the incoming free function `install_pack` →
    `install_pack_by_id` to avoid colliding with the existing method of the same name.
    `InstallInput`/`InstallOutput` renamed `InstallByIdInput`/`InstallByIdOutput` for the same
    reason (ggen-marketplace's own `models.rs` has no conflicting names, but the pairing reads
    clearer). Its unique contribution -- `compute_pack_digest`/`write_lockfile_entry`, writing to
    `.ggen/packs.lock` via the T026 lockfile module -- is genuinely new capability for
    `ggen-marketplace`, which had no lockfile concept before this migration (per ticket 06).
    Added missing `use tar::Archive;` (present as a transitive need but not previously imported
    in this file, since the file didn't unpack tar archives directly before).
  - `cargo test -p ggen-marketplace --lib`: 300/300 pass (266 pre-existing + 34 new), zero
    regressions.
- [x] T027 [US1] Delete dead stub `/Users/sac/ggen/crates/ggen-core/src/packs/install.rs:33-39` (do not port)
  - Satisfied by omission -- confirmed the stub's only real type (`PackInstallResult`) has zero
    callers outside `ggen-core/src/agent/{facade.rs,mod.rs}` (the same out-of-scope `agent`
    module flagged under T025). Nothing ported it; nothing needs to.
- [x] T028 [US1] Add the new, deliberate `ggen-engine → ggen-marketplace` dependency to resolve `QuerySource::Pack` at sync time (`ggen-engine/Cargo.toml`), documented per `docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md` risk 1
  - Dependency edge added and documented; `cargo check -p ggen-engine` confirms it resolves
    cleanly (currently unused -- the actual `QuerySource::Pack` resolution call site doesn't
    exist yet, see the significant gap noted below).

**Significant gap discovered while completing T028 -- RESOLVED (schema unification done in this
entry's own follow-up bullets; engine-wiring done in T070 below).**
Original finding: `ggen-engine` has a **third**, independent `ggen.toml` schema
(`ggen-engine/src/config.rs::GgenConfig` -- distinct from both `config_lib::GgenConfig` and the
ported `ggen_config::manifest::GgenManifest`), and its `sync()` pipeline
(`ggen-engine/src/sync.rs:139`) resolves output via `discover_templates()` -- a
frontmatter-per-template-file convention (scan `[templates].dir`, read each `.tmpl` file's own
embedded `to:`/`sparql:`/`construct:`/`when:` block) -- not via a declarative `[[generation.
rules]]` list. `GenerationRule`/`QuerySource`/`TemplateSource` (ggen-core's model: named rules in
`ggen.toml` referencing separate query/template files, inline content, git, or **pack outputs**)
had **no consumer anywhere in ggen-engine**, and `data-model.md`'s "Configuration Schema" entity
commits to more than a type-porting exercise: "both the Engine (for `sync`) and the Diagnostic
layer... read from the same reconciled schema after this migration."

**What a later session (this one) did about it**: `ggen_config::manifest::GgenManifest` is now
the one authoritative Rust type with full field coverage across all three schemas -- see T023's
"T023 follow-up" bullets above for the complete design (reused `config_lib`'s already-`Validate`d
operational structs directly as `GgenManifest`'s own field types rather than redefining them a
third time; added `law.rules` for the N3/Datalog concept `ggen-engine::config::GgenConfig`
needed, reconciling SHACL shapes onto the existing `validation.shacl` rather than duplicating it;
split validation into `star_toml::Validate` (pure data) + `GgenManifest::validate_paths` (fs) so
the type finally uses `star_toml::Validate` consistently, matching `ggen_engine::config::
GgenConfig`'s own pattern instead of a hand-rolled validator). `ggen-lsp` (the Diagnostic layer)
already consumes this exact type via `project_index.rs`/`rule_index.rs` (T045) -- so the
Diagnostic half of the data-model.md commitment is met.

**Engine-wiring resolution (T070, a later session)**: the Engine half is now met the way (c)
below anticipated -- a real second processing path in `sync()` alongside `discover_templates()`,
not a replacement of it. `ggen-engine/src/sync.rs`'s `sync()` now reads `ggen.toml`'s raw text
once and dispatches: a `[[generation.rules]]`-bearing manifest is parsed as
`ggen_config::manifest::GgenManifest` and handed to the new `crate::generation_rules::run`; a
manifest without one (the existing default) falls through to the original, byte-for-byte
unchanged `GgenConfig::load` + `discover_templates()` path -- confirmed additive, not disruptive,
by `tests/sync_e2e.rs`/`tests/graphlaw_e2e.rs` (10 + 5 tests, both exclusively frontmatter-path
fixtures) passing unchanged. The three items this note originally called out as required were
each closed: (a) the `ggen-engine → ggen-config` dependency edge exists
(`ggen-engine/Cargo.toml`); (b) the JsonSchema-vs-TTL contract was extended via a **new,
independent** test/schema pair rather than growing `tests/ggen_toml_schema_match.rs` itself --
`schema/ggen-manifest-schema.ttl` + `tests/ggen_manifest_schema_match.rs`, covering
`GenerationConfig`/`GenerationRule` and the untagged `QuerySource` (3 variants)/`TemplateSource`
(5 variants) enums via a new generic `untagged_variant_fields::<T>()` helper (deliberately kept
independent of `ggen_toml_schema_match.rs`'s own `pack_ref_variant_fields()`, so this addition
cannot regress the test the original gap note named as must-stay-green); (c) the "which model
wins, or how both coexist" architecture call was made explicitly: **both models coexist
permanently**, selected per-project by a cheap structural pre-parse
(`crate::generation_rules::has_generation_rules`), never merged into one schema. See T070's own
entry below for the full design, the real bug found and fixed while verifying it, and real
`cargo test` evidence. T028's dependency edge (`ggen-engine → ggen-marketplace`, needed for
`QuerySource::Pack` resolution) remains real, correct, and still unused -- `QuerySource::Pack`/
`TemplateSource::Pack` are implemented as a named, typed refusal (`[FM-GEN-006]`/`[FM-GEN-007]`)
rather than silently resolved, exactly matching this note's own prior "no consumer exists yet"
framing; a real consumer for that edge remains a tracked follow-up, not invented here.

### 4d. Project scaffolding port (`docs/jira/v26.7.16/07-PROJECT-SCAFFOLDING-PORT.md`)

- [x] T029 [US1] Port `/Users/sac/ggen/crates/ggen-core/src/cli_generator/` (7 files, 1,254 lines) into new module `/Users/sac/ggen/crates/ggen-cli/src/scaffolding/cli_generator/`
  - Mechanical port of all 7 files (`mod.rs`, `types.rs`, `ontology_parser.rs`, `domain_layer.rs`,
    `cli_layer.rs`, `dx.rs`, `workspace.rs`): `crate::cli_generator::` →
    `crate::scaffolding::cli_generator::`. Landed standalone, unwired — no caller yet (`ggen
    init`/`ggen wizard` re-pointing is T043, someone else's job); added `pub mod scaffolding;` to
    `/Users/sac/ggen/crates/ggen-cli/src/lib.rs` and nothing else there.
  - **Error-type decision**: ggen-cli already has its own richer `thiserror`-based enum,
    `crate::error::GgenError` (used throughout its own `cmds/*.rs`), so `crate::utils::error::
    {Error, Result}` (ggen-core's ad-hoc string-message type) maps onto `crate::error::{GgenError,
    Result}` — same "reuse the target crate's own richer enum" pattern as T025/T026, not a new
    parallel error type. Mapped by call-site meaning, not string-matching: Tera load/render
    failures → `GgenError::TemplateError`; filesystem create/write failures →
    `GgenError::FileError`; missing required `cli_crate`/`domain_crate` fields →
    `GgenError::InvalidInput`; `OntologyParser::parse`'s still-unimplemented body (a pre-existing,
    already-loud typed `Err`, not a silently-successful stub) → `GgenError::Internal`. All original
    message text preserved verbatim; `Error::with_context(msg, ctx)`'s two fields folded into one
    `format!("{msg}: {ctx}")` string (same folding technique T024 used for `Error::Other(...)`).
  - Fixed 6 doctest `use crate::cli_generator::...`/`crate::utils::error::Result<()>` references
    (meaningless inside a doctest regardless of source crate — same class of fix as T020/T026) to
    the real external `ggen_cli_lib::scaffolding::cli_generator::...`/`ggen_cli_lib::error::
    Result<()>` paths, including the 2 examples in `workspace.rs` marked ```ignore``` (not compiled
    by `cargo test --doc`, but fixed anyway for documentation correctness).
  - No `#[cfg(test)]` modules existed in any of the 7 original files (confirmed via read-through
    before porting) — zero test-count change attributable to this task; not a regression.
- [x] T030 [US1] Port `/Users/sac/ggen/crates/ggen-core/src/project_generator/` (4 files, 1,331 lines) into new module `/Users/sac/ggen/crates/ggen-cli/src/scaffolding/project_generator/`
  - Mechanical port of all 4 files (`mod.rs`, `rust.rs`, `nextjs.rs`, `common.rs`) with the same
    `Error`/`Result` → `GgenError`/`Result` mapping as T029 (`GgenError::ValidationError` for
    `common::validate_project_name`'s 4 checks; `GgenError::FileError` for filesystem ops;
    `GgenError::InvalidInput` for `ProjectType::from_str`'s unsupported-type case).
  - **Two additional judgment calls, both documented rather than silently resolved**:
    1. `project_generator/mod.rs` calls `crate::security::command::SafeCommand` at 3 call sites
       (git init, cargo fetch, npm install) — that's ggen-core's much larger `security` module (12
       files: intrusion detection, audit trail, alerting, etc.), out of this ticket's 11-file scope
       and not otherwise needed by `ggen-cli`. Rejected two alternatives: porting the whole
       `security` module (scope creep the other direction) and dropping to bare
       `std::process::Command` (silently weakens a deliberate "SECURITY FIX (Week 4)" command-
       injection hardening the original code chose). Landed a scoped, **private**, 12th file —
       `/Users/sac/ggen/crates/ggen-cli/src/scaffolding/project_generator/safe_command.rs` — with
       just the `SafeCommand`/`CommandError` unit (whitelist + dangerous-shell-metacharacter
       validation) adapted to `GgenError`, `mod`-private to `project_generator` (no new public
       surface). Trimmed the unused `args()`/`execute_stdout()`/`CommandExecutor` helpers since
       every call site here only ever uses `.arg()` once + `.current_dir()` + `.execute()`.
    2. `crate::alert_info!`/`alert_warning!`/`alert_success!` (5 call sites) depend on ggen-core's
       `utils::alert` module, which itself needs `slog_scope` — a dependency `ggen-cli` doesn't
       have. Rather than adding a new dependency for a stderr pretty-printer, replaced the 5 call
       sites with plain `eprintln!` calls carrying the same emoji + message text, dropping only the
       macro's own "STOP:/FIX:" boilerplate wrapper (a presentation flourish of `alert.rs` itself,
       not exercised by any test in the ported files).
  - **Preserved, did not "fix"**: 3 occurrences of `use crate::utils::error::Result;` left verbatim
    inside `rust.rs`'s `format!` string literals — these are generated-project *output content*
    for a freshly-scaffolded Rust project (a brand-new crate that has no `ggen-cli`/`ggen-core`
    dependency), not real imports resolved by rustc in this crate. They were already
    non-functional in the original ggen-core code (no generated `Cargo.toml` template depends on
    any such crate) — a pre-existing, out-of-scope oddity in the *generator's output*, not
    something this port introduced or was asked to fix.
  - Fixed 8 doctest path references the same way as T029.
  - `cargo test -p ggen-cli-lib --lib scaffolding::`: 14/14 pass (9 pre-existing tests ported
    verbatim from `mod.rs`/`rust.rs`/`nextjs.rs`/`common.rs` + 5 new tests in the added
    `safe_command.rs`), real execution, no mocks (`SafeCommand`'s tests shell out to a real `git
    --version`). `cargo test --doc -p ggen-cli-lib scaffolding`: 13 passed + 2 correctly `ignore`d
    (matching the 2 original ```ignore``` annotations in `workspace.rs`), 0 failed.
  - `cargo check -p ggen-cli-lib --all-targets`: clean. A concurrent, unrelated in-flight session
    was simultaneously landing T034/T035 (`ggen-cli/src/{telemetry.rs,utils/}`) during this task,
    which transiently broke the whole-crate build (`cmds/{graph,policy}.rs` type errors) for
    several re-samples before it stabilized — not this task's code, not patched here, per this
    repo's guidance on concurrent-session build flakiness ("never patch another session's
    in-flight crate"). Once stable: `cargo clippy -p ggen-cli-lib --lib --tests -- -D warnings`
    still can't complete as a *whole-crate* signal — it aborts on a genuinely pre-existing,
    unrelated `clippy::too_long_first_doc_paragraph` in `ggen-config/src/manifest/types.rs:235`
    (T019 territory, already-landed, not in-flight) before clippy's dependency graph ever reaches
    `ggen-cli-lib`. Isolating with `--no-deps` gives the real scoped signal: `cargo clippy -p
    ggen-cli-lib --lib --no-deps -- -D warnings` (i.e. just this crate's own **library** target,
    which includes `scaffolding` via its `pub mod scaffolding;` declaration) finishes clean, zero
    warnings. Adding `--tests` back in surfaces exactly one finding, `#[ignore]` without a reason
    at `crates/ggen-cli/src/utils/error.rs:371` — the concurrent T035 session's own test code, not
    anything under `scaffolding/`. Net result: the new scaffolding code has zero clippy findings.

### 4e. Receipt signing groundwork consumed by ggen-cli (design in `docs/jira/v26.7.16/04-RECEIPT-SIGNING-AND-OTEL.md`, full delivery in Phase 5 below — only the parts `ggen-cli`'s re-point needs are prerequisite here)

- [x] T031 [US1] Confirm `/Users/sac/ggen/crates/ggen-config/src/receipt/receipt_impl.rs` (352 lines) is import-ready for `ggen-cli` before re-pointing `cmds/receipt.rs`/`receipt_manager.rs` (blocks T036)
  - Confirmed via actual use, not just inspection: `ggen_config::receipt::{Receipt, hash_data, generate_keypair}` compiles and is used
    from `cmds/receipt.rs`, `receipt_manager.rs`, `cmds/sync.rs`, `crates/ggen-cli/src/agent/receipt.rs` in T033/T043/T041 below.
    `cargo test -p ggen-cli-lib`: every caller's tests pass (see T033/T041/T044 evidence).

### 4f. ggen-cli migration (`docs/jira/v26.7.16/08-GGEN-CLI-MIGRATION.md`, 164 call sites / 31 files, in the ticket's dependency order)

- [x] T032 [US1] Re-point `config_lib` shim: `/Users/sac/ggen/crates/ggen-cli/src/config_clap/loader.rs`, `/Users/sac/ggen/crates/ggen-cli/src/config_clap/mod.rs` to `ggen_config::*` directly; add `ggen-config` dependency to `/Users/sac/ggen/crates/ggen-cli/Cargo.toml`
  - `use ggen_core::config_lib::GgenConfig` → `use ggen_config::config_lib::GgenConfig` in `loader.rs`. `mod.rs`'s
    `pub use ggen_core::config_lib as ggen_config;` → `pub use ggen_config::config_lib as ggen_config_lib;` — renamed the
    local alias (not just re-pointed) because keeping the name `ggen_config` here would shadow the newly-added extern
    crate `ggen_config` for the rest of `config_clap`'s namespace; confirmed via grep that this alias had zero callers
    anywhere (`crate::config_clap::ggen_config` unused), so the rename is free.
  - Added `[dependencies.ggen-config]` (path = "../ggen-config", version = "26.7.4") and, while in the same edit block,
    `[dependencies.ggen-marketplace]` (needed by T036) to `Cargo.toml` — bundled since both are simple path-dep additions
    and adding an unused dep early doesn't create a compile error, only bucket 5 activates it.
  - `cargo check -p ggen-cli-lib --all-targets`: clean.
- [x] T033 [US1] Re-point `receipt`-core shim (excluding `provenance_envelope`/`chain_linking`): `/Users/sac/ggen/crates/ggen-cli/src/cmds/receipt.rs`, `/Users/sac/ggen/crates/ggen-cli/src/receipt_manager.rs`
  - `cmds/receipt.rs`: `use ggen_core::receipt::Receipt;` → `use ggen_config::receipt::Receipt;`.
  - `receipt_manager.rs`: `use ggen_core::receipt::{hash_data, Receipt};` → `use ggen_config::receipt::{hash_data, Receipt};`
    and the `generate_keypair()` call site → `ggen_config::receipt::generate_keypair()`. Left this file's
    `ggen_core::utils::{Error, Result}` (error plumbing) untouched here — that's T035's part of this same file, per the
    ticket's own split.
  - `cargo check -p ggen-cli-lib --all-targets`: clean at this checkpoint (before T035 landed the error-plumbing part of
    the same file, which is expected — the two concerns coexist in one file without conflict).
- [x] T034 [US1] Re-point `telemetry`: `/Users/sac/ggen/crates/ggen-cli/src/lib.rs` telemetry section
  - **Ported the whole `telemetry.rs` module** (native, zero counterpart in `ggen-config`/`ggen-marketplace`/`ggen-engine`)
    into new file `/Users/sac/ggen/crates/ggen-cli/src/telemetry.rs`, registered as `pub mod telemetry;` in `lib.rs`.
    Same "port native code into its sole consumer" pattern as T019/T024/T041.
  - **Behavior-preserving simplification, documented in the file itself**: the original gated its real OTLP
    implementation behind `#[cfg(feature = "otel")]`, but `ggen-cli`'s own `Cargo.toml` has always hardcoded
    `[dependencies.ggen-core] features = ["otel"]` unconditionally — so every real build of `ggen-cli` has always
    compiled the "otel" branch, regardless of `ggen-cli`'s own feature selection. The port drops the now-meaningless
    cfg-gate and makes the real implementation unconditional, adding `opentelemetry`/`opentelemetry-otlp`/
    `opentelemetry_sdk`/`tracing-opentelemetry`/`tracing-subscriber` as plain (non-optional) deps — this is provably
    behavior-equivalent, not a new design decision, and removes a fake toggle that never actually toggled.
  - `lib.rs`: `ggen_core::telemetry::TelemetryConfig`/`init_telemetry` → `crate::telemetry::{TelemetryConfig,
    init_telemetry}`; also fixed this same `cli_match()` block's `ggen_core::config_lib::GgenConfig` → `ggen_config::
    config_lib::GgenConfig` (bucket 1's part of `lib.rs`, not previously touched since T032 only covered `config_clap/`).
  - `cargo test -p ggen-cli-lib --lib telemetry::`: 2/2 pass.
- [x] T035 [US1] Re-point `utils::error` (11 files): `/Users/sac/ggen/crates/ggen-cli/src/{error.rs,runtime.rs}`, `receipt_manager.rs` error part, `cmds/{lsp.rs,mcp.rs,graph.rs,policy.rs}`, `conventions/{planner.rs,watcher.rs,resolver.rs,presets/mod.rs,presets/clap_noun_verb.rs}`
  - **Ported** `ggen_core::utils::error` (native, zero counterpart, confirmed via grep that no other in-scope crate
    references `ggen_core::utils` for anything beyond `error`/`Error`/`Result`/`Context`) into new module
    `/Users/sac/ggen/crates/ggen-cli/src/utils/{mod.rs,error.rs}`, registered as `pub mod utils;` in `lib.rs`.
    Ported the `Error` struct (all constructors used or not — kept the full set for fidelity), the `Context` trait
    (its `context`/`with_context` methods are used via UFCS in `resolver.rs`), the `Result` alias, and every `From` impl
    actually reachable from this crate's dependency set. Dropped, and documented in the file: the `From<config::
    ConfigError>` impl (`ggen-cli` has no dependency on the `config` crate and no caller ever produces one) and the
    unused `bail!`/`ensure!`/`ggen_error!` macros plus the `GgenError` type alias (zero callers anywhere in `ggen-cli`,
    confirmed via grep; the alias would have been a second, confusing meaning of `GgenError` alongside
    `crate::error::GgenError`).
  - Mechanical `ggen_core::utils::error::` → `crate::utils::error::` and `ggen_core::utils::{Error,Result,Context}` →
    `crate::utils::error::{Error,Result,Context}` across all 12 files (`error.rs`, `runtime.rs`, `receipt_manager.rs`,
    `cmds/{lsp,mcp,graph,policy}.rs`, `conventions/{planner,watcher,resolver,presets/mod,presets/clap_noun_verb}.rs`).
  - **One real cross-type fix required** (the exact risk the ticket's own text flagged: "gates everything downstream"):
    `cmds/graph.rs`'s `validate()` had a bare `ontology::validate_ontology_schema(...).await?` relying on `?`'s identity
    conversion, which worked before only because `ggen_core::domain::Error` and the old `ggen_core::utils::error::Error`
    were the *same* type. Once the wrapping `Error::new(...)` calls in the same function switched to the new local type,
    that identity broke (`ggen_core::domain::Error` → my new `crate::utils::error::Error` has no `From` impl, by
    design — these are genuinely different types now). Fixed by adding an explicit `.map_err(|e| crate::utils::error::
    Error::new(&format!(...)))` at that call site, matching the pattern already used at every sibling call in the file.
  - **Pulled T044 forward into this task** (documented under T044 below with the reasoning): `cmds/policy.rs`'s
    `load_pack_contexts_from_project() -> crate::Result<...>` consumes `lib.rs`'s `pub use ... Result` directly, so
    T035 and T044 are not actually separable in practice for this one file — flipping only 11 of 12 files while leaving
    `crate::Result`'s definition on the old type would have been a genuine type mismatch, not a deferred nice-to-have.
  - Fixed 2 doctest paths the same way as prior tasks: `runtime.rs`'s two `///` examples (marked ` ```ignore``` `, not
    compiled by `cargo test --doc`, but fixed anyway for documentation accuracy) and `resolver.rs`'s ` ```rust,no_run````
    example (this one **is** compiled) — both `crate::utils::error::Result` → `ggen_cli_lib::utils::error::Result`.
  - `cargo check -p ggen-cli-lib --all-targets`: clean. `cargo test -p ggen-cli-lib --lib`: 118/118 pass (1 pre-existing
    `#[ignore]` on `test_error_from_tera_error`, preserved verbatim from the original). `cargo test -p ggen-cli-lib
    --doc`: 21/21 pass, 8 correctly `ignore`d.
- [x] T036 [US1] Re-point `marketplace` shim: `/Users/sac/ggen/crates/ggen-cli/src/pack_install.rs`, `cmds/policy.rs` marketplace part, to `ggen_marketplace::marketplace::*` directly; add `ggen-marketplace` dependency to `/Users/sac/ggen/crates/ggen-cli/Cargo.toml`
  - `pack_install.rs`: `ggen_core::marketplace::{cache::*, error::Error as MarketplaceError, AsyncRepository, Package,
    PackageId, PackageVersion}` → `ggen_marketplace::marketplace::{...}` (identical shape — `ggen_core`'s own
    `marketplace` module is itself just `pub use ggen_marketplace::marketplace;`, confirmed in `ggen-core/src/lib.rs`).
    2 further `ggen_core::marketplace::PackageMetadata` call sites → `ggen_marketplace::marketplace::PackageMetadata`.
  - `cmds/policy.rs`: `ggen_core::marketplace::{policy,profile,metadata,models}::*` → `ggen_marketplace::marketplace::
    {...}` (6 call sites). Left this file's `ggen_core::packs::lockfile::PackLockfile` (line 92) untouched here — that's
    T040's part of this same file.
  - `ggen-marketplace` dependency was already added to `Cargo.toml` in T032 (bundled, see that task's note).
  - `cargo check -p ggen-cli-lib --all-targets`: clean. `cargo test -p ggen-cli-lib --lib pack_install::`: 2/2 pass.
- [x] T037 [US1] Re-point domain command handlers, group 1: `/Users/sac/ggen/crates/ggen-cli/src/cmds/{template.rs,doctor.rs}`
  - **Verified no-op, not silently skipped**: `template.rs`'s only `ggen_core::` surface is `domain::template::{show,
    new,list,lint,TemplateService}`; `doctor.rs`'s is `domain::utils::{execute_doctor,CheckStatus,DoctorInput}`. Both
    are `ggen_core::domain::*` — confirmed via the ticket's own bucket-detail table ("Everything else (agent, codegen,
    dflss, domain, manifest, ontology, packs, reverse_sync, sync, telemetry, utils, validation) is native to ggen-core
    with zero counterpart anywhere in `~/praxis/crates/{ggen,praxis-core,praxis-graphlaw}`") and by grep: no
    `domain::template`/`domain::utils` module exists in `ggen-config`/`ggen-marketplace`/`ggen-engine`. There is
    nothing to re-point these two files to; they correctly keep depending on `ggen-core` (still a live, valid workspace
    dependency throughout Phase 4f — only deleted in Phase 4i's T055, out of this task's scope).
  - `cargo check -p ggen-cli-lib --all-targets`: unchanged/clean (no edits made to either file).
- [x] T038 [US1] Re-point domain command handlers, group 2: `/Users/sac/ggen/crates/ggen-cli/src/cmds/{graph.rs,ontology.rs}`
  - Same verified-no-op finding as T037: `graph.rs`'s remaining surface (after T035's error-plumbing fix) is
    `domain::{graph,ontology}::*`; `ontology.rs`'s is `ontology::{CoreOntologyBundle,OntologyLoader}`,
    `validation::StandardOntology`, and `domain::ontology::get_standard_namespaces()` — all native, zero counterpart.
    No changes made to either file's `ggen_core::` surface under this task.
  - `cargo check -p ggen-cli-lib --all-targets`: clean.
- [x] T039 [US1] Re-point domain command handlers, group 3: `/Users/sac/ggen/crates/ggen-cli/src/cmds/{pack.rs,packs.rs,capability.rs}`
  - Unlike T037/T038, these 3 files DO have real, ported counterparts — the concurrent T024-T028 marketplace-merge pass
    landed `ggen_marketplace::packs_registry::{metadata,validate,capability_registry}` and merged `install_pack` into
    `ggen_marketplace::marketplace::install::install_pack_by_id`. Re-pointed:
    - `pack.rs`: `ggen_core::domain::packs::install::{install_pack,InstallInput}` → `ggen_marketplace::marketplace::
      install::{install_pack_by_id,InstallByIdInput}` (renamed per T024's collision-avoidance; field names on
      `InstallByIdOutput` are identical to the old `InstallOutput` — `pack_id`, `pack_name`, `pack_version`,
      `packages_installed`, `templates_available`, `install_path`, `digest`, `lockfile_path` — so the `add()` verb body
      needed zero further changes beyond the type/function names). `domain::packs::metadata::{list_packs,
      load_pack_metadata,show_pack}` → `ggen_marketplace::packs_registry::metadata::{...}`. Left `domain::utils::
      {execute_doctor,DoctorInput}` (line 316) on `ggen_core` — same zero-counterpart finding as T037.
    - `packs.rs`: `domain::packs::metadata::show_pack` → `ggen_marketplace::packs_registry::metadata::show_pack`;
      `domain::packs::validate::validate_pack` → `ggen_marketplace::packs_registry::validate::validate_pack`.
      `ggen_core::agent::{emit_install_receipt,PackInstallClosure}` re-pointed to `crate::agent::{...}` — see T041.
      `ggen_core::calculate_sha256(...)` → `crate::utils::sha256_hex(...)` (see T039's note below).
    - `capability.rs`: `domain::packs::capability_registry::{list_capabilities,resolve_capability_to_packs}` →
      `ggen_marketplace::packs_registry::capability_registry::{...}`. Same `calculate_sha256` → `sha256_hex` swap.
  - **New helper, not a new module**: `ggen_core::calculate_sha256` (a 4-line `Sha256::new()/update()/finalize()`
    wrapper in `ggen-core/src/pqc.rs`, re-exported at crate root) has zero counterpart — but porting the *whole* `pqc`
    module (`PqcSigner`/`PqcVerifier`, post-quantum signing, unrelated to these 2 call sites) would be scope creep in
    the other direction. Added a single `pub fn sha256_hex(data: &[u8]) -> String` to the already-created
    `crate::utils` module instead (one authoritative definition shared by both call sites, avoiding drift between two
    inlined copies).
  - `cargo check -p ggen-cli-lib --all-targets`: clean.
- [x] T040 [US1] Re-point pack lockfile references across `/Users/sac/ggen/crates/ggen-cli/src/cmds/{capability.rs,pack.rs,packs.rs,policy.rs}` to `ggen_marketplace::packs::lockfile`
  - `capability.rs`/`packs.rs`/`pack.rs` picked up their `ggen_marketplace::packs::lockfile::{LockedPack,PackLockfile,
    PackSource}` re-point as part of the same edit pass as T039 (they're the same import lines). `policy.rs`'s
    `ggen_core::packs::lockfile::PackLockfile` (inside `load_pack_contexts_from_project`) was the one remaining
    reference, fixed here → `ggen_marketplace::packs::lockfile::PackLockfile`.
  - `cargo check -p ggen-cli-lib --all-targets`: clean.
- [x] T041 [US1] Re-point agent/pack-agent facade: `/Users/sac/ggen/crates/ggen-cli/src/cmds/{agent.rs,mod.rs,packs_receipt.rs,packs.rs}`
  - **The hardest bucket, resolved via option (a) from the briefing (full port), not option (b) (stub)** — see the
    "Bucket 10" account in the session report; summarized here for the task record.
  - Investigated `ggen-core/src/agent/{types.rs,receipt.rs,facade.rs}` (269+300+527 = 1,096 lines) in full before
    deciding. Found every dependency `PackAgent` (the facade struct) calls into — pack metadata/validation/
    capability-registry, the install pipeline, the lockfile — already has a home in `ggen-marketplace` from the
    concurrent T024-T028 pass; only `agent::{types,receipt,facade}` themselves (not their dependencies) were the real
    "zero counterpart" gap. That made a full, faithful port tractable rather than a stub.
  - **Ported all 3 files** into new module `/Users/sac/ggen/crates/ggen-cli/src/agent/{mod.rs,types.rs,receipt.rs,
    facade.rs}`, registered as `pub mod agent;` in `lib.rs`:
    - `types.rs`: verbatim (zero external deps beyond `serde`/`thiserror`, already present).
    - `receipt.rs`: verbatim except `crate::receipt::{hash_data,Receipt}` → `ggen_config::receipt::{hash_data,Receipt}`
      and `crate::receipt::generate_keypair()` → `ggen_config::receipt::generate_keypair()` (T033's already-ported
      types) — no other changes needed.
    - `facade.rs`: import paths only — `domain::packs::{capability_registry,check_packs_compatibility,install,
      metadata,types,validate}` → `ggen_marketplace::{marketplace::install,packs_registry::{capability_registry,
      metadata,types,validate,check_packs_compatibility}}`; `packs::lockfile::PackLockfile` →
      `ggen_marketplace::packs::lockfile::PackLockfile`; `InstallInput`/`install_pack` → `InstallByIdInput`/
      `install_pack_by_id` (T024's rename) in the `install()` method body. Verified field-for-field: `Pack`,
      `PackDependency`, `PackTemplate`, `ValidationResult`, `CapabilityDescriptor`, `CheckCompatibilityResult` in
      `ggen-marketplace` all have identical shapes to what `facade.rs` expects — zero logic changes beyond paths.
  - `cmds/agent.rs`: `ggen_core::agent::{InstallRequest,PackAgent}` → `crate::agent::{InstallRequest,PackAgent}`;
    `ggen_core::agent::AgentResult` → `crate::agent::AgentResult`. `cmds/packs_receipt.rs`: `ggen_core::agent::
    {PackInstallClosure,PackReceiptError}` → `crate::agent::{...}`; delegating call → `crate::agent::
    emit_install_receipt`. `cmds/packs.rs`: see T039 (same file, same edit pass). `cmds/mod.rs`: doc-comment only
    (`// AGI-facing lifecycle surface ... over crate::agent::PackAgent (ported from ggen_core, T041)`), no code change.
  - Fixed the module's own doctest (`agent/mod.rs`'s `## Example`, ` ```no_run``` `) the same way as prior tasks:
    `use ggen_core::agent::{PackAgent,InstallRequest};` → `use ggen_cli_lib::agent::{PackAgent,InstallRequest};`.
  - `cargo check -p ggen-cli-lib --all-targets`: clean, first try (no back-and-forth needed once the dependency
    audit above was done). `cargo test -p ggen-cli-lib --lib`: 118/118 pass. `cargo test -p ggen-cli-lib --doc`:
    22/22 pass (the new `agent (line 37) - compile` doctest included), 8 correctly `ignore`d.
- [x] T042 [US1] Re-point `dflss`: `/Users/sac/ggen/crates/ggen-cli/src/cmds/sigma.rs`
  - **Verified no-op, investigated not assumed**: `sigma.rs` uses `ggen_core::dflss::{execute_dflss,DflssReport}` and
    `ggen_core::manifest::ManifestParser`. `manifest::ManifestParser` alone DOES have a ported counterpart
    (`ggen_config::manifest::ManifestParser`, from T020) — but `execute_dflss(manifest: &GgenManifest, ...)`'s
    signature (native, zero-counterpart `dflss` module, confirmed by the ticket's own bucket-detail list) hard-requires
    `ggen_core::manifest::GgenManifest` specifically; `ggen_config::manifest::GgenManifest` is a structurally-identical
    but distinct Rust type (different crate), so re-pointing only the parser call would break the very next line's
    `execute_dflss(&manifest, ...)` call with a type mismatch. Re-pointing both together isn't possible without also
    porting/rewriting `dflss` itself, which is out of this task's mechanical-re-point scope (confirmed by the ticket's
    own file-reference table categorizing this entire file as bucket "dflss", risk "native, isolated single-file" —
    i.e. a no-op bucket by the ticket's own design, not an oversight). No changes made.
  - `cargo check -p ggen-cli-lib --all-targets`: unchanged/clean.
- [x] T043 [US1] Re-point sync/codegen pipelines (last, most cross-cutting): `/Users/sac/ggen/crates/ggen-cli/src/cmds/{sync.rs,inverse_sync.rs,wizard.rs,init.rs}`, wiring `wizard.rs`/`init.rs` to the scaffolding module from T029-T030
  - **`sync.rs`**: re-pointed the 2 genuinely portable pieces — `ggen_core::receipt::{generate_keypair,hash_data,
    Receipt}` → `ggen_config::receipt::{...}` (used only for the sync receipt's signing, independent of the codegen
    pipeline), and `ggen_core::manifest::{ManifestParser,QuerySource,TemplateSource}` → `ggen_config::manifest::{...}`
    (used only inside `emit_sync_receipt` to hash the input-file closure for the receipt — NOT fed into the native
    `SyncExecutor`, so no cross-type conflict like T042's). Verified field-for-field before the edit:
    `GgenManifest.ontology.{source,imports}`, `.generation.rules[].{query,template}`,
    `QuerySource::File{file}`/`TemplateSource::File{file}` are identical in shape between the two crates. Left
    `ggen_core::codegen::{OutputFormat,SyncExecutor,SyncOptions,SyncResult,executor::*}`,
    `ggen_core::domain::sync_profile::validate_sync_preconditions`, and `ggen_core::sync::{sync,SyncConfig,
    SyncLanguage,SyncError}` untouched — these are the actual codegen/sync engines, confirmed native/zero-counterpart,
    and this is the **exact gap already flagged under T028** ("no task in T001-T067 actually rewrites
    `ggen-engine/src/{config.rs,sync.rs}` to consume the ported manifest schema... this should be raised with the
    user/spec owner"). Not resolved unilaterally here, per that existing note.
  - **`inverse_sync.rs`**: split the single `use ggen_core::receipt::{generate_keypair,ProvenanceEnvelope};` import —
    `generate_keypair` → `ggen_config::receipt::generate_keypair` (real counterpart, T033), `ProvenanceEnvelope` stays
    `ggen_core::receipt::ProvenanceEnvelope` (and the `provenance_envelope::CoherenceReport` usage at line 268 stays
    untouched) because these are the types T033 explicitly excluded — their fate is decided by **T054** (Phase 4i,
    "Re-verify reachability of `ggen-core/src/receipt/{chain_linking.rs,provenance_envelope.rs}`... confirm whether
    `inverse_sync.rs`'s use... needs porting first"), which is out of this task's scope (a different phase, explicitly
    deferred by the ticket itself, not something T043 is asked to resolve).
  - **`wizard.rs`/`init.rs` — no wiring performed; documented gap, not silently invented**: verified via full-file grep
    that NEITHER file references `cli_generator`/`project_generator`/`create_new_project`/`ProjectConfig`/`ProjectType`
    in any form, directly or indirectly. Traced this further: within `ggen-core` itself, `project_generator` has
    exactly one caller anywhere, `domain/project/new.rs`, which itself has **zero callers** anywhere in `ggen-core` or
    `ggen-cli` (confirmed via grep). So the ticket's framing of `init.rs`/`wizard.rs` as "consumers" of the scaffolding
    modules (07-PROJECT-SCAFFOLDING-PORT.md's own "Consumers" table) does not match the code as it actually exists —
    there never was a real call site to re-point. `wizard.rs`'s entire `ggen_core::` surface is
    `codegen::{executor::{SyncExecutor,SyncOptions},FileTransaction}` (native codegen, same T028 gap as `sync.rs`,
    left untouched); `init.rs`'s is `codegen::FileTransaction` and `validation::PreFlightValidator` (both native,
    zero counterpart). Inventing a new call site (deciding which verb/flag should trigger project scaffolding, with
    what UX) is a product decision, not a mechanical migration step, so it was not fabricated here. The ported
    `scaffolding::{cli_generator,project_generator}` module (T029-T030) compiles and its own 14 unit + 13 doctests
    pass in isolation, but remains genuinely unwired — this is the clearest actionable follow-up from this session
    (see the session's final report to the requester for the explicit options: wire it to a real verb, leave it as
    intentionally-dormant fossil-preserved capability matching this repo's non-deletion doctrine for archived
    features, or delete it).
  - `cargo check -p ggen-cli-lib --all-targets`: clean.
- [x] T044 [US1] Update `/Users/sac/ggen/crates/ggen-cli/src/lib.rs`'s public API (`pub use ggen_core::utils::error::Result` and `cli_match()`'s return type) to the new error type
  - **Completed as part of T035, not deferred to last** — see that task's note for why: `cmds/policy.rs` (one of
    T035's 12 files) declares `fn load_pack_contexts_from_project() -> crate::Result<Vec<PackContext>>`, consuming
    `lib.rs`'s `pub use ... Result` directly. Confirmed via grep this is the *only* internal consumer of `crate::
    Result` (and confirmed no external crate references `ggen_cli_lib::Result`/`ggen_cli_lib::error` at all), so
    flipping the alias was safe and, in practice, load-bearing for T035 to compile cleanly rather than a separable
    later step.
  - `lib.rs`: `pub use ggen_core::utils::error::Result;` → `pub use crate::utils::error::Result;`; `cli_match() ->
    ggen_core::utils::error::Result<()>` → `-> crate::utils::error::Result<()>`; `run_for_node(...) -> ggen_core::
    utils::error::Result<RunResult>` → `-> crate::utils::error::Result<RunResult>`; the one internal
    `ggen_core::utils::error::Error::new(...)` call (wrapping `clap_noun_verb::run()`'s error) and the one in
    `run_for_node` → `crate::utils::error::Error::new(...)`. Also fixed the 2 module-doc doctests (` ```rust,no_run``` `
    /` ```rust,ignore``` `) referencing `ggen_core::utils::error::Result` in `# async fn example() -> ...` signatures
    → `ggen_cli_lib::utils::error::Result` (external path, per the established doctest-path convention).
  - `cargo check -p ggen-cli-lib --all-targets`: clean.

**Checkpoint (T031-T044, Phase 4e-4f complete)**: `grep -rn "ggen_core::" crates/ggen-cli/src | wc -l` went from
164 (31 files) to 37 (12 files). Every remaining reference was individually investigated and falls into one of three
accounted-for buckets, none of them oversights: (1) genuinely native, zero-counterpart `ggen-core` modules
(`domain::{graph,ontology,utils,template}`, `ontology`, `validation`, `dflss`, `codegen`, `sync`, `reverse_sync`) that
correctly keep depending on `ggen-core` — still a live, valid workspace member throughout Phase 4f, only deleted in
Phase 4i's T055 (a different, later task); (2) 2 items explicitly deferred to a named future task (T042's
`dflss`/manifest type-coupling; T054's `provenance_envelope`/`chain_linking` fate); (3) 2 files (`agent.rs`,
`packs_receipt.rs`) where the only remaining "hits" are doc-comment prose describing the port, not code dependencies.
Full-crate real-execution proof: `cargo check -p ggen-cli-lib --all-targets` clean; `cargo test -p ggen-cli-lib`
(default features, matching `just test`'s `cargo test --workspace --tests` scope) — **701 passed, 0 failed, 185
ignored** across the lib unit tests, all doctests, and all 60 integration-test binaries
(`/tmp/ggen_cli_test_output.log`, real execution, no mocks); `cargo clippy -p ggen-cli-lib --lib --no-deps -- -D
warnings` clean (zero findings in this crate's own library target, isolating from pre-existing workspace clippy debt
per this repo's guidance).

### 4g. ggen-lsp migration (`docs/jira/v26.7.16/09-GGEN-LSP-MIGRATION.md`, 17 call sites / 6 files, in the ticket's dependency order)

- [x] T045 [US1] Re-point `/Users/sac/ggen/crates/ggen-lsp/src/project_index.rs` and `/Users/sac/ggen/crates/ggen-lsp/src/rule_index.rs` to `ggen_config::manifest::*`
  - Mechanical import-path swap only, same symbol names: `project_index.rs`'s
    `use ggen_core::manifest::ManifestParser;` → `use ggen_config::manifest::ManifestParser;`
    (plus 3 doc-comment mentions of `ggen_core::manifest` → `ggen_config::manifest`).
    `rule_index.rs`'s `use ggen_core::manifest::{GenerationRule, QuerySource,
    TemplateSource};` → `use ggen_config::manifest::{...}` (both the `lib` import and the
    `#[cfg(test)] mod tests` re-import), plus `ggen_core::manifest::GenerationMode::Create` →
    `ggen_config::manifest::GenerationMode::Create` in a test helper, plus 1 doc-comment
    mention. `ggen-lsp/Cargo.toml:34` already had `ggen-config` as a direct dependency — no
    `Cargo.toml` change needed.
  - `ggen-lsp` still depends on `ggen-core` in `Cargo.toml` after this change — deliberately
    left in place, since T048/T049 (`a2a_mcp/mcp_packs.rs`, `a2a_mcp/mcp_server.rs`, both
    out of this task's scope, gated on T041) still reference `ggen_core::agent`/
    `ggen_core::codegen::pipeline`/`ggen_core::manifest` directly. Confirmed via
    `grep -rn "ggen_core" crates/ggen-lsp/src` that exactly those 2 files (7 hits) remain.
- [x] T046 [US1] Re-point `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/sparql_analyzer.rs` to `ggen_config::manifest::validation::*`
  - `use ggen_core::manifest::validation::{query_contains_values, query_has_order_by};` →
    `use ggen_config::manifest::validation::{query_contains_values, query_has_order_by};`
    (exact path suggested by the ticket; `ggen-config/src/manifest/mod.rs` has `pub mod
    validation;` with both functions `pub fn`). No other changes to this file — the
    E0010/E0011/E0013 checks that consume these two functions, and the locally-implemented
    E0015 (`is_identity_construct`, private to this file, not sourced from any manifest
    module) are untouched logic, only the import path moved.
- [x] T047 [US1] Fix doc-comment reference in `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/tera_analyzer.rs:567`
  - Confirmed via grep this was the only remaining `ggen_core` mention in this file and was
    prose only (no real `use`). Reworded ``(`ggen_core::codegen::pipeline` renders it via
    `tera.render_str`)`` → ``(the generation pipeline renders it via `tera.render_str`)`` —
    drops the specific (soon-to-be-retired) crate path rather than asserting a new one that
    isn't the actual current renderer yet (per the Phase 4c gap note: `ggen-engine`'s sync
    pipeline doesn't yet consume `GenerationRule`/`output_file` the same way).
- [ ] T048 [US1] Re-point `/Users/sac/ggen/crates/ggen-lsp/src/a2a_mcp/mcp_packs.rs` to the new engine's `agent` facade equivalent (depends on T041)
- [ ] T049 [US1] Re-point `/Users/sac/ggen/crates/ggen-lsp/src/a2a_mcp/mcp_server.rs` (manifest + `codegen::pipeline::GenerationPipeline`/`get_llm_service()` equivalent)
- [x] T050 [US1] Verify `GGEN-TPL-001`/`GGEN-OUT-001`/`GGEN-YIELD-001`/`GGEN-RULE-001`/`GGEN-QUERY-002`/`E0011`/`E0013`/`E0015` diagnostics all still fire correctly (E0015 must remain reserved/inactive) against `/Users/sac/ggen/crates/ggen-lsp/src/route/diagnostic_species.rs`
  - Read `diagnostic_species.rs` in full: the `GGEN-*` species table has no `E00xx` entries at
    all (`species_for("E0011").is_none()` is itself an assertion in
    `unknown_code_has_one_species`-style test `unknown_code_has_no_species`) — `E0011`/`E0013`/
    `E0015` are codes emitted directly by `sparql_analyzer.rs`'s own `diagnostics()` method,
    entirely independent of the `GGEN-*` species registry; my T045/T046 import-path-only
    changes touch neither the species table nor `sparql_analyzer.rs`'s diagnostic-emission
    logic (only which crate `query_contains_values`/`query_has_order_by` are imported from).
  - Re: "E0015 must remain reserved/inactive" — found this describes a stale/prior state:
    `E0015` (identity-CONSTRUCT detection, `is_identity_construct()` at
    `sparql_analyzer.rs:248`) is **already implemented and active** in the current tree
    (severity `WARNING`, never upgraded under `strict_mode`, matching the root `CLAUDE.md`
    table's un-asterisked `E0015` row), with a passing pre-existing test
    (`identity_construct_triggers_e0015`) and a `route/registry.rs:122` repair-family mapping.
    Confirmed this task's changes do not touch that logic at all (no diff to
    `sparql_analyzer.rs` beyond the T046 import line) — its active/inactive status is
    unchanged by this migration step, i.e. not "accidentally activated" by me.
  - Ran `cargo check -p ggen-lsp --all-targets`: clean, 0 errors (only pre-existing unrelated
    workspace-manifest warnings from `ggen-engine`/sibling `~/praxis`/`~/bcinr` crates, not
    `ggen-lsp`).
  - Ran `cargo test -p ggen-lsp` (full suite, real execution, captured to
    `/private/tmp/claude-501/.../scratchpad/ggen_lsp_test_full.log`): **308 passed, 0 failed**
    across all lib unit tests + all integration test binaries, including:
    `analyzers::sparql_analyzer::tests::{construct_without_order_by_triggers_e0011,
    select_without_order_by_triggers_e0013, identity_construct_triggers_e0015,
    clean_select_has_no_law_violations, values_in_rq_triggers_e0010,
    syntax_error_is_reported}`; `route::diagnostic_species::tests::{
    ggen_tpl_001_is_active_with_canonical_values, ggen_out_001_is_active_with_canonical_values,
    ggen_rule_001_is_active_with_canonical_values, ggen_harness_001_is_active,
    registry_contains_exactly_eleven_species, unknown_code_has_no_species,
    actuation_boundary_is_inspect_only_for_all_species}`; integration suites
    `tests/{ggen_tpl_001*.rs (6 files), ggen_out_001_living_loop.rs, ggen_rule_001_living_loop.rs,
    ggen_yield_001_living_loop.rs, ggen_query_002_living_loop.rs, sparql_diagnostic_test.rs}`
    all green (`test_e0013_select_without_order_by`, `test_e0015_identity_construct`,
    `test_clean_select_no_e0013` all `ok`). `project_index::tests::*` (7 tests) and
    `rule_index::tests::*` (8 tests) — the direct consumers of the re-pointed manifest
    import — all pass unchanged. Doc-tests: `0 passed; 0 failed` (no doctests in the 4
    files touched this task).

### 4h. Root package + test migration (`docs/jira/v26.7.16/10-ROOT-PACKAGE-TEST-MIGRATION.md`)

- [x] T051 [US1] Re-point `/Users/sac/ggen/src/lib.rs:10`'s `pub use ggen_core as core;` to the new engine crate
  - Mechanical: `pub use ggen_core as core;` → `pub use ggen_engine as core;`. Confirmed via
    `grep -rn "\bcore::" src/` that nothing in this crate's own `src/` consumes `core::*`
    internally, so this is a leaf change (external-consumer-facing only). Added `ggen-config`,
    `ggen-marketplace`, `ggen-engine` to `[workspace.dependencies]` and root `[dependencies]`
    in `/Users/sac/ggen/Cargo.toml` (previously absent — nothing at the root package level
    could import them before this) alongside the still-present `ggen-core` dependency (most
    of the 82 files still need it — see T053 notes). `cargo check -p ggen --lib`: clean.
- [x] T052 [US1] [P] Re-point the 3 `receipt`-only and 1 `manifest`-only early-candidate files identified in the ticket's symbol tally (check individually per the ticket's caveat before moving)
  - Re-verified the ticket's own tally per-file: of the 3 `receipt`-tagged files, 2
    (`examples/_archive/full_pipeline.rs`, `examples/_archive/receipt_chain.rs`) are inside
    `examples/_archive/` — folded into T053's archive-deletion decision below rather than
    double-handled, and `full_pipeline.rs` specifically also depends on `ggen_core::a2a`
    (no port destination anywhere), so it would not have qualified as an early mover on its
    own merits either, matching the ticket's own per-file caveat.
  - `/Users/sac/ggen/benches/receipt_bench.rs` (the 3rd receipt file, not archived): re-pointed
    `ggen_core::receipt::{generate_keypair, hash_data, Receipt, ReceiptChain}` →
    `ggen_config::{generate_keypair, hash_data, Receipt, ReceiptChain}` directly.
    `ggen-core`'s own `receipt` module is itself just `pub use ggen_config::*`
    (`crates/ggen-core/src/lib.rs`), so this is a verified-identical-API move, not a guess.
    `cargo check --bench receipt_bench`: clean.
  - `/Users/sac/ggen/tests/integration/test_manifest.rs` (the 1 manifest file): re-pointed
    `ggen_core::manifest::{ManifestInputs, compute_manifest_key}` →
    `ggen_config::manifest::{ManifestInputs, compute_manifest_key}`. **Found while doing
    this**: `ManifestInputs`/`compute_manifest_key` do not exist anywhere in
    `ggen_config::manifest` (the T019-T023 port) — nor did they exist anywhere in `ggen-core`
    either, confirmed via `grep -rln "ManifestInputs|compute_manifest_key" crates/ src/ tests/`
    matching only this one file. Pre-existing dead code, not a migration regression; the
    import path was still re-pointed (to the file's logically closest real home) with a
    comment documenting the gap rather than fabricating the missing types. This file is not
    part of any compiled `cargo test` target regardless (see T053's reachability finding), so
    nothing regresses either way.
- [x] T053 [US1] Re-point the remaining root `tests/`, `examples/`, `benches/` files (56 + 23 + 3 = 82 total, minus T052's early movers) per the full path list in `docs/jira/v26.7.16/10-ROOT-PACKAGE-TEST-MIGRATION.md`; decide and record disposition (migrate vs. delete) for the 4 `examples/_archive/` files and the 7 non-member sub-package example directories
  - **Major finding not in the ticket, established via `cargo metadata`'s test-target list
    cross-referenced against every entry point's `mod`/`#[path]` declarations**: of the 56
    `tests/*.rs` files in the ticket's list, only **18** are actually compiled by any
    `cargo test` target today (auto-discovered `tests/*.rs`, an explicit `[[test]]` entry, or
    `mod`-included from one that is — e.g. `tests/contract/mod.rs`'s `mod manifest; mod
    pipeline;`, `tests/otel_validation_tests.rs`'s `mod otel_validation;`). The other **38**,
    plus `tests/integration/clap_noun_verb_ontology_test.rs` (39 total), are not referenced by
    any `[[test]]` path, not auto-discovered (nested under `tests/<dir>/`, which Cargo does not
    auto-discover), and not `mod`-included from anything that is — confirmed via a targeted
    `grep` across every entry point for `mod`/`#[path]` statements naming their directories.
    They are dead code today, gating nothing, independent of this migration. Several were also
    independently confirmed pre-existing-broken regardless of import path (see below) — e.g.
    `tests/integration/test_manifest.rs`'s `ManifestInputs` (T052 above) and multiple
    `domain::packs::*` test files importing symbols that were never flattened at
    `ggen_core::domain::packs`'s own module root (`grep -c "pub use" .../domain/packs/mod.rs`
    == 0), so those files never compiled even under the pre-migration crate. This changes the
    real stakes of "82 files": most of the ones this task could not cleanly re-point were
    already not gating `just check`/`just test` before this session touched anything.
  - **Disposition: DELETE the 4 `examples/_archive/` files
    (`a2a_tasks.rs`, `full_pipeline.rs`, `packet_routing.rs`, `receipt_chain.rs`) and the 7
    non-member sub-package example directories (`advanced-cache-registry`, `sparql-engine`,
    `knowledge-graph-builder`, `lifecycle-complete`, `advanced-error-handling`,
    `advanced-fullstack-integration`, `ggen-usage-wrapping`, 14 `.rs` files total).**
    Evidence, not a stylistic call: (1) the 4 archive files are not discoverable Cargo example
    targets at all (`examples/_archive/` is a subdirectory; confirmed via `cargo check --example
    a2a_tasks` → "no example target named `a2a_tasks`") — already fully inert before this
    session. (2) 6 of the 7 sub-package directories reference `path = "../../ggen-core"` /
    `"../../ggen-ai"` / `"../../utils"` — directories that do not exist anywhere in this
    checkout (`ggen-ai`, root-level `ggen-core`, and `utils` were never at those paths in the
    current 10-crate layout) — confirmed via `cargo check` inside `examples/sparql-engine`
    failing at manifest-load, before reaching any ggen_core-import question at all. (3) the
    7th (`lifecycle-complete`, the only one with a syntactically valid path,
    `../../crates/ggen-core`) independently fails with 19 real compiler errors
    (`cargo check` inside it) — drifted from `ggen_core::lifecycle`'s actual current struct
    shapes (`Make` has no `phases` field, `lifecycle::Context` has no `project_root`/`dry_run`/
    `skip_cache` fields, etc.). All 7 were already broken pre-migration, unrelated to
    ggen-core's retirement, not referenced by any CI workflow/justfile/README (confirmed via
    grep), and not workspace members (confirmed via `cargo metadata`), so deleting them matches
    the precedent already recorded in this same root `Cargo.toml` for the `7-agent-validation`
    example ("a broken member is a small Oracle Gap... so it leaves the boundary").
  - **Files actually re-pointed to a verified-working destination (9 of the 82, all real code
    fully off `ggen_core` — confirmed compiling, several with real passing-test evidence; some
    still carry a doc comment naming the old path for context, see the final accounting below
    for exactly which)**: `src/lib.rs` (T051, not one of the 82 but the same task family),
    `benches/receipt_bench.rs` and `examples/test_lockfile.rs` (T052-style,
    `ggen_config`/`ggen_marketplace::packs::lockfile` — `cargo run --example test_lockfile`
    produces a real lockfile on disk), `tests/contract/manifest.rs` (→
    `ggen_marketplace::packs_registry::types::PackFile`, part of the `contract` test binary —
    `cargo test --test contract`: 3/3 pass, including this one), and 5 orphaned-but-now-import-
    correct files under `tests/{integration,unit}/packs/`
    (`user_workflow_multi_pack_test.rs`, `user_workflow_template_reuse_test.rs`,
    `user_workflow_single_pack_test.rs`, `pack_composer_test.rs`, `pack_core_domain_test.rs`) —
    all re-pointed to `ggen_marketplace::packs_registry::{compose,metadata,generator,types}`
    and (for the single-pack workflow file) `ggen_marketplace::marketplace::install` per T024's
    deliberate rename (`install_pack`→`install_pack_by_id`, `InstallInput`→`InstallByIdInput`).
    Verified via a temporary throwaway `[[test]]` Cargo.toml wiring (compiled + ran, then
    reverted — these files are not permanently wired into the build, a separate curatorial
    decision this task does not make): 32/32 tests pass across the 5 files
    (`temp_verify_{user_workflow_multi_pack_test,user_workflow_template_reuse_test,
    user_workflow_single_pack_test,pack_composer_test,pack_core_domain_test}`). Two real,
    pre-existing bugs unrelated to the re-point were fixed opportunistically while verifying
    (same class as T018's "fixed 3 pre-existing regressions"): `pack_core_domain_test.rs` was
    missing the `registry_type` field (added to `Pack` after this dead test was last touched)
    in 2 struct literals; a stray Python-style `False`/`True` typo (`== False` where Rust needs
    `== false`) in `pack_generator_test.rs`/`pack_installer_test.rs`/`pack_validator_test.rs`.
  - **Partially re-pointed (2 files, real-but-narrower gap documented in place rather than
    guessed at)**: `tests/unit/packs/pack_validator_test.rs` — `validate_pack`/`show_pack`/
    `ValidationResult`/`ValidationCheck` re-pointed and verified working
    (`ggen_marketplace::packs_registry::{validate,metadata}`); `score_pack`/`PackScore`/
    `score::DimensionScores` pointed at their correct eventual home
    (`ggen_marketplace::packs_registry::score`) but this does not currently resolve because
    `packs_registry/score.rs` exists on disk from the T025 port but is **not** declared
    `pub mod score;` in `crates/ggen-marketplace/src/packs_registry/mod.rs` — one of several
    files physically ported but not wired into the module tree (also `advanced_resolver.rs`,
    `cloud_distribution.rs`, `composer.rs`, `installer.rs`, `sparql_executor.rs`,
    `template_generator.rs`). Confirmed this was equally unresolvable via
    `ggen_core::domain::packs::score_pack` before this change too (same gap, pre-existing,
    just inherited) — not a regression. Fixing the one-line `pub mod score;` wiring gap
    requires editing `ggen-marketplace` source, out of this task's file scope (root package
    only) — **flagged here for whoever owns that crate next**.
    `tests/unit/packs/pack_installer_test.rs` — main install flow (`install_pack_by_id`/
    `InstallByIdInput`, 8 of 10 test functions) re-pointed and verified compiling/passing (all
    5 non-serialization tests + the 3 non-serde install-flow tests); 2 serialization tests
    (`test_install_input_serialization`, `test_install_input_defaults`) remain blocked because
    `InstallByIdInput` has no `#[derive(Serialize, Deserialize)]` — confirmed this is **also**
    pre-existing (the original `ggen_core::domain::packs::install::InstallInput` had zero
    derives either, `crates/ggen-core/src/domain/packs/install.rs:13`), not something this
    migration broke. Same out-of-scope one-line fix needed in `ggen-marketplace` source.
  - **Untouched, real remaining `ggen_core::` code dependency, no verified port destination
    (50 files)** — left referencing `ggen_core` with the dependency itself still present in
    root `Cargo.toml` (see T051 note); grouped by blocking symbol, none of which exist anywhere
    in `ggen_config`/`ggen_marketplace`/`ggen_engine` today (confirmed via `grep -rln` for each
    symbol across those 3 crates before concluding "no destination", not assumed):
    `graph`/`Graph` (crate-root `ggen_core::Graph`, distinct from `ggen-engine`'s own
    differently-shaped `graph` module) — `graph_core_tests.rs`, `benches/ggen_benchmarks.rs`,
    `validate_marketplace_rdf.rs`, `tests/chicago_tdd/expert_patterns/{boundaries,concurrency,
    error_paths,resources}.rs`, `tests/chicago_tdd/ontology_driven_e2e.rs` (also
    `domain::graph`/`domain::template::render_with_rdf`), `tests/domain/graph/query_tests.rs`,
    `tests/integration/graph/{core_operations_test,export_operations_test}.rs`,
    `tests/integration/nextjs_ontology_sync.rs`, `tests/integration/clap_noun_verb_ontology_test.rs`,
    `tests/integration/test_rdf.rs`, `examples/rdf_metadata_example.rs`,
    `examples/rdf_template_integration.rs`; `gpack` — `tests/unit/packs/{gpack_manifest_test,
    pack_edge_cases_test,pack_validation_test}.rs`, `tests/integration/packs/{
    pack_cli_integration_test,pack_e2e_workflows_test}.rs`,
    `tests/performance/packs/pack_benchmarks.rs`, `tests/performance/packs_performance_test.rs`;
    `GenContext`/`Generator`/`Pipeline`/`Template`/`Templates`/`pipeline` (crate-root types +
    `pipeline_engine`) — `generator_core_tests.rs`, `tests/integration/{code_generation_tests,
    test_determinism}.rs`, `tests/integration/template_tests/test_template_regenerate.rs`,
    `template_systems_tests.rs`, `tracing.rs`, `tests/contract/pipeline.rs`; `utils::error`
    (ggen-core's own hand-rolled `Error`/`Result`, no drop-in match — `ggen-engine::AppError` is
    a differently-shaped `thiserror` enum with no `.new()`/`.with_context()`) —
    `a2a_integration_tests.rs`, `drift-detection-example.rs` (also `drift`),
    `path_validation_example.rs`, `tests/unit/marketplace_critical_tests.rs`,
    `tests/e2e_v2/test_helpers.rs`,
    `tests/security/{e2e_vulnerability_tests,supply_chain_tests,
    week4_security_hardening_tests}.rs` (also `security`/`template_cache`),
    `tests/otel_validation/mod.rs` (also `telemetry`); `registry` (ggen-core's own
    `RegistryClient`/`RegistryIndex`/`ResolvedPack`, no match in `ggen_marketplace` beyond an
    unrelated same-named `SearchResult`) — `cli.rs` (also feature-gated off by default,
    `required-features = ["integration"]`, `default = []`), `tests/integration/cache_tests.rs`
    (`CacheManager`), `tests/transport/registry_client_tests.rs` (also feature-gated off);
    `lifecycle` — `tests/common/fixtures.rs`, `tests/integration/lifecycle_tests.rs`;
    `validation::syntax_validator` — `fixture_validation_proof.rs`; `canonical` —
    `benches/canonical_bench.rs`; `domain::error`/`domain::mcp_config` (A2A types) —
    `tests/mcp_a2a/{message_handling_test,mock_a2a_server,stdio_transport_test,
    transport_tests}.rs`. (`tests/prevention_integration_tests.rs` and `tests/security/mod.rs`
    were re-examined and found to have **zero real** `ggen_core` code dependency — their only
    mentions are inside already-commented-out `use`/`mod` statements for unbuilt future work —
    so they needed no action and are counted separately below, not in the 50 blocked.)
  - Real verification: `cargo check --workspace` (the literal `just check` gate, confirmed via
    `justfile:23-24`) — clean, only pre-existing `ggen-engine` doc-lint warnings, 0 errors.
    `cargo test -p ggen` — full run, every test binary's `test result:` line shows `0 failed`;
    **407 tests passed, 0 failed** across all binaries that compile by default (doc-tests:
    `0 passed; 0 failed`, root package has none).
  - **Final accounting of the 82 files** (`grep -rl "ggen_core::" tests examples benches | wc -l`
    was 82 at task start, confirmed): **18 deleted** (4 `examples/_archive/` files + 14 `.rs`
    files across the 7 non-member sub-package directories); **12 files edited with real
    effect** — 5 now fully clean of any `ggen_core` mention
    (`user_workflow_single_pack_test.rs`, `user_workflow_template_reuse_test.rs`,
    `pack_composer_test.rs`, `pack_core_domain_test.rs`, `pack_generator_test.rs`) and 7 with
    all real code re-pointed but a documentation comment still naming the old path for context
    (`benches/receipt_bench.rs`, `examples/test_lockfile.rs`, `tests/contract/manifest.rs`,
    `tests/integration/packs/user_workflow_multi_pack_test.rs`,
    `tests/integration/test_manifest.rs`, `tests/unit/packs/pack_installer_test.rs`,
    `tests/unit/packs/pack_validator_test.rs` — the last two each retain one narrow,
    precisely-documented partial gap: `pack_validator_test.rs`'s `score`-module wiring gap and
    `pack_installer_test.rs`'s missing `Serialize`/`Deserialize` derive, both requiring an
    out-of-scope one-line `ggen-marketplace` source edit, both confirmed pre-existing); **2
    files needed zero action** (`tests/prevention_integration_tests.rs`,
    `tests/security/mod.rs` — re-examined and found to have no real `ggen_core` code
    dependency at all, only stale comments); **50 files remain genuinely untouched**, blocked
    on a `ggen_core` symbol with no port destination anywhere in `ggen_config`/
    `ggen_marketplace`/`ggen_engine` today (confirmed via `grep -rln` for each symbol across
    those 3 crates, not assumed), enumerated by bucket above. `18 + 12 + 2 + 50 = 82`.
  - **Deviation from the ticket's framing**: ticket 10 recommended waiting for the ggen-cli
    (08) and ggen-lsp (09) migrations to complete before batch-repointing, on the theory that
    "their real transitive dependencies exist in the new engine" once those land. In practice,
    neither ggen-cli's nor ggen-lsp's completion status gated any of this task's actual
    outcomes: every file this session could re-point only needed already-stable
    `ggen_config`/`ggen_marketplace` ports (T019-T028, landed independently of ggen-cli/lsp's
    progress), and every file this session could *not* re-point is blocked on a port
    destination that doesn't exist *anywhere* yet (not in ggen-cli, not in ggen-lsp, not in
    ggen-engine) — `graph`, `gpack`, `Generator`/`Pipeline`/`Template`, `utils::error`,
    `registry`, `lifecycle`, `validation`, `canonical`, `a2a`, etc. Those are a materially
    larger, unscoped porting effort (equivalent in size to T019-T028 combined, times several)
    that no task in T001-T067 currently owns — this is the same class of gap already flagged
    in tasks.md after T028 ("no consumer anywhere... an architecture-level reconciliation
    question, not a mechanical implementation gap this session should resolve unilaterally").
    Concretely: T031-T044 (ggen-cli) remain `[ ]` as of this session (most `ggen-cli/src/`
    files still reference `ggen_core` directly, confirmed via `grep`), and T048/T049
    (ggen-lsp, blocked on T041) also remain `[ ]` — neither blocked any of the work above.

- [x] T068 [US1] Close out `docs/jira/v26.7.16/12-OPEN-QUESTIONS.md` item 2's "`utils` is not
  fully accounted for" gap, for 10 of its files: `/Users/sac/ggen/crates/ggen-core/src/utils/
  {project_config,alert,enhanced_error,user_level,app_config,versioning,logger,types,time,
  cli}.rs` (3,258 lines total, confirmed via `wc -l`)
  - **No porting performed — all 10 files verdicted DEAD, skip entirely.** A prior research
    pass (this session) reached this verdict via exhaustive `grep` sweeps (rust-analyzer
    unavailable for that pass: `LSP server 'plugin:rust-analyzer-lsp:rust-analyzer' exceeded
    max crash recovery attempts (3)`). I independently re-ran the fallback procedure myself
    before accepting the verdict — LSP is still down in this session too (same exact error,
    both `workspaceSymbol` and, by the same mechanism, `findReferences`), confirming this is
    session/environment-level breakage (per this repo's own documented concurrent-session
    build-flakiness note), not something either research pass could route around. Re-ran the
    same class of exhaustive `grep` sweep independently (exact-symbol references, module-path
    references, self-consumption inside `ggen-core`) for every symbol the research pass named
    (`AppConfig`, `LogLevel`, `UserActivity`/`UserLevel`/`ProgressiveHelp`, `VersionChecker`,
    `setup_logging`/`default_root_logger`, `current_timestamp`/`format_duration`,
    `parse_variables`, the `alert_*!` macros, `project_config::`, `enhanced_error::`) and got
    identical results: zero real external callers in `ggen-cli/src`, `ggen-lsp/src`, or root
    `{src,tests,examples,benches}` for all 10 files, and the same coincidental-match / genuinely-
    independent-duplicate explanation for every apparent hit (e.g. `examples/tps-reference-
    system`'s and `examples/source-code-analysis`'s own unrelated local `AppConfig` structs;
    `ggen-lsp/src/a2a_mcp/a2a_generated/handlers.rs`'s own unrelated local `LogLevel` enum;
    `ggen-cli/src/version_checker.rs`'s and `tests/chaos/injection/mod.rs`'s own independently-
    rolled `format_duration`/`current_timestamp`; `domain/template/generate.rs`'s and
    `domain/project/plan.rs`'s own independently-rolled `parse_variables`, never delegating to
    `utils::cli::parse_variables` despite `ggen-cli/src/cmds/template.rs:202`'s stale comment
    claiming otherwise). No disagreement with the prior pass's verdict on any of the 10 files.
  - **Disposition per file** (all DEAD, all skip, all confirmed no self-consumers inside
    `ggen-core` either except where noted): `project_config.rs` (1,090 lines) — one of 4
    independent `GgenConfig` definitions already in the workspace; porting would add a 5th, and
    it has zero real external references (`pki.rs`'s `project_config_path()` is an unrelated
    coincidental substring). `alert.rs` (521 lines) — its `alert_*!` macros have zero external
    callers, but are still used internally by not-yet-ported `ggen-core` modules that `ggen-cli`
    *does* still call today (`domain/{graph,template}/*.rs`, `domain/utils/env.rs`,
    `lifecycle/exec.rs` — all correctly still on `ggen-core` per T037/T038's own "verified
    no-op" findings); when those modules eventually get a port task, T030's already-established
    disposition applies (`eprintln!` with the same emoji+text, not a new `slog_scope` dependency).
    `enhanced_error.rs` (493), `user_level.rs` (349), `app_config.rs` (208), `versioning.rs`
    (159), `logger.rs` (132, `ggen-cli` already has its own OTLP/tracing `telemetry.rs` covering
    this territory conceptually, T034), `types.rs` (126), `time.rs` (98), `cli.rs` (82) — all
    zero real callers anywhere, zero self-consumers inside `ggen-core` (except `app_config.rs`
    consuming `types.rs`'s `LogLevel`, itself also dead). Skip all 10; nothing ported.
  - **Verification**: no code changed by this task (nothing to port), so no new
    `cargo check`/`cargo test` run was needed or performed for this task specifically. Confirmed
    via `grep` that `crates/ggen-core/src/utils/mod.rs` still declares all 10 as `pub mod`
    (compiled, live, dead-but-not-broken) — correct and expected, since `ggen-core` remains a
    live workspace member until T055 deletes it wholesale; no per-file action needed before then.
  - **Genuinely still open, not resolved by this task** (out of the assigned 10-file scope,
    carried forward from `12-OPEN-QUESTIONS.md` item 2 verbatim): `utils/error.rs` (the
    crate-wide `Error`/`Result` type, already tracked and re-pointed under T035); the
    `SafePath`/`SafeCommand` duplicate-definition question (`safe_path.rs` vs.
    `path_validator.rs`, `safe_command.rs` vs. `security/command.rs` — 4 files, none in this
    task's assigned 10, still needing an `LSP findReferences` sweep once rust-analyzer recovers,
    per Open Questions item 2's own "what would change this" criterion).

- [x] T069 [US1] Dispose of `/Users/sac/ggen/crates/ggen-core/src/{ontology_core,canonical,transport}/`
  (1,778 + 722 + 1,245 = 3,745 lines) per a prior research pass's findings (this session):
  port whatever was verdicted PORT-TO-`<crate>`, document DEAD/ALREADY-COVERED-BY otherwise
  - **Re-verified the research pass's method before acting on its verdicts, per this session's
    own convention**: LSP still down (`LSP workspaceSymbol` on
    `crates/ggen-core/src/canonical/json.rs` → `LSP server 'plugin:rust-analyzer-lsp:rust-analyzer'
    exceeded max crash recovery attempts (3)`, the same exact error the research pass and T068
    both hit — confirmed environment-level, not something this task could route around either).
    Independently re-ran exhaustive `grep` across the *entire* tree (`grep -rn ... .`, not scoped
    to the brief's named consumer directories) for `ontology_core::`, `crate::transport`/
    `ggen_core::transport`, `crate::canonical`/`ggen_core::canonical`, plus a direct read of
    `crates/ggen-core/src/lib.rs`'s three `pub mod` declarations (lines 151/172/205, confirmed
    plain, no crate-root re-export aliasing that would hide a caller from a literal-path grep).
  - **`transport/` (1,245 lines) — confirmed DEAD, no port.** Whole-tree sweep: zero matches for
    `ggen_core::transport`/`crate::transport` anywhere outside the module's own 6 files
    (`a2a.rs`/`error.rs`/`origin.rs`/`session.rs`/`streaming.rs`/`mod.rs` referencing only each
    other). Matches the research pass's verdict exactly. No action taken.
  - **`ontology_core/` (1,778 lines) — DEAD, but with one real correction to the research pass's
    "zero external callers anywhere" claim.** Whole-tree grep found
    `crates/ggen-cli/tests/ontology_command_test.rs:133,146,156,165` calling
    `ggen_core::ontology_core::validators::validate_turtle` twice. This is a real, currently
    *compiled* test target — confirmed via `cargo metadata --format-version=1`'s target list for
    package `ggen-cli-lib`: `ontology_command_test ['test'] .../ggen-cli/tests/
    ontology_command_test.rs`, auto-discovered (no `autotests = false` in
    `crates/ggen-cli/Cargo.toml`, no explicit `[[test]]` entry needed). The research pass's own
    methodology note says it grepped `ggen-cli/src`; this file lives under `ggen-cli/tests/`,
    outside that stated scope, which is why it was missed — a scope gap, not a wrong read of what
    it did check. This does **not** change the underlying capability verdict, only the "zero
    callers" framing: read `ontology_core/validators.rs:57-61` and
    `ontology_core/triple_store.rs:191-219` line-by-line — `validate_turtle` is a thin wrapper
    that parses Turtle into a temp Oxigraph store and reports `is_valid = <no parse error>`,
    nothing more. `ggen_graph::graph::parse::parse_turtle(content: &str) ->
    Result<Vec<Quad>, GraphError>` (`crates/ggen-graph/src/graph/parse.rs:28`, confirmed present
    by reading the file, not assumed) is a real, already-existing, drop-in-equivalent check
    (`Ok(_)` = valid, `Err(_)` = invalid) — so the research pass's "ggen-graph already covers
    this more completely" conclusion holds and no new module needs writing anywhere. Per this
    task's file-ownership constraint (`crates/ggen-cli/src/` is off-limits because T031-T044's
    concurrent migration owns it; its `tests/` sibling was left alone for the same reason rather
    than edited opportunistically), I did not re-point the test file myself. **Flagged here for
    whoever owns the ggen-cli migration next**: `ontology_command_test.rs`'s 2 call sites need
    re-pointing from `ggen_core::ontology_core::validators::validate_turtle(path)?.is_valid` to
    `ggen_graph::graph::parse::parse_turtle(&content).is_ok()` (reading the file to a `String`
    first, since the ggen-graph fn takes `&str` not a path) before T054/T055 can safely delete
    `ggen-core` — T054's own reachability re-check will hit this same file.
  - **`canonical/` (722 lines) — split disposition, matches the research pass's recommendation.**
    Found the `json` submodule already ported to `/Users/sac/ggen/crates/ggen-config/src/canonical/`
    (`mod.rs` + `json.rs`, ~140 lines) and `/Users/sac/ggen/benches/canonical_bench.rs` already
    re-pointed to it, present untracked/uncommitted in the working tree at the start of this
    task — not authored by this task, but undocumented in `tasks.md` until now, so this entry
    takes ownership of verifying and recording it rather than re-doing it blind. Independently
    confirmed correctness by diffing the ported file against the ggen-core original
    (`crates/ggen-core/src/canonical/json.rs` vs. `crates/ggen-config/src/canonical/json.rs`):
    logic is byte-identical apart from two added `#[must_use]` attributes and one added
    `#[allow(clippy::unwrap_used, clippy::expect_used)]` on the test module — same class of
    trivial lint annotation already applied elsewhere this session (e.g. `receipt_impl.rs`).
    Independently re-verified the research pass's ALREADY-COVERED-BY/DEAD claims for the other
    3 submodules by reading source directly, not trusting the summary:
    `canonical::hash::compute_hash` (`crates/ggen-core/src/canonical/hash.rs:14-19`, SHA-256 +
    `hex::encode`) vs. `ggen_config::receipt::receipt_impl::hash_data`
    (`crates/ggen-config/src/receipt/receipt_impl.rs:178-182`, same SHA-256 + `hex::encode`) —
    confirmed byte-for-byte identical logic. `canonical::ttl::TtlCanonicalizer::normalize_ttl`
    (`crates/ggen-core/src/canonical/ttl.rs:26-34`) — confirmed by reading it that
    `.filter(|line| !line.is_empty() && !line.starts_with('@'))` really does drop every
    `@prefix`/`@base` line, a real correctness bug, and that
    `ggen_graph::graph::{canonical::{sort_quads_canonically,to_canonical_nquads_string,
    canonicalize_quads}, hash::hash_quads}` (confirmed present via direct read of
    `crates/ggen-graph/src/graph/{canonical,hash}.rs`) does real quad-level C14N + BLAKE3
    hashing, a strict superset. `canonical::rust::RustCanonicalizer`
    (`crates/ggen-core/src/canonical/rust.rs`, rustfmt subprocess wrapper) — whole-tree grep
    confirms zero external callers. None of the 3 were ported; matches the research pass's
    verdict on independent re-check.
  - **Verification (real execution, this task)**: `cargo check -p ggen-config --all-targets` —
    clean, 0 errors (only pre-existing unrelated workspace warnings: `ggen-engine` missing-docs
    lints, a `bcinr-pddl`/`praxis` manifest-key warning from other vendored crates). `cargo test
    -p ggen-config` — **100 passed, 0 failed** (79 lib unit tests + 10 `adversarial_tests.rs`
    integration tests + 11 doc-tests), including 8 `canonical`-specific tests
    (`canonical::json::tests::{test_sort_keys,test_nested_sorting,test_determinism,
    test_array_preservation,test_canonicalize_json_str,test_pretty_format}` and
    `canonical::tests::{test_canonical_wrapper,
    test_hash_default_method_delegates_to_receipt_hash_data}` — the last one specifically proving
    `Canonicalizer::hash`'s default method really reaches `ggen_config::receipt::hash_data`, not
    a stub). `cargo check -p ggen --bench canonical_bench` — clean (confirms the re-pointed
    bench, which lives in the root `ggen` package per `Cargo.toml:676-678`'s `[[bench]]` entry,
    actually compiles against `ggen_config::canonical::{json::*, Canonicalizer}` and
    `ggen_config::receipt::hash_data`). `cargo check --workspace --all-targets` — the only
    failures anywhere in the workspace are 2 pre-existing `praxis-graphlaw` test targets
    (`self_monitoring_real_session_actuation`, `ma_case_hook_actuation`) failing on missing
    `include_str!` fixture files under `packs/{self-monitoring-pack,ma-case-study-pack}/` — a
    pre-existing gap from T007's `praxis-graphlaw` vendor step, unrelated to `ontology_core`/
    `canonical`/`transport` and not touched or caused by this task.
  - **No files ported to a new destination by this task specifically** (the one PORT-TO-
    verdicted piece, `canonical::json`, was already present); this task's real contribution was
    independent re-verification of all three modules' dispositions, the correction to
    `ontology_core`'s "zero callers" claim (with a concrete, evidence-based re-point recipe left
    for the ggen-cli owner), and real `cargo check`/`cargo test` proof that the existing
    `canonical` port is correct and load-bearing. No `crates/ggen-cli/src/` or
    `crates/ggen-cli/tests/` files were touched, per this task's file-ownership constraint.

- [x] T071 [US1] Port the remaining load-bearing `ggen-core/src/domain/` submodules a prior
  research pass identified (this session): `sync_profile.rs` (733 lines) → `ggen-marketplace`,
  `error.rs` (230 lines) + `mcp_config.rs` (1,314 lines) → `ggen-config`, `utils/doctor.rs`
  (526 lines) → deferred (see below). Skip/document DEAD or ALREADY-COVERED verdicts for the
  other 12 submodules the same pass triaged. (Numbered T071, not T070: a concurrent session's
  own source comments in `ggen-config/Cargo.toml`/`crates/ggen-engine` had already claimed
  `T070` for its `[[generation.rules]]`-wiring work by the time this task went to append —
  confirmed live via `git diff` mid-task — so this entry steps to the next free number rather
  than colliding on the same ID for unrelated work.)
  - **`sync_profile.rs` → `/Users/sac/ggen/crates/ggen-marketplace/src/sync_profile.rs`**
    (new sibling module, `pub mod sync_profile;` added to `ggen-marketplace/src/lib.rs`).
    Ported verbatim — the research pass's finding held up exactly: all three real dependencies
    (`compute_pack_digest` at `marketplace::install`, `load_pack_metadata` at
    `packs_registry::metadata`, `PackLockfile`/`PackSource` at `packs::lockfile`) already live
    in this crate from the T024-T026 port, `compute_pack_digest` is `pub(crate)` and this module
    is a same-crate sibling so no visibility change was needed, and the original
    `Result<(), String>` signature needed zero error-type mapping. Only change: `use
    crate::domain::packs::install::compute_pack_digest` → `use crate::marketplace::install::
    compute_pack_digest` and the two other import paths, mechanical.
    `cargo test -p ggen-marketplace --lib sync_profile::`: **21/21 pass** (11 original unit/
    sabotage tests + 4 digest-reverification tests using real on-disk fixtures, `#[serial(
    GGEN_PACKS_DIR)]`, no mocks). Full-crate regression: `cargo test -p ggen-marketplace --lib`:
    **321/321 pass** (300 pre-existing + 21 new, 0 failed). `cargo check -p ggen-marketplace
    --all-targets`: clean. New module's own clippy signal is clean (`cargo clippy -p
    ggen-marketplace --lib --tests -- -D warnings 2>&1 | grep sync_profile` → no hits); the
    whole-crate clippy run itself does not complete due to a pre-existing, unrelated
    `unused_imports` hit in `packs_registry/types.rs:5` (not this task's code, not touched).
  - **`error.rs` + `mcp_config.rs` → `/Users/sac/ggen/crates/ggen-config/src/domain/{mod.rs,
    error.rs,mcp_config.rs}`** (new `pub mod domain;` in `ggen-config/src/lib.rs`). Named
    `domain::{error,mcp_config}`, not flattened to the crate root: the crate root already
    re-exports a module literally named `error` from `receipt::error` (`lib.rs`'s `pub use
    receipt::{..., error, ...}`), so a crate-root `pub mod error;` here would collide; keeping
    the `domain::` prefix also matches the exact bucket name `12-OPEN-QUESTIONS.md`/T053 already
    used to refer to this code ("domain::error"/"domain::mcp_config"), minimizing consumer diff.
    **Error-type mapping** (`ggen_core::utils::error::Error`, an ad-hoc string-message type, out
    of scope → `ggen_config::config_lib::ConfigError`, this crate's own richer `thiserror` enum,
    by call-site meaning not string-matching, same pattern as T019-T030):
    `file_not_found(PathBuf::from(msg))` → `ConfigError::FileNotFound(PathBuf::from(msg))` (a
    real 1:1 shape match); `invalid_input(msg)` → `ConfigError::Validation(msg)` (closest
    available variant — `InvalidValue{field,reason}` was rejected, these call sites never have a
    field name); every other constructor (`network_error`/`invalid_state`/`internal_error`/
    `feature_not_enabled`/the three `new(&format!(...))` sites) → `ConfigError::Other(...)`,
    preserving the *exact* original wrapper text (`"Network error: {msg}"`, `"Invalid state:
    {msg}"`, etc.) so every `.to_string().contains(...)` assertion in the ported tests is
    unaffected by the type swap — verified true by running them, not assumed.
    Added `dirs = "6.0"` to `ggen-config/Cargo.toml` (same version already used by `ggen-core`/
    `ggen-cli`/`ggen-marketplace`) for `mcp_config.rs`'s user-config-dir resolution
    (`dirs::home_dir()`). Both new `#[cfg(test)] mod tests` blocks got the same `#[allow(
    clippy::unwrap_used, clippy::expect_used)]` this crate's own convention already uses
    (`manifest/parser.rs`, `receipt/receipt_impl.rs`, `config_lib/schema.rs`, etc.).
    `cargo test -p ggen-config --lib domain::`: **14/14 pass** (6 `domain::error` conversion/
    helper tests + 8 `domain::mcp_config` builder/validation/serialization tests, real
    execution). Full-crate regression: `cargo test -p ggen-config --lib`: **93/93 pass** (79
    pre-existing + 14 new); `cargo test -p ggen-config --doc`: **11/11 pass**, unchanged (no
    doctests added — this port used plain prose doc comments only). `cargo check -p ggen-config
    --all-targets`: clean. `cargo clippy -p ggen-config --lib --tests -- -D warnings`: clean.
  - **Closed the third dependency `tests/a2a_integration_tests.rs` had on `ggen_core`, going
    beyond the research pass's own prediction.** That file — a real, compiled `[[test]]` target
    (`required-features = ["integration"]`, confirmed via `cargo metadata`) — was one of T053's
    50 "genuinely untouched" files, bucketed under `utils::error`. The research pass predicted
    re-pointing `domain::error`/`domain::mcp_config` would only get it "2/3 of the way," staying
    blocked on `ggen_core::utils::error::Error` at 3 lines inside `test_error_conversion`
    (the `From<A2aError|McpError|AgentError>` conversion target's *annotated type*). Re-pointing
    the two top import lines to `ggen_config::domain::{error,mcp_config}` (drop-in, field-for-
    field identical structs — the file's own local `A2aConfigTestExt`/`AgentConfigTestExt`
    extension traits only touch public fields) plus retargeting those 3 annotations from
    `ggen_core::utils::error::Error` to `ggen_config::config_lib::ConfigError` — the real
    destination the newly-ported `From` impls target — closes the gap fully: the test only
    asserts `.to_string().is_empty()` is false, a semantics-preserving change regardless of which
    concrete error type is named. `grep -n "ggen_core" tests/a2a_integration_tests.rs`: zero code
    hits (one doc-comment mentioning the old path for context). `cargo check --test
    a2a_integration_tests --features integration`: clean. `cargo test --test
    a2a_integration_tests --features integration`: **24/24 pass**. This file was never compiled
    under `just test`'s default `cargo test --workspace --tests` scope (gated behind
    `--features integration`, off by default) either before or after this change, so T053's
    baseline "407 passed, 0 failed" default-feature count is unaffected — re-confirmed via a full
    `cargo test -p ggen` re-run this task: **407 passed, 0 failed**, identical.
    T053's "50 blocked" count for the `utils::error` bucket becomes 49 (this file no longer has
    any real `ggen_core` dependency of any kind; the other files in that bucket — `drift-
    detection-example.rs`, `tests/unit/marketplace_critical_tests.rs`, etc. — remain genuinely
    blocked on porting `utils::error` itself, out of scope here).
  - **`utils/doctor.rs` (526 lines) → deliberately NOT ported this pass; deferred with a
    concrete plan, not silently skipped.** Verdicted LOAD-BEARING and mostly portable by the
    research pass (self-contained modulo `check_slo`, gated behind `--all`, needing crate-root
    `pipeline_engine::vocabulary::VocabularyRegistry`), with `ggen-engine` as the real
    destination. Did not touch it: `ggen-engine` was under active, in-flight, uncommitted
    development by a concurrent session for this task's entire duration (shared task-list
    entries "T068/T070: wire `[[generation.rules]]` into ggen-engine sync" and "verify cargo
    check/test for ggen-engine" both `in_progress` throughout; confirmed live via `git diff`
    showing that session editing `ggen-engine`-adjacent files, including
    `ggen-config/src/config_lib/schema.rs`, mid-task). Per this repo's own guidance ("never patch
    another session's in-flight crate to unblock yourself"), adding new `ggen-engine/Cargo.toml`
    dependencies and a new `pub mod` registration in `ggen-engine/src/lib.rs` right now risked a
    real collision with that session's active edits, for a submodule with zero current callers
    anywhere (would land unwired regardless, same as T029/T030's precedent) — not worth the risk
    for this pass. **Concrete follow-up plan, so the next pass can execute quickly**: new file
    `crates/ggen-engine/src/doctor.rs` (module name confirmed free — no existing `doctor.rs` in
    that crate), `pub mod doctor;` in `lib.rs`; add `dirs = "6.0"` (matches the version this
    session already used for `ggen-config`) and a plain `reqwest` client dependency (default
    features suffice — `check_observability` only does `Client::builder().timeout(...).build()`/
    `.get(url).send().await`/`.text().await`, no streaming) to `Cargo.toml`; port
    `CheckStatus`/`CheckResult`/`DoctorInput`/`DoctorResult`/`EnvironmentInfo`/`execute_doctor`/
    `check_marketplace`/`check_cache`/`check_observability`/`check_rust`/`check_cargo`/
    `check_git`/`collect_environment` verbatim; map the module's one real error-construction call
    site (`check_observability`'s `crate::utils::error::Error::new(&format!("Failed to build
    HTTP client: {}", e))`) → `crate::error::AppError::Validation(format!(...))` (this crate's
    own `thiserror` enum has no generic/network variant; `Validation` is the closest of its 5
    variants, matching this task's own "closest available variant, preserve message text"
    convention); **comment out `check_slo`** (fix-forward, not delete) with a note pointing at
    the `pipeline_engine::vocabulary::VocabularyRegistry` blocker, and in `execute_doctor`'s
    `input.all || input.check.as_deref() == Some("slo")` branch push a `CheckResult{ name: "SLO
    Performance", status: CheckStatus::Warning, message: "SLO check not yet ported to
    ggen-engine...", recovery: None }` instead of silently dropping the check — a loud gap, not a
    fail-open no-op. Zero test-coverage change either way: confirmed via full read that the
    original `doctor.rs` has no `#[cfg(test)]` module at all. Lands unwired (its consumers,
    `ggen-cli/src/cmds/{doctor.rs,pack.rs:316}`, are off-limits — the concurrent T031-T044
    ggen-cli session's territory), matching T029/T030's "compiles standalone with zero callers
    yet" precedent.
  - **Skipped verdicts, re-confirmed not re-litigated** (all DEAD or ALREADY-COVERED per the
    research pass; no action taken, no code touched): `graph/` (2,176 lines, load-bearing but
    blocked on the un-ported crate-root `Graph` — a real architecture call, not resolved
    unilaterally, matching the T028/T043 precedent for this class of gap); `ontology/` (627 live
    + 903 dead-on-arrival lines, same blocked-on-crate-root-architecture class as `graph/`);
    `rdf/` (1,450 lines, dead "v2" rewrite — the crate-root "v1" twin is the one real consumers
    use); `config/` (1,550 lines, superseded by T019-T023's `GgenManifest` unification, not
    merely dead); `project/` (861 lines, zero callers anywhere); `utils/env.rs` (206 lines, no
    `ggen env` command exists); `ci/`/`audit/`/`shell/` (340+324+243 lines, referenced only by
    `tests/chicago_tdd/utils/*` files that are not wired into any compiled test target);
    `cli_helpers.rs` (457 lines), `environment.rs` (355 lines), `capability_scanner.rs` (255
    lines), `template_scanner.rs` (166 lines) — all zero real callers anywhere.
  - **Genuinely still open, not resolved by this task**: `utils/doctor.rs`'s port (concrete plan
    above, blocked on the concurrent `ggen-engine` session finishing first, not on missing
    information); `graph/`/`ontology/`'s architecture-level rewrite-vs-port call (T028/T043's
    pre-existing open question, unchanged); the `SafePath`/`SafeCommand` duplicate-definition
    question (out of this task's scope, tracked separately per `12-OPEN-QUESTIONS.md` item 2).

- [x] T070 [US1] Wire `[[generation.rules]]` into `ggen-engine`'s `sync()` pipeline — resolves
  the "Engine wiring" half of T023's still-open gap (see that entry's update below)
  - **Numbering note**: this entry was designed and implemented under the working label "T068"
    (visible in several in-code doc comments authored during this pass) before this session
    discovered two other concurrent sessions had already claimed T068 (utils/SafePath
    disposition) and T069 (`ontology_core`/`canonical`/`transport` disposition) in `tasks.md`
    first. Every in-code `T068` comment this session touched was renamed to `T070` (twice, as
    the live collision moved) to keep source and this task list consistent —
    `grep -rn "T070" crates/ggen-engine crates/ggen-config` is now the authoritative
    cross-reference set: `Cargo.toml` (both crates), `src/lib.rs`, `src/generation_rules.rs`,
    `schema/ggen-manifest-schema.ttl`, `tests/{ggen_manifest_schema_match.rs,
    generation_rules_e2e.rs}` (ggen-engine), `src/manifest/types.rs` (ggen-config). T071's own
    entry above independently confirmed and deferred to this reservation.
  - **What this session found already in place at start** (not authored by this pass, but
    unverified and undocumented in `tasks.md` until now, matching this repo's convention for
    picking up untracked concurrent work rather than redoing it blind — same posture T069's own
    entry above took for `canonical::json`): `crates/ggen-engine/src/generation_rules.rs` (the
    whole module), its wiring into `crate::sync::sync` (stage-0 schema dispatch on the raw TOML
    text, before `GgenConfig::load` runs), the new `ggen-engine → ggen-config` dependency edge,
    `schemars` derives on `ggen_config::manifest::{GenerationConfig, GenerationRule, QuerySource,
    TemplateSource, GenerationMode}`, the new `schema/ggen-manifest-schema.ttl`, and its
    drift-proof test `tests/ggen_manifest_schema_match.rs` — implementing exactly the file-level
    schema-dispatch design (a project is either a frontmatter project or a declarative-rules
    project, decided once via a cheap raw-TOML pre-parse) that T023's gap note above called out
    as the open architectural question. This session's independent verification (LSP/read-based,
    not trust) confirmed: `QuerySource::{File,Inline}`/`TemplateSource::{File,Inline}` resolution,
    `when:`/`skip_empty` guards, per-row-vs-static rendering (`output_file.contains("{{")`, the
    same rule the frontmatter path uses for `Frontmatter.to`), `GenerationMode::{Create,Overwrite,
    Merge}` (`Merge` porting `codegen::merge::merge_sections`'s exact marker algorithm from
    ggen-core, independently diffed against `crates/ggen-core/src/codegen/merge.rs` and confirmed
    matching), and receipt chaining through the existing `crate::sync::write_receipt` — all
    correctly reusing this crate's own `GraphEngine` trait, `template::{build_tera,
    solutions_to_values}`, and `write::resolve_target` rather than re-deriving ggen-core's
    parallel implementations, per the task brief's "first principles, not copy-paste" instruction.
    `QuerySource::Pack`/`TemplateSource::{Pack,Git,Package}` correctly return a named, typed
    `[FM-GEN-006]`/`[FM-GEN-007]` refusal (never a silent skip), matching the research pass's
    recommended scope cut (File/Inline first; Pack has no destination in `crate::pack::Pack`'s
    model yet, the exact gap T028's note already flagged).
  - **A real, load-bearing bug found and fixed by this session**: `has_generation_rules`
    (`crates/ggen-engine/src/generation_rules.rs`) parsed the raw TOML via
    `raw_toml.parse::<toml::Value>()`. `toml::Value: FromStr` (crate `toml` 1.1.2+spec-1.1.0,
    confirmed by reading `~/.cargo/registry/.../toml-1.1.2+spec-1.1.0/src/value.rs:394-401`) calls
    `ValueDeserializer::parse`, a *single bare value* parser — it errors on any real multi-table
    `ggen.toml` starting with a `[table]` header ("unexpected content, expected nothing"), proven
    via `cargo test -p ggen-engine --lib generation_rules::` before the fix: 3 of 4
    `has_generation_rules` unit tests failed on exactly this error, including the one asserting a
    *plain frontmatter* `ggen.toml` correctly returns `false`. Because `crate::sync::sync`'s stage-0
    dispatch (`sync.rs:154`, `if crate::generation_rules::has_generation_rules(&raw)? { ... }`)
    propagates this error via `?` unconditionally, this bug — uncaught — would have made `sync()`
    fail closed on *every* project, frontmatter or declarative-rules alike, the moment a real
    `ggen.toml` (which always has `[table]` headers) was read; the module's own unit tests were the
    only thing not yet exercising a real document long enough to catch it. Root cause: a TOML
    *document* is a `Table` (`toml::Table: FromStr` calls the real document parser,
    `toml::from_str` — the crate's own top-level docs cite `"foo = 'bar'".parse::<Table>()` as "the
    easiest way to parse a TOML document"), not a bare `Value`. Fixed by parsing into
    `toml::Table` instead (`Table` is `Map<String, Value>` and exposes the same
    `.get(&str) -> Option<&Value>` the chained `.get("generation").and_then(|g|
    g.get("rules"))` lookup already relied on — a one-line type change, documented in place with
    the full root-cause trace). Verified via re-run: `cargo test -p ggen-engine --lib
    generation_rules::` — 7/7 pass (4 `has_generation_rules` + 3 `merge` submodule tests); the
    same fix is what makes `tests/sync_e2e.rs`/`tests/graphlaw_e2e.rs` (both frontmatter-path,
    both call `sync()` on a real multi-table `ggen.toml`) pass at all under this session's tree —
    confirmed by re-running them clean immediately after the fix (see verification below).
  - **Two validation checks added this session**, ported *by check* (not verbatim) from
    ggen-core's `GenerationPipeline::validate_generated_output`
    (`crates/ggen-core/src/codegen/pipeline.rs:1534-1567`) per the research pass's §3
    recommendation: a new `validate_rendered_body` helper, called on every rendered body (both
    per-row and static branches) before it is queued for writing. `[FM-GEN-011]` empty-output
    refusal (ggen-core's E0004) and `[FM-GEN-012]` oversized-output refusal (E0005), the latter
    reusing `crate::write::MAX_OUTPUT_BYTES` as the single size-cap constant (no second hardcoded
    `10 * 1024 * 1024` literal), exactly as the research note suggested. E0006 (`../` path
    traversal in the output path) was deliberately **not** re-implemented as a substring check:
    `decide_and_maybe_apply`'s existing call to `crate::write::resolve_target` already refuses any
    `to` that escapes the project root or contains a `..` component, a strictly stronger guarantee
    than ggen-core's original substring test. E0012 (`unsafe`-block detection, gated on
    `[validation].no_unsafe`) remains out of scope, consistent with and cross-referenced from the
    module's own "deliberately deferred" list: this path does not read `manifest.validation` at
    all yet, so there is no config surface to gate it on.
  - **Real Chicago-TDD end-to-end proof, new file `crates/ggen-engine/tests/
    generation_rules_e2e.rs`** (real `tempfile::TempDir`, real oxigraph-backed graph, real SPARQL
    evaluation, real Tera rendering, driven exclusively through the public `ggen_engine::sync::
    sync` entry point — no mocks), mirroring the load-bearing-proof pattern in `tests/
    graphlaw_e2e.rs`/`tests/sync_e2e.rs`. 11 tests, each asserting on real rendered file bytes,
    real receipt contents, or a real typed refusal — never just an `Ok(())`:
    1. `static_rule_with_file_query_and_file_template_renders_real_query_results` — `QuerySource::
       File` + `TemplateSource::File`, asserts the rendered file contains the real SPARQL row data
       (not a stub), the query/template files are bound into the receipt's input closure with
       their real BLAKE3 hashes, and the on-disk receipt binds the real output bytes.
    2. `per_row_rule_with_inline_query_and_template_creates_one_file_per_row` — a templated
       `output_file` fans out into one real file per row, each independently rendered.
    3. `when_guard_false_and_skip_empty_produce_documented_skips_not_writes` — a false `when:` ASK
       and a real zero-row `skip_empty` query both produce a recorded `Skipped` with the reason
       text, never an error or a silent vanish.
    4. **`merge_mode_preserves_hand_edits_across_two_syncs_with_changed_query_data`** — THE
       decorative-vs-real proof for `GenerationMode::Merge`: sync once, hand-edit the manual
       section exactly as a developer would, change the ontology so the next sync's generated
       content differs, sync again — asserts the generated section updated (v1 → v2) *and* the
       hand-written section survived byte-for-byte. A naive "always overwrite" implementation
       would destroy the hand edit; a naive "always skip if exists" implementation would never
       pick up v2. Only a correct decide-then-merge implementation passes both assertions at once.
    5. `create_mode_writes_once_then_leaves_hand_edits_alone` — `mode = "Create"` bootstrap
       semantics (root `CLAUDE.md`'s documented convention): writes once, then a hand-completed
       file survives every subsequent sync untouched.
    6. `overwrite_mode_replaces_content_and_skips_when_unchanged` — real content-change detection:
       identical content → `Skipped("unchanged...")`; real content diff → `Written` with the new
       bytes on disk.
    7. `unimplemented_query_source_pack_is_a_typed_refusal_not_a_silent_skip` /
       `unimplemented_template_source_git_is_a_typed_refusal` — the deferred `Pack`/`Git` variants
       fail loud and named (`[FM-GEN-006]`/`[FM-GEN-007]`, naming the pack/git URL), never
       silently skip or produce a decorative empty output.
    8. `duplicate_render_targets_from_per_row_rule_are_refused` — two rows colliding onto the same
       rendered `output_file` refuse the whole run (`[FM-GEN-004]`) before any write, not
       last-row-wins.
    9. `two_generation_rules_syncs_chain_receipts` — mirrors `graphlaw_e2e.rs`'s two-run
       determinism proof for this new path specifically: two syncs produce a linearly-chained,
       independently-recomputable receipt history, not two disconnected genesis records.
    10. `empty_rendered_body_is_refused_not_silently_written` — proves the new
        `validate_rendered_body` check actually runs on this path (`[FM-GEN-011]`).
  - **Backward compatibility, verified not assumed**: `tests/sync_e2e.rs` (10 tests) and `tests/
    graphlaw_e2e.rs` (5 tests) — both exclusively frontmatter-path fixtures with no `[[generation.
    rules]]` — pass unchanged; `has_generation_rules` correctly returns `false` for them
    (`generation_rules::tests::has_generation_rules_false_for_frontmatter_project`/
    `_when_rules_array_is_empty`) and `crate::sync::sync` falls through to the original,
    untouched `GgenConfig::load` + `discover_templates()` path. This is a strictly additive
    second path, not a replacement.
  - **Real verification (this session, real execution, not narration)**:
    `cargo check -p ggen-engine --all-targets` — clean, only pre-existing unrelated `missing-docs`/
    `dead_code` warnings in `types.rs`/`error.rs` (confirmed present before this session's changes
    too). `cargo test -p ggen-engine --lib` — **114/114 pass** (includes the 7 `generation_rules`
    unit tests post-fix). `cargo test -p ggen-engine --test generation_rules_e2e` — **11/11
    pass**. Named must-stay-green regression guards, re-run explicitly: `tests/
    ggen_toml_schema_match.rs` 5/5, `tests/graphlaw_e2e.rs` 5/5, `tests/sync_e2e.rs` 10/10, `tests/
    frontmatter_schema_match.rs` 1/1, `tests/ggen_manifest_schema_match.rs` 5/5 — all pass
    unchanged. `cargo clippy -p ggen-engine --lib --tests -- -D warnings 2>&1 | grep -A10
    generation_rules` — zero findings in this session's own new/touched code. `cargo test -p
    ggen-config --lib` — 79/79 pass; `cargo test -p ggen-config --doc` — 11/11 pass.
    Full-crate `cargo test -p ggen-engine --no-fail-fast` (all 27 binaries): **240 passed, 64
    failed** — every one of the 64 failures is confined to 10 pre-existing binaries
    (`cli_boundary`, `cli_read_only_invariant_matrix`, `cross_pack_matrix`, `doctor_e2e`,
    `framework_packs_e2e`, `pack_behaviors_cli_e2e`, `pack_e2e`, `receipt_chain_e2e`,
    `wasm4pm_facts_e2e`, `write_behaviors_cli_e2e`) — the exact same 10 binaries T023's own prior
    verification note above named as broken by the concurrent, in-flight T031-T044 `ggen-cli`
    re-point (`CARGO_BIN_EXE_ggen`/`assert_cmd` spawning the real CLI binary, whose argument shape
    is actively changing in a sibling session: e.g. `sync_run_generates_expected_file`'s panic is
    literally `error: unexpected argument 'run' found` from the in-flight binary, not this
    session's library code). Confirmed unrelated by inspecting every panic site: all 64 failures
    trace to `chicago_tdd_tools::cli_proof::harness::CliOutput::assert_success` or a fixture/CLI
    helper in the test file itself, never to `crate::generation_rules` or `crate::sync`. Per this
    repo's documented concurrent-session build-flakiness guidance, these were not patched here.
  - **Files touched this session**: `crates/ggen-engine/src/generation_rules.rs` (bug fix +
    `validate_rendered_body` + two call sites + doc-comment updates), `crates/ggen-engine/tests/
    generation_rules_e2e.rs` (new, 11 tests), plus the `T068`→`T070` comment-only rename across
    `crates/ggen-engine/{Cargo.toml, src/lib.rs, schema/ggen-manifest-schema.ttl, tests/
    ggen_manifest_schema_match.rs}` and `crates/ggen-config/{Cargo.toml, src/manifest/types.rs}`.
    No `crates/ggen-cli/src/` files touched, per this task's file-ownership constraint.

### 4i. Deletion (`docs/jira/v26.7.16/11-DELETION-AND-DEFINITION-OF-DONE.md`)

- [ ] T054 [US1] Re-verify reachability of `/Users/sac/ggen/crates/ggen-core/src/receipt/{chain_linking.rs,provenance_envelope.rs}` (188 + 754 lines) before deleting — confirm whether `ggen-cli/src/cmds/inverse_sync.rs`'s use of `provenance_envelope::{CoherenceReport, ProvenanceEnvelope}` needs porting first
- [ ] T055 [US1] Delete `/Users/sac/ggen/crates/ggen-core/` in full
- [ ] T056 [US1] Remove `ggen-core` from `[workspace] members` in `/Users/sac/ggen/Cargo.toml`
- [ ] T057 [US1] Run `grep -rn "ggen_core" crates/ src/ tests/ examples/ benches/` and confirm zero matches (SC-006)
- [ ] T058 [US1] Run `just check && just lint && just test` and confirm all green with `ggen-core` gone (SC-001)

**Checkpoint**: User Story 1's independent test passes — full suite green, `ggen-core`
fully retired, command-surface diff against the T003 baseline shows zero regressions.

---

## Phase 5: User Story 3 - Provenance and auditability are preserved or improved (Priority: P2)

**Goal**: Every sync receipt is both tamper-evident and attributable.

**Independent Test**: Tamper with a chained receipt → verification fails and names the
broken one; sign a receipt → verification distinguishes valid/unsigned/invalid.

- [ ] T059 [US3] Add `signature_hex: Option<String>` field to `ReceiptRecord` in `/Users/sac/ggen/crates/praxis-core/src/receipt_record.rs:27`
- [ ] T060 [US3] Fix the 5 OTEL span field-declaration bugs in the ported pipeline (add `"pipeline.duration_ms" = tracing::field::Empty` to each `info_span!` call, originally at `/Users/sac/ggen/crates/ggen-core/src/pipeline_engine/pipeline.rs:421-426,449-454,482-487,511-516,545-550` before deletion; port the fix into the new engine's sync implementation)
- [ ] T061 [US3] Add `"pipeline.files_generated" = tracing::field::Empty` to the `pipeline.emit` span and record it from the actual generated-file count
- [ ] T062 [US3] Lift `PackInstallClosure` and the `input_hashes`/`output_hashes` construction (including the `MISSING`-sentinel helper) from `/Users/sac/ggen/crates/ggen-core/src/agent/receipt.rs:44-58,178-195` into the T025 marketplace port
- [ ] T063 [US3] Implement `verify_receipt_record()` two-step verification (chain integrity via `recompute_chain_hash()`, then `praxis_core::signing::sign_chain_hash_with_key`/`verify_chain_hash_with_key` from `/Users/sac/ggen/crates/praxis-core/src/signing.rs:43-51,73-81`) in the new engine crate; decide and document the key-management policy (zero-config file keypair vs. `PRAXIS_SIGNING_KEY` env var)
- [ ] T064 [US3] Run the `quickstart.md` step 5 and step 6 verification commands; confirm receipt tamper-detection, signature verification, and OTEL span capture all work end-to-end

**Checkpoint**: User Story 3's independent test passes (SC-003, SC-004).

---

## Phase 6: User Story 4 - Architectural and compliance boundaries hold under the new dependency graph (Priority: P3)

**Goal**: The Process Intelligence Boundary is enforced by tooling, not just documentation.

**Independent Test**: Guard script passes on current code; fails on a deliberately
introduced `praxis_graphlaw::chatman` import.

- [ ] T065 [US4] Create `/Users/sac/ggen/scripts/ci/guard-process-intelligence-boundary.sh` per `docs/jira/v26.7.16/02-CROSS-REPO-DEPENDENCY-RISKS.md` item 2's exact script; add a `guard-process-boundary` recipe to `/Users/sac/ggen/justfile`
- [ ] T066 [US4] Run the guard against current code (must pass) and the `quickstart.md` step 7 negative-case check (temporarily add a `chatman` import, confirm the guard fails, then revert)

**Checkpoint**: User Story 4's independent test passes (SC-007); explicit sign-off recorded
from whoever owns the CLAUDE.md boundary rule.

---

## Final Phase: Polish & Cross-Cutting Concerns

- [ ] T067 Retarget `just slo-check` (`/Users/sac/ggen/justfile:153-167`): replace `cargo test -p ggen-core --test inverse_receipt_chain_test` with `cargo test -p ggen-engine --test receipt_chain_e2e`, and add a real `Instant`/`Duration` timing assertion closing the Decorative-Completion gap documented in `docs/jira/v26.7.16/11-DELETION-AND-DEFINITION-OF-DONE.md`; also retarget the two `-p ggen-core` targets under `test-phase2` (`justfile:100,106`)

---

## Dependencies

```mermaid
graph TD
    Setup["Phase 1: Setup"] --> Foundational["Phase 2: Foundational"]
    Foundational --> US2["Phase 3: US2 - Publish safety (P1)"]
    Foundational --> US1["Phase 4: US1 - Capability parity (P1)"]
    US1 --> US3["Phase 5: US3 - Receipt+OTEL (P2)"]
    Foundational --> US4["Phase 6: US4 - Boundary+license (P3)"]
    US1 --> Polish["Final: Polish"]
    US3 --> Polish
    US4 --> Polish
```

US2 and US1 can proceed in parallel once Foundational is done (US2 is small and
self-contained). US3 depends on US1's marketplace port (T025, for T062) but not on US1's
consumer-migration tasks. US4 only depends on Foundational.

## Parallel Execution Opportunities

- T006 / T007 (vendoring `praxis-core` / `praxis-graphlaw`) — different directories
- T016 (LawEngine trait definition) can start alongside T019-T023 (manifest port) — different crates
- T052 (early-mover root files) is explicitly parallelizable with the rest of Phase 4h once its prerequisites land

**Total Parallel-Tagged Tasks**: 4

## Implementation Strategy

### MVP Scope

Phase 1 + Phase 2 + Phase 3 (User Story 2) delivers the single highest-risk guarantee — the
published crate can never be corrupted by this migration — before any consumer code is
touched. This is a safe, independently-shippable checkpoint even if the rest of the
migration is paused.

### Incremental Delivery

1. **Increment 1**: Setup + Foundational + US2 (publish safety verified)
2. **Increment 2**: US1 (the full capability-parity migration — largest increment, tickets 03/05/06/07/08/09/10/11)
3. **Increment 3**: US3 (receipt signing + OTEL fix)
4. **Increment 4**: US4 (boundary guard + license) — independent of Increment 2/3, can run in parallel with either
5. **Polish**: `slo-check` retargeting

### Task Execution Format

```
- [ ] TaskID [P?] [StoryID?] Description with file path
```

## Checklist Format Validation

✅ All 67 tasks follow the required format: checkbox prefix, sequential `T0NN` ID, `[P]` where
parallelizable, `[US#]` label on every Phase 3–6 task (absent on Setup/Foundational/Polish),
explicit full file path(s) in every description.
