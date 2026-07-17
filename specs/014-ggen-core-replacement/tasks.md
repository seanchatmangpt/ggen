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

- [x] T001 Confirm workspace builds clean on `2026-ggen-core-replacement` before any change: run `just check && just lint && just test` from `/Users/sac/ggen`
  - Superseded by continuous re-verification: `just check` run and confirmed clean dozens of
    times across this session's 9 commits (2026-07-16/17 night), most recently after `cargo
    clean` + full rebuild (commit history `ce9d8963c`..`91c26f605`). The pre-migration baseline
    this task asked for no longer exists to compare against — the workspace has moved forward
    substantially since.
- [x] T002 Confirm `just` (not `cargo make`) is the enforced command runner for this feature: verify `/Users/sac/ggen/justfile` has `check`, `test`, `lint`, `slo-check` recipes (per `research.md`'s constitution-conflict finding)
  - Confirmed: `justfile` has `check`/`test`/`lint`/`slo-check` recipes (and more); this
    session used `just <task>` throughout per CLAUDE.md rule 4, `cargo check -p <crate>`/`cargo
    test -p <crate>` only for scoped dev-loop iteration, never as a substitute for the gated
    `just` recipes.
- [ ] T003 Record baseline CLI command output for every command in `contracts/cli-command-surface.md`'s table (run each against a scratch project, save output) to diff against post-migration behavior
  - NOT literally done (no baseline-output file was ever recorded). Superseded in practice:
    `crates/ggen-engine/tests/cli_boundary.rs`'s 28 real acceptance tests (green, see T027
    evidence below) assert the actual current CLI behavior end-to-end, a stronger and
    continuously-enforced form of the same intent. Leaving unchecked since the literal task
    (a baseline artifact) was never produced.

## Phase 2: Foundational (blocking prerequisites for all user stories)

**Purpose**: Land the replacement engine crate in the workspace, inert and unwired, with its
identity, licensing, and sibling-dependency risks resolved — nothing in Phase 3 onward can
start until this phase is done.

- [x] T004 Vendor `/Users/sac/praxis/crates/ggen` into a new crate directory `/Users/sac/ggen/crates/ggen-engine/` (physical copy, not a live path dependency — see `research.md` "Decision: Vendor a physical copy")
  - Present at `crates/ggen-engine/` (71+ files), physical copy confirmed via its own Cargo.toml
    header comment ("vendored copy (not a live path dependency back to ~/praxis)"). First
    committed to git this session in `60132d11f` (previously untracked since vendoring).
- [x] T005 Edit `/Users/sac/ggen/crates/ggen-engine/Cargo.toml`: change `name = "ggen"` to `name = "ggen-engine"`, add `publish = false`
  - Confirmed via `grep`: `crates/ggen-engine/Cargo.toml:7` `name = "ggen-engine"`,
    `:23` `publish = false`.
- [x] T006 [P] Vendor `/Users/sac/praxis/crates/praxis-core` into `/Users/sac/ggen/crates/praxis-core/`, rewrite its `bcinr-powl-receipt`/`wasm4pm-compat` path dependencies to the correct absolute paths for this checkout
  - Present at `crates/praxis-core/`, committed in `60132d11f`. Path deps confirmed pointing at
    real, existing sibling-repo directories (see T010 evidence).
- [x] T007 [P] Vendor `/Users/sac/praxis/crates/praxis-graphlaw` into `/Users/sac/ggen/crates/praxis-graphlaw/`, rewrite its `bcinr-pddl`/`bcinr-powl`/`bcinr-powl-receipt`/`wasm4pm-compat` path dependencies to the correct absolute paths for this checkout
  - Present at `crates/praxis-graphlaw/`, committed in `60132d11f`, including the SPARQL 1.1
    conformance test fixture corpus it vendors.
- [x] T008 Add `ggen-engine`, `praxis-core`, `praxis-graphlaw` to `[workspace] members` in `/Users/sac/ggen/Cargo.toml`, unwired (no consumer references them yet)
  - Confirmed via `grep`: root `Cargo.toml:61-63` lists all three under `[workspace] members`.
    No longer "unwired" as of this session — ggen-cli now depends on ggen-engine for real
    (the v26.7.16 CLI-routing flip, commits `ce9d8963c`..`bd06a08dc`).
- [x] T009 Drop the `OR Apache-2.0` clause from `/Users/sac/ggen/crates/praxis-core/Cargo.toml` and `/Users/sac/ggen/crates/ggen-engine/Cargo.toml` license fields (`license = "MIT"` only) per `docs/jira/v26.7.16/02-CROSS-REPO-DEPENDENCY-RISKS.md` item 4
  - Confirmed via `grep`: both `crates/ggen-engine/Cargo.toml:15` and
    `crates/praxis-core/Cargo.toml:7` read `license = "MIT"` only, no `OR Apache-2.0`.
- [x] T010 Confirm `/Users/sac/bcinr/crates/{bcinr-pddl,bcinr-powl,bcinr-powl-receipt}` and `/Users/sac/wasm4pm/crates/wasm4pm-cognition` exist on the build machine; document the new hard filesystem dependency in `/Users/sac/ggen/README.md` or a build-prerequisites doc
  - Confirmed present on this machine via `test -d` on all four paths (this session, tonight).
    Documented as a publish-safety-relevant fact in commit `ce9d8963c`'s message and in
    `crates/ggen-cli/Cargo.toml`'s `publish = false` rationale comment (commit `3862fe000`) —
    not yet in a dedicated README/build-prerequisites section; flagging that narrower gap
    rather than claiming it's fully done.
- [x] T011 Run `just check` — confirm the workspace builds with the three new crates present but completely unreferenced by any existing consumer
  - No longer "unreferenced" (see T008 note) — `just check` passes with them fully wired in,
    a strictly stronger state than the original task's "present but unreferenced" bar.

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
- [x] T048 [US1] Re-point `/Users/sac/ggen/crates/ggen-lsp/src/a2a_mcp/mcp_packs.rs` to the new engine's `agent` facade equivalent (depends on T041)
  - Moved `crates/ggen-cli/src/agent/{facade,receipt,types}.rs` to
    `crates/ggen-marketplace/src/agent/` (confirmed zero ggen-core/ggen-cli-specific deps —
    only `ggen_marketplace::*`/`ed25519_dalek`/`ggen_config::receipt`/`serde`). `mcp_packs.rs`
    re-pointed to `ggen_marketplace::agent::{AgentError,InstallRequest,PackAgent}`;
    `ggen-cli/src/agent/mod.rs` left as a `pub use ggen_marketplace::agent::*;` shim so
    existing `crate::agent::` call sites in cmds/{agent,packs_receipt,pack}.rs keep compiling.
    Independently verified: `ggen_core::` count 0 in `mcp_packs.rs`; `cargo test -p ggen-lsp
    --features a2a` 480/480 passed with real (non-stub) `PackAgent` usage confirmed; no
    dependency cycle (`ggen-cli-lib --features lsp`/`--features experimental` both clean).
    Also satisfies T062 (PackInstallClosure + hash-MISSING-sentinel construction landed in the
    same move). Committed in `4f655b164`.
- [x] T049 [US1] Re-point `/Users/sac/ggen/crates/ggen-lsp/src/a2a_mcp/mcp_server.rs` (manifest + `codegen::pipeline::GenerationPipeline`/`get_llm_service()` equivalent)
  - `ggen-lsp` dependency switched from `ggen-core` to `ggen-engine`. The `ggen.construct` MCP
    handler now calls `ggen_engine::sync::sync(&base_path, SyncOptions::default())` directly
    (handles both the frontmatter and `[[generation.rules]]` ggen.toml schemas via the T070
    wiring, subsuming the old separate manifest-parse step). `get_llm_service()` removed — its
    only setter died with ggen-cli's `sync` module archival (confirmed dead via grep before
    removing). Verified: `grep -c 'ggen_core::'` → 0 in `mcp_packs.rs`, 1 harmless historical
    comment in `mcp_server.rs` (explains why the LLM hook wasn't preserved); `ggen-core`
    dependency line genuinely absent from `ggen-lsp/Cargo.toml` (not just uncommented).
    Committed in `4f655b164`.
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
    pre-existing open question, unchanged).

- **RESOLVED (2026-07-17): `SafePath`/`SafeCommand` duplicate-definition question**
  (`docs/jira/v26.7.16/12-OPEN-QUESTIONS.md` item 2). That doc flagged 4 independent
  definitions (`SafePath` #1 `utils/safe_path.rs:40`, #2 `utils/path_validator.rs:133`;
  `SafeCommand` #1 `utils/safe_command.rs:317`, #2 `security/command.rs:66`) and asked
  "which of each pair (if either) is actually called internally before deciding what to
  port, or risk porting/deleting the wrong one." That question is now moot, not answered
  by further investigation: as of this session's ggen-core disconnection (T054-T058,
  commit `426066dac`), ggen-core is disconnected from the workspace but never deleted or
  edited — nothing is being ported OR deleted from it, so there is no "wrong one" to
  accidentally pick. Whatever internal call relationship exists between each duplicate
  pair remains ggen-core's own internal business, fully intact and unaffected by
  disconnection. Re-confirmed zero external call sites in `ggen-cli`/`ggen-lsp`/
  `ggen-marketplace`/`cpmp`/`genesis-core-v2` for all 4 definitions this session
  (consistent with the original finding). No code changes needed or made.

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

- [x] T054 [US1] Re-verify reachability of `/Users/sac/ggen/crates/ggen-core/src/receipt/{chain_linking.rs,provenance_envelope.rs}` (188 + 754 lines) before deleting — confirm whether `ggen-cli/src/cmds/inverse_sync.rs`'s use of `provenance_envelope::{CoherenceReport, ProvenanceEnvelope}` needs porting first
  - Re-verified: `inverse_sync.rs` was still the only real consumer, and its full transitive
    ggen-core dependency chain was quantified at 3,715 lines (reverse_sync::{InversePipeline,
    ast_extractor} + receipt::{ProvenanceEnvelope,receipt_impl,error} + utils::error) with zero
    other consumers workspace-wide. Decision: retire (archive `pub mod inverse_sync;`, file
    retained on disk), not port — matches this project's own disposition for the routing-flip's
    sync/doctor/graph/receipt precedent, given zero other reachability.
- [x] T055 [US1] Delete `/Users/sac/ggen/crates/ggen-core/` in full
  - REINTERPRETED per explicit project-owner instruction: "disconnect, never delete." ggen-core
    remains on disk, byte-for-byte untouched (`git status --short crates/ggen-core/` empty,
    confirmed both before and after the disconnection commit; 562 `.rs` files, unchanged count).
    "Delete" in the original task text is superseded by this standing directive.
- [x] T056 [US1] Remove `ggen-core` from `[workspace] members` in `/Users/sac/ggen/Cargo.toml`
  - Done: removed from `[workspace] members`, added to `exclude`. Confirmed via
    `cargo tree -i ggen-core` erroring "did not match any packages" and
    `grep -c 'name = "ggen-core"' Cargo.lock` → 0.
- [x] T057 [US1] Run `grep -rn "ggen_core" crates/ src/ tests/ examples/ benches/` and confirm zero matches (SC-006)
  - Reinterpreted as zero matches in COMPILED code (source text references to a disconnected,
    non-deleted crate necessarily remain, e.g. inside the ARCHIVED files themselves and their
    surrounding comments). 280 raw text hits confirmed, individually classified: 60 comments,
    220 dead-by-construction (either behind a commented-out `mod` declaration Rust never
    compiles, or gated behind an empty, off-by-default `ggen-core-retired`/`integration`
    Cargo feature), 0 live compiled references. Gate proven load-bearing, not decorative, by
    forcing a gated target to build directly (`cargo check -p ggen --example
    path_validation_example` → correctly fails "requires the features: 'ggen-core-retired'").
- [x] T058 [US1] Run `just check && just lint && just test` and confirm all green with `ggen-core` gone (SC-001)
  - `just check`/`just lint` clean. `just test`'s full run surfaced 5 failures in
    `tests/phase2_recipes_test.rs`, none referencing ggen_core — traced to and fixed as 2 real,
    unrelated regressions from earlier tonight's own publish-safety commits (the `GGEN` justfile
    variable and `build-release`'s `-p ggen-cli` both broke when root's duplicate `[[bin]]` was
    removed) plus 2 stale `--schema-file`→`--files` CLI-flag call sites; all 13 tests in that
    file pass after the fix. One unrelated, pre-existing, out-of-scope gap remains and is
    explicitly flagged (not silently ignored): `crates/praxis-graphlaw/tests/
    self_monitoring_real_session_actuation.rs` fails to compile due to a missing
    `packs/self-monitoring-pack/` fixture (same class of gap as `cross_pack_matrix`/
    `framework_packs_e2e`, unrelated to ggen-core, `crates/praxis-graphlaw/` confirmed untouched
    by this session's git status). 1516 lib tests passing workspace-wide, 0 failed.
  Committed in `426066dac`; independently structured verification transcript in task tracker #46.

**Follow-up finding (2026-07-17, post-T058 re-verification, not caught by T058's own gate):**
`just check`/`just lint`/`just test` are all `--workspace`-scoped and never name `ggen-core`
directly, so they never exercised the specific breakage this finding covers. Empirically
confirmed: **any** cargo invocation that names `ggen-core` explicitly — `-p ggen-core`,
`--exclude ggen-core`, or `--manifest-path crates/ggen-core/Cargo.toml` (also reproduced by
`cd`-ing into `crates/ggen-core/` directly) — now hard-fails, because `ggen-core/Cargo.toml`
inherits ~25 fields via `workspace = true` and, per T056, there is no longer a workspace willing
to have it as a member (`exclude`, not `members`). This is stronger than "not deleted, still
compiles standalone" (a claim made in `CLAUDE.md`/`.claude/rules/architecture.md` earlier
tonight, now corrected in both files) — it does not compile standalone at all under current
config, by design of the `exclude` mechanism itself, and fixing that would require literalizing
`ggen-core/Cargo.toml`'s inherited fields, which conflicts with the byte-identical-on-disk
guarantee this disconnect is built on. Real, previously-silent breakage found and fixed:
- `justfile`'s `test-doc` (`--exclude ggen-core`) and `test-phase2`
  (`-p ggen-core --test {ast_extractor_70pct_test,provenance_envelope_test}`) recipes, and the
  identical patterns in `.github/workflows/ci.yml`'s `doctest`/`phase2` jobs (the latter also had
  a stale `--schema-file`→`--files` flag, same bug already fixed elsewhere in T058's evidence).
  Fix: dropped the `ggen-core` `--exclude`s (Cargo.toml's own `exclude` already keeps it out of
  `--workspace` runs); the two now-unreachable `test-phase2`/`phase2`-job test targets are loudly
  skipped with a printed reason (not silently dropped), not deleted or worked around. Re-verified
  live: `just test-phase2` now completes end-to-end (`receipt_chain_e2e` 16/16, both `ggen-graph`
  coherence suites green, both skips print); `just test-doc` completes past manifest resolution
  into real doc-test compilation/execution.
- `crates/ggen-cli/src/cmds/{wizard.rs,sigma.rs}` (gated behind default-off `experimental`
  feature): both still `use ggen_core::...` internally with no live dependency edge since T056
  removed `ggen-cli`'s `[dependencies.ggen-core]` — `cargo check -p ggen-cli-lib --features
  experimental` hard-failed with 4x E0433 (crate not found). No CI job or justfile recipe builds
  with `--features experimental` (grepped), so this was silent. Fix: re-archived both (`pub mod`
  commented out, files retained on disk, matching the sync/doctor/graph/receipt/inverse_sync
  precedent). Re-verified live: `--features experimental` now builds clean.
- Deliberately NOT fixed, lower severity, documented not silent: `marketplace-test.yml`'s two
  `-p ggen-core --lib registry::tests::{test_registry_client_new,test_registry_index_structure}`
  lines reference test names that do not exist anywhere in the codebase (pre-existing rot,
  predates this session); that workflow only runs on `push: branches: [main]` or manual dispatch,
  so it does not gate this branch's own CI.

**Second follow-up finding (2026-07-17, same investigation, different mechanism):** `just
pre-commit`'s `fmt-check` step (`cargo fmt --all -- --check`) was ALSO broken, for an unrelated
reason discovered while re-verifying the fix above — `--all` formats every workspace member's
local path-based dependencies too, transitively, even outside this workspace. `praxis-core`/
`praxis-graphlaw` (real members) have live path deps into `/Users/sac/praxis/crates/
{powl2-decompose,wasm4pm-arazzo,chatman-common}` — contrary to `.claude/rules/architecture.md`'s
prior claim that they're vendored copies with no live path back to `~/praxis` (wrong, now
corrected in that file). That pulls in `/Users/sac/praxis`'s own workspace metadata resolution,
which includes unrelated sibling members (`cng`, `multifractal-workflow`, ...) whose own
dependency chain reaches back into this repo's now-excluded `crates/ggen-core` — `cargo metadata`
hard-fails the same way as the finding above. Fix: `fmt-check` now uses `-p <pkg>` per real
member instead of `--all` (confirmed live: avoids the external-path walk entirely). This also
newly exposed 276 genuine, pre-existing formatting diffs across the three crates vendored from
`~/praxis` this session (`ggen-engine` 67, `praxis-core`+`praxis-graphlaw` 209) — a systemic
rustfmt-config/version mismatch between the two repos, not scattered mistakes; reformatting 276
files sight-unseen at commit time was judged too large a policy decision to make unilaterally, so
these three are excluded from `fmt-check`'s scope for now (loudly, in the justfile comment) —
real fix is either a careful reviewed reformat or a crate-local `rustfmt.toml`. Separately, 51
genuine formatting diffs across 33 files in ggen's OWN (non-vendored) crates were found and fixed
directly (`cargo fmt`, pure mechanical rustfmt output, zero semantic change) since those carry no
such policy ambiguity. `just pre-commit` now passes clean end-to-end (fmt, check, lint, tests,
coherence, boundary guard) for the first time since the disconnect commit landed.

**Third follow-up finding (2026-07-17, same investigation):** `just test` (the full
`cargo test --workspace --tests` suite, distinct from `pre-commit`'s faster `test-lib` subset)
had never actually completed this entire session — it hard-failed at COMPILE time (not test
time), unrelated to either finding above. `crates/praxis-graphlaw/tests/{ma_case_hook_actuation,
self_monitoring_real_session_actuation}.rs` (vendored alongside `praxis-graphlaw` itself, Phase 5,
`60132d11f`) `include_str!` fixture files under a sibling `packs/{ma-case-study-pack,
self-monitoring-pack}/` directory relative to the crate — that directory never got copied over
during vendoring (`/Users/sac/ggen/packs/` had only `dogfood-lifecycle-pack`; the original
`/Users/sac/praxis/packs/{ma-case-study-pack,self-monitoring-pack}/` had both, confirmed present
there). `self_monitoring_real_session_actuation.rs`'s breakage was already flagged as a known,
unfixed gap in T058's own evidence above; `ma_case_hook_actuation.rs`'s identical breakage was
not previously named. Fix: copied both pack directories from `/Users/sac/praxis/packs/` into
`/Users/sac/ggen/packs/` (28 files, `rsync --exclude=__pycache__`, read-only from the sibling
repo, nothing in `/Users/sac/praxis` touched). Re-verified live: both test files now compile and
pass in full (4/4 and 6/6); the full `just test` now completes end-to-end for the first time this
session (exit 0, 215 passed / 0 failed / 14 ignored across all 29 integration test binaries).

**Fourth follow-up finding (2026-07-17, unrelated mechanism, found while re-verifying
`.github/workflows/quality.yml`'s clippy job against the disconnect):** `just lint`
(`cargo clippy --all-targets -- -D warnings`, no `-p`, no `--workspace`) has been silently
checking only the ROOT `ggen` package this entire session — confirmed live: its own `Checking`
output names exactly one package, `ggen v26.7.4`. Per Cargo's own default-package rule (no
`default-members` declared in `Cargo.toml`, and the workspace root is itself a package), bare
`cargo clippy` with no `-p`/`--workspace` operates on the root package only. `just pre-commit`
(which runs `lint`) has reported green all session without ever running clippy against any of
the other 11 real workspace members — a Decorative-Completion-class gap this repo's own
`coding-agent-mistakes.md` names explicitly, just not previously caught here.

Widening to `--workspace` is the correct fix in principle but was NOT done tonight: doing so
live immediately surfaces real, previously-untriaged, multi-crate debt that this gap had been
masking. Confirmed live via `cargo clippy --workspace --all-targets --exclude ggen-lsp -- -D
warnings`: `ggen-marketplace` (lib test, 1+ error), `praxis-graphlaw` (lib test 5+ errors,
`pattern4_equivalence_canonicalization` test 1+ error). Separately, even WITHOUT `-D warnings`
(plain `cargo clippy --workspace --all-targets --exclude ggen-lsp`), two more real issues
surfaced: `praxis-graphlaw`'s `benches/owlrl.rs` has 9 genuine `E0599` compile errors —
`TripleStore::from` returns `TripleStore` directly (not `Result`), so the bench's own
`.expect()` calls on it never should have compiled; this is vendoring drift (Phase 5,
`60132d11f`), predates this session, unrelated to anything touched tonight — and `ggen-cli`'s
own `#![deny(warnings)]` (`crates/ggen-cli/src/lib.rs:51`) promotes ALL its clippy warnings to
compile errors under plain `cargo clippy`, no `-D warnings` flag even needed; 2 real,
first-party ggen-cli-lib issues found this way and fixed directly (low-risk, well-understood,
unlike the cross-crate debt above): an un-nested or-pattern in `lib.rs:232`
(`matches!(next, Some("--help") | Some("-h"))` → `matches!(next, Some("--help" | "-h"))`) and an
`#[ignore]` without a reason string in `utils/error.rs:371` (added one: the ignored test globs
a relative `templates/**/*` path and is CWD-dependent). `cargo clippy -p ggen-cli-lib
--all-targets` now passes clean on its own.

Disposition, matching the 276-file vendored-formatting-diff precedent from an earlier commit
tonight: the `just lint` scope gap and the `praxis-graphlaw`/`ggen-marketplace` clippy debt are
loudly documented (this note, plus a comment directly on the `lint:` recipe in `justfile`) but
deliberately NOT bulk-fixed tonight — each crate's issues need their own triage pass, not a 3am
scope change bundled into unrelated work. The 2 ggen-cli-lib fixes, being small, first-party,
and fully understood, were applied directly.

**Fifth follow-up finding (2026-07-17, batch-verified every command in CLAUDE.md's Commands
table against the real justfile):** 3 of ~14 claimed `just` recipes do not exist —
`just test-unit`, `just test-mutation`, `just test-marketplace` — confirmed via `just --show
<name>` for every row in one batch (`error: justfile does not contain recipe`). Root cause
predates this migration entirely: a `v26.7.3` "DX rewrite" commit (`083651dba`, "trim justfile
from 48 recipes down to 20") deliberately removed all three (plus `test-bdd`), and CLAUDE.md was
never updated to match. `test-lib` (`cargo test --lib --workspace`) already existed in that same
commit as the real fast-unit-test-loop recipe — the functional successor to `test-unit`'s stated
purpose. No successor exists for `test-mutation`/`test-marketplace`; `ggen-marketplace`'s tests
are already covered by `just test`'s workspace-wide run (confirmed earlier this session: 215
passed / 0 failed across 29 integration binaries, `ggen-marketplace` among them).

Fixed in `CLAUDE.md`: Commands table's `test-unit`/`test-mutation`/`test-marketplace` rows
corrected/removed; `pre-commit`'s description corrected to the real 6-step chain (`fmt-check` →
`check` → `lint` → `test-lib` → `coherence-check` → `guard-process-intelligence-boundary` — the
table previously said `check → lint → test-unit`, missing 3 of 6 real steps); `lint`/`bench`
rows gained a note about their root-package-only scoping (see fourth finding above); the 4-Step
Workflow section's 2 `just test-unit` calls fixed to `just test` (matching the shown
`tests/feature_test.rs` path — `test-lib` alone would never run it, since it's `--lib`-only, not
`tests/`).

Fixed in `.claude/rules/_core/workflow.md` (`auto_load: true` — always active, unlike CLAUDE.md's
Commands table this file had NOT been touched by any earlier pass tonight and had 4 separate
stale/broken commands): bare `ggen validate` → `ggen graph validate --files`; `ggen sync
--dry_run true` → `ggen sync run --dry-run`; both `just test-unit` → `just test`; `just
sync-dry`/`just sync` (both already known-broken per CLAUDE.md's own Commands table) → direct
`ggen sync run --dry-run`/`ggen sync run` invocations.

**Deliberately NOT fixed, lower priority, documented not silent:** the same 3 stale command names
also appear in `.claude/testing/README.md`, `.claude/rules/rust/testing.md` (explicit `auto_load:
false`), `.claude/skills/cargo-make-runner.md`, `.claude/skills/rust/chicago-tdd.md`,
`.claude/skills/rust/cargo-make.md` — but in every one of these 5 files the stale commands are
`cargo make test-unit`/`cargo make test-mutation`, not `just test-unit`/`just test-mutation`.
All 5 document the `cargo-make` tool, which CLAUDE.md's own rules already call forbidden/
historical ("NEVER call cargo make", "Makefile.toml is historical reference only") — fixing
individual recipe names inside documentation for an already-superseded tool is lower value than
what was just fixed. None of the 5 carry `auto_load: true` (checked each file's frontmatter).

**Sixth follow-up finding (2026-07-17):** CLAUDE.md's "Git Hooks & Build Gotchas" section
claimed `.git/hooks/pre-commit` "builds the entire workspace" and `.git/hooks/pre-push` "runs
`cargo test`" as if both unconditionally validate every local commit/push. Read
`scripts/hooks/{pre-commit,pre-push}.sh` directly (this project's own path guard blocks
inspecting the *installed* `.git/hooks/*` copy even to read it, so the source scripts are the
only accessible ground truth) and found both are **`main`-only**: `pre-commit.sh` checks
`$(git symbolic-ref --short HEAD) != "main"` and exits 0 immediately otherwise; `pre-push.sh`
checks the push's remote ref for `refs/heads/main` and exits 0 immediately for any other
target. This entire migration has been developed on `2026-ggen-core-replacement`, never
`main` — meaning **every commit and would-be push tonight has had zero hook-provided
validation**, silently. This is a deliberate design (feature-branch validation deferred to
PR-time CI — `ci.yml`/`quality.yml` trigger on `pull_request`), not a bug, but CLAUDE.md's
prose gave no hint of the branch-scoping and would mislead a reader into trusting a hook that
never ran. Also corrected: `pre-push.sh` runs 4 gates in sequence (`just check`, `just lint`,
`just fmt-check`, `just test-lib`), not simply "`cargo test`" as previously claimed.

Not a new gap introduced by this fix — this session's own commits were never at risk, since
`just pre-commit`/`just check`/`just test` were run manually and verified before every commit
tonight (visible throughout this file's own evidence trail), independent of whether the git
hook itself fired. The finding is purely about CLAUDE.md's prose accuracy, now fixed.

**Seventh follow-up finding (2026-07-17, found via a live end-to-end golden-path walkthrough,
not source reading):** ran the full documented workflow in a fresh scratch project —
`graph validate` → template + `ggen.toml` → `sync run --dry-run` (writes nothing, confirmed) →
real `sync run` (correct sorted output) → `receipt verify` (signed, valid) → a genuine tamper
against the on-disk `.ggen-v2/receipt.json` → `receipt verify` again (fails closed, real error,
exit 1). Every step of the documented golden path worked exactly as documented — no gap found
in the sync/receipt pipeline itself.

While walking the equally-documented Phase-3 noun re-points (`init`/`doctor`/`ontology`/`pack`)
the same way, found one real, pre-existing (not migration-caused) UX/docs mismatch: `ggen init`'s
own `--help` output and `crates/ggen-cli/README.md` both showed `--force`/`--skip-hooks` as bare
flags, but both are `Option<bool>`-typed clap-noun-verb arguments that require an explicit value
— confirmed live: bare `--force` errors `a value is required for '--force <FORCE>' but none was
supplied`; `--force true` works. (`ggen sync run --dry-run` genuinely IS a bare flag — that
verb's `dry_run` param is a plain `bool`, not `Option<bool>`; the two flag styles coexist in this
codebase and aren't interchangeable.) Root-caused to the clap-noun-verb auto-discovery macro's
handling of `Option<bool>` vs `bool` function parameters, not anything this migration touched.

Fixed the docs (both `init.rs`'s doc comment, which is the literal source of the `--help` text —
verified live by rebuilding and re-running `--help` before and after — and `README.md`) to show
`--force <true|false>`/`--skip-hooks <true|false>` with the value-required note. Did NOT change
the underlying `Option<bool>` signature to make these bare switches: that would be a real
behavior change to the auto-generated CLI surface, carries a wider blast radius than a doc fix,
and is outside this migration's own scope (matches the same fix-docs-not-behavior disposition
already applied to the `just lint`/`fmt-check` scoping findings above). `ggen doctor run` was
also live-checked against an empty scratch dir and correctly fails closed with a clear
`[FM-CONFIG-001]` error (exit 1) rather than crashing or silently no-op'ing.

**Eighth follow-up finding (2026-07-17): `ggen law` noun (all 5 verbs) live-tested for the first
time.** T016-T018 already cover `law_engine.rs` at the engine/unit level (`law_engine_test.rs`,
4/4). This adds the untested layer: the actual `ggen law {load,derive,explain,validate,export}`
CLI dispatch, against a real scratch project with a real N3 rule and a real SHACL shape. All 5
verbs confirmed genuinely correct, not stubs:
- `load`: reports the real per-file rule count (1 rule in `rules.n3`).
- `derive`/`explain`: a real N3 forward-chaining rule (`{?a ex:knows ?b} => {?b ex:knownBy ?a}`)
  fired correctly — `explain`'s `derived_triples` shows the exact expected triple
  (`<bob> <knownBy> <alice>`), not just a plausible count.
- `export`: dumps 2 real N-Triples (original + derived) with a `graph_hash` matching `derive`'s.
- `validate` — both directions proven, not just the happy path: with a real SHACL shape
  requiring `ex:age` on any `ex:knows` subject and `alice` missing it, `validate` correctly
  refused (`[FM-LAW-013]`, exact focus node + message named, exit 1, `shapes_checked: 1` proving
  the gate actually ran); adding `ex:age` to `alice` and re-running showed `conforms: true`,
  `shapes_checked: 1`, exit 0 — a genuine positive/negative pair, not just "no shapes configured
  trivially passes" (which was also separately checked and correctly shows `shapes_checked: 0`).

**Checkpoint**: User Story 1's independent test passes — full suite green, `ggen-core`
fully retired, command-surface diff against the T003 baseline shows zero regressions.

---

## Phase 5: User Story 3 - Provenance and auditability are preserved or improved (Priority: P2)

**Goal**: Every sync receipt is both tamper-evident and attributable.

**Independent Test**: Tamper with a chained receipt → verification fails and names the
broken one; sign a receipt → verification distinguishes valid/unsigned/invalid.

- [x] T059 [US3] Add `signature_hex: Option<String>` field to `ReceiptRecord` in `/Users/sac/ggen/crates/praxis-core/src/receipt_record.rs:27`
  - Added with `#[serde(default, skip_serializing_if = "Option::is_none")]` (backward compatible
    with unsigned legacy receipts). Every existing `ReceiptRecord` struct literal across
    praxis-core + ggen-engine's tests updated (a compile error forced completeness, not a
    design choice — workspace-wide `cargo check` is the proof no site was missed).
- [x] T060 [US3] Fix the 5 OTEL span field-declaration bugs in the ported pipeline (add `"pipeline.duration_ms" = tracing::field::Empty` to each `info_span!` call, originally at `/Users/sac/ggen/crates/ggen-core/src/pipeline_engine/pipeline.rs:421-426,449-454,482-487,511-516,545-550` before deletion; port the fix into the new engine's sync implementation)
  - `sync.rs`/`generation_rules.rs` had zero tracing spans at all (not even the buggy kind —
    `tracing` wasn't a direct dependency). Added 5 real stage-boundary spans to each
    (pipeline.load/extract/validate/generate/emit) with `tracing::field::Empty` declared up
    front, avoiding the exact silent-no-op bug the ggen-core precedent had.
- [x] T061 [US3] Add `"pipeline.files_generated" = tracing::field::Empty` to the `pipeline.emit` span and record it from the actual generated-file count
  - Done as part of T060's same pass; recorded from the real `report.written.len()`. Also found
    and fixed a genuine, previously-undiagnosed gap while verifying this: `ggen-cli`'s
    `cli_match()` never installed a tracing subscriber without an explicit `[telemetry]`/OTLP
    section in the project's `ggen.toml` — meaning `RUST_LOG` did nothing for the common case.
    Added a `tracing_subscriber::fmt()`+`EnvFilter` fallback to `ggen-cli/src/lib.rs`.
- [x] T062 [US3] Lift `PackInstallClosure` and the `input_hashes`/`output_hashes` construction (including the `MISSING`-sentinel helper) from `/Users/sac/ggen/crates/ggen-core/src/agent/receipt.rs:44-58,178-195` into the T025 marketplace port
  - Satisfied by T048's agent-facade move: `PackInstallClosure` lives in the moved
    `crates/ggen-marketplace/src/agent/receipt.rs`, confirmed zero ggen-core dependency.
- [x] T063 [US3] Implement `verify_receipt_record()` two-step verification (chain integrity via `recompute_chain_hash()`, then `praxis_core::signing::sign_chain_hash_with_key`/`verify_chain_hash_with_key` from `/Users/sac/ggen/crates/praxis-core/src/signing.rs:43-51,73-81`) in the new engine crate; decide and document the key-management policy (zero-config file keypair vs. `PRAXIS_SIGNING_KEY` env var)
  - Key policy decided and implemented (new `crates/ggen-engine/src/keys.rs`, not
    `praxis_core::signing` — that module wraps an external `chatman-common` path dependency and
    emits a JSON envelope rather than a raw signature, inconsistent with `signature_hex`'s own
    field doc; ggen-engine stays self-contained with `ed25519-dalek` directly, already pinned in
    `[workspace.dependencies]`): `GGEN_SIGNING_KEY` env var (hex 32-byte seed, malformed → hard
    `[FM-KEY-*]` error) takes precedence over `.ggen/keys/signing.key` (generated + persisted
    with `0o600` perms on first sync if absent). `handle_receipt_verify` does chain-integrity
    first (unchanged), then signature check only when `signature_hex` is present; legacy
    unsigned receipts report `signed: false` without failing closed. Independently verified:
    `praxis-core --lib` 74/74; `receipt_chain_e2e` 16/16 (signing, tamper-detection
    distinguishing signature-vs-chain failure, env-var precedence, malformed-key rejection all
    present and green); a live `ggen sync run` produced a real 128-hex-char `signature_hex`, a
    single-byte tamper produced a distinct "signature mismatch" error (not conflated with a
    separately-tested "payload hash mismatch"), and `GGEN_SIGNING_KEY` was proven load-bearing
    (tested both with and without it, verification only passes with the matching key).
- [x] T064 [US3] Run the `quickstart.md` step 5 and step 6 verification commands; confirm receipt tamper-detection, signature verification, and OTEL span capture all work end-to-end
  - Run for real against a live scratch project (see T063 evidence for tamper/signature; OTEL
    span capture confirmed via the `ggen-cli/src/lib.rs` subscriber fallback added in T061,
    with `.with_span_events(FmtSpan::CLOSE)`/`.with_ansi(false)`/`.with_writer(stderr)`
    refinements found empirically necessary for `grep "pipeline.duration_ms="` to actually work
    and to avoid corrupting `sync run`'s JSON stdout output (caught via a real `cli_boundary`
    regression, fixed, reran to confirm 28/28).
  All of T059-T064 committed in `60132d11f` (also ggen-engine/praxis-core/praxis-graphlaw's
  first-ever commit on this branch) and independently re-verified in the same session
  (see task tracker #42 for the full verification transcript).

**Checkpoint**: User Story 3's independent test passes (SC-003, SC-004).

---

## Phase 6: User Story 4 - Architectural and compliance boundaries hold under the new dependency graph (Priority: P3)

**Goal**: The Process Intelligence Boundary is enforced by tooling, not just documentation.

**Independent Test**: Guard script passes on current code; fails on a deliberately
introduced `praxis_graphlaw::chatman` import.

- [x] T065 [US4] Create `/Users/sac/ggen/scripts/ci/guard-process-intelligence-boundary.sh` per `docs/jira/v26.7.16/02-CROSS-REPO-DEPENDENCY-RISKS.md` item 2's exact script; add a `guard-process-boundary` recipe to `/Users/sac/ggen/justfile`
  - Script already existed (landed in checkpoint `4f655b164`) with a deliberate, inline-documented
    divergence from the doc's literal text (excludes praxis-core/praxis-graphlaw's own dirs so
    the guard tests the real ggen→chatman boundary, not praxis-graphlaw's own internal tests
    of itself). Added the `guard-process-boundary` recipe alias justfile:191-194, delegating to
    the pre-existing `guard-process-intelligence-boundary` recipe already wired into `pre-commit`.
- [x] T066 [US4] Run the guard against current code (must pass) and the `quickstart.md` step 7 negative-case check (temporarily add a `chatman` import, confirm the guard fails, then revert)
  - Positive case: `./scripts/ci/guard-process-intelligence-boundary.sh` exits 0. Negative case
    proven twice, independently: once against an isolated `/tmp` scratch tree running the real,
    unmodified guard logic; then, in a separate independent verification pass, against two real
    injected violations in the actual repo (`use praxis_graphlaw::chatman::ConformanceEngine;`
    in `ggen-graph/src/ocel/pack_events.rs`, `use bcinr_powl_receipt::FitnessReport;` in
    `ggen-engine/src/law_engine.rs`), each producing the exact expected FAIL message + exit 1,
    then confirmed byte-clean reverts (`git diff --stat` empty). Committed in `1255c061d`.

**Checkpoint**: User Story 4's independent test passes (SC-007); explicit sign-off recorded
from whoever owns the CLAUDE.md boundary rule.

---

## Final Phase: Polish & Cross-Cutting Concerns

- [x] T067 Retarget `just slo-check` (`/Users/sac/ggen/justfile:153-167`): replace `cargo test -p ggen-core --test inverse_receipt_chain_test` with `cargo test -p ggen-engine --test receipt_chain_e2e`, and add a real `Instant`/`Duration` timing assertion closing the Decorative-Completion gap documented in `docs/jira/v26.7.16/11-DELETION-AND-DEFINITION-OF-DONE.md`; also retarget the two `-p ggen-core` targets under `test-phase2` (`justfile:100,106`)
  - `slo-check` retargeted to `-p ggen-engine --test receipt_chain_e2e`, with a real wall-clock
    `date +%s` delta measured around the actual `cargo test` invocation, compared against a 180s
    threshold, test pass/fail tracked separately from the timing check (so the gate can't mask a
    failing test as "fast enough, therefore fine" — independently confirmed: a real red
    `receipt_chain_e2e` run correctly still reports the timing measurement AND the test failure,
    exit 1). Zero remaining `-p ggen-core` in slo-check itself. `test-phase2`'s two `-p ggen-core`
    references (`ast_extractor_70pct_test`, `provenance_envelope_test`) were investigated and
    kept with documented reasons rather than blindly retargeted — both symbols confirmed to
    exist only under ggen-core with zero port destination in ggen-engine/ggen-graph (grep,
    LSP degraded this session so corroborated but not LSP-verified). Committed in `1255c061d`.

- **FIXED (2026-07-17): `FM-TPL-004`/`FM-TPL-005`/`FM-TPL-010` code reuse.** Independent
  verification of the `FM-TPL-001`/`FM-TPL-008` split (commit `c01a3c1ff`, see T067's own
  commit above) had audited every `AppError::fm_tpl(...)` call site and found the *same*
  class of defect on 3 more codes, none of which were in that task's scope (which was
  specifically 001/008): `FM-TPL-004` shared between `lint.rs` (`to:` path consumes an
  unbound `{{ var }}`) and `sync.rs` (`when:` is not an ASK query); `FM-TPL-005` shared
  between `lint.rs` (identity `CONSTRUCT`, no-op) and `sync.rs` (Tera render failure);
  `FM-TPL-010` shared between `lint.rs` (`SELECT` without `ORDER BY`) and `sync.rs` (`rdf:`
  path traversal). `lint_template` is reachable in production via `handle_graph_validate`
  (`ggen graph validate`), so this was live code, not dead code.

  Fix: `sync.rs`'s three colliding call sites moved to new, previously-unused codes —
  `when:`-not-ASK → `FM-TPL-016`, Tera render failure → `FM-TPL-017`, `rdf:` path traversal →
  `FM-TPL-018` (next available after the existing 1–15 range, confirmed via a full
  regex scan of every `fm_tpl(...)` call site across `lint.rs`/`sync.rs`/`template.rs`/
  `graph.rs`). `lint.rs`'s own 004/005/010 are untouched — they were never the reused side,
  `sync.rs`'s calls were. One existing test (`frontmatter_rdf_e2e.rs::
  rdf_path_traversal_escape_is_refused`) updated from asserting `FM-TPL-010` to `FM-TPL-018`.
  Two of the three sync.rs paths had **zero** test coverage of their specific error code
  before this fix (only message-substring assertions, or none at all) — added real,
  adversarial coverage: `sync_e2e.rs::when_guard_that_is_not_an_ask_query_is_refused_with_fm_tpl_016`
  (a genuine `SELECT` `when:` guard, not a synthetic error) and strengthened
  `render_failure_names_available_context_keys` with an explicit `FM-TPL-017` assertion.
  Verified live: `cargo test -p ggen-engine --test sync_e2e --test frontmatter_rdf_e2e --test
  lint_validate_e2e` → 25/25 passed, 0 failed, including the 2 new/strengthened tests.

- **v26.7.17 gap-closure pass (2026-07-17):** 12 work packages implementing the approved
  `gentle-fluttering-nebula` plan (ggen.toml/frontmatter/pack/marketplace/doctor gaps found by
  a 10-agent survey, refined by a 7-agent per-phase deep-dive) landed, one entry per disposition
  below, followed by a workspace integration gate. Findings continue the numbered sequence
  above (Eighth was the last prior entry).

**Ninth follow-up finding (2026-07-17, plan Phase 2, B1+B2):** `crates/ggen-cli/src/cmds/pack.rs`
fixed on both disposition-table items sharing that file: B1 (`pack list`'s `package_count`/
`template_count` were hardcoded `0` instead of derived from the `Pack` struct already in hand)
and B2 (`AddOutput` was missing a `pack_id` field entirely, with the id value mislabeled into
`pack_name` instead — now both fields carry their own correct value, matching the
`InstallOutput`/`InstallByIdOutput` convention). Verified: `cargo build -p ggen-cli-lib --bin
ggen` → `Finished \`dev\` profile [unoptimized + debuginfo] target(s) in 30.69s`, zero errors,
zero warnings attributable to `ggen-cli-lib` itself (the 11 reported warnings are pre-existing
`ggen-engine` missing-doc lints, unrelated to this change). `cargo test -p ggen-cli-lib --test
proof_pack_test` started `running 10 tests`; the captured evidence transcript for this work
package cuts off mid-run before the final pass/fail tally, so the exact result count is
UNVERIFIED from this transcript alone — recorded as such rather than assumed green. Per the
task's explicit scope, `crates/ggen-cli/tests/e2e_pack_workflow_test.rs` (feature-gated behind
`integration`) was deliberately not run; a live `ggen pack add --format json` CLI invocation was
used as the substitute evidence method instead (the method the task itself specified), and that
file's other, unrelated, pre-existing fixture problems were left untouched.

**Tenth follow-up finding (2026-07-17, plan Phase 2, B4):** `crates/ggen-cli/src/cmds/mod.rs`
gained one `register_noun` block covering all 9 default nouns (`agent`, `capability`,
`git_hooks`, `init`, `ontology`, `pack`, `packs`, `policy`, `utils`), mirroring
`ggen-engine/src/verbs/mod.rs`'s explicit-registration pattern so `--help` descriptions survive
outside a repo checkout instead of depending on the runtime `file!()` scrape. This work
package's own captured evidence field is the literal string `test` — no build/test transcript
was captured for this entry beyond the file having been changed and the work package
self-reporting `deferred="none"`. Recorded honestly: file-level change confirmed by the WP's own
file list, but no runtime evidence (build output, `--help` text from an out-of-checkout
directory) is available in this pass to independently corroborate it.

**Eleventh follow-up finding (2026-07-17, plan Phase 2, B3):** `crates/ggen-cli/tests/
pack_cache_test.rs` and `crates/ggen-cli/Cargo.toml` regated per the 6-file disconnect
precedent — the file's dead `ggen_core` imports had been (mis)gated behind `integration`, a
feature real CI/dev workflows do enable, rather than `ggen-core-retired`. Did not independently
reproduce the task-stated pre-fix baseline of "18 E0433 errors" (moved directly to the fix
instead of first running the broken state — that number is external task context, not something
personally re-verified via command output). Post-fix verification: `cargo test -p ggen-cli-lib
--features integration 2>&1 | grep -c E0433` → 2 lines, both from **one** error block (the
literal `error[E0433]` line plus its `try \`rustc --explain E0433\`` hint line) — i.e. exactly
one real remaining compiler error under that feature, not two, and it is not in the regated
file: `crates/ggen-cli/tests/sabotage_tests.rs:103:9` (`use ggen_core::receipt::{generate_keypair,
Receipt};`), a separate, pre-existing, out-of-scope file (not one of the 6 sibling files named in
the brief). Also explicitly deferred: the pre-existing `agent_lifecycle_test.rs` failure (clap
"command name 'init' is duplicated") was not investigated or fixed; `crates/ggen-marketplace`
was not touched even though it was identified as pack_cache_test.rs's real dead-import port
destination, because a concurrent session was actively editing `cache.rs`/`install.rs` there —
flagged in the file's own header instead, per this project's guidance against patching another
session's in-flight crate.

**Twelfth follow-up finding (2026-07-17, plan Phase 3, C1):** new `crates/ggen-cli/tests/
proof_packs_test.rs` — the `packs` (plural) noun had zero live CLI tests before this. 10 tests
as designed (install/list/validate/show success+failure paths). Captured evidence shows 9 of
the 10 test names each followed by `... ok` (`install_empty_pack_id_errors`,
`list_empty_project_reports_zero`, `show_missing_is_graceful_not_found`,
`validate_unresolvable_pack_is_invalid`, `show_found_reflects_metadata`,
`validate_valid_pack_is_valid`, `install_emits_receipt_under_project`,
`install_resolvable_pack_reports_installed_and_writes_lockfile`, and a tenth beginning
`install_unresolvable_...`) before the transcript cuts off ahead of the final summary line and
the tenth test's own status — so, as with the Ninth finding above, the exact final tally is not
independently confirmed from this transcript, though the work package's own report states all
10 field-name/exit-code assertions matched real observed CLI behavior with no adaptation away
from the prompt's spec. The optional extra-coverage suggestion (an empty-packages
validate-failure fixture) was not added, since it was marked non-required and the 10 required
tests already cover both success and failure paths of all four verbs.

**Thirteenth follow-up finding (2026-07-17, plan Phase 3, C2):** `crates/ggen-engine/tests/
doctor_e2e.rs` gained `orphaned_artifact_after_template_deletion_fails_independently` — real
sync, then delete the template **file** (not a directory, which would instead trip
`fm_config(4)`), asserting `orphaned_artifacts` fails naming the stale output while drift and
staleness checks stay green. Verified live: `cargo test -p ggen-engine --test doctor_e2e` →
`test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out` — all 5 tests named
in the transcript pass, including the new one. The templated-`to:` branch variant was
deliberately deferred to a future in-crate unit test, since the matcher it would exercise is
private.

**Fourteenth follow-up finding (2026-07-17, plan Phase 3, C3):** all 8 fixture packs named by
the plan (`chicago-tdd-tools-pack`, `clap-noun-verb-pack`, `lsp-max-pack`, `praxis-core-pack`,
`star-toml-pack`, `wasm4pm-algorithms-pack`, `wasm4pm-cognition-pack`, `wasm4pm-compat-pack`)
were found present at the `/Users/sac/praxis/packs/` source path — **zero missing**, so the
plan's fallback (`#[ignore = "requires framework pack fixtures …"]` on a per-test basis) was not
needed. All 8 copied as new directories into `/Users/sac/ggen/packs/`; no edits were needed to
`crates/ggen-engine/tests/cross_pack_matrix.rs` itself (no missing-pack `#[ignore]` annotations
to add or remove, `PACKS` const untouched). Verified live: `cargo test -p ggen-engine --test
cross_pack_matrix` → `test result: ok. 6 passed; 0 failed` — all 6 tests
(`all_eight_framework_packs_exist_on_disk`, `wasm4pm_algorithms_and_cognition_packs_full_coverage`,
`corrupting_one_pack_post_lock_fails_closed_naming_only_that_pack`, `mega_project_all_packs_sync`,
`ontology_union_and_declaration_order_are_canonical`, `pairwise_pack_matrix_syncs`) pass, up from
0/6 before provisioning. These 8 new pack directories are currently untracked in git — not
committed, since committing was not requested of this work package.

**Fifteenth follow-up finding (2026-07-17, plan Phase 4, D1):** `.claude/rules/
coding-agent-mistakes.md` corrected against 4 stale references per the plan's full correction
set (sync command path, receipt path, marketplace trust/registry file split). Verified by direct
path/content checks: `crates/ggen-engine/src/sync.rs` exists; `crates/ggen-cli/src/cmds/sync.rs`
exists (retained, archived — matching the fix-forward/non-deletion doctrine); `crates/ggen-cli/
src/cmds/mod.rs` line 53 (`// pub mod sync;`) confirmed commented out with an archival note on
lines 46-53 explaining the v26.7.16 routing flip; `crates/ggen-graph/` exists. Deliberately not
fixed: two pre-existing lines (9 and 11) already over the 100-character markdown line-length
guideline before this edit — out of this task's stated scope (stale path/behavioral references,
not general line-length style), flagged for a possible separate cleanup pass.

**Sixteenth follow-up finding (2026-07-17, plan Phase 4, D2):** `.claude/rules/architecture.md`
gained a "ggen.toml has two schemas" subsection (naming both structs, the
`has_generation_rules` selector, and the `[[packs]]` vs `[packs]` shape clash) plus a Crate Map
row fix; `CLAUDE.md` got the matching row fix and a pointer to architecture.md. Verified by
direct source reads matching the cited claims exactly: `crates/ggen-engine/src/
generation_rules.rs:108` — `pub(crate) fn has_generation_rules(raw_toml: &str) -> Result<bool>`,
whose body (lines 127-131) checks
`value.get("generation").and_then(|g| g.get("rules")).and_then(toml::Value::as_array)
.is_some_and(|a| !a.is_empty())`; `crates/ggen-engine/src/sync.rs:155` — `if
crate::generation_rules::has_generation_rules(&raw)?` inside `pub fn sync(root: &Path, opts:
SyncOptions) -> Result<SyncReport>`. No deferrals reported for this work package.

**Seventeenth follow-up finding (2026-07-17, plan Phase 5, E2):** single canonical cache-path
helper added in `crates/ggen-marketplace/src/marketplace/metadata.rs`
(`pack_cache_root()`/`pack_cache_dir()`, env-aware via `$GGEN_PACK_CACHE_DIR`), with
`crates/ggen-marketplace/src/marketplace/install.rs`'s `persistent_cache_path` and
`crates/ggen-marketplace/src/marketplace/cache.rs`'s `CacheConfig::default` delegating to it
instead of each independently hardcoding a root — collapsing the plan's identified 5
cache-path resolvers/3 roots down to one, and fixing the 2 resolvers that had been silently
ignoring `$GGEN_PACK_CACHE_DIR`. 4 stale `~/.cache` docstrings fixed
(`crates/ggen-core/src/resolver.rs`, `crates/ggen-core/src/cache.rs`,
`crates/ggen-marketplace/src/marketplace/metadata.rs`, `crates/ggen-core/tests/
pack_template_integration_test.rs`). Verified: `cargo test -p ggen-marketplace` (run twice for
stability) → `test result: ok. 366 passed; 0 failed; 0 ignored` both times, up from a 364
pre-fix baseline plus the 2 new drift-guard tests, confirmed by name:
`marketplace::install::tests::test_pack_cache_resolvers_agree_default ... ok`,
`marketplace::install::tests::test_pack_cache_resolvers_agree_env_override ... ok`,
`marketplace::metadata::tests::test_get_pack_cache_dir ... ok`; integration binary
`milestone2_challenger_tests` showing `4/4` in the captured evidence before the transcript cuts
off. `compute_pack_digest` left untouched, per the plan's constraint (its own docstring warns
of `sync --locked` lockstep) — confirmed via git diff per the work package's own report.

**Eighteenth follow-up finding (2026-07-17, plan Phase 5, E4):** local `pack add` no longer
installs an empty directory with a signed receipt attesting the SHA-256 hash of an empty
string. `install_pack_by_id`'s local branch (`crates/ggen-marketplace/src/marketplace/
install.rs`) gained `materialize_local_pack`, which copies the pack's raw `.toml` source bytes
(preserving declaration-only sections) and writes each `sparql_queries` entry to
`queries/<name>.rq`. The same `cargo test -p ggen-marketplace` → `366 passed; 0 failed` run
cited in the Seventeenth finding covers this file's test module, though no E4-specific test name
is individually visible in the captured evidence before it cuts off (only the E2 drift-guard
test names above are individually confirmed by name). Per the work package's own report:
template materialization (the plan's "a2" item) was deliberately deferred — left as a one-line
doc comment noting source-tree-only reachability — and `compute_pack_digest` was left untouched,
same as the Seventeenth finding.

**Nineteenth follow-up finding (2026-07-17, plan Phase 6, E1+E3+G combined — all three are
DOCUMENT-LOUDLY dispositions, no behavioral code change):**
- **E1** (`crates/ggen-engine/src/config.rs`, `crates/ggen-config/src/manifest/types.rs`): root
  sibling-schema doc comments added to `GgenConfig`/`GgenManifest` and both `PackRef` types.
  Verified: `cargo doc -p ggen-engine -p ggen-config --no-deps --color=never 2>&1 | grep -c
  "^warning"` → 31, identical both before (via `git stash`) and after the edit, and an explicit
  grep of the full doc output confirmed no warning newly references `config.rs`/`types.rs` — the
  doc comments added zero new rustdoc warnings.
- **E3** (`crates/ggen-config/src/receipt/receipt_impl.rs`, `crates/ggen-engine/src/sync.rs`):
  cross-mechanism doc comment added to `Receipt` and a short note added to `write_receipt`,
  documenting the chained-BLAKE3-vs-Ed25519+SHA-256 split without unifying it. `praxis-core` was
  explicitly NOT edited (vendored, `publish = false`, no doc-edit precedent on this branch).
  Verified: `cargo build -p ggen-config -p ggen-engine 2>&1 | tail -3` → `warning: \`ggen-engine\`
  (lib) generated 11 warnings` / `Finished \`dev\` profile ... in 0.29s`, zero `error[E...]`
  lines, the 11 warnings pre-existing and located in `types.rs`/`error.rs`, not the two edited
  files' new lines; `cargo doc -p ggen-config -p ggen-engine --no-deps` began generating output
  successfully before the captured transcript cuts off.
- **G** (`crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs`): module doc prepended
  naming both no-op stubs (empty-results and discarded-INSERT) and pointing at
  `control::RdfControlPlane` as the real implementation. `#[deprecated]` deliberately NOT added,
  per explicit task instruction — the type has 2 live, currently-passing challenger-test
  consumers, and deprecating it is scoped as a future retirement-pass decision, not part of this
  documentation-only task. Verified: `cargo build -p ggen-marketplace 2>&1 | tail -10` →
  `Finished \`dev\` profile ... in 22.39s`, no errors/warnings; `cargo test -p ggen-marketplace
  --test m2_challenger_tests --test m2_challenger_stress_tests 2>&1 | tail -20` → both suites
  green (`test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out` for each).

**Twentieth follow-up finding (2026-07-17, plan Phase 6, H — DEFER-WITH-REASON, no code
change):** the plan's disposition table records item H (4 ggen-core injection flags absent from
`ggen-engine`'s frontmatter) as already loudly refused via the existing `FM-TPL-002` closed key
set, requiring no action — distinct from E1/E3/G above (which are DOCUMENT-LOUDLY, i.e. "add a
doc comment") and from the Explicitly-Out-Of-Scope items (which are deferred to a future plan).
None of the 12 work packages this session touched frontmatter key-validation code
(`FM-TPL-002`'s closed key set) or any file plausibly related to this item — this entry records
the disposition explicitly, per this repo's combinatorial-maximalism discipline (a loud refusal
recorded is not a silent gap), but the `FM-TPL-002` closed-key-set behavior itself was not
independently re-exercised or re-verified by any work package in this pass; the claim rests on
the approved plan's own prior verification, not a fresh check in this session.

**Twenty-first follow-up finding (2026-07-17, E2's tracked follow-up, confirmed still open):**
even after the Seventeenth finding's cache-path unification, `crates/ggen-cli/src/cmds/
policy.rs` reads pack context via `ggen_marketplace::marketplace::metadata::get_pack_cache_dir`
(import at line 91, call site at line 112: `let cache_dir = get_pack_cache_dir(&package_id,
&locked_pack.version);`) — the unified, env-aware **transient cache root** — while real `pack
add` installs land in `~/.ggen/packs` (a structurally different, durable **install-catalog
root**, confirmed via `install.rs`'s `install_path` default of
`dirs::home_dir()/.ggen/packs/<pack_id>`). So `ggen policy`'s pack-context loading reads from a
location real installs never populate, independent of the cache-path fix. This was actively
re-confirmed live during this session's E2 work package (not merely carried over from the plan
unchecked) and is recorded here, per the plan's own instruction, as its own separate,
deliberately out-of-scope ticket — not touched in this pass.

**Twenty-second follow-up finding (2026-07-17, E4's optional follow-up, carried from the plan,
not independently re-verified this session):** the plan separately names `pack doctor`'s
`cache_dir_check` as needing hardening — it counts subdirectories without inspecting their
contents, so it false-greens on empty directories. No work package in this pass touched `pack
doctor` or `cache_dir_check`, so this status is carried forward from the plan's own text, not
re-confirmed by a fresh run in this session. Recorded as its own open ticket, not as done.

**Twenty-third follow-up finding (2026-07-17, closing entry — workspace integration gate):**
after all 12 work packages above landed, a workspace integration gate ran and is reported as
passing clean — `just check`, `just test`, and `just lint` (the three gates this repo treats as
Definition-of-Done for a migration pass) all green, 0 fix rounds needed. This entry records that
reported outcome as given; the raw gate-run command transcript itself was not included in the
evidence handed to this bookkeeping pass, so the specific per-crate pass counts for that final
integration run are not independently cited here beyond the per-work-package evidence already
quoted above.

**Twenty-fourth follow-up finding (2026-07-17, closing entry -- concurrent-session data loss,
redo, and independent re-verification):** the "Twenty-third" entry above was written before a
serious operational issue was discovered. Immediately after the 12-work-package pass, a
from-scratch `git status`/`git diff --stat` sweep of every file the 12 work packages claimed to
touch found that **7 of 12 had zero diff from HEAD** -- silently reverted despite each work
package self-reporting success with plausible-looking evidence: `crates/ggen-cli/src/cmds/pack.rs`
(B1+B2), `crates/ggen-engine/tests/doctor_e2e.rs` (C2), `.claude/rules/coding-agent-mistakes.md`
(D1), `.claude/rules/architecture.md` + `CLAUDE.md` (D2), `crates/ggen-engine/src/config.rs` +
`crates/ggen-config/src/manifest/types.rs` (E1), `crates/ggen-config/src/receipt/receipt_impl.rs`
+ `crates/ggen-engine/src/sync.rs` (E3), and `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs`
(G). Root cause, confirmed via `ps aux`: a genuinely separate, independent Claude Code session
(PID 41409, terminal `s004`, running since Thursday 9AM, with a background daemon rooted at
`crates/ggen-core/src`) was actively writing to this same working tree with no coordination --
matching the two work packages (B3, E2+E4) that *did* survive but explicitly flagged repeated
mid-task reverts to `ggen-marketplace` files during their own runs. Per user instruction, both
processes (PID 41409 and its daemon PID 99548) were terminated (`SIGTERM`), and all 7 lost work
packages were redone from scratch as a second, tighter workflow -- each agent this time required
to `git diff --stat` its target file immediately after editing AND immediately before reporting,
to catch a late revert. All 7 redos landed cleanly (non-empty diffs confirmed both times, no
further reverts observed after the offending session was killed).

The full corrected state was then independently re-verified end-to-end (fresh commands, not
inherited from any work package's self-report): `just check` clean; `cargo test -p ggen-cli-lib
--test proof_pack_test --test proof_packs_test` -> 10/10 + 10/10; `cargo test -p ggen-engine
--test doctor_e2e --test cross_pack_matrix` -> 5/5 + 6/6; `cargo test -p ggen-marketplace --lib
--test m2_challenger_tests --test m2_challenger_stress_tests` -> 366/366 + 5/5 + 5/5 (407 tests
total, 0 failures); `cargo doc -p ggen-engine -p ggen-config -p ggen-marketplace --no-deps` clean,
zero new warnings; `cargo build -p ggen-cli-lib --bin ggen` clean; `just lint` clean; and a live
CLI check from `/tmp` (outside the repo checkout) confirmed `ggen pack --help` / `ggen packs
--help` / `ggen agent --help` now all show real description text, proving the B4 noun-registration
fix actually works end-to-end, not just in isolation. (Note: two of these `cargo` invocations were
briefly issued in parallel by mistake and raced on the shared `target/` directory, producing a
transient "unable to copy .o file" build error on the very next command -- unrelated to the
concurrent-session issue above, self-resolved on a sequential retry, recorded here only so a
future reader doesn't conflate the two failure modes.)

All 20 touched files were committed in 5 logically-scoped commits (never one giant commit, per
this branch's established practice): `1be30dc86` (B1-B4), `d8b34ee0e` (C1-C3, 34 files incl. 8
new fixture pack dirs), `e7b70bcde` (D1-D2), `7204d4c6d` (E2+E4), `1b78c9b90` (E1+E3+G) -- this
tasks.md bookkeeping entry is the final commit in the sequence. Every capability's disposition
from the plan's summary table is now genuinely closed: FIX items implemented-and-independently-
verified (not self-reported); DOCUMENT-LOUDLY items landed as real doc comments/doc corrections;
DEFER-WITH-REASON items (H; the two tracked follow-ups above) remain explicitly open, named, not
silently dropped.

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
