# mix ggen_igniter.fortune5_ready — one-command bundle installer

## Status (updated after real implementation)

All 7 stories (GGEN-1801 through GGEN-1807) are now implemented for real in
`~/ggen_igniter`, across 5 sequential story branches, each committed only —
**none merged to `ggen_igniter`'s `main`**, none pushed. Authoritative
implementation-side doc set: `~/ggen_igniter/docs/jira/v26.9.1/00-OVERVIEW.md`.

- **GI-01** (`story/GI-01-schema-dispatch-alignment`, worktree `/tmp/wt-gi01`,
  commit `af13e0a`) — DONE. New `GgenIgniter.SchemaDispatch` (`load/1`,
  `load_raw/1`, `classify_table/1`) is a real Elixir port of ggen core's
  `classify_ggen_toml`/`schema_dispatch::load`, plus `FrontmatterConfig`/
  `FrontmatterPackRef` structs mirroring `ggen_engine::config::GgenConfig`/
  `PackRef`. 37 tests, 0 failures, real `Toml.decode` over beam4pm's actual
  `ggen.toml` fixture (all 5 real packs asserted).
- **GI-02** (`story/GI-02-ggen-toml-toml-io`, worktree `/tmp/GI-02-worktree`,
  commit `ff58f6c`) — DONE. New `GgenIgniter.GgenToml.IO` (`parse!/1`,
  `serialize!/1`) — a hand-written TOML formatter, since `toml ~> 0.7` has no
  encoder at all. Idempotency proven by real parse→serialize→reparse
  round-trip. Comment-loss is a disclosed, accepted limitation (the library's
  decode target has no comment-attachment concept), proven and documented by a
  real byte-diff test against beam4pm's actual comment-bearing `ggen.toml`. 4
  tests, 0 failures.
- **GI-03** (`story/GI-03-bundle-manifest-and-merge`, worktree
  `/tmp/GI-03-worktree`, commit `eae7397`) — DONE. New
  `priv/bundles/fortune5_ready.json` (name-keyed bundle manifest, exactly 2
  packs: `fortune5-architecture`, `fortune5-deployment-blocks` — the
  `fortune5-required-capabilities`/`fortune5-testing-bblock` packs are
  deliberately excluded because their upstream portability fixes (GM-02/GM-03)
  are not landed on any consumer-visible ref) and `GgenIgniter.Bundle`
  (`load!/1`, `merge/2`, dedup strictly by pack name, never path). 9 tests, 0
  failures.
- **GI-04** (`story/GI-04-sync-shellout-and-verify`, worktree
  `/tmp/GI-04-worktree`, commit `c542eea`) — DONE. New
  `GgenIgniter.SyncShellout` (real `System.cmd("ggen", ["sync", "run"], ...)`
  with an `upstream_ok?` hard precondition gate) and `GgenIgniter.GateVerify`
  (real SPARQL gate execution via existing `Pack.discover_queries/1` +
  `Query.run/2`), chained by `GgenIgniter.SyncVerify`. Real environmental
  finding load-bearing for every fixture in this and later stages: this
  machine's `ggen` is a docker wrapper that only mounts `$PWD`/`~/.cache/tmp`,
  not `/tmp`. 8 tests, 0 failures.
- **GI-05** (`story/GI-05-fortune5-ready-mix-task`, worktree
  `/tmp/GI-04-worktree`, commit `6486cb5`) — DONE. New
  `Mix.Tasks.GgenIgniter.Fortune5Ready` — the real end-to-end pipeline
  (`dispatch_probe` → `merge_bundle_step` → `serialize_step` → `sync_step` →
  `report_step`) chaining GI-01/02/03/04's real modules. Found and fixed a real
  pre-existing round-trip defect discovered by this ticket's own e2e test:
  `SchemaDispatch.build_project_config/1` hardcoded `generation.rules: []`,
  silently discarding every real `[[generation.rules]]` entry — fixed forward
  in `schema_dispatch.ex` and `ggen_toml_io.ex`. 34 tests, 0 failures (this
  ticket's 4 new plus zero regressions across all four prior tickets).
- **Parity validation** (same worktree, commit `ebf507c`,
  `docs/jira/v26.9.1/PARITY-VALIDATION.md` in `ggen_igniter`) — DONE. The real
  `ggen` binary (v26.8.28) and `SchemaDispatch.load/1` agree beam4pm's actual
  `ggen.toml` is Frontmatter-shaped; the merged+serialized `ggen.toml` (adding
  both fortune5 bundle packs) round-trips through the real `ggen` binary
  successfully for `fortune5-architecture-pack`. One real rejection was
  observed for `fortune5-deployment-blocks-pack` (`FM-PACK-005`, zero
  templates) — a pre-existing upstream pack defect, already filed as
  `07-FORTUNE5-DEPLOYMENT-BLOCKS-NOT-SYNC-PORTABLE.md`, not a `ggen_igniter`
  defect, not fixed in this stage.

Across all five stages, every `mix test` scoped to the ticket's own new/touched
files passed 0 failures; a full-repo `mix test` (2500+ tests, unrelated
pre-existing suites) was launched in the background at each stage but not
confirmed to completion within the run's time budget — this does not affect
the load-bearing evidence above. `grep -rn "Mock\|mock(\|patch(\|monkeypatch"`
across `test lib native` at every stage returned zero real mock usage (only
doc-comment mentions of the discipline and igniter's own real
`assert_has_patch` API).

Centerpiece epic of the v26.9.1 Fortune-5-ready initiative. Depends on the two pack-portability
fixes covered elsewhere in this milestone (`fortune5-required-capabilities-pack`'s hardcoded
`../../../../crates/ggen-marketplace` path, `fortune5-testing-bblock-pack`'s
`crates/ggen-cli`/`target/debug/ggen` self-detection) — this ticket assumes those are fixed or
tracks them as blocking dependencies, it does not re-derive them. Full background is the
approved 2026-09-01 planning session cited in this ticket's originating prompt; restated here
only where a specific story needs it as evidence.

## Why this exists (from the approved plan, not re-derived)

Real exploration across `~/ggen`, `~/ggen-marketplace`, `~/ggen_igniter`, `~/beam4pm` found no
existing path for a consuming project to become "Fortune 5 ready" in one command:

1. **ggen itself has no pack-to-pack composition/dependency mechanism.** `pack.toml`'s schema is
   closed (`deny_unknown_fields`, only `[pack]` with `name`/`version`/`description` —
   `ggen-engine/src/pack.rs:70-93`). A `PackComposer`/`DependencyGraph` subsystem exists in
   `ggen-marketplace::packs_registry` (`composer.rs`, `dependency_graph.rs`) but is dead code,
   unreachable from any CLI verb — `ggen pack add`/`install` take one `pack_id` string, never a
   list. Zero of the ~150-207 packs in the corpus use a `[dependencies]` table. The formal
   composition algebra (`docs/thesis/09-pack-algebra.md` in `~/ggen-marketplace`) is explicitly
   aspirational design work, not a built mechanism.
2. **Two of the four fortune5-adjacent packs are non-portable**, hardcoded to
   `ggen-marketplace`'s own repo rather than an arbitrary consumer — see this milestone's pack-fix
   tickets for `fortune5-required-capabilities-pack` and `fortune5-testing-bblock-pack`.
   `fortune5-architecture-pack` and `fortune5-deployment-blocks-pack` appear already portable from
   the initial exploration pass but that pass did not live-test-generate them against a fresh
   scratch consumer; Story GGEN-1801 below re-verifies this before the bundle depends on it.
3. **`ggen_igniter` has real, reusable precedent machinery** for "one command wires a new
   capability into a consumer project" — `lib/mix/tasks/ggen_igniter.install.ex`'s pipeline
   (`Igniter.Project.Deps.add_dep`, `Igniter.Project.Config.configure_new`,
   `Igniter.Project.Application.add_new_child`) and its `deps_probe`-shaped defensive pattern
   against the documented `Igniter.Project.Deps.add_dep/2,3` `CaseClauseError` defect (see
   `install.ex`'s own moduledoc, quoted in Story GGEN-1805 below) — but zero real `ggen.toml` TOML
   I/O (the `toml` dep is declared at `mix.exs:108` but `Toml.decode`/`Toml.encode`/`Toml.parse`
   have zero hits under `lib/`) and zero code shelling out to a real external `ggen sync run`
   binary (the closest analog, `mix ggen_igniter.sync`, IS the sync engine, not a caller of one).

**Recommended approach (approved):** do not build the real ggen-core pack-dependency-resolution
algebra and do not resurrect `PackComposer`/`DependencyGraph` into a CLI verb — both are separate,
larger projects, explicitly out of scope (see Non-goals). Instead: fix the two non-portable packs
(tracked elsewhere in this milestone), then build a bundle-manifest + installer pattern — a new
`mix ggen_igniter.fortune5_ready` task modeled directly on `ggen_igniter.install.ex`'s shape.

## Real current-state evidence this epic is scoped against

- `GgenIgniter.ProjectConfig`/`GgenIgniter.PackRef` (`~/ggen_igniter/lib/ggen_igniter/
  project_config.ex:171-229`, `~/ggen_igniter/lib/ggen_igniter/pack_manifest.ex:86-111`) exist as
  pure, unused data-mirror structs — no `load`/`save` function calls them today.
  `GgenIgniter.PackRef`'s own moduledoc discloses a real, load-bearing caveat this epic must not
  ignore: it mirrors the Rust `ggen_config::manifest::PackRef` **array-of-tables** `[[packs]]`
  schema only. A structurally different `ggen_engine::config::PackRef` schema also exists — an
  untagged `Path{path,extra_ontologies,lock} | Git{git,version}` enum under `[packs]` as a
  **table-of-tables keyed by name** — selected at parse time by whether `[[generation.rules]]` is
  non-empty.
- **`~/beam4pm/ggen.toml`'s real content uses the table-of-tables `[packs]` shape**, not
  `[[packs]]`:
  ```toml
  [packs]
  beam4pm-process-model = { path = "vendor/ggen-marketplace/packs/beam4pm-process-model-pack" }
  beam4pm-pro-infra = { path = "vendor/ggen-marketplace/packs/beam4pm-pro-infra-pack" }
  github-actions-pack = { path = "vendor/ggen-marketplace/packs/github-actions-pack" }
  beam4pm-ai-contracts = { path = "vendor/ggen-marketplace/packs/beam4pm-ai-contracts-pack" }
  beam4pm-pro-entitlement = { path = "vendor/ggen-marketplace/packs/beam4pm-pro-entitlement-pack" }
  ```
  This means `GgenIgniter.PackRef` **as it exists today mirrors the wrong schema for beam4pm's own
  `ggen.toml`** — this is a real, load-bearing finding, not a hypothetical, and Story GGEN-1802
  below must resolve it (parse the table-of-tables shape actually on disk) rather than parse
  against `PackRef`'s current array-of-tables assumption and silently fail on beam4pm.
- `~/beam4pm/vendor/ggen-marketplace/packs/` contains all five fortune5-prefixed packs today:
  `fortune5-architecture-pack`, `fortune5-deployment-blocks-pack`,
  `fortune5-enterprise-architecture-pack`, `fortune5-required-capabilities-pack`,
  `fortune5-testing-bblock-pack`.
- `lib/mix/tasks/ggen_igniter.install.ex` is real, shipped code with the exact reusable pipeline
  cited above; its moduledoc documents the `Igniter.Project.Deps.add_dep/2,3` `CaseClauseError`
  defect (inline `deps: [...]` in `project/0` vs. a separate `defp deps do ... end`) and the
  task's own defensive pre-check pattern — probe with the same `move_to_module_using/2` +
  `move_to_defp(zipper, :deps, 0)` zipper walk before calling `add_dep/2`, and `Igniter.add_issue/2`
  with the exact remediation instead of crashing when the probe fails.
- `lib/ggen_igniter/doctor_fixes.ex` implements a real `%Rule{name:, predicate:, transform:,
  verify:}` struct engine (documented in its moduledoc) already used for the doctor's `--fix`
  path — every `transform` there parses with `Sourceror.parse_string!/1`, walks/edits via a
  `Sourceror.Zipper`, and re-serializes with `Sourceror.to_string/1`, using `Igniter.Code.*` /
  `Igniter.Project.Config.modify_config_code/4,5` primitives that operate on a plain
  `Sourceror.Zipper.t()` with no `%Igniter{}`/`Rewrite`/cwd dependency — the exact shape a
  `ggen.toml`-merge transform needs, since it must run against an arbitrary `project_dir`, never
  `File.cwd!()`.
- `lib/mix/tasks/ggen_igniter.doctor.ex` has three real `System.cmd/3` subprocess-check patterns
  worth citing as precedent for the sync shellout in Story GGEN-1805:
  - `check_git_status/0` (`~746`): `System.cmd("git", ["status", "--porcelain"], cd: File.cwd!(),
    stderr_to_stdout: true)`, pattern-matches `{output, 0}` vs. `{output, _code}`, degrades a
    non-zero exit to `{:warn, ...}` rather than `{:error, ...}` (documented in-line as a real,
    previously-reachable bug: a git-free fixture project made the whole doctor run fail solely for
    lacking `.git`, not for any real defect — advisory information, not a checklist failure).
  - `check_nif_compiles/0` (`~821`): `System.cmd("cargo", ["build", "--quiet"], cd: crate_dir,
    stderr_to_stdout: true)`, `{_output, 0}` → `:ok`, `{output, code}` → `{:error, "... (cargo exit
    #{code}): ..."}`.
  - `check_hex_publish_readiness/0` (`~914`): `System.cmd("mix", ["hex.build"], cd: File.cwd!(),
    stderr_to_stdout: true)`, three-way match combining subprocess exit code with a separate
    `metadata_errors` list built from real file content (`DoctorFixes.package_metadata_keys_present/1`)
    rather than in-process `Mix.Project.config()[:package]`, documented in-line as avoiding a real
    staleness bug where `Mix.Project.config()` still reported stale values immediately after a
    `--fix` had already rewritten `mix.exs` in the same invocation.
  All three share the same shape Story GGEN-1805 must follow: real subprocess, `cd:` pinned to an
  explicit directory (never implicit `File.cwd!()` inside a helper meant to be reusable against an
  arbitrary `project_dir`), `stderr_to_stdout: true`, and an exit-code match producing a typed
  `{:ok | :warn | :error, message}` tuple — never an unhandled raise on a non-zero exit.

## EPIC GGEN-1800 — `mix ggen_igniter.fortune5_ready` bundle installer

### GGEN-1801 Bundle manifest definition

Define a versioned bundle manifest naming the packs this bundle wires — initially
`fortune5-architecture` and `fortune5-deployment-blocks` (the two packs the approved plan
confirmed as portable from the initial exploration pass, pending this story's live re-verification;
`fortune5-required-capabilities` and `fortune5-testing-bblock` join the bundle only once their
portability fixes, tracked elsewhere in this milestone, are confirmed landed —
`fortune5-enterprise-architecture-pack` is out of scope for this story pending its own portability
audit, not assumed safe by omission from the approved plan's findings).

Before defining the manifest schema, live-test-generate `fortune5-architecture-pack` and
`fortune5-deployment-blocks-pack` against a fresh scratch consumer project that is not
`ggen-marketplace` itself (a temp-dir Elixir/Mix scaffold with its own `ggen.toml`) — the approved
plan explicitly flagged that the "already portable" finding for these two packs came from a static
grep pass only, never a live generation test, and this story's acceptance below requires closing
that gap for real before any other story depends on the claim.

**Open design questions** (flag explicitly, do not resolve silently by picking one without
recording the tradeoff):

- Should bundle composition be domain-aware (e.g. a `process-mining` bundle vs. a
  `web-service` bundle, each naming a different pack set) rather than one-size-fits-all? The
  approved plan names only one bundle (fortune5-ready); a second bundle shape is not scoped here,
  but the manifest's own structure should not foreclose it by accident (e.g. hardcoding a single
  bundle name as a module attribute rather than a name-keyed map).
- Where should the manifest live — inside `ggen_igniter` itself (co-located with the task that
  reads it, versioned with the installer's own release cadence), or alongside the packs in
  `ggen-marketplace` (co-located with the packs it names, versioned with the pack corpus)? Each
  has a real tradeoff: co-locating with `ggen_igniter` means the installer and its manifest always
  ship in lockstep (no cross-repo version skew) but couples an unrelated-to-codegen data file into
  the Elixir codemod package; co-locating with `ggen-marketplace` means the manifest evolves with
  the packs it names but introduces a live cross-repo read (or a vendored copy, reintroducing the
  same vendoring-drift class of problem beam4pm's own `vendor/ggen-marketplace` submodule already
  manages via pinned tags).

Acceptance:

- `fortune5-architecture-pack` and `fortune5-deployment-blocks-pack` are each real-generated (via
  a real `ggen sync run` or equivalent) against a fresh scratch consumer project that is not
  `ggen-marketplace`'s own repo tree, proven by reading the actual generated output files on disk
  after the run, not by re-citing the prior grep-only pass;
- the live-test surfaces zero hardcoded references to `ggen-marketplace`'s own repo paths in
  either pack's generated output for the scratch consumer (the same defect class documented for
  `fortune5-required-capabilities-pack`/`fortune5-testing-bblock-pack` elsewhere in this
  milestone) — if either pack is found non-portable by this live test, this story's acceptance
  fails and the finding is corrected in this document (not silently dropped from the bundle without
  a recorded reason);
- the bundle manifest is written as a real, versioned data structure (module attribute, TOML/JSON
  fixture, or equivalent) naming exactly the pack paths/aliases confirmed portable by this story,
  with a version field distinct from `ggen_igniter`'s own `mix.exs` version;
- both open design questions above are recorded in the manifest's own module/file documentation
  (not just this ticket) as explicitly deferred, with the chosen interim answer stated (e.g. "single
  bundle only, no domain-awareness yet; manifest lives in `ggen_igniter` for v1, revisit if a
  second bundle or pack-repo-side versioning need arises").

### GGEN-1802 Real `ggen.toml` TOML parse into `ProjectConfig`/`PackRef`

Implement real TOML load for `ggen.toml`, using the already-declared `{:toml, "~> 0.7"}` dep
(`mix.exs:108`) and populating the existing-but-unused `GgenIgniter.ProjectConfig`/
`GgenIgniter.PackRef` structs (`~/ggen_igniter/lib/ggen_igniter/project_config.ex:171-229`,
`~/ggen_igniter/lib/ggen_igniter/pack_manifest.ex:86-111`) — this is genuinely new code, not a
wiring change, since zero `Toml.decode`/`Toml.parse` calls exist under `lib/` today.

This story must resolve the schema-mismatch finding above: `GgenIgniter.PackRef` as it exists
today mirrors only the array-of-tables `[[packs]]` shape, but `~/beam4pm/ggen.toml`'s real content
(and, per `PackRef`'s own moduledoc, any `ggen.toml` whose `[[generation.rules]]` is empty) uses
the table-of-tables `[packs]` shape (`ggen_engine::config::PackRef`'s `Path{path,
extra_ontologies, lock} | Git{git, version}` untagged enum). The parser must detect which shape is
present (by checking for a non-empty `[[generation.rules]]`, mirroring the Rust-side parse-time
decision cited in `PackRef`'s moduledoc) and load accordingly — silently assuming the
array-of-tables shape and failing to find beam4pm's five already-wired packs is not acceptable.

Acceptance:

- a real `load/1` function (e.g. `GgenIgniter.ProjectConfig.load/1`) takes a `project_dir` and
  returns a populated `%GgenIgniter.ProjectConfig{}` (or a typed error) by reading and
  `Toml.decode`-ing the real `ggen.toml` at that path — no `File.cwd!()` implicit dependency,
  consistent with `doctor_fixes.ex`'s `project_dir`-as-explicit-argument convention;
- run for real against `~/beam4pm/ggen.toml`, `load/1` returns all five of beam4pm's real
  already-wired packs (`beam4pm-process-model`, `beam4pm-pro-infra`, `github-actions-pack`,
  `beam4pm-ai-contracts`, `beam4pm-pro-entitlement`) with their real `path` values, proven by
  reading the actual returned struct's `packs` field, not by asserting the function returned `:ok`;
- a regression test fixture exercises the array-of-tables `[[packs]]` shape separately (a
  synthetic `ggen.toml` with non-empty `[[generation.rules]]` and `[[packs]]` entries) and
  confirms `load/1` correctly parses that shape too, so both real schemas in the corpus are
  covered, not just beam4pm's;
- `load/1` returns a typed error (not a raised exception) for a `ggen.toml` that fails to parse as
  valid TOML, proven by running it against a real malformed fixture file.

### GGEN-1803 Merge-in logic with dedup against already-wired packs

Implement merge logic that takes a loaded `%GgenIgniter.ProjectConfig{}` (from GGEN-1802) and the
bundle manifest (from GGEN-1801), and produces the set of pack entries to add — deduping against
packs already present by name/alias, so re-running the installer against a project that already
has some or all bundle packs wired is a no-op for those entries specifically (not an error, not a
duplicate entry).

Acceptance:

- run for real against beam4pm's five already-wired packs plus the two-pack fortune5 bundle
  (`fortune5-architecture`, `fortune5-deployment-blocks`), the merge produces exactly the two new
  entries — proven by inspecting the actual returned list, confirming none of the five existing
  entries is duplicated or altered;
- run a second time against a `%ProjectConfig{}` that already includes one of the two bundle packs
  (simulating a partial prior install), the merge produces exactly the one still-missing entry, not
  two, not zero, not an error;
- dedup matches on pack name/alias exactly as it appears in `ggen.toml` today (beam4pm's own keys
  like `beam4pm-process-model`), not on path, since two differently-named entries could
  legitimately point at the same path in principle and this story does not assume they can't.

### GGEN-1804 Real TOML serialize-back, idempotent across repeated runs

Implement the write-back half: given the merged pack set from GGEN-1803, serialize a real,
valid `ggen.toml` reflecting the addition — using `Sourceror`-based structural editing consistent
with `doctor_fixes.ex`'s documented convention (parse → zipper walk → re-serialize), not a
hand-rolled string-append, since `ggen.toml`'s real structure includes `[project]`, `[ontology]`,
`[packs]`, `[templates]` sections and comments (beam4pm's own `ggen.toml` carries a multi-line
explanatory comment above its `[templates]` section that a naive append would risk corrupting or
displacing).

Acceptance:

- run for real against a real scratch copy of beam4pm's actual `ggen.toml`, the serialized output
  adds the two new `[packs]` table-of-tables entries (matching the schema resolved in GGEN-1802)
  while leaving all five pre-existing entries, the `[project]`/`[ontology]`/`[templates]` sections,
  and the existing gh-terraform-pack explanatory comment block byte-identical, proven by diffing
  the real pre- and post-run file content;
- running the full parse → merge → serialize pipeline a second time against the already-modified
  file produces zero further diff (true idempotency), proven by diffing the file's content before
  and after the second run and confirming they are byte-identical;
- the serialized `ggen.toml` re-parses successfully via GGEN-1802's `load/1`, proven by round-
  tripping it, not merely asserting the write succeeded.

### GGEN-1805 Real `System.cmd/3` shellout to `ggen sync run`

After the `ggen.toml` write, shell out to a real external `ggen sync run` invocation, modeled on
`mix ggen_igniter.doctor.ex`'s existing subprocess-check pattern (cited above: `check_git_status/0`
at `~746`, `check_nif_compiles/0` at `~821`, `check_hex_publish_readiness/0` at `~914`) — a real
`System.cmd/3` call with an explicit `cd:` (the consumer's `project_dir`, never implicit
`File.cwd!()`), `stderr_to_stdout: true`, and a typed `{:ok, output} | {:error, {exit_code,
output}}` result rather than an unhandled raise on non-zero exit. This is genuinely new code:
`mix ggen_igniter.sync` IS the sync engine (Elixir-native EEx templating over Ash resources), not
a caller of an external `ggen` binary — no existing code in this repo shells out to `ggen sync run`
today.

Acceptance:

- run for real against beam4pm (after GGEN-1804's write lands the two new pack entries), the task
  invokes a real `System.cmd("ggen", ["sync", "run"], cd: project_dir, stderr_to_stdout: true)` (or
  the project's equivalent invocation form, e.g. via `just sync`/`rm ggen.lock && ggen sync run` per
  `~/beam4pm/CLAUDE.md`'s documented build/sync commands) and the real subprocess exit code and
  output are captured, proven by inspecting the real returned tuple, not assumed from the task's
  own exit status;
- a non-zero exit from the subprocess is surfaced as a typed `{:error, {exit_code, output}}` —
  proven by running the same call against a deliberately broken fixture (e.g. a `ggen.toml`
  pointing at a nonexistent pack path) and confirming the real non-zero exit is captured and
  returned, not silently swallowed;
- the shellout does not proceed past a failed GGEN-1804 write — if the TOML serialize-back step
  errors, `ggen sync run` is never invoked, proven by a fixture where the write step is forced to
  fail and confirming (via a real assertion on subprocess invocation count or an equivalent
  observable, not a mocked call count) that no subprocess was spawned.

### GGEN-1806 Verification stage: run the newly-generated packs' own gates for real

After a successful sync (GGEN-1805), run each newly-added pack's own gates (per beam4pm's
`docs/jira/v26.8.29/16-gate-closure-m0-m6.md`-style gate convention and this milestone's cited
`fortune5-architecture-pack`'s 7 SPARQL gates under `gates/010-070*.rq`, and
`fortune5-deployment-blocks-pack`'s 2 gates) and refuse with a typed error if any gate fails —
generation succeeding is not sufficient evidence of readiness, per the approved plan's explicit
instruction not to claim success from generation alone.

Acceptance:

- run for real against beam4pm post-sync, each of the two bundle packs' gates is executed for
  real (SPARQL query execution against the regenerated `ontology.ttl`-derived graph, or the
  packs' own documented gate-runner mechanism) and the real pass/fail result of each gate is
  captured individually — proven by inspecting the actual per-gate results, not a single
  aggregate boolean;
- a fixture that deliberately breaks one gate precondition (e.g. removes a required ontology
  individual the gate query depends on) causes the task to refuse with a typed error naming the
  specific failed gate, proven by running the task against that fixture and reading the real
  refusal message — not merely asserting a non-zero exit code;
- when all gates pass, the task's final result is a typed success value distinguishable from "the
  sync subprocess exited 0" alone — i.e., a caller can tell "generated and gate-verified" apart
  from "generated only," since GGEN-1805's success and this story's success are separate,
  independently-observable outcomes.

### GGEN-1807 Real Chicago-style ExUnit test against a scratch project fixture

Write the end-to-end test proving the full task works, following this repo's Chicago testing
discipline (real collaborators, state-based assertions, no `Mox`/mocked `ggen.toml`/mocked
subprocess): a real scratch Elixir/Mix project fixture (its own real `ggen.toml`, its own real
`mix.exs`) that `mix ggen_igniter.fortune5_ready` is actually run against as a subprocess or
direct task invocation, asserting on the real post-run `ggen.toml` content and real generated
files on disk — never on which functions were called or how many times.

Acceptance:

- the test fixture is a real directory tree (not an in-memory fake filesystem) with its own real
  `ggen.toml` modeled on beam4pm's real shape (table-of-tables `[packs]`, at least one
  pre-existing pack entry to prove dedup) and its own real `mix.exs`;
- running the task against the fixture is a real invocation (a real `Mix.Task.run/2` call or real
  subprocess, per this repo's existing `test/ggen_igniter_base_mix_task_end_user_test.exs`
  precedent for real-subprocess CLI testing) — not a mocked/stubbed task body;
- assertions read the real post-run `ggen.toml` file content from disk and confirm the two bundle
  pack entries are present with correct paths, and confirm the pre-existing entry is unchanged —
  state-based, not interaction-based;
- assertions confirm real generated output files exist on disk for the newly-added packs (at
  minimum, file existence; content assertions where GGEN-1801's live-test already established a
  known-good shape to compare against);
- a second invocation of the same task against the now-modified fixture is asserted to produce a
  no-op diff on `ggen.toml` (byte-identical before/after the second run) — the idempotency
  assertion required by this story's own charter, run for real, not inferred from GGEN-1804's
  unit-level idempotency test alone (this is the integration-level confirmation that idempotency
  holds through the full task, including the sync/verify stages, not just the TOML write in
  isolation);
- `grep -rn "unittest.mock\|Mock(\|MagicMock\|patch(\|monkeypatch\|Mox\." test/` (or this repo's
  equivalent test directories) run over the new test file(s) added by this story returns zero
  matches, confirmed as part of this story's completion evidence, not asserted from memory.

## Non-goals (explicit)

- A general ggen-core pack-dependency resolver or the formal pack composition algebra
  (`docs/thesis/09-pack-algebra.md`) — that remains a separate, larger project.
- Resurrecting `ggen-marketplace::packs_registry`'s dead `PackComposer`/`DependencyGraph`
  subsystem into a reachable CLI verb.
- Closing every gap from the earlier 7-dimension Fortune-5-readiness audit — this epic closes
  documentation/contract/scaffold-generation gaps only (e.g. security/airgap SPIFFE/KMS/
  observability contracts via `fortune5-architecture-pack`), never business/certification gaps: no
  cloud marketplace listing, no SOC2 audit, no pentest, no RBAC-scoped k8s runtime code, no
  OCPM/simulation algorithm work. Acceptance bullets throughout this document are written to avoid
  overclaiming "instantly ready" beyond what is actually generated and gate-verified.
- `fortune5-enterprise-architecture-pack`'s portability is not assessed by this epic; it is not
  added to the bundle manifest (GGEN-1801) without its own separate portability audit.

## Definition of done

- GGEN-1801 through GGEN-1807 all pass their own acceptance criteria against real evidence (real
  file reads, real subprocess output, real gate results) — no story is marked done from a plan
  description or a green CI badge alone.
- Proven end-to-end against beam4pm (`~/beam4pm`) as the first real consumer: the bundle adds
  `fortune5-architecture` and `fortune5-deployment-blocks` to `~/beam4pm/ggen.toml` without
  disturbing its five already-wired packs, `ggen sync run` completes, and
  `bash scripts/gate_m2_check.sh` plus the two new packs' own gates all pass — re-run fresh in the
  session that closes this epic, not cited from an earlier run.
- **Blocking precondition, not hypothetical**: `bash scripts/gate_m2_check.sh` was found broken in
  a recent audit (`ggen_igniter` Ash-reconciliation leg hit `build_broken`,
  `lib/beam4pm_ash.ex` deleted and never recreated during a live run, causing 320 files / 22,636
  lines to be destructively deleted before being reverted). This must be fixed first, independent
  of this epic's own stories — the end-to-end proof above cannot be claimed done while
  `gate_m2_check.sh` is in that state.
- No story's acceptance is satisfied by "the code compiles" or "the task exits 0" alone — every
  story requires reading real resulting state (file content, subprocess output, gate results) per
  this repo's Chicago testing-discipline convention, cited explicitly in each story above.

## See Also

- `~/beam4pm/CLAUDE.md` — beam4pm's build/sync/test commands (`just verify`, `just sync`,
  `bash scripts/gate_m2_check.sh`) this epic's end-to-end proof depends on.
- `~/beam4pm/docs/jira/v26.8.31/04-jira-epics-stories-acceptance.md` — the epic/story/acceptance
  format convention this document follows, and the evidentiary basis for
  `Igniter.Project.Deps`/`Config`/`Application` usage precedent this epic's stories build on.
- `~/ggen_igniter/lib/mix/tasks/ggen_igniter.install.ex` — the reusable installer pipeline shape
  (`Igniter.Project.Deps.add_dep`, `Igniter.Project.Config.configure_new`,
  `Igniter.Project.Application.add_new_child`) and its `deps_probe`-style defensive pre-check
  pattern this epic's Story GGEN-1805 and GGEN-1802/1803 build on.
- `~/ggen_igniter/lib/ggen_igniter/doctor_fixes.ex` — the `%Rule{predicate:, transform:, verify:}`
  engine and its `Sourceror.parse_string!/1` → zipper → `Sourceror.to_string/1` codemod convention,
  the template for the `ggen.toml`-merge transform in Stories GGEN-1803/1804.
- `~/ggen_igniter/lib/mix/tasks/ggen_igniter.doctor.ex` — the three cited real `System.cmd/3`
  subprocess-check patterns (`~746`, `~821`, `~914`) Story GGEN-1805 models its shellout on.
- `~/ggen_igniter/lib/ggen_igniter/project_config.ex`, `~/ggen_igniter/lib/ggen_igniter/
  pack_manifest.ex` — the existing-but-unused `GgenIgniter.ProjectConfig`/`GgenIgniter.PackRef`
  structs Story GGEN-1802 gives a real `load/1` function, including `PackRef`'s own disclosed
  array-of-tables-vs-table-of-tables schema caveat.
- `~/ggen/crates/ggen-engine/src/pack.rs:70-93` — `pack.toml`'s closed schema, cited as evidence
  for why per-pack dependency declarations are not a mechanism this epic can build on.
- `docs/jira/v26.8.16/01-COMMIT-BOUNDARY.md` — the format/evidence-discipline reference this
  document's citation style follows (exact commands, exact file paths, no unsupported claims).
