---
# 2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS: ggen Defects Found During Combinatorial Real-Execution Verification

**Discovered:** 2026-07-17
**Discovered by:** a combinatorial real-execution JTBD verification workflow (13 agents, one per
CLI noun, each running real subprocess invocations against a real compiled `ggen` binary — real
exit codes, real stdout/stderr, real filesystem checks, no mocks) — run after PR #255 (ggen-core
retirement) and alongside PR #257 (test-suite cleanup, added `ggen-cheat-scanner`).
**ggen version under analysis:** 26.7.4 (post PR #255/#256/#257, source-verified against
`crates/ggen-engine/src/verbs/handlers.rs`, `crates/ggen-cli/src/cmds/{init,agent}.rs`, capability
registry, policy verbs)
**Severity legend:** HIGH (core JTBD non-functional) | FOOTGUN (silently wrong, no error) |
UX-GAP (works but confusing/inconsistent) | TECH-DEBT (test-quality backlog, not a product bug)
**Filed here instead of GitHub Issues** because Issues are disabled on `seanchatmangpt/ggen`
(confirmed via `gh issue create` → `the 'seanchatmangpt/ggen' repository has disabled issues`).
Follows the same doc-based tracking convention as `WASM4PM-DISCOVERED-BUGS.md` in this directory.
**Machine-readable standing** for these bugs' parent claims (per-CLI-noun `ALIVE`/`PARTIAL`/
`BLOCKED`/`UNVERIFIED` with falsifiers and evidence coordinates) lives in
`docs/aps/claims.toml` — keep the two in sync.

---

## BUG-001 — `ggen policy check`/`ggen policy validate` unconditionally fail (HIGH)

**Source evidence:** reproduced live, root call site not yet localized (grep for the literal
composed string found nothing; likely assembled at runtime by `AtomicPackId`/capability-registry
composing `surface-` + a capability name containing a dot, given `compliance-soc2` appears in
`capability_registry.rs`).
**Manifestation:** `policy list` and `policy show` work correctly (happy path, missing-arg,
invalid-id, idempotency, format handling all match expectations). `policy check` and
`policy validate` — the noun's actual compliance-enforcement JTBD — fail on every invocation,
regardless of profile (`development`, `enterprise-strict`, a bogus profile name) or environment
(real machine `HOME`, or a from-scratch `mktemp` `HOME` with zero real pack cache):
```
ERROR: CLI execution failed: Argument parsing failed: Invalid package ID surface-compliance.soc2: Invalid package ID format: Package ID can only contain alphanumeric, hyphens, and underscores
```
Reproduces byte-for-byte across repeated runs and a fully isolated `HOME`, ruling out local
pack-cache pollution.
**Impact:** neither `check` nor `validate` ever reach a pass/fail compliance verdict — they crash
on an internal, self-generated package ID before compliance logic runs at all.
**Fix:** find where `surface-<capability-name>` package IDs are composed (capability registry /
`AtomicPackId` construction path) and either sanitize dots out of capability names before
composing the ID, or relax the package-ID format validator to accept the composed shape.
**Repro:**
```bash
ggen policy check                                # no --profile flag exists on `check`; fails identically regardless
ggen policy validate --profile development       # fails identically for every --profile value
```
(Corrected 2026-07-18: `ggen policy check --profile ...` as originally written here is not
runnable — `check` has no `--profile` flag at all, so clap rejects it before ever reaching the
bug. `docs/aps/claims.toml`'s `cli.policy` falsifier had the same error and was fixed alongside
this.)

---

## BUG-002 — `ggen init --force <malformed-bool>` silently coerces to `false` instead of rejecting (FOOTGUN)

**Source evidence:** reproduced live against `crates/ggen-cli/src/cmds/init.rs`'s `--force` flag
parsing.
**Manifestation:** `ggen init --force garbage` (or any non-`true`/`false` string) is accepted
without a clap type error. On a fresh directory it proceeds as if `--force` were absent/false.
Disambiguated on an already-initialized directory: `--force garbage` returns
`status:error` ("already initialized... use --force to reinitialize") — proving `garbage` was
silently coerced to `false`, not rejected as invalid.
**Impact:** a typo'd `--force` value (e.g. `--force ture`) silently does the opposite of what a
caller likely intended, with no warning.
**Fix:** parse `--force`/`--skip-hooks` as a real `bool` type at the clap level so malformed
values are a parse error, not a silent `false`.
**Repro:**
```bash
D=$(mktemp -d); ggen init --path "$D"
echo MARKER > "$D/README.md"
ggen init --path "$D" --force garbage   # expect: clap error; actual: silently treated as false
```

---

## BUG-003 — `ggen init` exits 0 even when its own JSON body reports `status:error` (FOOTGUN)

**Source evidence:** reproduced live against `crates/ggen-cli/src/cmds/init.rs`.
**Manifestation:** re-running `ggen init` against an already-initialized directory (without
`--force`) correctly refuses to clobber and reports
`{"status":"error","error":"ggen project already initialized here. Use --force to reinitialize."}`
in its JSON output — but the process exit code is still `0`.
**Impact:** any script/CI checking only the exit code (the normal Unix convention) silently
treats this refusal as success.
**Fix:** exit nonzero whenever the JSON body's `status` field is `"error"`.
**Repro:**
```bash
D=$(mktemp -d); ggen init --path "$D"; echo "first exit: $?"
ggen init --path "$D"; echo "second exit: $?"   # expect nonzero; actual: 0
```

---

## BUG-004 — `ggen agent install`'s "already installed" check reads a fixed global path, not the project-local target (FOOTGUN)

**Source evidence:** reproduced live via a full real `agent` lifecycle re-run
(capabilities→search→show→resolve→compatibility→install→status→verify→remove) in a fresh
`mktemp` tmpdir.
**Manifestation:** `ggen agent install <pack-id>` checks for prior installation against a fixed
global path (`~/.ggen/packs/<pack-id>`, the invoking user's real home directory) rather than the
project-local `.ggen/packs.lock`. In a brand-new, empty tmpdir with a completely empty
`.ggen/packs.lock`, `ggen agent install framework-lsp` fails with "Pack already installed at
/Users/<user>/.ggen/packs/framework-lsp" purely because that pack happens to already exist
globally on the machine — even though the tmpdir's own lockfile has zero entries.
**Related, unresolved side-observation:** `--dry-run <anything-not-"false">` (e.g. `notabool`) is
accepted without clap type validation, and dry-run behavior itself could not be independently
verified in this pass because it was confounded by this same global-path bug — needs re-testing
once BUG-004 is fixed.
**Impact:** a caller working in an isolated/CI environment can get a false "already installed"
refusal purely due to unrelated global machine state.
**Fix:** scope the "already installed" check to the project-local `.ggen/packs.lock`, not a
global filesystem path.

---

## BUG-005 — `ggen doctor run` unconditionally uses one `ggen.toml` schema parser, hard-failing on the other (HIGH)

**Source evidence:** `crates/ggen-engine/src/verbs/handlers.rs:630` (`handle_doctor` calls
`GgenConfig::load` unconditionally) vs. `sync()`'s Stage-0 dispatch, which picks between the two
incompatible `ggen.toml` schemas via `has_generation_rules` (`crates/ggen-engine/src/generation_rules.rs`).
**Manifestation:** `ggen.toml` has two incompatible schemas in this codebase (declarative-rules
`GgenManifest` vs. frontmatter `GgenConfig`). `handle_doctor` does not replicate `sync()`'s schema
dispatch, so `ggen doctor run` hard-fails with `FM-CONFIG-002` on any project using the
declarative-rules/`GgenManifest` schema — **including this repo's own root `ggen.toml`** and
`examples/simple-project`/`playground/ggen.toml` — even immediately after a real `ggen sync run`
against that exact file succeeded.
**Impact:** `doctor` is unusable on the majority of this repo's own `ggen.toml` files (30 of 38
examples use the RULES schema per this session's earlier audit).
**Fix:** have `handle_doctor` call the same `has_generation_rules`-based schema dispatch `sync()`
uses before choosing which parser to load the project config with.
**Note:** all three doctor checks (lockfile_drift, orphaned_artifacts, receipt_staleness) work
correctly for the frontmatter-schema code path when exercised directly (real sabotage scenarios
all correctly flip from pass to a specifically-worded fail) — this is a schema-scoping bug, not a
defect in the checks themselves.

---

## GAP-001 — `ggen capability`'s unknown-surface/wrong-context handling (UX-GAP)

**Source evidence:** reproduced live against `capability list`/`inspect`/`enable`.
**Manifestation:** three real, run-verified gaps: (1) `inspect`/`enable` on an unknown surface
name silently succeed with `atomic_packs: []` instead of erroring — a caller can't distinguish
"valid surface with zero packs" from "typo'd surface name"; (2) `enable` never checks it's inside
a real ggen project — run in a brand-new empty tmpdir with no `ggen.toml`, it still exits 0 and
creates a fresh `.ggen/packs.lock` from nothing; (3) `--projection foo123` (an arbitrary made-up
string) is accepted with no validation and echoed straight into a synthesized pack id, suggesting
no real projection registry backs this flag.
**Impact:** lower severity than BUG-001..005 — the happy path genuinely works (list/inspect/enable
all produce real, non-fabricated JSON and real lockfile side effects), but silent-empty-on-typo
and no wrong-context refusal are footguns waiting to surface as confusing support requests.

---

## TECH-DEBT-001 — 464 pre-existing test-quality violations found by `ggen-cheat-scanner` (TECH-DEBT)

**Source evidence:** `cargo run -p ggen-cheat-scanner` (added in #257) run against `main`
post-#257 merge. **Reconfirmed 2026-07-18** (release hardening pass) at exactly this count,
after `ggen-core`'s deletion and after this same pass's removal of 4 orphaned files under
`crates/ggen-cli/src/cmds/`.
**Breakdown (reconfirmed 2026-07-18):**
- **456 `CHEAT-T03` (no-assertion-test)** — `#[test]` functions with zero
  `assert*!`/`.unwrap()`/`.expect()`/panic-triggering calls in their body (can never fail no
  matter what the code under test does). Spread across `chicago-tdd-tools`, `ggen-cli/tests/*`,
  `bcinr-mfw-ir`, `bcinr-pddl`.
- **7 `CHEAT-T01` (vacuous-assert)** — `assert!(true)`-only test bodies, in `chicago-tdd-tools`'s
  observability feature-gate tests.
- **1 `CHEAT-T04` (mock-import)** — a `FakeDataGenerator`-style mock-like `Default` impl at
  `chicago-tdd-tools/src/core/builders.rs:883`.

**Note on the count change (515 → 464):** this section originally reported 515 findings,
measured against `main` post-#257. `ggen-core` was fully deleted from the workspace in PR #259
(2026-07-17) — some of that original 515's `CHEAT-T03` findings lived under the
now-nonexistent `ggen-core/src/*`, and were retired along with the crate, not fixed by triage.
464 is the real current count, not a partial fix.

**Impact:** none of these are shipped-product bugs — they're test-suite debt giving false
confidence that untested paths are covered. Not fixed in #257; needs a dedicated triage pass
(not all 456 `CHEAT-T03` hits are necessarily equally severe — some may be legitimate
compile-only/type-shape tests).
**Reproduce:** `cargo run -p ggen-cheat-scanner` from the repo root (exits nonzero with a full
`file:line` violation list when any are found).

**RETIRED 2026-07-20 (feat/cheat-scan-debt-retirement): 464 → 0. `guard-cheat-scan` is green
(`ALIVE: no cheat patterns detected across 1152 scanned file(s)`).** The dedicated triage pass
this entry called for was run to completion. The 464 split into two honest halves:

1. **Scanner false positives (~305)** — the triage prediction above ("not all hits are equally
   severe") was right: the T03 detector could not see `#[should_panic]` tests, Result-returning
   tests using `?`, or `assert_*`-prefixed helper fns/macros (`assert_killed_at`,
   `assert_eq_msg!`), and T04 flagged `FakeDataGenerator: Default` where the only shared trait
   was a ubiquitous std trait. Fixed as scanner *precision* improvements (5 new clean fixtures +
   tests in `crates/ggen-cheat-scanner/tests/`; positive fixtures still flag), not rule
   weakening.
2. **Genuine debt (~159)** — fixed for real or deleted per the London-TDD migration policy:
   real observable-state assertions added across `bcinr-mfw-ir`, `bcinr-pddl`, `ggen-cli`,
   `ggen-marketplace`, `chicago-tdd-tools`, `praxis-graphlaw`, and root `tests/`; deletions
   include 50 all-`Ok(())` sham tests (`tests/marketplace_integration_tests.rs`,
   `tests/security_validation_tests.rs`), the in-file-mock suite
   `tests/a2a_rig_mcp_integration.rs` (+ its `[[test]]` entry), the dead-by-construction
   `ggen-core-retired`-gated `tests/{tracing,graph_core_tests}.rs`, the unwired
   `ggen-config/src/config/qa_integration_test.rs`, and assorted print-only/stringify!-only
   tests. Notable strengthening: `praxis-graphlaw`'s 8 `zz_ocel_evidence_sealed` tests now
   assert the sealed OCEL/receipt files exist (previously fail-open via a never-panicking
   `Drop`).

**Remaining count: 0.** Known pre-existing, *unrelated* failure surfaced during verification:
`praxis-graphlaw`'s `chatman_acceptance_agents` suite fails on `main` because
`tests/chatman_engine_acceptance/fixtures/agents/*.json` were never committed.

---

## TECH-DEBT-002 — 649 pre-existing clippy violations found by widening `just lint` to `--workspace` (TECH-DEBT)

**Discovered:** 2026-08-02, while widening `justfile`'s `lint:` recipe from root-package-only
(the scope gap this same doc's `justfile` history already flagged on 2026-07-17 — see that
recipe's own comment) to the whole workspace, per this repo's own standing warning
(`CLAUDE.md`: "Real, untriaged debt exists in other crates once workspace scope is added").

**Source evidence:** `cargo clippy --workspace --all-targets --keep-going -- -D warnings -A
unexpected_cfgs`, run in an isolated `CARGO_TARGET_DIR` (to avoid corrupting/being corrupted by
other concurrent sessions' builds against the shared `target/`, a real, observed hazard this run
hit once against the default `target/` — see the `-A unexpected_cfgs` note below for why that
flag is present; it is a single-lint carve-out, not a broad suppression, and does not affect this
count). `--keep-going` is required to get a real total at all: without it, cargo's default
fail-fast means the run aborts at the first broken crate (`ggen-graph`) and never reaches the
other four, silently hiding most of the count — the same masking failure mode this widening
exists to close, just relocated.

**Real count: 649**, across exactly 5 of the 19 real workspace members (14 compile clean):

| Crate | Errors | Breakdown |
|-------|--------|-----------|
| `ggen-engine` | 547 | 127 (lib) + 420 (lib test) |
| `ggen-graph` | 50 | 25 (lib) + 25 (lib test) |
| `praxis-graphlaw` | 21 | spread across `lib test` (7) + 6 integration test files + 1 bench (`owlrl`) |
| `ggen-config` | 18 | all 18 are `clippy::panic` in test files, spread across 4 integration test targets + `lib test` |
| `ggen-marketplace` | 13 | spread across 2 integration test targets (`fortune5_required_capabilities` 12, `part_passport_fixture` 1) |

**Two genuinely different sources make up the 649, not one undifferentiated pile:**

1. **~323 are this repo's own already-declared, already-phased Poka-Yoke backlog surfacing for
   the first time under `-D warnings`.** `Cargo.toml`'s `[workspace.lints.clippy]` (lines
   304-320) sets `unwrap_used`/`expect_used`/`panic`/`todo`/`unimplemented` to `"warn"` under an
   explicit, dated comment: *"**Warn-first mode** (Phase B.1): Inventory violations before
   enforcing... will be flipped to 'deny' after fixes are applied (Phase B.2)."* `ggen-engine`
   additionally declares the same three lints at `"warn"` directly in its own
   `Cargo.toml` (`[lints.clippy]`, not workspace-inherited). Command-line `-D warnings` does not
   know about "Phase B.1 vs B.2" — it promotes every warn-level lint to a hard error uniformly,
   which forces this crate's own declared Phase B.2 onto code that was deliberately left at
   Phase B.1. Breakdown of this half (global counts, from the same run): 245 `expect()` on
   `Result`, 23 `expect_err()` on `Result`, 18 `panic` in production code (all in `ggen-config`'s
   test files), 15 `unwrap()` on `Result`, 14 `expect()` on `Option`, 4 `unwrap()` on `Option`, 4
   `unwrap_err()` on `Result` = 323. This is real backlog, not noise — but it is backlog the
   workspace's own lint config already scheduled for a later, deliberate phase, not a surprise.
2. **~326 are ordinary, previously-unchecked clippy debt** — e.g. `clippy::result_large_err` (23
   occurrences, all in `ggen-graph/src/rwr/automatic.rs`'s `OperationsError` enum, one 248-byte
   variant flagged on every function returning that error type — a single root cause, many call
   sites), `too_many_arguments` (1), missing docs / `# Errors` sections (21 + 10), unused imports
   (2+), `len_zero`/length-comparison idioms (5), and other one-off style findings. This half has
   no single root cause and needs real per-crate triage, not a mechanical batch fix.

**Separately, one hard compile blocker (not counted above, not ordinary lint debt):**
`crates/bcinr-pddl/src/mfw/mod.rs:43`'s `#[cfg(feature = "mfw-planner")]` references a Cargo
feature that `crates/bcinr-pddl/Cargo.toml`'s own comment (lines 43-49) says was deliberately
removed from `[features]` in PR #255 — the cfg-gate attribute was left dangling. Under plain `-D
warnings` (no carve-out), this alone hard-fails the *entire* `--workspace` clippy invocation
before any other crate is even reached (confirmed live: `cargo clippy -p bcinr-pddl -- -D
warnings` fails standalone with `error: unexpected 'cfg' condition value: 'mfw-planner'`). The
widened `lint:` recipe carries a narrow `-A unexpected_cfgs` carve-out specifically to avoid this
one pre-existing, unrelated bug silently reverting the whole widening back to "checks almost
nothing" — see the recipe's own comment in `justfile`. Real fix (not done here, out of this
task's file scope): declare `mfw-planner = []` in that Cargo.toml's `[features]`, matching the
crate's existing `dhat-heap = []` pattern, then remove the carve-out from `lint:`.

**Disposition:** 649 is well past the "small, mechanically-fixable-in-one-pass" bar. Following
this doc's own TECH-DEBT-001/`guard-cheat-scan` precedent, `lint:` stays wired into the blocking
`pre-commit` chain at its existing position (not silently narrowed back to root-only, not quietly
dropped from the chain) — `just pre-commit` is red on this gate starting 2026-08-02 until a
dedicated triage pass (mirroring TECH-DEBT-001's, split along the two-source breakdown above)
closes it. Machine-readable standing: `docs/aps/claims.toml`'s `dev.lint-workspace` entry.

**Reproduce:** `cargo clippy --workspace --all-targets --keep-going -- -D warnings -A
unexpected_cfgs` from the repo root (or `just lint`).

**UPDATE (2026-08-03) — 649 → 4, real progress, not resolved.** A parallel fix pass (tasks
covering `ggen-engine`, `ggen-graph`, and related crates) closed the great majority of the
original 649. Independently reverified in this pass via a fresh, isolated `just lint` run
(exit code 101, same command as the falsifier above):

- `ggen-engine` (was 547): **0 findings.** Genuinely clean.
- `ggen-graph` (was 50): **0 findings.** Genuinely clean.
- `praxis-graphlaw` (was 21): **0 findings.** Genuinely clean.
- `ggen-config` (was 18): **0 findings.** Genuinely clean.
- `ggen-marketplace` (was 13): **0 findings.** Genuinely clean.
- `chicago-tdd-tools` (not in the original 649): **1 new warning**,
  `clippy::redundant_closure` at `crates/chicago-tdd-tools/src/cli_proof/receipt.rs:130`
  (`.any(|s| predicate(s))` → `.any(predicate)`). Non-gating: clippy's own diagnostic notes
  "the `clippy::redundant_closure` lint ignores `-D warnings`", confirmed live in the
  reverification run. Does not by itself fail the build.
- `ggen-cli` / `ggen-cli-lib` (not in the original 649): **2 new compile-gating errors**, both
  in `crates/ggen-cli/src/generated_commands.rs` — `clippy::too_long_first_doc_paragraph`
  (line 1) and `clippy::single_element_loop` (line 99), both promoted to hard `error`s by that
  crate's own `#![deny(warnings)]` (`crates/ggen-cli/src/lib.rs:51`). Root cause (verified, not
  assumed): before this pass's fixes, `ggen-engine` had 127 lib-level clippy errors that stopped
  `cargo clippy --workspace --keep-going` from ever reaching its dependent `ggen-cli-lib` in the
  same run — the identical "each fix unlocks a previously-unreached batch" dynamic this doc's own
  TECH-DEBT-001 precedent describes, one dependency-hop further out. `git log` on
  `generated_commands.rs` shows it last touched by an unrelated `cargo fmt --all` commit
  (`657cc904a`) and `git status` shows it untouched by any of this pass's fixes — this is a
  genuine pre-existing defect the widened `--keep-going` run had never reached before, not a
  regression introduced by this pass.

**Real total: 4** (1 non-gating warning + 2 gating errors, spread across the 3 diagnostic
emissions clippy reports for `ggen-cli-lib`'s lib + lib-test compile passes), down from 649, but
**not zero** — `just lint` still exits 101 (real, reproduced independently twice in this
session), so `just pre-commit` is **still red** on this gate. Standing is not changed to
resolved; see the matching `docs/aps/claims.toml` `dev.lint-workspace` update. Task #28 (this
session's task-tracker) remains open, scoped now to exactly these 2 gating findings plus the 1
deferred warning, not the original 649.

**UPDATE (2026-08-03, same day) — 4 → 0. RESOLVED.** The 2 gating errors and 1 deferred warning
above were fixed at their real source, not by suppression:

- `clippy::too_long_first_doc_paragraph` / `clippy::single_element_loop` in
  `crates/ggen-cli/src/generated_commands.rs`: this file is generated (`DO NOT EDIT` header,
  `ggen.toml`'s `cli-commands-reference` rule, `mode = "Overwrite"`), so the fix went into
  `.specify/templates/cli/commands-reference.rs.tera` — a blank `//!` line splitting the
  over-long first doc paragraph, and the single-element `for noun in ["unknown"]` loop replaced
  with a direct binding — then proved via a real `ggen sync run` regeneration (not a hand-edit):
  `git diff --stat crates/ggen-cli/src/generated_commands.rs` shows exactly the 2 targeted lines
  changed, nothing else.
- `clippy::redundant_closure` in `crates/chicago-tdd-tools/src/cli_proof/receipt.rs:130`:
  `.any(|s| predicate(s))` → `.any(predicate)` (verified `predicate: impl Fn(&str) -> bool`
  matches `Iterator::any`'s expected signature directly, so the closure was genuinely redundant).

**Reverified for real, fresh run:** `cargo clippy --workspace --all-targets --keep-going -- -D
warnings -A unexpected_cfgs` → exit 0, grep-confirmed 0 `error`/`clippy::` lines in the full log
(not just a truncated tail). `cargo test --workspace --lib` → 2256 passed, 0 failed across all 20
lib-test binaries (no regression from these 3 fixes). `just lint`'s `-A unexpected_cfgs`
carve-out for `bcinr-pddl`'s dangling `mfw-planner` cfg (see above) is still in place and still
out of this task's scope — it does not affect the 0-finding count, since that cfg warning is
allowed, not counted. Standing: **RESOLVED**. See `docs/aps/claims.toml`'s `dev.lint-workspace`
entry for the matching machine-readable update.

---

## TECH-DEBT-003 — real integration-test failures + one unbounded-subprocess hang found by adding `just test-integration` (TECH-DEBT / FOOTGUN)

**Discovered:** 2026-08-02, same session as TECH-DEBT-002, while adding a `test-integration:`
justfile recipe to close a real gap: before this recipe existed, `pre-commit`'s only test gate
was `test-lib` (`cargo test --lib --workspace`), which never executes a single file under any
`crates/*/tests/`. The pre-existing `test:` recipe (`cargo test --workspace --tests`) is *not* an
integration-only substitute despite its `--tests` flag's name — confirmed live: `--tests` also
builds and runs each crate's `[lib]` unit tests (e.g. it reports "Running unittests src/lib.rs"
for `ggen-engine` and surfaces a real `ggen-engine` lib-test failure), and `test:` itself is not
part of `pre-commit` either way.

**Source evidence:** `cargo test --workspace --tests --no-fail-fast`, isolated `CARGO_TARGET_DIR`,
run for real. 373 real integration-test targets exist workspace-wide (`cargo metadata`, `kind ==
["test"]`, deduplicated by name). **The run was killed partway through (134 of 373 target-runs
completed) after stalling on a genuine hang** — see below — so the totals here are real but
partial, not a completed full-workspace count.

**Partial real totals (134 of 373 targets, before the hang):** 1737 passed, 8 failed, 216 ignored.

**7 genuine integration-test failures** (the 8th, `ggen-engine`'s
`schema_dispatch::tests::declarative_rules_document_parses_as_declarative_rules`, is a `[lib]`
test — out of `test-integration`'s scope, already covered by the existing `test-lib` gate, and a
separate pre-existing `test-lib` red not discovered by this pass):

| Crate | Target | Test | 
|-------|--------|------|
| `ggen-cli-lib` | `tests/performance.rs` | `perf_cold_start_with_config` |
| `ggen-config` | `tests/system_crate_map_parity_test.rs` | `repo_facts_ttl_crate_map_matches_cargo_toml_workspace_members` — a real, current `Cargo.toml` `[workspace] members` vs `.specify/repo-facts.ttl` drift (the same class of drift CLAUDE.md's Crate Map section says was closed 2026-07-31 for `openapi-cnv-reflect`; something has drifted again since) |
| `ggen-engine` | `tests/ci_g0_inventory_e2e.rs` | `exact_repository_inventory_manufactures_partial_alive_evidence` |
| `ggen-engine` | `tests/cli_boundary.rs` | `root_help_gives_each_noun_a_non_blank_description` |
| `ggen-engine` | `tests/config_schema_dispatch_e2e.rs` | `doctor_succeeds_with_correct_diagnostic_on_each_supported_schema` |
| `ggen-engine` | `tests/cross_pack_matrix.rs` | `mega_project_all_packs_sync` |
| `ggen-engine` | `tests/custom_behavior_e2e.rs` | `custom_behavior_scaffolds_once_and_survives_hand_completion` |

Root causes not individually diagnosed here (out of this pass's scope — this is an inventory
pass, not a fix pass, same posture TECH-DEBT-001 took on first discovery). 5 of 7 are in
`ggen-engine`, the same crate that dominates TECH-DEBT-002's clippy count.

**One genuine hang (FOOTGUN, not counted in the 8 above — the run was terminated before either of
its two tests finished):** `crates/ggen-engine/tests/economics_measured_evidence_test.rs` spawns
a nested `cargo test -p ggen-engine --test receipt_chain_e2e` subprocess via
`std::process::Command::output()` with **no timeout on the wait**. Both of its `#[test]` fns
(`economics_receipt_chain_wall_clock_measured_under_slo_threshold`,
`economics_measurement_rejects_a_fabricated_zero_duration_reading`) share that one blocking call
via a `OnceLock`. Observed live: the process sat at 0% CPU for minutes with no sign of returning.
Whether this is a true deadlock or just severe contention under concurrent-session load was not
determined (out of scope to debug further here) — either way, a test whose termination depends on
an *unbounded* nested subprocess wait is a real liveness risk, structurally similar to (but not
the same pattern as) the sub-second-timeout flakiness `guard-short-test-timeout` already guards
against. Not caught by that guard because the risk here is the opposite shape: no timeout at all,
not too short a one.

**Disposition:** real failures plus a genuine hang mean `test-integration` is **not** wired into
`pre-commit`'s blocking chain — a gate that can hang forever is strictly worse than one that fails
fast and red, so this is the "documented, explicitly time-boxed exception" branch (not
`lint`/`guard-cheat-scan`'s "stays wired in and red" branch, per TECH-DEBT-002 above). The recipe
itself is real and runnable (not silently absent, not decorative) with a hard outer `timeout` so
it cannot hang `just test-integration` itself forever, even though it cannot reach into and kill
an already-orphaned grandchild subprocess of the hanging test. Machine-readable standing:
`docs/aps/claims.toml`'s `dev.test-integration` entry. Real fixes needed before this can be
promoted into `pre-commit` (not done here, out of this pass's scope): triage the 7 failures, and
add a timeout to `economics_measured_evidence_test.rs`'s subprocess wait (e.g.
`wait_timeout`/manual polling, not a bare `.output()`).

**Reproduce:** `just test-integration` (or `cargo test --workspace --no-fail-fast --test <name>`
per target, enumerated via `cargo metadata --no-deps --format-version=1`, `kind == ["test"]`).

**UPDATE (2026-08-03) — the 7 + hang are fixed; the recipe itself had a separate bug; a larger
residual surfaced; still not promoted to `pre-commit`.**

*Recipe bug found and fixed in this pass (was never covered by the original disposition above):*
`just test-integration`, as committed, hard-errored immediately at cargo's target-*selection*
step — before compiling a single file — for any `--test <name>` naming a target with
`required-features` not currently enabled (e.g. `error: target 'doctor_adversarial_tests' in
package 'ggen-cli-lib' requires the features: 'integration'`). Reproduced live, 2026-08-03, on
the plain pre-fix recipe. This means `just test-integration` had never actually run to completion
even once via the `just` entry point — every real number in this doc's original TECH-DEBT-003
text above came from an ad hoc `cargo test --features integration,a2a,mcp ...` invocation, not
from the committed recipe, and the recipe itself was never updated to match. Fixed in `justfile`
(this pass): added `--features integration,a2a,mcp` to the recipe's `cargo test` invocation and
excluded `test_telco_routing` from the enumerated target list (it is permanently unrunnable by
design, per its own file header — the prior 373-target count included it; the corrected count is
372).

*Original 7 + hang: fixed and reconfirmed.* Independently re-run this same day (fresh binaries,
`--features integration,a2a,mcp`): `ci_g0_inventory_e2e`, `cli_boundary`,
`config_schema_dispatch_e2e`, `cross_pack_matrix`, `custom_behavior_e2e`, `performance`
(`perf_cold_start_with_config`), and `system_crate_map_parity_test` all pass cleanly. The
`economics_measured_evidence_test.rs` hang did not reproduce across multiple full-workspace runs
today.

*Real, larger residual surfaced by the first-ever full completion (2026-08-03):* once the recipe
bug above was fixed and a genuine full 372-target run completed for the first time, it surfaced
**2070 passed, 90 failed, 256 ignored, 15/372 targets with at least one failure** — a strictly
larger, previously-never-exercised surface than the original 7 (those 7 were found from a
*partial*, killed-by-the-hang run; the hang was masking these 15 the same way `ggen-engine`'s own
127 clippy errors masked `ggen-cli-lib`'s 2 in TECH-DEBT-002 above). Independently
re-spot-checked in this pass (13 of the 15 relevant targets re-run fresh, exact same root causes
and failure counts reproduced):

1. **11 legacy dead-CLI-surface targets** (`ggen --test {ci_validate, cli, cli_command_tests,
   e2e_github_integration, ontology_workflows_e2e, ontology_workflows_hipaa,
   ontology_workflows_multi_cloud}`, `ggen-cli-lib --test {doctor_adversarial_tests, e2e,
   e2e_pack_workflow_test, sabotage_tests}`) — reference subcommands/args removed by the
   v26.7.16 CLI routing flip. 2 of the 11 spot-re-run live: `cli_command_tests` and
   `doctor_adversarial_tests` both fail with `error: unrecognized subcommand`; independently
   confirmed via `ggen --help` / `ggen doctor --help` on a freshly built binary that the current
   noun surface genuinely has no `market`/`ci`/`ontology`/`audit`/`config`/`security` etc.
   subcommands these tests still assume. Needs a real rewrite-or-archive pass, not a quick fix.
2. **2 more `strict_mode`-default-flip ORDER BY fixtures** (`ggen-engine --test
   generation_rules_e2e`, 14 failing fns; `generation_rules_typed_causes_e2e`, 1 failing fn) —
   re-run live, identical counts reproduced (5 passed/14 failed; 7 passed/1 failed). Same
   mechanical fix pattern as task #30's original fixture: add `ORDER BY` to each fixture's inline
   SPARQL.
3. **1 diagnostic-code-catalog drift** (`ggen-engine --test product_mirror_conformance`'s
   `fm_diagnostic_codes_ttl_matches_real_source`) — re-run live, reproduced (1 failing fn).
   Root cause not yet investigated.
4. **1 plugin-manifest version drift** (`ggen-lsp --test manifest_contract_test`'s
   `manifest_declares_the_ggen_lsp_server`) — re-run live, reproduced exactly:
   `.claude-plugin/marketplace.json` still declares `"26.7.4"`, the crate is now `v26.8.4`.
   One-line fix (bump the JSON), not yet applied.

**Disposition (updated):** the recipe is now genuinely runnable end-to-end (the target-selection
bug is fixed), and the originally-named 7 + hang are closed. But the full suite does **not**
pass — 15/372 targets remain real, reproducible red — so `test-integration` **stays out of**
`pre-commit`'s blocking chain; the promotion condition (full suite green) is not met. Tracked as
task #33 in this session's task tracker. Machine-readable standing:
`docs/aps/claims.toml`'s `dev.test-integration` entry (updated to match).

---

## TECH-DEBT-004 — `docs/security/SECURITY_DEFINITION_OF_DONE.json` claimed key rotation is
supported; it is not implemented anywhere (TECH-DEBT / doc-drift)

**Discovered:** 2026-08-03, during a targeted investigation of a specific docs-vs-reality gap
(not the combinatorial JTBD sweep that produced BUG-001 through TECH-DEBT-003 above; filed in
this doc anyway to keep the one TECH-DEBT-tracking convention this repo actually uses).

**Source evidence:** `docs/security/SECURITY_DEFINITION_OF_DONE.json`, dimension 7 ("Key
Management"), `short_description` and `what_it_is` fields (previously) stated "key rotation
supported" / "Key rotation must be supported so that compromised keys can be replaced without
invalidating old receipts," citing `ggen-config::crate::receipt::receipt_impl` as the
implementation. Both the claim and the citation were false:

1. `crates/ggen-config/src/receipt/receipt_impl.rs` read in full — `Receipt` has no key-id
   field, `generate_keypair()`/`hash_data()` are the only free functions, and there is no
   rotation logic of any kind. `crates/ggen-config/src/receipt/envelope.rs` (the sibling
   `ReceiptEnvelope` mechanism) has a `public_key_ref: String` field but it is explicitly
   documented "Free-form; not authenticated" — callers must already know and supply the correct
   `VerifyingKey` to `verify()`; there is no keyring, no key-id-based key selection.
2. Workspace-wide `grep -rin "rotat"` (case-insensitive, all files) returns zero rotation
   *implementations* — only unrelated hits: `manual_rotate` clippy lint config, `u32::rotate_*`
   bit-rotation math in `bcinr-pddl`/`chicago-tdd-tools` (SHA-256 internals), log-file
   `rotation` config fields in `ggen-config::config_lib::schema`, and other projects'
   aspirational architecture docs / audit reports describing rotation as a gap, not a feature
   (e.g. `docs/archive/legacy_structure/security/V6_WEEK1_SECURITY_AUDIT.md`: "No key rotation
   strategy (cannot invalidate compromised keys)").
3. `git log --all -i --grep="rotat"` surfaces no commit implementing key rotation for the
   current `ggen-config`/`ggen-engine` receipt-and-key system; the closest hit
   (`cc4104289`, "Security roadmap implementation (Weeks 2-12)") is pre-migration v6.1
   roadmap prose, not code that reached the current architecture.
4. The real key-resolution code backing the live `ggen sync`/`ggen receipt verify` pipeline is
   `crates/ggen-engine/src/keys.rs` (`resolve_signing_key`/`resolve_verifying_key`), an entirely
   different crate than the one the doc cited. It resolves exactly one verifying key from
   exactly one location (`GGEN_SIGNING_KEY` env var, else the single file
   `.ggen/keys/verifying.key`), and `resolve_signing_key` never overwrites an existing
   `signing.key` once one exists. There is no `ggen key rotate` (or equivalent) CLI verb
   anywhere in the workspace.

**Impact:** MEDIUM — doc-drift, not a security hole by itself, but a false "supported" claim in
a security Definition-of-Done doc is exactly the kind of overclaiming that could lead an
operator to believe a compromised signing key can be safely replaced without invalidating the
receipt chain. Today it cannot: replacing `signing.key`/`verifying.key` by hand breaks
verification of every receipt signed under the old key (the doc's own "Key mismatch" failure
mode, immediately below the corrected claim).

**Fix applied (this pass, doc-only):** corrected `short_description` and `what_it_is` in
dimension 7 to state plainly that key rotation is NOT implemented, and added a
`key_rotation_status` sub-object (status `NOT_IMPLEMENTED`, the verified gap description above,
why it is new cross-crate design rather than a small addition, current single-key behavior, and
this tracking reference). Confirmed the file still parses as valid JSON after the edit
(`python3 -c "import json; json.load(open('docs/security/SECURITY_DEFINITION_OF_DONE.json'))"`
— no schema validator exists for this file anywhere in the repo, unlike `docs/aps/claims.toml`'s
`guard-publish-standing.sh`, so a parse check is the strongest available verification).

**Not fixed in this pass (adjacent, larger, separate staleness found but out of scope):** the
rest of dimension 7's `implementation` block cites APIs that do not exist anywhere in this
workspace — `KeyPersistence::save_signing_key`/`load_signing_key`/`save_verifying_key` (confirmed
via workspace-wide grep: zero hits), `ggen-cli/src/cmds/sync.rs::emit_receipt()` (that file does
not exist under that path), and CLI flags `ggen init --show-public-key`/`--force-keys` (grep:
zero hits). `sabotage_tests` in the same dimension cite
`crates/ggen-cli/tests/key_security_tests.rs`, which also does not exist. This is a materially
larger doc-vs-reality gap than the rotation claim alone and deserves its own dedicated audit
pass of dimension 7's `implementation`/`validation_command`/`key_invariants`/`failure_modes`/
`sabotage_tests` blocks against the real `ggen-engine::keys` module — not attempted here to avoid
scope creep on top of a rushed rotation-specific fix, per this session's own instructions.

**Recommendation:** implement key rotation as its own dedicated design pass (new key-id field
threaded through `Receipt`/`ReceiptEnvelope`/`praxis_core::ReceiptRecord`, a keyring format, and
a real `ggen key rotate` CLI verb) if/when the requirement becomes load-bearing, and separately
re-audit the rest of dimension 7 against `ggen-engine::keys.rs` for the same class of stale
citation found here.

**Reproduce:** `grep -rin "rotat" --include="*.rs" --include="*.toml" --include="*.json"
--include="*.md" .` (workspace root); `git log --all --oneline -i --grep="rotat"`; read
`crates/ggen-config/src/receipt/receipt_impl.rs` and `crates/ggen-engine/src/keys.rs` in full.
