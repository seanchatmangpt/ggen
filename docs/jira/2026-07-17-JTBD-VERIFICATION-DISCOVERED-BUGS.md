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
