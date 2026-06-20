# CI Architecture — Design (Agent 1)

> Authoritative design for ggen's first-principles CI refactor. Pairs with
> [`CI_TEARDOWN.md`](CI_TEARDOWN.md) (the per-workflow action table the orchestrator
> applies) and obeys [`CI_REFACTOR_CONTRACT.md`](CI_REFACTOR_CONTRACT.md).
> Evidence: every claim below cites the real `on:`/jobs of the workflows in
> `.github/workflows/*.yml` as read on 2026-06-20.

---

## 1. The four root causes (and how the new design fixes each)

These are the verified causes of the all-red CI seen in PR #216, with the fix baked
into the target architecture.

### Root cause 1 — CI never provisions the sibling repos → every build dies at workspace load

`Cargo.toml` line 789 declares a `[patch.crates-io]` block that redirects the
`lsp-max` family and the `wasm4pm` family to **local sibling paths**:

```toml
[patch.crates-io]
lsp-max          = { path = "../lsp-max" }
lsp-max-protocol = { path = "../lsp-max/lsp-max-protocol" }
lsp-max-macros   = { path = "../lsp-max/lsp-max-macros" }
lsp-max-client   = { path = "../lsp-max/crates/lsp-max-client" }
wasm4pm-compat   = { path = "../wasm4pm-compat" }
wasm4pm          = { path = "../wasm4pm/wasm4pm" }
```

Cargo resolves `../lsp-max` relative to the workspace root, i.e.
`$GITHUB_WORKSPACE/../lsp-max` = `/home/runner/work/ggen/lsp-max`. Every existing
workflow checks out **only** `ggen` (`actions/checkout@v4` with no sibling clone),
so cargo fails at manifest-load time:
`failed to read /home/runner/work/ggen/lsp-max/Cargo.toml`. This is why **every**
cargo job in every workflow is red regardless of what it actually tests — the
failure is upstream of compilation.

**Fix:** the DRY composite action `./.github/actions/setup-ggen-build`
(Agent 2, frozen in contract §2.1) clones the four top-level sibling repos
(`lsp-max`, `lsp-types-max`, `wasm4pm`, `wasm4pm-compat`) into
`$GITHUB_WORKSPACE/..` *before* any cargo invocation. The `lsp-max-protocol`,
`-macros`, and `-client` paths and `wasm4pm/wasm4pm` all live **inside** those
cloned repos, so four clones satisfy all six patch entries. Every new workflow
that runs cargo calls this action as its first step after checkout. This is
proven locally — see `examples/tpot2-wasm4pm-autoconfig/BUILD_PROVISIONING.md`,
which reproduced a green `cargo build` + `ggen sync` using exactly this provisioning.

### Root cause 2 — the toolchain matrix is a lie (pinned nightly overrides it)

`rust-toolchain.toml` line 2 pins `channel = "nightly-2026-04-15"`. Rustup honors
that file for **every** invocation, so a `matrix: rust: [stable, beta, nightly]`
entry installs the matrix toolchain but then *runs* nightly-2026-04-15 anyway. The
matrix produces no real coverage; it only multiplies the cost and the red X's.
Workflows that do this today: `build.yml` (`[stable, beta, nightly]`), `test.yml`
(`[stable, beta]`), `ci.yml` build-matrix/comprehensive-test, `ultra-deploy-test.yml`
(`[stable, nightly]` × 2 OS), `validate-docs.yml` (`[stable]` × 2 OS). Worse,
`build.yml` adds `continue-on-error` semantics around nightly to mask its failure,
so only stable/beta showed red while nightly's real failure (root cause 1) was hidden.

**Fix:** build **only** on the pinned nightly. The setup action installs
`nightly-2026-04-15` via `dtolnay/rust-toolchain` (contract §2.4). No
stable/beta/nightly matrix — the workspace **cannot** compile on stable or beta, so
a matrix is dishonest. No `continue-on-error` anywhere in the new set: jobs fail
honestly.

### Root cause 3 — 40 workflows / 27 PR-gating with heavy overlap

`grep -lE 'pull_request' .github/workflows/*.yml` returns **27** files. Functional
duplication is rampah:

- **Core build/test/lint, ~4×:** `ci.yml`, `ci-complete.yml`, `ci-quality-gates.yml`,
  `quality-gates.yml` all run check + clippy + fmt + unit/integration tests on PRs,
  plus the standalone `build.yml`, `lint.yml`, `test.yml`.
- **Performance, 3×:** `performance.yml`, `performance_benchmarks.yml`,
  `pattern-benchmarks.yml` (+ `ultra-deploy-test.yml`) all run benchmarks on PRs
  with `timeout-minutes: 60`.
- **Andon, 2×:** `andon-validation.yml`, `andon_ci.yml` — both re-run check/clippy/test
  and wrap them in "signal" theater.
- **Security, 2×:** `security.yml`, `security-audit.yml` — both run `cargo audit` +
  `cargo deny` on PRs.
- **Docs, 3×:** `docs-validation.yml`, `validate-docs.yml`, `toc.yml`.
- **Marketplace, 2× PR-gating** (`marketplace-test.yml`, `marketplace-validate.yml`)
  of 5 marketplace workflows total.
- Plus PR-gating CD/infra that should never gate a PR: `docker-build-push.yml`,
  `erlang-ci.yml`, `gitops-sync-flux.yml`, `helm-validation.yml`, `weaver-validation.yml`,
  `london-tdd-tests.yml`.

**Fix:** collapse all PR-gating Rust/docs validation into **four** workflows
(`ci.yml` rewritten, `quality.yml` new, `example-tpot2.yml` new, `docs.yml` new). Move
every operational CD/infra workflow off the `pull_request` trigger (see §4). Delete
the redundant/theatrical core dupes. See `CI_TEARDOWN.md` for the per-file decision.

### Root cause 4 — entry-point drift (`cargo make` vs `just`)

CLAUDE.md and the contract declare `just` the single entry point, and `justfile`
implements native-cargo recipes with **zero** `cargo make` references. Yet the legacy
workflows variously call `cargo make check/lint/test-unit/...` (`andon-validation.yml`,
`ci-quality-gates.yml`, `security.yml`, `marketplace-test.yml`, `lint.yml`,
`test.yml` install cargo-make), raw `cargo ...` (`ci.yml`, `quality-gates.yml`,
`ci-complete.yml`, `build.yml`), and even the deprecated `actions-rs/toolchain@v1`
(`andon_ci.yml`, `ci-complete.yml`, `docs-validation.yml`, `deploy-docs.yml`,
`performance_benchmarks.yml`). The `cargo make` calls would fail outright (no
`Makefile.toml` task graph backing them) on top of root cause 1.

**Fix:** the new gating workflows invoke **only** the frozen `just` recipes
(contract §2.2): `check`, `build`, `fmt-check`, `lint`, `test-lib`, `test-doc`,
`audit`, `slo-check`, `timeout-check` — all verified present in `justfile`
(lines 10/21/25/43/49/55/65/69/102/106). One entry point, no cargo-make, no
`actions-rs/*`.

---

## 2. Before → after

### Before (current state on disk)

| Metric | Count |
|--------|-------|
| Total workflow files | **40** |
| Workflows triggered on `pull_request` (PR-gating) | **27** |
| Distinct core build/test/lint workflows on PRs | 7 (`ci`, `ci-complete`, `ci-quality-gates`, `quality-gates`, `build`, `lint`, `test`) |
| Benchmark workflows on PRs | 4 (`performance`, `performance_benchmarks`, `pattern-benchmarks`, `ultra-deploy-test`) |
| Andon workflows on PRs | 2 |
| Security workflows on PRs | 2 |
| Docs workflows on PRs | 3 (`docs-validation`, `validate-docs`, `toc`) |
| Build provisions siblings? | **No** (0/40) → every cargo job red |
| Toolchain honesty | matrix is a no-op; pinned nightly always wins |
| Entry point | mixed: `cargo make`, raw `cargo`, `actions-rs/*`, `just` |

### After (target state)

| Metric | Count |
|--------|-------|
| Total workflow files | **24** (43 current − 19 deleted) |
| Workflows triggered on `pull_request` (PR-gating) | **4** |
| The 4 PR-gating workflows | `ci.yml`, `quality.yml`, `example-tpot2.yml`, `docs.yml` |
| Operational CD/infra retriggered off PRs | **4** (erlang-ci, helm-validation, marketplace-test, marketplace-validate) — gitops-sync-flux & docker-build-push are already off PRs |
| Build provisions siblings? | **Yes** — `setup-ggen-build` clones 4 siblings first |
| Toolchain honesty | builds only on pinned `nightly-2026-04-15` |
| Entry point | `just` only |

**Headline: 27 PR-gating workflows → 4.** 19 legacy files deleted, 4 retriggered
off `pull_request` to push/schedule/manual. No real PR check is lost beyond the
explicitly-flagged follow-ups (every deleted check is either absorbed by the new set
or was theatrical/duplicative — see the per-file rationale in `CI_TEARDOWN.md`).

> **State note (as of 2026-06-20 re-sample):** Agents 4/5 have already landed
> `quality.yml`, `example-tpot2.yml`, `docs.yml`, so the directory now holds **43**
> files (the original 40 + 3 new), and `ci.yml` is being rewritten in place. The
> 27 active `pull_request` triggers therefore split into **4 keep** (the new/rewritten
> `ci`/`quality`/`example-tpot2`/`docs`) and **23 legacy to remove** (19 DELETE +
> 4 RETRIGGER). After teardown: **24** files (43 − 19 deleted). See
> `CI_TEARDOWN.md` §F for the exact arithmetic.

---

## 3. Target architecture (the new minimal set)

```
.github/
  actions/
    setup-ggen-build/action.yml   # [Agent 2] DRY: clone 4 siblings → $GITHUB_WORKSPACE/..,
                                   #            install nightly-2026-04-15, Swatinem cache
  workflows/
    ci.yml             # [Agent 3] core gate: check / build / test-lib / doctest + ci-status
    quality.yml        # [Agent 4] fmt-check / clippy(lint) / audit / slo(non-blocking)
    example-tpot2.yml  # [Agent 5] Python-only example verification (NO cargo)
    docs.yml           # [Agent 5] consolidated docs/TOC validation
  CI_ARCHITECTURE.md   # [Agent 1] this file
  CI_TEARDOWN.md       # [Agent 1] per-workflow DELETE/RETRIGGER/KEEP table
  CI_CONVENTIONS.md    # [Agent 5] conventions
scripts/ci/validate-workflows.py  # [Agent 5] structural YAML validation
```

### 3.1 `ci.yml` (rewrite, Agent 3) — the merge gate

Jobs, all via `just` on pinned nightly through `setup-ggen-build`:

| Job | Command | Absorbs (from deleted workflows) |
|-----|---------|----------------------------------|
| `check` | `just check` | check from ci-complete, ci-quality-gates(G1), quality-gates, andon-validation(L1), build, andon_ci |
| `build` | `just build` | `cargo build` from build.yml, quality-gates(G6) |
| `test` | `just test-lib` | lib/unit tests from test.yml, ci.yml, ci-complete, ci-quality-gates(G3), quality-gates(G5), andon-validation(L2), andon_ci, london-tdd |
| `doctest` | `just test-doc` | doc tests from test.yml, ci.yml, ci-quality-gates(G5), quality-gates(G7) |
| `ci-status` | `needs: [check, build, test, doctest]` | single aggregator — **the** required status check |

### 3.2 `quality.yml` (new, Agent 4) — lint + audit + perf budget

| Job | Command | Absorbs |
|-----|---------|---------|
| `fmt` | `just fmt-check` | `cargo fmt --check` from lint, ci, ci-complete, ci-quality-gates(G2), andon-validation(L1) |
| `clippy` | `just lint` | clippy `-D warnings` from lint, ci, ci-complete, ci-quality-gates(G2), quality-gates(G2), andon_ci, security |
| `audit` | `just audit` | `cargo audit`/`cargo deny` from security, security-audit, ci-complete(security-scan) |
| `slo` | `just slo-check` | SLO/perf-budget surface from performance×3 + ultra-deploy (non-blocking; documented) |

### 3.3 `example-tpot2.yml` (new, Agent 5) — Python example, no cargo

`runs-on: ubuntu-latest`, Python 3.11, `paths: ['examples/tpot2-wasm4pm-autoconfig/**']`.
Runs the example's `verify/validate_artifacts.py`, `test_tpot2_autoconfig.py`,
`test_vision2030.py`, `conformance_check.py`, `slo_check.py` (all confirmed present
under `examples/tpot2-wasm4pm-autoconfig/verify/`). No cargo, so no sibling
provisioning needed.

### 3.4 `docs.yml` (new, Agent 5) — consolidated docs/TOC

Absorbs the **non-cargo** doc validation from `docs-validation.yml`,
`validate-docs.yml`, and `toc.yml` (TOC freshness check, doc-script shellcheck). The
heavy `cargo build --release` + per-guide validation in `validate-docs.yml` that
depends on a built `ggen-cli` is the one borderline case — see §5 "real-check risk".

---

## 4. Required status checks (recommendation)

Configure GitHub branch protection on `main` to require **exactly one** required
status check:

> **`CI Status`** — the `ci-status` aggregator job in `ci.yml`
> (`needs: [check, build, test, doctest]`).

Rationale (per contract §3): a single aggregator is the only required check, so the
gate is honest and stable — adding/renaming a sub-job never silently drops the gate,
and the branch-protection config never has to enumerate volatile job names.

**Strongly recommended as additional required checks** (gate merge, but kept separate
so a fmt nit doesn't block on a build flake and vice-versa):

| Required check | Workflow / job | Why it gates |
|----------------|----------------|--------------|
| `CI Status` | `ci.yml` / `ci-status` | compile + build + lib tests + doctests must pass |
| `fmt` | `quality.yml` / `fmt` | formatting is cheap, deterministic, non-negotiable |
| `clippy` | `quality.yml` / `clippy` | `-D warnings`; Andon discipline ("warnings not acceptable in committed code") |

**Recommended NON-required** (run on PRs for signal, but do not block merge):

| Check | Workflow / job | Why not required |
|-------|----------------|------------------|
| `audit` | `quality.yml` / `audit` | advisory DB is time-varying; a new RUSTSEC advisory should not retro-block an unrelated PR. Keep it visible; also runs on `schedule`. |
| `slo` | `quality.yml` / `slo` | needs a full build budget; documented as non-blocking (contract §4, Agent 4). |
| `example-tpot2` | `example-tpot2.yml` | path-filtered; only meaningful when the example changes. |
| `docs` | `docs.yml` | path-filtered docs/TOC; should not block code-only PRs. |

Path-filtered workflows (`example-tpot2`, `docs`) must **not** be set as universally
required, or PRs that don't touch those paths will hang forever waiting on a check
that never runs. If you require them, pair with a "skip → success" job or rely on
GitHub's "skipped = success" behavior with `paths:` filters (verify on your repo
ruleset version).

---

## 5. Real-check risk register (flags for the orchestrator)

Checks in the deleted/retriggered set that are **not 1:1 reproduced** by the new
four-workflow set. None is a hard blocker for the refactor, but each is called out so
no real coverage silently vanishes.

| At-risk check | Source workflow | Status in new set | Recommendation |
|---------------|-----------------|-------------------|----------------|
| Integration/BDD/E2E tests (`cargo test --test cli/marketplace/e2e_*`, `--test bdd`) | `ci.yml`(old), `test.yml`, `ci-complete.yml` | **Partially covered.** `just test-lib` is lib-only; `just test` runs the full suite. Agent 3 was scoped to `test-lib`. | Confirm with Agent 3/orchestrator whether `ci.yml`'s `test` job should call `just test-lib` (fast) or `just test` (full). If `test-lib`, integration/E2E coverage drops vs. old `test.yml`. Flag. |
| File-organization check (no `.rs`/`.tmpl`/`.sh` in repo root) | `ci.yml`(old) `file-organization` job | **Not in new set.** | Cheap, no-cargo. Optionally fold into `quality.yml` or a tiny job. Low risk (CLAUDE.md "no root files" rule). Flag as optional. |
| LSP/MCP plugin harness + smoke (`ggen-lsp` contract tests, `lsp-smoke.py`, `mcp-smoke.py`, `find-fakes.sh`) | `ci.yml`(old) `plugin-harness` job | **Not in new set.** | Real, non-theatrical. Candidate for a future `ci.yml` job or a dedicated `plugin.yml`. Not in the frozen 4-workflow scope. **Flag — real check at risk.** |
| Code coverage ≥80% (tarpaulin / codecov) | `ci.yml`(old) `coverage`, `quality-gates.yml`(G3) | **Not in new set.** | Coverage gates are slow and flaky on this workspace; deferring is defensible but is a real check. **Flag — coverage gate dropped.** |
| Panic-point / hardcoded-path scanners | `quality-gates.yml`(G1,G4) | **Not in new set.** | These map to repo "no unwrap / no hardcoded paths" rules. Genuinely useful but were grep-theater in places. Optionally re-add to `quality.yml` later. Flag as optional. |
| OTEL/LLM e2e verification (`llm_e2e_test`, OTEL span grep) | `ci-quality-gates.yml`(G4), `weaver-validation.yml` | **Not in new set.** | Requires live LLM creds / weaver docker stack; flaky as a PR gate by design. Move to `schedule`/`workflow_dispatch` if retained. **Flag — OTEL gate dropped from PRs** (acceptable: it was non-deterministic). |
| `validate-docs.yml` per-guide validation that needs a built `ggen-cli` | `validate-docs.yml` | **Cargo-dependent; Agent 5's `docs.yml` is doc-only.** | If the guide validation must run, it needs `setup-ggen-build`. Decide: (a) keep doc-only `docs.yml` and accept loss of CLI-backed guide checks, or (b) add a sibling-provisioned job. **Flag.** |

**Summary of flags:** the four genuinely-real checks not covered by the minimal set
are **(1) integration/BDD/E2E tests** (resolvable by choosing `just test` over
`just test-lib`), **(2) LSP/MCP plugin harness**, **(3) code-coverage gate**, and
**(4) CLI-backed doc-guide validation**. All are deferrable; the contract's frozen
4-workflow scope intentionally trades them for an honest, fast, green baseline. The
orchestrator should record these as follow-ups, not silently drop them.

---

## 6. Best-practices applied to the new set (per contract §3)

Every new workflow MUST carry: `concurrency` (cancel-in-progress on ref),
top-level `permissions: { contents: read }` (escalate per-job only where needed),
`timeout-minutes` on every job (build/test ≤30, lint ≤15, python ≤5), version-pinned
third-party actions (`actions/checkout@v4`, `Swatinem/rust-cache@v2`,
`dtolnay/rust-toolchain` commit-pinned in the action), `on: { pull_request:, push: {
branches: [main] } }` for gates, `paths:`/`paths-ignore:` where it makes sense, **no**
`continue-on-error` to mask failure, a single `ci-status` aggregator, and stable job
`name:`s. These are enforced structurally by `scripts/ci/validate-workflows.py`
(Agent 5).
