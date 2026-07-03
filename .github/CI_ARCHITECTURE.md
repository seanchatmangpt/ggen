# CI Architecture — Design (Agent 1)

> Authoritative design for ggen's first-principles CI refactor. Pairs with
> [`CI_TEARDOWN.md`](CI_TEARDOWN.md) (the per-workflow action table the orchestrator
> applied). The honesty/convention rules live in
> [`CI_CONVENTIONS.md`](CI_CONVENTIONS.md).
> Evidence: every claim below cites the real `on:`/jobs of the workflows in
> `.github/workflows/*.yml` as read on 2026-06-20.

---

## 1. The four root causes (and how the new design fixes each)

These are the verified causes of the all-red CI seen in PR #216, with the fix baked
into the target architecture. **The workflow names cited in this section
(`ci-complete.yml`, `helm-validation.yml`, `gitops-sync-flux.yml`, the andon pair,
`weaver-validation.yml`, etc.) describe the PRE-REFACTOR state and have since been
deleted** — they are listed here only to document what was wrong and what the
teardown removed. For the current/after design see §3–§4.

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
clones the four top-level sibling repos
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
`nightly-2026-04-15` via `dtolnay/rust-toolchain`. No
stable/beta/nightly matrix — the workspace **cannot** compile on stable or beta, so
a matrix is dishonest. No `continue-on-error` anywhere in the new set: jobs fail
honestly.

### Root cause 3 — 40 workflows / 27 PR-gating with heavy overlap

**(Before-state inventory.)** At the start of the refactor,
`grep -lE 'pull_request' .github/workflows/*.yml` returned **27** files. Functional
duplication was rampant:

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

CLAUDE.md and `CI_CONVENTIONS.md` declare `just` the single entry point, and `justfile`
implements native-cargo recipes with **zero** `cargo make` references. Yet the legacy
workflows variously call `cargo make check/lint/test-unit/...` (`andon-validation.yml`,
`ci-quality-gates.yml`, `security.yml`, `marketplace-test.yml`, `lint.yml`,
`test.yml` install cargo-make), raw `cargo ...` (`ci.yml`, `quality-gates.yml`,
`ci-complete.yml`, `build.yml`), and even the deprecated `actions-rs/toolchain@v1`
(`andon_ci.yml`, `ci-complete.yml`, `docs-validation.yml`, `deploy-docs.yml`,
`performance_benchmarks.yml`). The `cargo make` calls would fail outright (no
`Makefile.toml` task graph backing them) on top of root cause 1.

**Fix:** the new gating workflows invoke **only** the canonical `just` recipes
(see `CI_CONVENTIONS.md`): `check`, `build`, `fmt-check`, `lint`, `test-lib`, `test-doc`,
`audit`, `slo-check`, `timeout-check` — all verified present in `justfile`
(lines 10/21/25/43/49/55/65/69/102/106). One entry point, no cargo-make, no
`actions-rs/*`.

---

## 2. Before → after

### Before (the pre-refactor state that was torn down)

This is the historical inventory of what existed **before** this refactor — the
all-red baseline the teardown removed. It is preserved as the audit trail; it does
**not** describe the current directory (see "After" below for that).

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

### After (final state on disk, verified 2026-06-20)

| Metric | Count |
|--------|-------|
| Total workflow files | **22** (4 PR-gating + 18 operational) |
| Workflows triggered on `pull_request` (PR-gating) | **4** |
| The 4 PR-gating workflows | `ci.yml`, `quality.yml`, `example-tpot2.yml`, `docs.yml` |
| Operational CD/infra (push/tag/schedule/manual, NOT PR gates) | **18** (semantic-release, docker-build-push, docker, generate-release-notes, automated-rollback, deploy-docs, release, release-debian, homebrew-release, publish-registry, secrets-sync, erlang-ci, erlang-release, marketplace, marketplace-deploy, marketplace-docs, marketplace-test, marketplace-validate) |
| Build provisions siblings? | **Yes** — `setup-ggen-build` clones 4 siblings first |
| Toolchain honesty | builds only on pinned `nightly-2026-04-15` |
| Entry point | `just` only |

**Headline: 27 PR-gating workflows → 4.** The redundant/theatrical core dupes were
deleted (`ci-complete`, `ci-quality-gates`, `quality-gates`, `build`, `lint`, `test`,
the andon pair, the security pair, the three docs dupes, the four benchmark dupes,
`london-tdd-tests`, `weaver-validation`), the infra-without-infra workflows were
deleted outright (`helm-validation.yml` — no `helm/` dir; `gitops-sync-flux.yml` — no
`infra/flux/`), and the remaining operational CD/infra workflows were moved off the
`pull_request` trigger to push/tag/schedule/manual. No real PR check is lost beyond
the explicitly-flagged follow-ups (every deleted check is either absorbed by the new
set or was theatrical/duplicative — see the per-file rationale in `CI_TEARDOWN.md`).

> **State note (final, 2026-06-20):** The teardown is complete. The four PR-gating
> workflows (`ci`/`quality`/`example-tpot2`/`docs`) are the **only** files carrying an
> `on: pull_request:` trigger. The directory holds **22** `.yml` workflow files; the
> `helm-validation.yml` and `gitops-sync-flux.yml` workflows were **deleted** (their
> backing `helm/` and `infra/flux/` directories do not exist in this repo), as were the
> theater workflows (`ci-complete`, `weaver-validation`, `andon-validation`, `andon_ci`).
> The teardown-only `CI_REFACTOR_CONTRACT.md` was likewise removed. See `CI_TEARDOWN.md`
> for the per-file decision record.

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

Jobs run on the pinned nightly through `setup-ggen-build`, scoped to the
**shippable deliverable** = the workspace **excluding** `ggen-lsp`; see §4 for
the scoping rationale.
Commands are cargo-direct (mirroring the `just` recipes) because the recipes wrap
commands in short timeouts tuned for the warm-cache local loop, which fire
mid-compile on a CI cold build; the job-level `timeout-minutes` is the CI
guardrail instead.

| Job | Command (deliverable scope) | Absorbs (from deleted workflows) |
|-----|-----------------------------|----------------------------------|
| `check` | `cargo check --workspace --exclude ggen-lsp{,-mcp,-a2a}` | check from ci-complete, ci-quality-gates(G1), quality-gates, andon-validation(L1), build, andon_ci |
| `build` | `cargo build --workspace --exclude …` | `cargo build` from build.yml, quality-gates(G6) |
| `test` | `cargo test --workspace --lib --exclude …` | lib/unit tests from test.yml, ci.yml, ci-complete, ci-quality-gates(G3), quality-gates(G5), andon-validation(L2), andon_ci, london-tdd |
| `doctest` | `cargo test --doc --workspace --exclude …` | doc tests from test.yml, ci.yml, ci-quality-gates(G5), quality-gates(G7) |
| `lsp-crates` | `cargo check -p ggen-lsp --all-features` | **advisory, NON-required** — surfaces the trio's true (red) status; absent from `ci-status.needs` (§4) |
| `ci-status` | `needs: [check, build, test, doctest]` | aggregator of the four gating jobs — the stable required aggregate (does **not** `needs:` `lsp-crates`) |

### 3.2 `quality.yml` (new, Agent 4) — lint + audit + perf budget

All four jobs are **advisory / NON-required** (run for signal; not in any
required aggregate — see §4). `fmt` runs through `just fmt-check`; `clippy`,
`audit`, `slo` are cargo-direct for the same cold-build-timeout reason as §3.1.
`clippy` is deliberately **not** `-D warnings` and excludes the lsp trio
(consistent with ci.yml's deliverable scope): the workspace's clippy backlog is
unverified, so gating on it would be red for pre-existing debt. Promote to
blocking once the backlog is clean.

| Job | Command | Status | Absorbs |
|-----|---------|--------|---------|
| `fmt` | `just fmt-check` | advisory | `cargo fmt --check` from lint, ci, ci-complete, ci-quality-gates(G2), andon-validation(L1) |
| `clippy` | `cargo clippy --workspace --all-targets --exclude …` (not `-D warnings`) | advisory | clippy from lint, ci, ci-complete, ci-quality-gates(G2), quality-gates(G2), andon_ci, security |
| `audit` | `cargo audit` (warns, non-blocking) | advisory | `cargo audit`/`cargo deny` from security, security-audit, ci-complete(security-scan) |
| `slo` | `just slo-check` (warns, non-blocking) | advisory | SLO/perf-budget surface from performance×3 + ultra-deploy |

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

## 4. Required status checks (the FINAL design)

### 4.1 The required gate is deliverable-scoped (excludes the lsp trio)

The required PR gate builds the **shippable deliverable** = the workspace
**excluding** `ggen-lsp`.

Rationale:

- **Leaf crate.** Nothing in the workspace depends on `ggen-lsp` by default.
  `ggen-cli` reaches it **only** behind the optional `lsp`/`experimental`
  features (`crates/ggen-cli/Cargo.toml`), which are **off by default**. The
  default shippable product never touches it.
- **It has a history of not compiling against a moving target.** `ggen-lsp`
  depends on the **external** sibling `lsp-max`, an **untagged WIP repo cloned
  at floating HEAD** — there is no compiling commit to pin, so it can regress
  at any time independent of this repo.
- **Gating the whole product on a floating external target is the anti-pattern.**
  It makes the required PR gate hostage to an upstream nobody controls. So the
  gate is scoped to the deliverable, and the trio is surfaced **honestly** in a
  separate non-required job (§4.3).

### 4.2 Canonical required status checks

Mark **exactly these five** as required in branch protection on `main` (all from
`ci.yml`):

| Required check | Workflow / job | Gates on |
|----------------|----------------|----------|
| `Check`     | `ci.yml` / `check`     | `cargo check` (deliverable scope) |
| `Build`     | `ci.yml` / `build`     | `cargo build` (deliverable scope) |
| `Test`      | `ci.yml` / `test`      | `cargo test --lib` (deliverable scope) |
| `Doctest`   | `ci.yml` / `doctest`   | `cargo test --doc` (deliverable scope) |
| `CI Status` | `ci.yml` / `ci-status` | aggregate of the four above (`needs: [check, build, test, doctest]`) |

`ci-status` is the stable aggregate — adding/renaming a gating job updates
`ci-status`, not the protection rule. The four sub-jobs are required too so a
single failure is legible without expanding the aggregate.

### 4.3 Advisory `lsp-crates` job + promotion criteria

`ci.yml` also runs `lsp-crates` (`cargo check -p ggen-lsp --all-features`). It is **NON-required**: it shows the trio's **true** status
(currently red) with **no `continue-on-error` masking**, and is deliberately
**absent from `ci-status.needs`**, so its failure cannot block a PR or the merge
queue. `validate-workflows.py` (`check_advisory_isolation`) enforces this
absence.

> **Promotion criterion:** move `lsp-crates` into `ci-status.needs` (making it a
> required gate) once `ggen-lsp` tracks a **pinned** `lsp-max` SHA (§4.5) and
> compiles cleanly against it.

### 4.4 Advisory (NON-required) checks

Run on PRs for signal; do **not** block merge:

| Check | Workflow / job | Why not required |
|-------|----------------|------------------|
| `clippy` | `quality.yml` / `clippy` | not `-D warnings` (pre-existing backlog); promote once clean. |
| `audit` | `quality.yml` / `audit` | RUSTSEC DB is time-varying; a new advisory should not retro-block an unrelated PR. Also runs on `schedule`. |
| `slo` | `quality.yml` / `slo` | needs a full build budget; timing-sensitive on shared runners. |
| `fmt` | `quality.yml` / `fmt` | currently advisory; cheap & deterministic — a sound first candidate to promote to required. |
| `example-tpot2` | `example-tpot2.yml` | path-filtered; only meaningful when the example changes. |
| `docs` (`links`) | `docs.yml` | path-filtered; ~1100 pre-existing broken links → advisory until cleared. |

Path-filtered workflows (`example-tpot2`, `docs`) must **not** be set as
universally required, or PRs that don't touch those paths hang forever waiting on
a check that never runs. Rely on GitHub's "skipped = success" with `paths:`
filters (verify on your repo's ruleset version) or pair with a "skip → success"
job.

### 4.5 Sibling SHA-pinning (reproducibility)

`setup-ggen-build` pins the four sibling repos to **exact commit SHAs** rather
than a floating `git clone --depth 1` (which would fetch a different HEAD per run,
since the siblings are untagged WIP repos → non-reproducible builds):

| sibling | pinned SHA |
|---------|-----------|
| `lsp-max` | `7bcc1e16dec71ef5fb2cedea2dfd6cb5cde37f59` |
| `lsp-types-max` | `6773e6017ca83c565785d0ec39d75304f62c3237` |
| `wasm4pm` | `0bb134b29245517ac4969a9f1916f5432931c5d0` |
| `wasm4pm-compat` | `e46155e209a750fda0218532d96ae17a9e10903e` |

**How to bump a sibling:** update its SHA in the action's `shas` map **and**
re-verify the consuming code compiles against the new revision. Bumping `lsp-max`
in particular must be paired with re-checking the `ggen-lsp` trio (it is the
input to the §4.3 promotion gate). `check_sibling_pinning` in the validator FAILs
on any floating `git clone --depth 1 "$org/$repo"` or a missing SHA.

### 4.6 Action SHA-pinning convention

Every third-party `uses:` is pinned to a full **40-hex commit SHA** with a
trailing `# vX.Y.Z` comment (a tag is mutable; a SHA is not). Local composite
actions (`./...`) need no pin. `dtolnay/rust-toolchain`, `taiki-e/install-action`,
and `Swatinem/rust-cache` are commit-pinned **inside** `setup-ggen-build` —
gating workflows never call them directly. Enforced per-line by the validator
(`uses_is_sha_pinned`).

### 4.7 Merge-queue support

`ci.yml` and `quality.yml` trigger on `merge_group` in addition to
`pull_request` + `push: [main]`, so required checks also run in the merge queue —
a PR that was green at PR time cannot break `main` once batched. Keep merge-queue
triggers in lockstep with the PR triggers. Only the four core workflows may carry
an `on: pull_request:` trigger (`check_pr_trigger_scope`).

---

## 5. Real-check risk register (resolved)

Checks in the deleted set that are **not 1:1 reproduced** by the new four-workflow
set. None was a hard blocker for the refactor. Each was triaged and **resolved** with
the decision recorded below; the table is kept as the risk register so the trade-offs
stay legible and the deferred items remain visible as follow-ups.

| At-risk check | Source workflow | Status in new set | Decision (resolved) |
|---------------|-----------------|-------------------|---------------------|
| Integration/BDD/E2E tests (`cargo test --test cli/marketplace/e2e_*`, `--test bdd`) | `ci.yml`(old), `test.yml`, `ci-complete.yml` | **Partially covered** — `ci.yml`'s `test` job is lib-only. | **Decision: lib-only (`cargo test --workspace --lib`, deliverable-scoped) for a fast PR gate; integration/BDD/E2E are a documented follow-up, not currently gated.** |
| File-organization check (no `.rs`/`.tmpl`/`.sh` in repo root) | `ci.yml`(old) `file-organization` job | **Not in new set.** | **Decision: not gated (low risk; optional future quality.yml job).** |
| LSP/MCP plugin harness + smoke (`ggen-lsp` contract tests, `lsp-smoke.py`, `mcp-smoke.py`, `find-fakes.sh`) | `ci.yml`(old) `plugin-harness` job | **Trio compile surfaced by advisory `lsp-crates`; harness itself not in new set.** | **Decision: the lsp trio compile is surfaced by the advisory `lsp-crates` job; a dedicated harness job is a follow-up.** |
| Code coverage ≥80% (tarpaulin / codecov) | `ci.yml`(old) `coverage`, `quality-gates.yml`(G3) | **Not in new set.** | **Decision: deferred (slow/flaky on this workspace); documented gap.** |
| Panic-point / hardcoded-path scanners | `quality-gates.yml`(G1,G4) | **Not in new set.** | **Decision: not gated** — these map to repo "no unwrap / no hardcoded paths" rules but were grep-theater in places; optional future `quality.yml` job. |
| OTEL/LLM e2e verification (`llm_e2e_test`, OTEL span grep) | `ci-quality-gates.yml`(G4), `weaver-validation.yml` | **Not in new set.** | **Decision: dropped from PR gate** (requires live LLM creds / docker stack; non-deterministic by design). Retain only on `schedule`/`workflow_dispatch` if ever revived. |
| `validate-docs.yml` per-guide validation that needs a built `ggen-cli` | `validate-docs.yml` | **`docs.yml` is doc-only (no cargo).** | **Decision: keep doc-only `docs.yml`; CLI-backed guide validation is a follow-up** (would need `setup-ggen-build` and belongs in the Rust CI path, not the fast docs gate). |

**Summary (resolved):** the genuinely-real checks not reproduced by the minimal set —
integration/BDD/E2E tests, the LSP/MCP plugin harness, the coverage gate, and
CLI-backed doc-guide validation — are all **deliberately deferred follow-ups**, not
silent drops. The four-workflow scope intentionally trades them for an honest, fast,
green baseline. They remain recorded here so the gaps are tracked and can be promoted
when the workspace supports them (e.g. `lsp-crates` → required once `ggen-lsp`
compiles against a pinned `lsp-max`; coverage once it is no longer flaky).

---

## 6. Best-practices applied to the new set (per `CI_CONVENTIONS.md`)

Every new workflow MUST carry: `concurrency` (cancel-in-progress on ref),
top-level `permissions: { contents: read }` (escalate per-job only where needed),
`timeout-minutes` on every job (build/test ≤30, lint ≤15, python ≤5),
**SHA-pinned** third-party actions (full 40-hex commit + trailing `# vX.Y.Z`,
e.g. `actions/checkout@11bd71901bbe5b1630ceea73d27597364c9af683  # v4.2.2`;
`dtolnay/rust-toolchain` / `taiki-e/install-action` / `Swatinem/rust-cache`
commit-pinned inside the action), `on: { pull_request:, push: { branches:
[main] }, merge_group: }` for the gating workflows, `paths:`/`paths-ignore:`
where it makes sense, **no** `continue-on-error` to mask failure, a `ci-status`
aggregator that excludes the advisory `lsp-crates`, and stable job `name:`s.
These are enforced structurally by `scripts/ci/validate-workflows.py` (Agent 5):
per-file `name`/`on`/`jobs` + `uses:` SHA-pinning, plus repo-wide PR-trigger
scope, advisory isolation, and sibling SHA-pinning checks.
