# CI Teardown Map — Per-Workflow Decisions (Agent 1)

> The orchestrator applies this table **verbatim**: DELETE = remove the file;
> RETRIGGER = replace the `on:` block with the one given here; KEEP = leave as-is.
> Pairs with [`CI_ARCHITECTURE.md`](CI_ARCHITECTURE.md). Agents do **not** apply this;
> only the orchestrator does, after the new ci/quality/example-tpot2/docs land.
>
> Evidence basis: triggers and jobs read from `.github/workflows/*.yml` on 2026-06-20.
> Cross-checked against `grep -lE 'pull_request' .github/workflows/*.yml` → **27** files.

---

## Legend

- **DELETE** — redundant/theatrical core dupe superseded by the new
  `ci.yml` / `quality.yml` / `docs.yml`. Each row names which new workflow absorbs its
  real checks, or states "no real check lost".
- **RETRIGGER** — operational CD/infra; must **not** gate PRs. The exact replacement
  `on:` block is given. Remove every `pull_request:` clause.
- **KEEP** — already correct (not PR-triggered, or correctly scoped). No change.
- **MERGE** is recorded as DELETE-with-absorber (the new workflows already exist;
  there is no separate file to merge into).

---

## A. The new PR-gating set (created by Agents 3/4/5 — listed for completeness)

| workflow | current triggers | decision | rationale | orchestrator action |
|----------|------------------|----------|-----------|---------------------|
| `ci.yml` | `push:[main]` + `pull_request:` | **REPLACED (Agent 3)** | Rewritten to `check`/`build`/`test-lib`/`doctest` + `ci-status` via `just` on pinned nightly + `setup-ggen-build`. | None for Agent 1; Agent 3 owns the rewrite. Keep file. |
| `quality.yml` | (new) | **NEW (Agent 4)** | `fmt`/`clippy`/`audit`/`slo`. | None. Keep file. |
| `example-tpot2.yml` | (new) | **NEW (Agent 5)** | Python-only example verification, `paths:`-filtered. | None. Keep file. |
| `docs.yml` | (new) | **NEW (Agent 5)** | Consolidated docs/TOC validation. | None. Keep file. |

---

## B. DELETE — redundant / theatrical core duplicates (17 files)

| workflow | current triggers | decision | rationale (what it really runs) | absorbed by / orchestrator action |
|----------|------------------|----------|----------------------------------|-----------------------------------|
| `ci-complete.yml` | `push:[main,develop,feature/**]` + `pull_request:[main,develop]` | **DELETE** | quick-checks (`cargo fmt`/`clippy`/`check`), unit-tests, integration-tests, `security-scan` (`cargo audit`), spec-validation, andon-signal theater. Full overlap with new ci+quality. Uses raw `cargo` + `actions-rs`. | check/test → **ci.yml**; fmt/clippy/audit → **quality.yml**. `rm`. No real check lost (spec-validation is a TTL grep; not in frozen scope). |
| `ci-quality-gates.yml` | `push:[main]` + `pull_request:[opened,synchronize,reopened,ready_for_review]` | **DELETE** | G1 `cargo make check`, G2 `cargo make lint`+fmt, G3 `cargo make test-unit`, G4 LLM+OTEL e2e, G5 `cargo make test-doc`. All `cargo make` (no Makefile.toml → would fail). | check/test/doctest → **ci.yml**; lint/fmt → **quality.yml**. G4 OTEL e2e is non-deterministic, dropped from PRs (flagged §5 of architecture). `rm`. |
| `quality-gates.yml` | `pull_request:[opened,synchronize,reopened,ready_for_review]` + `push:[main]` | **DELETE** | G1 panic-points, G2 clippy-strict, G3 coverage≥80% (tarpaulin), G4 hardcoded-paths, G5 all-tests, G6 build-all-platforms, G7 doc-consistency. | clippy → **quality.yml**; tests/build/doc → **ci.yml**. **Real-check risk:** coverage gate (G3) + panic/hardcoded scanners (G1,G4) not reproduced — flagged in architecture §5. `rm`. |
| `andon-validation.yml` | `push:[main]` + `pull_request:` + `workflow_dispatch` | **DELETE** | L1 `cargo make check`+`lint`, L2 `cargo make test-unit`+`test-clnrm`, L3 `build-release`+`verify-cli`, report. `cargo make` theater. | check/lint/test → **ci.yml**+**quality.yml**. `rm`. No real check lost (report is a STEP_SUMMARY writer). |
| `andon_ci.yml` | `push:[main]` + `pull_request:[main]` | **DELETE** | red-alert (`cargo test --no-run`, 120s timeout), yellow-alert (`cargo clippy ... || true`), dashboard. Uses deprecated `actions-rs/toolchain@v1`, `checkout@v3`. | compile/test → **ci.yml**; clippy → **quality.yml**. `rm`. No real check lost. |
| `weaver-validation.yml` | `pull_request:[main]` + `workflow_dispatch` | **DELETE** | Builds `ggen` release, spins docker weaver+OTEL stack, greps reports for `level.*violation`. Docker-stack-dependent → flaky PR gate. | No new home (frozen scope excludes it). **Real-check risk: OTEL semconv check dropped from PRs** (flagged §5). Retain only via `workflow_dispatch` if desired — but contract lists it DELETE. `rm`. |
| `london-tdd-tests.yml` | `push:[main]` + `pull_request:[main]` | **DELETE** | `cargo test --test london_tdd_main`. Project policy is **Chicago TDD only** (CLAUDE.md); a dedicated London-TDD gate is anti-policy theater. | Any surviving real tests run under **ci.yml** `test`. `rm`. No real check lost. |
| `performance.yml` | `push:[main]` + `pull_request:[main]` + `workflow_dispatch` | **DELETE** | `cargo criterion` on pipeline/cli/template benches, memory-profiling, continuous-benchmark (`timeout-minutes: 60`). Benches as a PR gate = slow + noisy. | SLO surface → **quality.yml** `slo` (non-blocking). `rm`. No hard check lost (benches were `|| true`). |
| `performance_benchmarks.yml` | `push:[main]` + `pull_request:[main]` + `schedule(daily 2AM)` + `workflow_dispatch` | **DELETE** | `cargo test -p ggen-core --test performance_benchmarks` ×3 tiers + regression-detection + daily tracking. Duplicate of `performance.yml`. | SLO surface → **quality.yml** `slo`. `rm`. Note: drops the daily `schedule`; if perf-history tracking is wanted, re-add a `schedule`-only perf workflow later (follow-up). |
| `pattern-benchmarks.yml` | `push:[main,develop]`+paths + `pull_request:[main,develop]`+paths + `schedule(weekly Mon)` + `workflow_dispatch` | **DELETE** | `cargo bench --bench pattern_performance/memory_profiling/regression_detection` + `validate_benchmark_targets.sh` (`timeout-minutes: 60`). Third perf dupe. | SLO surface → **quality.yml** `slo`. `rm`. Same `schedule` follow-up note as above. |
| `ultra-deploy-test.yml` | `push:[main]` + `pull_request:[main]` + `schedule(nightly 2AM)` | **DELETE** | `cargo test --test ultra_deploy_test` across `[ubuntu,macos]×[stable,nightly]` matrix (lie — root cause 2) + perf-comparison. | Build/test → **ci.yml**. `rm`. No real check lost (the only unique asset, `ultra_deploy_test`, runs under the normal test suite if it's a real test target). |
| `build.yml` | `push:[main]` + `pull_request:[main]` | **DELETE** | `cargo build --verbose` + `cargo test --verbose` on `[stable,beta,nightly]` matrix (lie) with nightly-failure masking. Calls non-existent `./.github/actions/setup-rust-cached` + cargo-make. | build → **ci.yml** `build`; test → **ci.yml** `test`. `rm`. No real check lost. |
| `lint.yml` | `push:[main]` + `pull_request:[main]` | **DELETE** | `cargo fmt --all -- --check` + `cargo clippy --all-targets --all-features -- -D warnings`. Installs cargo-make (unused). | fmt+clippy → **quality.yml** `fmt`/`clippy`. `rm`. No real check lost. |
| `test.yml` | `push:[main]` + `pull_request:[main]` | **DELETE** | `cargo test --workspace --lib`, integration (`--test cli/marketplace/e2e_*`), BDD (`--test bdd`), `cargo test --doc`, `cargo make test-all-features` on `[stable,beta]` (lie). | lib/doc → **ci.yml** `test`/`doctest`. **Real-check risk:** integration/BDD/E2E coverage depends on whether `ci.yml` uses `just test-lib` (lib-only) vs `just test` (full) — flagged §5. `rm`. |
| `docs-validation.yml` | `push:`+paths + `pull_request:` | **DELETE** | `./scripts/validate-all-docs.sh` (+ `--bench`). Uses `actions-rs`, `checkout@v3`. | Doc validation → **docs.yml** (Agent 5). `rm`. (If the script needs a built CLI, see `validate-docs.yml` risk note.) |
| `validate-docs.yml` | `push:[main]`+paths + `pull_request:[main]`+paths + `schedule(nightly)` + `workflow_dispatch` | **DELETE** | `cargo build --release -p ggen-cli-lib` then `validate-quick-start/sparql-guide/cli-reference/all.sh` + shellcheck. On `[ubuntu,macos]×[stable]`. | Shellcheck + doc checks → **docs.yml**. **Real-check risk:** the CLI-backed per-guide validation is cargo-dependent and **not** reproduced by a doc-only `docs.yml` — flagged §5. `rm`. |
| `toc.yml` | `pull_request:`+paths(`README.md`,`docs/**/*.md`) | **DELETE** | TOC freshness check (CHECK_ONLY, never auto-commit). | TOC check → **docs.yml** (Agent 5 consolidates). `rm`. No real check lost (folded). |

**DELETE count: 17.** Of these, **15 are currently PR-gating** (all rows above except
`performance_benchmarks.yml`… wait — it IS PR-gating). Recount of PR-gating among
DELETE: `ci-complete`, `ci-quality-gates`, `quality-gates`, `andon-validation`,
`andon_ci`, `weaver-validation`, `london-tdd-tests`, `performance`,
`performance_benchmarks`, `pattern-benchmarks`, `ultra-deploy-test`, `build`, `lint`,
`test`, `docs-validation`, `validate-docs`, `toc` = **17** PR-gating files deleted.

---

## C. RETRIGGER — operational CD/infra (remove `pull_request`; exact new `on:`)

These 6 are currently PR-triggered but are operational and must not gate PRs.

### C.1 `marketplace-test.yml`
Current: `push:[main]` + `pull_request:[main]`. Runs `cargo test test_marketplace_local`
+ two `ggen-core` registry unit tests; installs cargo-make. Marketplace unit coverage
belongs in the normal suite, not a separate PR gate.
**Replace `on:` with:**
```yaml
on:
  push:
    branches: [main]
  workflow_dispatch:
```

### C.2 `marketplace-validate.yml`
Current: `push:[main,develop]`+paths + `pull_request:[main,develop]`+paths. Builds
`ggen-cli` release, runs `ggen marketplace emit-receipts/generate-artifacts/report` +
regression compare. This is a release/health operation (writes `HEALTH_METRICS.json`),
not a PR correctness gate.
**Replace `on:` with:**
```yaml
on:
  push:
    branches: [main, develop]
    paths:
      - 'marketplace/**'
      - 'crates/ggen-domain/src/marketplace/**'
      - 'crates/ggen-cli/src/cmds/marketplace.rs'
      - '.github/workflows/marketplace-validate.yml'
  workflow_dispatch:
```

### C.3 `helm-validation.yml`
Current: `push:[main,develop]`+paths(`helm/**`) + `pull_request:[main,develop]`+paths.
`helm lint` / template / kubeconform of charts. Infra; should run on merge to main,
not gate every PR.
**Replace `on:` with:**
```yaml
on:
  push:
    branches: [main, develop]
    paths:
      - 'helm/**'
      - '.github/workflows/helm-validation.yml'
  workflow_dispatch:
```

### C.4 `erlang-ci.yml`
Current: `push:[main,develop]`+paths + `pull_request:`+paths(`crates/tps-jidoka/**`,
`crates/tai-erlang-autonomics/**`,`rebar.lock`). Erlang/OTP build+test on a
non-workspace subtree. Out of the Rust gate's scope; keep it operational.
**Replace `on:` with:**
```yaml
on:
  push:
    branches: [main, develop]
    paths:
      - 'crates/tps-jidoka/**'
      - 'crates/tai-erlang-autonomics/**'
      - '.github/workflows/erlang-ci.yml'
      - 'rebar.lock'
  workflow_dispatch:
```
> If Erlang correctness must gate PRs that touch those Erlang crates, this is the one
> RETRIGGER worth reconsidering — it is path-scoped and tests a real, separate
> toolchain. Default decision per contract: move off `pull_request`. **Flag.**

### C.5 `gitops-sync-flux.yml`
Current: `push:[main,develop]`+paths(`infra/flux/**`,`helm/**`) + `workflow_dispatch`.
**No `pull_request` clause already** — but it is operational GitOps. It is **not** in
the 27 PR-gating set, so technically a KEEP; listed here for clarity. Decision:
**KEEP** (already correctly off PRs).
**No change.**

### C.6 `docker-build-push.yml`
Current: `push:[main,develop]` + `tags:['v*']` + `workflow_dispatch`. **No
`pull_request` clause already.** Not in the 27 PR-gating set. Decision: **KEEP**.
**No change.**

> Net of section C: only **4 files actually need a trigger edit**
> (`marketplace-test`, `marketplace-validate`, `helm-validation`, `erlang-ci`).
> `gitops-sync-flux` and `docker-build-push` are already off PRs → KEEP.

---

## D. KEEP — already correct (no change)

These are operational CD/release/deploy/docs workflows already off `pull_request`
(verified: none appears in the 27 PR-gating list). No edit.

| workflow | current triggers (verified) | decision | rationale |
|----------|------------------------------|----------|-----------|
| `release.yml` | `push: tags:['v*']` | **KEEP** | Multi-target release build; tag-only. |
| `release-debian.yml` | `push: tags:['v*.*.*']` | **KEEP** | Debian package on version tags. |
| `homebrew-release.yml` | `release:[published]` | **KEEP** | Updates Homebrew tap on release. |
| `semantic-release.yml` | `push:[main]` + `workflow_dispatch` | **KEEP** | Conventional-commits → SemVer on main. |
| `generate-release-notes.yml` | `release:[published]` + `workflow_dispatch` | **KEEP** | Changelog generation on release. |
| `docker-build-push.yml` | `push:[main,develop]` + `tags:['v*']` + `workflow_dispatch` | **KEEP** | Multi-registry image build; already off PRs. |
| `docker.yml` | `push:[main]` | **KEEP** | DockerHub image on main. |
| `marketplace.yml` | `push:[main]`+paths + `workflow_dispatch` | **KEEP** | Marketplace → GitHub Pages. |
| `marketplace-deploy.yml` | `push:[main]`+paths + `workflow_dispatch` | **KEEP** | Marketplace registry deploy. |
| `marketplace-docs.yml` | `push:[main]`+paths + `workflow_dispatch` | **KEEP** | Marketplace mdBook deploy. |
| `deploy-docs.yml` | `push:[main]`+paths + `workflow_dispatch` | **KEEP** | API docs + mdBook → Pages. |
| `publish-registry.yml` | `push:[main]` + `workflow_dispatch` | **KEEP** | Registry/docs → GitHub Pages. |
| `erlang-release.yml` | `push: tags:['erlang-v*','tps-jidoka-v*','tai-erlang-v*']` | **KEEP** | Erlang release on component tags. |
| `gitops-sync-flux.yml` | `push:[main,develop]`+paths + `workflow_dispatch` | **KEEP** | Flux GitOps sync; already off PRs. |
| `secrets-sync.yml` | `push:[main]`+paths(`infra/gcp/secrets*.tf`) + `workflow_dispatch` | **KEEP** | GCP secrets sync; already off PRs. |
| `automated-rollback.yml` | `workflow_dispatch` + `schedule(*/5 * * * *)` | **KEEP** | Health-check + auto-revert; manual/scheduled. |
| `security-audit.yml` | `push:[main]` + `pull_request:[main]` + `schedule(daily)` + `workflow_dispatch` | **see note** | **PR-gating!** Folded into `quality.yml` `audit`. → Decision below. |
| `security.yml` | `push:[main,develop]` + `pull_request:[main,develop]` + `schedule(daily)` | **see note** | **PR-gating!** Folded into `quality.yml` `audit`. → Decision below. |

> ⚠️ `security.yml` and `security-audit.yml` are **PR-gating** (they have
> `pull_request` clauses) — they do not belong in KEEP. Their real per-PR check
> (`cargo audit` + `cargo deny`) is **absorbed by `quality.yml` `audit`**. Decision:
> **DELETE** both (folded into quality.yml), per contract ("security(+audit) PR
> triggers" listed under DELETE). They are itemized in section E below to keep the
> arithmetic exact.

---

## E. Security workflows — explicit decision (folded into quality.yml)

| workflow | current triggers | decision | rationale | orchestrator action |
|----------|------------------|----------|-----------|---------------------|
| `security.yml` | `push:[main,develop]` + `pull_request:[main,develop]` + `schedule(daily 2AM)` | **DELETE (fold)** | security-tests (`cargo make test-security`), dependency-audit (`cargo audit`), sast (`cargo clippy -D warnings`), supply-chain (`cargo deny check`). The audit+deny+clippy surface is the real check. | audit/deny → **quality.yml** `audit`; clippy → **quality.yml** `clippy`. `rm`. **Real-check risk:** the Redis-service `security-tests` integration job + daily `schedule` are dropped — if security integration tests are real, re-add as a `schedule`/`workflow_dispatch` job (follow-up). **Flag.** |
| `security-audit.yml` | `push:[main]` + `pull_request:[main]` + `schedule(daily 2AM)` + `workflow_dispatch` | **DELETE (fold)** | cargo-lock-verification, comprehensive `cargo audit --json --deny warnings`, SBOM gen, dependency-review, typosquatting detection, clippy-security. | audit → **quality.yml** `audit`. `rm`. **Real-check risk:** SBOM generation + typosquatting + the daily schedule are dropped; valuable but out of frozen PR-gate scope. Recommend a separate `schedule`-only `security-nightly.yml` as follow-up. **Flag.** |

These two raise the **DELETE total from 17 to 19 files** (17 in section B + 2 here).

---

## F. Arithmetic & validation cross-check

### F.1 The 27 PR-gating files — every one has a decision

`grep -lE 'pull_request' .github/workflows/*.yml` (27 files) and their decisions:

| # | PR-gating workflow | decision |
|---|--------------------|----------|
| 1 | `andon-validation.yml` | DELETE (→ ci+quality) |
| 2 | `andon_ci.yml` | DELETE (→ ci+quality) |
| 3 | `build.yml` | DELETE (→ ci) |
| 4 | `ci-complete.yml` | DELETE (→ ci+quality) |
| 5 | `ci-quality-gates.yml` | DELETE (→ ci+quality) |
| 6 | `ci.yml` | **REPLACED** (Agent 3 rewrite — stays PR-gating) |
| 7 | `docker-build-push.yml` | KEEP (no `pull_request` clause — see note) |
| 8 | `docs-validation.yml` | DELETE (→ docs) |
| 9 | `erlang-ci.yml` | RETRIGGER (drop PR) |
| 10 | `erlang-release.yml` | KEEP (tag-only; see note) |
| 11 | `gitops-sync-flux.yml` | KEEP (no `pull_request` clause — see note) |
| 12 | `helm-validation.yml` | RETRIGGER (drop PR) |
| 13 | `lint.yml` | DELETE (→ quality) |
| 14 | `london-tdd-tests.yml` | DELETE (anti-policy; → ci) |
| 15 | `marketplace-test.yml` | RETRIGGER (drop PR) |
| 16 | `marketplace-validate.yml` | RETRIGGER (drop PR) |
| 17 | `pattern-benchmarks.yml` | DELETE (→ quality slo) |
| 18 | `performance.yml` | DELETE (→ quality slo) |
| 19 | `performance_benchmarks.yml` | DELETE (→ quality slo) |
| 20 | `quality-gates.yml` | DELETE (→ ci+quality) |
| 21 | `security-audit.yml` | DELETE (fold → quality audit) |
| 22 | `security.yml` | DELETE (fold → quality audit+clippy) |
| 23 | `test.yml` | DELETE (→ ci) |
| 24 | `toc.yml` | DELETE (→ docs) |
| 25 | `ultra-deploy-test.yml` | DELETE (→ ci) |
| 26 | `validate-docs.yml` | DELETE (→ docs) |
| 27 | `weaver-validation.yml` | DELETE (no new home — flagged) |

> ⚠️ **`grep` artifact note:** rows 7, 10, 11 match the `pull_request` *string* but do
> **not** have an active `pull_request:` trigger clause — `docker-build-push.yml` /
> `erlang-release.yml` / `gitops-sync-flux.yml` mention `pull-requests:` (permissions)
> or `pull_request` only in comments/permissions, not under `on:`. Their real `on:`
> blocks (verified in §C/§D) are tag/push/dispatch only → **KEEP, no trigger edit
> needed.** This is why the effective RETRIGGER set is 4, not 6: only
> `erlang-ci`, `helm-validation`, `marketplace-test`, `marketplace-validate` carry a
> genuine `on: pull_request:` clause among the operational set.

### F.2 Before / after PR-gating count

Two equivalent ways to count, both landing on **4**.

**(a) Against the original 40 (the `grep -l pull_request` = 27 set):**

| | count |
|---|------|
| `grep -l pull_request` matches (original 40) | 27 |
| − grep false-positives (string in comments/permissions/`if:`, **no** `on: pull_request:`) | −3 (docker-build-push, erlang-release, gitops-sync-flux → already off PRs, KEEP) |
| = genuine **legacy** PR-gating | **24** |
| − DELETE (section B = 15 of these + section E = 2 security; remaining 2 of B's 17 = `ci`/none) | see breakdown |
| − RETRIGGER (drop PR clause, keep file) | −4 |
| − rewritten in place (stays PR-gating) | −1 (`ci.yml`) |
| **legacy PR-gating after** | **0** |
| + new PR-gating workflows | +4 (`ci` rewritten, `quality`, `example-tpot2`, `docs`) |
| **PR-gating after** | **4** |

**(b) Against the current 43 (`on:`-scoped active-trigger scan = 27):** the 27 active
`pull_request` triggers = 4 new/rewritten gates + 23 legacy. Delete 19, retrigger 4
→ 0 legacy left → **4** remain.

Exact breakdown of the legacy 23 (= 24 genuine − `ci.yml` rewritten):
- **DELETE, PR-gating (19):** rows 1,2,3,4,5,8,13,14,17,18,19,20,21,22,23,24,25,26,27.
  (17 from section B + `security.yml` + `security-audit.yml` from section E.)
- **RETRIGGER, drop PR (4):** rows 9,12,15,16 (`erlang-ci`, `helm-validation`,
  `marketplace-test`, `marketplace-validate`).
- **grep-false-positive, already off PRs → KEEP (3):** rows 7,10,11.

Check: 4 (stay) + 19 (delete) + 4 (retrigger) + 3 (false-positive keep) = **30**.
The 27 PR-gating set is the 30 minus the 3 false-positives = **27.** ✔

### F.3 File-count before / after

> **Re-sample (2026-06-20):** Agents 4/5 have already created `quality.yml`,
> `example-tpot2.yml`, `docs.yml`, so the directory now holds **43** files, not 40.
> `ci.yml` is rewritten in place (not a new file).

| | count |
|---|------|
| Original legacy workflow files | 40 |
| + new workflows already landed (`quality`, `example-tpot2`, `docs`) | +3 |
| Total workflow files **now** | **43** |
| − DELETE (section B = 17 + section E = 2) | −19 |
| Total workflow files **after teardown** | **24** |

The 24 survivors = 4 PR-gating (`ci` rewritten + `quality`/`example-tpot2`/`docs`) +
20 KEEP/RETRIGGER operational workflows (16 KEEP unchanged + 4 RETRIGGER, file kept).
RETRIGGER edits the `on:` block but does not delete the file.

---

## G. Orchestrator action list (apply in this order)

1. **Confirm** the new workflows exist and are well-formed: `ci.yml` (rewritten),
   `quality.yml`, `example-tpot2.yml`, `docs.yml`, and the action
   `.github/actions/setup-ggen-build/action.yml`. Do **not** delete anything until
   these are present (otherwise the gate is empty).
2. **DELETE these 19 files** (`git rm`):
   `andon-validation.yml`, `andon_ci.yml`, `build.yml`, `ci-complete.yml`,
   `ci-quality-gates.yml`, `docs-validation.yml`, `lint.yml`, `london-tdd-tests.yml`,
   `pattern-benchmarks.yml`, `performance.yml`, `performance_benchmarks.yml`,
   `quality-gates.yml`, `security.yml`, `security-audit.yml`, `test.yml`, `toc.yml`,
   `ultra-deploy-test.yml`, `validate-docs.yml`, `weaver-validation.yml`.
3. **RETRIGGER these 4 files** — replace each `on:` block with the exact YAML in §C:
   `erlang-ci.yml`, `helm-validation.yml`, `marketplace-test.yml`,
   `marketplace-validate.yml`.
4. **KEEP unchanged** (15 operational + the 4 new): `release.yml`,
   `release-debian.yml`, `homebrew-release.yml`, `semantic-release.yml`,
   `generate-release-notes.yml`, `docker-build-push.yml`, `docker.yml`,
   `marketplace.yml`, `marketplace-deploy.yml`, `marketplace-docs.yml`,
   `deploy-docs.yml`, `publish-registry.yml`, `erlang-release.yml`,
   `gitops-sync-flux.yml`, `secrets-sync.yml`, `automated-rollback.yml`.
5. **Branch protection:** set required checks per architecture §4 — require
   `CI Status` (ci.yml aggregator), and recommended `fmt` + `clippy` (quality.yml).
6. **Record follow-ups** for the flagged real-check losses (architecture §5 + this
   doc's flags): integration/BDD/E2E (resolve via `just test` vs `test-lib`),
   LSP/MCP plugin harness, coverage gate, CLI-backed doc-guide validation, OTEL/weaver
   semconv, security SBOM/typosquat/nightly schedule, perf-history schedules.
