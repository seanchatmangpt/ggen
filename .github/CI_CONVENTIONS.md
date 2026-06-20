# CI Conventions — ggen (core-team)

Every workflow in `.github/workflows/` follows these conventions. They exist
because the old CI (40 workflows, ~8000 lines) was slow, theatrical, and red for
the wrong reasons. New CI is **minimal, honest, fast**. Enforced structurally by
`scripts/ci/validate-workflows.py`.

## 1. SHA-pin every third-party `uses:`

Never float on `@main` / `@master` / a bare `@vN` tag (a tag is mutable — the
author can re-point it). It broke `validate-docs.yml`, which used
`dtolnay/rust-toolchain@main`. **Every third-party action is pinned to a full
40-character commit SHA, with the human-readable version in a trailing comment:**

```yaml
- uses: actions/checkout@11bd71901bbe5b1630ceea73d27597364c9af683       # v4.2.2
- uses: actions/setup-python@0b93645e9fea7318ecaed2b359559ac225c90a2b   # v5.3.0
- uses: taiki-e/install-action@81ecf985428d5c2ea81dbf079bceca32bc9604ab # v2.62.43
- uses: ./.github/actions/setup-ggen-build   # local action: no @version needed
```

Local composite actions (`./...`) resolve from the repo and need no pin. The
SHA-pinning rule is enforced structurally by `validate-workflows.py`
(`uses_is_sha_pinned`): a `@vN` / `@main` / `@<branch>` ref on a third-party
action is a **FAIL**, named by file and line.

`dtolnay/rust-toolchain`, `taiki-e/install-action`, and `Swatinem/rust-cache`
are **commit-pinned inside** the `setup-ggen-build` action — gating workflows
never call them directly.

## 2. Least-privilege `permissions`

Default-deny at the top level; escalate per-job only when a job genuinely needs
write (e.g. release/deploy).

```yaml
permissions:
  contents: read
```

## 3. `concurrency` with cancel-in-progress

One in-flight run per workflow+ref; superseded pushes are cancelled, saving runners.

```yaml
concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true
```

## 4. `timeout-minutes` on every job

No hung job ever burns the full 6h default.

| Job kind | Budget |
|----------|--------|
| build / test (cargo) | ≤ 30 |
| lint / clippy / fmt   | ≤ 15 |
| python / docs / example | ≤ 5 |

## 5. Path filters — run only when relevant

Gating workflows trigger on PR + push-to-main; scope with `paths:` (or
`paths-ignore:` for the Rust CI on pure-docs changes).

```yaml
on:
  pull_request:
    paths: ['examples/tpot2-wasm4pm-autoconfig/**']
  push:
    branches: [main]
    paths: ['examples/tpot2-wasm4pm-autoconfig/**']
```

## 6. Pinned nightly + `setup-ggen-build` (the cargo rule)

Any job that runs **cargo** MUST use the shared composite action
`./.github/actions/setup-ggen-build`. Two root causes it fixes:

- **Siblings (pinned to exact SHAs).** The workspace `[patch.crates-io]` points
  `lsp-max` / `lsp-types-max` / `wasm4pm` / `wasm4pm-compat` at `../<repo>`. The
  action fetches all four into `$GITHUB_WORKSPACE/..` so the workspace loads.
  These siblings are **untagged WIP repos whose default HEAD moves**, so a plain
  `git clone --depth 1` would grab a different revision on every run —
  non-reproducible builds. The action instead `fetch --depth 1`-es an **exact
  commit SHA** per sibling and checks out `FETCH_HEAD`:

  | sibling | pinned SHA |
  |---------|-----------|
  | `lsp-max` | `7bcc1e16dec71ef5fb2cedea2dfd6cb5cde37f59` |
  | `lsp-types-max` | `6773e6017ca83c565785d0ec39d75304f62c3237` |
  | `wasm4pm` | `0bb134b29245517ac4969a9f1916f5432931c5d0` |
  | `wasm4pm-compat` | `e46155e209a750fda0218532d96ae17a9e10903e` |

  **How to bump a sibling:** change its SHA in the action's `shas` map AND
  re-verify the consuming code compiles against the new revision (in particular,
  bumping `lsp-max` must be paired with re-checking the `ggen-lsp` trio — see §8
  and `CI_ARCHITECTURE.md`). A floating `git clone --depth 1 "$org/$repo"` is a
  **FAIL** in `validate-workflows.py` (`check_sibling_pinning`).
- **Toolchain.** `rust-toolchain.toml` pins `nightly-2026-04-15`. This is the
  ONLY channel that compiles the workspace — there is **no stable/beta matrix**
  (it would silently run nightly anyway, then fail). Build on the pinned nightly.

```yaml
- uses: ./.github/actions/setup-ggen-build
  with:
    components: rustfmt,clippy        # optional
    cache-key-suffix: test            # optional
```

Cargo-free workflows (example, docs) MUST NOT pull in the action — keep them
instant. `actions/setup-python@v5` with `python-version: '3.11'` instead.

## 7. No `continue-on-error` to mask failures

`continue-on-error: nightly` is exactly how the old matrix hid the real nightly
build failure. Fail honestly. The only acceptable non-blocking job is one that is
explicitly documented as advisory (e.g. an SLO check needing a build budget) —
and it must say so in a comment.

## 8. Required gate is deliverable-scoped; the lsp trio is advisory

The required PR gate builds the **shippable deliverable** = the workspace
**excluding** the lsp trio (`ggen-lsp`, `ggen-lsp-mcp`, `ggen-lsp-a2a`). Those
three are **leaf crates**: nothing depends on them by default (`ggen-cli` pulls
them in only behind the off-by-default `lsp` feature), and they currently fail to
compile (51 errors) against the **untagged, floating-HEAD** external sibling
`lsp-max` v26.6.18. Gating the entire product on a moving external target nobody
controls is the anti-pattern, so the required gate is scoped to the deliverable
and the trio is surfaced **honestly** in a separate, non-required job.

**Required status checks** (mark these "required" in branch protection on `main`):

| Required check | Workflow / job | Gates on |
|----------------|----------------|----------|
| `Check`     | `ci.yml` / `check`     | `cargo check` (deliverable scope) |
| `Build`     | `ci.yml` / `build`     | `cargo build` (deliverable scope) |
| `Test`      | `ci.yml` / `test`      | `cargo test --lib` (deliverable scope) |
| `Doctest`   | `ci.yml` / `doctest`   | `cargo test --doc` (deliverable scope) |
| `CI Status` | `ci.yml` / `ci-status` | aggregate of the four above |

`ci-status` `needs:` the four gating jobs and is the stable aggregate — adding or
renaming a gating job updates `ci-status`, not the protection rule. The four
sub-jobs are listed as required too so a single failure is legible at a glance
without expanding the aggregate.

```yaml
ci-status:
  name: CI Status
  needs: [check, build, test, doctest]   # NOTE: lsp-crates is deliberately absent
  if: always()
  runs-on: ubuntu-latest
  steps:
    - name: Assert all gating jobs succeeded
      run: |
        # fail if any need did not succeed
        ...
```

**Advisory `lsp-crates` job (NON-required).** `ci.yml` also carries an advisory
`lsp-crates` job that runs `cargo check -p ggen-lsp -p ggen-lsp-mcp -p
ggen-lsp-a2a`. It shows the trio's **true** status (currently red) — **no
`continue-on-error` masking** — and is deliberately **absent from
`ci-status.needs`**, so its failure cannot block a PR or the merge queue.
`validate-workflows.py` (`check_advisory_isolation`) enforces that `lsp-crates`
never appears in `ci-status.needs`.

**Promotion criteria:** move `lsp-crates` into `ci-status.needs` (making it a
required gate) once `ggen-lsp` tracks a **pinned** `lsp-max` SHA (see §6) and
compiles cleanly against it. Advisory clippy/audit/slo and the path-filtered
`example-tpot2` / `docs` workflows likewise stay non-required (run for signal,
do not block).

## 8b. Merge-queue support

The gating workflows (`ci.yml`, `quality.yml`) trigger on `merge_group` in
addition to `pull_request` + `push: [main]`. Required status checks must also run
in the merge queue, or a PR that was green at PR time could break `main` once
batched. Keep the merge-queue triggers in lockstep with the PR triggers.

```yaml
on:
  pull_request:
  push:
    branches: [main]
  merge_group:
```

## 9. Stable, descriptive `name:`s

Every workflow has a top-level `name:`; every job has a clear `name:`. These are
the strings that appear in branch protection and PR checks — keep them stable so
the required-check name does not drift.

## 10. Local validation (no actionlint)

actionlint/yamllint are not assumed present. Before pushing workflow changes:

```bash
python3 scripts/ci/validate-workflows.py
```

Stdlib-only (PyYAML used if importable, line-based fallback otherwise; no
external deps). It runs **per-file** checks on every `*.yml` it globs:

- `name:` / `on:` / `jobs:` present;
- every `uses:` is version-pinned (`uses_is_pinned`); and
- every **third-party** `uses:` is pinned to a full 40-char commit SHA
  (`uses_is_sha_pinned`) — bare `@vN` / `@main` is a FAIL (§1).

…plus three **repo-wide** checks:

- `check_pr_trigger_scope` — only the 4 core workflows (`ci.yml`, `quality.yml`,
  `docs.yml`, `example-tpot2.yml`) may carry an `on: pull_request:` trigger.
  String mentions of `pull_request` in a job `if:` / `github.event_name` guard
  are **not** triggers and are ignored — only the `on:` block is inspected.
- `check_advisory_isolation` — `ci.yml`'s `ci-status` job must **not** `needs:`
  the advisory `lsp-crates` job (§8).
- `check_sibling_pinning` — `setup-ggen-build/action.yml` must pin all four
  sibling SHAs and contain no floating `git clone --depth 1` (§6).

The check set is derived by globbing the workflows dir (no hardcoded file count),
so it stays correct as workflows are added or removed.
