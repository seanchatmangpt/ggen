# CI Conventions — ggen (core-team)

Every workflow in `.github/workflows/` follows these conventions. They exist
because the old CI (40 workflows, ~8000 lines) was slow, theatrical, and red for
the wrong reasons. New CI is **minimal, honest, fast**. Enforced structurally by
`scripts/ci/validate-workflows.py`.

## 1. Pin every `uses:` to a version

Never float on `@main` / `@master` (it broke `validate-docs.yml`, which used
`dtolnay/rust-toolchain@main`). Pin to a tag at minimum; SHA-pin where practical.

```yaml
- uses: actions/checkout@v4
- uses: actions/setup-python@v5
- uses: actions/cache@v4
- uses: Swatinem/rust-cache@v2
- uses: technote-space/toc-generator@v4
- uses: ./.github/actions/setup-ggen-build   # local action: no @version needed
```

`dtolnay/rust-toolchain` is **commit-pinned inside** the `setup-ggen-build`
action — workflows never call it directly.

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

- **Siblings.** The workspace `[patch.crates-io]` points `lsp-max` /
  `lsp-types-max` / `wasm4pm` / `wasm4pm-compat` at `../<repo>`. The action clones
  all four into `$GITHUB_WORKSPACE/..` so the workspace loads.
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

## 8. One required status check: `ci-status`

`ci.yml` ends with an aggregator job `ci-status` that `needs:` all gating jobs.
**Branch protection requires only `ci-status`** — not each individual job. Adding
or renaming a gating job updates `ci-status`, not the protection rule.

```yaml
ci-status:
  name: CI Status
  needs: [check, build, test, doctest]
  if: always()
  runs-on: ubuntu-latest
  steps:
    - name: Verify all gating jobs succeeded
      run: |
        # fail if any need did not succeed
        ...
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

It checks every workflow has `name:` / `on:` / `jobs:` and that every `uses:` is
version-pinned, using PyYAML if available and a line-based fallback otherwise.
