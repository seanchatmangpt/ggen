# ZD-5: cyberpunk-tv-platform workflow fails — sccache referenced but never installed

Standing: UNVERIFIED

- Date: 2026-11-04
- Subject: `.github/workflows/cyberpunk-tv-platform.yml`, job `manufacture-execute-replay` ("Manufacture, polyglot Chicago TDD, receipt, replay"), branch `v26-11-3-wave`
- Status: FIXED (uncommitted, per lane contract)

## Symptom

Job fails at "Build exact ggen binary" with:

```
error: could not execute process 'sccache .../rustc -vV' (never executed)
... No such file or directory
```

## Root cause

`.cargo/config.toml` sets `[build] rustc-wrapper = "sccache"` **unconditionally**.
The cyberpunk workflow provisions its own toolchain (`rustup toolchain install
nightly-2026-06-22`) but never installs sccache — every cargo invocation, including
`cargo build -p ggen-cli-lib --bin ggen`, dies the moment cargo tries to exec the
wrapper. This is XA-1 root cause 4; the same failure class was already root-caused
for release.yml (PR #255 / #426) and closed there by routing through
`.github/actions/setup-ggen-build` STEP 2a (taiki-e install-action, prebuilt sccache).

## Fix chosen

Added an "Install sccache" step to the job, immediately before "Install pinned
wasm-pack":

```yaml
- name: Install sccache
  uses: taiki-e/install-action@81ecf985428d5c2ea81dbf079bceca32bc9604ab # v2.62.43
  with:
    tool: sccache
```

## Rationale

- **Install beats remove**: the sccache wrapper is workspace-wide policy
  (`.cargo/config.toml`); removing it for this one job would require a
  `CARGO_BUILD_RUSTC_WRAPPER=""` override, fork config from every other CI job,
  and give up cross-run compilation caching for a 90-minute polyglot build job —
  the worst place to lose sccache.
- **Copied, not invented**: taiki-e install-action fetches a prebuilt binary
  (no `cargo install` compile delay), pinned to the *same commit SHA*
  (`81ecf985428d5c2ea81dbf079bceca32bc9604ab`, v2.62.43) already used repo-wide
  by the proven `setup-ggen-build` composite action consumed by 35+ workflows.
  `mozilla-actions/sccache-action` was considered per the task brief but no
  sibling workflow in this repo uses it; the local proven pattern wins on
  consistency.
- The step sits after toolchain orientation and before any cargo step, so no
  cargo invocation can precede the wrapper being on PATH.

## Verification

- YAML parsed clean: `python3 -c "import yaml; yaml.safe_load(...)"` → OK.
- Pattern identity with the repo's proven install confirmed by diffing the pinned
  SHA against `.github/actions/setup-ggen-build/action.yml` STEP 2a (identical).
- Full job execution NOT run (no runner available locally); first push-triggered
  run is the real gate. The fix removes the known-fatal condition (wrapper binary
  absent); remaining risk is pre-existing and out of this lane's scope (e.g. the
  job also does not provision the `[patch.crates-io]` sibling repos that
  setup-ggen-build STEP 1 provides — flagged here as a possible follow-on
  failure of the same provisioning class, not fixed in this receipt).

## Tradeoffs / notes

- No perf tradeoff taken: sccache retained, cold-run cost is one small prebuilt
  binary download (~seconds).
- Follow-on risk (not this receipt's fix): sibling-repo provisioning gap — this
  workflow hand-rolls toolchain setup instead of consuming
  `.github/actions/setup-ggen-build`; if the build fails next on
  `failed to read ../lsp-max/Cargo.toml`, the honest fix is switching the job to
  the composite action (its wasm-pack/Node-version steps would need reconciling).
