# ZD-39: polyglot job build step still dies on sccache wrapper (ZB-3)

Standing: FIXED (uncommitted, per lane contract)

- Date: 2026-11-04
- Subject: `.github/workflows/cyberpunk-tv-platform.yml`, job `manufacture-execute-replay`
  ("Manufacture, polyglot Chicago TDD, receipt, replay"), branch `v26-11-3-wave` @ 58a3b4108
- Status: FIXED (uncommitted, per lane contract)

## Symptom (ZB-3)

Job fails at "Build exact ggen binary":

```
error: could not execute process 'sccache .../rustc -vV' (never executed) ... No such file or directory
```

## Diagnosis (what was already true at HEAD before this receipt)

- ZD-5's "Install sccache" step **already exists at HEAD** (landed in
  bda447441, a verified ancestor of 58a3b4108 via `git merge-base --is-ancestor`),
  at line 53, BEFORE any cargo invocation ("Orient" runs only rustup/npm-version
  checks; first cargo is the wasm-pack fallback, then the build at line 73).
  So ZD-5's ordering was already correct — there was no ordering bug to fix.
- Root dependency: `.cargo/config.toml` sets `[build] rustc-wrapper = "sccache"`
  **unconditionally**. Every cargo invocation in the workspace execs a wrapper
  unless neutralized, so every workflow must either have sccache on PATH or
  kill the wrapper. The cyberpunk job was the only heavy-build job left doing
  the former via a dedicated install step.
- Majority pattern (verified by grep over `.github/workflows/*.yml`): 11+
  sibling workflows (ggen-lsp-runtime-crown, live-examples, tcps-generated-validation,
  architecture-autonomics, cell4-ggen-ecosystem-ocel, examples-repair-constructor,
  examples-core-team-constructor, receipt-chain-migration-constructor,
  measure-r52-epistemic-consumer, verify-v26-8-1-source-removal,
  ecosystem-alive-generated-command-format) neutralize the wrapper instead:
  `RUSTC_WRAPPER: ""` (+ `SCCACHE_DISABLE: "1"` in ggen-lsp-runtime-crown.yml).
  Env `RUSTC_WRAPPER` overrides `build.rustc-wrapper` at the cargo config layer
  and empty = no wrapper; `SCCACHE_DISABLE=1` belts against direct sccache exec.

## Fix chosen

Removed ZD-5's "Install sccache" step and added job-level env:

```yaml
env:
  RUSTC_WRAPPER: ""
  SCCACHE_DISABLE: "1"
```

## Rationale (why majority, not ZD-5's install)

- **Fix already landed and still reported failing** → the wrapper-on-PATH
  dependency itself is the fragile variable; the majority pattern removes the
  dependency entirely instead of re-provisioning the binary.
- **ZD-5's caching rationale does not hold here**: sccache on an ephemeral
  `ubuntu-latest` runner with no `sccache-action` GHA cache backend configured
  (verified: no workflow in this repo sets `SCCACHE_GHA_ENABLED` except
  verify-v26-8-1-source-removal.yml setting it to `false`) caches within-run
  only — no cross-run benefit for a one-shot 90-minute job. ZD-5's "worst
  place to lose sccache" premise was wrong for this topology.
- **Consistency**: identical env wiring to 11 proven sibling workflows that
  build the same workspace successfully without sccache.

## Verification

- YAML parsed clean: `python3 -c "import yaml; yaml.safe_load(...)"` → OK.
- `bash -n` on all 9 embedded `run:` scripts → all OK (via python harness,
  exit 0).
- Job env confirmed on disk: `{'RUSTC_WRAPPER': '', 'SCCACHE_DISABLE': '1'}`.
- No cargo invocation precedes the env (job-level env applies to every step).
- Full job execution NOT run (no runner available locally); first
  push-triggered run is the real gate. Residual risk (carried from ZD-5's
  receipt, unchanged): the job hand-rolls toolchain setup instead of consuming
  `.github/actions/setup-ggen-build` STEP 1 sibling-repo provisioning; if the
  build fails next on `failed to read ../lsp-max/Cargo.toml`, that is the same
  provisioning class and is out of this lane's scope.

## Tradeoffs / notes

- Delete outranks generate: one install step (dead weight once the wrapper is
  neutralized) removed; nothing generated.
- This supersedes ZD-5's fix for this workflow (its install step is removed);
  the composite-action STEP 2a install stays untouched for the 35+ workflows
  that legitimately route through it.
