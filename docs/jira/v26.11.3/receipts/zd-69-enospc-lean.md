# ZD-69 — ENOSPC × 3 + Lean verify manifest failure

Standing: FIXED (uncommitted, working tree)

Status: FIXED (uncommitted, working tree)
Lane: ZB-13 item 6, 2026-11-04
Subject: branch `v26-11-3-wave`, exact subject = PR #824 head at edit time
(41816929907 et al. all ran against the same PR head).

## Failure evidence (freshest runs, 2026-11-04 ~10:51Z)

| workflow | run | job | evidence |
|---|---|---|--|
| Examples core-team constructor (`examples-core-team-constructor.yml`) | 41816929907 | Construct core-team example closure | annotation: `System.IO.IOException: No space left on device : '/home/runner/actions-runner/cached/2.338.2/_diag/Worker_20261104-105233-utc.log'`; job died at 9m49s mid "Repair receipt..." step |
| GALL-001 Replay Court (`gall-001-replay.yml`) | 41816929778 | Exact-head clean replay | annotation: `Unhandled exception. System.IO.IOException: No space left on device : '.../Worker_20261104-105213-utc.log'`; died at 23m39s in "Observe bounded ggen-engine suite" |
| CI (`ci.yml`) | 41816929805 | Deep / integration_deep | `collect2: fatal error: ld terminated with signal 7 [Bus error], core dumped`, `error: couldn't create a temp dir: No space left on device (os error 28)`, `rustc-LLVM ERROR: IO failure on output stream: No space left on device` |
| Crown Conjecture Lean (`crown-lean` workflow) | 41816929733 | verify | step `Install Lean and compile once` (lean-action, `build: true`, continue-on-error): `##[error]No lake-manifest.json found. Run lake update to generate manifest`; job later cancelled (SIGTERM via cancel-in-progress on a newer head), NOT a YAML defect |

## Fixes (4 files, uncommitted)

1. **Disk hygiene** — identical minimal block added before the heavy build in
   `examples-core-team-constructor.yml` (before toolchain install) and
   `gall-001-replay.yml` (before toolchain install). In `ci.yml`, the existing
   `Reclaim runner space` step (integration_deep lane only) was upgraded to the
   same block with `df -h /` before/after for log observability. Block:
   `df -h /` / `sudo rm -rf /usr/share/dotnet /opt/ghc /usr/local/lib/android || true` / `df -h /`.
   This is the repo-standard pattern (ci.yml's own reclaim step, generalized).
2. **Lean verify** — the failing lean-action step (`Install Lean and compile
   once`) had `build: true` and errored on the absent `lake-manifest.json`
   (manifest is not committed; `lakefile.lean` requires mathlib v4.30.0).
   The later "Re-run build with persistent receipt" step already runs
   `lake update` → `lake exe cache get` → `lake build`, so the installer step
   was changed to `build: false` and renamed "Install Lean". Single real build
   now happens with the manifest generated in-job.

Note on the manifest: committing `lake-manifest.json` was preferred per task
brief, but no manifest exists anywhere in the tree and minting one locally
would require fetching full Mathlib — the in-job `lake update` path was chosen
instead. If reproducibility of the mathlib resolution is later required,
generate the manifest in a Lean-enabled environment and commit it.

## Validation

- `python3 yaml.safe_load` on all 4 files: OK
  (jobs: construct / replay / admission+deep+ci-status / verify).
- No commits made, per task brief. Fixes land with the coordinator's commit.

## Falsifier

The next PR-#824 push reruns all four: core-team and exact-head replay jobs
should survive past their previous death points; integration_deep should not
reproduce `os error 28`; Crown Conjecture Lean `Install Lean` step should be
green with `build: false` and `Re-run build with persistent receipt` green.
