# Construct byte-clean — exclude `.ggen-v2` receipt churn (v27.1.1)

| Field | Value |
|-------|-------|
| Spec binding | Construct core-team / repair example closure |
| Tip (land) | crown `7bb443e99` (receipt-exclude ⊇ lock sync; prior `b07cda77c` / `50cfb1280`) |
| Status | **IMPLEMENTED** in-tree; local + CI validate-examples **55/55 EXIT 0** **ALIVE** (run [`45078869386`](https://github.com/seanchatmangpt/ggen/actions/runs/45078869386)); residual `npm ci` lock drift fixed @ `7bb443e99`; full Construct job **PENDING_CI** on tip |

## Finding

Emit-before-sync (`d8cb014f0`) closed FM-PACK-013 for `tcps-generated`, so Construct
reached the byte-identical projection gate for the first time in this settle window.

Check-mode then refused on ~92 tracked `examples/**/.ggen-v2/{receipt,receipt-log,lease}*`
paths. Those files change every sync by design (generation bump, chain append) — the same
class already excluded by `.github/workflows/publish-candidate.yml` `DRIFT_PATHSPEC` and
documented for tcps under ZD-135.

## Fix

`scripts/validate-examples.sh` check-mode now uses:

```text
examples/
:(exclude)**/.ggen-v2/**
:(exclude)examples/tcps-generated/evidence/**
:(exclude)examples/tcps-generated/receipts/**
```

Source projection drift (e.g. `src/*.py`) still fails closed. Receipt-only dirtiness is ignored.

## Local proof (2026-11-22 2:10–2:20 PM PST)

| Step | Result |
|------|--------|
| `ggen sync run --skip-ea-admission` ×2 in `examples/all-modules-coverage` | EXIT **0** / EXIT **0** |
| `git diff --quiet` with DRIFT_PATHSPEC (excludes `**/.ggen-v2/**`) | EXIT **0** |
| Same tree without exclude (`.ggen-v2` only) | EXIT **1** (receipt/log dirty — expected) |
| `GGEN_BIN=target/debug/ggen scripts/validate-examples.sh` (check mode) | **ALIVE: 55/55**; EXIT **0** (`/tmp/local-construct-check.log`; ~48s) |

Local Construct **55/55 ALIVE**. CI validate-examples on tip `b07cda77c` also **55/55 EXIT 0** (run `45078869386`), then failed at Interview Assist `npm ci` (lock `next@16.4.5`/`vitest@5.1.0` vs `package.json` `next@16.2.11`/`vitest@^2.1.9`). Lock regenerated with `npm install --package-lock-only` → tip-wins `7bb443e99`. Full Construct job **PENDING_CI** on `7bb443e99` (run `45081862735`).

## Refuse

- Inventing full Construct CI `ALIVE` before core-team + repair EXIT **0** on tip `7bb443e99+`.
- Equating receipt-exclude / 55/55 / lock sync with Foundry / cos / revops / WebAuthn `ALIVE`.
