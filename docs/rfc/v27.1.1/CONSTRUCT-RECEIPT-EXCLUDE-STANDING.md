# Construct byte-clean — exclude `.ggen-v2` receipt churn (v27.1.1)

| Field | Value |
|-------|-------|
| Spec binding | Construct core-team / repair example closure |
| Tip (land) | crown `50cfb1280` (receipt-exclude ⊇ `c0be492e1`) |
| Status | **IMPLEMENTED** in-tree; local validate-examples **55/55 EXIT 0** **ALIVE**; CI Construct **PENDING_CI** until EXIT **0** |

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

Local Construct **55/55 ALIVE** ≠ crown Construct CI `ALIVE` (still **PENDING_CI** on tip `50cfb1280` — GitHub Actions job not yet started).

## Refuse

- Inventing Construct CI `ALIVE` before core-team + repair EXIT **0** on tip.
- Equating this exclude with Foundry / cos / revops / WebAuthn `ALIVE`.
