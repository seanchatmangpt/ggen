# Construct byte-clean — exclude `.ggen-v2` receipt churn (v27.1.1)

| Field | Value |
|-------|-------|
| Spec binding | Construct core-team / repair example closure |
| Tip (land) | `cursor/v2711-construct-receipt-exclude-bc54` → crown tip-wins |
| Status | **IMPLEMENTED** in-tree; CI Construct **PENDING_CI** until EXIT **0** |

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

## Refuse

- Inventing Construct CI `ALIVE` before core-team + repair EXIT **0** on tip.
- Equating this exclude with Foundry / cos / revops / WebAuthn `ALIVE`.
