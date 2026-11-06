# zd-79 — rung 7 (AoQ gate0 byte-stable replay + freestanding C header compile)

Standing: ALIVE

- **Date**: 2026-11-05
- **Lane**: OI-4 (v26.11.4 open-items register)
- **Subject**: cgap @ /Users/sac/cgap HEAD e4c28118e372d390e2851a4b77ffd2b187bbb10d
  (21 dirty paths, coordinator owns git; no commits made)
- **Definition**: wave-closing-receipt-draft.md:108-110 — rung 7 = `make gate0`
  byte-stable replay + freestanding header compile; prior standing BLOCKED at
  FM-LAW-015 (ea.graph.json absent at cgap root, deterministic exit 2).

## Unblock attempt (premise-rot check)

The registered close conditions were: commit `ea.graph.json` at cgap root, OR
amend the recipe to pass `--skip-ea-admission`. On disk at lane start:

```
sed -n '46,54p' /Users/sac/cgap/Makefile
  → gate0 recipe lines 48 and 50 already read:
    ggen sync run --skip-ea-admission
```

The recipe amendment close condition has landed since the BLOCKED was recorded
(zd-55 disclosed the `--skip-ea-admission` ruling; zd-44's court witnesses the
flag wired CLI→gate). `ea.graph.json` still absent at cgap root — that branch of
the close condition remains open, but it is now irrelevant: the compliant
recipe is the rung-7 gate itself.

## Run (bounded: two full invocations, ~2s each, deadline-checked via timeout 300)

```
cd /Users/sac/cgap && timeout 300 make gate0   → exit 0   (run 1)
cd /Users/sac/cgap && timeout 300 make gate0   → exit 0   (run 2)
cmp /tmp/cgap_gate0_run2.sha /tmp/cgap_gate0_run1.sha → SHA_FILES_IDENTICAL
```

Evidence from run output:

- Both `ggen sync run --skip-ea-admission` passes completed (ggen 26.11.3,
  OTEL spans pipeline.load/extract/validate/generate/emit, `files_generated=0`,
  `written: []` — replay is byte-stable no-op, exactly what rung 7 claims).
- Byte-stable replay proof: `gate0: byte-stable replay proof PASS`, with
  sha256 over the five GATE0_FILES, run-1 vs run-2 files byte-identical:
  - `01d828b3…` include/cgap_c_data.h
  - `bf7f5b54…` include/cgap_format_codes.h
  - `96c67758…` crates/cgap-arrow/src/generated_layout.rs
  - `d2942600…` crates/cgap-arrow/src/generated_version.rs
  - `23865c00…` include/cgap_aoq.h
- Freestanding header compile: `gate0: header compile PASS`
  (`cc -std=c99 -ffreestanding -Wall -Wextra -pedantic -c include/cgap_c_data.h`).

## Standing flips

- rung 7: UNWITNESSED/BLOCKED → **WITNESSED/ALIVE** (both gate0 invocations
  exit 0 on the compliant `--skip-ea-admission` recipe — the exact close
  condition wave-closing-receipt-draft.md falsifier 4 names: "gate0 exits 0 on
  a compliant recipe").
- OI-4 (v26.11.4 open-items-register): BLOCKED → discharged by this receipt
  file landing in docs/jira/v26.11.3/receipts/.

## Residual disclosure

- "stress/chaos" in the OI-4 title is a register-label only; the rung-7
  definition (draft :108) carries no stress/chaos element — no stress leg
  exists to run. This receipt witnesses the rung as defined, not the label.
- Zero FM-LAW-015 refusal observed this session (the flag path is taken, so
  the EA admission gate is not reached). Non-vacuity for the FM-LAW-015 gate
  itself is carried by zd-44's court (no flag → exit 1), not re-run here.
- No git commits made, per contract.
