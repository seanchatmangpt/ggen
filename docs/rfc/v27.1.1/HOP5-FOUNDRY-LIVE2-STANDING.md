# Hop-5 foundry `--live` (live2) — standing receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `HOP5-FOUNDRY-LIVE2-STANDING-27.1.1` |
| Branch | `cursor/v2711-hop5-foundry-live2-bc54` |
| Companion | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 5, [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md), [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md), [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) |
| Ecospec tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `d05a131c7521d5a6325ade2e8295fee650659d5a` (prior pin ~`88e84d545` / mix land `28c3f4d84`) |
| zcode-cli land | `cursor/v2711-hop5-foundry-live2-bc54` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Status | LEDGER (not path-crown `ALIVE`) |

## Diagnosis (prior live1)

Gall spine `scripts/powl-foundry-loop.ts --live` injected **no** `createZcodeAgentExecutor`
→ default executor → `live executor not configured (SUBJECT_MISSING)` →
`aggregateStanding=REFUSED` / process exit **1** (ENOENT overlay after lifecycle
rollback wiped `out/`). Jobs/C4 (`mix ggen_igniter.sync --check`) remains a
separate residual (`mix` **ABSENT** on this host).

## Fix (zcode-cli)

| Change | Path |
|--------|------|
| Inject `createZcodeAgentExecutor` on `--live` (parity with `zcode foundry --live`) | `scripts/powl-foundry-loop.ts` |
| Ensure live candidates have `targetFile` (write-scope fail-closed) | same |
| `sandbox: dryRun`; remkdir `out/` before summary; external logDir | same |
| Remkdir `logDir` per transition after transactional rollback | `src/adapters/zcode-executor.ts` |

## Evidence (this land)

| Probe | Exit / standing |
|-------|-----------------|
| `layout_court fixtures/ui-layout-court/product-subject.json` | **0** (prebuilt on PATH) |
| Default adapter (mix-SUBJECT land) | **0** — see [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) |
| `bun scripts/powl-foundry-loop.ts --live` @ zcode `f32df08` | **foundry_exit=1** |
| `liveExecutor` | `createZcodeAgentExecutor` (configured — prior SUBJECT_MISSING closed) |
| `aggregateStanding` | **REFUSED** |
| Exact failure (live2) | `ZCode runtime is missing. Reinstall the package or run bun run sync:local` |
| Runtime lock (committed) | `zcode-runtime.lock.json` = **darwin/arm64** → Linux **UNSUPPORTED** for `sync:locked` / `sync:local` |
| Jobs/C4 | `mix` **ABSENT** (Gall spine `ui_layout` disabled; igniter check stubbed dry-run only) |

Receipt: `/tmp/cursor/hop5-foundry-live2-receipt.md`.

**Successor (runtime land):** Linux tip-matched `.deb` materialization closed runtime ABSENT;
`--live` now fails on provider/model access — see [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md).

## Standing

| Claim | Standing |
|-------|----------|
| Live executor **configured** in Gall `--live` | **ALIVE** (injection present @ `f32df08`) |
| Live foundry converge / hop-5 crown (live2 snapshot) | **BLOCKED** / **REFUSED** — ZCode runtime **ABSENT** at live2; exit **1**; **not** `ALIVE` |
| Jobs/C4 (`mix ggen_igniter.sync --check`) | **BLOCKED** (`mix` ABSENT) |
| Hop-5 aggregate / path / Foundry crown | **PARTIAL_ALIVE** / **PARTIAL** — do **not** invent crown `ALIVE` |

## Exact next verifier

Superseded by [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md):

```bash
# Runtime PRESENT on Linux (tip-matched lock); provider credentials required:
cd /agent/repos/zcode-cli   # @ f32df08+; vendor/zcode.cjs PRESENT
bun scripts/powl-foundry-loop.ts --live
# expect: foundry_exit=0 after model access configured

# Jobs/C4 residual (separate):
mix ggen_igniter.sync --check   # → 0 in igniter workspace
```

Antifalsifier: executor injection alone ≠ hop-5 / path-crown `ALIVE`;
`foundry_exit=1` with runtime ABSENT (live2) or model access ABSENT (runtime land)
is typed **BLOCKED**/**REFUSED**, not silent pass.
