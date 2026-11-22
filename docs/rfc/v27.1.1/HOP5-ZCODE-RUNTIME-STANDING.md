# Hop-5 ZCode runtime (Linux materialization) — standing receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `HOP5-ZCODE-RUNTIME-STANDING-27.1.1` |
| Branch | `cursor/v2711-hop5-zcode-runtime-bc54` |
| Companion | [`HOP5-FOUNDRY-LIVE2-STANDING.md`](./HOP5-FOUNDRY-LIVE2-STANDING.md), [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 5, [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) G2-9, [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) |
| Ecospec tip base | `origin/cursor/v2711-hop5-foundry-live2-bc54` @ `68b2f33c7e88d31075277f41ee46f01eed415782` (ancestor `d05a131c` / #910 work) |
| zcode-cli land | `cursor/v2711-hop5-foundry-live2-bc54` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Host | Linux x86_64 cloud VM |
| Status | LEDGER (not path-crown `ALIVE`) |

## How zcode-cli expects runtime

Runtime subject is **`vendor/zcode.cjs`** (not `node_modules`, not bun alone).
Launcher refuses with `ZCode runtime is missing… bun run sync:local` when absent
(`src/launcher.ts`). Gall `--live` injects `createZcodeAgentExecutor` @ `f32df08`
(live2 closed prior `live executor not configured`).

| Path | Mechanism | Linux standing |
|------|-----------|----------------|
| `bun run sync:local` | `scripts/sync-runtime.ts --app $HOME/Applications/ZCode.app` | **UNSUPPORTED** — macOS App + `--app` version discovery requires darwin |
| `bun run sync:locked` | `--lock zcode-runtime.lock.json` (committed **darwin/arm64** 3.14.4 zip) | **UNSUPPORTED** — lock platform ≠ host; wrong-arch artifact |
| `bun run sync` (unlocked) | Service/CDN latest for `linux-x86_64` (observed **3.19.1** `.deb`) | Path **PRESENT**; extract OK; **REFUSED** at required patch `tui-execution-state` (3.19.1 incompatible with tip patches) |
| Tip-matched Linux `--lock` | Hand lock for App **3.14.4** `linux-x64` `.deb` + `bun scripts/sync-runtime.ts --lock …` | **ALIVE** materialization — `vendor/zcode.cjs` **PRESENT** (cliVersion `0.16.9`) |
| `node_modules` / bun | Build/test tooling only | Not a substitute for `vendor/zcode.cjs` |

Committed lock remains darwin for CI pin; Linux tip-matched lock is a **host local**
materialization (vendor gitignored). Do **not** treat darwin `sync:locked` as the
Linux verifier.

## Evidence (this land)

| Probe | Exit / standing |
|-------|-----------------|
| Service manifest `platform=linux-x86_64&channel=1` | **200** — version `3.19.1` includes `.deb` |
| `bun run sync` (unlocked → 3.19.1) | exit **1** — `Required runtime patch tui-execution-state failed` |
| Tip-matched Linux lock sync (3.14.4 `.deb`) | exit **0** — `Prepared zcode-app-cli@3.14.4-1 with 0.16.9` |
| `vendor/zcode.cjs` | **PRESENT** (~14.8 MiB) |
| `bun scripts/powl-foundry-loop.ts --live` | **foundry_exit=1** |
| `liveExecutor` | `createZcodeAgentExecutor` |
| `aggregateStanding` | **REFUSED** |
| Exact failure | `Model access is not configured. Run /login or /setup in zcode, or configure its API key before sending a prompt. No model request was sent.` |
| Prior failure closed | `ZCode runtime is missing` |
| Provider config | `~/.zcode/v2/provider_config.json` **ABSENT** |
| Jobs/C4 | `mix` **ABSENT** |

Receipt: `/tmp/cursor/hop5-zcode-runtime-receipt.md`.

## Standing

| Claim | Standing |
|-------|----------|
| Darwin `sync:locked` / `sync:local` on Linux | **UNSUPPORTED** |
| Linux tip-matched runtime materialization (`vendor/zcode.cjs`) | **ALIVE** (local; not hop-5 crown) |
| Live foundry converge / hop-5 crown | **BLOCKED** / **REFUSED** — provider/model access **ABSENT**; exit **1**; **not** `ALIVE` |
| Jobs/C4 (`mix ggen_igniter.sync --check`) | **BLOCKED** (`mix` ABSENT) |
| Hop-5 aggregate / path / Foundry crown | **PARTIAL_ALIVE** / **PARTIAL** — do **not** invent crown `ALIVE` |

## Exact next verifier (OOR)

```bash
# Out-of-repo zcode-cli — runtime PRESENT; provider credentials required:
cd /agent/repos/zcode-cli   # @ f32df08+; vendor/zcode.cjs PRESENT
# provision model access: zcode /login or /setup, or write ~/.zcode/v2/provider_config.json
bun scripts/powl-foundry-loop.ts --live
# expect: foundry_exit=0 / aggregateStanding=ALIVE for hop-5 live crown

# Jobs/C4 residual (separate):
mix ggen_igniter.sync --check   # → 0 in igniter workspace
```

Antifalsifier: runtime materialization alone ≠ hop-5 / path-crown `ALIVE`;
`foundry_exit=1` with model access ABSENT is typed **BLOCKED**/**REFUSED**, not silent pass.
Do not restate `ZCode runtime is missing` after tip-matched Linux vendor PRESENT.
