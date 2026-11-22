# Founder HITL `--live` — exact next verifier pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `HITL-LIVE-NEXT-VERIFIER-STANDING-27.1.1` |
| Branch | tip-wins absorb `cursor/v2711-hitl-live-next-bc54` @ `723c3a14f` onto crown `00b53cec9` via `cursor/v2711-hitl-live-absorb-8df9` (⊇ Smalt residual mix `00b53cec9` + OOR retip12 `79b64496b` + affidavit `7ddfd8b6e` + BRCE V8+R01 `1dbf1cc2e` + VW prod `fe72be0fb` + Smalt GA residual `9173c6529` + CASTLE wire — **not** dropped) |
| Companion | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) §1a / §2, [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md), [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md), [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) **G2-9**, [`CROWN-PROGRESS-RECEIPT.md`](./CROWN-PROGRESS-RECEIPT.md) |
| Observed ggen tip (absorb base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `00b53cec9134e97cc2967bfd61af9a156e7defa7` (Smalt residual mix courts absorb after OOR retip12 / affidavit / BRCE V8+R01 / VW / CASTLE) |
| Side-branch evidence | `cursor/v2711-hitl-live-next-bc54` @ `723c3a14f5232db9e40c21858a6c41b2b1fa67c8` (`bc-ea2b3d10`); side base was `fe72be0fb` |
| zcode-cli land | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` (Gall `--live` injects `createZcodeAgentExecutor`; Linux tip-matched `vendor/zcode.cjs` **PRESENT**) |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty absorb) |
| Status | LEDGER — **BLOCKED_HITL**; **not** hop-5 / Foundry / zero-HITL `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document pins the **exact Founder HITL next verifier** for foundry `--live` (OOR **G2-9**) so crown progress cites one closed command set. It does **not** rewrite Gate-1 / OOR tip-pin headers. It does **not** promote runtime **PRESENT**, adapter exit **0**, dry-run **PARTIAL**, or in-repo bounded-refusal tip-cargo to hop-5 / Foundry crown `ALIVE`. **Do not invent `--live` `ALIVE`.**

---

## 1. Standing pin (honesty)

| Claim | Standing | Evidence (held; not re-run this absorb) |
|-------|----------|---------------------------------------|
| Linux tip-matched runtime (`vendor/zcode.cjs`) | **PRESENT** / prior materialization **ALIVE** (≠ crown) | [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md) @ zcode `f32df08` |
| Gall `--live` executor injection | **ALIVE** (configured) | `createZcodeAgentExecutor` @ `f32df08` |
| G2-8 layout adapter / `layout_court` | exit **0** (≠ hop-5 crown) | [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) |
| In-repo zcode POWL/OCEL refuse path | tip-cargo **ALIVE** (sjira **6** / bound **4** @ `cd7c2433b`) | ≠ `--live` |
| Provider registry `~/.zcode/v2/provider_config.json` | **ABSENT** | [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md) |
| Host usable provider env names | **ABSENT** | same |
| Admitted `--live` fixture / dry-run bypass | **REFUSED** | dry-run ceiling **PARTIAL** ≠ live |
| **`bun scripts/powl-foundry-loop.ts --live`** | **BLOCKED_HITL** / **REFUSED** | **foundry_exit=1**; `Model access is not configured. Run /login or /setup in zcode, or configure its API key before sending a prompt. No model request was sent.` |
| Hop-5 live / path crown | **PARTIAL_ALIVE** / **PARTIAL** | do **not** invent `ALIVE` |
| Foundry / zero-HITL crown | **PARTIAL** | founder HITL still required for `--live` (among cos DB / WebAuthn V8) |

---

## 2. Owner / HITL

| Role | Owner |
|------|-------|
| Exact next live converge | **Founder HITL** — provision model access on the zcode host (agent cannot invent Cursor secret **values**) |
| Execution surface | **zcode-cli** (`powl-foundry-loop` Gall spine @ `f32df08+`) |
| Contract / refuse-path fence (non-substitute) | **ggen** (`zcode_powl_ocel_court` / `remediation_bound`) |
| OOR row | **G2-9** ([`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md)) |
| Spine ledger | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) zcode-cli / §1a `foundry --live` |

---

## 3. Exact next verifier (absorb text)

**Falsifier for hop-5 live crown:** the command below must exit **0** (`foundry_exit=0`). Until then standing stays **BLOCKED_HITL** — **not** `ALIVE`.

```bash
# Founder HITL — provision real credentials first (pick ONE path):
#   A) Interactive: zcode → /login (Coding Plan API key on Linux) or /setup
#   B) Materialize ~/.zcode/v2/provider_config.json (schemaVersion 1) with
#      non-empty config.providerConfigRules.providerRules[].config.access.apiKey
#      + enabled=true + visibility≠hidden + access.type ∈ {api-key,zhipu-coding-plan-api-key}
#      + config.defaultModelSelection.{providerId,modelId}
#      (template: /agent/repos/zcode-cli/provider.example.json → install -m 600 …)
#   C) Export a non-empty preflight env for this shell only (names; Founder sets values):
#      ANTHROPIC_API_KEY | ANTHROPIC_AUTH_TOKEN | ZCODE_API_KEY | ZCODE_MODEL_MAIN
#      (or other /^(?:ZCODE|ZAI|BIGMODEL|ZHIPU|ANTHROPIC)_.*(?:KEY|TOKEN|MODEL|CONFIG|PROVIDER|BASE_URL)/)
#      NOTE: OPENAI_API_KEY alone does NOT clear missingCodingPlanKey when registry ABSENT
#
# Then re-run live foundry (zcode-cli @ f32df08+; vendor/zcode.cjs PRESENT):
cd /agent/repos/zcode-cli
bun scripts/powl-foundry-loop.ts --live
# FALSIFIER / pass: foundry_exit=0  (process exit 0)
# Until then: BLOCKED_HITL / REFUSED — Model access is not configured
# Antifalsifiers:
#   - dry-run / default powl-foundry-loop PARTIAL ≠ live ALIVE
#   - empty registry / fixture writeProviderFixture ≠ live ALIVE
#   - runtime PRESENT alone ≠ hop-5 / Foundry crown ALIVE
#   - in-repo zcode_powl_ocel_court tip-cargo ≠ --live ALIVE
```

Keys / JSON shape detail: [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md) §1–§2.
Runtime PRESENT residual (closed): [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md).

Jobs/C4 residual is **separate** (not this falsifier): admitted igniter pack `--check` → **0** ([`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)).

---

## 4. Explicit non-claims

- This absorb does **not** promote hop-5 live, path crown, Foundry finish, or zero-HITL to `ALIVE`.
- Adapter exit **0**, Chicago in-process tests, and bounded-refusal tip-cargo do **not** close **G2-9**.
- Smalt residual mix courts **ALIVE**, OOR retip12, affidavit gatekeeper size-court **ALIVE**, BRCE **V8**, VW prod honesty, Smalt GA residual, OOR cos **503** / revops **NXDOMAIN**, and CASTLE wire standings are **retained** — this land only adds the Founder HITL `--live` pin.
- Retain **BLOCKED_HITL** until the §3 command exits **0**; never absorb this ledger as crown `ALIVE` from docs alone.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Side-branch pin | 2026-11-22 — `cursor/v2711-hitl-live-next-bc54` @ `723c3a14f` from crown tip `fe72be0fb` (`bc-ea2b3d10`); docs-only; no cargo; no ManagePullRequest; no crown push |
| Tip-wins absorb | 2026-11-22 — `cursor/v2711-hitl-live-absorb-8df9` re-applies side pin onto live crown `00b53cec9` (⊇ Smalt residual mix / OOR retip12 / affidavit / BRCE V8+R01 / VW / CASTLE); standing remains **BLOCKED_HITL**; exact next `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**; **no** invented `--live` `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Prior evidence chain | hop5-foundry-live → live2 (`ZCode runtime is missing`) → zcode-runtime (vendor **PRESENT**; model access **ABSENT**) → model-access / secrets probe (registry/env **ABSENT**; MCP secret inventory **UNSUPPORTED**) → HITL next-verifier pin |
| Re-probe retip | 2026-11-22 — `cursor/v2711-hitl-live-retip-bc54` @ `0364129c5` from `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `d2c919401` (⊇ CASTLE digest-bind wire); `bc-cdfdae43`; exact §3 command re-run; **foundry_exit=1**; **BLOCKED_HITL** held; **no** invented `--live` / hop-5 / Foundry crown `ALIVE`; sidebranch push only; no ManagePullRequest |
| Tip-wins absorb (re-probe) | 2026-11-22 — §6 body already tip-wins via `7ea96a70d` (CI blank-EOF + HITL retip absorb of `0364129c5` / `bc-cdfdae43` onto `d2c919401`); `cursor/v2711-hitl-live-retip-absorb-d91a` finishes absorb honesty + CROWN pin onto live crown `7ea96a70d` (⊇ CASTLE digest-bind / VW CI-wasm / OOR retip13 / CI blank-EOF); standing remains **BLOCKED_HITL** / **REFUSED** (`foundry_exit=1`; Model access not configured); exact next `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**; **no** invented `--live` `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Re-probe retip2 | 2026-11-22 — `cursor/v2711-hitl-retip2-bc54` from `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `01eccfffb` (⊇ zcode refusal reconfirm ALIVE + OOR retip14); `bc-43dfbc3c`; exact §3 command re-run; **foundry_exit=1**; **BLOCKED_HITL** held; **no** invented `--live` / hop-5 / Foundry crown `ALIVE`; sidebranch push only; no ManagePullRequest; **no** crown push |

---

## 6. Re-probe retip (2026-11-22) — `cursor/v2711-hitl-live-retip-bc54`

| Field | Value |
|-------|-------|
| Base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `d2c91940194f80bf855e7e9b8c0f114583440309` (`d2c919401+`) |
| Branch | `cursor/v2711-hitl-live-retip-bc54` |
| zcode-cli | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Command | `cd /agent/repos/zcode-cli && bun scripts/powl-foundry-loop.ts --live` |
| UTC | `2026-11-22T16:22:30Z` |
| Cargo / tip-cargo | **NOT RUN** (docs-only retip + exact live probe) |

### Observed

| Probe | Result |
|-------|--------|
| `vendor/zcode.cjs` | **PRESENT** |
| `~/.zcode/v2/provider_config.json` | **ABSENT** |
| Host provider env names (`ANTHROPIC_*` / `ZCODE_*` / `ZAI_*` / `BIGMODEL_*` / `ZHIPU_*`) | **ABSENT** |
| Model access configured | **NO** |
| `foundry_exit` | **1** |
| `aggregateStanding` | **REFUSED** |
| `liveExecutor` | `createZcodeAgentExecutor` (injected; ≠ live pass) |
| Diagnostic (WO-A / remediate-*) | `Model access is not configured. Run /login or /setup in zcode, or configure its API key before sending a prompt. No model request was sent.` |
| Receipt artifacts | stdout `/tmp/cursor/hitl-live-retip-stdout.txt`; exit `/tmp/cursor/hitl-live-retip-exit.txt`; workspace `/tmp/zcode-foundry-yp8g5F`; loop receipt `/tmp/zcode-foundry-yp8g5F/out/loop_receipt.json` |

### Standing (this retip)

| Claim | Standing |
|-------|----------|
| Exact §3 verifier this probe | **BLOCKED_HITL** / **REFUSED** (`foundry_exit=1`; model access not configured) |
| Hop-5 live / Foundry crown | **PARTIAL** — **not** promoted; do **not** invent `ALIVE` |
| Exact next (unchanged) | Founder HITL provision (§3 A/B/C) → `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |

**Antifalsifiers held:** dry-run / runtime PRESENT / adapter exit 0 / in-repo tip-cargo ≠ `--live` `ALIVE`. Tip-wins absorb onto crown `7ea96a70d+` (body @ `7ea96a70d`; honesty this land) — standing **not** promoted.

---

## 7. Re-probe retip2 (2026-11-22) — `cursor/v2711-hitl-retip2-bc54`

| Field | Value |
|-------|-------|
| Base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `01eccfffb56c1c59be67a220eadb5bd98783c806` (`01eccfffb+`; tip-wins zcode refusal ALIVE + OOR retip14) |
| Branch | `cursor/v2711-hitl-retip2-bc54` |
| Agent | `bc-43dfbc3c` (`https://cursor.com/agents/bc-43dfbc3c-8e2c-59bc-be2a-e3ee3ee09020`) |
| zcode-cli | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Command | `cd /agent/repos/zcode-cli && bun scripts/powl-foundry-loop.ts --live` |
| UTC | `2026-11-22T16:38:02Z` |
| Cargo / tip-cargo | **NOT RUN** (docs-only retip2 + exact live probe) |

### Observed

| Probe | Result |
|-------|--------|
| `vendor/zcode.cjs` | **PRESENT** |
| `~/.zcode/v2/provider_config.json` | **ABSENT** |
| Host provider env names (`ANTHROPIC_*` / `ZCODE_*` / `ZAI_*` / `BIGMODEL_*` / `ZHIPU_*`) | **ABSENT** (`provider_env_count=0`) |
| Model access configured | **NO** |
| `foundry_exit` | **1** |
| `aggregateStanding` | **REFUSED** |
| `liveExecutor` | `createZcodeAgentExecutor` (injected; ≠ live pass) |
| Diagnostic (WO-A / remediate-*) | `Model access is not configured. Run /login or /setup in zcode, or configure its API key before sending a prompt. No model request was sent.` |
| Receipt artifacts | stdout `/tmp/cursor/hitl-retip2-stdout.txt`; exit `/tmp/cursor/hitl-retip2-exit.txt`; workspace `/tmp/zcode-foundry-vtJ3yM`; summary `/tmp/zcode-foundry-vtJ3yM/out/foundry-summary.json`; external logs `/tmp/zcode-foundry-live-logs-Pfs7fz/` |

### Standing (this retip2)

| Claim | Standing |
|-------|----------|
| Exact §3 verifier this probe | **BLOCKED_HITL** / **REFUSED** (`foundry_exit=1`; model access not configured) |
| Hop-5 live / Foundry crown | **PARTIAL** — **not** promoted; do **not** invent `ALIVE` |
| Exact next (unchanged) | Founder HITL provision (§3 A/B/C) → `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |

**Antifalsifiers held:** dry-run / runtime PRESENT / adapter exit 0 / in-repo refuse-path tip-cargo (reconfirm **ALIVE** @ `8bd3fe407` / `cd7c2433b`) ≠ `--live` `ALIVE`. Sidebranch only — **no** crown push; standing **not** promoted.
