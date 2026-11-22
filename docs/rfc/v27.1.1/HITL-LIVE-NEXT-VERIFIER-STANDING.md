# Founder HITL `--live` — exact next verifier pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `HITL-LIVE-NEXT-VERIFIER-STANDING-27.1.1` |
| Branch | `cursor/v2711-hitl-live-next-bc54` @ crown base `fe72be0fbc8e37cff6db04e08f64732273b7c893` (**side branch only** — does **not** race VW/BRCE/OOR crown tip writers; no ManagePullRequest; no crown push) |
| Companion | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) §1a / §2, [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md), [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md), [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) **G2-9**, [`CROWN-PROGRESS-RECEIPT.md`](./CROWN-PROGRESS-RECEIPT.md) |
| Observed ggen tip (read-only baseline) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `fe72be0fbc8e37cff6db04e08f64732273b7c893` (`9173c6529+`; tip-wins verify_witness prod honesty after Smalt GA residual) |
| Prior tip observation | First read-only fetch this land: `9173c65296cd8db544f6ae3544f3531a4bb9399c` — crown advanced before side-branch land; pin retargeted to live tip without pushing crown |
| zcode-cli land | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` (Gall `--live` injects `createZcodeAgentExecutor`; Linux tip-matched `vendor/zcode.cjs` **PRESENT**) |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty pin) |
| Status | LEDGER — **BLOCKED_HITL**; **not** hop-5 / Foundry / zero-HITL `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document pins the **exact Founder HITL next verifier** for foundry `--live` (OOR **G2-9**) so a later crown absorb can cite one closed command set. It does **not** rewrite Gate-1 / OOR tip-pin headers. It does **not** promote runtime **PRESENT**, adapter exit **0**, dry-run **PARTIAL**, or in-repo bounded-refusal tip-cargo to hop-5 / Foundry crown `ALIVE`.

---

## 1. Standing pin (honesty)

| Claim | Standing | Evidence (held; not re-run this land) |
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

- This side branch does **not** push `cursor/ecosystem-spec-27-1-1-bc54`.
- This land does **not** claim hop-5 live, path crown, Foundry finish, or zero-HITL `ALIVE`.
- Adapter exit **0**, Chicago in-process tests, and bounded-refusal tip-cargo do **not** close **G2-9**.
- Do not absorb this ledger as crown `ALIVE` — absorb only after the §3 command exits **0**, or retain **BLOCKED_HITL** with this pin.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Materialized | 2026-11-22 — `cursor/v2711-hitl-live-next-bc54` from crown tip `fe72be0fb` (`fe72be0fbc8e37cff6db04e08f64732273b7c893`; first observation `9173c6529`); docs-only; no cargo; no ManagePullRequest; no crown push |
| Prior evidence chain | hop5-foundry-live → live2 (`ZCode runtime is missing`) → zcode-runtime (vendor **PRESENT**; model access **ABSENT**) → model-access / secrets probe (registry/env **ABSENT**; MCP secret inventory **UNSUPPORTED**) |
| For later absorb | Parent tip-wins onto live crown when ready; cite §3 verbatim; keep standing **BLOCKED_HITL** until `--live` exit **0** |
