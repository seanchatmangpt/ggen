# Hop-5 foundry `--live` model access — standing receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `HOP5-MODEL-ACCESS-STANDING-27.1.1` |
| Branch | `cursor/v2711-hop5-secrets-probe-bc54` (probe land); prior `cursor/v2711-hop5-model-access-bc54` |
| Companion | [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md), [`HOP5-FOUNDRY-LIVE2-STANDING.md`](./HOP5-FOUNDRY-LIVE2-STANDING.md), [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) G2-9 |
| Ecospec tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `a29e1cd92fa1edc0cd0b744a3b628c871039e825` |
| zcode-cli land | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Host | Linux x86_64 cloud VM |
| Cargo / tip-cargo | **NOT RUN** (admitted scope: investigation only) |
| Status | LEDGER (not path-crown `ALIVE`) |

## Probe (this land)

| Probe | Result |
|-------|--------|
| `vendor/zcode.cjs` | **PRESENT** (prior runtime land) |
| `~/.zcode/v2/provider_config.json` | **ABSENT** |
| `~/.zcode/cli/setup-pending` | **UNKNOWN** (no `~/.zcode/v2/` tree on this VM at secrets probe) |
| Env provider token names | **ABSENT** (see §3) |
| `cursor-cloud` MCP `list-available-secrets` | **UNSUPPORTED** — tool absent from namespace catalog; invoke → `Tool … was not found` (no secret **names** returned; values never queried) |
| Cursor **app** `bind-app-secrets` / publish secrets | **wrong surface** for hop-5 VM zcode — does not materialize `~/.zcode/v2/provider_config.json` on cloud agent disk without HITL |
| Admitting `--live` fixture / dry-run bypass | **REFUSED** (see §2) |

Failure text (prior / expected until provisioned):
`Model access is not configured. Run /login or /setup in zcode, or configure its API key before sending a prompt. No model request was sent.`
Source: `zcode-cli` `src/prompt-preflight.ts` (`missingCodingPlanKey`).

## 1) What satisfies provider / model access

### Path

Default file: `~/.zcode/v2/provider_config.json`
(`sharedDataBaseDir` + `.zcode/v2/…`; see `src/config-paths.ts`).

Overrides:

| Env / mechanism | Effect |
|-----------------|--------|
| `ZCODE_PERSONAL_PROVIDER_CONFIG_FILE` | Absolute path to provider JSON (replaces default) |
| `ZCODE_DATA_BASE_DIR` | Moves the `.zcode/` tree under this base |
| Interactive `zcode` `/login` or `/setup` | Writes registry (Coding Plan OAuth / API-key picker). Linux: Z.AI browser OAuth callback is **macOS-only**; use Coding Plan **API key** path in `/login` |
| Project markers | Presence of `zcode.json`, `.zcode/config.json`, or `.env` in cwd ancestry **defers** preflight to runtime (does **not** invent credentials) |

### Exact JSON keys (`schemaVersion: 1`)

Public shape: `provider.example.json`. Minimum for configured access
(`readConfiguredModelAccess` / `hasConfiguredProviderAccess` in `src/model-access.ts`):

```text
schemaVersion = 1
config.providerConfigRules.providerRules[]:
  providerId
  enabled !== false
  templateId?                    # OR personalModelIds non-empty
  config.visibility !== "hidden"
  config.access.type ∈ {"api-key","zhipu-coding-plan-api-key"}
  config.access.apiKey           # non-empty string (secret — never commit)
  config.personalModelIds[]      # must include selected modelId when present
  config.api?                    # custom providers: type + baseUrl; headers may also satisfy preflight
config.defaultModelSelection:
  providerId
  modelId
```

Account providers (`providerId` starts with `account:`) additionally require
`~/.zcode/v2/credentials.json` identity + `account-provider:coding-plan:…:api-key`
entries (runtime-owned; not a dry-run substitute).

### Env names that clear the preflight refusal

`missingCodingPlanKey` treats a non-empty match of this regex as “defer to runtime”
(so the typed ABSENT message is **not** emitted):

```text
/^(?:ZCODE|ZAI|BIGMODEL|ZHIPU|ANTHROPIC)_.*(?:KEY|TOKEN|MODEL|CONFIG|PROVIDER|BASE_URL)/
```

Documented examples (tests): `ANTHROPIC_API_KEY`, `ANTHROPIC_AUTH_TOKEN`,
`ZCODE_API_KEY`, `ZCODE_MODEL_MAIN`.

Explicitly **excluded** from that deferral: `ZCODE_BASE_URL`,
`ZCODE_MODEL_RETRY_*`, `ZCODE_MODEL_TELEMETRY_ENABLED`.

`OPENAI_API_KEY` is redacted elsewhere but is **not** in the preflight regex —
alone it does **not** clear “Model access is not configured” when the registry is ABSENT.

## 2) Admitted `--live` bypass?

| Path | Standing |
|------|----------|
| `bun scripts/powl-foundry-loop.ts` (default / `--dry-run`) | **ADMITTED** as dry-run — ceiling **PARTIAL_ALIVE**; no live executor |
| `bun scripts/powl-foundry-loop.ts --live` | Injects `createZcodeAgentExecutor` → real `bin/zcode.js -p …`; requires runtime **and** model access |
| Unit/smoke `writeProviderFixture` / temp HOME keys | Test-only; **not** admitted for hop-5 `--live` crown `ALIVE` |
| Fake empty `provider_config.json` / inventing ALIVE | **REFUSED** |

Script contract (`scripts/powl-foundry-loop.ts` header): without credentials,
standing is typed **BLOCKED** / **REFUSED**, never false `ALIVE`.

## 3) Host secret / env names (values never printed)

| Name class | Present? |
|------------|----------|
| `ANTHROPIC_*` / `ZAI_*` / `ZHIPU_*` / `BIGMODEL_*` / `ZCODE_*KEY*` / `ZCODE_*TOKEN*` / `OPENAI_*` | **ABSENT** |
| Cursor cloud / app provider tokens (by env name) | **ABSENT** |
| Cursor cloud MCP secret name inventory (`list-available-secrets`, `project_dir=/workspace`) | **UNSUPPORTED** on this pod — **not** admitted as usable keys |
| Agent infra only | `CURSOR_AGENT`, `CURSOR_AGENT_SOCKET`, `CURSOR_CONVERSATION_ID`, `CURSOR_REQUEST_ID`, `CURSOR_RIPGREP_PATH`, `CURSOR_SPAWN_CWD_HOP`, `CURSOR_WORKLOAD_CGROUP`, `__CURSOR_SANDBOX_ENV_RESTORE` — **not** provider credentials |

`secrets_present_names=` (usable provider tokens): _(empty — MCP inventory **UNSUPPORTED**; host env names **ABSENT**)_

Required provider key **names** to match when inventory exists (never print values): non-empty `config.providerConfigRules.providerRules[].config.access.apiKey` plus `config.defaultModelSelection.{providerId,modelId}`; or preflight env names `ANTHROPIC_API_KEY`, `ANTHROPIC_AUTH_TOKEN`, `ZCODE_API_KEY`, `ZCODE_MODEL_MAIN`, or other matches of §1 deferral regex.

## Standing

| Claim | Standing |
|-------|----------|
| Linux tip-matched runtime (`vendor/zcode.cjs`) | **ALIVE** (prior; ≠ crown) |
| Provider registry / model access | **ABSENT** → **BLOCKED** / **REFUSED** |
| `--live` admitted fixture bypass | **REFUSED** (none) |
| Host usable provider env/secret names | **ABSENT** |
| Hop-5 live foundry / path / Foundry crown | **PARTIAL_ALIVE** / **PARTIAL** — do **not** invent crown `ALIVE` |

## Exact next verifier (OOR)

```bash
# 1) Provision real credentials (HITL — agent cannot read Cursor secret values here):
#    Interactive:  zcode → /login (Coding Plan API key on Linux) or /setup
#    Or materialize ~/.zcode/v2/provider_config.json from schema (paste apiKey locally):
mkdir -p ~/.zcode/v2
cp /agent/repos/zcode-cli/provider.example.json /tmp/provider_config.template.json
# edit in editor: set providerRules[].config.access.apiKey (non-empty secret),
#   enabled=true, visibility≠hidden, access.type api-key or zhipu-coding-plan-api-key,
#   personalModelIds + defaultModelSelection.{providerId,modelId}; then:
install -m 600 /tmp/provider_config.template.json ~/.zcode/v2/provider_config.json
#    Or export a non-empty preflight env for this shell only (names only; you set values):
#      export ANTHROPIC_API_KEY=…   # or ZCODE_API_KEY / ANTHROPIC_AUTH_TOKEN / …
# 2) Re-run live foundry (zcode-cli @ f32df08+; vendor PRESENT):
cd /agent/repos/zcode-cli
bun scripts/powl-foundry-loop.ts --live
# expect: foundry_exit=0 / aggregateStanding=ALIVE for hop-5 live crown
# antifalsifier: dry-run PARTIAL ≠ live ALIVE; empty registry ≠ ALIVE
```

Jobs/C4 residual (separate): `mix ggen_igniter.sync --check` → **0**.
