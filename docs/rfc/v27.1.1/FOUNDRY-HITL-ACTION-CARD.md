# FOUNDRY-HITL-ACTION-CARD — Founder `--live` action card (v27.1.1)

```text
tip_base=63da993cc194ff2177ee757e01c883b582028493
branch=cursor/v2711-foundry-hitl-action-retip-bc54
agent=bc-64334001-9fdc-5ee1-863d-3b283238e71d
utc=2026-11-22T18:36Z
standing=BLOCKED_HITL
foundry_finish=PARTIAL
--live=BLOCKED_HITL
refuse=Foundry_ALIVE
cargo=NOT_RUN
```

| Field | Value |
|-------|-------|
| Document ID | `FOUNDRY-HITL-ACTION-CARD-27.1.1` |
| Branch | `cursor/v2711-foundry-hitl-action-retip-bc54` |
| Agent | `bc-64334001-9fdc-5ee1-863d-3b283238e71d` (`https://cursor.com/agents/bc-64334001-9fdc-5ee1-863d-3b283238e71d`) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.4 Foundry finish; OOR **G2-9** |
| Companions | [`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md); [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §3 / §8 / §10 |
| Observed tip (write-time) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `63da993cc194ff2177ee757e01c883b582028493` (`63da993cc`) |
| zcode-cli | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` — `scripts/powl-foundry-loop.ts` **PRESENT**; `~/.zcode/v2/provider_config.json` **ABSENT** this env |
| Cargo / tip-cargo | **NOT RUN** (docs-only; Taffy tip7 owns EXIT0 cargo) |
| Status | ACTION CARD — `--live` **BLOCKED_HITL**; Foundry finish **PARTIAL**; **never invent Foundry `ALIVE`** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document is the **push-first Founder HITL action card** for foundry
`--live` (G2-9). It pins exact founder commands and pass/fail criteria so crown
progress cites one closed command set. It does **not** rewrite Gate-1 / OOR tip-pin
headers. It does **not** re-run tip-cargo (Taffy tip7 owns EXIT0 cargo). It does
**not** promote runtime **PRESENT**, adapter exit **0**, dry-run **PARTIAL**,
refusal-court tip-cargo, Smalt/CASTLE sibling `ALIVE`, or this docs pin to Foundry
crown `ALIVE`.

**Crown honesty (held):**

| Claim | Standing | Note |
|-------|----------|------|
| foundry `--live` (G2-9 / hop-5) | **BLOCKED_HITL** | model access / `zcode /login` not configured this env; prior **foundry_exit=1** |
| Foundry finish / zero-HITL crown | **PARTIAL** | daily founder HITL still required — **refuse** Foundry `ALIVE` |
| Provider registry | **ABSENT** | `~/.zcode/v2/provider_config.json` |
| Runtime / Gall executor | **PRESENT** / configured | ≠ `--live` pass |

---

## 1. Pass / fail criteria

| Result | Standing |
|--------|----------|
| **`foundry_exit=0`** (process exit **0**) | `--live` gate may advance; still do **not** invent zero-HITL Foundry crown `ALIVE` from this card alone without crown absorb + spine §1 closure ([`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md)) |
| **`foundry_exit=1`** / `Model access is not configured` | keep **BLOCKED_HITL**; Foundry finish stays **PARTIAL** |
| Docs pin / absorb alone | keep **BLOCKED_HITL** — card ≠ live pass |

**Refuse:** inventing `--live` / Foundry / hop-5 / zero-HITL `ALIVE` from runtime
PRESENT, adapter exit 0, dry-run PARTIAL, in-repo refuse-path tip-cargo, VW size
ALIVE, Smalt RAID ALIVE, CASTLE wire ALIVE/`WIRED`, or this action card alone.

---

## 2. Exact Founder HITL commands (copy/paste)

Canonical Path A condensed (≡ [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §3 / §8 / §10 and [`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md) §3 — **not** a third path):

```bash
# === FOUNDER HITL ACTION CARD — ECOSYSTEM-SPEC-27.1.1 / G2-9 ===
# Standing NOW: BLOCKED_HITL (foundry_exit=1; model access not configured)
# PASS only when: foundry_exit=0 (process exit 0)
# FAIL: foundry_exit=1 → keep BLOCKED_HITL
# Do NOT claim Foundry / hop-5 / zero-HITL ALIVE until foundry_exit=0
#   (and even then: card alone ≠ zero-HITL Foundry crown ALIVE).

# --- Path A (preferred; condensed) ---
cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live

# --- Path A (split form; same verifier) ---
# cd /agent/repos/zcode-cli   # @ f32df08+
# # inside zcode: /login  —or— /setup
# bun scripts/powl-foundry-loop.ts --live

# --- Path B — materialize provider registry (non-interactive) ---
# mkdir -p ~/.zcode/v2
# cp /agent/repos/zcode-cli/provider.example.json /tmp/provider_config.template.json
# # edit: providerRules[].config.access.apiKey (non-empty),
# #       enabled=true, visibility≠hidden,
# #       access.type ∈ {api-key,zhipu-coding-plan-api-key},
# #       defaultModelSelection.{providerId,modelId}
# install -m 600 /tmp/provider_config.template.json ~/.zcode/v2/provider_config.json
# cd /agent/repos/zcode-cli && bun scripts/powl-foundry-loop.ts --live

# --- Path C — shell env only (names; Founder sets values; never commit) ---
# export ANTHROPIC_API_KEY=…   # or ANTHROPIC_AUTH_TOKEN / ZCODE_API_KEY / ZCODE_MODEL_MAIN
# # NOTE: OPENAI_API_KEY alone does NOT clear missingCodingPlanKey when registry ABSENT
# cd /agent/repos/zcode-cli && bun scripts/powl-foundry-loop.ts --live

# FALSIFIER / PASS: foundry_exit=0
# Until then: BLOCKED_HITL — refuse Foundry ALIVE
```

Keys detail: [`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md) §1–§2.
Runtime PRESENT: [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md).

---

## 3. Explicit non-claims

- This land does **not** run cargo, mix, or `--live` (docs-only retip; Taffy tip7 owns EXIT0 cargo).
- This land does **not** ManagePullRequest or force-push.
- Sibling CASTLE/Smalt `ALIVE` does **not** clear daily founder HITL.
- In-repo refuse-path / layout courts do **not** close G2-9.
- Absorbing this card onto crown does **not** promote Foundry finish to `ALIVE`.

---

## 4. Provenance

| Event | Note |
|-------|------|
| Materialized | 2026-11-22 — `cursor/v2711-foundry-hitl-action-retip-bc54` from crown tip `63da993cc`; agent `bc-64334001`; push-first Foundry HITL action card; standing **BLOCKED_HITL**; Foundry finish **PARTIAL**; Founder card §2; docs-only; no cargo; no ManagePullRequest; no force-push |
| Companions read | `FOUNDRY-FINISH-SPINE-STANDING` §1–§3; `HITL-LIVE-NEXT-VERIFIER-STANDING` §3 / §8 / §10; `FOUNDRY-FINISH-STANDING` §1a / §9 |
| Exact next (live) | §2 Founder card → **foundry_exit=0**; else keep **BLOCKED_HITL** |
