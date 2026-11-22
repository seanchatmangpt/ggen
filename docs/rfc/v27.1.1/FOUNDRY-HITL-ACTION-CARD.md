# FOUNDRY-HITL-ACTION-CARD — Founder `--live` action card (v27.1.1)

```text
tip_base=b960245cf647ebfadb3fbc4d16bbc53852ba159f
branch=cursor/v2711-foundry-live-retip-bc54
agent=bc-23737a65-4398-52e6-a180-afd65acfa8f3
utc=2026-11-22T19:13Z
standing=BLOCKED_HITL
foundry_exit=1
foundry_finish=PARTIAL
spine=PARTIAL (not daily-HITL-free)
photolith_path_crown=PARTIAL
gate1_trio_ALIVE≠Foundry_ALIVE≠zero_HITL_ALIVE
--live=BLOCKED_HITL
refuse=Foundry_ALIVE
cargo=NOT_RUN
```

| Field | Value |
|-------|-------|
| Document ID | `FOUNDRY-HITL-ACTION-CARD-27.1.1` |
| Branch | `cursor/v2711-foundry-live-retip-bc54` (prior finish honesty `cursor/v2711-foundry-finish-honesty-bc54`; prior card land `cursor/v2711-foundry-hitl-action-retip-bc54`) |
| Agent | `bc-23737a65-4398-52e6-a180-afd65acfa8f3` (`https://cursor.com/agents/bc-23737a65-4398-52e6-a180-afd65acfa8f3`); prior finish honesty `bc-94d5f431`; prior card `bc-64334001` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.4 Foundry finish; OOR **G2-9** |
| Companions | [`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md); [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §3 / §8 / §10; [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) |
| Observed tip (write-time) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `b960245cf647ebfadb3fbc4d16bbc53852ba159f` (`b960245cf`; ⊇ live re-probe `51cbe7830` / Gate1 close absorb / Foundry finish honesty) |
| zcode-cli | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` — `scripts/powl-foundry-loop.ts` **PRESENT**; `~/.zcode/v2/provider_config.json` **ABSENT** this env |
| Cargo / tip-cargo | **NOT RUN** (live `--live` re-probe + honesty append; Gate1 tip-cargo held separately) |
| Status | ACTION CARD — live re-probe **foundry_exit=1** → `--live` **BLOCKED_HITL**; Foundry finish / horizontal spine **PARTIAL**; photolith path crown **PARTIAL**; **never invent Foundry `ALIVE`** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document is the **push-first Founder HITL action card** for foundry
`--live` (G2-9). It pins exact founder commands and pass/fail criteria so crown
progress cites one closed command set. It does **not** rewrite Gate-1 / OOR tip-pin
headers. It does **not** re-run tip-cargo. It does **not** promote runtime
**PRESENT**, adapter exit **0**, dry-run **PARTIAL**, refusal-court tip-cargo,
Gate1 tip-cargo `ALIVE`, Smalt/CASTLE sibling `ALIVE`, or this docs pin to Foundry
crown `ALIVE` or zero-HITL closed-loop `ALIVE`.

**Crown honesty (held):**

| Claim | Standing | Note |
|-------|----------|------|
| foundry `--live` (G2-9 / hop-5) | **BLOCKED_HITL** | model access / `bun bin/zcode.js login` not completed this env (**not** bare `zcode /login`); prior **foundry_exit=1** |
| Foundry finish / zero-HITL crown | **PARTIAL** | daily founder HITL still required — **refuse** Foundry `ALIVE` |
| Horizontal spine daily-HITL-free? | **PARTIAL** — **not** daily-HITL-free | §5.2 |
| Photolithography path crown | **PARTIAL** | ≠ Foundry `ALIVE` |
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
PRESENT, adapter exit 0, dry-run PARTIAL, in-repo refuse-path tip-cargo, Gate1
tip-cargo ALIVE trio, VW size ALIVE, Smalt RAID ALIVE, CASTLE wire ALIVE/`WIRED`,
or this action card alone.

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
# Antifalsifier: Gate1 tip-cargo ALIVE trio ≠ Foundry ALIVE ≠ zero-HITL ALIVE

# --- Path A (preferred; condensed) ---
cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live

# --- Path A (split form; same verifier) ---
# cd /agent/repos/zcode-cli   # @ f32df08+
# bun bin/zcode.js login   # Z.AI OAuth (HITL) — NOT slash /login; or /setup path
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

- Live re-probe land (§6) **re-ran** `--live` (EXIT **1**) and does **not** run cargo or mix.
- Live EXIT **1** / docs pin do **not** invent Foundry / hop-5 / zero-HITL `ALIVE`.
- This land does **not** ManagePullRequest or force-push.
- Sibling CASTLE/Smalt `ALIVE` does **not** clear daily founder HITL.
- Gate1 tip-cargo ALIVE trio does **not** close G2-9 / Foundry finish.
- In-repo refuse-path / layout courts do **not** close G2-9.
- Absorbing this card onto crown does **not** promote Foundry finish to `ALIVE`.

---

## 4. Provenance

| Event | Note |
|-------|------|
| Materialized | 2026-11-22 — `cursor/v2711-foundry-hitl-action-retip-bc54` from crown tip `63da993cc`; agent `bc-64334001`; push-first Foundry HITL action card; standing **BLOCKED_HITL**; Foundry finish **PARTIAL**; Founder card §2; docs-only; no cargo; no ManagePullRequest; no force-push |
| Companions read | `FOUNDRY-FINISH-SPINE-STANDING` §1–§3; `HITL-LIVE-NEXT-VERIFIER-STANDING` §3 / §8 / §10; `FOUNDRY-FINISH-STANDING` §1a / §9 |
| Exact next (live) | §2 / §5.3 Founder card → **foundry_exit=0**; else keep **BLOCKED_HITL** |
| Finish honesty retip | 2026-11-22T19:03Z — `cursor/v2711-foundry-finish-honesty-bc54` from crown tip `d85104a02`; agent `bc-94d5f431`; §5 Gate1-trio ≠ Foundry ≠ zero-HITL; spine **PARTIAL**; photolith path crown **PARTIAL**; `--live` **BLOCKED_HITL**; docs-only; no cargo; no ManagePullRequest; no force-push; no Foundry `ALIVE` |
| Live re-probe retip | 2026-11-22T19:11Z — `cursor/v2711-foundry-live-retip-bc54` from crown tip `15eefb628`; agent `bc-23737a65`; §6 live **foundry_exit=1** (`Model access is not configured`); `--live` **BLOCKED_HITL**; Foundry finish **PARTIAL**; tip-wins; no cargo; no ManagePullRequest; no Foundry `ALIVE` |

---

## 5. Finish honesty retip — Gate1 ALIVE trio ≠ Foundry ALIVE (2026-11-22T19:03Z)

Docs-only retip after tip ⊇ Gate1 tip-cargo ALIVE trio @ `2685119bd` + Taffy tip7
EXIT:**0** @ `63da993cc` / absorb `4ebcb9d46`. **Does not** re-run `--live`. **Does not**
promote Foundry / zero-HITL / path-crown `ALIVE`.

### 5.1 Honesty law (do not invent)

```text
Gate1 tip-cargo ALIVE trio @ 2685119bd
  (brand 5+12 · Taffy 7 · ui_layout 11)
    ≠ Foundry ALIVE
    ≠ zero-HITL closed-loop ALIVE

Photolithography path crown = PARTIAL (non-ALIVE)

Foundry --live = BLOCKED_HITL
  until founder: bun bin/zcode.js login + bun scripts/powl-foundry-loop.ts --live
  yields foundry_exit=0
```

| Claim | Standing | Antifalsifier |
|-------|----------|---------------|
| Gate1 tip-cargo ALIVE trio | **ALIVE (exact tip-cargo)** @ `2685119bd` | ≠ Foundry / ≠ zero-HITL closed-loop |
| Taffy tip7 exact reconfirm | **ALIVE** EXIT:**0** @ `63da993cc` | ≠ Foundry / ≠ path crown |
| Photolithography path crown | **PARTIAL** / **non-ALIVE** | [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) |
| Foundry `--live` (G2-9) | **BLOCKED_HITL** | prior **foundry_exit=1**; provider registry **ABSENT** |
| Foundry finish / zero-HITL crown | **PARTIAL** | daily founder HITL still required |
| Horizontal spine (daily-HITL-free?) | **PARTIAL** — **not** daily-HITL-free | §5.2 |

### 5.2 Horizontal spine inventory (held)

`ggen → Taffy → zcode-cli → CASTLE → Smalt → revops`

| Component | Standing (held) | Clears daily founder HITL? |
|-----------|-----------------|----------------------------|
| **ggen** + Gate1 fences | tip-cargo **ALIVE** trio @ `2685119bd` (≠ Foundry) | **No** for Foundry finish |
| **Taffy** | tip-cargo **ALIVE** (**7** @ `2685119bd`; tip7 **ALIVE** @ `63da993cc`) | **No** for Foundry finish |
| **zcode-cli** | hop-5 **PARTIAL**; refuse-path lineage **ALIVE** held; `--live` **BLOCKED_HITL** | **No** — Founder `/login` open |
| **CASTLE** | digest-bind **WIRED** / sibling courts **ALIVE**; spine wire **PARTIAL_ALIVE** | **No** — LinkedIn/cos still open |
| **Smalt** | RAID / GA economic face **ALIVE** @ `91997da76f0f` (≠ Foundry) | **No** for Foundry finish |
| **revops** | `revops.fnox.io` **NXDOMAIN** → **UNKNOWN**/**BLOCKED**; cos `/readyz` **503** **BLOCKED** | **No** |
| **Foundry `--live`** | **BLOCKED_HITL** | **No** until **foundry_exit=0** |

```text
FOUNDRY_FINISH / HORIZONTAL_SPINE = PARTIAL
  ∧ Taffy tip-cargo ALIVE
  ∧ CASTLE WIRED
  ∧ Smalt ALIVE
  ∧ revops UNKNOWN|BLOCKED
  ∧ Foundry --live BLOCKED_HITL
⇒ spine PARTIAL / not daily-HITL-free
⇒ never invent Foundry ALIVE
⇒ never invent zero-HITL closed-loop ALIVE
```

### 5.3 Exact next verifier (Founder HITL)

```bash
cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live
# PASS only if foundry_exit=0
```

| Result | Standing |
|--------|----------|
| **foundry_exit=0** | `--live` gate may advance; still do **not** invent zero-HITL Foundry crown `ALIVE` from this card alone without spine §1 closure |
| otherwise | keep **BLOCKED_HITL**; Foundry finish / spine stay **PARTIAL** |

Held this env (2026-11-22T19:03Z): provider registry **ABSENT**; docs-only — live
probe **not** re-run; standing **BLOCKED_HITL** unchanged.

---

## 6. Live re-probe — foundry_exit=1 → BLOCKED_HITL (2026-11-22T19:11Z)

Push-first tip-wins land from crown tip `15eefb628`. **Re-ran** the live verifier.
**Do not invent Foundry `ALIVE`.**

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-foundry-live-retip-bc54` |
| Agent | `bc-23737a65-4398-52e6-a180-afd65acfa8f3` |
| Crown tip (base) | `15eefb628368afcbb230d082d6ca00c286ca9d42` |
| zcode-cli | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Provider registry | **ABSENT** (`~/.zcode/v2/provider_config.json`) |
| Command | `cd /agent/repos/zcode-cli && bun scripts/powl-foundry-loop.ts --live` |
| Log | `/tmp/foundry-live-retip.txt` |
| **foundry_exit** | **1** |
| `aggregateStanding` | **REFUSED** |
| `liveExecutor` | `createZcodeAgentExecutor` (configured ≠ live pass) |
| Exact failure | `Model access is not configured. Run /login or /setup in zcode, or configure its API key before sending a prompt. No model request was sent.` |
| Standing | **BLOCKED_HITL** (login / model access required) |
| Foundry finish / spine | **PARTIAL** (not daily-HITL-free) |
| Refuse | Foundry `ALIVE` / zero-HITL closed-loop `ALIVE` / hop-5 crown `ALIVE` |

```text
foundry_exit=1
  ∧ Model access is not configured
  ∧ provider_config.json ABSENT
⇒ --live = BLOCKED_HITL
⇒ Foundry finish = PARTIAL
⇒ never invent Foundry ALIVE
```

### 6.1 Exact next verifier (unchanged)

```bash
cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live
# PASS only if foundry_exit=0
# FAIL / foundry_exit=1 → keep BLOCKED_HITL
```

| Result | Standing |
|--------|----------|
| **foundry_exit=0** | `--live` gate may advance; still do **not** invent zero-HITL Foundry crown `ALIVE` from this card alone without spine §1 closure |
| **foundry_exit=1** (this probe) | keep **BLOCKED_HITL**; Foundry finish / spine stay **PARTIAL** |

Provenance: 2026-11-22T19:11Z — live re-probe EXIT:**1**; tip-wins absorb base `15eefb628`; companion spine §6; no cargo; no ManagePullRequest; no force-push; no Foundry `ALIVE`.

## Exact CLI (retip — command form)

In this cloud image `zcode` is not on `PATH`. Use the repo bundle:

```bash
cd /agent/repos/zcode-cli
export PATH="/home/ubuntu/.bun/bin:$PATH"
bun bin/zcode.js login          # Z.AI OAuth (HITL) — NOT `zcode /login`
bun scripts/powl-foundry-loop.ts --live
# PASS only if JSON exitCode==0 / foundry_exit=0
# Observed without login: exitCode=1 · aggregateStanding=REFUSED · "Model access is not configured"
```

`setup-pending` under `~/.zcode/cli/` confirms first-run setup incomplete. No model API keys in agent env. Never invent Foundry ALIVE.
