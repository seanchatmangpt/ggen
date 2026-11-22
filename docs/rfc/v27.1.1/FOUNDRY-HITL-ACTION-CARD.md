# FOUNDRY-HITL-ACTION-CARD — Founder `--live` action card (v27.1.1)

```text
tip_base=6f0f646111e91830cb1b2a18db18f67049cfe90d
branch=cursor/v2711-foundry-finish-honesty-bc54
agent=bc-94d5f431-76af-5467-b369-49fbcc2fcc82
utc=2026-11-22T19:03Z
standing=BLOCKED_HITL
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
| Branch | `cursor/v2711-foundry-finish-honesty-bc54` (prior card land `cursor/v2711-foundry-hitl-action-retip-bc54`) |
| Agent | `bc-94d5f431-76af-5467-b369-49fbcc2fcc82` (`https://cursor.com/agents/bc-94d5f431-76af-5467-b369-49fbcc2fcc82`); prior card `bc-64334001` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.4 Foundry finish; OOR **G2-9** |
| Companions | [`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md); [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §3 / §8 / §10; [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) |
| Observed tip (write-time) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `d85104a02606ac77408cbea028a94c67c6382af1` (`d85104a02`; ⊇ tip7 Taffy absorb `4ebcb9d46` / Gate1 ALIVE trio @ `2685119bd` / prior card `376d0d283`) |
| zcode-cli | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` — `scripts/powl-foundry-loop.ts` **PRESENT**; `~/.zcode/v2/provider_config.json` **ABSENT** this env |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty retip; Gate1 trio sealed @ `2685119bd`; Taffy tip7 EXIT0 held @ `63da993cc`) |
| Status | ACTION CARD — `--live` **BLOCKED_HITL**; Foundry finish / horizontal spine **PARTIAL**; photolith path crown **PARTIAL**; **never invent Foundry `ALIVE`** |

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
| foundry `--live` (G2-9 / hop-5) | **BLOCKED_HITL** | model access / `zcode /login` not configured this env; prior **foundry_exit=1** |
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

- This land does **not** run cargo, mix, or `--live` (docs-only honesty retip).
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
  until founder: zcode /login + bun scripts/powl-foundry-loop.ts --live
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
cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live
# PASS only if foundry_exit=0
```

| Result | Standing |
|--------|----------|
| **foundry_exit=0** | `--live` gate may advance; still do **not** invent zero-HITL Foundry crown `ALIVE` from this card alone without spine §1 closure |
| otherwise | keep **BLOCKED_HITL**; Foundry finish / spine stay **PARTIAL** |

Held this env (2026-11-22T19:03Z): provider registry **ABSENT**; docs-only — live
probe **not** re-run; standing **BLOCKED_HITL** unchanged.
