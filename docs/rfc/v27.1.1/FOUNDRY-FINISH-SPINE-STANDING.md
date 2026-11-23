# FOUNDRY-FINISH-SPINE-STANDING — push-first honesty (v27.1.1)

```text
tip_base=c19ad401d (Construct ALIVE@45086131007 tip-wins)
branch=cursor/v2711-construct-alive-retip-bc54
agent=bc-c5848e85-7857-4989-9a23-d4416c85bc54
utc=2026-11-23T00:25Z
spine=ggen → Taffy → zcode-cli → CASTLE → Smalt → revops
foundry_finish=PARTIAL
spine_daily_HITL_free=PARTIAL (not daily-HITL-free)
foundry_exit=1
construct=ALIVE@45086131007 ≠ Foundry_ALIVE
gate1_trio_ALIVE≠Foundry_ALIVE≠zero_HITL_ALIVE
photolith_path_crown=PARTIAL
--live=BLOCKED_HITL
daily_founder_HITL=REQUIRED
refuse=Foundry_ALIVE
```

| Field | Value |
|-------|-------|
| Document ID | `FOUNDRY-FINISH-SPINE-STANDING-27.1.1` |
| Branch | `cursor/v2711-construct-alive-retip-bc54` (prior foundry live retip `cursor/v2711-foundry-live-retip-bc54`) |
| Agent | `bc-c5848e85-7857-4989-9a23-d4416c85bc54`; prior `bc-23737a65` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.4 Foundry finish (horizontal spine) |
| Companions | [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) §5 / §6; [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md); [`CONSTRUCT-FULL-ALIVE-STANDING.md`](./CONSTRUCT-FULL-ALIVE-STANDING.md); [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md); [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md); [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md); [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) |
| Observed tip (write-time) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c19ad401d` (Construct **ALIVE** tip-wins; Gate1 tip7 ALIVE; Foundry `--live` still BLOCKED_HITL) |
| zcode-cli | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` — `scripts/powl-foundry-loop.ts` **PRESENT**; `~/.zcode/v2/provider_config.json` **ABSENT** this env; no model API keys in env |
| Cargo / tip-cargo | Construct CI **ALIVE** @ `45086131007`; Foundry `--live` re-probe **foundry_exit=1** held |
| Status | LEDGER — Construct **ALIVE** ≠ Foundry **ALIVE**; `--live` **BLOCKED_HITL**; Foundry finish / horizontal spine **PARTIAL** (not daily-HITL-free); path crown **PARTIAL**; **never invent Foundry `ALIVE`** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document is the **push-first horizontal spine honesty pin** for
Foundry finish without daily founder HITL. It summarizes held companion
standings on tip `d85104a02` (⊇ Gate1 ALIVE trio @ `2685119bd`) and names the
exact Founder HITL gate. It does **not** rewrite `GATE-1-GAP-MATRIX.md`. It does
**not** re-run tip-cargo. It does **not** promote sibling `ALIVE`, Gate1 tip-cargo
`ALIVE`, refuse-path courts, runtime **PRESENT**, or docs presence to Foundry
crown `ALIVE` or zero-HITL closed-loop `ALIVE`.

**Crown claim.** Spine
`ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` runs as an admitted,
receipt-bound loop with **no ambient daily founder HITL**.

| Crown claim standing | Reason |
|----------------------|--------|
| **PARTIAL** | Gate1 tip-cargo ALIVE trio / sibling RAID / CASTLE wire can be green while founder HITL remains required for foundry `--live`, cos DB, and WebAuthn/BRCE V8. Photolith path crown stays **PARTIAL**. Until `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**, Foundry finish / horizontal spine stay **PARTIAL** (not daily-HITL-free; or **BLOCKED_HITL** on the live gate alone) — **never invent Foundry `ALIVE`**. |

---

## 1. Horizontal spine table

| Component | Standing (held) | Daily founder HITL still required? | Exact next verifier |
|-----------|-----------------|------------------------------------|---------------------|
| **ggen** | Gate1 tip-cargo **ALIVE** trio @ `2685119bd` (brand **5+12** / Taffy **7** / ui_layout **11**) + lineage fences held — **≠** Foundry crown / **≠** zero-HITL closed-loop | **Yes** (Gate-1→GA promote + shui tip7 + bodies + Foundry `--live`) | Sealed trio replay only if sources drift; Founder card [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) §5 |
| **Taffy** | Tip-cargo **ALIVE** (**7** @ `2685119bd`); tip7 exact reconfirm **ALIVE** EXIT:**0** @ `63da993cc` / absorb `4ebcb9d46` — **≠** Foundry / **≠** path crown | **No** for ordinary AOT replay when lineage holds; **Yes** if receipt law drifts | Sealed: `CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| **zcode-cli** | Hop-5 slice **PARTIAL**; refuse-path lineage **ALIVE** held @ `cd7c2433b`; tip-cargo honesty **PARTIAL** (EXIT **137**); `--live` **BLOCKED_HITL** — [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md) | **Yes** — model access / `bun bin/zcode.js login` | Founder card §3 / action card §5.3 → **foundry_exit=0** |
| **CASTLE** | Sibling courts **ALIVE** @ `2774039ea`; digest-bind wire **ALIVE**/`WIRED` @ `e0d99c6`; spine crown **PARTIAL_ALIVE** — [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) | **Yes** for LinkedIn AND-gate (cos `/readyz` **200**) | Replay `cargo test --test publish_digest_bind_court` → **6/6**; then cos readiness ∧ 2× durable LOW |
| **Smalt** | RAID / GA economic face **ALIVE** @ `91997da76f0f` (4-cloud + FinOps + residual) — ≠ Foundry — [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) | **No** for ordinary RAID membrane when courts hold | Replay FinOps / residual mix courts @ `91997da76f0f` → EXIT **0** |
| **revops** | `revops.fnox.io` **NXDOMAIN** **UNKNOWN**/**BLOCKED**; cos `/readyz` **503** `DB_UNAVAILABLE` **BLOCKED**; `/healthz` **PARTIAL** — [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) | **Yes** — cos DB + DNS + BRCE V8 / WebAuthn live | `curl … https://cos.fnox.io/readyz` → **200**; `getent hosts revops.fnox.io` ∧ revops `/readyz` → DNS ∧ **200** |

```text
FOUNDRY_FINISH / HORIZONTAL_SPINE = PARTIAL (not daily-HITL-free)
  ∧ Gate1 tip-cargo ALIVE trio @ 2685119bd (≠ Foundry ≠ zero-HITL)
  ∧ Taffy tip-cargo ALIVE (+ tip7 ALIVE @ 63da993cc)
  ∧ zcode --live BLOCKED_HITL
  ∧ CASTLE wire ALIVE/WIRED / spine PARTIAL_ALIVE
  ∧ Smalt RAID ALIVE (≠ Foundry)
  ∧ revops NXDOMAIN UNKNOWN|BLOCKED ∧ cos 503 BLOCKED
  ∧ photolith path crown PARTIAL
  ∧ daily founder HITL REQUIRED (--live, cos DB, WebAuthn/V8)
⇒ never invent Foundry ALIVE
⇒ never invent zero-HITL closed-loop ALIVE
```

---

## 2. HITL gate status (Foundry finish)

| Gate | Standing | Clears when |
|------|----------|-------------|
| **Foundry finish / zero-HITL crown** | **PARTIAL** | All §1 rows ALIVE **and** no daily founder HITL for ordinary spine SELECT/CONSTRUCT; Plane-5 DO via live BRCE+AS-01 only |
| foundry `--live` (G2-9 / hop-5) | **BLOCKED_HITL** | §3 Founder card → **foundry_exit=0** |
| cos `/readyz` | **BLOCKED** (**503** `DB_UNAVAILABLE`) | HTTP **200** |
| WebAuthn live / BRCE V8 | **UNSUPPORTED** / **BLOCKED** | Founder ceremony + broker seat → EXIT **0** |
| Daily founder HITL required today? | **YES** | At least `--live` + cos DB + WebAuthn/V8 remain open |

**Honesty law (held):**
Gate1 tip-cargo ALIVE trio ≠ Foundry `ALIVE` ≠ zero-HITL closed-loop `ALIVE`.
Smalt RAID `ALIVE` ≠ Foundry `ALIVE`.
CASTLE wire `ALIVE`/`WIRED` ≠ Foundry `ALIVE`.
zcode refuse-path `ALIVE` ≠ `--live` `ALIVE` ≠ Foundry `ALIVE`.
Taffy / ggen tip-cargo `ALIVE` ≠ Foundry `ALIVE`.
Photolithography path crown `PARTIAL` ≠ Foundry `ALIVE`.
Docs pin / this ledger ≠ Foundry `ALIVE`.

Until `--live` EXIT **0**, overall Foundry finish / horizontal spine standing must
remain **PARTIAL** (not daily-HITL-free) or **BLOCKED_HITL** on the live gate —
**never invent Foundry `ALIVE`**.

---

## 3. Explicit Founder HITL action card

Canonical Path A condensed (≡ [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §3 / §8 / §10 and [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) §5.3 — **not** a third path):

```bash
cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live
# PASS only if foundry_exit=0
```

| Result | Standing |
|--------|----------|
| **foundry_exit=0** (process exit **0**) | `--live` gate may advance; still do **not** invent zero-HITL Foundry crown `ALIVE` from this card alone without crown absorb + §1 closure |
| **foundry_exit=1** / `Model access is not configured` | keep **BLOCKED_HITL**; Foundry finish stays **PARTIAL** |

Held this env (2026-11-22T19:03Z): provider registry **ABSENT**; prior probes
**foundry_exit=1** @ zcode `f32df08` — standing **BLOCKED_HITL** unchanged
(docs-only; live probe not re-run that land).

**Live re-probe (2026-11-22T19:11Z)** @ zcode `f32df08` — **re-ran**:

```bash
cd /agent/repos/zcode-cli && bun scripts/powl-foundry-loop.ts --live
# → foundry_exit=1
# → aggregateStanding=REFUSED
# → Model access is not configured. Run /login or /setup …
# log: /tmp/foundry-live-retip.txt
```

Standing remains **BLOCKED_HITL**. Foundry finish / spine remain **PARTIAL**.
**Refuse** Foundry `ALIVE`. See [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) §6.

Path B/C (registry JSON / env names) remain documented in HITL-LIVE §8 — same
falsifier command after provision.

---

## 4. Explicit non-claims

- This land **re-ran** `--live` (EXIT **1**) and does **not** run cargo or mix.
- Live EXIT **1** / docs pin / tip-wins absorb do **not** invent Foundry `ALIVE`.
- This land does **not** ManagePullRequest or force-push.
- Sibling CASTLE/Smalt `ALIVE` does **not** clear daily founder HITL.
- Gate1 tip-cargo ALIVE trio does **not** clear Foundry finish / zero-HITL.
- In-repo refuse-path / layout courts do **not** clear G2-9.
- Absorbing this ledger onto crown does **not** promote Foundry finish to `ALIVE`.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Materialized | 2026-11-22 — `cursor/v2711-foundry-spine-honesty-bc54` from crown tip `3d13c19e9`; agent `bc-14b0377a`; push-first spine honesty; Foundry **PARTIAL**; `--live` **BLOCKED_HITL**; Founder card §3; docs-only; no cargo; no ManagePullRequest; no force-push |
| Companions read | `FOUNDRY-FINISH-STANDING` §1/§9; `HITL-LIVE` §10; `CASTLE-SPINE` §13–§14; `SMALT-RAID` §6; `REVOPS-COS` §1; `TAFFY-AOT-PREFLIGHT`; `ZCODE-POWL-PATH` |
| Exact next (live) | §3 Founder card → **foundry_exit=0** |
| Finish honesty retip | 2026-11-22T19:03Z — `cursor/v2711-foundry-finish-honesty-bc54` from crown tip `d85104a02`; agent `bc-94d5f431`; Gate1 trio **ALIVE** ≠ Foundry ≠ zero-HITL; Taffy tip-cargo **ALIVE**; CASTLE **WIRED**; Smalt **ALIVE**; revops **UNKNOWN**; Foundry **BLOCKED_HITL** ⇒ spine **PARTIAL** / not daily-HITL-free; photolith path crown **PARTIAL**; companion [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) §5; docs-only; no cargo; no ManagePullRequest; no Foundry `ALIVE` |
| Live re-probe retip | 2026-11-22T19:11Z — `cursor/v2711-foundry-live-retip-bc54` from crown tip `15eefb628`; agent `bc-23737a65`; **foundry_exit=1** (`Model access is not configured`); `--live` **BLOCKED_HITL**; Foundry finish / spine **PARTIAL**; companion action card §6; tip-wins; no cargo; no ManagePullRequest; no Foundry `ALIVE` |
