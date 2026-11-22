# FOUNDRY-FINISH-SPINE-STANDING — push-first honesty (v27.1.1)

```text
tip_base=3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed
branch=cursor/v2711-foundry-spine-honesty-bc54
agent=bc-14b0377a-f9c2-5db0-aeca-2f39ba0ad983
utc=2026-11-22T18:26Z
spine=ggen → Taffy → zcode-cli → CASTLE → Smalt → revops
foundry_finish=PARTIAL
--live=BLOCKED_HITL
daily_founder_HITL=REQUIRED
refuse=Foundry_ALIVE
```

| Field | Value |
|-------|-------|
| Document ID | `FOUNDRY-FINISH-SPINE-STANDING-27.1.1` |
| Branch | `cursor/v2711-foundry-spine-honesty-bc54` |
| Agent | `bc-14b0377a-f9c2-5db0-aeca-2f39ba0ad983` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.4 Foundry finish (horizontal spine) |
| Companions | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md); [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md); [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md); [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md); [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md); [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md); [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) |
| Observed tip (write-time) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed` (`3d13c19e9`; ⊇ O(N²) ops `e6d00965a` / Gate matrix §18 / tip6 courts / zcode path `552c85f29`) |
| zcode-cli | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` — `scripts/powl-foundry-loop.ts` **PRESENT**; `~/.zcode/v2/provider_config.json` **ABSENT** this env |
| Cargo / tip-cargo | **NOT RUN** (docs-only push-first honesty) |
| Status | LEDGER — Foundry finish **PARTIAL**; `--live` **BLOCKED_HITL**; **never invent Foundry `ALIVE`** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document is the **push-first horizontal spine honesty pin** for
Foundry finish without daily founder HITL. It summarizes held companion
standings on tip `3d13c19e9` and names the exact Founder HITL gate. It does
**not** rewrite `GATE-1-GAP-MATRIX.md`. It does **not** re-run tip-cargo. It does
**not** promote sibling `ALIVE`, refuse-path courts, runtime **PRESENT**, or
docs presence to Foundry crown `ALIVE`.

**Crown claim.** Spine
`ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` runs as an admitted,
receipt-bound loop with **no ambient daily founder HITL**.

| Crown claim standing | Reason |
|----------------------|--------|
| **PARTIAL** | Tip fences / sibling RAID / CASTLE wire can be green while founder HITL remains required for foundry `--live`, cos DB, and WebAuthn/BRCE V8. Until `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**, Foundry finish stays **PARTIAL** (or **BLOCKED_HITL** on the live gate alone) — **never invent Foundry `ALIVE`**. |

---

## 1. Horizontal spine table

| Component | Standing (held) | Daily founder HITL still required? | Exact next verifier |
|-----------|-----------------|------------------------------------|---------------------|
| **ggen** | Tip-cargo / lineage fences **ALIVE** or **PARTIAL_ALIVE** (taffy/ui_layout/shui/brand tip6 thrash honesty; CCS `write_apply` / r01+BRCE seat lineage held) — ≠ Foundry crown | **Yes** (Gate-1→GA promote + brand exact-tip EXIT:0 + bodies) | Brand/taffy/ui_layout exact-tip reconfirm → EXIT **0** where still PARTIAL; see [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) §1 |
| **Taffy** | Lineage tip-cargo **ALIVE** (**7** @ `19c08a3b0`); exact-tip honesty **PARTIAL** / **PENDING** (tip6 thrash) — [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) | **No** for ordinary AOT replay when lineage holds; **Yes** if receipt law drifts | `CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| **zcode-cli** | Hop-5 slice **PARTIAL**; refuse-path lineage **ALIVE** held @ `cd7c2433b`; tip-cargo honesty **PARTIAL** (EXIT **137**); `--live` **BLOCKED_HITL** — [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md) | **Yes** — model access / `zcode /login` | Founder card §3 → **foundry_exit=0** |
| **CASTLE** | Sibling courts **ALIVE** @ `2774039ea`; digest-bind wire **ALIVE**/`WIRED` @ `e0d99c6`; spine crown **PARTIAL_ALIVE** — [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) | **Yes** for LinkedIn AND-gate (cos `/readyz` **200**) | Replay `cargo test --test publish_digest_bind_court` → **6/6**; then cos readiness ∧ 2× durable LOW |
| **Smalt** | RAID / GA economic face **ALIVE** @ `91997da76f0f` (4-cloud + FinOps + residual) — ≠ Foundry — [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) | **No** for ordinary RAID membrane when courts hold | Replay FinOps / residual mix courts @ `91997da76f0f` → EXIT **0** |
| **revops** | `revops.fnox.io` **NXDOMAIN** **UNKNOWN**/**BLOCKED**; cos `/readyz` **503** `DB_UNAVAILABLE` **BLOCKED**; `/healthz` **PARTIAL** — [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) | **Yes** — cos DB + DNS + BRCE V8 / WebAuthn live | `curl … https://cos.fnox.io/readyz` → **200**; `getent hosts revops.fnox.io` ∧ revops `/readyz` → DNS ∧ **200** |

```text
FOUNDRY_FINISH = PARTIAL
  ∧ ggen fences ALIVE|PARTIAL_ALIVE (≠ crown)
  ∧ Taffy lineage ALIVE / exact-tip PARTIAL
  ∧ zcode --live BLOCKED_HITL
  ∧ CASTLE wire ALIVE/WIRED / spine PARTIAL_ALIVE
  ∧ Smalt RAID ALIVE (≠ Foundry)
  ∧ revops NXDOMAIN BLOCKED ∧ cos 503 BLOCKED
  ∧ daily founder HITL REQUIRED (--live, cos DB, WebAuthn/V8)
⇒ never invent Foundry ALIVE
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
Smalt RAID `ALIVE` ≠ Foundry `ALIVE`.
CASTLE wire `ALIVE`/`WIRED` ≠ Foundry `ALIVE`.
zcode refuse-path `ALIVE` ≠ `--live` `ALIVE` ≠ Foundry `ALIVE`.
Taffy / ggen tip-cargo `ALIVE` ≠ Foundry `ALIVE`.
Docs pin / this ledger ≠ Foundry `ALIVE`.

Until `--live` EXIT **0**, overall Foundry finish standing must remain
**PARTIAL** or **BLOCKED_HITL** — **never invent Foundry `ALIVE`**.

---

## 3. Explicit Founder HITL action card

Canonical Path A condensed (≡ [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §3 / §8 / §10 — **not** a third path):

```bash
cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live
```

| Result | Standing |
|--------|----------|
| **foundry_exit=0** (process exit **0**) | `--live` gate may advance; still do **not** invent zero-HITL Foundry crown `ALIVE` from this card alone without crown absorb + §1 closure |
| **foundry_exit=1** / `Model access is not configured` | keep **BLOCKED_HITL**; Foundry finish stays **PARTIAL** |

Held this env (2026-11-22T18:26Z): provider registry **ABSENT**; prior probes
**foundry_exit=1** @ zcode `f32df08` — standing **BLOCKED_HITL** unchanged
(docs-only; live probe not re-run this land).

Path B/C (registry JSON / env names) remain documented in HITL-LIVE §8 — same
falsifier command after provision.

---

## 4. Explicit non-claims

- This land does **not** run cargo, mix, or `--live`.
- This land does **not** ManagePullRequest or force-push.
- Sibling CASTLE/Smalt `ALIVE` does **not** clear daily founder HITL.
- In-repo refuse-path / layout courts do **not** clear G2-9.
- Absorbing this ledger onto crown does **not** promote Foundry finish to `ALIVE`.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Materialized | 2026-11-22 — `cursor/v2711-foundry-spine-honesty-bc54` from crown tip `3d13c19e9`; agent `bc-14b0377a`; push-first spine honesty; Foundry **PARTIAL**; `--live` **BLOCKED_HITL**; Founder card §3; docs-only; no cargo; no ManagePullRequest; no force-push |
| Companions read | `FOUNDRY-FINISH-STANDING` §1/§9; `HITL-LIVE` §10; `CASTLE-SPINE` §13–§14; `SMALT-RAID` §6; `REVOPS-COS` §1; `TAFFY-AOT-PREFLIGHT`; `ZCODE-POWL-PATH` |
| Exact next (live) | §3 Founder card → **foundry_exit=0** |
