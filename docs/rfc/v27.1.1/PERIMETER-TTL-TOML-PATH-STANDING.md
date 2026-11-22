# Photolithography perimeter → `.ttl`/`.toml` — path honesty pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `PERIMETER-TTL-TOML-PATH-STANDING-27.1.1` |
| Branch | tip-wins absorb `cursor/v2711-perimeter-path-bc54` @ `46c347281` onto crown via `cursor/v2711-perimeter-path-absorb-ef01` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.1 hop chain `perimeter → .ttl/.toml → …` |
| Companions | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hops 1–2; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md); [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md); [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) |
| Observed ggen tip (absorb base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `8629691f8d713f8a5cd3d28c5daa3ec2ead18701` (⊇ HITL action-card §8 `8629691f8` on `893070836+`) |
| Sidebranch | `cursor/v2711-perimeter-path-bc54` @ `46c34728199bf0b75eb67f492d47640ffb641847` |
| Crown absorb | **HELD** tip-wins reapply onto `origin/cursor/ecosystem-spec-27-1-1-bc54` @ pre-absorb `8629691f8` via `cursor/v2711-perimeter-path-absorb-ef01` (side base was `893070836`; HITL raced ahead); no ManagePullRequest |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty absorb; prior tip-cargo held by ancestry) |
| Status | LEDGER — hops 1–2 **ALIVE** (held); path crown **PARTIAL** / **non-ALIVE**; Foundry / `--live` **BLOCKED_HITL** / **not** `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document pins **path honesty** for the photolithography
opening edge `perimeter transducer → .ttl/.toml` on the exact tip SHA above.
Tip-wins absorb retains that honesty — it does **not** re-run tip-cargo, invent
Foundry / `--live` `ALIVE`, or promote the photolithography path crown.

---

## 1. Census (side base tip `893070836`; absorb base `8629691f8`)

### 1.1 In-repo hop subjects (PRESENT)

| Hop | Subject | Path | Tip presence |
|-----|---------|------|--------------|
| 1 | Perimeter transducer (public Turtle / `shui:` fence) | `crates/ggen-graph/src/vocab/public_prefixes.rs` (`ADMITTED_TURTLE_PREFIXES` includes `shui`); `crates/ggen-graph/src/vocab/shui.rs`; `crates/ggen-graph/tests/shui_public_prefix_court.rs`; `crates/ggen-lsp/assets/policies/no-private-namespace.shacl.ttl` | **PRESENT** |
| 1 | Product perimeter (Truex admit) | out-of-repo `seanchatmangpt/truex` @ `68d2d233` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED**) | held **ALIVE** (not re-run) |
| 2 | `.toml` brand rhythm + completeness | `docs/rfc/v27.1.1/fixtures/{fourth-watch,cindycat}.brand.toml`; `crates/ggen-graph/src/layout/brand.rs`; `tests/brand_toml_{completeness,rhythm}_court.rs` | **PRESENT** |
| 2 | `.ttl` law surfaces | `ontologies/core/ocel2.ttl`; `crates/ggen-graph/src/vocab/{ocel,powl}.rs` | **PRESENT** |

There is **no** file named “transducer”. Hop 1 is the O→O\* admission fence
(`AGENTS.md` §3) — public-prefix / private-namespace gate — not a separate pack
binary. Full DTCG color/type/CASTLE inventories remain Gate 2+ /
**UNSUPPORTED** here (`REFUSED_UNGROUNDED` if invented in ggen).

### 1.2 `packs/` + `examples/` (honesty)

`rg -i 'perimeter|transducer|photolithography'` under `packs/` → **no matches**.
Under `examples/` → only unrelated archive “perimeter” prose
(`examples/archive/**`) — **not** this hop chain. OEM brand stubs and courts
live under `docs/rfc/v27.1.1/fixtures/` + `crates/ggen-graph/`, not under packs.

### 1.3 Ancestry of prior tip-cargo (held; not re-run)

| Evidence tip | Subject | `git merge-base --is-ancestor` vs `893070836` |
|--------------|---------|-----------------------------------------------|
| `008a81d21` | `shui_public_prefix_court` **4** EXIT 0 | **ok** |
| `77eb46fd0` | brand completeness **5** + rhythm **12** EXIT 0 | **ok** |
| `cd7c2433b` | zcode refuse sjira **6** + bound **4** EXIT 0 | **ok** |
| `d0fec2f2f` | igniter clean-room reconfirm EXIT 0 | **ok** |

---

## 2. Standing pin (honesty)

| Claim | Standing | Exact reason (held) |
|-------|----------|---------------------|
| Hop 1 — ggen perimeter fence (`shui:` / public prefixes) | **ALIVE (lineage tip-cargo)** | **4 passed; exit 0** @ `008a81d21`; sources ≡ tip `c5101d597`; **no exact tip EXIT:0** on live crown — do **not** invent tip-subject fence `ALIVE` |
| Hop 1 — Truex product admit-court | **ALIVE** | vitest **6/6** exit 0 @ `truex@68d2d233` (merged `b4672131`; not re-run) |
| Hop 2 — `.ttl`/`.toml` rhythm + completeness | **ALIVE (exact tip-cargo)** | completeness **5** + rhythm **12** EXIT 0 @ `77eb46fd0` ([`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md)); dual-schema `[primitive.space]` held |
| Hop 2 — full DTCG / CASTLE token inventory | **UNSUPPORTED** | Gate 2+ / brand plane; inventing tokens → `REFUSED_UNGROUNDED` |
| `ggen sync` (Jobs/C4 admitted pack `--check` + hop-5 dry-run fallback) | **ALIVE** (held) | Admitted `--pack-dir test/fixtures/sample-pack … --check` exit **0** ([`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)); hop-5 adapter falls back to `ggen sync run --dry-run` when `mix` ABSENT ([`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md)). Bare `--check` remains **REFUSED_INVOCATION** antifalsifier |
| Igniter clean-room / BrandTheme manufacture (hop 4) | **ALIVE** (held) | Mount `git diff --exit-code` → **0**; BrandTheme compare `diff_exit=0` ([`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); tip-wins reconfirm @ `d0fec2f2f`) |
| Zcode POWL/OCEL bounded refusal (hop 5 refuse path) | **ALIVE** (held) | sjira **6** + bound **4** EXIT 0 @ `cd7c2433b`; reconfirm EXIT 0 ([`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md)) — **refusal ALIVE ≠ `--live` ALIVE** |
| Foundry `powl-foundry-loop --live` | **BLOCKED_HITL** / **REFUSED** | **foundry_exit=1**; `Model access is not configured` @ zcode `f32df08` ([`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md)) |
| Foundry / zero-HITL crown | **PARTIAL** / **non-ALIVE** | Do **not** invent Foundry `ALIVE` from hops 1–2, igniter, or refusal tip-cargo |
| Photolithography **path crown** | **PARTIAL** / **non-ALIVE** | Hops 1–2 ALIVE ≠ path crown ALIVE until hop-5 `--live` → **0** |

---

## 3. Exact next verifier

**Already closed (do not restate as open):**

```bash
# Hop 1 fence replay (held ALIVE lineage @ 008a81d21)
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 \
  cargo test -p ggen-graph --test shui_public_prefix_court --locked

# Hop 2 brand.toml replay (held ALIVE exact tip-cargo @ 77eb46fd0)
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-brand-exact-tip \
  cargo test -p ggen-graph \
  --test brand_toml_completeness_court \
  --test brand_toml_rhythm_court \
  --locked -- --test-threads=1

# Igniter clean-room / Jobs/C4 (held ALIVE)
cd /home/ubuntu/ggen_igniter && git diff --exit-code
# admitted: mix ggen_igniter.sync --pack-dir test/fixtures/sample-pack … --check → 0

# Zcode bounded refusal (held ALIVE)
SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court
# + remediation_bound --lib → 4; exit 0
```

**Exact next (path crown / Foundry — still open):**

```bash
# Founder HITL — provision model access, then:
cd /agent/repos/zcode-cli
bun scripts/powl-foundry-loop.ts --live
# FALSIFIER: foundry_exit=0
# Until then: BLOCKED_HITL / REFUSED — Model access is not configured
# Antifalsifier: refusal tip-cargo ALIVE ≠ --live ALIVE; hops 1–2 ALIVE ≠ Foundry ALIVE
```

Detail: [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §3.

---

## 4. Explicit non-claims

- This pin does **not** promote photolithography path crown, Foundry finish, or
  zero-HITL to `ALIVE`.
- Hop-1 lineage tip-cargo ≠ exact tip EXIT:0 fence `ALIVE` on live crown.
- Hop-2 exact tip-cargo ≠ full DTCG/CASTLE inventory `ALIVE`.
- Igniter clean-room EXIT:0 ≠ path crown `ALIVE`.
- Zcode refusal tip-cargo ≠ `--live` `ALIVE`.
- `ggen sync run --dry-run` fallback ≠ manufacture / Foundry `ALIVE`.
- No packs/examples photolithography subject invented.
- Tip-wins absorb does **not** invent path crown / Foundry / `--live` `ALIVE`.
- No ManagePullRequest. No force-push.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Sidebranch honesty pin | 2026-11-22 — `cursor/v2711-perimeter-path-bc54` @ `46c347281` from tip `893070836ef83ab711e657cfacc98bb4dc7ea369`; docs-only; `rg` census under `docs/rfc/v27.1.1` + `packs`/`examples`; hops 1–2 ALIVE held; ggen sync / igniter / zcode refusal ALIVE vs `--live` **BLOCKED_HITL**; **no** Foundry `ALIVE`; no cargo; no ManagePullRequest |
| Tip-wins absorb | 2026-11-22 — `cursor/v2711-perimeter-path-absorb-ef01` re-applies `46c347281` onto live crown `8629691f8` (HITL action-card absorb raced ahead of FF from `893070836`); standing held — path crown **PARTIAL**; `--live` **BLOCKED_HITL**; **no** Foundry `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Foundry HITL Retip3 cross-pin | 2026-11-22 — `cursor/v2711-foundry-hitl-retip3-bc54` (`bc-6cdda390`) from crown tip `864ba13dc`: path crown stays **PARTIAL**; Foundry **PARTIAL**; `--live` **BLOCKED_HITL** (model access / `zcode /login` not configured); refusal-court PARTIAL ≠ Foundry; VW size ALIVE ≠ Foundry; exact next canonical `cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live` → **0**; docs-only; no cargo — [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §10 |

### Compact return

```text
tip_base=893070836ef83ab711e657cfacc98bb4dc7ea369
hop1=ALIVE(lineage tip-cargo@008a81d21)+Truex@68d2d233
hop2=ALIVE(exact tip-cargo@77eb46fd0) DTCG/CASTLE=UNSUPPORTED
ggen_sync+igniter+zcode_refusal=ALIVE(held)
foundry_--live=BLOCKED_HITL foundry_exit=1
path_crown=PARTIAL exact_next=bun scripts/powl-foundry-loop.ts --live → 0
# Retip3 (2026-11-22 @ 864ba13dc): Foundry=PARTIAL --live=BLOCKED_HITL
# exact_next=cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live → 0
```
