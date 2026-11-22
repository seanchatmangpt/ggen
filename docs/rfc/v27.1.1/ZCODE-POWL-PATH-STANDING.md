# ZCODE-POWL path honesty pin — hop-5 autonomic remediations (v27.1.1)

```text
tip=83c954e5fea4d454f3cc79410a368844a0469fb2
tip_cargo=PARTIAL(EXIT137@retip6) lineage_refusal=ALIVE(held@cd7c2433b)
refusal≠Foundry_ALIVE  --live=BLOCKED_HITL  hop5_slice=PARTIAL
side=cursor/v2711-zcode-path-retip-bc54
agent=bc-3708a779-35be-5efa-83ee-873bf7c3ff24
```

| Field | Value |
|-------|-------|
| Document ID | `ZCODE-POWL-PATH-STANDING-27.1.1` |
| Branch | `cursor/v2711-zcode-path-retip-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.1 hop chain `… → zcode-cli POWL/OCEL (bounded)` |
| Companions | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 5; [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md); [`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md); [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md); [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md); [`PERIMETER-TTL-TOML-PATH-STANDING.md`](./PERIMETER-TTL-TOML-PATH-STANDING.md) |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `83c954e5fea4d454f3cc79410a368844a0469fb2` (`83c954e5f`; ⊇ `e24ce7a73`+) |
| Agent | `bc-3708a779-35be-5efa-83ee-873bf7c3ff24` |
| UTC | `2026-11-22T18:15Z` |
| Cargo / tip-cargo | **NOT RUN** this land (docs-only; Gate-1 tip6 / prior retip6 own thrash) |
| Status | LEDGER — hop-5 autonomic remediations slice **PARTIAL**; bounded-refusal lineage **ALIVE** held; tip-cargo honesty **PARTIAL** (EXIT **137**); `--live` **BLOCKED_HITL**; Foundry **not** `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document pins **path honesty** for photolithography hop 5
(`zcode-cli` POWL/OCEL autonomic remediations, bounded refusal) on the exact tip
SHA above. It does **not** re-run tip-cargo, invent Foundry / `--live` `ALIVE`,
or promote the photolithography path crown.

---

## 1. Tip SHA + ancestry (held)

| Tip / evidence | Role | `git merge-base --is-ancestor` vs `83c954e5f` |
|----------------|------|-----------------------------------------------|
| `83c954e5fea4d454f3cc79410a368844a0469fb2` | Live crown tip this pin | **self** |
| `e24ce7a73` | Mandated crown floor (`~e24ce7a73+`) | **ok** |
| `2685119bd92e607cdb93d3dafdbb80cd9da0447d` | Retip6 tip-cargo EXIT **137** evidence tip | **ok** |
| `cd7c2433b7e124db6606008a92081d23aa3727ce` | Bounded-refusal tip-cargo **ALIVE** (sjira **6** + bound **4**) | **ok** |
| `df9597c18` | Refusal reconfirm EXIT **0** | held via lineage docs |

Court sources **PRESENT** on tip (not promoted without EXIT 0 this land):

- `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]`
- `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests
- IO-free contract: `crates/ggen-sjira/src/zcode_powl_ocel.rs`

---

## 2. Standing pin (honesty)

| Claim | Standing | Exact reason (held) |
|-------|----------|---------------------|
| Tip-cargo refuse path (retip6 / this crown) | **PARTIAL** | warm `/home/ubuntu/zcode-powl-refusal-target` → **EXIT 137** (OOM/killed @ ~8s); bound **SKIPPED** — [`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md) |
| Bounded-refusal lineage tip-cargo | **ALIVE** (held; historical) | sjira **6** + bound **4** EXIT **0** @ `cd7c2433b`; reconfirm EXIT **0** — [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) — **not** re-minted this land |
| Hop-5 autonomic remediations **slice** | **PARTIAL** / **PARTIAL_ALIVE** | in-process bun **ALIVE** + spatial adapter **ALIVE** + refusal lineage **ALIVE** held + tip-cargo honesty **PARTIAL** + foundry `--dry-run` **PARTIAL** (exit **2**) + `--live` open |
| Foundry / hop-5 `--live` | **BLOCKED_HITL** / **REFUSED** | **foundry_exit=1**; `Model access is not configured` @ zcode `f32df08` — [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §10 |
| Foundry / zero-HITL crown | **PARTIAL** / **non-ALIVE** | **refusal ≠ Foundry `ALIVE`** — never invent from refuse-path courts |
| Photolithography **path crown** | **PARTIAL** / **non-ALIVE** | Hop-5 slice PARTIAL ≠ path crown `ALIVE` until `--live` → **0** |

```text
PARTIAL(zcode_POWL_path @ tip 83c954e5fea4d454f3cc79410a368844a0469fb2)
  ⇔  tip-cargo honesty PARTIAL (EXIT 137 @ retip6)
  ∧  lineage refusal ALIVE held @ cd7c2433b (not re-minted)
  ∧  --live BLOCKED_HITL
  ∧  refusal ≠ Foundry ALIVE
  ∧  next verifiers named below
```

**Honesty:** tip-cargo **PARTIAL (EXIT 137)** vs lineage **ALIVE** if held — both
true; do **not** collapse them. Prior refusal `ALIVE` ≠ this tip EXIT 0. **refusal
ALIVE ≠ Foundry ALIVE**. Do **not** invent Foundry / hop-5 crown / zero-HITL
`ALIVE` from this docs pin.

---

## 3. Exact next verifiers

### 3.1 Tip-cargo refuse path → EXIT **0** (promote refusal ALIVE on a later retip)

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court --locked \
  --config build.jobs=1 -- --test-threads=1
CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target \
  cargo test -p ggen-graph remediation_bound --lib --locked \
  --config build.jobs=1 -- --test-threads=1
# PASS: 6 + 4 passed; EXIT 0
# FAIL / thrash: EXIT 137 → keep tip-cargo PARTIAL (lineage ALIVE still held)
```

### 3.2 Founder `--live` → **foundry_exit=0** (Foundry / hop-5 crown residual)

```bash
# Founder HITL — Path A condensed (≡ HITL-LIVE §10):
cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live
# PASS: foundry_exit=0 (process exit 0)
# FAIL: foundry_exit=1 + Model access not configured → keep BLOCKED_HITL
#
# Antifalsifier: refusal tip-cargo ALIVE/PARTIAL ≠ --live ALIVE
# Antifalsifier: adapter exit 0 / runtime PRESENT / dry-run PARTIAL ≠ Foundry ALIVE
```

Detail: [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §3 / §10;
[`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md).

---

## 4. Explicit non-claims

- This pin does **not** promote photolithography path crown, Foundry finish, or
  zero-HITL to `ALIVE`.
- Tip-cargo EXIT **137** ≠ lineage refusal demotion (lineage **ALIVE** held).
- Lineage refusal **ALIVE** ≠ tip-subject refuse-path EXIT **0** this land.
- Refusal courts ≠ Foundry / `--live` `ALIVE`.
- In-process bun / spatial adapter EXIT **0** ≠ hop-5 autonomic remediations
  crown `ALIVE`.
- No ManagePullRequest. No force-push. No cargo this land.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Path honesty pin | 2026-11-22 — `cursor/v2711-zcode-path-retip-bc54` (`bc-3708a779`) from crown tip `83c954e5f` (`e24ce7a73+`); docs-only; tip-cargo **PARTIAL** (EXIT **137** @ retip6) vs lineage refusal **ALIVE** held @ `cd7c2433b`; hop-5 slice **PARTIAL**; `--live` **BLOCKED_HITL**; **refusal ≠ Foundry ALIVE**; no cargo; no ManagePullRequest |
| Prior tip-cargo honesty | Retip6 [`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md) @ side `d1c4740f7` / tip `2685119bd` — EXIT **137** held through this crown |
| Prior refusal ALIVE land | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) @ `cd7c2433b` — historical; not re-minted |

### Compact return

```text
tip=83c954e5fea4d454f3cc79410a368844a0469fb2
tip_cargo=PARTIAL(EXIT137) lineage_refusal=ALIVE(held@cd7c2433b)
hop5_autonomic_remediations=PARTIAL  --live=BLOCKED_HITL
refusal≠Foundry_ALIVE  path_crown=PARTIAL
exact_next_tip_cargo=zcode_powl_ocel_court+remediation_bound → 6+4/EXIT0
exact_next_live=cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live → 0
```
