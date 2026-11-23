# ZCODE / POWL bounded-refusal standing (exact-tip reconfirm ALIVE)

```text
exit=0 standing=ALIVE(exact_tip_reconfirm)
sjira=6/6 bound=4/4 tip=15eefb628368afcbb230d082d6ca00c286ca9d42
agent=bc-a5e2f5df live=BLOCKED_HITL refusal_ALIVE≠live_ALIVE
absorb=cursor/v2711-zcode-powl-retip3-absorb-9f55 tip_before=2e1b0f561 tip_after=8c67bfd81
absorb_agent=bc-4de431e2 Foundry=BLOCKED_HITL (held; not invented ALIVE)
```

| Field | Value |
|-------|-------|
| Document ID | `ZCODE-POWL-REFUSAL-STANDING-27.1.1` |
| Sidebranch | `cursor/v2711-zcode-powl-retip3-bc54` @ `dd7746b6693f6dc575d5ffad4b3913615ea1b2be` |
| Absorb branch | `cursor/v2711-zcode-powl-retip3-absorb-9f55` |
| Side agent | `bc-a5e2f5df-7249-5f0c-973d-a4521b671913` |
| Absorb agent | `bc-4de431e2-a532-5e36-b6f9-55dc1efe9f55` |
| Ecosystem tip (side observed) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `15eefb628368afcbb230d082d6ca00c286ca9d42` (⊇ Gate1 close / tip-wins Gate1 EXIT0 absorb `712cdfafb`) |
| tip_before (absorb) | `2e1b0f561c044ba5c55c5853bd9df5bb4f3b8c20` |
| tip_after (absorb) | `8c67bfd8115c48dee54898aa5ba91a6d2a2bb8da` |
| Cargo evidence tip | `edf2ec22aa2108905397be77665aa4c73f8a0000` (exact-tip reconfirm EXIT **0**; court sources unchanged through tip `15eefb6283`; prior compile `dd61fefb29` also EXIT **0**) |
| Photolithography | Clause 1 / hop 5 — **bounded refusal** leg only |
| Prior lineage ALIVE | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) @ evidence tip `cd7c2433b` (`c91b22561` ⊆ tip) |
| Prior tip-wins reconfirm | [`ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md`](./ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md) @ `8bd3fe407` / `bc-934bba30` (EXIT **0** on `df9597c18`; tip-wins absorbed) |
| Prior sidebranch reconfirm | `bc-32bab2bc` EXIT **0** on tip `bebea5dd0` — superseded by this exact-tip retip3 |
| Court sources | unchanged vs `cd7c2433b` (`zcode_powl_ocel_court` / `zcode_powl_ocel` / `remediation_bound`) |
| zcode-cli mount | `/agent/repos/zcode-cli` (and `/home/ubuntu/zcode-cli`) @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Contract court | `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]` |
| Bound vocab | `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests |
| Tip-cargo (sjira) | `SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/zcode-powl-retip3-private-target cargo test -p ggen-sjira --test zcode_powl_ocel_court` → **6 passed; 0 failed; exit 0** |
| Tip-cargo (bound) | same env → `cargo test -p ggen-graph remediation_bound --lib` → **4 passed; 0 failed; exit 0** |
| Logs | `/tmp/cursor/zcode-powl-refusal-retip3-sjira-retip.log`, `/tmp/cursor/zcode-powl-refusal-retip3-bound-retip.log` |
| Observed at | `2026-11-22T19:05:22Z` (cargo @ `edf2ec22aa`); standing land `2026-11-22T19:10:00Z`; tip-wins absorb `2026-11-22T19:15Z` |
| Standing (bounded refusal) | **ALIVE** (exact-tip reconfirm; in-repo refuse path only) |
| Foundry `--live` | **BLOCKED_HITL** — held (`foundry_exit=1` / `Model access is not configured`; crown re-probe `51cbe7830`); **not** Foundry `ALIVE` |
| Clause-1 / hop-5 path crown | **PARTIAL_ALIVE** / **non-ALIVE** |
| Crown push | tip-wins absorb onto `cursor/ecosystem-spec-27-1-1-bc54` (FF; no force) |
| ManagePullRequest | **not used** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*` | `SCHEDULED` | `STALLED`.

**Authority.** Exact tip-cargo replay of the stall-pin next verifier on tip `edf2ec22aa2108905397be77665aa4c73f8a0000` (court sources held through tip `15eefb628368afcbb230d082d6ca00c286ca9d42`) produced **EXIT 0** (sjira **6** + bound **4**). Exact-tip reconfirm standing is therefore **ALIVE**. Tip-wins absorb of side `dd7746b66` onto crown tip_before `2e1b0f561` (tip_after `8c67bfd81`) does **not** mint hop-5 / foundry `--live` / photolithography path-crown `ALIVE`. Foundry `--live` stays **BLOCKED_HITL**.

---

## 1. Standing matrix

| Claim | Standing | Evidence |
|-------|----------|----------|
| Bounded-refusal tip-cargo (lineage) | **ALIVE** | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md); `c91b22561` ⊆ tip |
| Exact-tip reconfirm | **ALIVE** | This land: sjira **6**/bound **4**; both **exit 0** |
| Foundry `--live` / hop-5 crown | **BLOCKED_HITL** / **PARTIAL** | Unchanged; not this land's subject |
| Invented live / path-crown ALIVE | **REFUSED_UNGROUNDED** | Explicit antifalsifier |

---

## 2. Tip-cargo observation (this land)

| Court | Result | EXIT |
|-------|--------|------|
| `cargo test -p ggen-sjira --test zcode_powl_ocel_court` | **6 passed; 0 failed** | **0** |
| `cargo test -p ggen-graph remediation_bound --lib` | **4 passed; 0 failed** | **0** |

Aggregate: **6 + 4 passed; exit 0**.

---

## 3. Replay

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git checkout -B cursor/v2711-zcode-powl-retip3-bc54 origin/cursor/ecosystem-spec-27-1-1-bc54
# tip must be edf2ec22aa+ (cargo evidence); standing tip: 15eefb628368afcbb230d082d6ca00c286ca9d42

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/zcode-powl-retip3-private-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court
# expect: 6 passed; 0 failed; exit 0

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/zcode-powl-retip3-private-target \
  cargo test -p ggen-graph remediation_bound --lib
# expect: 4 passed; 0 failed; exit 0
```

**Antifalsifiers**

- Exact-tip refuse-path `ALIVE` ≠ foundry `--live` `ALIVE`
- Lineage ALIVE @ `cd7c2433b` alone ≠ this reconfirm (reconfirm required EXIT **0** on live tip)
- Agent **RUNNING** alone ≠ tip-cargo `ALIVE` (this land pushes EXIT **0** evidence)

Exact next for live crown movement: provider `/login` or `~/.zcode/v2/provider_config.json` → `bun scripts/powl-foundry-loop.ts --live` → **0**.
