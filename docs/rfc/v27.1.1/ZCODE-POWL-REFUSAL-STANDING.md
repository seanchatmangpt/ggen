# ZCODE / POWL bounded-refusal standing (exact-tip reconfirm ALIVE)

```text
exit=0 standing=ALIVE(exact_tip_reconfirm)
sjira=6/6 bound=4/4 tip=bebea5dd0d4c9af533457e04e0ed068ae8844f69
agent=bc-32bab2bc live=BLOCKED_HITL refusal_ALIVE≠live_ALIVE
```

| Field | Value |
|-------|-------|
| Document ID | `ZCODE-POWL-REFUSAL-STANDING-27.1.1` |
| Sidebranch | `cursor/v2711-zcode-refusal-reconfirm-bc54` |
| Agent | `bc-32bab2bc-e32b-5ff9-9792-8cd84ae13df5` |
| Ecosystem tip (observed) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `bebea5dd0d4c9af533457e04e0ed068ae8844f69` (⊇ `01eccfffb` ⊇ stall absorb `1f10949a7` ⊇ `df9597c18+`) |
| Cargo evidence tip | `01eccfffb56c1c59be67a220eadb5bd98783c806` (court sources unchanged through tip-wins to `bebea5dd0d4c9af533457e04e0ed068ae8844f69`) |
| Photolithography | Clause 1 / hop 5 — **bounded refusal** leg only |
| Prior lineage ALIVE | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) @ evidence tip `cd7c2433b` (`c91b22561` ⊆ tip) |
| Prior tip-wins reconfirm | [`ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md`](./ZCODE-POWL-REFUSAL-RECONFIRM-STANDING.md) @ `8bd3fe407` / `bc-934bba30` (EXIT **0** on `df9597c18`; tip-wins absorbed) |
| Sibling reconfirm push | `bc-d4e6c905` EXIT **0** on tip `01eccfffb` — same courts; this land is independent tip-cargo replay |
| Court sources | unchanged vs `cd7c2433b` (`zcode_powl_ocel_court` / `zcode_powl_ocel` / `remediation_bound`) |
| zcode-cli mount | `/agent/repos/zcode-cli` (and `/home/ubuntu/zcode-cli`) @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Contract court | `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]` |
| Bound vocab | `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests |
| Tip-cargo (sjira) | `SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-reconfirm-target cargo test -p ggen-sjira --test zcode_powl_ocel_court` → **6 passed; 0 failed; exit 0** |
| Tip-cargo (bound) | same env → `cargo test -p ggen-graph remediation_bound --lib` → **4 passed; 0 failed; exit 0** |
| Logs | `/tmp/cursor/zcode-powl-refusal-reconfirm-sjira.log`, `/tmp/cursor/zcode-powl-refusal-reconfirm-bound.log` |
| Observed at | `2026-11-22T16:41:54Z` (cargo); standing land `2026-11-22T16:43:51Z` |
| Standing (bounded refusal) | **ALIVE** (exact-tip reconfirm; in-repo refuse path only) |
| Prior stall ledger claim | **STALLED** / **SCHEDULED** — **cleared** by this EXIT **0** |
| Foundry `--live` | **BLOCKED_HITL** — not re-probed; prior `Model access is not configured` held |
| Clause-1 / hop-5 path crown | **PARTIAL_ALIVE** / **non-ALIVE** |
| Crown push | **none** — sidebranch only |
| ManagePullRequest | **not used** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*` | `SCHEDULED` | `STALLED`.

**Authority.** Exact tip-cargo replay of the stall-pin next verifier on tip `01eccfffb` (court sources held through tip `bebea5dd0d4c9af533457e04e0ed068ae8844f69`) produced **EXIT 0** (sjira **6** + bound **4**). Exact-tip reconfirm standing is therefore **ALIVE**. This does **not** mint hop-5 / foundry `--live` / photolithography path-crown `ALIVE`.

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
git checkout -B cursor/v2711-zcode-refusal-reconfirm-bc54 origin/cursor/ecosystem-spec-27-1-1-bc54
# tip must be df9597c18+ (cargo evidence: 01eccfffb; standing tip: bebea5dd0d4c9af533457e04e0ed068ae8844f69)

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-reconfirm-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court
# expect: 6 passed; 0 failed; exit 0

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-reconfirm-target \
  cargo test -p ggen-graph remediation_bound --lib
# expect: 4 passed; 0 failed; exit 0
```

**Antifalsifiers**

- Exact-tip refuse-path `ALIVE` ≠ foundry `--live` `ALIVE`
- Lineage ALIVE @ `cd7c2433b` alone ≠ this reconfirm (reconfirm required EXIT **0** on live tip)
- Agent **RUNNING** alone ≠ tip-cargo `ALIVE` (this land pushes EXIT **0** evidence)

Exact next for live crown movement: provider `/login` or `~/.zcode/v2/provider_config.json` → `bun scripts/powl-foundry-loop.ts --live` → **0**.
