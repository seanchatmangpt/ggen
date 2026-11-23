# zcode_powl_ocel + remediation_bound — retip5 ALIVE (v27.1.1)

```text
tip=6cdb9749204b857b7ddd79aa1c63162c79ff6c80
cargo_evidence_tip=4af3c6ec58d936c3c094946d1a05914d4196d00c
branch=cursor/v2711-zcode-powl-retip5-bc54
sjira=6/6 bound=4/4 exit=0
standing=ALIVE (exact tip-cargo bounded refusal)
live=BLOCKED_HITL Foundry=NOT_CLAIMED
refusal_ALIVE_neq_Foundry_ALIVE=held
refusal_ALIVE_neq_live_ALIVE=held
target=/tmp/zcode-powl-retip3-private-target
crown_absorb=none ManagePullRequest=not_used force_push=none
agent=bc-925cb029-76a7-5b99-bcb0-5c5ac5dcbbdb
```

| Field | Value |
|-------|-------|
| Document ID | `ZCODE-POWL-RETIP5-STANDING-27.1.1` |
| Branch | `cursor/v2711-zcode-powl-retip5-bc54` |
| Ecosystem tip (land) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `6cdb9749204b857b7ddd79aa1c63162c79ff6c80` (⊇ cargo evidence `4af3c6ec58d936c3c094946d1a05914d4196d00c` ⊇ admitted `49ebdb27c35ca8c666796482510de481fde7fcd9`+) |
| Cargo evidence tip | `4af3c6ec58d936c3c094946d1a05914d4196d00c` (court sources held through land tip) |
| Photolithography | Clause 1 / hop 5 — **bounded refusal** leg only |
| Prior ALIVE lands | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) @ `cd7c2433b`; [`ZCODE-POWL-RETIP4-STANDING.md`](./ZCODE-POWL-RETIP4-STANDING.md) EXIT **0** |
| Prior PARTIAL | [`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md) EXIT **137** (superseded for refuse-path by this EXIT **0**) |
| Contract court | `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs` — **6** `#[test]` |
| Bound vocab | `crates/ggen-graph/src/layout/remediation_bound.rs` — **4** unit tests |
| Tip-cargo (sjira) | `SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 RUSTUP_TOOLCHAIN=nightly-2026-06-22 CARGO_TARGET_DIR=/tmp/zcode-powl-retip3-private-target cargo test -p ggen-sjira --test zcode_powl_ocel_court -- --test-threads=1` → **6 passed; 0 failed; exit 0** |
| Tip-cargo (bound) | same env → `cargo test -p ggen-graph remediation_bound --lib -- --test-threads=1` → **4 passed; 0 failed; exit 0** |
| Logs | `/tmp/cursor/zcode-powl-retip5-sjira.log`, `/tmp/cursor/zcode-powl-retip5-bound.log`, `/tmp/cursor/zcode-powl-retip5-sjira.exit` (=0), `/tmp/cursor/zcode-powl-retip5-bound.exit` (=0) |
| Observed at | sjira `2026-11-22T19:11:02Z`→`19:27:31Z`; bound `2026-11-22T19:27:39Z`→`19:34:40Z` |
| zcode-cli mount | `/agent/repos/zcode-cli` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Standing (bounded refusal) | **ALIVE** (exact tip-cargo refuse path only) |
| Foundry `--live` | **BLOCKED_HITL** — not re-probed; prior `Model access is not configured` held |
| Clause-1 / hop-5 path crown | **PARTIAL_ALIVE** / **non-ALIVE** |
| Crown push | **none** — sidebranch only |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Exact tip-cargo replay of `zcode_powl_ocel_court` + `remediation_bound` on tip `4af3c6ec` (⊇ `49ebdb27c+`; court sources held through land tip `6cdb97492`) produced **EXIT 0** (sjira **6** + bound **4**). Bounded-refusal standing is therefore **ALIVE**. This does **not** mint Foundry `--live` / hop-5 path-crown `ALIVE`.

---

## 1. Standing matrix

| Claim | Standing | Evidence |
|-------|----------|----------|
| Bounded-refusal tip-cargo (this retip) | **ALIVE** | sjira **6**/bound **4**; both **exit 0** |
| Foundry `--live` / hop-5 crown | **BLOCKED_HITL** / **PARTIAL** | Unchanged; not this land's subject |
| Invented live / Foundry / path-crown ALIVE | **REFUSED_UNGROUNDED** | Explicit antifalsifier |

---

## 2. Tip-cargo observation (this land)

| Court | Result | EXIT |
|-------|--------|------|
| `cargo test -p ggen-sjira --test zcode_powl_ocel_court` | **6 passed; 0 failed** | **0** |
| `cargo test -p ggen-graph remediation_bound --lib` | **4 passed; 0 failed** | **0** |

Aggregate: **6 + 4 passed; exit 0**.

---

## 3. Honesty

**refusal ALIVE ≠ Foundry ALIVE ≠ `--live` ALIVE.** This retip seals the cheapest in-repo refuse courts only. Sibling Chicago `bun test test/powl-ocel-chicago.test.ts` and `bun scripts/powl-foundry-loop.ts --live` remain out of this land's promotion claim.

---

## 4. Replay

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git checkout -B cursor/v2711-zcode-powl-retip5-bc54 origin/cursor/ecosystem-spec-27-1-1-bc54
# tip must be 49ebdb27c+ (cargo evidence: 4af3c6ec; land: 6cdb97492)

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 RUSTUP_TOOLCHAIN=nightly-2026-06-22 \
  CARGO_TARGET_DIR=/tmp/zcode-powl-retip3-private-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court -- --test-threads=1
# expect: 6 passed; 0 failed; exit 0

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 RUSTUP_TOOLCHAIN=nightly-2026-06-22 \
  CARGO_TARGET_DIR=/tmp/zcode-powl-retip3-private-target \
  cargo test -p ggen-graph remediation_bound --lib -- --test-threads=1
# expect: 4 passed; 0 failed; exit 0
```

Exact next for live crown movement: provider `/login` or `~/.zcode/v2/provider_config.json` → `bun scripts/powl-foundry-loop.ts --live` → **0** ([`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md)).
