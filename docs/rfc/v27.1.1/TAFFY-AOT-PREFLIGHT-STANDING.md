# Taffy AOT preflight — honesty retip6 (v27.1.1)

```text
tip_base=2d7b47b2ea5eb5108f52fa46682a68a1af7e0097
branch=cursor/v2711-taffy-preflight-retip6-bc54
bc=bc-8acadc37-03a8-5010-aa5d-949783f0111f
court=taffy_aot_preflight_court
package=ggen-graph
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
impl=275ea25dd5a0082eb16b23c56ac494dfa3018165
tests=7
EXIT=137
target=/tmp/gate1-taffy-retip6
standing=PARTIAL (PRESENT + lineage tip-cargo ALIVE held; exact tip reconfirm NOT ALIVE)
```

| Field | Value |
|-------|-------|
| Document ID | `TAFFY-AOT-PREFLIGHT-STANDING` |
| Side branch | `cursor/v2711-taffy-preflight-retip6-bc54` |
| Tip base (crown) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2d7b47b2ea5eb5108f52fa46682a68a1af7e0097` (⊇ retip7 absorb `c2cd54f56` + ui_layout exact-retip5 PARTIAL `2d7b47b2e`) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Gate 1 D2 / photolithography hop 3 |
| Companions | [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md); [`CROWN-PROGRESS-RECEIPT.md`](./CROWN-PROGRESS-RECEIPT.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) |
| Prior sidebranches | `cursor/v2711-taffy-preflight-retip-bc54` @ `ef6fb84ed` (**PRESENT**); retip7 @ `1bc005fe3` absorbed tip-wins `c2cd54f56` (**PARTIAL** / cargo SKIPPED); retip3 EXIT:**143** never pushed |
| Crown absorb | **REFUSED** this land (sidebranch push only — no tip-wins onto ecosystem tip) |
| ManagePullRequest | **not used** |
| Foundry crown | **PARTIAL** held — **no** Foundry `ALIVE` invented |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*` | `PARTIAL` | `PRESENT` | `PENDING`.

**Authority.** This document is the push-first honesty ledger for Taffy AOT
preflight retip6. It does **not** tip-wins absorb onto crown. It does **not**
invent exact-tip tip-cargo `ALIVE` on `2d7b47b2e`. Lineage tip-cargo **ALIVE**
@ `19c08a3b0` / absorb `cde02d1f2` is **held**, not re-proven as tip-subject
EXIT:0. Foundry / zero-HITL crown remains **PARTIAL**.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ photolithography path crown `ALIVE` ≠ Foundry `ALIVE`.

---

## 1. Standing pin (this retip6)

| Claim | Standing | Evidence (this land) |
|-------|----------|----------------------|
| Court source on tip | **PRESENT** | `git ls-tree 2d7b47b2e crates/ggen-graph/tests/taffy_aot_preflight_court.rs` → blob `d8923b07eb4325ace90087a604b9daec0a83d29e` |
| Impl source on tip | **PRESENT** | `git ls-tree 2d7b47b2e crates/ggen-graph/src/layout/taffy_aot.rs` → blob `275ea25dd5a0082eb16b23c56ac494dfa3018165` |
| Tip `#[test]` count | **7** | `rg -c '#\[test\]' …/taffy_aot_preflight_court.rs` → **7** |
| Exact tip-cargo on `2d7b47b2e` | **NOT ALIVE** / **PARTIAL** | Cargo **ran**; **EXIT:137** (SIGKILL) while compiling `oxrocksdb-sys` under tip thrash. Seeded warm `/tmp/gate1-taffy` → `/tmp/gate1-taffy-retip6`. Receipt `/tmp/cursor/taffy-preflight-retip6-receipt.md`. Tip-exclusive quiet-wait previously deadlocked (lock held; thrash_n never →0). |
| Lineage tip-cargo (held, not re-proven EXIT:0) | **ALIVE** | **7 passed; exit 0** @ crown `19c08a3b0` absorb @ `cde02d1f2` (`cursor/v2711-taffy-brand-absorb-a77d`; `/tmp/gate1-taffy`) — retained; **not** tip-subject EXIT:0 this land |
| Photolithography path / Foundry crown | **PARTIAL** (unchanged) | Fence lineage ALIVE ≠ consumer bind ALIVE ≠ Foundry zero-HITL ALIVE |

```text
tip_base=2d7b47b2ea5eb5108f52fa46682a68a1af7e0097
present=yes
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
tests=7
EXIT=137
target=/tmp/gate1-taffy-retip6
mode=no-exclusive (tip-exclusive quiet-wait deadlocked under thrash)
thrash_n_at_start=2
loadavg_1m≈8.66
lineage_alive_held=19c08a3b0/cde02d1f2
exact_tip_reconfirm=NOT_ALIVE (EXIT 137)
standing=PARTIAL
next=§3 exact verifier → EXIT 0
ManagePullRequest=not_used
crown_absorb=REFUSED
Foundry=PARTIAL (no ALIVE)
```

---

## 2. Cargo attempt log (retip6)

| Step | Result |
|------|--------|
| Tip fetch | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2d7b47b2e` (advanced past kickoff `8c8b1e6e5` / OOR17 `49ebdb27c`) |
| `git ls-tree` court | blob `d8923b07e` **PRESENT**; **7** tests |
| Tip-exclusive flock | Waited ~18min; quiet-wait deadlocked (lock holder never saw thrash_n→0); launcher died without cargo start |
| Direct tip-cargo | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy-retip6 cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` |
| Compile progress | Began `oxrocksdb-sys v0.5.11` after warm seed |
| **EXIT** | **137** (SIGKILL) @ 2026-11-22T17:48:29Z — elapsed ~19s after START |
| Artifacts | `/tmp/cursor/taffy-preflight-retip6.{log,status,exit,receipt.md}` |

Prior wave: retip3 EXIT:**143**; retip7 cargo **SKIPPED** (PARTIAL) absorbed tip-wins `c2cd54f56`. This retip6 records a real EXIT, still **not** exact-tip `ALIVE`.

---

## 3. Exact next verifier (tip-cargo reconfirm)

**Falsifier for exact tip-cargo `ALIVE` on live crown:** the command below must
exit **0** with **7 passed** against tip `2d7b47b2e` (or a later tip whose
`taffy_aot` + court blobs remain `275ea25dd` / `d8923b07e`). Until then exact tip
reconfirm stays **NOT ALIVE** / aggregate fence standing for this retip stays
**PARTIAL** — lineage **ALIVE** held, not invented as tip-subject EXIT:0.

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 2d7b47b2e (or successor with same court blob)
git ls-tree HEAD crates/ggen-graph/tests/taffy_aot_preflight_court.rs
# expect blob d8923b07eb4325ace90087a604b9daec0a83d29e

# Prefer quiet tip (thrash_n→0) + exclusive jobs=1
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/gate1-taffy \
  cargo test -p ggen-graph --test taffy_aot_preflight_court --locked \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 7 passed; EXIT:0
```

Success promotes this ledger's exact tip reconfirm → **ALIVE (exact tip-cargo)**.
It still does **not** promote photolithography path crown or Foundry zero-HITL
to `ALIVE`.

---

## 4. Compact return line

```text
tip_base=2d7b47b2ea5eb5108f52fa46682a68a1af7e0097
branch=cursor/v2711-taffy-preflight-retip6-bc54
bc=bc-8acadc37
EXIT=137
standing=PARTIAL (PRESENT + lineage ALIVE held; exact tip NOT ALIVE)
next=CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked → EXIT 0
crown_absorb=REFUSED
ManagePullRequest=not_used
Foundry=PARTIAL (no ALIVE)
```
