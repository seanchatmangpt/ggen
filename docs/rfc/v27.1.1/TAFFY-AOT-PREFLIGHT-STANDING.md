# Taffy AOT preflight — push-first honesty retip7 (v27.1.1)

```text
tip_base=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
parent=2300c5d09196a2fa0a4713e7d49dd05ef9f4ce04
branch=cursor/v2711-taffy-preflight-retip7-bc54
bc=bc-4ba325d2-0de8-507b-9f23-f40f435ff2fa
court=taffy_aot_preflight_court
package=ggen-graph
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
tests=7
cargo=SKIPPED (>3min budget; tip thrash)
standing=PARTIAL (PRESENT + lineage tip-cargo ALIVE held; exact tip reconfirm PENDING)
```

| Field | Value |
|-------|-------|
| Document ID | `TAFFY-AOT-PREFLIGHT-STANDING` |
| Side branch | `cursor/v2711-taffy-preflight-retip7-bc54` |
| Tip base (crown) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` |
| Side parent | `2300c5d09196a2fa0a4713e7d49dd05ef9f4ce04` (curve25519 honesty retip atop same crown tip; court blob unchanged) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Gate 1 D2 / photolithography hop 3 |
| Companions | [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md); [`CROWN-PROGRESS-RECEIPT.md`](./CROWN-PROGRESS-RECEIPT.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) |
| Prior sidebranches | `cursor/v2711-taffy-preflight-retip-bc54` @ `ef6fb84ed` (**PRESENT** on origin); `…-retip3` / `…-retip4` / `…-retip6` local-only — **remote ABSENT** (never `ls-remote`) |
| Crown absorb | **REFUSED** this land (push-first sidebranch only) |
| ManagePullRequest | **not used** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*` | `PARTIAL` | `PRESENT` | `PENDING`.

**Authority.** This document is the push-first honesty ledger for Taffy AOT
preflight after tip4/retip6 failed to appear on origin. It does **not** tip-wins
absorb onto crown. It does **not** invent exact-tip tip-cargo `ALIVE` on
`167352d0d`. Lineage tip-cargo **ALIVE** @ `19c08a3b0` / absorb `cde02d1f2` is
**held**, not re-proven here.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ photolithography path crown `ALIVE`.

---

## 1. Standing pin (this retip7)

| Claim | Standing | Evidence (this land) |
|-------|----------|----------------------|
| Court source on tip | **PRESENT** | `git ls-tree 167352d0d crates/ggen-graph/tests/taffy_aot_preflight_court.rs` → blob `d8923b07eb4325ace90087a604b9daec0a83d29e` |
| Impl source on tip | **PRESENT** | `git ls-tree 167352d0d crates/ggen-graph/src/layout/taffy_aot.rs` → blob `275ea25dd5a0082eb16b23c56ac494dfa3018165` |
| Tip `#[test]` count | **7** | `rg -c '#\[test\]' crates/ggen-graph/tests/taffy_aot_preflight_court.rs` → **7** |
| Exact tip-cargo on `167352d0d` | **PENDING** / **PARTIAL** | Cargo **SKIPPED** — tip thrash `n≈20` concurrent `cargo test`; loadavg_1m≈11; prior retip3 EXIT:**143**; retip6 stuck on `/tmp/ggen-tip-exclusive.lock` (>6min, never pushed). Budget rule: run only if &lt;3min else PARTIAL + next verifier. |
| Lineage tip-cargo (held, not re-run) | **ALIVE** | **7 passed; exit 0** @ crown `19c08a3b0` absorb @ `cde02d1f2` (`cursor/v2711-taffy-brand-absorb-a77d`; `/tmp/gate1-taffy`) — retained; **not** re-executed this land |
| Photolithography path / Foundry crown | **PARTIAL** (unchanged) | Fence ALIVE ≠ consumer bind ALIVE ≠ Foundry zero-HITL ALIVE |

```text
tip_base=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
present=yes
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
tests=7
cargo=SKIPPED
thrash_n≈20
loadavg_1m≈11
lineage_alive_held=19c08a3b0/cde02d1f2
exact_tip_reconfirm=PENDING
standing=PARTIAL
next=§3 exact verifier → EXIT 0
ManagePullRequest=not_used
crown_absorb=REFUSED
prior_retip4_retip6_origin=ABSENT
```

---

## 2. Why prior tip4 / retip6 never appeared on origin

| Sidebranch | Local observation | `git ls-remote origin` |
|------------|-------------------|------------------------|
| `cursor/v2711-taffy-preflight-retip-bc54` | honesty @ `ef6fb84ed` | **PRESENT** (`ef6fb84ed…`) |
| `cursor/v2711-taffy-preflight-retip3-bc54` | cargo attempted; EXIT:**143**; no unique honesty push | **ABSENT** |
| `cursor/v2711-taffy-preflight-retip4-bc54` | local tip only; no push | **ABSENT** |
| `cursor/v2711-taffy-preflight-retip6-bc54` | flock wait on exclusive lock; cargo never started; no push | **ABSENT** |

**Retip7 law:** push-first. Do not wait on tip-exclusive cargo thrash before the
sidebranch exists on origin. Exact tip-cargo remains the **next** verifier (§3),
not a gate for remote presence of this honesty ledger.

---

## 3. Exact next verifier (tip-cargo reconfirm)

**Falsifier for exact tip-cargo `ALIVE` on live crown:** the command below must
exit **0** with **7 passed** against tip `167352d0d` (or a later tip whose
`taffy_aot` + court blobs remain `275ea25dd` / `d8923b07e`). Until then exact tip
reconfirm stays **PENDING** / aggregate fence standing for this retip stays
**PARTIAL** — lineage **ALIVE** held, not invented as tip-subject EXIT:0.

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 167352d0d (or successor with same court blob)
git ls-tree HEAD crates/ggen-graph/tests/taffy_aot_preflight_court.rs
# expect blob d8923b07eb4325ace90087a604b9daec0a83d29e

df -h /tmp
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
tip_base=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
parent=2300c5d09196a2fa0a4713e7d49dd05ef9f4ce04
branch=cursor/v2711-taffy-preflight-retip7-bc54
bc=bc-4ba325d2
EXIT=skipped (PARTIAL; thrash)
standing=PARTIAL (PRESENT + lineage ALIVE held; exact tip PENDING)
next=CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked → EXIT 0
crown_absorb=REFUSED
ManagePullRequest=not_used
```
