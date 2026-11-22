# Taffy AOT preflight — exact-tip6 push-first honesty (v27.1.1)

```text
tip_base=552c85f291bae312677e4448e5afa87400b86632
cited_tip_at_kickoff=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
branch=cursor/v2711-taffy-preflight-tip6-bc54
bc=bc-c92c420b-176d-5056-89e8-f4fa88893816
court=taffy_aot_preflight_court
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
tests=7
budget_s=1500
exit=137
standing=PARTIAL_ALIVE
lineage_alive=19c08a3b0 / absorb cde02d1f2
exact_tip_reconfirm=PARTIAL (EXIT:137)
crown_absorb=none
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `TAFFY-AOT-PREFLIGHT-TIP6-STANDING` |
| Branch | `cursor/v2711-taffy-preflight-tip6-bc54` |
| Agent | `bc-c92c420b-176d-5056-89e8-f4fa88893816` |
| Tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `552c85f291bae312677e4448e5afa87400b86632` (live FF beyond kickoff cite `cc2a4ea66…`) |
| Court blob on tip | `d8923b07eb4325ace90087a604b9daec0a83d29e` — **7** `#[test]` |
| Exact command | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy-tip6 cargo test -p ggen-graph --test taffy_aot_preflight_court --locked` |
| Target dir | `/tmp/gate1-taffy-tip6` (isolated; seeded from `/tmp/gate1-taffy` warm 2.0G) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:137** (SIGKILL) @ `2026-11-22T18:18:39Z` — cargo killed before any compile output; log empty. Quiet never reached (3×15s; cargo≈8 / cc1plus≈4–5) |
| Thrash | concurrent `/tmp/gate1-exact-tip3/bc-fafc-*` courts; loadavg≈5.5–5.8 |
| Log / status | `/tmp/cursor/taffy-tip6.log` · `/tmp/cursor/taffy-tip6.status` → `TAFFY_EXIT=137` |
| Lineage tip-cargo ALIVE | **held** — **7 passed; EXIT 0** @ `19c08a3b0` / absorb `cde02d1f2` — **not** reconfirmed this tip6 |
| Prior retip7 | `cursor/v2711-taffy-preflight-retip7-bc54` — PARTIAL (cargo SKIPPED thrash); see [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) |
| Crown absorb | **none** (EXIT≠0; push-first sidebranch only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip6 push-first honesty: exclusive tip-cargo did **not** obtain EXIT 0 under pod thrash → standing **PARTIAL_ALIVE**. Lineage tip-cargo **ALIVE** @ `19c08a3b0` remains lineage evidence only. Do **not** invent exact-tip `ALIVE`, photolithography path crown, tip-wins crown absorb, or Foundry `ALIVE` from source presence / lineage alone.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ photolithography path crown `ALIVE`.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries `taffy_aot_preflight_court` (7 tests; blob `d8923b07e`) | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **NO** — EXIT **137** (SIGKILL before first cargo log line; thrash n≈8) |
| Lineage tip-cargo ALIVE @ `19c08a3b0` | **held** (ancestor; not a tip-subject substitute) |

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 552c85f29; court blob d8923b07e
df -h /tmp
# Prefer quiet tip (thrash_n→0); exclusive jobs=1
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/gate1-taffy-tip6 \
  cargo test -p ggen-graph --test taffy_aot_preflight_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 7 passed; EXIT 0 → exact tip-cargo ALIVE
```

Until that EXIT **0**, keep standing **PARTIAL_ALIVE** — lineage ALIVE @ `19c08a3b0` is **not** a substitute for tip reconfirm.

---

## Compact return

```text
tip_base=552c85f291bae312677e4448e5afa87400b86632
branch=cursor/v2711-taffy-preflight-tip6-bc54
bc=bc-c92c420b
EXIT=137
standing=PARTIAL_ALIVE
diagnostic=SIGKILL_instant_under_thrash_n≈8
next=CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy-tip6 cargo test -p ggen-graph --test taffy_aot_preflight_court --locked
lineage_alive=19c08a3b0/cde02d1f2
crown_absorb=none ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
