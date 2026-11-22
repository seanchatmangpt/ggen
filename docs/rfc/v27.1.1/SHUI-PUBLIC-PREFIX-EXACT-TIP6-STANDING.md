# shui_public_prefix_court — exact-tip6 push-first honesty (v27.1.1)

```text
tip_base=c661d228ac74ce4297ff73a396db5a1a24508979
cited_tip_at_kickoff=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
branch=cursor/v2711-shui-exact-tip6-bc54
bc=bc-c92c420b-176d-5056-89e8-f4fa88893816
court=shui_public_prefix_court
budget_s=1500
exit=137
standing=PARTIAL_ALIVE
prior_tip5_partial=d96b2e867 / tip-wins dd54e7821
prior_tip5_alive_attempt=7f8e0765b EXIT137 / absorb tip-wins
prior_alive_lineage=008a81d21 (4 passed; tip-cargo ALIVE held)
crown_absorb=none
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING` |
| Branch | `cursor/v2711-shui-exact-tip6-bc54` |
| Agent | `bc-c92c420b-176d-5056-89e8-f4fa88893816` |
| Observed tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c661d228ac74ce4297ff73a396db5a1a24508979` (live FF beyond kickoff cite `cc2a4ea662b12b35ac90246b3a2bfbd89db615e4`) |
| Court sources on tip | `crates/ggen-graph/tests/shui_public_prefix_court.rs` blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior ALIVE blob) |
| Exact command | `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip6 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/ggen-shui-exact-tip6` (isolated; seeded from `/tmp/ggen-shui-exact-tip5` warm 1.6G) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:137** (SIGKILL) @ `2026-11-22T18:13:30Z`→`18:13:31Z` — cargo killed before any compile output; log empty (0 bytes). Prior instant kills at `18:06:33Z` / `18:10:11Z` same signature |
| Thrash (retry window) | **7–8** concurrent `cargo test` + **2–3** `cc1plus` (other agents `/tmp/gate1-exact-tip3/bc-fafc-*`); loadavg≈5.1–6.5; quiet window **never** reached in 12×15s polls |
| Log / status | `/tmp/cursor/shui-exact-tip6.log` · `/tmp/cursor/shui-exact-tip6.status` → `SHUI_EXIT=137` |
| Prior tip5 | `cursor/v2711-shui-exact-tip5-bc54` @ `d96b2e867` PARTIAL; tip5-alive `7f8e0765b` EXIT137 tip-wins absorb — see [`SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING.md) / [`SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md) |
| Prior tip-cargo ALIVE (lineage) | **held** — 4 passed; EXIT 0 @ `008a81d21` (ancestor of tip; blob `3cda065f`) — **not** reconfirmed this tip6 |
| Crown absorb | **none** (EXIT≠0; push-first sidebranch only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip6 push-first honesty: exclusive tip-cargo did **not** obtain EXIT 0 under pod thrash → standing **PARTIAL_ALIVE**. Prior tip-cargo **ALIVE** @ `008a81d21` remains lineage evidence only. Do **not** invent tip-subject `ALIVE`, tip-wins crown absorb, or Foundry `ALIVE` from source presence / lineage alone. Prior tip5/tip5-alive pins remain **PARTIAL_ALIVE**.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries strengthened `shui_public_prefix_court` (4 tests) | **YES** |
| Court blob ≡ prior ALIVE blob `3cda065f` | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **NO** — EXIT **137** (SIGKILL before first cargo log line; thrash n≈7–8) |
| Lineage tip-cargo ALIVE @ `008a81d21` | **held** (ancestor; not a tip-subject substitute) |

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ c661d228a (⊇ cc2a4ea66 ⊇ 008a81d21); court blob 3cda065f
df -h /tmp
# Prefer quiet tip (thrash_n→0 concurrent oxrocksdb/cargo); exclusive jobs=1
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip6
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0 → promote this doc to tip-cargo ALIVE
```

Until that EXIT **0**, keep standing **PARTIAL_ALIVE** — lineage ALIVE @ `008a81d21` is **not** a substitute for tip reconfirm.

---

## Compact return

```text
tip_base=c661d228ac74ce4297ff73a396db5a1a24508979
branch=cursor/v2711-shui-exact-tip6-bc54
bc=bc-c92c420b
EXIT=137
standing=PARTIAL_ALIVE
diagnostic=SIGKILL_instant_under_thrash_n≈8
next=CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip6 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1
lineage_alive=008a81d21
crown_absorb=none ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
