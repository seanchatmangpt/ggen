# ui_layout_court — exact-tip6 push-first honesty (v27.1.1)

```text
tip_base=4abd4adfb2eb60fb2f7e43808345544c9d120412
court_tip=552c85f291bae312677e4448e5afa87400b86632
cited_tip_at_kickoff=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
branch=cursor/v2711-uilayout-exact-tip6-bc54
bc=bc-c92c420b-176d-5056-89e8-f4fa88893816
court=ui_layout_court
budget_s=1500
exit=137
standing=PARTIAL_ALIVE
prior_exact_alive=a508e6bae (11 passed; lineage held)
prior_retip5=cursor/v2711-uilayout-exact-retip5-bc54 @ 7b5453e67 PARTIAL EXIT:124
prior_retip6=cursor/v2711-uilayout-exact-retip6-bc54 PARTIAL EXIT:124
crown_absorb=none
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-EXACT-TIP6-STANDING` |
| Branch | `cursor/v2711-uilayout-exact-tip6-bc54` |
| Agent | `bc-c92c420b-176d-5056-89e8-f4fa88893816` |
| Tip base (push) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `4abd4adfb2eb60fb2f7e43808345544c9d120412` (live FF beyond kickoff cite `cc2a4ea66…` and court tip `552c85f29…`) |
| Court tip (probe) | `552c85f291bae312677e4448e5afa87400b86632` — layout sources ≡ tip at push (court blob `1dfe6bdf…`) |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-tip6 --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/ggen-uilayout-exact-tip6` (isolated; seeded from `/tmp/ggen-uilayout-exact-retip-warm2` 3.9G) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:137** (SIGKILL) @ `2026-11-22T18:20:24Z` — cargo killed before any compile output; log empty (0 bytes). Quiet never reached (3×15s; cargo≈8 / cc1plus≈4–5) |
| Thrash | concurrent `/tmp/gate1-exact-tip3/bc-fafc-*` courts; loadavg≈6.9 |
| Log / status | `/tmp/cursor/uilayout-exact-tip6.log` · `/tmp/cursor/uilayout-exact-tip6.status` → `UILAYOUT_EXIT=137` |
| Layout source drift vs prior ALIVE | **none** expected — court blob `1dfe6bdf…`; lineage ALIVE @ `a508e6bae` (11 passed) held |
| Prior exact tip-cargo ALIVE | **held (lineage)** — **11 passed; EXIT:0** @ `a508e6bae` — **not** reconfirmed this tip6 |
| Prior retip5 / retip6 | PARTIAL EXIT:124 on origin — see [`UILAYOUT-EXACT-RETIP5-STANDING.md`](./UILAYOUT-EXACT-RETIP5-STANDING.md) / [`UILAYOUT-EXACT-RETIP6-STANDING.md`](./UILAYOUT-EXACT-RETIP6-STANDING.md) |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** |
| Crown absorb | **none** (EXIT≠0; push-first sidebranch only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip6 push-first honesty: exclusive tip-cargo did **not** obtain EXIT 0 under pod thrash → standing **PARTIAL_ALIVE**. Prior exact tip-cargo **ALIVE** @ `a508e6bae` remains lineage evidence only. Do **not** invent Gate-1 hop-3b exact-tip reconfirm `ALIVE`, photolithography path crown, tip-wins crown absorb, or Foundry zero-HITL `ALIVE` from this land.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries `ui_layout_court` (11 tests) | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **NO** — EXIT **137** (SIGKILL before first cargo log line; thrash n≈8) |
| Lineage tip-cargo ALIVE @ `a508e6bae` | **held** (ancestor; not a tip-subject substitute) |

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 4abd4adfb (⊇ 552c85f29 ⊇ a508e6bae); court blob 1dfe6bdf…
df -h /tmp
# Prefer quiet tip (thrash_n→0); exclusive jobs=1
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test ui_layout_court --locked \
  --target-dir /tmp/ggen-uilayout-exact-tip6 \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 11 passed; EXIT 0 → tip-cargo ALIVE
```

Until that EXIT **0**, keep standing **PARTIAL_ALIVE** — lineage ALIVE @ `a508e6bae` is **not** a substitute for tip reconfirm.

---

## Compact return

```text
tip_base=4abd4adfb2eb60fb2f7e43808345544c9d120412
court_tip=552c85f291bae312677e4448e5afa87400b86632
branch=cursor/v2711-uilayout-exact-tip6-bc54
bc=bc-c92c420b
EXIT=137
standing=PARTIAL_ALIVE
diagnostic=SIGKILL_instant_under_thrash_n≈8
next=CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-tip6 --config build.jobs=1 -- --test-threads=1
lineage_alive=a508e6bae
crown_absorb=none ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
