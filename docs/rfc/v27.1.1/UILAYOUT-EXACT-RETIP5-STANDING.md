# ui_layout_court — exact-retip5 honesty → tip-wins absorb (v27.1.1)

```text
ggen_tip_before=c2cd54f562f1fe675d9ea5fabf989b89a9f8a796
content_absorb=2685119bd92e607cdb93d3dafdbb80cd9da0447d
sidebranch=cursor/v2711-uilayout-exact-retip5-bc54@7b5453e67a1968d27f16a4a02af931579e2d82e4
absorb_branch=cursor/v2711-uilayout-exact-retip5-absorb-25e7
side_parent=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
court=ui_layout_court
budget_s=180
exit=124
prior_exact_alive=a508e6bae (11 passed; lineage held; sources ≡ tip)
prior_retip4=cursor/v2711-uilayout-exact-retip4-bc54 NEVER_ON_ORIGIN
standing=PARTIAL_ALIVE (EXIT:124 thrash; lineage ALIVE held; tip-wins absorb)
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-EXACT-RETIP5-STANDING` (tip-wins absorb) |
| Sidebranch | `cursor/v2711-uilayout-exact-retip5-bc54` @ `7b5453e67a1968d27f16a4a02af931579e2d82e4` |
| Absorb branch | `cursor/v2711-uilayout-exact-retip5-absorb-25e7` |
| Tip before (content absorb) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c2cd54f562f1fe675d9ea5fabf989b89a9f8a796` (Taffy AOT retip7 tip-wins; raced past side parent `167352d0d`) |
| Content absorb SHA | `2685119bd92e607cdb93d3dafdbb80cd9da0447d` (sidebranch standing blob ≡ tip; parent `c2cd54f56`) |
| Side parent | `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` (Truex exact-tip4 tip-wins) |
| Agent (side) | `bc-73c09a80-9a28-5884-9711-24917d8fc3d3` |
| Observed at (sidebranch) | `2026-11-22T17:37:49Z`→`17:40:49Z` |
| Court | `cargo test -p ggen-graph --test ui_layout_court` |
| Budget | **180s** hard `timeout` (push-first: &lt;3min else PARTIAL) |
| This probe | **EXIT:124** (timeout) — cold `--target-dir /tmp/ggen-uilayout-exact-retip5`; still compiling deps (reached `purrdf-core`); **no** test binary; **no** pass/fail count |
| Thrash (start) | **13** concurrent `cargo` (oxrocksdb thrash elsewhere) |
| Log / status | `/tmp/cursor/uilayout-exact-retip5.log` · `/tmp/cursor/uilayout-exact-retip5.status` → `EXIT:124` |
| Layout source drift vs prior ALIVE | **none** — `crates/ggen-graph/src/layout/` + `tests/ui_layout_court.rs` ≡ cargo tip `a508e6bae9e61d7894ad98df8c507fdb1fb28eb2` |
| Prior exact tip-cargo ALIVE | **held (lineage)** — **11 passed; EXIT:0** @ `a508e6bae` (`bc-70188ac3`; [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)) — **not** reconfirmed this retip5 |
| Prior tip4 | `cursor/v2711-uilayout-exact-retip4-bc54` — **never appeared on origin** (falsifier that forced retip5 push-first) |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** |
| Crown absorb | tip-wins content @ `2685119bd` onto `c2cd54f56`; ledger honesty via `cursor/v2711-uilayout-exact-retip5-absorb-25e7` (FF only; no force-push) |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Retip5 push-first honesty: exclusive tip-cargo did **not** complete inside the 180s budget under thrash → standing **PARTIAL_ALIVE**. Prior exact tip-cargo **ALIVE** @ `a508e6bae` remains lineage evidence only. This absorb does **not** mint tip EXIT:0 `ALIVE`, Gate-1 hop-3b exact-tip reconfirm `ALIVE`, photolithography path crown, or Foundry zero-HITL `ALIVE`. Standing remains **PARTIAL_ALIVE**.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries `ui_layout` + `ui_layout_court` ≡ prior ALIVE tip `a508e6bae` | **YES** |
| Tip-cargo `cargo test -p ggen-graph --test ui_layout_court` EXIT 0 @ live tip within 180s | **NO** — EXIT:**124** under thrash; no test binary |
| `origin` ref for tip4 | **ABSENT** — falsifier that forced tip5 push-first |

---

## Tip-wins absorb (2026-11-22)

| Field | Value |
|-------|-------|
| Tip before | `c2cd54f562f1fe675d9ea5fabf989b89a9f8a796` |
| Sidebranch content | `7b5453e67a1968d27f16a4a02af931579e2d82e4` (parent `167352d0d`; tip raced → shui/`dd54e7821` → Gate-1/brand/Taffy → `c2cd54f56`) |
| Content absorb | `2685119bd` (blob ≡ sidebranch standing; reapply onto live tip) |
| Tip-cargo | **PARTIAL_ALIVE held** — EXIT:124; EXIT:0 not claimed |
| Foundry | **not** promoted (stays PARTIAL / `--live` BLOCKED_HITL lineage) |
| Push policy | FF-only onto `cursor/ecosystem-spec-27-1-1-bc54`; **no** force-push; **no** ManagePullRequest |

---

## Exact next verifier

Falsifier for promoting this retip to **ALIVE (exact tip-cargo)** on tip `2685119bd` (or its live FF successor with layout sources ≡ `a508e6bae`):

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 2685119bd; layout/ui_layout + ui_layout_court ≡ a508e6bae
df -h /tmp
# prefer thrash=0 concurrent oxrocksdb cargo
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test ui_layout_court --locked \
  --target-dir /tmp/ggen-uilayout-exact-retip5 \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 11 passed; EXIT:0
```

Until that EXIT **0**, keep standing **PARTIAL_ALIVE** (budget miss / thrash) — lineage ALIVE @ `a508e6bae` is **not** a substitute for tip reconfirm.

---

## Compact return

```text
tip_before=c2cd54f562f1fe675d9ea5fabf989b89a9f8a796
tip_after=2685119bd92e607cdb93d3dafdbb80cd9da0447d
sidebranch=cursor/v2711-uilayout-exact-retip5-bc54@7b5453e67
absorb=cursor/v2711-uilayout-exact-retip5-absorb-25e7
EXIT=124
standing=PARTIAL_ALIVE held
prior_alive=a508e6bae (lineage; 11 passed)
prior_retip4=NEVER_ON_ORIGIN
next=exclusive ui_layout_court jobs=1 → expect 11/EXIT:0
Foundry=NOT_CLAIMED ManagePullRequest=not_used force_push=none
```
