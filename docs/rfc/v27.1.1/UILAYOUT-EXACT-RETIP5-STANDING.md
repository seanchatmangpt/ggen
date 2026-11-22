# ui_layout_court — exact-tip retip5 push-first honesty (v27.1.1)

```text
tip_base=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
branch=cursor/v2711-uilayout-exact-retip5-bc54
bc=bc-73c09a80-9a28-5884-9711-24917d8fc3d3
court=ui_layout_court
budget_s=180
exit=124
standing=PARTIAL_ALIVE
prior_exact_alive=a508e6bae (11 passed; lineage held; sources ≡ tip)
prior_retip4=cursor/v2711-uilayout-exact-retip4-bc54 NEVER_ON_ORIGIN
crown_absorb=none
ManagePullRequest=not_used
force_push=none
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-EXACT-RETIP5-STANDING` |
| Branch | `cursor/v2711-uilayout-exact-retip5-bc54` |
| Agent | `bc-73c09a80-9a28-5884-9711-24917d8fc3d3` |
| Observed tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` (Truex exact-tip4 tip-wins absorb) |
| Court | `cargo test -p ggen-graph --test ui_layout_court` |
| Budget | **180s** hard `timeout` (push-first: &lt;3min else PARTIAL) |
| This probe | **EXIT:124** (timeout) @ `2026-11-22T17:37:49Z`→`17:40:49Z` — cold `--target-dir /tmp/ggen-uilayout-exact-retip5`; still compiling deps (reached `purrdf-core`); **no** test binary; **no** pass/fail count |
| Thrash (start) | **13** concurrent `cargo` (oxrocksdb thrash elsewhere) |
| Log / status | `/tmp/cursor/uilayout-exact-retip5.log` · `/tmp/cursor/uilayout-exact-retip5.status` → `EXIT:124` |
| Layout source drift vs prior ALIVE | **none** — `crates/ggen-graph/src/layout/` + `tests/ui_layout_court.rs` ≡ cargo tip `a508e6bae9e61d7894ad98df8c507fdb1fb28eb2` |
| Prior exact tip-cargo ALIVE | **held (lineage)** — **11 passed; EXIT:0** @ `a508e6bae` (`bc-70188ac3`; [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)) — **not** reconfirmed this retip5 |
| Prior tip4 | `cursor/v2711-uilayout-exact-retip4-bc54` base was stale tip `8c8b1e6e5`; local only; `git ls-remote origin` **empty** — **never appeared on origin** (this retip5 exists to land push) |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** |
| Path / Foundry crown | **not claimed** |
| Crown absorb | **none** (push-first sidebranch only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Retip5 push-first honesty: exclusive tip-cargo did **not** complete inside the 180s budget under thrash → standing **PARTIAL_ALIVE**. Prior exact tip-cargo **ALIVE** @ `a508e6bae` remains lineage evidence only. Do **not** invent Gate-1 hop-3b exact-tip reconfirm `ALIVE`, photolithography path crown, or Foundry zero-HITL `ALIVE` from this land.

---

## Exact next verifier

Falsifier for promoting this retip to **ALIVE (exact tip-cargo)** on tip `167352d0d` (or its live FF successor with layout sources ≡):

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 167352d0d; layout/ui_layout + ui_layout_court ≡ a508e6bae
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
tip_base=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
branch=cursor/v2711-uilayout-exact-retip5-bc54
EXIT=124
standing=PARTIAL_ALIVE
next=exclusive ui_layout_court jobs=1 → expect 11/EXIT:0
prior_retip4=NEVER_ON_ORIGIN
crown_absorb=none ManagePullRequest=not_used force_push=none
```
