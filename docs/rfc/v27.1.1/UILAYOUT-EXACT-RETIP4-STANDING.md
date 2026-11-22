# ui_layout_court — exact-tip retip4 push-first honesty (v27.1.1)

```text
tip_base=2d7b47b2ea5eb5108f52fa46682a68a1af7e0097
tip_base_requested_floor=8c8b1e6e5ed7314a4e37f3fdc6078e4383aecd26
branch=cursor/v2711-uilayout-exact-retip4-bc54
bc=bc-d4c111fb-e29a-5721-92a7-eb810b87bf78
court=ui_layout_court
tip_cargo_exit=137
chicago_exit=0
chicago_passed=11
standing=PARTIAL_ALIVE
prior_exact_alive=a508e6bae (11 passed; lineage held; sources ≡ tip)
crown_absorb=none
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-EXACT-RETIP4-STANDING` |
| Branch | `cursor/v2711-uilayout-exact-retip4-bc54` |
| Agent | `bc-d4c111fb-e29a-5721-92a7-eb810b87bf78` |
| Observed tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2d7b47b2ea5eb5108f52fa46682a68a1af7e0097` (⊇ requested floor `8c8b1e6e5`) |
| Court (package) | `cargo test -p ggen-graph --test ui_layout_court` |
| Package tip-cargo this probe | **EXIT:137** (SIGKILL) @ `2026-11-22T17:50:13Z` — exclusive lock held; `jobs=1`; target `/tmp/ggen-uilayout-exact-retip4`; killed by foreign `/tmp/gate1-exact-tip3/guardian.py` (`ALLOWED=/tmp/gate1-exact-tip3/exclusive` only). Prior exclusive queue waiters also SIGKILL'd (EXIT:137). **No** test binary; **no** pass/fail count from package cargo. |
| Chicago oxigraph-free backup | **EXIT:0** — **11 passed; 0 failed** @ `2026-11-22T17:40:55Z`→`17:41:19Z` — tip-synced `layout/{taffy_aot,remediation_bound,ui_layout}` + rewritten `tests/ui_layout_court.rs` under `/tmp/ggen-uilayout-exact-retip4-chicago` (nightly-2026-06-22). Standing for mirror alone: **PARTIAL_ALIVE** (not package tip-cargo). |
| Thrash / blockers | Persistent multi-agent oxrocksdb thrash; later tip3 guardian monopolizes machine and SIGKILLs non-`gate1-exact-tip3/exclusive` cargo/rustc/cc1plus |
| Log / status | `/tmp/cursor/uilayout-exact-retip4.log` · `/tmp/cursor/uilayout-exact-retip4.status` → `137` · chicago `/tmp/cursor/uilayout-exact-retip4-chicago.log` → `0` |
| Layout source drift vs prior ALIVE | **none** — `crates/ggen-graph/src/layout/{ui_layout,taffy_aot,remediation_bound}.rs` + `tests/ui_layout_court.rs` ≡ cargo tip `a508e6bae9e61d7894ad98df8c507fdb1fb28eb2` |
| Prior exact tip-cargo ALIVE | **held (lineage)** — **11 passed; EXIT:0** @ `a508e6bae` (`bc-70188ac3`; [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)) — **not** reconfirmed this retip4 |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** |
| Path / Foundry crown | **not claimed** (no Foundry `ALIVE`) |
| Crown absorb | **none** (push-first sidebranch only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Retip4 push-first honesty: package tip-cargo did **not** complete (EXIT:137 SIGKILL under foreign exclusive guardian / prior thrash) → standing **PARTIAL_ALIVE**. Chicago oxigraph-free mirror proved the same 11 court assertions (**EXIT:0**) as backup only — it does **not** promote Gate-1 hop-3b to exact tip-cargo `ALIVE`. Prior exact tip-cargo **ALIVE** @ `a508e6bae` remains lineage evidence only. Do **not** invent photolithography path crown or Foundry zero-HITL `ALIVE`. No crown absorb.

---

## Exact next verifier

Falsifier for promoting this retip to **ALIVE (exact tip-cargo)** on tip `2d7b47b2e` (or its live FF successor with layout sources ≡):

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 2d7b47b2e (⊇ 8c8b1e6e5); layout/ui_layout + ui_layout_court ≡ a508e6bae
df -h /tmp
# prefer thrash=0 and no foreign guardian killing non-allowed targets
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test ui_layout_court --locked \
  --target-dir /tmp/ggen-uilayout-exact-retip4 \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 11 passed; EXIT:0
```

Until that EXIT **0**, keep standing **PARTIAL_ALIVE** — lineage ALIVE @ `a508e6bae` and Chicago EXIT:0 are **not** substitutes for package tip-cargo reconfirm.

---

## Compact return

```text
tip_base=2d7b47b2ea5eb5108f52fa46682a68a1af7e0097
remote_sha=<fill after git push + ls-remote>
branch=cursor/v2711-uilayout-exact-retip4-bc54
EXIT=137
chicago_EXIT=0
standing=PARTIAL_ALIVE
next=exclusive ui_layout_court jobs=1 without foreign SIGKILL guardian → expect 11/EXIT:0
crown_absorb=none ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
