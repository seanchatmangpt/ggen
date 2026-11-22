# ui_layout_court — exact-tip retip6 push-first honesty (v27.1.1)

```text
tip_base=dd54e7821accd2bfc8f2966a7fe78046edb7102b
tip_base_prior=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
branch=cursor/v2711-uilayout-exact-retip6-bc54
bc=bc-35ebf7f4-32d2-5a8d-b21d-b5e7c14d8a3f
court=ui_layout_court
budget_s=180
exit=124
standing=PARTIAL_ALIVE
prior_exact_alive=a508e6bae (11 passed; lineage held; sources ≡ tip)
prior_retip4=cursor/v2711-uilayout-exact-retip4-bc54 NEVER_ON_ORIGIN
prior_retip5=cursor/v2711-uilayout-exact-retip5-bc54 @ 7b5453e67 (PARTIAL EXIT:124; on origin)
crown_absorb=none
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-EXACT-RETIP6-STANDING` |
| Branch | `cursor/v2711-uilayout-exact-retip6-bc54` |
| Agent | `bc-35ebf7f4-32d2-5a8d-b21d-b5e7c14d8a3f` |
| Observed tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `dd54e7821accd2bfc8f2966a7fe78046edb7102b` (tip-wins shui exact-tip5 PARTIAL absorb; ⊇ `167352d0d`) |
| Court | `cargo test -p ggen-graph --test ui_layout_court` |
| Budget | **180s** hard `timeout` (push-first: &lt;3min else PARTIAL) |
| This probe | **EXIT:124** (timeout) @ `2026-11-22T17:43:32Z`→`17:46:32Z` — seeded `--target-dir /tmp/ggen-uilayout-exact-retip6` from retip5 warm; still compiling deps (reached `oxrocksdb-sys` / `zerovec`); **no** test binary; **no** pass/fail count |
| Thrash (start) | **44** concurrent `cargo`/`oxrocksdb` processes |
| Log / status | `/tmp/cursor/uilayout-exact-retip6.log` · `/tmp/cursor/uilayout-exact-retip6.status` → `EXIT:124` |
| Layout source drift vs prior ALIVE | **none** — `crates/ggen-graph/src/layout/` + `tests/ui_layout_court.rs` ≡ cargo tip `a508e6bae9e61d7894ad98df8c507fdb1fb28eb2` |
| Prior exact tip-cargo ALIVE | **held (lineage)** — **11 passed; EXIT:0** @ `a508e6bae` (`bc-70188ac3`; [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)) — **not** reconfirmed this retip6 |
| Prior tip4 | `cursor/v2711-uilayout-exact-retip4-bc54` — `git ls-remote origin` **empty** — **never on origin** |
| Prior tip5 | `cursor/v2711-uilayout-exact-retip5-bc54` @ `7b5453e67` — **PARTIAL** EXIT:124 on origin; retip6 is push-first successor under continued thrash |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** |
| Path / Foundry crown | **not claimed** (no Foundry `ALIVE`) |
| Crown absorb | **none** (push-first sidebranch only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Retip6 push-first honesty: exclusive tip-cargo did **not** complete inside the 180s budget under thrash → standing **PARTIAL_ALIVE**. Prior exact tip-cargo **ALIVE** @ `a508e6bae` remains lineage evidence only. Do **not** invent Gate-1 hop-3b exact-tip reconfirm `ALIVE`, photolithography path crown, or Foundry zero-HITL `ALIVE` from this land. No crown absorb.

---

## Exact next verifier

Falsifier for promoting this retip to **ALIVE (exact tip-cargo)** on tip `dd54e7821` (or its live FF successor with layout sources ≡):

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ dd54e7821 (⊇ 167352d0d); layout/ui_layout + ui_layout_court ≡ a508e6bae
df -h /tmp
# prefer thrash=0 concurrent oxrocksdb cargo
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test ui_layout_court --locked \
  --target-dir /tmp/ggen-uilayout-exact-retip6 \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 11 passed; EXIT:0
```

Until that EXIT **0**, keep standing **PARTIAL_ALIVE** (budget miss / thrash) — lineage ALIVE @ `a508e6bae` is **not** a substitute for tip reconfirm.

---

## Compact return

```text
tip_base=dd54e7821accd2bfc8f2966a7fe78046edb7102b
remote_sha=<fill after git push + ls-remote>
branch=cursor/v2711-uilayout-exact-retip6-bc54
EXIT=124
standing=PARTIAL_ALIVE
next=exclusive ui_layout_court jobs=1 → expect 11/EXIT:0
prior_retip4=NEVER_ON_ORIGIN
prior_retip5=ON_ORIGIN_PARTIAL
crown_absorb=none ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
