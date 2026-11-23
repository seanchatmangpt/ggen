# ui_layout_court — exact-tip6 tip-cargo ALIVE (v27.1.1)

```text
tip_base=ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4
cargo_tip=2e452873ad1229e3fdf3b9a1f8a91ae5105d4e4c
branch=cursor/v2711-uilayout-exact-tip6-bc54
bc=bc-262603dc-8fb9-50aa-93a9-5a3190c363b9
court=ui_layout_court
passed=11
failed=0
exit=0
standing=ALIVE (exact tip-cargo)
prior_tip6_partial=bc-c92c420b EXIT137 PARTIAL_ALIVE (superseded)
prior_exact_alive=a508e6bae (lineage held; tip-subject now EXIT0)
blob=1dfe6bdfab2cb8dfd35fa7870692185cf004ab4c
ManagePullRequest=not_used
force_push=sidebranch_lease_only
Foundry_ALIVE=not_claimed
sibling_ts=UNSUPPORTED
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-EXACT-TIP6-STANDING` |
| Branch | `cursor/v2711-uilayout-exact-tip6-bc54` |
| Agent | `bc-262603dc-8fb9-50aa-93a9-5a3190c363b9` |
| Tip base (push) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4` |
| Cargo tip (EXIT0) | `2e452873ad1229e3fdf3b9a1f8a91ae5105d4e4c` — layout/ui_layout + court blob `1dfe6bdf` ≡ tip |
| Observed at | `2026-11-22T19:37:23Z`→`19:38:45Z` |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-tip6 --config build.jobs=1 -- --test-threads=1` |
| Result | **11 passed; EXIT 0** |
| Prior tip6 PARTIAL | `75b909dac` / `bc-c92c420b` EXIT **137** — **superseded** |
| Prior exact tip-cargo ALIVE | `a508e6bae` lineage retained; tip6 tip-subject now **ALIVE** |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** |
| Log / status | `/tmp/cursor/uilayout-exact-tip6-court.log` · `/tmp/cursor/uilayout-exact-tip6.status` → `UIL_EXIT=0` |
| ManagePullRequest | **not used** |
| Foundry ALIVE | **not claimed** |
| Path crown | **not** promoted |

**Authority.** Exact tip-cargo EXIT **0** promotes Gate-1 hop-3b `ui_layout_court` tip6 to **ALIVE (exact tip-cargo)**. Do **not** invent Foundry zero-HITL or photolithography path crown `ALIVE`. Sibling `.ts` remains **UNSUPPORTED**.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `ui_layout_court` | `/tmp/ggen-uilayout-exact-tip6` | **11 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

---

## Compact return

```text
tip_base=ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4
cargo_tip=2e452873ad1229e3fdf3b9a1f8a91ae5105d4e4c
branch=cursor/v2711-uilayout-exact-tip6-bc54
EXIT=0 standing=ALIVE (exact tip-cargo) passed=11
prior_partial=EXIT137 superseded
Foundry_ALIVE=not_claimed sibling_ts=UNSUPPORTED ManagePullRequest=not_used
```
