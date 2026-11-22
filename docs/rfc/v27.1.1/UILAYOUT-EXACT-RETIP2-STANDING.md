# ui_layout_court — exact-retip2 honesty (v27.1.1)

```text
ggen_tip_base=0c18b3d1a7aff7fd8e804743c61685ab0d428918
branch=cursor/v2711-uilayout-exact-retip2-bc54
court=ui_layout_court
prior_alive=a508e6bae (exact tip-cargo 11 passed)
package_tip_cargo_exit=137
chicago_mirror_exit=0
chicago_passed=11
standing=PARTIAL_ALIVE (package tip-cargo EXIT:137; Chicago oxigraph-free mirror EXIT:0 backup)
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-EXACT-RETIP2-STANDING` |
| Branch | `cursor/v2711-uilayout-exact-retip2-bc54` |
| Tip base | `0c18b3d1a7aff7fd8e804743c61685ab0d428918` |
| Observed at | `2026-11-22T17:51:49Z` |
| Court blob on tip | `1dfe6bdfab2cb8dfd35fa7870692185cf004ab4c` (**11** `#[test]`; ≡ prior ALIVE @ `a508e6bae`) |
| Package tip-cargo | private target → **EXIT:137** (OOM/SIGKILL under oxrocksdb thrash) |
| Chicago oxigraph-free backup | binary `ui_layout_court-1f4c9a9c0f88a459` → **11 passed; EXIT:0** — standing **PARTIAL_ALIVE** only (≠ package tip-cargo ALIVE) |
| Prior exact tip-cargo ALIVE | **11 passed; EXIT 0** @ `a508e6bae` (`bc-70188ac3`) retained ([`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)) |
| Sibling `.ts` | **UNSUPPORTED** |
| Prior exact-retip | origin **ABSENT** for tip2 name — push-first |
| Crown absorb | **not performed** |
| ManagePullRequest | **not used** |

**Authority.** Package tip-cargo **ALIVE** requires EXIT **0** for `cargo test -p ggen-graph --test ui_layout_court` on this tip. Chicago mirror EXIT 0 is backup **PARTIAL_ALIVE** only. Do not invent photolithography path crown or Foundry `ALIVE`.

## Exact next verifier

```bash
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
cargo test -p ggen-graph --test ui_layout_court --locked \
  --target-dir /tmp/ggen-uilayout-exact-retip2 \
  --config build.jobs=1 -- --test-threads=1
# expect: 11 passed; EXIT 0 → promote to tip-cargo ALIVE
```

## Compact return

```text
tip_base=0c18b3d1a7aff7fd8e804743c61685ab0d428918
branch=cursor/v2711-uilayout-exact-retip2-bc54
package_EXIT=137
chicago_EXIT=0 chicago_passed=11
standing=PARTIAL_ALIVE
prior_alive=a508e6bae
crown_absorb=none ManagePullRequest=not_used
```
