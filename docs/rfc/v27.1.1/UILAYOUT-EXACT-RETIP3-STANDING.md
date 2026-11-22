# ui_layout_court — exact-retip3 push-first honesty (v27.1.1)

```text
tip=3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed
branch=cursor/v2711-uilayout-exact-retip3-bc54
court=ui_layout_court
exit=NOT_RUN
standing=PARTIAL_ALIVE
lineage_alive=a508e6bae (11 passed EXIT 0 exact tip-cargo)
blob=1dfe6bdf
sibling_ts=UNSUPPORTED
crown_absorb=none ManagePullRequest=not_used Foundry=NOT_CLAIMED
```

| Field | Value |
|-------|-------|
| Observed tip | `3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed` |
| Court blob | `1dfe6bdf` |
| Replay | `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/only-fafc690f-tip3-courts --config build.jobs=1 -- --test-threads=1` |
| This probe | **EXIT:NOT_RUN** @ 2026-11-22T18:25:59Z — exclusive queue behind shui; thrash |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** here |

**Authority.** No tip EXIT 0 → **PARTIAL_ALIVE**. Lineage exact tip-cargo ALIVE @ `a508e6bae` held. No path-crown / Foundry ALIVE.
