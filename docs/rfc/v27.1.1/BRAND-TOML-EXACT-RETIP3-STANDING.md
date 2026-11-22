# brand.toml courts — exact-retip3 push-first honesty (v27.1.1)

```text
tip=3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed
branch=cursor/v2711-brand-exact-retip3-bc54
courts=brand_toml_completeness_court + brand_toml_rhythm_court
exit=NOT_RUN
standing=PARTIAL_ALIVE
lineage_alive=77eb46fd0 (5+12 EXIT 0 exact tip-cargo)
production_theme=UNSUPPORTED (REFUSED_PRODUCTION)
crown_absorb=none ManagePullRequest=not_used Foundry=NOT_CLAIMED
```

| Field | Value |
|-------|-------|
| Observed tip | `3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed` |
| Completeness blob | `a07477c6` |
| Rhythm blob | `4843ed9f` |
| Replay | `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/only-fafc690f-tip3-courts --config build.jobs=1 -- --test-threads=1` |
| This probe | **EXIT:NOT_RUN** @ 2026-11-22T18:25:59Z — serialized behind shui oxrocksdb under thrash; prior exact tip-cargo ALIVE @ `77eb46fd0` held as lineage |
| Production theme | **UNSUPPORTED** |

**Authority.** No tip EXIT 0 this land → **PARTIAL_ALIVE**. Lineage ALIVE ≠ tip-subject ALIVE. No Foundry; no crown.
