# brand_toml_hotswap_court — exact-retip2 honesty (v27.1.1)

```text
ggen_tip_base=0c18b3d1a7aff7fd8e804743c61685ab0d428918
branch=cursor/v2711-brand-exact-retip2-bc54
court=brand_toml_hotswap_court
prior_alive=19c08a3b0 / cde02d1f2 (tip-cargo 6 passed)
tip_cargo_exit=137
standing=PARTIAL_ALIVE (lineage ALIVE cited; tip EXIT:0 not obtained)
production_theme=UNSUPPORTED (REFUSED_PRODUCTION)
```

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-HOTSWAP-EXACT-RETIP2-STANDING` |
| Branch | `cursor/v2711-brand-exact-retip2-bc54` |
| Tip base | `0c18b3d1a7aff7fd8e804743c61685ab0d428918` |
| Observed at | `2026-11-22T17:51:49Z` |
| Court blob on tip | `22e417ab307428fdcaf228059923fec644ce57c3` (≡ prior ALIVE @ `19c08a3b0`) |
| Package tip-cargo | private target → **EXIT:137** (OOM/SIGKILL under oxrocksdb thrash; cgroup `memory.max` ~15.7GB) |
| Prior tip-cargo ALIVE | **6 passed; EXIT 0** absorb @ `cde02d1f2` / crown `19c08a3b0` retained |
| Production theme | **UNSUPPORTED** / `REFUSED_PRODUCTION` — never claimed ALIVE |
| Prior exact-retip / tip4 | origin **ABSENT** for tip2 name — push-first |
| Crown absorb | **tip-wins** with brand-hotswap-prod-retip onto crown |
| ManagePullRequest | **not used** |

**Authority.** Hot-swap fence lineage tip-cargo **ALIVE** is cited. This retip does **not** mint tip-subject hot-swap `ALIVE` or production-theme `ALIVE`.

## Exact next verifier

```bash
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/ggen-brand-hotswap-exact-retip2
cargo test -p ggen-graph --test brand_toml_hotswap_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 6 passed; EXIT 0
```

## Compact return

```text
tip_base=0c18b3d1a7aff7fd8e804743c61685ab0d428918
branch=cursor/v2711-brand-exact-retip2-bc54
EXIT=137
standing=PARTIAL_ALIVE
prior_alive=19c08a3b0/cde02d1f2
production=UNSUPPORTED
crown_absorb=none ManagePullRequest=not_used
```
