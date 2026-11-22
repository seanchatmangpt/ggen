# taffy_aot_preflight_court — retip4 honesty (v27.1.1)

```text
ggen_tip_base=0c18b3d1a7aff7fd8e804743c61685ab0d428918
branch=cursor/v2711-taffy-preflight-retip4-bc54
court=taffy_aot_preflight_court
prior_alive=19c08a3b0 / cde02d1f2 (tip-cargo 7 passed)
tip_cargo_exit=137
standing=PARTIAL_ALIVE (lineage ALIVE cited; tip EXIT:0 not obtained)
```

| Field | Value |
|-------|-------|
| Document ID | `TAFFY-AOT-PREFLIGHT-RETIP4-STANDING` |
| Branch | `cursor/v2711-taffy-preflight-retip4-bc54` |
| Tip base | `0c18b3d1a7aff7fd8e804743c61685ab0d428918` |
| Observed at | `2026-11-22T17:51:49Z` |
| Court blob on tip | `d8923b07eb4325ace90087a604b9daec0a83d29e` (**7** `#[test]`; ≡ prior ALIVE) |
| Package tip-cargo | private target → **EXIT:137** (OOM/SIGKILL under oxrocksdb thrash) |
| Prior tip-cargo ALIVE | **7 passed; EXIT 0** @ crown `19c08a3b0` absorb `cde02d1f2` retained |
| Photolithography path | remains **PARTIAL_ALIVE** (consumer bind open) — fence ≠ path crown |
| Prior retip / tip3 | origin **ABSENT** for retip4 name — push-first |
| Crown absorb | **not performed** |
| ManagePullRequest | **not used** |

## Exact next verifier

```bash
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/gate1-taffy-retip4
cargo test -p ggen-graph --test taffy_aot_preflight_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 7 passed; EXIT 0
```

## Compact return

```text
tip_base=0c18b3d1a7aff7fd8e804743c61685ab0d428918
branch=cursor/v2711-taffy-preflight-retip4-bc54
EXIT=137
standing=PARTIAL_ALIVE
prior_alive=19c08a3b0/cde02d1f2
crown_absorb=none ManagePullRequest=not_used
```
