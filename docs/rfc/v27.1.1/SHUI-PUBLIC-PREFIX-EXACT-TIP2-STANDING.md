# shui_public_prefix_court — exact-tip2 honesty retip (v27.1.1)

```text
ggen_tip_base=0c18b3d1a7aff7fd8e804743c61685ab0d428918
branch=cursor/v2711-shui-exact-tip2-bc54
court=shui_public_prefix_court
prior_alive=008a81d21 (lineage tip-cargo) / 26256ca9d (gate1-next)
tip_cargo_exit=137
standing=PARTIAL_ALIVE (lineage ALIVE cited; tip EXIT:0 not obtained)
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-EXACT-TIP2-STANDING` |
| Branch | `cursor/v2711-shui-exact-tip2-bc54` |
| Tip base | `0c18b3d1a7aff7fd8e804743c61685ab0d428918` (`origin/cursor/ecosystem-spec-27-1-1-bc54`) |
| Observed at | `2026-11-22T17:51:16Z` |
| Court blob on tip | `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior ALIVE) |
| Package tip-cargo | `CARGO_TARGET_DIR=/tmp/gate1-exact-tip2/shui-target` → **EXIT:137** (SIGKILL/OOM; cgroup peak hit `memory.max` ~15.7GB; `oom_kill` counter ≥37 on pod) |
| Warm seed | reused tip4-era `libggen_graph` + `librocksdb.a` under private target; cargo still attempted `oxrocksdb-sys` rebuild (fingerprint `ed7d45b0`) and was OOM-killed |
| Prior tip-cargo ALIVE | lineage **4 passed; EXIT 0** @ `008a81d21` / `26256ca9d` retained |
| Prior exact-tip / tip4 | local-only / origin **ABSENT** — falsifier forcing tip2 push-first |
| Crown absorb | **not performed** |
| ManagePullRequest | **not used** |
| Force-push | **none** |

**Authority.** Lineage tip-cargo **ALIVE** is cited. This retip does **not** mint tip-subject `ALIVE`, Foundry `ALIVE`, or Truex product admit standing. Do not invent tip-cargo `ALIVE` from source presence or warm rlib alone.

## Exact next verifier

```bash
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip2
# Prefer quiet window (loadavg ≤5; no competing oxrocksdb; cgroup free ≥8GB)
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; EXIT 0 → promote to tip-cargo ALIVE
```

## Compact return

```text
tip_base=0c18b3d1a7aff7fd8e804743c61685ab0d428918
branch=cursor/v2711-shui-exact-tip2-bc54
EXIT=137
standing=PARTIAL_ALIVE
prior_alive=008a81d21/26256ca9d
crown_absorb=none ManagePullRequest=not_used
```
