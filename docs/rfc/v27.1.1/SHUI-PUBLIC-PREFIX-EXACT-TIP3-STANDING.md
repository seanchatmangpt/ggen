# shui_public_prefix_court — exact-tip3 push-first honesty (v27.1.1)

```text
tip=3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed
branch=cursor/v2711-shui-exact-tip3-bc54
bc=bc-fafc690f-44f9-5b50-a390-5c6c1d8d7c56
court=shui_public_prefix_court
exit=COMPILING
standing=PARTIAL_ALIVE
lineage_alive=008a81d21 (4 passed; blob 3cda065f ≡ tip)
crown_absorb=none ManagePullRequest=not_used Foundry=NOT_CLAIMED
```

| Field | Value |
|-------|-------|
| Observed tip | `3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed` |
| Court blob | `3cda065f` (≡ lineage ALIVE @ `008a81d21`) |
| Command | `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test shui_public_prefix_court --locked --target-dir /tmp/only-fafc690f-tip3-courts --config build.jobs=1 -- --test-threads=1` |
| This probe | **EXIT:COMPILING** @ 2026-11-22T18:25:59Z — oxrocksdb-sys rebuild under pod thrash (n≈3 cargo; loadavg 3.12 4.93 5.27); guardian kills rival cc1plus |
| Log | `/tmp/gate1-exact-tip3/shui.log` |
| Lineage tip-cargo ALIVE | **held** — 4 passed EXIT 0 @ `008a81d21` — **not** tip-subject substitute |

**Authority.** Tip3 push-first: no tip EXIT 0 yet → **PARTIAL_ALIVE**. Do not invent tip-subject ALIVE from blob≡lineage. No Foundry ALIVE; no crown absorb.

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
export CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --target-dir /tmp/only-fafc690f-tip3-courts --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; EXIT 0 → promote to tip-cargo ALIVE
```
