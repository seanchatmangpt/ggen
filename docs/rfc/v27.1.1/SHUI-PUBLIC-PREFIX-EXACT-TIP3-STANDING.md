# shui_public_prefix_court — exact-tip3 ALIVE (v27.1.1)

```text
tip=3afe4545e97f8992c4e3af5d61001c6d2b38ecd3
branch=cursor/v2711-shui-exact-tip3-bc54
bc=bc-fafc690f-44f9-5b50-a390-5c6c1d8d7c56
court=shui_public_prefix_court
passed=4
failed=0
exit=0
standing=ALIVE (exact tip-cargo)
target=/tmp/only-fafc690f-tip3-courts
crown_absorb=none ManagePullRequest=not_used Foundry=NOT_CLAIMED
```

| Field | Value |
|-------|-------|
| Observed tip | `3afe4545e97f8992c4e3af5d61001c6d2b38ecd3` |
| Court blob | `3cda065f` |
| Command | `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test shui_public_prefix_court --locked --target-dir /tmp/only-fafc690f-tip3-courts --config build.jobs=1 -- --test-threads=1` |
| Result | **4 passed; 0 failed; EXIT 0** @ 2026-11-22T18:44:31Z |
| Log | `/tmp/gate1-exact-tip3/shui.log` |
| Lineage | prior ALIVE @ `008a81d21` superseded by this tip-subject EXIT 0 |

**Authority.** Exact tip-cargo EXIT **0** promotes Gate-1 `shui:` fence to **ALIVE (exact tip-cargo)**. Not Foundry ALIVE; not Truex product crown; no crown absorb.

## Replay

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
# tip ≡ 3afe4545e97f8992c4e3af5d61001c6d2b38ecd3 (or later tip-wins with court blob 3cda065f)
CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --target-dir /tmp/only-fafc690f-tip3-courts --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; EXIT 0
```
