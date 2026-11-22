# shui_public_prefix_court — exact-tip5 ALIVE reconfirm attempt (v27.1.1)

```text
ggen_tip_base=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
branch=cursor/v2711-shui-exact-tip5-alive-bc54
court=shui_public_prefix_court
prior_tip5_partial=d96b2e867 (cursor/v2711-shui-exact-tip5-bc54; PARTIAL_ALIVE push-first)
prior_alive=008a81d21 (lineage tip-cargo; crown reconfirm) / 26256ca9d (gate1-next)
tip_cargo_exit=137
standing=PARTIAL_ALIVE (tip EXIT:0 not obtained; OOM SIGKILL during cold oxrocksdb)
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING` |
| Branch | `cursor/v2711-shui-exact-tip5-alive-bc54` |
| Tip base | `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` (`docs(rfc): tip-wins absorb Truex exact-tip4 admit-court onto crown` on `origin/cursor/ecosystem-spec-27-1-1-bc54`) |
| Observed at | `2026-11-22T17:40:03Z` → `2026-11-22T17:47:30Z` (~7.5 min wall) |
| Court sources on tip | `crates/ggen-graph/tests/shui_public_prefix_court.rs` blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior ALIVE blob) |
| sha256 (tip working tree) | court `bd032856…5fe0`; `public_prefixes.rs` `bff5aa82…ee5c`; `shui.rs` `d7040f32…78d2` |
| Prior tip5 honesty | `cursor/v2711-shui-exact-tip5-bc54` @ `d96b2e867f7afe26e4000195fc02010c62dcbc8a` — **PARTIAL_ALIVE** (push-first; no EXIT:0 within 3min) |
| Prior tip-cargo ALIVE | `cursor/v2711-shui-prefix-alive-bc54` @ `9c8b68c2947ec4ad28b03c2a775a4e0c6b7bf87d` pinning crown reconfirm **ALIVE** @ `008a81d21` (4 passed / EXIT 0; target `/tmp/ggen-shui-prefix-target2`) |
| Exact command | `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/ggen-shui-exact-tip5` (isolated; freshly created) |
| EXIT | **137** (SIGKILL) |
| Diagnostic | cold compile reached `oxrocksdb-sys` / late crate graph; process **Killed**; `dmesg` shows `oom-kill` of `rustc` under concurrent oxrocksdb cold builds (`/tmp/ggen-shui-exact-tip4`, `/tmp/ggen-uilayout-exact-retip-warm`, `/tmp/ggen-zcode-powl-retip6-court`, …). loadavg before ≈10.8 / after ≈8.9. Log: `/tmp/cursor/shui-exact-tip5-alive-court.log` (154 lines; no `test result:` line). Status: `/tmp/cursor/shui-exact-tip5-alive.status` → `SHUI_EXIT=137` |
| Crown absorb | **not performed** (sidebranch honesty only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Prior tip-cargo **ALIVE** on lineage `008a81d21` / `26256ca9d` is cited; court blob identity on tip `167352d0d` matches that lineage. This reconfirm does **not** mint tip EXIT:0 `ALIVE`, Foundry `ALIVE`, or Truex admit-court standing. Do not invent tip-cargo `ALIVE` from source presence alone. Prior tip5 pin remains `PARTIAL_ALIVE` @ `d96b2e867`.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries strengthened `shui_public_prefix_court` (4 tests) | **YES** |
| Court blob ≡ prior ALIVE blob `3cda065f` | **YES** |
| Exact isolated tip-cargo EXIT 0 @ `167352d0d` within 10min | **NO** — EXIT **137** (OOM SIGKILL mid-compile of `oxrocksdb-sys` under pod thrash) |
| Prior tip5 push-first | **PARTIAL_ALIVE** @ `d96b2e867` retained |

---

## Exact next verifier

```bash
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5
# Prefer quiet window (loadavg ≤5; no competing oxrocksdb cold builds)
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0 → promote this doc to tip-cargo ALIVE @ 167352d0d
```

Replay identity: `git rev-parse HEAD` must be `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` (or a descendant that still carries blob `3cda065f` for the court).

---

## Compact return

```text
tip_base=167352d0d
branch=cursor/v2711-shui-exact-tip5-alive-bc54
prior_tip5_partial=d96b2e867
EXIT=137
standing=PARTIAL_ALIVE
diagnostic=OOM_SIGKILL_oxrocksdb_cold_thrash
next=CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1
crown_absorb=none ManagePullRequest=not_used force_push=none foundry_alive=none
```
