# shui_public_prefix_court — exact-tip5 honesty retip (v27.1.1)

```text
ggen_tip_base=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
branch=cursor/v2711-shui-exact-tip5-bc54
court=shui_public_prefix_court
prior_alive=008a81d21 (lineage tip-cargo; crown reconfirm) / 26256ca9d (gate1-next)
tip4_origin=ABSENT (never appeared on origin)
tip5_tip_cargo=PARTIAL (no EXIT:0 within 3min budget)
standing=PARTIAL_ALIVE (lineage ALIVE cited; tip EXIT:0 pending)
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING` |
| Branch | `cursor/v2711-shui-exact-tip5-bc54` |
| Tip base | `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` (`docs(rfc): tip-wins absorb Truex exact-tip4 admit-court onto crown` on `origin/cursor/ecosystem-spec-27-1-1-bc54`) |
| Observed at | `2026-11-22T17:37:00Z` |
| Court sources on tip | `crates/ggen-graph/tests/shui_public_prefix_court.rs` blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior ALIVE blob) |
| sha256 (tip working tree) | court `bd032856…5fe0`; `public_prefixes.rs` `bff5aa82…ee5c`; `shui.rs` `d7040f32…78d2` |
| Prior tip-cargo ALIVE | `cursor/v2711-shui-prefix-alive-bc54` @ `9c8b68c2947ec4ad28b03c2a775a4e0c6b7bf87d` pinning crown reconfirm **ALIVE** @ `008a81d21` (4 passed / EXIT 0; target `/tmp/ggen-shui-prefix-target2`) |
| Prior gate1-next ALIVE | `shui_public_prefix_court` **ALIVE** on lineage `26256ca9d` (`GATE-1-NEXT-COURTS-STANDING.md`; 4 passed / EXIT 0) |
| Prior tip4 attempt | local `cursor/v2711-shui-exact-tip4-bc54` — **never pushed**; tip then `8c8b1e6e5` → raced to `49ebdb27c`; court log `/tmp/cursor/shui-exact-tip4-court.log` = file-lock thrash; status `SHUI_EXIT=143` |
| Tip5 tip-cargo | **not obtained** within 3min — pod loadavg ~11 / ~21 concurrent `cargo test`; existing tip4-era shui cargos still compiling into `/tmp/ggen-shui-prefix-target2` and `/tmp/ggen-gate1-tip3-target` |
| Crown absorb | **not performed** (sidebranch honesty only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Prior tip-cargo **ALIVE** on lineage `008a81d21` / `26256ca9d` is cited; court blob identity on tip `167352d0d` matches that lineage. This retip does **not** mint tip EXIT:0 `ALIVE`, Foundry `ALIVE`, or Truex admit-court standing (Truex exact-tip4 absorb is a sibling fence on the same tip — non-substitute). Do not invent tip-cargo `ALIVE` from source presence alone.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries strengthened `shui_public_prefix_court` (4 tests) | **YES** |
| Court blob ≡ prior ALIVE blob `3cda065f` | **YES** |
| Tip-cargo `cargo test -p ggen-graph --test shui_public_prefix_court` EXIT 0 @ `167352d0d` within 3min | **NO** — cold/shared oxrocksdb thrash; tip4 already SIGKILL/143 under same pressure; tip5 records PARTIAL per push-first ≤3min rule |
| `origin` ref for tip4 | **ABSENT** — falsifier that forced tip5 push-first |

---

## Exact next verifier

```bash
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5
# Prefer quiet window (loadavg ≤10; no competing oxrocksdb cold builds)
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0 → promote this doc to tip-cargo ALIVE @ 167352d0d
```

Replay identity: `git rev-parse HEAD` must be `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` (or a descendant that still carries blob `3cda065f` for the court).

---

## Compact return

```text
tip_base=167352d0d
branch=cursor/v2711-shui-exact-tip5-bc54
prior_alive=008a81d21/26256ca9d (lineage tip-cargo)
tip4_origin=ABSENT tip4_exit=143
tip5_standing=PARTIAL_ALIVE
next=CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1
crown_absorb=none ManagePullRequest=not_used force_push=none
```
