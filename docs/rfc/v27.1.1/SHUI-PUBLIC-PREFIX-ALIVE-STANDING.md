# shui_public_prefix_court — ALIVE (exact tip-cargo EXIT:0 reconfirm) / v27.1.1

```text
tip_base=a4b3d3fb495f32af9ef4597663755cf13d75a9e9
branch=cursor/v2711-shui-exact-alive-bc54
bc=bc-d35011e6-b98b-54aa-a1ca-fe1d6192ecaf
court=shui_public_prefix_court
package=ggen-graph
tests=4
EXIT=0
standing=ALIVE (exact tip-cargo)
target=/tmp/gate1-shui-alive
prior_tip6=PARTIAL_ALIVE EXIT137 @ c661d228a
prior_lineage_alive=008a81d21
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-ALIVE-STANDING` |
| Side branch | `cursor/v2711-shui-exact-alive-bc54` |
| Tip base (cargo) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `a4b3d3fb495f32af9ef4597663755cf13d75a9e9` (⊇ Gate1 ALIVE trio absorb past `2b34f3a02`) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Gate 1 / Truex fence (`shui:` public-prefix) |
| Court sources | `crates/ggen-graph/tests/shui_public_prefix_court.rs` blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior lineage ALIVE blob) |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-shui-alive cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/gate1-shui-alive` (isolated; hardlink-seeded from `/tmp/ggen-shui-exact-tip6` warm after tip6 cargo released; not overlapping `/tmp/gate1-tip7-taffy`) |
| Exact tip-cargo | **ALIVE** — **4 passed; 0 failed; EXIT:0** @ tip `a4b3d3fb4` |
| Observed at | `2026-11-22T19:00:43Z`→`19:03:17Z` (wall **154s**; finished `test` profile in 2m34s) |
| Log / status | `/tmp/cursor/shui-exact-alive.log` · `/tmp/cursor/shui-exact-alive.status` → `SHUI_EXIT=0` |
| Thrash at start | **n=1** concurrent `cargo test` (taffy tip7 already gone; quiet window after gate wait) |
| Prior tip6 | **PARTIAL_ALIVE** EXIT:**137** @ tip_base `c661d228a` — [`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md) — **superseded** by this EXIT:0 |
| Prior tip5-alive attempt | **PARTIAL_ALIVE** EXIT:**137** @ `7f8e0765b` — [`SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md) |
| Prior lineage tip-cargo ALIVE | **held** @ `008a81d21` (**4**/EXIT:**0**) — now also **exact tip-cargo ALIVE** on descendant tip |
| Photolithography / Foundry | **not** promoted (fence ALIVE ≠ Foundry zero-HITL ALIVE) |
| Crown absorb | tip-wins (see absorb section; updated on land) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Exact tip-cargo EXIT **0** with **4** passed on tip `a4b3d3fb4` (court blob `3cda065f`) promotes Gate-1 `shui:` public-prefix fence to **ALIVE (exact tip-cargo)**. Tip6/tip5-alive PARTIAL pins are superseded for tip-subject standing. Do **not** invent Foundry `ALIVE` from this fence land.

---

## Why ALIVE (not PARTIAL)

| Check | Result |
|-------|--------|
| Tip carries strengthened `shui_public_prefix_court` (4 tests) | **YES** |
| Court blob ≡ prior ALIVE blob `3cda065f` | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip (cargo base) | **YES** — EXIT:**0**; **4 passed** |
| Target isolated from Taffy tip7 | **YES** — `/tmp/gate1-shui-alive` (taffy tip7 released before start) |

---

## Tip-cargo evidence (this land)

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `shui_public_prefix_court` | `/tmp/gate1-shui-alive` | **4 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

Tests: `shui_namespace_is_w3c_shacl_ui_slash_iri`; `shui_prefix_is_admitted_and_private_prefixes_remain_refused`; `shui_ui_property_group_and_editor_iris_are_open_admitted`; `unknown_turtle_prefix_is_refused_unknown`.

---

## Exact replay

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ a4b3d3fb4; court blob 3cda065f
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/gate1-shui-alive
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0
```

---

## Compact return

```text
tip_base=a4b3d3fb495f32af9ef4597663755cf13d75a9e9
branch=cursor/v2711-shui-exact-alive-bc54
bc=bc-d35011e6
EXIT=0
standing=ALIVE (exact tip-cargo)
tests=4
target=/tmp/gate1-shui-alive
prior_tip6_partial=superseded
Foundry=NOT_CLAIMED ManagePullRequest=not_used force_push=none
```
