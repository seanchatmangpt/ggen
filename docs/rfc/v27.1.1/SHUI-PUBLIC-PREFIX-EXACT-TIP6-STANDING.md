# shui_public_prefix_court — exact-tip6 tip-cargo ALIVE (v27.1.1)

```text
tip_base=dd61fefb29fa7ea6e1cf789f002ccd9879393988
cargo_tip=dd61fefb29fa7ea6e1cf789f002ccd9879393988
prior_cargo_tips=4ebcb9d46f7ce6da0cafe7671c6942d9932029f8,f12972590e280609ddaedeafad84cda63372ea4b
branch=cursor/v2711-shui-exact-tip6-bc54
bc=bc-262603dc-8fb9-50aa-93a9-5a3190c363b9
court=shui_public_prefix_court
passed=4
failed=0
exit=0
standing=ALIVE (exact tip-cargo)
prior_tip6_partial=bc-c92c420b EXIT137 PARTIAL_ALIVE (superseded)
prior_alive_lineage=008a81d21 (held; now tip-subject EXIT0)
ManagePullRequest=not_used
force_push=sidebranch_lease_only
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING` |
| Branch | `cursor/v2711-shui-exact-tip6-bc54` |
| Agent | `bc-262603dc-8fb9-50aa-93a9-5a3190c363b9` |
| Tip base (ALIVE reconfirm) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `dd61fefb29fa7ea6e1cf789f002ccd9879393988` |
| EXIT0 observations | `4ebcb9d46` @ 19:00:48Z; `f12972590` @ 19:01:01Z; **`dd61fefb2` @ 19:03:39Z** — all **4 passed; EXIT 0** |
| Court sources on tip | `crates/ggen-graph/tests/shui_public_prefix_court.rs` blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior ALIVE blob) |
| sha256 (tip working tree) | court `bd032856…5fe0` |
| Exact command | `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip6 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/ggen-shui-exact-tip6` (isolated; warm-seeded) |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; `CXX=g++`; `SCCACHE_DISABLE=1`; `RUSTC_WRAPPER=` |
| Prior tip6 PARTIAL | `bc-c92c420b` / tip_base `c661d228a` — EXIT **137** — **superseded** |
| Prior tip-cargo ALIVE (lineage) | `008a81d21` — retained; tip-subject now **ALIVE** |
| Log / status | `/tmp/cursor/shui-exact-tip6-court.log` · `/tmp/cursor/shui-exact-tip6.status` → `SHUI_EXIT=0` |
| ManagePullRequest | **not used** |
| Force-push | sidebranch `--force-with-lease` only (supersede PARTIAL); **no** crown force |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Exact tip-cargo EXIT **0** on live crown `dd61fefb2` (court blob `3cda065f`) promotes Gate-1 shui public-prefix fence to **ALIVE (exact tip-cargo)**. Do **not** invent Foundry zero-HITL `ALIVE` or photolithography path crown `ALIVE` from this fence alone.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `shui_public_prefix_court` | `/tmp/ggen-shui-exact-tip6` | **4 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

Tests exercised (all **ok**):

- `shui_namespace_is_w3c_shacl_ui_slash_iri`
- `shui_prefix_is_admitted_and_private_prefixes_remain_refused`
- `shui_ui_property_group_and_editor_iris_are_open_admitted`
- `unknown_turtle_prefix_is_refused_unknown`

---

## Prior PARTIAL (retained history)

Tip6 push-first @ `c661d228a` (`bc-c92c420b`) recorded EXIT **137** (instant SIGKILL under thrash / sibling exclusive guardians). That pin is retained as history only; standing for tip-subject tip-cargo is now **ALIVE**.

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ dd61fefb2; court blob 3cda065f
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip6
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; EXIT 0
```

---

## Compact return

```text
tip_base=dd61fefb29fa7ea6e1cf789f002ccd9879393988
branch=cursor/v2711-shui-exact-tip6-bc54
bc=bc-262603dc
EXIT=0
standing=ALIVE (exact tip-cargo)
passed=4
prior_partial=bc-c92c420b EXIT137 superseded
next=Foundry zero-HITL / path crown not ALIVE; absorb tip-wins onto crown
ManagePullRequest=not_used crown_force_push=none Foundry_ALIVE=not_claimed
```
