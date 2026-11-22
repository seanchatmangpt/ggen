# shui_public_prefix_court — exact-tip5 EXIT:0 reconfirm honesty → tip-wins absorb (v27.1.1)

```text
ggen_tip_before=2d7b47b2ea5eb5108f52fa46682a68a1af7e0097
sidebranch=cursor/v2711-shui-exact-tip5-alive-bc54@7f8e0765b4b4f33650cd17fa5b9f9714da70cee7
absorb_branch=cursor/v2711-shui-exact-tip5-alive-absorb-3acf
side_parent=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
court=shui_public_prefix_court
prior_tip5_partial=d96b2e867 (cursor/v2711-shui-exact-tip5-bc54; tip-wins @ dd54e7821)
prior_alive=008a81d21 (lineage tip-cargo; crown reconfirm) / 26256ca9d (gate1-next)
tip_cargo_exit=137
standing=PARTIAL_ALIVE (tip EXIT:0 not obtained; OOM SIGKILL mid oxrocksdb cold; tip-wins absorb)
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING` (tip-wins absorb) |
| Sidebranch | `cursor/v2711-shui-exact-tip5-alive-bc54` @ `7f8e0765b4b4f33650cd17fa5b9f9714da70cee7` |
| Absorb branch | `cursor/v2711-shui-exact-tip5-alive-absorb-3acf` |
| Tip before (this absorb) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2d7b47b2ea5eb5108f52fa46682a68a1af7e0097` (ui_layout exact-retip5 ledger honesty; tip raced past side parent `167352d0d`) |
| Side parent | `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` (Truex exact-tip4 tip-wins) |
| Observed at (sidebranch) | `2026-11-22T17:40:03Z` → `2026-11-22T17:47:30Z` (~7.5 min wall) |
| Court sources on tip | `crates/ggen-graph/tests/shui_public_prefix_court.rs` blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior ALIVE blob) |
| sha256 (tip working tree) | court `bd032856…5fe0`; `public_prefixes.rs` `bff5aa82…ee5c`; `shui.rs` `d7040f32…78d2` |
| Prior tip5 honesty | `cursor/v2711-shui-exact-tip5-bc54` @ `d96b2e867f7afe26e4000195fc02010c62dcbc8a` — tip-wins absorb `dd54e7821` / [`SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING.md) — **PARTIAL_ALIVE** (push-first; no EXIT:0 within 3min) |
| Prior tip-cargo ALIVE | `cursor/v2711-shui-prefix-alive-bc54` @ `9c8b68c2947ec4ad28b03c2a775a4e0c6b7bf87d` pinning crown reconfirm **ALIVE** @ `008a81d21` (4 passed / EXIT 0; target `/tmp/ggen-shui-prefix-target2`) |
| Exact command | `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/ggen-shui-exact-tip5` (isolated; freshly created) |
| EXIT | **137** (SIGKILL) |
| Diagnostic | cold compile reached `oxrocksdb-sys` / late crate graph; process **Killed**; `dmesg` shows `oom-kill` of `rustc` under concurrent oxrocksdb cold builds (`/tmp/ggen-shui-exact-tip4`, `/tmp/ggen-uilayout-exact-retip-warm`, `/tmp/ggen-zcode-powl-retip6-court`, …). loadavg before ≈10.8 / after ≈8.9. Log: `/tmp/cursor/shui-exact-tip5-alive-court.log` (154 lines; no `test result:` line). Status: `/tmp/cursor/shui-exact-tip5-alive.status` → `SHUI_EXIT=137` |
| Crown absorb | tip-wins via `cursor/v2711-shui-exact-tip5-alive-absorb-3acf` onto `2d7b47b2e` (race reapply; side parent was `167352d0d`; FF only; no force-push) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Prior tip-cargo **ALIVE** on lineage `008a81d21` / `26256ca9d` is cited; court blob identity on tip `2d7b47b2e` (descendant of `167352d0d`) matches that lineage. This EXIT:0 reconfirm absorb does **not** mint tip EXIT:0 `ALIVE`, Foundry `ALIVE`, or Truex admit-court standing. Do not invent tip-cargo `ALIVE` from source presence alone. Prior tip5 pin remains `PARTIAL_ALIVE` @ `d96b2e867` / absorb `dd54e7821`. Standing remains **PARTIAL_ALIVE**.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries strengthened `shui_public_prefix_court` (4 tests) | **YES** |
| Court blob ≡ prior ALIVE blob `3cda065f` | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip within 10min | **NO** — EXIT **137** (OOM SIGKILL mid-compile of `oxrocksdb-sys` under pod thrash) |
| Prior tip5 push-first | **PARTIAL_ALIVE** @ `d96b2e867` / tip-wins `dd54e7821` retained |

---

## Tip-wins absorb (2026-11-22)

| Field | Value |
|-------|-------|
| Tip before | `2d7b47b2ea5eb5108f52fa46682a68a1af7e0097` |
| Sidebranch content | `7f8e0765b4b4f33650cd17fa5b9f9714da70cee7` (parent `167352d0d`; tip raced → shui tip5 / Gate-1 / brand / Taffy / ui_layout → `2d7b47b2e`) |
| Reapply | tip-wins onto live tip → absorb branch (race reapply; content ≡ sidebranch standing + absorb metadata) |
| Tip-cargo | **PARTIAL_ALIVE held** — EXIT:**137**; EXIT:0 not claimed |
| Foundry | **not** promoted (stays PARTIAL / `--live` BLOCKED_HITL lineage) |
| Push policy | FF-only onto `cursor/ecosystem-spec-27-1-1-bc54`; **no** force-push; **no** ManagePullRequest |

---

## Exact next verifier

```bash
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5
# Prefer quiet window (loadavg ≤5; no competing oxrocksdb cold builds)
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0 → promote this doc to tip-cargo ALIVE @ tip carrying blob 3cda065f
```

Replay identity: `git rev-parse HEAD` must be a descendant of `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` that still carries blob `3cda065f` for the court.

---

## Compact return

```text
tip_before=2d7b47b2e
sidebranch=cursor/v2711-shui-exact-tip5-alive-bc54@7f8e0765b
absorb=cursor/v2711-shui-exact-tip5-alive-absorb-3acf
prior_tip5_partial=d96b2e867
EXIT=137
standing=PARTIAL_ALIVE held
diagnostic=OOM_SIGKILL_oxrocksdb_cold_thrash
next=CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1
Foundry=NOT_CLAIMED ManagePullRequest=not_used force_push=none
```
