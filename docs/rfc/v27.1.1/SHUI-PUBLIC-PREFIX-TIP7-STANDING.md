# shui_public_prefix_court — tip7 serial honesty after Taffy (v27.1.1)

```text
gate_reason=taffy_side_branch_exists_then_absorbed_ALIVE
tip_base=f12972590e280609ddaedeafad84cda63372ea4b
live_crown_at_ledger=6138ca663e2792ebcf346ddeb3b4e08213cdfd3e
cited_kickoff_tip=63da993cc194ff2177ee757e01c883b582028493
branch=cursor/v2711-shui-exact-tip7-bc54
bc=bc-8c208d11-71d2-54c4-a21c-9f32c1fc9d05
court=shui_public_prefix_court
blob=3cda065f6564917504f241ac3626c111cd2dfb6a
tests=4
budget_s=1500
exit=101
standing=PARTIAL_ALIVE
prior_exact_tip7_partial=881f2ec16 EXIT137 (bc-7a87dbfb; EXACT-TIP7)
prior_tip6_partial=c661d228a EXIT137
prior_alive_lineage=008a81d21 (4 passed; tip-cargo ALIVE held)
taffy_tip7=ALIVE EXIT0 @ 63da993cc / absorb 4ebcb9d46
crown_absorb=ABSORBED@1b42fa3e6→d35482374←cf0288b18
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-TIP7-STANDING` |
| Branch | `cursor/v2711-shui-exact-tip7-bc54` |
| Agent | `bc-8c208d11-71d2-54c4-a21c-9f32c1fc9d05` |
| Gate | Opened on `origin/cursor/v2711-taffy-preflight-tip7-bc54` exists; cargo deferred until Taffy tip7 (`bc-bf13a704`) EXIT:**0** + crown absorb `TAFFY-AOT-PREFLIGHT-TIP7-STANDING` @ `4ebcb9d46` |
| Tip base at cargo start | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f12972590e280609ddaedeafad84cda63372ea4b` (FF beyond kickoff cite `63da993cc…`; ⊇ Taffy tip7 absorb) |
| Live crown at ledger | `6138ca663e2792ebcf346ddeb3b4e08213cdfd3e` (raced past cargo-start tip; court blob unchanged) |
| Court sources on tip | `crates/ggen-graph/tests/shui_public_prefix_court.rs` blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior ALIVE blob; **4** `#[test]`) |
| sha256 (working tree) | court `bd032856…5fe0`; `public_prefixes.rs` `bff5aa82…ee5c`; `shui.rs` `d7040f32…78d2` |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-shui cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/gate1-tip7-shui` **only** (serial one-cargo; warm ~4.1G at start with `librocksdb.a`) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:101** @ `2026-11-22T19:00:58Z`→`19:17:51Z` (~1013s wall) — single cargo; mid-`oxrocksdb-sys` the entire `CARGO_TARGET_DIR` tree was **removed** (`can't create …/out/*.o: No such file or directory`); status `SHUI_EXIT=101`. No second cargo (serial rule). |
| Thrash | Peer `shui_public_prefix_court` on `/tmp/gate1-shui-alive` (bc-7a87dbfb lineage) + other gate1 cargos; loadavg≈5.9→6.9 during compile; target dir gone by end (`ls: cannot access '/tmp/gate1-tip7-shui'`) |
| Log / status | `/tmp/cursor/shui-tip7.log` · `/tmp/cursor/shui-tip7.status` → `SHUI_EXIT=101` |
| Prior EXACT tip7 | [`SHUI-PUBLIC-PREFIX-EXACT-TIP7-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP7-STANDING.md) — **PARTIAL_ALIVE** EXIT:**137** @ absorb `881f2ec16` (bc-7a87dbfb; same branch name; SIGKILL mid-oxrocksdb) |
| Prior tip6 | [`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md) — PARTIAL EXIT:**137** |
| Prior tip-cargo ALIVE (lineage) | **held** — 4 passed; EXIT 0 @ `008a81d21` (ancestor; blob `3cda065f`) — **not** reconfirmed this tip7 |
| Taffy tip7 peer | [`TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md`](./TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md) — **ALIVE** EXIT:**0** @ `63da993cc` / absorb `4ebcb9d46` |
| Crown absorb | **ABSORBED** tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` @ pre-absorb `1b42fa3e68b031c94c4a68468fb97d0dca921f57` → absorb `d3548237420ea661c8c46bfa796c1e4b20fb21c4` (side `cf0288b188c922338e16ffb07897fdcbe79126cd`) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip7 serial honesty after Taffy gate: exclusive tip-cargo did **not** obtain EXIT 0 — target dir wiped mid-`oxrocksdb-sys` → EXIT **101** → standing **PARTIAL_ALIVE**. Prior tip-cargo **ALIVE** @ `008a81d21` and peer EXACT-TIP7 PARTIAL @ `881f2ec16` remain held. Do **not** invent tip-subject `ALIVE` or Foundry `ALIVE` from source presence / lineage / Taffy peer ALIVE alone.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ Foundry `ALIVE`.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries strengthened `shui_public_prefix_court` (4 tests; blob `3cda065f`) | **YES** |
| Court blob ≡ prior ALIVE blob `3cda065f` | **YES** |
| Gate: Taffy tip7 side branch / absorb before cargo | **YES** — waited; Taffy EXIT:**0** ALIVE absorbed |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **NO** — EXIT **101** (`oxrocksdb-sys` ToolExecError after `CARGO_TARGET_DIR=/tmp/gate1-tip7-shui` removed mid-build; no `test result:`) |
| Serial one-cargo rule | **held** — no retry |
| Lineage tip-cargo ALIVE @ `008a81d21` | **held** (ancestor; not a tip-subject substitute) |

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ f12972590 (⊇ 63da993cc ⊇ 008a81d21); court blob 3cda065f
# Prefer quiet window; ensure /tmp/gate1-tip7-shui is exclusive (no peer rm -rf)
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/gate1-tip7-shui
timeout 1500s cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0 → promote this doc to tip-cargo ALIVE
```

Until that EXIT **0**, keep standing **PARTIAL_ALIVE** — lineage ALIVE @ `008a81d21` is **not** a substitute for tip reconfirm.

---

## Compact return

```text
gate_reason=taffy_side_branch_exists_then_absorbed_ALIVE
tip_base=f12972590e280609ddaedeafad84cda63372ea4b
branch=cursor/v2711-shui-exact-tip7-bc54
bc=bc-8c208d11
EXIT=101
standing=PARTIAL_ALIVE
diagnostic=TARGET_DIR_WIPED_mid_oxrocksdb_ToolExecError
target=/tmp/gate1-tip7-shui
lineage_alive=008a81d21
taffy_tip7=ALIVE@4ebcb9d46
prior_exact_tip7=PARTIAL@881f2ec16
crown_absorb=ABSORBED@1b42fa3e6→d35482374←cf0288b18 Foundry_ALIVE=not_claimed ManagePullRequest=not_used force_push=none
```
