# ui_layout_court — exact-tip7 push-first honesty (v27.1.1)

```text
gate_reason=shui_tip7_side_with_standing+crown_SHUI-PUBLIC-PREFIX-EXACT-TIP7
tip_base=2e452873ad1229e3fdf3b9a1f8a91ae5105d4e4c
live_crown_at_ledger=675c614051c5c55eb96ee9454d290b1a646f189d
branch=cursor/v2711-uilayout-exact-tip7-bc54
bc=bc-404c8bbd-4640-5987-9d46-f0cbd728a163
court=ui_layout_court
blob=1dfe6bdfab2cb8dfd35fa7870692185cf004ab4c
tests=11
budget_s=1500
exit=124
standing=PARTIAL_ALIVE
prior_exact_tip6=ALIVE@2e452873ad (11 passed; tip-cargo ALIVE held / superseded prior PARTIAL)
prior_alive_lineage=a508e6bae / sealed 2685119bd (11 passed)
peer_tip7_serial_alive=UILAYOUT-TIP7-STANDING.md EXIT0 @ /tmp/tip7-serial (bc-e40d4b25; not this probe)
crown_absorb=pending
ManagePullRequest=not_used
force_push=sidebranch_lease_only
Foundry_ALIVE=not_claimed
sibling_ts=UNSUPPORTED
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-EXACT-TIP7-STANDING` |
| Branch | `cursor/v2711-uilayout-exact-tip7-bc54` |
| Agent | `bc-404c8bbd-4640-5987-9d46-f0cbd728a163` |
| Gate | Waited for `origin/cursor/v2711-shui-exact-tip7-bc54` + `SHUI-PUBLIC-PREFIX-EXACT-TIP7-STANDING.md` (and crown copy). Opened `2026-11-22T19:07:17Z`. Held cargo until `/tmp/gate1-tip7-taffy` / `/tmp/gate1-tip7-shui` quiet; Taffy peer `pid 3873369` exited `19:36:40Z`. |
| Tip base at cargo | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2e452873ad1229e3fdf3b9a1f8a91ae5105d4e4c` |
| Live crown at ledger | `675c614051c5c55eb96ee9454d290b1a646f189d` (docs-only FF past cargo tip; court blob ≡) |
| Court sources on tip | `crates/ggen-graph/tests/ui_layout_court.rs` blob `1dfe6bdfab2cb8dfd35fa7870692185cf004ab4c` — **11** `#[test]`; `src/layout/` tree `d4370c6effefaab8d6ffe55eedaa88eb4b6b45b1` |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-uilayout cargo test -p ggen-graph --test ui_layout_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/gate1-tip7-uilayout` (isolated; seeded from `/tmp/ggen-uilayout-exact-tip6` warm 5.5G after ENOSPC cleanup) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:124** (timeout) @ `2026-11-22T19:37:24Z`→`20:02:24Z` — cargo still compiling (`ggen-marketplace` / deps) when killed; **no** `test result:` line; log 19899 bytes. Quiet never reached test binary. |
| Thrash | Brand court cargo concurrent (`brand_toml_*` jobs=1) during window; loadavg≈2.1→3.3; tip7 taffy/shui dirs quiet at start |
| Log / status | `/tmp/cursor/gate1-tip7-uilayout/court.log` · `/tmp/cursor/gate1-tip7-uilayout/court.status` → `UILAYOUT_EXIT=124` |
| Prior exact tip6 | **ALIVE** tip-cargo EXIT0 @ `2e452873ad` (11 passed) — lineage held; **not** reconfirmed by this exact-tip7 probe |
| Peer tip7 serial | [`UILAYOUT-TIP7-STANDING.md`](./UILAYOUT-TIP7-STANDING.md) EXIT0 @ `/tmp/tip7-serial` (`bc-e40d4b25`) — **not** this agent's `/tmp/gate1-tip7-uilayout` EXIT:0 |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** |
| Crown absorb | tip-wins honesty ledger (PARTIAL; EXIT≠0) |
| ManagePullRequest | **not used** |
| Force-push | sidebranch lease only (stale remote tip7 branch rewritten onto live crown) |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Exact-tip7 isolated tip-cargo under `/tmp/gate1-tip7-uilayout` did **not** obtain EXIT 0 within 1500s → standing **PARTIAL_ALIVE**. Prior exact tip6 **ALIVE** and peer tip7-serial **ALIVE** (`UILAYOUT-TIP7-STANDING`) remain separate evidence — **not** substitutes for this probe's tip-subject EXIT:0. Do **not** invent Gate-1 hop-3b exact-tip7 reconfirm `ALIVE`, photolithography path crown, or Foundry zero-HITL `ALIVE` from this land.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Gate: Shui tip7 side/ledger present before cargo | **YES** (`SHUI-PUBLIC-PREFIX-EXACT-TIP7-STANDING.md` on origin side + crown @ `19:07:17Z`) |
| Tip carries `ui_layout_court` (11 tests); blob `1dfe6bdf` | **YES** |
| Exact isolated tip-cargo EXIT 0 @ tip within 1500s (`/tmp/gate1-tip7-uilayout`) | **NO** — EXIT **124** (timeout mid-compile; no tests executed) |
| Peer tip7-serial EXIT0 @ `/tmp/tip7-serial` | held separately — **not** this probe |

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 2e452873ad; court blob 1dfe6bdf…
# Prefer quiet tip (no concurrent ggen-graph cargo); exclusive jobs=1
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/gate1-tip7-uilayout \
  cargo test -p ggen-graph --test ui_layout_court --locked \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 11 passed; EXIT 0 → exact-tip7 tip-cargo ALIVE
```

Until that EXIT **0**, keep this exact-tip7 standing **PARTIAL_ALIVE**.

---

## Compact return

```text
gate_reason=shui_tip7_side_with_standing+crown_SHUI-PUBLIC-PREFIX-EXACT-TIP7
tip_base=2e452873ad1229e3fdf3b9a1f8a91ae5105d4e4c
branch=cursor/v2711-uilayout-exact-tip7-bc54
bc=bc-404c8bbd
EXIT=124
standing=PARTIAL_ALIVE
diagnostic=timeout_1500s_mid_compile_no_test_binary
next=CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-uilayout cargo test -p ggen-graph --test ui_layout_court --locked --config build.jobs=1 -- --test-threads=1
peer_tip7_serial_alive=UILAYOUT-TIP7-STANDING.md
lineage_alive=a508e6bae / tip6 ALIVE@2e452873ad
crown_absorb=tip-wins ManagePullRequest=not_used force_push=sidebranch_lease_only Foundry_ALIVE=not_claimed
```
