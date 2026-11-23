# ui_layout_court — ALIVE (exact tip-cargo) / v27.1.1

```text
tip_base=2685119bd92e607cdb93d3dafdbb80cd9da0447d
branch=cursor/v2711-uilayout-exact-alive-bc54
bc=bc-94bc66c6-e8e3-532e-b410-ac9a9d42e7d6
court=ui_layout_court
EXIT=0
tests=11
standing=ALIVE (exact tip-cargo)
target=/tmp/ggen-uilayout-exact-retip5
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-EXACT-RETIP5-STANDING` |
| Side branch | `cursor/v2711-uilayout-exact-alive-bc54` |
| Tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2685119bd92e607cdb93d3dafdbb80cd9da0447d` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Gate 1 hop-3b / ui_layout |
| Court | `cargo test -p ggen-graph --test ui_layout_court` |
| Target dir | `/tmp/ggen-uilayout-exact-retip5` |
| Constraint | `CXX=g++`; `SCCACHE_DISABLE=1`; `RUSTC_WRAPPER=`; `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1` |
| Exact tip-cargo | **ALIVE** — **11 passed; EXIT:0** @ tip `2685119bd` |
| Observed at | `2026-11-22T18:52:02Z`→`18:52:05Z` |
| Log / status | `/tmp/cursor/uilayout-exact-alive.log` · `/tmp/cursor/uilayout-exact-alive.status` → `0` |
| Prior exact tip-cargo ALIVE (lineage) | `a508e6bae` (11 passed) — superseded by this tip reconfirm |
| Prior retip5 | EXIT:**124** thrash/timeout — superseded |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** |
| Foundry | **not** promoted |
| Crown absorb | **NONE** this land |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Exact tip-cargo EXIT **0** with **11** passed on tip `2685119bd` promotes this ledger to **ALIVE (exact tip-cargo)**. Prior PARTIAL_ALIVE retip5 (EXIT:124) is superseded. Gate-1 hop-3b exact-tip reconfirm closes. Photolithography path crown and Foundry zero-HITL are **not** promoted.

---

## Tip-cargo evidence (this land)

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `ui_layout_court` | `/tmp/ggen-uilayout-exact-retip5` | **11 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

Finished `test` profile in 2.68s (warm seed from taffy `/tmp/gate1-taffy`).

---

## Compact return

```text
tip_base=2685119bd92e607cdb93d3dafdbb80cd9da0447d
branch=cursor/v2711-uilayout-exact-alive-bc54
EXIT=0
standing=ALIVE (exact tip-cargo)
tests=11
target=/tmp/ggen-uilayout-exact-retip5
no_crown_absorb=1
no_mpr=1
no_force_push=1
Foundry=NOT_CLAIMED
```
