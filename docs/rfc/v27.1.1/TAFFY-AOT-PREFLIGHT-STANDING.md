# Taffy AOT preflight — ALIVE (exact tip-cargo) / v27.1.1

```text
tip_base=2685119bd92e607cdb93d3dafdbb80cd9da0447d
branch=cursor/v2711-taffy-preflight-alive-bc54
bc=bc-94bc66c6-e8e3-532e-b410-ac9a9d42e7d6
court=taffy_aot_preflight_court
package=ggen-graph
tests=7
EXIT=0
standing=ALIVE (exact tip-cargo)
target=/tmp/gate1-taffy
```

| Field | Value |
|-------|-------|
| Document ID | `TAFFY-AOT-PREFLIGHT-STANDING` |
| Side branch | `cursor/v2711-taffy-preflight-alive-bc54` |
| Tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2685119bd92e607cdb93d3dafdbb80cd9da0447d` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Gate 1 D2 / photolithography hop 3 |
| Court | `cargo test -p ggen-graph --test taffy_aot_preflight_court` |
| Target | `CARGO_TARGET_DIR=/tmp/gate1-taffy` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; `--locked` |
| Exact tip-cargo | **ALIVE** — **7 passed; EXIT:0** @ tip `2685119bd` |
| Observed at | `2026-11-22T18:51:17Z`→`18:51:20Z` |
| Log / status | `/tmp/cursor/taffy-preflight-alive.log` · `/tmp/cursor/taffy-preflight-alive.status` → `0` |
| Photolithography / Foundry crown | **PARTIAL** (unchanged) — fence ALIVE ≠ consumer bind ALIVE ≠ Foundry zero-HITL ALIVE |
| Crown absorb | **REFUSED** this land |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*` | `PARTIAL` | `PRESENT` | `PENDING`.

**Authority.** Exact tip-cargo EXIT **0** with **7** passed on tip `2685119bd` promotes this ledger's exact tip reconfirm to **ALIVE (exact tip-cargo)**. Prior PARTIAL retip7 (`167352d0d`, cargo SKIPPED under thrash) is superseded. Photolithography path crown and Foundry zero-HITL remain **PARTIAL** / not promoted.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ photolithography path crown `ALIVE`.

This land closes `exact_tip_reconfirm_ALIVE` only.

---

## Tip-cargo evidence (this land)

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `taffy_aot_preflight_court` | `/tmp/gate1-taffy` | **7 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

Finished `test` profile in 2.98s (warm seed from brand exact tip target).

---

## Compact return

```text
tip_base=2685119bd92e607cdb93d3dafdbb80cd9da0447d
branch=cursor/v2711-taffy-preflight-alive-bc54
EXIT=0
standing=ALIVE (exact tip-cargo)
tests=7
target=/tmp/gate1-taffy
no_crown_absorb=1
no_mpr=1
no_force_push=1
Foundry=NOT_CLAIMED
```
