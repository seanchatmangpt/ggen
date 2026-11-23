# brand.toml tip-cargo — ALIVE (exact tip-cargo) / v27.1.1

```text
tip_base=2685119bd92e607cdb93d3dafdbb80cd9da0447d
branch=cursor/v2711-brand-exact-alive-bc54
standing=ALIVE (exact tip-cargo)
EXIT=0
completeness=5
rhythm=12
target=/tmp/ggen-brand-exact-tip
bc=bc-94bc66c6-e8e3-532e-b410-ac9a9d42e7d6
```

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-TIP-CARGO-STANDING` |
| Side branch | `cursor/v2711-brand-exact-alive-bc54` |
| Tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2685119bd92e607cdb93d3dafdbb80cd9da0447d` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §8 brand rhythm / completeness |
| Court | `brand_toml_completeness_court` + `brand_toml_rhythm_court` |
| Target dir | `/tmp/ggen-brand-exact-tip` |
| Constraint | `CXX=g++`; `SCCACHE_DISABLE=1`; `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1` |
| Exact tip-cargo | **ALIVE** — EXIT:**0**; completeness **5** + rhythm **12** @ tip `2685119bd` |
| Observed at | `2026-11-22T18:46:05Z`→`18:50:32Z` (warm retry after EXIT:124 cold oxrocksdb) |
| Log / status | `/tmp/cursor/brand-exact-alive.log` · `/tmp/cursor/brand-exact-alive.status` → `0` |
| Production theme | **UNSUPPORTED** (`REFUSED_PRODUCTION`) — unchanged |
| Foundry | **not** promoted |
| Crown absorb | **NONE** |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Official `ggen-graph` tip-cargo EXIT **0** for both brand courts on tip `2685119bd` promotes Gate-1 brand completeness/rhythm to **ALIVE (exact tip-cargo)**. Prior PARTIAL retip5 (`167352d0d`, budget miss / thrash) is superseded by this reconfirm. Production theme remains **UNSUPPORTED**. Photolithography path crown and Foundry zero-HITL are **not** promoted.

---

## Tip-cargo evidence (this land)

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `brand_toml_completeness_court` | `/tmp/ggen-brand-exact-tip` | **5 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |
| `brand_toml_rhythm_court` | `/tmp/ggen-brand-exact-tip` | **12 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

Prior cold attempt on the same tip hit EXIT:**124** (10min timeout still compiling `oxrocksdb-sys` under residual tip thrash). Warm retry finished `test` profile in 4m27s then both courts passed.

---

## Compact return

```text
tip_base=2685119bd92e607cdb93d3dafdbb80cd9da0447d
branch=cursor/v2711-brand-exact-alive-bc54
EXIT=0
standing=ALIVE (exact tip-cargo)
completeness=5
rhythm=12
target=/tmp/ggen-brand-exact-tip
no_crown_absorb=1
no_mpr=1
no_force_push=1
Foundry=NOT_CLAIMED
```
