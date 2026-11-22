# brand.toml exact-tip6 — tip-cargo ALIVE (v27.1.1)

```text
tip_base=cfd93c1cbd2cc95c13b7c325bf88a59f22c988f3
cargo_tip=cfd93c1cbd2cc95c13b7c325bf88a59f22c988f3
branch=cursor/v2711-brand-exact-tip6-bc54
bc=bc-262603dc-8fb9-50aa-93a9-5a3190c363b9
court=brand_toml_completeness_court + brand_toml_rhythm_court
completeness=5
rhythm=12
exit=0
standing=ALIVE (exact tip-cargo)
prior_tip6_partial=bc-c92c420b EXIT137 PARTIAL_ALIVE (superseded)
lineage_alive_tip=77eb46fd05d5bc847050a8f0a9147e1133e52c83
production_theme=UNSUPPORTED
ManagePullRequest=not_used
force_push=sidebranch_lease_only
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-EXACT-TIP6-STANDING` |
| Branch | `cursor/v2711-brand-exact-tip6-bc54` |
| Agent | `bc-262603dc-8fb9-50aa-93a9-5a3190c363b9` |
| Tip base (ALIVE) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `cfd93c1cbd2cc95c13b7c325bf88a59f22c988f3` |
| Observed at | `2026-11-22T19:05:49Z`→`19:06:56Z` |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip6 --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/ggen-brand-exact-tip6` (isolated; warm-seeded from `/tmp/ggen-brand-exact-tip`) |
| Result | completeness **5** + rhythm **12**; EXIT **0** |
| Prior tip6 PARTIAL | `bc-c92c420b` EXIT **137** — **superseded** |
| Lineage ALIVE | `77eb46fd0` retained; tip-subject now **ALIVE** |
| Production theme | **UNSUPPORTED** (`REFUSED_PRODUCTION`) |
| Log / status | `/tmp/cursor/brand-exact-tip6-court.log` · `/tmp/cursor/brand-exact-tip6.status` → `BRAND_EXIT=0` |
| ManagePullRequest | **not used** |
| Force-push | sidebranch `--force-with-lease` only; **no** crown force |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Exact tip-cargo EXIT **0** on live crown `cfd93c1cb` promotes Gate-1 brand completeness+rhythm to **ALIVE (exact tip-cargo)**. Production theme remains **UNSUPPORTED**. Do **not** invent Foundry zero-HITL `ALIVE`.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `brand_toml_completeness_court` | `/tmp/ggen-brand-exact-tip6` | **5 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |
| `brand_toml_rhythm_court` | `/tmp/ggen-brand-exact-tip6` | **12 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

---

## Prior PARTIAL (retained history)

Tip6 push-first (`bc-c92c420b`) recorded EXIT **137** under thrash/guardian SIGKILL. Superseded by this EXIT0.

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph \
  --test brand_toml_completeness_court \
  --test brand_toml_rhythm_court \
  --locked --target-dir /tmp/ggen-brand-exact-tip6 \
  --config build.jobs=1 -- --test-threads=1
# expect: completeness 5 + rhythm 12; EXIT 0
```

---

## Compact return

```text
tip_base=cfd93c1cbd2cc95c13b7c325bf88a59f22c988f3
branch=cursor/v2711-brand-exact-tip6-bc54
bc=bc-262603dc
EXIT=0
standing=ALIVE (exact tip-cargo)
completeness=5 rhythm=12
prior_partial=bc-c92c420b EXIT137 superseded
production_theme=UNSUPPORTED
ManagePullRequest=not_used crown_force_push=none Foundry_ALIVE=not_claimed
```
