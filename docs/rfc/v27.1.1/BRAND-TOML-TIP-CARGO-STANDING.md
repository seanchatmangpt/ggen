# brand.toml tip-cargo — ALIVE receipt (v27.1.1)

```text
tip=c91b225614a3ca0472303c05bab25601db1d51e1
completeness=ALIVE
rhythm=ALIVE
exit=0
standing=ALIVE
```

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-TIP-CARGO-STANDING` |
| Branch | `cursor/v2711-brand-toml-tip-dee5` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §8 brand rhythm / completeness |
| Cargo evidence tip | `c91b225614a3ca0472303c05bab25601db1d51e1` (brand sources ≡ live crown `8f9d8caa2+`) |
| Target dir | `/tmp/ggen-brand-toml-tip` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; isolated worktree |
| OOR / `write_apply` | **untouched** (OOR stays **BLOCKED**; write_apply owned elsewhere) |
| ManagePullRequest | **not used** (parent owns #849) |

**Law:** Official `ggen-graph` tip-cargo EXIT **0** for `brand_toml_completeness_court` +
`brand_toml_rhythm_court` promotes Gate-1 brand completeness from **PARTIAL_ALIVE** /
**BUILD_BROKEN** to tip-cargo **ALIVE**. Production theme remains **UNSUPPORTED**
(`REFUSED_PRODUCTION`). Full DTCG color/type/CASTLE inventory remains Gate 2+ /
OOR **G2-13**.

Prior land `cd7c2433b` absorbed `BrandRefusal::Incomplete` + court as
**PARTIAL_ALIVE** (logic twin 5/5; official tip-cargo **BUILD_BROKEN** oxrocksdb OOM).
This receipt closes the official tip-cargo gap.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `brand_toml_completeness_court` | `/tmp/ggen-brand-toml-tip` | **5 passed; 0 failed** | **0** | **ALIVE** |
| `brand_toml_rhythm_court` | `/tmp/ggen-brand-toml-tip` | **12 passed; 0 failed** | **0** | **ALIVE** |

Log: `/tmp/cursor/brand-toml-tip-cargo.log` · status: `/tmp/cursor/brand-toml-tip-cargo.status`.

Completeness tests exercised (all **ok**):

- `complete_oem_fixtures_encode_grid_touch_gap_and_fluid`
- `missing_grid_base_is_refused_incomplete`
- `missing_grid_half_is_refused_incomplete`
- `missing_inter_target_gap_is_refused_incomplete`
- `missing_touch_floor_is_refused_incomplete`

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
# brand sources ≡ c91b22561 through live tip
df -h /tmp
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph \
  --test brand_toml_completeness_court \
  --test brand_toml_rhythm_court \
  --locked \
  --target-dir /tmp/ggen-brand-toml-tip \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: completeness 5 passed; rhythm 12 passed; EXIT:0
```

---

## Compact return line

```text
tip=c91b225614a3ca0472303c05bab25601db1d51e1
EXIT=0
standing=ALIVE
completeness=5
rhythm=12
landed=cursor/v2711-brand-toml-tip-dee5
next=production theme UNSUPPORTED (BRAND-PRODUCTION-THEME); BRCE/R-01 tip-cargo; OOR BLOCKED
```
