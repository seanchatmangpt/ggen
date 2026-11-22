# brand.toml tip-cargo — ALIVE (exact tip-cargo) receipt (v27.1.1)

```text
cargo_tip=77eb46fd05d5bc847050a8f0a9147e1133e52c83
completeness=ALIVE
rhythm=ALIVE
exit=0
standing=ALIVE (exact tip-cargo)
```

**Exact-tip reconfirm (bc-a184ad6e):** `CARGO_BUILD_JOBS=1` exclusive `/tmp/ggen-brand-exact-tip` EXIT:0 @ `77eb46fd0` for both courts. Live crown at absorb may advance on docs-only tip-wins; brand/graph sources ≡ through `77eb46fd0`. Prior lineage tip-cargo EXIT:0 @ `c91b22561` retained. Production theme remains **UNSUPPORTED** (`REFUSED_PRODUCTION`).

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-TIP-CARGO-STANDING` |
| Official evidence branch | `cursor/v2711-brand-toml-exact-tip-7ac2` (bc-a184ad6e) tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §8 brand rhythm / completeness |
| Cargo evidence tip | `77eb46fd05d5bc847050a8f0a9147e1133e52c83` — **exact tip-cargo ALIVE** (`jobs=1`; completeness 5 + rhythm 12) |
| Lineage evidence | `c91b225614a3ca0472303c05bab25601db1d51e1` — prior **ALIVE (lineage tip-cargo)** |
| Target dir | `/tmp/ggen-brand-exact-tip` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; isolated worktree `/tmp/ggen-brand-exact-tip-wt` |
| Exact-tip reconfirm | **ALIVE** — EXIT:0 @ `77eb46fd0` (2026-11-22T15:09:37Z) |
| OOR / `write_apply` | **untouched** (OOR stays **BLOCKED**; write_apply owned elsewhere) |
| ManagePullRequest | **not used** (parent owns #849) |

**Law:** Official `ggen-graph` tip-cargo EXIT **0** for `brand_toml_completeness_court` +
`brand_toml_rhythm_court` on the live crown promotes Gate-1 brand completeness to
**ALIVE (exact tip-cargo)**. Production theme remains **UNSUPPORTED**
(`REFUSED_PRODUCTION`). Full DTCG color/type/CASTLE inventory remains Gate 2+ /
OOR **G2-13**.

Prior land `cd7c2433b` absorbed `BrandRefusal::Incomplete` + court as
**PARTIAL_ALIVE**. Lineage tip-cargo closed at `c91b22561`. This receipt seals
exact tip-cargo at `77eb46fd0`.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `brand_toml_completeness_court` | `/tmp/ggen-brand-exact-tip` | **5 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |
| `brand_toml_rhythm_court` | `/tmp/ggen-brand-exact-tip` | **12 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

Log: `/tmp/ggen-brand-exact-tip-run.log`.

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
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# cargo evidence @ 77eb46fd0; brand sources ≡ through subsequent docs-only tip-wins
df -h /tmp
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph \
  --test brand_toml_completeness_court \
  --test brand_toml_rhythm_court \
  --locked \
  --target-dir /tmp/ggen-brand-exact-tip \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: completeness 5 passed; rhythm 12 passed; EXIT:0
```

---

## Compact return line

```text
cargo_tip=77eb46fd05d5bc847050a8f0a9147e1133e52c83
EXIT=0
standing=ALIVE (exact tip-cargo)
completeness=5
rhythm=12
landed=cursor/v2711-brand-toml-exact-tip-7ac2
bc=bc-a184ad6e
next=production theme UNSUPPORTED (BRAND-PRODUCTION-THEME); BRCE/R-01 tip-cargo sealed; OOR BLOCKED
```
