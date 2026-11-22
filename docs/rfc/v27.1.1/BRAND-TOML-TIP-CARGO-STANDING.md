# brand.toml tip-cargo — ALIVE (lineage tip-cargo) receipt (v27.1.1)

```text
cargo_tip=c91b225614a3ca0472303c05bab25601db1d51e1
completeness=ALIVE
rhythm=ALIVE
exit=0
standing=ALIVE (lineage tip-cargo)
```

**Honesty pin (Gate-1 retip `cursor/v2711-gate1-honesty-retip-5536` on tip `fdcad534d`):** lineage tip-cargo EXIT:0 @ `c91b22561` is retained as evidence. Tip-subject standing in [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md) / [`CROWN-PROGRESS-RECEIPT.md`](./CROWN-PROGRESS-RECEIPT.md) remains **COMPILING** / **PARTIAL_ALIVE** until exact live-crown EXIT:0. Do **not** invent tip-subject `ALIVE`.

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-TIP-CARGO-STANDING` |
| Official evidence branch | `cursor/v2711-brand-toml-tip-dee5` @ `72b762f20` (bc-7f0c72b7) |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §8 brand rhythm / completeness |
| Cargo evidence tip | `c91b225614a3ca0472303c05bab25601db1d51e1` — **ancestor of live crown**; label **ALIVE (lineage tip-cargo)** until exact-tip reconfirm EXIT:0 with `jobs=1` |
| Target dir | `/tmp/ggen-brand-toml-tip` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; isolated worktree |
| Exact-tip reconfirm | **SCHEDULED** — deferred while other oxrocksdb cargo (r01) is exclusive |
| OOR / `write_apply` | **untouched** (OOR stays **BLOCKED**; write_apply owned elsewhere) |
| ManagePullRequest | **not used** (parent owns #849) |

**Law:** Official `ggen-graph` tip-cargo EXIT **0** for `brand_toml_completeness_court` +
`brand_toml_rhythm_court` promotes Gate-1 brand completeness from **PARTIAL_ALIVE** /
**BUILD_BROKEN** to **ALIVE (lineage tip-cargo)**. Production theme remains **UNSUPPORTED**
(`REFUSED_PRODUCTION`). Full DTCG color/type/CASTLE inventory remains Gate 2+ /
OOR **G2-13**. Do **not** promote to exact tip-cargo `ALIVE` without reconfirm on live crown.

Prior land `cd7c2433b` absorbed `BrandRefusal::Incomplete` + court as
**PARTIAL_ALIVE** (logic twin 5/5; official tip-cargo **BUILD_BROKEN** oxrocksdb OOM).
This receipt closes the official tip-cargo gap at lineage SHA `c91b22561`.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `brand_toml_completeness_court` | `/tmp/ggen-brand-toml-tip` | **5 passed; 0 failed** | **0** | **ALIVE (lineage tip-cargo)** |
| `brand_toml_rhythm_court` | `/tmp/ggen-brand-toml-tip` | **12 passed; 0 failed** | **0** | **ALIVE (lineage tip-cargo)** |

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
# cargo evidence @ c91b22561 (ancestor); brand sources ≡ through live tip
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

Exact-tip reconfirm (only when no other oxrocksdb cargo / r01 is running):

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# same cargo command at live tip with jobs=1 → EXIT:0 promotes to exact tip-cargo ALIVE
```

---

## Compact return line

```text
cargo_tip=c91b225614a3ca0472303c05bab25601db1d51e1
EXIT=0
standing=ALIVE (lineage tip-cargo)
completeness=5
rhythm=12
landed=cursor/v2711-brand-toml-tip-dee5@72b762f20
bc=bc-7f0c72b7
next=exact-tip reconfirm when cargo exclusive; production theme UNSUPPORTED (BRAND-PRODUCTION-THEME); BRCE/R-01 tip-cargo; OOR BLOCKED
```
