# brand.toml tip-cargo — PARTIAL_ALIVE (retip5) / lineage ALIVE (v27.1.1)

```text
tip_base=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
branch=cursor/v2711-brand-exact-retip5-bc54
standing=PARTIAL_ALIVE
reason=court_budget_<3min_REFUSED (cargo thrash n≈20 + exclusive-lock queue)
lineage_alive_tip=77eb46fd05d5bc847050a8f0a9147e1133e52c83
completeness_lineage=ALIVE (5)
rhythm_lineage=ALIVE (12)
exact_tip_reconfirm=PARTIAL (not EXIT:0 on tip_base)
```

**Retip5 push-first honesty (bc-ec3bbaf0):** tip base
`167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` on
`cursor/v2711-brand-exact-retip5-bc54`. Prior tip4
(`cursor/v2711-brand-exact-retip4-bc54`) **never appeared on origin**
(stuck on `/tmp/ggen-tip-exclusive.lock`). This land **must PUSH**.
Court budget **&lt;3min** → **PARTIAL_ALIVE** (do **not** invent tip-subject
`ALIVE`). Lineage tip-cargo EXIT:0 @ `77eb46fd0` retained (ancestor of tip;
completeness **5** + rhythm **12** sources present on tip). Production theme
remains **UNSUPPORTED** (`REFUSED_PRODUCTION`). No crown absorb. No ManagePullRequest.
No force-push.

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-TIP-CARGO-STANDING` |
| Retip5 branch | `cursor/v2711-brand-exact-retip5-bc54` (bc-ec3bbaf0) — **PUSH-FIRST** |
| Tip base | `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` |
| Retip5 standing | **PARTIAL_ALIVE** — exact-tip court **not** reconfirmed under &lt;3min budget |
| Official evidence branch (lineage) | `cursor/v2711-brand-toml-exact-tip-7ac2` (bc-a184ad6e) tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §8 brand rhythm / completeness |
| Cargo evidence tip (lineage) | `77eb46fd05d5bc847050a8f0a9147e1133e52c83` — **ALIVE (lineage tip-cargo)** (`jobs=1`; completeness 5 + rhythm 12) |
| Lineage evidence | `c91b225614a3ca0472303c05bab25601db1d51e1` — prior **ALIVE (lineage tip-cargo)** |
| Target dir | `/tmp/ggen-brand-exact-tip` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; isolated worktree |
| Exact-tip reconfirm @ tip_base | **PARTIAL** — cargo thrash (`n≈20` concurrent `cargo test`) + exclusive-lock queue; warm target `/tmp/ggen-brand-exact-tip` **ABSENT**; tip4 never pushed |
| Prior tip4 | `cursor/v2711-brand-exact-retip4-bc54` — **ABSENT on origin** (lock wait @ 2026-11-22T17:29:47Z) |
| OOR / `write_apply` | **untouched** (OOR stays **BLOCKED**; write_apply owned elsewhere) |
| ManagePullRequest | **not used** |
| Crown absorb | **NONE** |

**Law:** Official `ggen-graph` tip-cargo EXIT **0** for `brand_toml_completeness_court` +
`brand_toml_rhythm_court` on the live crown promotes Gate-1 brand completeness to
**ALIVE (exact tip-cargo)**. Production theme remains **UNSUPPORTED**
(`REFUSED_PRODUCTION`). Full DTCG color/type/CASTLE inventory remains Gate 2+ /
OOR **G2-13**.

Prior land `cd7c2433b` absorbed `BrandRefusal::Incomplete` + court as
**PARTIAL_ALIVE**. Lineage tip-cargo closed at `c91b22561` and sealed exact
tip-cargo at `77eb46fd0`. Retip5 on tip_base `167352d0d` is **PARTIAL_ALIVE**
until the Exact next verifier returns EXIT:**0** (do not invent tip-subject
`ALIVE` from lineage alone).

---

## Tip-cargo evidence (lineage — retained)

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `brand_toml_completeness_court` | `/tmp/ggen-brand-exact-tip` | **5 passed; 0 failed** | **0** | **ALIVE (lineage tip-cargo @ `77eb46fd0`)** |
| `brand_toml_rhythm_court` | `/tmp/ggen-brand-exact-tip` | **12 passed; 0 failed** | **0** | **ALIVE (lineage tip-cargo @ `77eb46fd0`)** |

Log: `/tmp/ggen-brand-exact-tip-run.log` (2026-11-22T15:09:37Z). Tip sources on
`167352d0d`: completeness **5** `#[test]`, rhythm **12** `#[test]`;
`77eb46fd0` is an ancestor of tip_base.

Completeness tests exercised (all **ok** at lineage tip):

- `complete_oem_fixtures_encode_grid_touch_gap_and_fluid`
- `missing_grid_base_is_refused_incomplete`
- `missing_grid_half_is_refused_incomplete`
- `missing_inter_target_gap_is_refused_incomplete`
- `missing_touch_floor_is_refused_incomplete`

---

## Exact next verifier (retip5 → tip-subject ALIVE)

```bash
git fetch origin cursor/v2711-brand-exact-retip5-bc54
git checkout cursor/v2711-brand-exact-retip5-bc54
git rev-parse HEAD   # expect 167352d0d… + honesty commit
# quiet cargo / exclusive lock; warm or cold target OK
df -h /tmp
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph \
  --test brand_toml_completeness_court \
  --test brand_toml_rhythm_court \
  --locked \
  --target-dir /tmp/ggen-brand-exact-tip \
  --config build.jobs=1 \
  -- --test-threads=1
# PASS only: completeness 5 + rhythm 12; EXIT:0 → tip-subject ALIVE
# Until then: PARTIAL_ALIVE — refuse inventing tip-subject ALIVE from lineage alone
```

**Refuse:** promoting tip-subject brand exact-tip to `ALIVE` from lineage
`77eb46fd0`, source presence, or this docs retip alone.

---

## Compact return line (retip5)

```text
tip_base=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
branch=cursor/v2711-brand-exact-retip5-bc54
EXIT=PARTIAL (court not run under <3min)
standing=PARTIAL_ALIVE
lineage_alive=77eb46fd05d5bc847050a8f0a9147e1133e52c83
completeness_lineage=5
rhythm_lineage=12
bc=bc-ec3bbaf0
next=exclusive CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip --config build.jobs=1 -- --test-threads=1 → EXIT:0
no_crown_absorb=1
no_mpr=1
no_force_push=1
```
