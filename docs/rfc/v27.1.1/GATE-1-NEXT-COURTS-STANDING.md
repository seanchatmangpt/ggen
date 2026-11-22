# Gate-1 next courts — tip-cargo standing

```text
tip=26256ca9d413617f0e8b1418d101708ec4fbc2a3
shui=ALIVE
taffy=ALIVE
brand=ALIVE
standing=ALIVE
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-gate1-next-courts-bc54` |
| Base tip | `26256ca9d413617f0e8b1418d101708ec4fbc2a3` (quiet tip-cargo on `cursor/v2711-gate1-next-courts-bc54`; crown observed `678a6c106`) |
| Constraint | `CARGO_BUILD_JOBS=1`; separate `/tmp` target-dirs; loadavg prefer ≤10 |
| ManagePullRequest | **not used** |

## Courts (tip-cargo)

| Court | Target dir | Result | Exit | Standing |
|-------|------------|--------|------|----------|
| `shui_public_prefix_court` | `/tmp/gate1-next-shui` | **4 passed; 0 failed** | **0** | **ALIVE** |
| `taffy_aot_preflight_court` | `/tmp/gate1-next-taffy` | **7 passed; 0 failed** | **0** | **ALIVE** |
| `brand_toml_rhythm_court` | `/tmp/gate1-next-brand` | **12 passed; 0 failed** | **0** | **ALIVE** |

Companion lib fence (post-fix): `cargo test -p ggen-graph --lib layout::brand::` → **7 passed; exit 0**.

## Brand fix (required for tip-cargo ALIVE)

`dtcg_has_primitive_space` used `str::parse::<toml::Value>()`. Under workspace `toml = "1.1.6"` that API rejects real documents (`unexpected content, expected nothing` at line 1), so every OEM dual-surface fixture collapsed to `false` and `oem_fixtures_carry_dtcg_primitive_space_from_rhythm` **FAILED** (11/12).

Repair: parse as `toml::Table` (toml 1.x document root). Also `ref`-bind two lib-test `@` patterns that E0382 under current nightly when compiling `cfg(test)`.

Pre-fix tip-cargo brand standing would have been **FAILED**; post-fix tip-cargo = **ALIVE**. Production theme remains **UNSUPPORTED** (`REFUSED_PRODUCTION`); in-repo hot-swap fence is tip-cargo **ALIVE** on evidence `26256ca9d` (≠ production theme `ALIVE`).

## Replay

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1
CARGO_TARGET_DIR=/tmp/gate1-next-shui cargo test -p ggen-graph --test shui_public_prefix_court --locked -- --test-threads=1
CARGO_TARGET_DIR=/tmp/gate1-next-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked -- --test-threads=1
CARGO_TARGET_DIR=/tmp/gate1-next-brand cargo test -p ggen-graph --test brand_toml_rhythm_court --locked -- --test-threads=1
```

## Aggregate honesty

- These three fences: tip-cargo **ALIVE** on `27436608c` (+ brand Table fix on this branch).
- Gate-1 crown aggregate remains **PARTIAL** until remaining fences (`verify_witness_size_court`, BRCE seat tip-cargo, production theme, etc.) and out-of-repo HITL clear — do not invent crown `ALIVE`.
- `ui_layout` tip-cargo **ALIVE** retained from prior absorb on tip lineage.
