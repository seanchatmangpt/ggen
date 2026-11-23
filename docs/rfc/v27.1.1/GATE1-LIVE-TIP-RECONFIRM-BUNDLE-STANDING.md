# Gate 1 live-tip reconfirm bundle (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `GATE1-LIVE-TIP-RECONFIRM-BUNDLE-STANDING` |
| Subject tip | `cursor/v2711-r01-r07-bodies-admit-bc54` @ `ef3921a7e`+ (⊇ crown `d6c04b390`) |
| Target dir | `/tmp/gate1-tip7-brand` (+ shui `/tmp/gate1-tip7-shui`) |
| Standing | **ALIVE (live-tip reconfirm bundle)** |
| Foundry / path crown / zero-HITL | **NOT** promoted |

## Courts (EXIT 0 this lane)

| Court | Passed | Standing ledger |
|-------|--------|-----------------|
| `shui_public_prefix_court` | **4** | [`SHUI-PUBLIC-PREFIX-LIVE-TIP-RECONFIRM-STANDING.md`](./SHUI-PUBLIC-PREFIX-LIVE-TIP-RECONFIRM-STANDING.md) |
| `ui_layout_court` | **11** | [`UILAYOUT-LIVE-TIP-RECONFIRM-STANDING.md`](./UILAYOUT-LIVE-TIP-RECONFIRM-STANDING.md) |
| `taffy_aot_preflight_court` | **7** | this bundle |
| `brand_toml_completeness_court` | **5** | this bundle |
| `brand_toml_hotswap_court` | **6** | this bundle |
| `brand_toml_rhythm_court` | **12** | this bundle |

**Total:** **45** passed / EXIT **0** across Gate1 in-repo courts on live tip.

## Replay

```bash
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
CARGO_TARGET_DIR=/tmp/gate1-tip7-shui \
  cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
CARGO_TARGET_DIR=/tmp/gate1-tip7-brand \
  cargo test -p ggen-graph \
  --test ui_layout_court --test taffy_aot_preflight_court \
  --test brand_toml_completeness_court --test brand_toml_hotswap_court \
  --test brand_toml_rhythm_court --locked \
  --config build.jobs=1 -- --test-threads=1
```

**Law:** Bundle ALIVE ≠ Foundry ALIVE ≠ photolithography path crown ALIVE ≠ zero-HITL COMPLETE.
Truex product admit remains sibling-ALIVE (held). Full DTCG inventory **UNSUPPORTED**.
