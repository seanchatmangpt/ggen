# ui_layout_court — live-tip reconfirm (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-LIVE-TIP-RECONFIRM-STANDING` |
| Subject tip | feature `cursor/v2711-r01-r07-bodies-admit-bc54` @ `9dc7388b2` (⊇ crown `d6c04b390`) |
| Court | `ui_layout_court` |
| Result | **11 passed; EXIT 0** |
| Target dir | `/tmp/gate1-tip7-brand` |
| Standing | **ALIVE (live-tip reconfirm)** |
| Sealed tip-cargo lineage | **ALIVE** @ `2685119bd` held |
| Foundry / zero-HITL / path crown | **NOT** promoted |

## Command

```bash
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/gate1-tip7-brand \
  cargo test -p ggen-graph --test ui_layout_court --locked \
  --config build.jobs=1 -- --test-threads=1
# → 11 passed; EXIT 0
```

**Law:** Live-tip reconfirm ≠ Foundry ALIVE ≠ path crown ALIVE. Sibling `ui-layout-court.ts` remains **UNSUPPORTED**.
