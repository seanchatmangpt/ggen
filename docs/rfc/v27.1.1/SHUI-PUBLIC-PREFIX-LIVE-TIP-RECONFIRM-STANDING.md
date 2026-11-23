# shui: public-prefix fence — live-tip reconfirm (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-LIVE-TIP-RECONFIRM-STANDING` |
| Subject tip | feature `cursor/v2711-r01-r07-bodies-admit-bc54` @ `9dc7388b2` (⊇ crown `d6c04b390`) |
| Court | `shui_public_prefix_court` |
| Result | **4 passed; EXIT 0** |
| Target dir | `/tmp/gate1-tip7-shui` |
| Standing | **ALIVE (live-tip reconfirm)** |
| Sealed tip-cargo lineage | **ALIVE** @ `a4b3d3fb4` held — this reconfirm does not demote or replace it |
| Foundry / zero-HITL | **NOT** promoted |

## Command

```bash
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/gate1-tip7-shui \
  cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# → 4 passed; EXIT 0
```

**Law:** Live-tip reconfirm ≠ Foundry ALIVE ≠ path crown ALIVE ≠ zero-HITL COMPLETE.
Prior tip7 thrash PARTIAL EXIT:**101** is superseded for this live-tip subject class.
