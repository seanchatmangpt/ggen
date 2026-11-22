# Gate-1 remaining courts standing

Branch: `cursor/v2711-gate1-remaining-courts-bc54` (FF of ecosystem tip).
Land commits already on tip: `eaf0dad40` (LayoutRefusal + `slope_cqi`), `7c0ba2a90` /
`6ab75e6a3` (ui-layout viewport + JSON harness + anti-vacuity).

## Falsifiers (reuse `crates/ggen-graph/src/layout/` + shui vocab — no forks)

| # | Falsifier | In-repo surface | Next verifier | Standing |
|---|-----------|-----------------|---------------|----------|
| 1 | `verify_witness` ≤250KB (`< 262_144`) | `src/witness/mod.rs` + `tests/verify_witness_size_court.rs` | `cargo test -p ggen-graph --test verify_witness_size_court` | **ALIVE** (size court). Production artifact path typed **UNSUPPORTED** when unset — [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md). |
| 2 | `ui-layout-court` Taffy AOT + anti-vacuity | `layout/ui_layout.rs` + `tests/ui_layout_court.rs` (mutation refuse Overflow@320) | `cargo test -p ggen-graph --test ui_layout_court` | **ALIVE** (in-repo). zcode-cli TS runner **UNSUPPORTED** (out-of-repo next verifier). |
| 3 | brand.toml fluid `slope_cqi` | `layout/brand.rs` + OEM fixtures + `brand_fluid_clamp_court` / extended rhythm court | `cargo test -p ggen-graph --test brand_fluid_clamp_court` | **ALIVE** (schema + fixtures + courts on tip ≥ `eaf0dad40`). |
| 4 | `LayoutRefusal` parity | `layout/taffy_aot.rs` (`LayoutRefusal = AotRefusal`) + Overflow/Overlap/Touch/Gap courts naming width | `cargo test -p ggen-graph --test taffy_aot_preflight_court` | **ALIVE** (all four variants name `viewport_width_px`). |

## Replay

```bash
export CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/target-gate1-remain RUSTC_WRAPPER=
cargo test -p ggen-graph --test verify_witness_size_court -- --test-threads=1
cargo test -p ggen-graph --test ui_layout_court -- --test-threads=1
cargo test -p ggen-graph --test brand_toml_rhythm_court -- --test-threads=1
cargo test -p ggen-graph --test brand_fluid_clamp_court -- --test-threads=1
cargo test -p ggen-graph --test taffy_aot_preflight_court -- --test-threads=1
```

Shared-pod cgroup ~15GB + concurrent oxrocksdb cold builds can SIGKILL full
`ggen-graph` links; record exact tip SHA + exit 0 under exclusive target when
reconfirming. Tip-synced oxigraph-free harnesses compiling the same `layout/*.rs`
sources are **PARTIAL_ALIVE** until package courts exit 0 on that SHA.
