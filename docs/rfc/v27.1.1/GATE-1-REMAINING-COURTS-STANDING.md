# Gate-1 remaining courts standing

Branch: `cursor/v2711-gate1-remaining-courts-bc54` (FF of ecosystem tip).
Land commits already on tip: `eaf0dad40` (LayoutRefusal + `slope_cqi`), `7c0ba2a90` /
`6ab75e6a3` (ui-layout viewport + JSON harness + anti-vacuity).

## Falsifiers (reuse `crates/ggen-graph/src/layout/` + shui vocab — no forks)

| # | Falsifier | In-repo surface | Next verifier | Standing |
|---|-----------|-----------------|---------------|----------|
| 1 | `verify_witness` ≤250KB (`< 262_144`) | `src/witness/mod.rs` + `tests/verify_witness_size_court.rs` | `cargo test -p ggen-graph --test verify_witness_size_court` | Size court **ALIVE** tip-cargo **6 passed; EXIT=0** @ `f5f44571f` (absorb base `9173c6529` post-Smalt; sources ≡; placeholder **35** B). Affidavit pin size court **ALIVE** (**28673** B @ `563eb322a`) — [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md). ggen production wire **UNSUPPORTED** — [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md). |
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

## Evidence recorded (tip `53dda8ffa7c65b12320854ee0db8a305372b0849`)

Oxigraph-free tip-synced harnesses compiling the same `layout/*.rs` / witness
sources (pod OOM blocked exclusive `cargo test -p ggen-graph` under 15–21 concurrent cargos):

| Court harness | Result | Exit |
|---------------|--------|------|
| `verify_witness_size_court` warm binary (`/tmp/vw-tip-target`) | **6 passed** | **0** |
| LayoutRefusal tip-verify (`taffy_aot` + court, tip SHA sources) | **7 passed** (Overflow/Overlap/Touch/Gap name width) | **0** |
| brand fluid tip-verify (`brand_fluid_clamp_court` + `brand_toml_rhythm_court`) | **6 + 11 passed** | **0** |
| ui-layout tip-verify (`ui_layout_court` + remediation_bound) | **11 + 4 passed** (incl. anti-vacuity mutation) | **0** |

### Chicago mirror reconfirm (tip `c832cabdc+`, 2026-11-22; absorb honesty)

Backup path while tip exclusive flock `/tmp/ggen-uilayout-tip-alive-protected` was
**HELD** (not taken). Oxigraph-free harness on
`CARGO_TARGET_DIR=/tmp/ggen-layout-court-chicago-mirror-target2`:

| Court harness | Result | Exit | Standing |
|---------------|--------|------|----------|
| ui-layout Chicago mirror (`ui_layout_court` tip sources; no oxigraph/oxrocksdb) | **11 passed** | **0** | **PARTIAL_ALIVE** |

Ledger: [`UILAYOUT-CHICAGO-MIRROR-STANDING.md`](./UILAYOUT-CHICAGO-MIRROR-STANDING.md).
Tip exclusive package court **ALIVE** absorbed via #926 @ tip `27436608c` —
mirror stays backup **PARTIAL_ALIVE** only.

Package falsifier (preferred CI): still `cargo test -p ggen-graph --test <court>` under
`CARGO_TARGET_DIR=/tmp/target-gate1-remain` when exclusive headroom allows.
