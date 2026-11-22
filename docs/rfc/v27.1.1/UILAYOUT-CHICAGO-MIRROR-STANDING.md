# ui-layout Chicago mirror standing (oxigraph-free Gate-1 backup)

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-CHICAGO-MIRROR-STANDING-27.1.1` |
| Branch | `cursor/v2711-uilayout-chicago-mirror-bc54` |
| Observed tip (this reconfirm) | `2fd8971414519f5a1c51f6b04c1f7a85831def52` |
| Source tip (harness sync) | tip `layout/{ui_layout,taffy_aot,remediation_bound}.rs` + court body byte-identical to `2fd897141` (imports rewritten for harness crate name) |
| Agent | `bc-365b004b` (reconfirm; prior ledger `bc-1e3f6f87` / absorb notes) |
| Standing | **PARTIAL_ALIVE** (oxigraph-free mirror only — **not** tip-cargo `ALIVE`) |
| tip-cargo package court | **not claimed** from this run (exclusive may still hold oxigraph build under `/tmp/ggen-uilayout-*`) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

## Claim boundary

This ledger records an **oxigraph-free** Chicago mirror of tip `layout/ui_layout.rs` +
`tests/ui_layout_court.rs` (same sources as Gate-1 hop 3b). It is a **backup**
path while tip-exclusive `/tmp/ggen-uilayout-*` oxigraph/oxrocksdb builds are held.

| Claim | Standing |
|-------|----------|
| Tip-synced `ui_layout_court` Chicago court via oxigraph-free harness | **PARTIAL_ALIVE** (**11 passed; exit 0**) |
| Oxigraph-free `--lib` (`remediation_bound` unit courts) | **PARTIAL_ALIVE** (**4 passed; exit 0**) |
| Package `cargo test -p ggen-graph --test ui_layout_court` / tip-cargo exclusive | **not claimed here** (do **not** invent tip-cargo `ALIVE` from mirror) |
| Package `cargo test -p ggen-layout-court --lib` this run | **BLOCKED** / aborted — workspace `target/` flock wait → exit **143**; private seeded target began `Compiling oxrocksdb-sys` → aborted (not oxigraph-free) |
| Did not write `/tmp/ggen-uilayout-*` protected targets | **ALIVE** (observation) |

**Antifalsifier:** this mirror alone never promotes Gate-1 / hop-3b / path-crown /
tip-cargo `ALIVE`. Prefer exclusive quiet
`CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court` → **0** for
package ALIVE.

## Evidence reconfirm (2026-11-22, tip `2fd897141`)

| Field | Value |
|-------|-------|
| Harness | `/tmp/ggen-layout-court-chicago-mirror-oxifree` (vendored tip `taffy_aot` + `remediation_bound` + `ui_layout` + rewritten `ui_layout_court`) |
| Target | `CARGO_TARGET_DIR=/tmp/ggen-layout-court-chicago-mirror-target2` (**not** `/tmp/ggen-uilayout-*`) |
| Court log | `/tmp/cursor/uilayout-chicago-mirror-oxifree-court.log` |
| Lib log | `/tmp/cursor/uilayout-chicago-mirror-oxifree-lib2.log` |
| Prebuilt probe | `/tmp/ggen-layout-court-chicago-mirror-target2/debug/deps/ui_layout_court-6bf8f20e38b6c5e2` → **11 passed; exit 0** (`/tmp/cursor/uilayout-chicago-mirror-prebuilt.log`) |
| Court result | **11 passed; 0 failed** |
| Lib result | **4 passed; 0 failed** |
| Exit | **0** |
| Oxigraph / oxrocksdb | **ABSENT** from oxifree compile graph (`rg` of court/lib logs → no matches) |
| Protected targets | `/tmp/ggen-uilayout-*` **not touched** by this agent |

## Package path attempts (honesty)

| Attempt | Result |
|---------|--------|
| `cargo test -p ggen-layout-court --lib` on workspace `target/` | **Blocking** file lock (tip exclusive / concurrent cargo) → process exit **143** |
| Same `--lib` with hardlink-seeded `/tmp/ggen-layout-court-chicago-mirror-target` | Began `Compiling oxrocksdb-sys` → **aborted** (not oxigraph-free) |
| Oxigraph-free chicago mirror harness (above) | **exit 0** → **PARTIAL_ALIVE** |

`layout_court_chicago` / package `ggen-layout-court` remain tip **PRESENT**
(9 + 4 `#[test]` sources) but were **not** package-cargo reconfirmed this run.

## Replay

```bash
# Must NOT use /tmp/ggen-uilayout-* when tip exclusive holds oxigraph there.
HARNESS=/tmp/ggen-layout-court-chicago-mirror-oxifree
TARGET=/tmp/ggen-layout-court-chicago-mirror-target2
TIP=$(git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54)
# sync tip layout sources into harness, then:
cd "$HARNESS"
env -u RUSTC_WRAPPER SCCACHE_DISABLE=1 \
  CARGO_TARGET_DIR="$TARGET" CARGO_BUILD_JOBS=1 CXX=g++ CC=gcc \
  cargo test --test ui_layout_court -- --test-threads=1
# expect: 11 passed; exit 0; no oxrocksdb/oxigraph in the build
cargo test --lib -- --test-threads=1
# expect: 4 passed; exit 0
```

## Exact next

Quiet tip-cargo exclusive (when flock free):
`CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court` → **0**
for tip-cargo `ALIVE`. Optional light package path when headroom allows:
`CXX=g++ RUSTFLAGS='-C link-arg=-L/usr/lib/gcc/x86_64-linux-gnu/13' cargo test -p ggen-layout-court --lib`.
Mirror stays **PARTIAL_ALIVE** backup only.

## Return line

```text
tip=2fd8971414519f5a1c51f6b04c1f7a85831def52 exit=0 standing=PARTIAL_ALIVE
```
