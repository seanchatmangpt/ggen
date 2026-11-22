# ui-layout Chicago mirror standing (oxigraph-free Gate-1 backup)

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-CHICAGO-MIRROR-STANDING-27.1.1` |
| Branch | `cursor/v2711-uilayout-chicago-mirror-bc54` |
| Observed tip (this reconfirm) | `2fd8971414519f5a1c51f6b04c1f7a85831def52` |
| Source tip (harness sync) | tip `layout/{ui_layout,taffy_aot,remediation_bound}.rs` + court body byte-identical to `2fd897141` (imports rewritten for harness crate name) |
| Branch head (this ledger) | absorb of `cursor/v2711-uilayout-chicago-mirror-bc54` @ `f771cac9b` onto tip `89ca2da9c+` (prior absorb `1b0072b6c` via `cursor/v2711-absorb-smalt-uilayout-5cea`) |
| Agent | `bc-365b004b` (reconfirm; prior ledger `bc-1e3f6f87` / absorb notes) |
| Standing | **PARTIAL_ALIVE** (oxigraph-free mirror only — **not** a second tip-cargo `ALIVE`) |
| Gate-1 package ALIVE | **ALIVE on tip** via absorb of #926 @ `8090022f4` → tip `27436608c` (exclusive quiet **11 passed; exit 0** @ `ca52e9c16`). This mirror remains backup **PARTIAL_ALIVE** — do not double-count as a second package ALIVE |

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
| Package `cargo test -p ggen-graph --test ui_layout_court` on tip | **ALIVE** via #926 absorb @ tip `27436608c` (exclusive quiet @ `ca52e9c16`) — **not** from this mirror |
| Package `cargo test -p ggen-layout-court --lib` this reconfirm | **BLOCKED** / aborted — workspace `target/` flock wait → exit **143**; private seeded target began `Compiling oxrocksdb-sys` → aborted (not oxigraph-free) |
| Did not write `/tmp/ggen-uilayout-*` protected targets | **ALIVE** (observation) |

**Antifalsifier:** this mirror alone never promotes Gate-1 / hop-3b / path-crown /
tip-cargo `ALIVE`. Tip exclusive package court exit **0** (#926 → tip `27436608c`)
is the Gate-1 ALIVE falsifier; mirror stays **PARTIAL_ALIVE** backup evidence.

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
Tip-cargo `ui_layout_court` **ALIVE** remains via prior #926 absorb (not inventable from mirror).

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

Tip exclusive package court **ALIVE** already absorbed (#926 → tip `27436608c`).
Mirror ledger remains **PARTIAL_ALIVE** backup. Next Gate-1 falsifiers: quiet
tip-cargo reconfirm of remaining fences (`shui` / Taffy / brand) — not a second
`ui_layout` ALIVE invent from this mirror. Optional light package path when
headroom allows:
`CXX=g++ RUSTFLAGS='-C link-arg=-L/usr/lib/gcc/x86_64-linux-gnu/13' cargo test -p ggen-layout-court --lib`.

## Return line

```text
tip=89ca2da9c37d7ba61abd665fe5c8de176201ae33 exit=0 standing=PARTIAL_ALIVE absorbed=f771cac9b next=remaining Gate-1 tip-cargo fences (ui_layout ALIVE via #926)
```
