# ui-layout Chicago mirror standing (oxigraph-free Gate-1 backup)

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-CHICAGO-MIRROR-STANDING-27.1.1` |
| Branch | `cursor/v2711-uilayout-chicago-mirror-bc54` |
| Source tip (harness sync) | `c832cabdcd3378ef528a4ee2ee387d5731d376a8` (~`c832cabdc+`; layout/`ui_layout_court` byte-identical through tip `a29e1cd92`) |
| Branch head (this ledger) | `cursor/v2711-uilayout-chicago-mirror-bc54` atop `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `a29e1cd92` |
| Agent | `bc-1e3f6f87` (backup path; prior notes `bc-365b004b`) |
| Standing | **PARTIAL_ALIVE** |
| Gate-1 package ALIVE | **not claimed** — tip exclusive `cargo test -p ggen-graph --test ui_layout_court` under `/tmp/ggen-uilayout-tip-alive-protected` still required |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

## Claim boundary

This ledger records an **oxigraph-free** Chicago mirror of tip `layout/ui_layout.rs` +
`tests/ui_layout_court.rs` (same sources as Gate-1 hop 3b). It is a **backup**
path while tip-exclusive flock `/tmp/ggen-uilayout-tip-alive-protected` is held by
package cargo.

| Claim | Standing |
|-------|----------|
| Tip-synced `ui_layout_court` Chicago court via oxigraph-free harness | **PARTIAL_ALIVE** (**11 passed; exit 0**) |
| Package `cargo test -p ggen-graph --test ui_layout_court` on tip | **PENDING** / tip exclusive (do **not** invent Gate-1 **ALIVE** from this mirror) |
| Did not take tip exclusive flock | **ALIVE** (observation: flock **HELD** by tip-cargo; mirror used separate target) |

**Antifalsifier:** do **not** promote Gate-1 / hop-3b / path-crown `ALIVE` from this
mirror alone. Tip exclusive package court exit **0** remains the Gate-1 ALIVE
falsifier.

## Evidence (2026-11-22)

| Field | Value |
|-------|-------|
| Harness | `/tmp/ggen-layout-court-chicago-mirror-oxifree` (vendored tip `taffy_aot` + `remediation_bound` + `ui_layout` + rewritten `ui_layout_court`) |
| Target | `CARGO_TARGET_DIR=/tmp/ggen-layout-court-chicago-mirror-target2` |
| Log | `/tmp/cursor/uilayout-chicago-mirror-oxifree-target2.log` |
| Result | **11 passed; 0 failed** |
| Exit | **0** |
| Oxigraph / oxrocksdb | **ABSENT** from compile graph (`rg` of log → no matches) |
| Tip flock | `/tmp/ggen-uilayout-tip-alive-protected` **HELD** by tip-cargo — **not taken** by this agent |
| Competing target | `/tmp/ggen-layout-court-chicago-mirror-target` (other oxrocksdb build) — **not used** |

## Replay

```bash
# Must NOT flock /tmp/ggen-uilayout-tip-alive-protected when tip exclusive holds it.
HARNESS=/tmp/ggen-layout-court-chicago-mirror-oxifree
TARGET=/tmp/ggen-layout-court-chicago-mirror-target2
# harness already tip-synced from c832cabdc layout sources + rust-toolchain.toml
cd "$HARNESS"
env -u RUSTC_WRAPPER SCCACHE_DISABLE=1 \
  CARGO_TARGET_DIR="$TARGET" CARGO_BUILD_JOBS=1 CXX=g++ CC=gcc \
  cargo test --test ui_layout_court -- --test-threads=1
# expect: 11 passed; exit 0; no oxrocksdb/oxigraph in the build
```

## Exact next (Gate-1 ALIVE)

```bash
# Only when tip exclusive flock is free / held by the tip-alive agent:
flock /tmp/ggen-uilayout-tip-alive-protected \
  env CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/ggen-uilayout-tip-alive-target \
  cargo test -p ggen-graph --test ui_layout_court --locked -- --test-threads=1
# expect: 11 passed; exit 0 → Gate-1 hop-3b tip-cargo ALIVE
```

## Return line

```text
exit=0 standing=PARTIAL_ALIVE tip=c832cabdcd3378ef528a4ee2ee387d5731d376a8 next=tip-exclusive package ui_layout_court under /tmp/ggen-uilayout-tip-alive-protected
```
