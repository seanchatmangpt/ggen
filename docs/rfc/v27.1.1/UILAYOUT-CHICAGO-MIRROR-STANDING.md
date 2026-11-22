# ui-layout Chicago mirror standing (oxigraph-free Gate-1 backup)

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-CHICAGO-MIRROR-STANDING-27.1.1` |
| Branch | `cursor/v2711-uilayout-chicago-mirror-bc54` |
| Source tip (harness sync) | `c832cabdcd3378ef528a4ee2ee387d5731d376a8` (~`c832cabdc+`; layout/`ui_layout_court` byte-identical through tip `a29e1cd92`) |
| Branch head (this ledger) | absorbed onto tip via `cursor/v2711-absorb-smalt-uilayout-5cea` (source `cursor/v2711-uilayout-chicago-mirror-bc54` @ `1b0072b6c`) |
| Agent | `bc-1e3f6f87` (backup path; prior notes `bc-365b004b`) |
| Standing | **PARTIAL_ALIVE** (oxigraph-free mirror only) |
| Gate-1 package ALIVE | **ALIVE on tip** via absorb of #926 @ `8090022f4` → tip `27436608c` (exclusive quiet **11 passed; exit 0** @ `ca52e9c16`). This mirror remains backup **PARTIAL_ALIVE** — do not double-count as a second package ALIVE |

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
| Package `cargo test -p ggen-graph --test ui_layout_court` on tip | **ALIVE** via #926 absorb @ tip `27436608c` (exclusive quiet @ `ca52e9c16`) — **not** from this mirror |
| Did not take tip exclusive flock | **ALIVE** (observation: flock **HELD** by tip-cargo during mirror; mirror used separate target) |

**Antifalsifier:** this mirror alone never promotes Gate-1 / hop-3b / path-crown
`ALIVE`. Tip exclusive package court exit **0** (#926) is the Gate-1 ALIVE
falsifier; mirror stays **PARTIAL_ALIVE** backup evidence.

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

## Exact next (post-absorb)

Tip exclusive package court **ALIVE** already absorbed (#926 → tip `27436608c`).
Mirror ledger remains **PARTIAL_ALIVE** backup. Next Gate-1 falsifiers: quiet
tip-cargo reconfirm of remaining fences (`shui` / Taffy / brand) — not a second
`ui_layout` ALIVE invent from this mirror.

## Return line

```text
exit=0 standing=PARTIAL_ALIVE tip=27436608c absorbed=#927 next=remaining Gate-1 tip-cargo fences (ui_layout ALIVE via #926)
```
