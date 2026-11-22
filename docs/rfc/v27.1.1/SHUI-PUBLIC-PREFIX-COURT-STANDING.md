# `shui:` public-prefix court standing (Gate-1 tip reconfirm)

```text
tip=c329b7ee5c15936f662f4eef996967bd66a6daa8
exit=SKIPPED
standing=PENDING
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-shui-prefix-court-bc54` |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c329b7ee5c15936f662f4eef996967bd66a6daa8` (≥ `b565b8e83` / ~`ad290800f+` / `ba9824207+` / `8cee4246f+`) |
| Court | `crates/ggen-graph/tests/shui_public_prefix_court.rs` — blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (**4** `#[test]`) |
| Vocab | `crates/ggen-graph/src/vocab/` (`shui` + `TurtlePrefixStanding`) |
| Lineage tip-cargo | **ALIVE** @ evidence `26256ca9d413617f0e8b1418d101708ec4fbc2a3` — **4 passed; exit 0** (`/tmp/gate1-next-shui`; [`GATE-1-NEXT-COURTS-STANDING.md`](./GATE-1-NEXT-COURTS-STANDING.md)); court blob identical through this tip |
| Ancestor evidence | `0478fde59` — **3 passed; exit 0** (pre-strengthen) |
| This agent cargo | **SKIPPED** — loadavg_1m **>12** (spare → docs-only). Prior cold attempt mid-compile incomplete (`/tmp/ggen-shui-prefix-target`; log ends at `sparopt`; tee-masked `EXIT:0` ≠ cargo exit). Exclusive thrash: ui_layout oxrocksdb + concurrent `/tmp/ggen-shui-prefix-target2` |
| Standing (this reconfirm) | **PENDING** — do **not** invent tip-exit **0** from incomplete / spared run |
| Standing (lineage) | **ALIVE** retained @ `26256ca9d` (sources ≡ tip) — fence ≠ Truex product crown |
| Truex product admit | **ALIVE** @ `truex@68d2d233` (merged) — separate row |
| ManagePullRequest | **not used** |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

## Claim boundary

| Claim | Standing |
|-------|----------|
| Tip-synced court source **PRESENT** (4 tests; W3C slash IRI; ADMITTED / REFUSED_PRIVATE / REFUSED_UNKNOWN; open-admitted UI IRIs) | **ALIVE** (presence) |
| Lineage tip-cargo `shui_public_prefix_court` exit **0** | **ALIVE** @ `26256ca9d` |
| This-agent exclusive tip-cargo reconfirm on `c329b7ee5` / `8cee4246f+` | **PENDING** / **SKIPPED** (load&gt;12) |
| Truex product admit of `shui:` shapes | **ALIVE** (sibling; non-substitute) |

**Antifalsifier:** spare / incomplete compile / tee exit ≠ tip-cargo `ALIVE`. Lineage
`ALIVE` ≠ this-agent tip-exit **0**. Fence ≠ Truex crown.

## Spare evidence (2026-11-22)

| Field | Value |
|-------|-------|
| Decision | loadavg_1m **16.66** (&gt;12) → spare cargo; docs-only |
| Attempt window | `2026-11-22T12:29:02Z` → `12:37:52Z` (`CARGO_BUILD_JOBS=1` `/tmp/ggen-shui-prefix-target`) |
| Attempt outcome | Incomplete cold compile (no `test result:` / no `Finished`); status file recorded tee exit, not cargo |
| Concurrent | `/tmp/ggen-uilayout-0ae8-protected` oxrocksdb + `/tmp/shui_public_prefix_court3.log` target2 |

## Exact next (when loadavg_1m ≤12 + exclusive quiet)

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test shui_public_prefix_court \
  --locked --target-dir /tmp/ggen-shui-prefix-target
# expect: 4 passed; 0 failed; exit 0 → this-agent tip-cargo ALIVE
```

## Return line

```text
tip=c329b7ee5c15936f662f4eef996967bd66a6daa8 exit=SKIPPED standing=PENDING
```
