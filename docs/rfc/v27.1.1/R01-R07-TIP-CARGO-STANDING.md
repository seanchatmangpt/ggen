# R-01…R-07 contract tip-cargo — ALIVE (lineage tip-cargo) receipt (v27.1.1)

```text
cargo_tip=cd7c2433b
court=r01_r07_resolution_court
passed=7
exit=0
standing=ALIVE (lineage tip-cargo)
bodies=BLOCKED
```

| Field | Value |
|-------|-------|
| Document ID | `R01-R07-TIP-CARGO-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §11 / D7 |
| Cargo evidence tip | `cd7c2433b` — **ancestor of live crown** |
| Target dir | `/tmp/ggen-r01-r07-tip` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1` |
| Exact-tip reconfirm | **SCHEDULED** — only when no other cargo |
| Bodies | remain **BLOCKED** — contract fence ≠ body closure |
| Kill log | dual thrash PID **1065550** SIGTERM |

**Law:** EXIT **0** on stale tip SHA labels **ALIVE (lineage tip-cargo)** only.
Do **not** invent tip-subject `ALIVE` or body `ALIVE`/`CLOSED` from this receipt.

## Tip-cargo evidence

| Court | Result | Exit | Standing |
|-------|--------|------|----------|
| `r01_r07_resolution_court` | **7 passed; 0 failed** | **0** | **ALIVE (lineage tip-cargo)** |

Log/meta: `/tmp/ggen-r01-r07-tip-cargo.log` (`EXIT:0`) · `/tmp/ggen-r01-r07-tip-cargo.meta` (`PIPE_EXIT:0`).

## Replay

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-r01-r07-tip \
  cargo test -p ggen-graph --test r01_r07_resolution_court \
  --locked --config build.jobs=1 -- --test-threads=1
```
