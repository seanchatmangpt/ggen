# R-01…R-07 contract tip-cargo — ALIVE (lineage tip-cargo) receipt (v27.1.1)

```text
cargo_tip=cd7c2433b
court=r01_r07_resolution_court
passed=7
exit=0
standing=ALIVE (lineage tip-cargo)
bodies=BLOCKED
brce_seat=ALIVE (lineage tip-cargo; EXIT:0 @ 59c090984; sources ≡ tip)
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
| Companion pin | BRCE AS-01 seat tip-cargo **ALIVE** (lineage) — EXIT:0 @ `59c090984` (`bc-5a9dbef8`); `as01_seat` sources ≡ tip; no seat-alone land |
| Kill log | dual thrash PID **1065550** SIGTERM |

**Law:** EXIT **0** on stale tip SHA labels **ALIVE (lineage tip-cargo)** only.
Do **not** invent tip-subject `ALIVE` or body `ALIVE`/`CLOSED` from this receipt.
Fence ≠ broker / Plane-5 `ALIVE`. **V8** remains **BLOCKED**.

## Tip-cargo evidence

| Court | Result | Exit | Standing |
|-------|--------|------|----------|
| `r01_r07_resolution_court` | **7 passed; 0 failed** | **0** | **ALIVE (lineage tip-cargo)** |
| `brce_as01_seat_court` (companion pin) | **6 passed; 0 failed** @ `59c090984` | **0** | **ALIVE (lineage tip-cargo; sources ≡ tip)** |

Log/meta: `/tmp/ggen-r01-r07-tip-cargo.log` (`EXIT:0`) · `/tmp/ggen-r01-r07-tip-cargo.meta` (`PIPE_EXIT:0`) · seat exit `/tmp/gate1-exit-brce_as01_seat_court.txt` (`0`).

## Replay

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-r01-r07-tip \
  cargo test -p ggen-graph --test r01_r07_resolution_court \
  --locked --config build.jobs=1 -- --test-threads=1

cargo test -p ggen-abb-sbb --test brce_as01_seat_court --locked
# expect: 6 passed; exit 0 (sources ≡ tip since 59c090984)
```
