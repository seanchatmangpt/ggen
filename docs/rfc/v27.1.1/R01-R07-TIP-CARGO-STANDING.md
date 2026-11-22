# R-01…R-07 contract tip-cargo — ALIVE (lineage tip-cargo) receipt (v27.1.1)

```text
cargo_tip=cd7c2433b
absorb_tip=b157ef9773d0e85d3b9b540b856c3a87897b7341
court=r01_r07_resolution_court
passed=7
exit=0
standing=ALIVE (lineage tip-cargo; sources ≡ tip)
bodies=BLOCKED
brce_seat=ALIVE (lineage tip-cargo; EXIT:0 @ 59c090984; sources ≡ tip)
landed=cursor/v2711-r01-r07-alive-bc54
```

| Field | Value |
|-------|-------|
| Document ID | `R01-R07-TIP-CARGO-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §11 / D7 |
| Branch | `cursor/v2711-r01-r07-alive-bc54` |
| Cargo evidence tip | `cd7c2433b` — **ancestor of live crown**; harness sources ≡ absorb tip (`git diff` empty on court + `ecosystem_risk`) |
| Absorb tip | `b157ef9773d0e85d3b9b540b856c3a87897b7341` (`origin/cursor/ecosystem-spec-27-1-1-bc54`) |
| Target dir | `/tmp/ggen-r01-r07-tip` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; serialized after ui_layout oxrocksdb |
| Standing (contract fence) | **ALIVE** (lineage tip-cargo; sources ≡ tip) |
| Exact-tip re-run | **not required** for harness identity — sources unchanged through absorb tip |
| Bodies | remain **BLOCKED** — contract fence ≠ body closure |
| Companion pin | BRCE AS-01 seat tip-cargo **ALIVE** (lineage) — EXIT:0 @ `59c090984` (`bc-5a9dbef8`); `as01_seat` sources ≡ tip; no seat-alone land |
| ManagePullRequest | not used (parent owns #849) |

**Law:** EXIT **0** on cargo evidence SHA with harness sources ≡ live tip labels **ALIVE (lineage tip-cargo)**.
Do **not** invent body `ALIVE`/`CLOSED` from this receipt. Fence ≠ broker / Plane-5 `ALIVE`.

## Tip-cargo evidence

| Court | Result | Exit | Standing |
|-------|--------|------|----------|
| `r01_r07_resolution_court` | **7 passed; 0 failed** | **0** | **ALIVE (lineage tip-cargo; sources ≡ tip)** |
| `brce_as01_seat_court` (companion pin) | **6 passed; 0 failed** @ `59c090984` | **0** | **ALIVE (lineage tip-cargo; sources ≡ tip)** |

Log/meta: `/tmp/ggen-r01-r07-tip-cargo.log` (`EXIT:0`) · `/tmp/ggen-r01-r07-tip-cargo.meta` (`PIPE_EXIT:0`) · seat exit `/tmp/gate1-exit-brce_as01_seat_court.txt` (`0`).

## Replay

```bash
set -o pipefail
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-r01-r07-tip CXX=g++ SCCACHE_DISABLE=1 \
  cargo test -p ggen-graph --test r01_r07_resolution_court \
  --locked --config build.jobs=1 -q -- --test-threads=1
# expect: 7 passed; exit 0

cargo test -p ggen-abb-sbb --test brce_as01_seat_court --locked
# expect: 6 passed; exit 0 (sources ≡ tip since 59c090984)
```
