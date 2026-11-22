# R-01…R-07 contract tip-cargo — ALIVE (exact tip-cargo) receipt (v27.1.1)

```text
cargo_tip=e7569893a
court=r01_r07_resolution_court
passed=7
exit=0
standing=ALIVE (exact tip-cargo)
bodies=BLOCKED
```

| Field | Value |
|-------|-------|
| Document ID | `R01-R07-TIP-CARGO-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §11 / D7 |
| Exact tip-cargo | `e7569893aba3bea6d3273bcf463b651bd94601d3` — **7 passed; exit 0** (`/tmp/ggen-gate1-brce-r01-target`) |
| Cold evidence | `d8eb26e9c` — **7 passed; exit 0** (same harness) |
| Lineage evidence | `cd7c2433b` — prior lineage tip-cargo (`/tmp/ggen-r01-r07-tip`) |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--locked`; `--test-threads=1` |
| Bodies | remain **BLOCKED** — contract fence ≠ body closure |

**Law:** EXIT **0** seals the **contract** fence only.
Do **not** invent body `ALIVE`/`CLOSED` from this receipt.

## Tip-cargo evidence

| Court | Result | Exit | Standing |
|-------|--------|------|----------|
| `r01_r07_resolution_court` | **7 passed; 0 failed** | **0** | **ALIVE** (exact tip-cargo @ `e7569893a`) |

Status: `/tmp/ggen-gate1-brce-r01-logs/r01_r07_tipreconfirm.status`.
Prior lineage log: `/tmp/ggen-r01-r07-tip-cargo.log` (`EXIT:0` @ `cd7c2433b`).

## Replay

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-gate1-brce-r01-target SCCACHE_DISABLE=1 \
  cargo test -p ggen-graph --test r01_r07_resolution_court \
  --locked --config build.jobs=1 -- --test-threads=1
```
