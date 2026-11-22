# BRCE AS-01 seat tip-cargo — ALIVE receipt (v27.1.1)

```text
cargo_tip=e7569893a
court=brce_as01_seat_court
passed=6
exit=0
standing=ALIVE (exact tip-cargo)
broker_v8=BLOCKED
plane5=BLOCKED
```

| Field | Value |
|-------|-------|
| Document ID | `BRCE-AS01-SEAT-TIP-CARGO-STANDING` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Plane 5 / AS-01 V1 |
| Cargo evidence tip | `e7569893aba3bea6d3273bcf463b651bd94601d3` — **exact live crown** |
| Prior cold evidence | `d8eb26e9c46bfa02b3f5117bac5f3dfbedd5334d` — **6 passed; exit 0** |
| Target dir | `/tmp/ggen-gate1-brce-r01-target` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--locked`; `--test-threads=1` |
| Branch | `cursor/v2711-gate1-brce-r01-alive-bc54` |
| V8 broker / Plane-5 DO | remain **BLOCKED** — fence ≠ broker `ALIVE` |
| Live WebAuthn | **UNSUPPORTED** |

**Law:** EXIT **0** on the seat fence seals tip-cargo fence `ALIVE` only.
Do **not** invent BRCE **V8**, WebAuthn hardware, or Plane-5 DO `ALIVE` from this receipt.

## Tip-cargo evidence

| Court | Result | Exit | Standing |
|-------|--------|------|----------|
| `brce_as01_seat_court` | **6 passed; 0 failed** | **0** | **ALIVE** (exact tip-cargo) |

Status: `/tmp/ggen-gate1-brce-r01-logs/brce_as01_seat_court_tipreconfirm.status`.

## Replay

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-gate1-brce-r01-target SCCACHE_DISABLE=1 \
  cargo test -p ggen-abb-sbb --test brce_as01_seat_court \
  --locked --config build.jobs=1 -- --test-threads=1
```

Exact next (OOR): BRCE **V8** broker court — intent without AS-01 → `REFUSED_*`;
with verified seat → execution receipt + replay digest → exit **0**
([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5; OOR **G2-3**).
