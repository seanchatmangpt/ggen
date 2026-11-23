# Ops surfaces + OOR honesty live reconfirm (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `OPS-SURFACES-LIVE-RECONFIRM-STANDING` |
| Crown tip (quiet) | `99514910c` — Construct `45098816614` **in_progress** |
| Lane tip | `#958` absorb |

## Reconfirmed this lane

| Surface | Command / probe | Result | Standing |
|---------|-----------------|--------|----------|
| `verify_witness_size_court` | `cargo test -p ggen-graph --test verify_witness_size_court` (`/tmp/gate1-tip7-brand`) | **11**/EXIT:**0** | **ALIVE** (size court) — ≠ Foundry |
| zcode POWL/OCEL Chicago (R-06 sealed) | `bun test test/powl-ocel-chicago.test.ts` @ `/agent/repos/zcode-cli` `f32df08` | **11**/EXIT:**0** | **ALIVE** (R-06 held) — ≠ Foundry `--live` |
| cos `/readyz` | `curl https://cos.fnox.io/readyz` | **503** `DB_UNAVAILABLE` | **BLOCKED** (Founder AND-gate) |
| revops.fnox.io | `getent` / `curl` | **NXDOMAIN** / exit **6** | **UNSUPPORTED** (MISSING) |

**Law:** Ops court EXIT 0 ≠ Construct tip ALIVE ≠ Foundry ALIVE ≠ zero-HITL. cos/revops probes do not invent readiness.
