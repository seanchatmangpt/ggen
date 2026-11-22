# verify_witness CI-wasm — retip3 honesty (v27.1.1)

```text
tip=f3b0d1a49fbf5570ed47df857025abc1f30175cf
branch=cursor/v2711-vw-ci-wasm-retip3-bc54
size_court=8/8 exit=0 standing=ALIVE (size+env Chicago)
affidavit_mount=PRESENT (28673 B; sha 5faae464…)
production_runtime=UNSUPPORTED
honesty=size_ALIVE_neq_env_PARTIAL_neq_affidavit_ALIVE_neq_production_ALIVE
crown_absorb=none ManagePullRequest=not_used Foundry=NOT_CLAIMED
```

| Field | Value |
|-------|-------|
| Observed tip | `f3b0d1a49fbf5570ed47df857025abc1f30175cf` |
| `verify_witness_size_court` | **8 passed; EXIT 0** @ 2026-11-22T19:03:58Z |
| Affidavit mount | `/tmp/cursor/verify-witness-prod/affidavit_gatekeeper_verify_witness.wasm` — **28673** B; SHA `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| Env wire | **PARTIAL** (held) |
| CI default env | **unset** |
| **Production runtime** | **UNSUPPORTED** — local mount ≠ CI consume EXIT 0 |

**Honesty law (held):** size_court_ALIVE ≠ env_wire_PARTIAL ≠ affidavit_pin_ALIVE ≠ production_runtime_ALIVE. Do **not** invent production ALIVE.

## Exact next for production

Mount real wasm in CI + `export GGEN_VERIFY_WITNESS_WASM=…` + consume EXIT **0**.
