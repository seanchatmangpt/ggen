# verify_witness CI-wasm — retip3 push-first honesty (v27.1.1)

```text
tip=3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed
branch=cursor/v2711-vw-ci-wasm-retip3-bc54
size_court_exit=NOT_RUN
affidavit_mount=PRESENT (28673 B; sha 5faae4642ca2…)
production_runtime=UNSUPPORTED
standing=UNSUPPORTED (production) / PARTIAL (env wire held)
crown_absorb=none ManagePullRequest=not_used Foundry=NOT_CLAIMED
```

| Field | Value |
|-------|-------|
| Observed tip | `3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed` |
| Affidavit mount | `/tmp/cursor/verify-witness-prod/affidavit_gatekeeper_verify_witness.wasm` — **28673** B; SHA-256 `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` (pin match) |
| Size+env court | `verify_witness_size_court` — **NOT_RUN** this land (queued behind thrash) |
| Env wire | **PARTIAL** (held from crown `f65434315`) |
| CI default `GGEN_VERIFY_WITNESS_WASM` | **unset** in CI image |
| **Production runtime** | **UNSUPPORTED** — mount PRESENT ≠ consume EXIT 0 in CI |

**Honesty law (held):** size_court_ALIVE ≠ env_wire_PARTIAL ≠ affidavit_pin_ALIVE ≠ production_runtime_ALIVE.

**Authority.** Affidavit bytes mounted locally; production CI consume not closed → production **UNSUPPORTED**. No invented production ALIVE.
