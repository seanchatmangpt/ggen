# verify_witness size court — live-tip reconfirm (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `VERIFY-WITNESS-LIVE-TIP-RECONFIRM-STANDING` |
| Subject tip | feature ⊇ crown `d6c04b390` (head at pin time) |
| Court | `verify_witness_size_court` |
| Result | **11 passed; EXIT 0** |
| Pin | `affidavit_gatekeeper_verify_witness.pin.wasm` **28673** B |
| Env | `GGEN_VERIFY_WITNESS_WASM` → pin path |
| Standing | **ALIVE (live-tip reconfirm)** — CI-wasm consume lineage **ALIVE** held |
| Foundry / zero-HITL / Plane-4 DO | **NOT** promoted (API remains PartialAlive + ExternalPath CONSTRUCT) |

## Command

```bash
PIN=crates/ggen-graph/fixtures/witness/affidavit_gatekeeper_verify_witness.pin.wasm
export GGEN_VERIFY_WITNESS_WASM="$(pwd)/$PIN"
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/gate1-tip7-brand \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --config build.jobs=1 -- --test-threads=1
# → 11 passed; EXIT 0
```

**Law:** Live-tip reconfirm ≠ Foundry ALIVE ≠ path crown ALIVE.
