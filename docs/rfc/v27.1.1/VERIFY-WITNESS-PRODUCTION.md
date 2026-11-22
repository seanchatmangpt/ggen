# `verify_witness` production path (out of ggen)

Companion to `ECOSYSTEM-SPEC-27.1.1` §10 and Gate-1 gap matrix row
`verify_witness WASM < 250KB`.

## Ownership

| Concern | Owner | Standing claim allowed from ggen |
|---------|-------|----------------------------------|
| Size court (`< 262_144` bytes) | **ggen** (`ggen_graph::witness`) | `PARTIAL_ALIVE` when court green |
| Production WASM build / runtime | affidavit / Truex / witness plane | **not** `ALIVE` from ggen alone |
| Pack emit of artifact bytes | optional ggen pack | `CONSTRUCT` only |

ggen **does not host** the production `verify_witness` runtime. Plane 4 verify
is CONSTRUCT; DO remains behind BRCE+AS-01 on the owning plane.

## In-repo stub (this repository)

| Path | Role |
|------|------|
| `crates/ggen-graph/src/witness/mod.rs` | API: `verify_witness`, `court_verify_witness_size`, typed refusals |
| `crates/ggen-graph/fixtures/witness/verify_witness.placeholder.wasm` | Minimal WASM magic stub for the size court |
| `crates/ggen-graph/tests/verify_witness_size_court.rs` | Chicago size court |

Replay:

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/target-witness \
  cargo test -p ggen-graph --test verify_witness_size_court
```

## Out-of-repo production build

1. **Source of truth** lives in the affidavit / witness plane (sibling checkout
   such as `~/affidavit` or the Truex witness package — not a ggen workspace
   member).
2. **Build** with a wasm32 toolchain, for example:
   ```bash
   rustup target add wasm32-unknown-unknown
   cargo build --release --target wasm32-unknown-unknown -p verify_witness
   # optional: wasm-opt -Oz -o verify_witness.wasm target/wasm32-unknown-unknown/release/*.wasm
   ```
3. **Size gate** before promote:
   ```bash
   wc -c verify_witness.wasm   # must print a value < 262144
   ```
4. **Hand-off to ggen** (optional pack emit): set
   `GGEN_VERIFY_WITNESS_WASM=/abs/path/to/verify_witness.wasm` and call
   `ggen_graph::witness::verify_production_witness(None)` — or pass the path
   explicitly. Over-budget bytes → `WitnessRefusal::OverBudget`.
5. **Missing toolchain / artifact** in a ggen checkout → typed
   `WitnessRefusal::Unsupported` (Gate still encoded via the placeholder court).

## Standing discipline

- Placeholder size court green → `PARTIAL_ALIVE` (gate encoded).
- Production path not mounted / rustup target missing → `UNSUPPORTED`.
- Never promote inspection of this markdown file to `ALIVE`.
