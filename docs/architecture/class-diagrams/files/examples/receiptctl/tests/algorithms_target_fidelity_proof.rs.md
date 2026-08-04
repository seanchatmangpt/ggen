# `examples/receiptctl/tests/algorithms_target_fidelity_proof.rs`

Source SHA-256: `b34724ad5d71991cec6cf06c2b371e9561aee09343b74bcddf7c1ba52c22167b`

```mermaid
classDiagram
    class mod_w4pm_algorithms_catalog {
      <<mod>>
    }
    class fn_exactly_one_row_is_currently_target_verified_and_it_is_ocel_dfg {
      <<fn>>
    }
    class fn_every_verified_row_wasm_export_resolves_via_by_wasm_export {
      <<fn>>
    }
    class fn_wasm_export_discover_ocel_dfg_actually_resolves_and_runs_against_pinned_wasm4pm_compat {
      <<fn>>
    }
```

## Dependencies

- `chrono::DateTime`
- `w4pm_algorithms_catalog::{by_wasm_export, AlgorithmId}`
- `wasm4pm_compat::ocel::{OCELEvent, OCELRelationship, OCELType, OCEL}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
