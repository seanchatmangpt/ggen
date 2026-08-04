# `examples/receiptctl/tests/algorithms_invocation_dispatch_proof.rs`

Source SHA-256: `50b4fe36fb7d7c7717d1730a7f6a31068534f9448b76a9d4534bf544e9e7c1d6`

```mermaid
classDiagram
    class mod_w4pm_algorithms_invocation_dispatch {
      <<mod>>
    }
    class fn_minimal_ocel {
      <<fn>>
    }
    class fn_composable_set_is_exactly_the_sparql_derived_verified_rows {
      <<fn>>
    }
    class fn_the_one_composable_row_actually_runs_the_real_target_function {
      <<fn>>
    }
    class fn_every_non_composable_catalog_id_fails_closed_not_silently_or_by_panic {
      <<fn>>
    }
```

## Dependencies

- `chrono::DateTime`
- `w4pm_algorithms_invocation_dispatch::{ dispatch_verified, AlgorithmId, DispatchError, CATALOG, COMPOSABLE, }`
- `wasm4pm_compat::ocel::{OCELEvent, OCELRelationship, OCELType, OCEL}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
