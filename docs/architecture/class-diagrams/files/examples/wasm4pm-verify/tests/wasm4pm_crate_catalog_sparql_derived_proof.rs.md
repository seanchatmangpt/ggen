# `examples/wasm4pm-verify/tests/wasm4pm_crate_catalog_sparql_derived_proof.rs`

Source SHA-256: `80e2966b4e61fdb342a1bc02a5f493d452b611583b310bd5d981919724cc6e8d`

```mermaid
classDiagram
    class mod_wasm4pm_crate_catalog {
      <<mod>>
    }
    class fn_sparql_query_returned_at_least_one_crate {
      <<fn>>
    }
    class fn_crate_count_matches_generated_catalog_right_now {
      <<fn>>
    }
    class fn_catalog_matches_live_ontology_row_for_row {
      <<fn>>
    }
    class fn_live_row_order_is_lexical_ascending_by_label {
      <<fn>>
    }
    class fn_live_ontology_rejects_unknown_crate_name {
      <<fn>>
    }
```

## Dependencies

- `wasm4pm_crate_catalog::{Wasm4pmCrate, WASM4PM_CRATES}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
