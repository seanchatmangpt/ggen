# `examples/wasm4pm-verify/tests/wasm4pm_crate_catalog_proof.rs`

Source SHA-256: `b34dba2cbae4427db34fc771d2291ab94d18181027f757b6924c1c69bdcb7d83`

```mermaid
classDiagram
    class mod_wasm4pm_crate_catalog {
      <<mod>>
    }
    class fn_find {
      <<fn>>
    }
    class fn_catalog_has_exactly_nine_crates {
      <<fn>>
    }
    class fn_wasm4pm_matches_ontology {
      <<fn>>
    }
    class fn_tps_metrics_matches_ontology {
      <<fn>>
    }
    class fn_wasm4pm_cognition_matches_ontology {
      <<fn>>
    }
    class fn_ocpq_matches_ontology {
      <<fn>>
    }
    class fn_wasm4pm_planner_matches_ontology {
      <<fn>>
    }
    class fn_bench_tools_matches_ontology {
      <<fn>>
    }
    class fn_catalog_is_sorted_by_name {
      <<fn>>
    }
    class fn_catalog_has_no_duplicate_names {
      <<fn>>
    }
    class fn_every_crate_has_non_empty_name_and_purpose {
      <<fn>>
    }
```

## Dependencies

- `wasm4pm_crate_catalog::{Wasm4pmCrate, WASM4PM_CRATES}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
