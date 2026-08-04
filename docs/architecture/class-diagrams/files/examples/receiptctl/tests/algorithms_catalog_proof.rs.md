# `examples/receiptctl/tests/algorithms_catalog_proof.rs`

Source SHA-256: `2d80d88587305d2dda9a27354200aa0b65a4d0cfd21eaf46ef52a1a12e630712`

```mermaid
classDiagram
    class mod_w4pm_algorithms_catalog {
      <<mod>>
    }
    class fn_find {
      <<fn>>
    }
    class fn_catalog_len_matches_sparql_derived_total {
      <<fn>>
    }
    class fn_category_counts_match_sparql_group_by_aggregate {
      <<fn>>
    }
    class fn_wasm_export_values_are_unique_across_the_catalog {
      <<fn>>
    }
    class fn_algorithm_id_values_are_unique_across_the_catalog {
      <<fn>>
    }
    class fn_generated_full_field_assertions_over_every_catalog_row {
      <<fn>>
    }
    class fn_predict_remaining_time_wasm_export_does_not_share_the_algorithm_id_prefix {
      <<fn>>
    }
    class fn_by_wasm_export_dispatches_discover_dfg_to_dfg {
      <<fn>>
    }
    class fn_by_wasm_export_dispatches_compute_alignments_to_alignments {
      <<fn>>
    }
    class fn_by_wasm_export_dispatches_run_agentic_pipeline_to_agentic_pipeline {
      <<fn>>
    }
    class fn_by_wasm_export_dispatches_the_mismatched_predict_case_duration_export {
      <<fn>>
    }
    class fn_by_wasm_export_rejects_empty_string {
      <<fn>>
    }
    class fn_by_wasm_export_rejects_near_miss_prefix_match {
      <<fn>>
    }
    class fn_by_wasm_export_rejects_the_algorithm_id_itself {
      <<fn>>
    }
    class fn_by_wasm_export_rejects_an_unrelated_string {
      <<fn>>
    }
    class fn_algorithm_id_as_str_literal_checks {
      <<fn>>
    }
    class fn_every_row_id_as_str_round_trips_to_its_own_algorithm_id_field {
      <<fn>>
    }
```

## Dependencies

- `std::collections::HashSet`
- `w4pm_algorithms_catalog::{by_wasm_export, AlgorithmId, AlgorithmInfo, CATALOG}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
