# `examples/receiptctl/tests/wasm4pm_facts_pack_full_coverage_proof.rs`

Source SHA-256: `88b208057433ef733e58361a54358fd16bab7769c41fda73a4ea49edbd8dd1f6`

```mermaid
classDiagram
    class mod_wasm4pm_facts_registry {
      <<mod>>
    }
    class fn_registry_path {
      <<fn>>
    }
    class fn_read_registry {
      <<fn>>
    }
    class fn_all_55_breeds_render_verbatim_in_registry {
      <<fn>>
    }
    class fn_breed_standing_is_internally_consistent_across_all_55_rows {
      <<fn>>
    }
    class fn_all_60_algorithms_render_verbatim_in_registry {
      <<fn>>
    }
    class fn_no_duplicate_ids_in_generated_tables {
      <<fn>>
    }
    class fn_lookup_functions_exercise_real_behavior_over_every_row {
      <<fn>>
    }
    class fn_algorithms_by_category_partitions_all_algorithms_exactly {
      <<fn>>
    }
```

## Dependencies

- `std::fs`
- `std::path::PathBuf`
- `wasm4pm_facts_registry::{ALGORITHMS, BREEDS}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
