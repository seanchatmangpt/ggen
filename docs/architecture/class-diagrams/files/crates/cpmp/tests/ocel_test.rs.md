# `crates/cpmp/tests/ocel_test.rs`

Source SHA-256: `17edd47db54dd65a3821ad4999d7b5808f9454d6da121c52d41e283daaac719a`

```mermaid
classDiagram
    class fn_p2p_fixture_path {
      <<fn>>
    }
    class fn_load_p2p {
      <<fn>>
    }
    class fn_from_json_parses_minimal_log {
      <<fn>>
    }
    class fn_from_json_returns_parse_error_on_garbage_input {
      <<fn>>
    }
    class fn_from_json_file_loads_p2p_fixture {
      <<fn>>
    }
    class fn_object_type_names_returns_all_declared_types {
      <<fn>>
    }
    class fn_activity_types_returns_sorted_distinct_activities {
      <<fn>>
    }
    class fn_sorted_events_returns_ascending_timestamp_order {
      <<fn>>
    }
    class fn_sorted_events_first_is_earliest_timestamp {
      <<fn>>
    }
    class fn_events_for_object_returns_only_order1_events_in_temporal_order {
      <<fn>>
    }
    class fn_events_for_object_returns_two_events_for_order2 {
      <<fn>>
    }
    class fn_events_for_nonexistent_object_returns_empty {
      <<fn>>
    }
    class fn_directly_follows_for_order1_has_three_pairs {
      <<fn>>
    }
    class fn_directly_follows_for_item1_has_one_pair {
      <<fn>>
    }
    class fn_directly_follows_graph_has_three_distinct_edges {
      <<fn>>
    }
    class fn_variants_per_object_type_identifies_two_order_variants {
      <<fn>>
    }
    class fn_variants_per_object_type_item_type_has_one_variant {
      <<fn>>
    }
    class fn_stats_counts_are_correct_for_p2p_fixture {
      <<fn>>
    }
    class fn_object_ids_returns_all_related_object_ids {
      <<fn>>
    }
    class fn_qualifiers_for_returns_correct_qualifier {
      <<fn>>
    }
    class fn_qualifiers_for_unknown_object_returns_empty {
      <<fn>>
    }
    class fn_order2_has_one_o2o_relationship_to_order1 {
      <<fn>>
    }
```

## Dependencies

- `cpmp::ocel::{OcelEventLog, OcelReader}`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
