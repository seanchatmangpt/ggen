# `crates/ggen-graph/tests/adversarial_dfg_test.rs`

Source SHA-256: `ce71c83518d2e4c1972fd7eb26e58c916af253e739ea37123817029277627a51`

```mermaid
classDiagram
    class fn_ev {
      <<fn>>
    }
    class fn_distinct_cases_do_not_create_cross_edges {
      <<fn>>
    }
    class fn_single_event_has_no_edges {
      <<fn>>
    }
    class fn_empty_graph_yields_empty_dfg {
      <<fn>>
    }
    class fn_equal_timestamps_produce_no_direct_follow {
      <<fn>>
    }
    class fn_lifecycle_requires_all_three_in_order {
      <<fn>>
    }
```

## Dependencies

- `chrono::{TimeZone, Utc}`
- `ggen_graph::ocel::{EvidenceProjector, OcelEvent, OcelLog, OcelObjectRef}`
- `ggen_graph::{check_lifecycle_order, discover_dfg, DeterministicGraph}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
