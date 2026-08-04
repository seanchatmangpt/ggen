# `crates/ggen-cli/tests/marketplace/performance/consolidated_performance.rs`

Source SHA-256: `d9316303ac71259b6be3e7df7dd1d07e4e3d512aac06ed87e16e699abd891251`

```mermaid
classDiagram
    class fn_test_search_performance_100_packages {
      <<fn>>
    }
    class fn_test_maturity_assessment_batch_performance {
      <<fn>>
    }
    class fn_test_dashboard_generation_performance {
      <<fn>>
    }
    class fn_test_filter_performance_multiple_criteria {
      <<fn>>
    }
    class fn_test_score_breakdown_performance {
      <<fn>>
    }
    class fn_test_level_calculation_performance {
      <<fn>>
    }
    class fn_test_csv_export_performance {
      <<fn>>
    }
    class fn_test_json_export_performance {
      <<fn>>
    }
    class fn_test_comparison_performance {
      <<fn>>
    }
    class fn_test_recommendation_performance {
      <<fn>>
    }
    class fn_test_use_case_matching_performance {
      <<fn>>
    }
    class fn_test_memory_efficiency_large_dataset {
      <<fn>>
    }
    class fn_test_repeated_filtering_no_performance_degradation {
      <<fn>>
    }
    class fn_test_feedback_generation_performance {
      <<fn>>
    }
    class fn_test_concurrent_assessment_creation {
      <<fn>>
    }
    class fn_measure_latencies {
      <<fn>>
    }
    class fn_test_lookup_latency_meets_slo {
      <<fn>>
    }
    class fn_test_search_latency_meets_slo {
      <<fn>>
    }
    class fn_test_install_latency_meets_slo {
      <<fn>>
    }
    class fn_test_list_latency_meets_slo {
      <<fn>>
    }
    class fn_test_cold_cache_vs_warm_cache {
      <<fn>>
    }
    class fn_test_concurrent_lookup_latency {
      <<fn>>
    }
    class fn_test_bulk_operation_throughput {
      <<fn>>
    }
    class fn_test_search_with_100_packages {
      <<fn>>
    }
    class fn_test_search_with_1000_packages {
      <<fn>>
    }
    class fn_test_memory_efficiency {
      <<fn>>
    }
    class fn_test_cache_hit_rate_simulation {
      <<fn>>
    }
    class fn_test_sparql_query_performance {
      <<fn>>
    }
    class fn_test_rdf_triple_insertion_performance {
      <<fn>>
    }
    class fn_test_version_comparison_performance {
      <<fn>>
    }
```

## Dependencies

- `ggen_core::marketplace::prelude::*`
- `std::cmp::Ordering`
- `std::sync::Arc`
- `std::thread`
- `std::time::{Duration, Instant}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
