# `benches/fortune500_performance.rs`

Source SHA-256: `8b42ff679d1fe2f8d03c41f43037ba4de2d961044cf84e64a5e9efda85213549`

```mermaid
classDiagram
    class fn_bench_cli_startup_times {
      <<fn>>
    }
    class fn_bench_template_rendering_scale {
      <<fn>>
    }
    class fn_bench_rdf_query_performance {
      <<fn>>
    }
    class fn_bench_memory_usage {
      <<fn>>
    }
    class fn_bench_concurrent_operations {
      <<fn>>
    }
    class fn_bench_e2e_workflows {
      <<fn>>
    }
```

## Dependencies

- `criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput}`
- `std::fs`
- `std::hint::black_box`
- `std::path::PathBuf`
- `std::process::Command`
- `std::time::{Duration, Instant}`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
