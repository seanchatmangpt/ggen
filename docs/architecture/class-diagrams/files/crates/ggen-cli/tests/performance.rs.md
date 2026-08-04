# `crates/ggen-cli/tests/performance.rs`

Source SHA-256: `94dbb3f449f05e6ec57fd210a988474fd07f78b6edf906b9093151a5a4b182b3`

```mermaid
classDiagram
    class fn_perf_startup_time_help_command {
      <<fn>>
    }
    class fn_perf_startup_time_version_command {
      <<fn>>
    }
    class fn_perf_startup_time_subcommand_help {
      <<fn>>
    }
    class fn_perf_cold_start_with_config {
      <<fn>>
    }
    class fn_perf_memory_usage_basic_command {
      <<fn>>
    }
    class fn_perf_memory_stress_large_template {
      <<fn>>
    }
    class fn_perf_concurrent_help_commands {
      <<fn>>
    }
    class fn_perf_concurrent_version_commands {
      <<fn>>
    }
    class fn_perf_concurrent_template_generation {
      <<fn>>
    }
    class fn_perf_concurrent_marketplace_searches {
      <<fn>>
    }
    class fn_perf_response_time_marketplace_search {
      <<fn>>
    }
    class fn_perf_response_time_simple_template {
      <<fn>>
    }
    class fn_perf_scalability_many_variables {
      <<fn>>
    }
    class fn_perf_scalability_deep_nesting {
      <<fn>>
    }
    class fn_perf_no_resource_leaks_repeated_commands {
      <<fn>>
    }
    class fn_perf_no_resource_leaks_failed_commands {
      <<fn>>
    }
    class fn_perf_throughput_sequential_generations {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `assert_fs::TempDir`
- `assert_fs::prelude::*`
- `std::process::{Command as StdCommand, Stdio}`
- `std::sync::Arc`
- `std::thread`
- `std::time::{Duration, Instant}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
