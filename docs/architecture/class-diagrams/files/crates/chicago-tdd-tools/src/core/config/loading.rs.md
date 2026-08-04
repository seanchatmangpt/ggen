# `crates/chicago-tdd-tools/src/core/config/loading.rs`

Source SHA-256: `acb206df331cca037a93c00b1fd8bc921c08c7c7ce65063d3a05df9e48fc3a54`

```mermaid
classDiagram
    class fn_find_config_file {
      <<fn>>
    }
    class fn_read_config_value {
      <<fn>>
    }
    class fn_read_config_value_usize {
      <<fn>>
    }
    class fn_unit_test_timeout_seconds {
      <<fn>>
    }
    class fn_integration_test_timeout_seconds {
      <<fn>>
    }
    class fn_property_test_cases {
      <<fn>>
    }
    class fn_hot_path_tick_budget {
      <<fn>>
    }
    class fn_max_run_len {
      <<fn>>
    }
    class fn_max_batch_size {
      <<fn>>
    }
    class fn_testcontainers_container_wait_timeout_seconds {
      <<fn>>
    }
    class fn_testcontainers_http_connection_timeout_seconds {
      <<fn>>
    }
    class fn_testcontainers_default_http_port {
      <<fn>>
    }
    class fn_testcontainers_default_https_port {
      <<fn>>
    }
    class fn_testcontainers_default_http_alt_port {
      <<fn>>
    }
    class fn_testcontainers_concurrent_containers_count {
      <<fn>>
    }
    class fn_testcontainers_concurrent_commands_count {
      <<fn>>
    }
    class fn_testcontainers_multi_container_count {
      <<fn>>
    }
    class fn_testcontainers_commands_per_container {
      <<fn>>
    }
    class fn_weaver_otlp_grpc_port {
      <<fn>>
    }
    class fn_weaver_startup_wait_milliseconds {
      <<fn>>
    }
    class fn_weaver_telemetry_processing_wait_milliseconds {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::core::config::poka_yoke::{BoundedTimeout, PositiveU32, PositiveUsize}`
- `std::env`
- `std::fs`
- `std::path::PathBuf`
- `std::sync::{Mutex, OnceLock}`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
