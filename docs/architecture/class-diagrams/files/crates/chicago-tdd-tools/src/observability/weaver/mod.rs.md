# `crates/chicago-tdd-tools/src/observability/weaver/mod.rs`

Source SHA-256: `92b67f19af472a3790ae9df0cba46a59813c31650e5bf512fa0f44034dbbee50`

```mermaid
classDiagram
    class mod_poka_yoke {
      <<mod>>
    }
    class mod_types {
      <<mod>>
    }
    class mod_lifecycle {
      <<mod>>
    }
    class enum_WeaverValidationError {
      <<enum>>
    }
    class type_WeaverValidationResult {
      <<type>>
    }
    class struct_WeaverValidator {
      <<struct>>
      +"live_check: Option~WeaverLiveCheck~"
      +"process: Option~Child~"
      +"registry_path: PathBuf"
      +"otlp_grpc_port: u16"
      +"admin_port: u16"
    }
    class fn_send_test_span_to_weaver {
      <<fn>>
    }
    class fn_validate_schema_static {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Drop for WeaverValidator"
    note "WeaverValidator"
```

## Dependencies

- `crate::assert_err`
- `crate::observability::weaver::WeaverValidationError`
- `crate::observability::weaver::types::WeaverLiveCheck`
- `crate::testcontainers::check_docker_available`
- `opentelemetry::KeyValue`
- `opentelemetry::trace::{Span, Tracer, TracerProvider as _}`
- `opentelemetry_sdk::Resource`
- `opentelemetry_sdk::trace::{RandomIdGenerator, Sampler, SdkTracerProvider}`
- `std::marker::PhantomData`
- `std::path::PathBuf`
- `std::path::{Path, PathBuf}`
- `std::process::Child`
- `std::process::Command`
- `std::time::Duration`
- `super::*`
- `thiserror::Error`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
