# `crates/ggen-cli/src/telemetry.rs`

Source SHA-256: `8986af896e524f068b7ac83d0c956989e82b5a847abec358ec53b4ab98d706ce`

```mermaid
classDiagram
    class struct_TelemetryConfig {
      <<struct>>
      +"endpoint: String"
      +"service_name: String"
      +"console_output: bool"
    }
    class struct_TelemetryGuard {
      <<struct>>
      +"provider: SdkTracerProvider"
    }
    class fn_init_telemetry {
      <<fn>>
    }
    class fn_shutdown_telemetry {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for TelemetryConfig"
    note "Drop for TelemetryGuard"
```

## Dependencies

- `crate::utils::error::{Error, Result}`
- `opentelemetry::{global, KeyValue}`
- `opentelemetry_otlp::WithExportConfig`
- `opentelemetry_sdk::Resource`
- `opentelemetry_sdk::trace::SdkTracerProvider`
- `super::*`
- `tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Registry}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
