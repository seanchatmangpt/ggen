# `examples/star-toml-verify/src/star_toml_config.rs`

Source SHA-256: `1ad933dc4d55c958e75141a3d42c9895a0f344c3e7a499bead42935f1fa8e60d`

```mermaid
classDiagram
    class struct_AdmissionConfig {
      <<struct>>
      +"fail_closed: bool"
      +"witness_dir: std::path::PathBuf"
    }
    class struct_TelemetryConfig {
      <<struct>>
      +"exporter_endpoint: String"
      +"retry_count: Option~u32~"
      +"sample_rate: f64"
    }
    class struct_StarTomlConfig {
      <<struct>>
      +"admission: AdmissionConfig"
      +"telemetry: TelemetryConfig"
    }
    note "AdmissionConfig"
    note "StarTomlConfig"
    note "TelemetryConfig"
```

## Dependencies

- `serde::Deserialize`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
