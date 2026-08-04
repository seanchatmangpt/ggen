# `crates/ggen-engine/tests/economics_measured_evidence_test.rs`

Source SHA-256: `6785e4a2ee61c484afc5027a791b54c3ff2b31d39e108f8dcf7839aa41041f2d`

```mermaid
classDiagram
    class struct_Measurement {
      <<struct>>
      +"elapsed_ms: u128"
      +"exit_success: bool"
    }
    class fn_measure_receipt_chain_e2e {
      <<fn>>
    }
    class fn_economics_receipt_chain_wall_clock_measured_under_slo_threshold {
      <<fn>>
    }
    class fn_economics_measurement_rejects_a_fabricated_zero_duration_reading {
      <<fn>>
    }
```

## Dependencies

- `std::path::Path`
- `std::process::Command`
- `std::sync::OnceLock`
- `std::time::Instant`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
