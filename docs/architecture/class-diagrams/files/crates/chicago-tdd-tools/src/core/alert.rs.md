# `crates/chicago-tdd-tools/src/core/alert.rs`

Source SHA-256: `eeb44934890103beca43084bd7871d990f21e4d5f6f87a5537deae30c04d051d`

```mermaid
classDiagram
    class fn_write_alert {
      <<fn>>
    }
    class struct_AlertLogger {
      <<struct>>
    }
    class mod_logging_tests {
      <<mod>>
    }
    class mod_tests {
      <<mod>>
    }
    note "AlertLogger"
    note "log::Log for AlertLogger"
```

## Dependencies

- `log::Log`
- `std::io::{self, Write}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
