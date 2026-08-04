# `tests/lean_quality/gemba_walk.rs`

Source SHA-256: `577f20c0c821572a61cbccb4abd1c35d3754fa40e6350c9fdbeda4a3a6c49e08`

```mermaid
classDiagram
    class struct_GembaWalkChecklist {
      <<struct>>
      +"test_file: PathBuf"
      +"checks: Vec~GembaCheck~"
      +"score: f32"
      +"findings: Vec~String~"
    }
    class struct_GembaCheck {
      <<struct>>
      +"name: String"
      +"passed: bool"
      +"observation: String"
      +"weight: f32"
    }
    class struct_GembaWalkInspector {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "GembaWalkChecklist"
    note "GembaWalkInspector"
```

## Dependencies

- `serde::{Serialize, Deserialize}`
- `std::path::PathBuf`
- `std::process::Command`
- `std::time::{Duration, Instant}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
