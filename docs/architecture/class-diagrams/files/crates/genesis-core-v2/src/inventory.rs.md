# `crates/genesis-core-v2/src/inventory.rs`

Source SHA-256: `69d31bccdd0fa1b716a287319d2be590ce3cca1e6d7e7c3f41ef2e1ff421380b`

```mermaid
classDiagram
    class enum_ArtifactStatus {
      <<enum>>
    }
    class struct_ClassifiedArtifact {
      <<struct>>
      +"path: &'static str"
      +"status: ArtifactStatus"
      +"patterns: &'static [&'static str]"
      +"capability_present: &'static str"
      +"finish_gap: &'static str"
      +"finish_action: &'static str"
    }
    class enum_FinishStep {
      <<enum>>
    }
    class enum_ConnectionMechanism {
      <<enum>>
    }
    class mod_tests {
      <<mod>>
    }
    note "ArtifactStatus"
```

## Dependencies

- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
