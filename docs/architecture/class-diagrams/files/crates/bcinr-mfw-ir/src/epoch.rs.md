# `crates/bcinr-mfw-ir/src/epoch.rs`

Source SHA-256: `4f4a3417e35760b16b8b6090d12eee01a90ae3509736088e1ec2c7db618f4f12`

```mermaid
classDiagram
    class struct_EpochBounds {
      <<struct>>
      +"max_ground_actions: usize"
      +"max_plan_depth: usize"
      +"max_search_steps: u64"
      +"max_partition_boxes: usize"
    }
    class struct_DescentMeter {
      <<struct>>
      +"budget: usize"
      +"depth: usize"
    }
    class mod_tests {
      <<mod>>
    }
    note "DescentMeter"
```

## Dependencies

- `crate::outcome::{BoundHit, BoundKind}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
