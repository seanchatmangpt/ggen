# `tools/ggen-architecture/src/state.rs`

Source SHA-256: `8e4b196874a7f83ee97842f3dafb6899d3d40f3166686574bc7bcf4a69d524ae`

```mermaid
classDiagram
    class struct_AutonomicPolicy {
      <<struct>>
      +"enabled: bool"
      +"max_intents_per_cycle: usize"
      +"direct_actuation_allowed: bool"
    }
    class struct_ArchitectureState {
      <<struct>>
      +"schema_version: u32"
      +"name: String"
      +"registry: ArchitectureRegistry"
      +"capacity_samples: Vec~CapacitySample~"
      +"capacity_policy: CapacityPolicy"
      +"autonomic_policy: AutonomicPolicy"
    }
    note "Default for AutonomicPolicy"
```

## Dependencies

- `crate::{ capacity::{CapacityPolicy, CapacitySample}, registry::ArchitectureRegistry, }`
- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
