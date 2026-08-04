# `crates/bcinr-pddl/src/powl_bridge.rs`

Source SHA-256: `e13ec903edea513787cc0f53e57480f5ba05ed031246e648e8c788f7bf5fce5f`

```mermaid
classDiagram
    class struct_PowlOpSpec {
      <<struct>>
      +"kind: PowlOpKind"
      +"label: String"
      +"pred_mask: u64"
      +"succ_mask: u64"
      +"start_time: Option~f64~"
      +"duration: Option~f64~"
    }
    class enum_PowlOpKind {
      <<enum>>
    }
    class fn_temporal_plan_to_powl_tape {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::error::Pddl8Error`
- `super::*`
- `wasm4pm_compat::pddl::TemporalPlan`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
