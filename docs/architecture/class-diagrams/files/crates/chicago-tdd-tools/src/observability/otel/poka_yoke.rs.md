# `crates/chicago-tdd-tools/src/observability/otel/poka_yoke.rs`

Source SHA-256: `987ed129394486945a966e05f8ad1e2e8953a286e6b1c54a66e1e94f04a43bdf`

```mermaid
classDiagram
    class mod_state {
      <<mod>>
    }
    class enum_ValidAttributeValue {
      <<enum>>
    }
    class struct_ValidAttributeName {
      <<struct>>
      +"name: String"
    }
    class struct_ValidAttribute {
      <<struct>>
      +"name: ValidAttributeName"
      +"value: ValidAttributeValue"
    }
    class struct_Span {
      <<struct>>
      +"name: String"
      +"attributes: Vec~ValidAttribute~"
      +"_state: PhantomData~S~"
      +"end_time: Option~std::time::SystemTime~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Span~state::Active~"
    note "Span~state::Completed~"
    note "ValidAttribute"
    note "ValidAttributeName"
    note "ValidAttributeValue"
```

## Dependencies

- `std::marker::PhantomData`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
