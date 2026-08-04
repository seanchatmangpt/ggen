# `crates/praxis-core/src/graphlaw_authority.rs`

Source SHA-256: `a7c998c36ba250b206ed904d66fdb11b353f379fe4de597a0ccfa4d44d274cc2`

```mermaid
classDiagram
    class enum_QuarantineState {
      <<enum>>
    }
    class struct_DialectDeclaration {
      <<struct>>
      +"name: &'static str"
      +"authority: &'static str"
      +"quarantine_state: QuarantineState"
      +"refusal_codes: &'static [&'static str]"
      +"admitted_input_classes: Option~&'static [&'static str]~"
      +"output_classes: Option~&'static [&'static str]~"
      +"receipt_requirements: Option~&'static str~"
      +"replay_surface: Option~&'static str~"
      +"executable_route: Option~&'static str~"
    }
    class fn_authority_for {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
