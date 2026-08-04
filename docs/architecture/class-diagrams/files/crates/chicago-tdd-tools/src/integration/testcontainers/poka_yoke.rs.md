# `crates/chicago-tdd-tools/src/integration/testcontainers/poka_yoke.rs`

Source SHA-256: `dbcd5d8ed819162429c813944becc4771e49fef23ba8d450865caf71b517e226`

```mermaid
classDiagram
    class mod_state {
      <<mod>>
    }
    class struct_Container {
      <<struct>>
      +"image: String"
      +"tag: String"
      +"client: crate::testcontainers::ContainerClient"
      +"running: Option~crate::testcontainers::GenericContainer~"
      +"_state: PhantomData~S~"
    }
    class struct_ValidContainerConfig {
      <<struct>>
      +"image: String"
      +"tag: String"
      +"_command: Option~String~"
      +"_args: Option~Vec~String~~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Container~state::Running~"
    note "Container~state::Stopped~"
    note "ValidContainerConfig"
```

## Dependencies

- `std::marker::PhantomData`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
