# `crates/chicago-tdd-tools/src/core/contract.rs`

Source SHA-256: `13855be484663e02aeec6fc06667447958a60e192f309590005737b34c959446`

```mermaid
classDiagram
    class struct_ResourceEnvelope {
      <<struct>>
      +"max_ticks: u64"
      +"max_memory_bytes: u64"
      +"requires_network: bool"
      +"requires_storage: bool"
      +"requires_privileged: bool"
    }
    class enum_TestThermalClass {
      <<enum>>
    }
    class struct_TestContract {
      <<struct>>
      +"name: &'static str"
      +"coverage: &'static [&'static str]"
      +"invariants: &'static [&'static str]"
      +"resources: ResourceEnvelope"
      +"environment: &'static [&'static str]"
    }
    class struct_TestContractRegistry {
      <<struct>>
      +"contracts: &'static [TestContract]"
    }
    class mod_tests {
      <<mod>>
    }
    note "ResourceEnvelope"
    note "TestContract"
    note "TestContractRegistry"
    note "fmt::Display for TestThermalClass"
```

## Dependencies

- `core::fmt`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
