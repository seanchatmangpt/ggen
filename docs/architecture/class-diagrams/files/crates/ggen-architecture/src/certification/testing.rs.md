# `crates/ggen-architecture/src/certification/testing.rs`

Source SHA-256: `ed46f9fa6fb03ef10f02c97cab0b43cd737e7e9f97b02b14bf58fdc53282f730`

```mermaid
classDiagram
    class enum_TestingSuiteKind {
      <<enum>>
    }
    class enum_TestingSuiteStatus {
      <<enum>>
    }
    class enum_TestingBblockStanding {
      <<enum>>
    }
    class struct_TestingSuite {
      <<struct>>
      +"id: String"
      +"kind: TestingSuiteKind"
      +"status: TestingSuiteStatus"
      +"acceptance: String"
      +"falsifier: String"
    }
    class struct_TestingBblockProtocol {
      <<struct>>
      +"id: String"
      +"version: String"
      +"suites: Vec~TestingSuite~"
    }
    class fn_testing_bblock_protocol {
      <<fn>>
    }
    class enum_TestingBblockRefusal {
      <<enum>>
    }
    note "TestingBblockProtocol"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::BTreeSet`
- `thiserror::Error`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
