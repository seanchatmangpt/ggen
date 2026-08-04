# `crates/chicago-tdd-tools/src/observability/ocel/types.rs`

Source SHA-256: `e67eadcdfe66a67f1d99574ba49555f4e90c541343a0ffd32571f020af33543f`

```mermaid
classDiagram
    class enum_TestActivity {
      <<enum>>
    }
    class enum_TestObjectType {
      <<enum>>
    }
    class struct_TestOcelEvent {
      <<struct>>
      +"event_id: String"
      +"case_id: RunId"
      +"activity: TestActivity"
      +"timestamp_ns: u64"
      +"objects: Vec~(String"
      +"attributes: HashMap~String"
    }
    class struct_OcelLog {
      <<struct>>
      +"global_log: HashMap~String"
      +"events: HashMap~String"
      +"objects: HashMap~String"
    }
    class struct_TestObject {
      <<struct>>
      +"object_type: TestObjectType"
      +"attributes: HashMap~String"
    }
    note "OcelLog"
```

## Dependencies

- `crate::core::governance::{DiagnosticCategory, RunId, Severity}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
