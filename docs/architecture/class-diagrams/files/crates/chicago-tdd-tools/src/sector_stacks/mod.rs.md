# `crates/chicago-tdd-tools/src/sector_stacks/mod.rs`

Source SHA-256: `641fc08d4faa4c6747697c23d1b7b28b10d1186ff3370b014420c28297bf1c5f`

```mermaid
classDiagram
    class mod_academic {
      <<mod>>
    }
    class mod_claims {
      <<mod>>
    }
    class mod_rdf {
      <<mod>>
    }
    class struct_OperationReceipt {
      <<struct>>
      +"id: String"
      +"sector: String"
      +"operation: String"
      +"status: OperationStatus"
      +"result: String"
      +"merkle_root: String"
      +"timestamp: String"
    }
    class enum_OperationStatus {
      <<enum>>
    }
    class trait_SectorOperation {
      <<trait>>
      +"sector_name(&self) -~ &'static str"
      +"description(&self) -~ &'static str"
      +"is_deterministic(&self) -~ bool"
      +"generate_receipt(&self, status: OperationStatus) -~ OperationReceipt"
    }
    class mod_tests {
      <<mod>>
    }
    note "fmt::Display for OperationStatus"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::fmt`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
