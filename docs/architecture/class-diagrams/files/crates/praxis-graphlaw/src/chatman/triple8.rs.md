# `crates/praxis-graphlaw/src/chatman/triple8.rs`

Source SHA-256: `e202420efde2ea412bb1def72c9e074f1d39f7df16fb4877e98a90d542f54b09`

```mermaid
classDiagram
    class struct_Term8 {
      <<struct>>
    }
    class struct_RDFTriple8 {
      <<struct>>
      +"s: Term8"
      +"p: Term8"
      +"o: Term8"
    }
    class struct_RDFQuad8 {
      <<struct>>
      +"s: Term8"
      +"p: Term8"
      +"o: Term8"
      +"g: Term8"
    }
    class fn_canonical_term {
      <<fn>>
    }
    class struct_ProfileSymbolTable {
      <<struct>>
      +"profile_id: ProfileId"
      +"by_name: BTreeMap~String"
      +"by_id: Vec~String~"
      +"hash: String"
    }
    class mod_tests {
      <<mod>>
    }
    note "ProfileSymbolTable"
    note "fmt::Display for Term8"
```

## Dependencies

- `chicago_tdd_tools::prelude::*`
- `oxrdf::{Literal, NamedNode, Quad, Triple}`
- `oxrdf::{QuadRef, TermRef, TripleRef}`
- `serde::{Deserialize, Serialize}`
- `std::collections::BTreeMap`
- `std::fmt`
- `super::*`
- `super::abi::{GraphSnapshotId, ProfileId, Refusal}`
- `wasm4pm_compat::hash::{blake3_combined, blake3_hex}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
