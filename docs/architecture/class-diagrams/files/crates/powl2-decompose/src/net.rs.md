# `crates/powl2-decompose/src/net.rs`

Source SHA-256: `c3da7792f74dabd1bbb61e55b90ae9bea911612ed92468dbdf5b1afb01dfdcff`

```mermaid
classDiagram
    class type_Label {
      <<type>>
    }
    class struct_WfNet {
      <<struct>>
      +"places: BTreeSet~String~"
      +"transitions: BTreeMap~String"
      +"pt: BTreeSet~(String"
      +"tp: BTreeSet~(String"
      +"source: String"
      +"sink: String"
    }
    class enum_NetError {
      <<enum>>
    }
    note "WfNet"
```

## Dependencies

- `std::collections::{BTreeMap, BTreeSet, VecDeque}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
