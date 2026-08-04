# `crates/praxis-graphlaw/src/tripleindex.rs`

Source SHA-256: `9419c27529c71aff900a255de9d6ee3d8d7437951b1492b252a8840eb2f74479`

```mermaid
classDiagram
    class struct_TripleIndex {
      <<struct>>
      +"triples: Vec~Triple~"
      +"spo: FxHashMap~usize"
      +"pos: FxHashMap~usize"
      +"osp: FxHashMap~usize"
      +"counter: usize"
    }
    class struct_EncodedBinding {
      <<struct>>
      +"var: usize"
      +"val: usize"
    }
    class struct_QuadIterator {
      <<struct>>
      +"query: Triple"
      +"index: &'a TripleIndex"
    }
    class struct_TripleIndexSnapshot {
      <<struct>>
      +"inner: Arc~TripleIndex~"
    }
    class enum_QueryResultIter {
      <<enum>>
    }
    class mod_tripleindex_test {
      <<mod>>
    }
    note "Default for TripleIndex"
    note "Iterator for QuadIterator~"
    note "QueryResultIter"
    note "TripleIndex"
    note "TripleIndexSnapshot"
```

## Dependencies

- `crate::fastmap::FxHashMap`
- `crate::{Binding, Term, Triple, VarOrTerm}`
- `std::iter::empty`
- `std::rc::Rc`
- `std::sync::Arc`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
