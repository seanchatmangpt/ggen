# `crates/praxis-graphlaw/src/shacl/equivalence.rs`

Source SHA-256: `c1c59e0ca22e2cc86f1c8b2ceef5400bc15b809d635366a3ac98da6dd7939203`

```mermaid
classDiagram
    class struct_EquivalenceCanonical {
      <<struct>>
      +"class_edges: Vec~(u32"
      +"property_edges: Vec~(u32"
      +"term_edges: Vec~(u32"
    }
    class fn_canonicalize_equivalences {
      <<fn>>
    }
    class fn_collect_all_term_ids {
      <<fn>>
    }
    class fn_render_all_equivalences_canonical {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::tripleindex::TripleIndex`
- `super::*`
- `super::Vocab`
- `super::canonicalization::UnionFind`
- `super::index_utils::get_triples_by_predicate`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
