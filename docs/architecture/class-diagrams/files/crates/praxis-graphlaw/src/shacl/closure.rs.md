# `crates/praxis-graphlaw/src/shacl/closure.rs`

Source SHA-256: `2ae945970024bfa029d7a212914cb912ba83bfc7b292097cc257e82b23a531f5`

```mermaid
classDiagram
    class struct_SubclassClosure {
      <<struct>>
      +"matrix: ClosureMatrix"
      +"global_to_dense: HashMap~usize"
      +"dense_to_global: Vec~usize~"
    }
    class struct_ClosureMatrix {
      <<struct>>
      +"matrix: Vec~FixedBitSet~"
      +"max_id: u32"
    }
    class fn_has_class {
      <<fn>>
    }
    note "ClosureMatrix"
    note "SubclassClosure"
```

## Dependencies

- `crate::tripleindex::TripleIndex`
- `fixedbitset::FixedBitSet`
- `std::collections::HashMap`
- `super::index_utils::get_triples_by_predicate`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
