# `crates/praxis-graphlaw/src/shacl/model.rs`

Source SHA-256: `d70a18acc13aa31ea9e095f5c711ba03c964d55d73cbef01e06e55c7e3e89bc2`

```mermaid
classDiagram
    class enum_CostClass {
      <<enum>>
    }
    class struct_CompiledConstraint {
      <<struct>>
      +"cost_class: CostClass"
      +"predicate: usize"
      +"value: usize"
      +"is_optional: bool"
    }
    class struct_PropertyMask {
      <<struct>>
    }
    class enum_TargetType {
      <<enum>>
    }
    class struct_CompiledTarget {
      <<struct>>
      +"target_value: usize"
      +"target_type: TargetType"
    }
    class struct_CompiledShape {
      <<struct>>
      +"iri: usize"
      +"targets: Vec~CompiledTarget~"
      +"constraints: Vec~CompiledConstraint~"
      +"closed: bool"
      +"property_shapes: Vec~CompiledShape~"
      +"allowed_predicates: Vec~usize~"
      +"required_properties_mask: PropertyMask"
    }
    class struct_ShapesGraph {
      <<struct>>
      +"raw_index: Arc~TripleIndex~"
      +"compiled_shapes: Arc~std::collections::HashMap~usize"
    }
    note "ShapesGraph"
```

## Dependencies

- `crate::encoding::Encoder`
- `crate::parser::Syntax`
- `crate::tripleindex::TripleIndex`
- `std::sync::Arc`
- `super::index_utils::get_objects`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
