# `crates/praxis-graphlaw/src/owlrl/mod.rs`

Source SHA-256: `a1c021de501aa3dc6e59d1eb62f67086bae4129f7d80a74ee57cf61e1cf0eadb`

```mermaid
classDiagram
    class mod_rules {
      <<mod>>
    }
    class struct_OwlRlVocab {
      <<struct>>
      +"rdf_type: usize"
      +"rdfs_subclass_of: usize"
      +"rdfs_subproperty_of: usize"
      +"rdfs_domain: usize"
      +"rdfs_range: usize"
      +"owl_equivalent_class: usize"
      +"owl_equivalent_property: usize"
      +"owl_inverse_of: usize"
      +"owl_symmetric_property: usize"
      +"owl_transitive_property: usize"
      +"owl_same_as: usize"
      +"owl_property_chain_axiom: usize"
      +"owl_cardinality: usize"
      +"owl_min_cardinality: usize"
      +"owl_max_cardinality: usize"
      +"owl_union_of: usize"
      +"owl_intersection_of: usize"
      +"owl_one_of: usize"
      +"owl_imports: usize"
    }
    class enum_OwlRlFeature {
      <<enum>>
    }
    class enum_OwlRlDecision {
      <<enum>>
    }
    class fn_classify_owlrl_feature {
      <<fn>>
    }
    class struct_ScanReport {
      <<struct>>
      +"supported: Vec~(OwlRlFeature"
      +"refused: Vec~(OwlRlFeature"
    }
    class fn_scan_ontology {
      <<fn>>
    }
    class struct_OwlRlEngine {
      <<struct>>
      +"vocab: OwlRlVocab"
    }
    class mod_owlrl_test {
      <<mod>>
    }
    note "OwlRlEngine"
    note "OwlRlVocab"
```

## Dependencies

- `crate::TripleStore`
- `crate::encoding::Encoder`
- `crate::term::Triple`
- `crate::term::VarOrTerm`
- `crate::tripleindex::TripleIndex`
- `rules::*`
- `std::sync::Arc`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
