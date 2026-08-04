# `crates/ggen-marketplace/src/ontology_core/core_bundle.rs`

Source SHA-256: `00d75bb79536434228a3b16505ef4d5105cc5f003611791f6448c180b883ced7`

```mermaid
classDiagram
    class struct_OntologyMetadata {
      <<struct>>
      +"name: &'static str"
      +"namespace: &'static str"
      +"size: usize"
      +"content: &'static [u8]"
    }
    class struct_CoreOntologyBundle {
      <<struct>>
    }
    class struct_OntologyStats {
      <<struct>>
      +"count: usize"
      +"total_size_bytes: usize"
    }
    class mod_tests {
      <<mod>>
    }
    note "CoreOntologyBundle"
```

## Dependencies

- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
