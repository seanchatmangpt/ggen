# `crates/ggen-marketplace/src/ontology_core/standard_ontologies.rs`

Source SHA-256: `9ed9767d8f12643399e46eac5d45212343156bf8b76b77ee143fdd69096629af`

```mermaid
classDiagram
    class enum_StandardOntology {
      <<enum>>
    }
    class struct_NamespaceInfo {
      <<struct>>
      +"prefix: String"
      +"uri: String"
      +"source: String"
    }
    class fn_get_standard_namespaces {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "StandardOntology"
```

## Dependencies

- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
