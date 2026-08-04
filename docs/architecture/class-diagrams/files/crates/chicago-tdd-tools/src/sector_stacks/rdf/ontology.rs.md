# `crates/chicago-tdd-tools/src/sector_stacks/rdf/ontology.rs`

Source SHA-256: `81f1a658fc29e3647480362bd33290e06dbc0b3e265228f904ae317437904a46`

```mermaid
classDiagram
    class struct_WorkflowStage {
      <<struct>>
      +"id: String"
      +"name: String"
      +"stage_number: u32"
      +"is_deterministic: bool"
      +"max_latency_seconds: u32"
    }
    class struct_GuardConstraint {
      <<struct>>
      +"id: String"
      +"guard_type: String"
      +"constraints: Vec~String~"
    }
    class struct_KnowledgeHook {
      <<struct>>
      +"id: String"
      +"name: String"
      +"description: String"
      +"input_type: String"
      +"output_type: String"
    }
    class struct_SectorOntology {
      <<struct>>
      +"sector: String"
      +"stages: HashMap~String"
      +"guards: HashMap~String"
      +"hooks: HashMap~String"
      +"triples: Vec~(String"
    }
    class mod_tests {
      <<mod>>
    }
    note "SectorOntology"
```

## Dependencies

- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
