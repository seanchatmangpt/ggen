# `crates/ggen-marketplace/src/packs_registry/types.rs`

Source SHA-256: `c69186f207040a8ce84527ddf8eedc501258b89a615f1fa57b9026fc6ed0f8ba`

```mermaid
classDiagram
    class struct_Pack {
      <<struct>>
      +"id: String"
      +"name: String"
      +"version: String"
      +"description: String"
      +"category: String"
      +"author: Option~String~"
      +"repository: Option~String~"
      +"license: Option~String~"
      +"registry_type: Option~String~"
      +"packages: Vec~String~"
      +"templates: Vec~PackTemplate~"
      +"sparql_queries: HashMap~String"
      +"dependencies: Vec~PackDependency~"
      +"tags: Vec~String~"
      +"keywords: Vec~String~"
      +"production_ready: bool"
      +"metadata: PackMetadata"
    }
    class struct_PackTemplate {
      <<struct>>
      +"name: String"
      +"path: String"
      +"description: String"
      +"variables: Vec~String~"
    }
    class struct_PackDependency {
      <<struct>>
      +"pack_id: String"
      +"version: String"
      +"optional: bool"
    }
    class struct_PackMetadata {
      <<struct>>
      +"test_coverage: Option~String~"
      +"rdf_ontology_size: Option~String~"
      +"sparql_templates: Option~usize~"
      +"code_examples: Option~usize~"
      +"documentation_files: Option~usize~"
    }
    class enum_CompositionStrategy {
      <<enum>>
    }
    class struct_PackFile {
      <<struct>>
      +"pack: Pack"
    }
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
