# `crates/ggen-cli/src/error.rs`

Source SHA-256: `1bfa09ddf411a144d870e552429065e411c7f45fd1b641b479e91eb4240cb49c`

```mermaid
classDiagram
    class enum_GgenError {
      <<enum>>
    }
    class type_Result {
      <<type>>
    }
    class trait_GgenResultExt {
      <<trait>>
      +"to_ggen_result(self) -~ Result~T~"
    }
    class struct_AuditTrail {
      <<struct>>
      +"input_ontology_hash: String"
      +"sparql_query: String"
      +"template_name: String"
      +"output_code: String"
      +"validation_passed: bool"
      +"exit_code: i32"
      +"duration_ms: u64"
      +"validation_errors: Vec~String~"
    }
    class mod_tests {
      <<mod>>
    }
    note "AuditTrail"
    note "From~GgenError~ for clap_noun_verb::NounVerbError"
    note "From~GgenError~ for crate::utils::error::Error"
    note "From~serde_json::error::Error~ for GgenError"
    note "From~std::io::Error~ for GgenError"
    note "GgenError"
    note "GgenResultExt~T~ for std::result::Result~T"
```

## Dependencies

- `super::*`
- `thiserror::Error`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
