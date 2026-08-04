# `packs/mfact-pack/reference/mfact-core/src/lib.rs`

Source SHA-256: `63be598eff28035b36269ff84361ab52cc226c22a8ec282f31e9efa683818c29`

```mermaid
classDiagram
    class mod_receipt {
      <<mod>>
    }
    class mod_validate {
      <<mod>>
    }
    class enum_Refusal {
      <<enum>>
    }
    class struct_Artifact {
      <<struct>>
      +"name: String"
      +"hash: String"
      +"axioms: Vec~String~"
      +"proven: bool"
    }
    class struct_Evidence {
      <<struct>>
      +"kind: String"
      +"subject: String"
      +"hash: String"
    }
    class struct_Manifest {
      <<struct>>
      +"schema: String"
      +"release: String"
      +"declaration_source: String"
      +"lean_source_origin: String"
      +"trusted_base: Vec~String~"
      +"llm_trusted_base: bool"
      +"scope: String"
      +"run_identifier: String"
      +"quadrature: String"
      +"artifacts: Vec~Artifact~"
      +"evidence: Vec~Evidence~"
      +"stated_not_proven: Vec~String~"
      +"fold_hash: String"
    }
    class fn_hash_bytes {
      <<fn>>
    }
    class fn_compute_genesis_fold {
      <<fn>>
    }
    class fn_parse_manifest {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `super::*`
- `thiserror::Error`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
