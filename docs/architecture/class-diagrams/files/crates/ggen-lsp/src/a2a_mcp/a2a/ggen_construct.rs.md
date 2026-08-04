# `crates/ggen-lsp/src/a2a_mcp/a2a/ggen_construct.rs`

Source SHA-256: `760802f829345b2d46a5579e0839d1d62dcd25df5bdd6781f2f0f2da5de31e1c`

```mermaid
classDiagram
    class struct_GgenConstructInput {
      <<struct>>
      +"task_id: String"
      +"jtbd: String"
      +"avatar: String"
      +"ontology_uri: String"
      +"target_language: String"
      +"output_format: String"
    }
    class struct_ProofGateResult {
      <<struct>>
      +"compiler: String"
      +"lint: String"
      +"tests: String"
      +"slo: String"
    }
    class struct_GgenConstructResult {
      <<struct>>
      +"artifact_path: String"
      +"artifact_hash: String"
      +"receipt_path: String"
      +"proof_gates: ProofGateResult"
      +"generation_time_ms: u64"
      +"error_details: Option~String~"
    }
    class struct_GgenConstructOutput {
      <<struct>>
      +"status: String"
      +"task_id: String"
      +"jtbd: String"
      +"avatar: String"
      +"message: String"
      +"result: Option~GgenConstructResult~"
    }
    class fn_tool_definition {
      <<fn>>
    }
    class fn_execute {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::time::Instant`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
