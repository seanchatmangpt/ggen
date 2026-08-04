# `crates/ggen-cli/src/cmds/a2a.rs`

Source SHA-256: `d7f913798f6d2a7864fae55be3a2ff82b9479e5e71cb9b484aee5b6e51cab75d`

```mermaid
classDiagram
    class fn_get_task_dir {
      <<fn>>
    }
    class struct_A2aTaskOutput {
      <<struct>>
      +"id: String"
      +"state: String"
      +"title: String"
    }
    class fn_create {
      <<fn>>
    }
    class fn_status {
      <<fn>>
    }
    class fn_execute {
      <<fn>>
    }
    class struct_RunInput {
      <<struct>>
      +"tasks: Vec~TaskEntry~"
    }
    class struct_TaskEntry {
      <<struct>>
      +"avatar: Avatar8"
      +"jtbd: Jtbd8"
      +"task_id: String"
    }
    class struct_VerifyOutput {
      <<struct>>
      +"receipt_chain: Vec~A2ATaskReceipt~"
      +"status: String"
    }
    class fn_do_verify {
      <<fn>>
    }
    class fn_verify {
      <<fn>>
    }
    class fn_prune {
      <<fn>>
    }
```

## Dependencies

- `clap_noun_verb::Result`
- `clap_noun_verb_macros::verb`
- `ggen_lsp::a2a_mcp::a2a::receipt::{ AlignmentEvidence, ExpectedPathEvidence, McpInvocationEvidence, ObservedPathEvidence, OcelEvent, OcelObject, OcelObjectRef, ReceiptOcelSlice, }`
- `ggen_lsp::a2a_mcp::a2a::{ A2ARefusalState, A2AState, A2ATaskReceipt, Avatar8, Jtbd8, Task, TaskState, }`
- `serde::{Deserialize, Serialize}`
- `std::fs`
- `std::path::PathBuf`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
