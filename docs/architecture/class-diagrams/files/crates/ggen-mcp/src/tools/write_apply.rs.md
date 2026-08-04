# `crates/ggen-mcp/src/tools/write_apply.rs`

Source SHA-256: `681f5ad0ca24e7b4c8cbffa93d98efa5261c9727b7055f7b1a931c4b35b7600c`

```mermaid
classDiagram
    class struct_WriteApplyParams {
      <<struct>>
      +"root: String"
      +"confirm: bool"
    }
    class struct_WrittenFile {
      <<struct>>
      +"path: String"
      +"blake3: Option~String~"
      +"verification_error: Option~String~"
    }
    class struct_WriteApplyResult {
      <<struct>>
      +"ok: bool"
      +"written: Vec~WrittenFile~"
      +"skipped: Vec~SkippedFile~"
      +"write_count: usize"
      +"skip_count: usize"
      +"graph_hash: String"
      +"receipt_path: String"
    }
    class struct_SkippedFile {
      <<struct>>
      +"path: String"
      +"reason: String"
    }
    class fn_write_apply {
      <<fn>>
    }
```

## Dependencies

- `crate::error::{ErrorCategory, McpError}`
- `crate::project_root::resolve_root`
- `ggen_engine::sync::{sync, SyncOptions}`
- `schemars::JsonSchema`
- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
