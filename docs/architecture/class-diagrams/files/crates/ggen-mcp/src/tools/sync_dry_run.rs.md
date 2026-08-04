# `crates/ggen-mcp/src/tools/sync_dry_run.rs`

Source SHA-256: `19b3cc03bb6505a39fc62ba23c35abeb7cf4ca05c463d3e6e8fb048bddab65ed`

```mermaid
classDiagram
    class struct_SyncDryRunParams {
      <<struct>>
      +"root: String"
    }
    class struct_PlannedWrite {
      <<struct>>
      +"path: String"
      +"decision: String"
    }
    class struct_PlannedSkip {
      <<struct>>
      +"path: String"
      +"reason: String"
      +"raw_reason: String"
    }
    class struct_SyncDryRunResult {
      <<struct>>
      +"ok: bool"
      +"would_write: Vec~PlannedWrite~"
      +"would_skip: Vec~PlannedSkip~"
      +"write_count: usize"
      +"skip_count: usize"
      +"graph_hash: String"
    }
    class fn_classify {
      <<fn>>
    }
    class fn_sync_dry_run {
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
