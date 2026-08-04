# `crates/ggen-lsp/tests/lsp_contract_completion_test.rs`

Source SHA-256: `ef5db7cd8813d8127ddd939504d01b28426f23ed9a92404d70784b5af4dcb0bd`

```mermaid
classDiagram
    class struct_LspClient {
      <<struct>>
      +"stdin: ChildStdin"
      +"rx: Receiver~Value~"
      +"child: Child"
      +"next_id: i64"
      +"process_root: TempDir"
    }
    class fn_read_frame {
      <<fn>>
    }
    class fn_hierarchy_item {
      <<fn>>
    }
    class fn_write_source_contract_fixture {
      <<fn>>
    }
    class fn_negotiated_hierarchy_source_contract_and_workspace_gate_are_live_over_stdio {
      <<fn>>
    }
    note "Drop for LspClient"
    note "LspClient"
```

## Dependencies

- `serde_json::{json, Value}`
- `std::io::{BufRead, BufReader, Read, Write}`
- `std::process::{Child, ChildStdin, Command, Stdio}`
- `std::sync::mpsc::{self, Receiver}`
- `std::thread`
- `std::time::Duration`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
