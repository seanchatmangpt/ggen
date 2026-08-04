# `crates/ggen-lsp/tests/lsp_protocol_test.rs`

Source SHA-256: `9ae4dcbbcb2aedfc51fc7462f37372a4bc515f3b9dc41b89f20528072fb510a2`

```mermaid
classDiagram
    class struct_LspClient {
      <<struct>>
      +"stdin: ChildStdin"
      +"rx: Receiver~Value~"
      +"child: Child"
      +"next_id: i64"
      +"stashed_notifications: Vec~Value~"
      +"_tmp: TempDir"
    }
    class fn_read_frame {
      <<fn>>
    }
    class fn_initialize {
      <<fn>>
    }
    class fn_lsp_full_lifecycle_over_stdio {
      <<fn>>
    }
    class fn_lsp_each_law_surface_engages_its_analyzer {
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
