# `crates/ggen-lsp/tests/mcp_protocol_test.rs`

Source SHA-256: `2606f6e4b2a769d6828eda2cb25228f233baf5c3b1351448f4fe38e30b4f04f1`

```mermaid
classDiagram
    class struct_McpClient {
      <<struct>>
      +"stdin: ChildStdin"
      +"rx: Receiver~Value~"
      +"child: Child"
      +"next_id: i64"
      +"_tmp: TempDir"
    }
    class fn_mcp_handshake_lists_three_tools_with_current_identity {
      <<fn>>
    }
    class fn_mcp_repair_route_returns_e0011_envelope {
      <<fn>>
    }
    class fn_mcp_repair_route_refuses_empty_path_without_panicking {
      <<fn>>
    }
    class fn_tool_doc {
      <<fn>>
    }
    class fn_mcp_replay_case_returns_structured_case_replay {
      <<fn>>
    }
    class fn_mcp_replay_case_without_case_id_verifies_promotion_binding {
      <<fn>>
    }
    class fn_mcp_metrics_returns_improve_metrics_refusing_absent_evidence {
      <<fn>>
    }
    class fn_mcp_every_tool_advertises_a_non_empty_typed_input_schema {
      <<fn>>
    }
    class fn_mcp_initialize_echoes_a_protocol_version_string {
      <<fn>>
    }
    class fn_mcp_malformed_args_are_refused_and_the_server_survives {
      <<fn>>
    }
    class fn_mcp_replay_case_and_metrics_refuse_empty_and_oversized_root {
      <<fn>>
    }
    class fn_mcp_replay_case_and_metrics_accept_valid_root_after_hardening {
      <<fn>>
    }
    class fn_mcp_repair_route_on_clean_law_surface_reports_no_diagnostics {
      <<fn>>
    }
    note "Drop for McpClient"
    note "McpClient"
```

## Dependencies

- `serde_json::{json, Value}`
- `std::io::{BufRead, BufReader, Write}`
- `std::process::{Child, ChildStdin, Command, Stdio}`
- `std::sync::mpsc::{self, Receiver}`
- `std::thread`
- `std::time::Duration`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
