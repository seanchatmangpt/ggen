# `boilerplate/crates/mcp-server/src/protocol.rs`

Source SHA-256: `dd752ced7c74c7b91761ed331d7ede55e476e5fe9956173977886151441fd0d0`

```mermaid
classDiagram
    class struct_JsonRpcRequest {
      <<struct>>
      +"id: serde_json::Value"
      +"method: String"
      +"params: Option~serde_json::Value~"
    }
    class struct_JsonRpcResponse {
      <<struct>>
      +"jsonrpc: String"
      +"id: serde_json::Value"
      +"result: Option~serde_json::Value~"
      +"error: Option~JsonRpcError~"
    }
    class struct_JsonRpcError {
      <<struct>>
      +"code: i32"
      +"message: String"
      +"data: Option~serde_json::Value~"
    }
    note "JsonRpcResponse"
```

## Dependencies

- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
