# `examples/archive_ggen_core/mcp-a2a-self-hosting/src/lib.rs`

Source SHA-256: `2fb092b974ab31045df1eb98b758a1487bf4932ac309b0a7d93da3f94e5d9e81`

```mermaid
classDiagram
    class struct_AgentInfo {
      <<struct>>
      +"name: String"
      +"version: String"
      +"description: String"
      +"url: String"
      +"provider: String"
      +"system_prompt: Option~String~"
    }
    class fn_agent_info {
      <<fn>>
    }
    class struct_GgenAgentHandler {
      <<struct>>
    }
    class struct_McpHandler {
      <<struct>>
      +"ontology_path: String"
    }
    note "AgentInfo"
    note "AsyncMessageHandler for GgenAgentHandler"
    note "GgenAgentHandler"
    note "McpHandler"
```

## Dependencies

- `a2a_rs::domain::{A2AError, Message, Task}`
- `a2a_rs::port::AsyncMessageHandler`
- `async_trait::async_trait`
- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
