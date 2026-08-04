# `crates/ggen-lsp/src/a2a_mcp/a2a_generated/agent.rs`

Source SHA-256: `b78bb65293486ed21c30e168e514190d6fc80488672c0d7d6c94fc825b0041b5`

```mermaid
classDiagram
    class struct_Agent {
      <<struct>>
      +"id: String"
      +"name: String"
      +"agent_type: String"
      +"config: serde_json::Value"
    }
    class trait_AgentBehavior {
      <<trait>>
      +"initialize(&mut self, config: &serde_json::Value) -~ Result~(), String~"
      +"execute(
        &mut self, input: serde_json::Value,
    ) -~ impl std::future::Future~Output = Result~serde_json::Value, String~~ + Send"
      +"shutdown(&mut self) -~ Result~(), String~"
    }
    class struct_DefaultAgent {
      <<struct>>
      +"agent: Agent"
      +"initialized: bool"
    }
    class struct_AgentFactory {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "AgentBehavior for DefaultAgent"
    note "AgentFactory"
    note "DefaultAgent"
```

## Dependencies

- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
