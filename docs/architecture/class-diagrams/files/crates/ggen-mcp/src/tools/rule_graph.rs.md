# `crates/ggen-mcp/src/tools/rule_graph.rs`

Source SHA-256: `92f49003fbabf11dfc9598bdf582ec4dfcb69cb5094fcefba9d2dceb2dd0c2a4`

```mermaid
classDiagram
    class struct_RuleGraphParams {
      <<struct>>
      +"root: String"
      +"rule_name: Option~String~"
      +"offset: Option~usize~"
      +"limit: Option~usize~"
    }
    class struct_RuleEdge {
      <<struct>>
      +"rule_id: String"
      +"query_inline: bool"
      +"selected_vars: Vec~String~"
      +"template_path: Option~String~"
      +"template_unresolved: bool"
      +"output_file: String"
      +"issues: Vec~String~"
    }
    class struct_RuleGraphResult {
      <<struct>>
      +"ok: bool"
      +"total_rules: usize"
      +"returned: usize"
      +"offset: usize"
      +"has_more: bool"
      +"next_offset: Option~usize~"
      +"rules: Vec~RuleEdge~"
    }
    class fn_rule_graph {
      <<fn>>
    }
```

## Dependencies

- `crate::error::{ErrorCategory, McpError}`
- `crate::project_root::resolve_root`
- `ggen_lsp::project_index::ProjectIndex`
- `schemars::JsonSchema`
- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
