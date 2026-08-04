# `crates/ggen-mcp/src/tools/frontmatter_schema.rs`

Source SHA-256: `ca3feb47d4ae143999fb1472c7932e385fb580c8baf4079359e31a0cef7e0778`

```mermaid
classDiagram
    class struct_FrontmatterSchemaParams {
      <<struct>>
      +"key: Option~String~"
    }
    class struct_FrontmatterKey {
      <<struct>>
      +"name: String"
      +"schema: serde_json::Value"
      +"required: bool"
      +"description: Option~String~"
    }
    class struct_FrontmatterSchemaResult {
      <<struct>>
      +"ok: bool"
      +"key_count: usize"
      +"keys: Vec~FrontmatterKey~"
      +"projection_modes: Vec~ProjectionMode~"
    }
    class struct_ProjectionMode {
      <<struct>>
      +"mode: String"
      +"condition: String"
      +"effect: String"
    }
    class fn_projection_modes {
      <<fn>>
    }
    class fn_frontmatter_schema {
      <<fn>>
    }
```

## Dependencies

- `crate::error::McpError`
- `schemars::JsonSchema`
- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
