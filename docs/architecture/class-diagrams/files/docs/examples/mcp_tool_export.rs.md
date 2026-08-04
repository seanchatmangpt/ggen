# `docs/examples/mcp_tool_export.rs`

Source SHA-256: `ec882f8a533dea60241caafda3db7c1f9e81021840777bb51f5a8e0aed9e1a01`

```mermaid
classDiagram
    class struct_Signature {
      <<struct>>
      +"name: String"
      +"description: String"
      +"inputs: Vec~InputField~"
      +"outputs: Vec~OutputField~"
    }
    class struct_InputField {
      <<struct>>
      +"name: String"
      +"type_annotation: String"
      +"description: String"
      +"required: bool"
      +"constraints: HashMap~String"
    }
    class struct_OutputField {
      <<struct>>
      +"name: String"
      +"type_annotation: String"
      +"description: String"
    }
    class struct_MCPExporter {
      <<struct>>
    }
    class fn_main {
      <<fn>>
    }
    note "InputField"
    note "MCPExporter"
    note "OutputField"
    note "Signature"
```

## Dependencies

- `serde_json::{json, Value}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
