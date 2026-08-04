# `crates/ggen-lsp/src/utils.rs`

Source SHA-256: `169fc795a526caa29e7135a6b5b032698ba45a2c44eb76edda8fe2fb0fd14052`

```mermaid
classDiagram
    class struct_LineColMapper {
      <<struct>>
      +"line_starts: Vec~usize~"
      +"text: String"
    }
    class mod_tests {
      <<mod>>
    }
    note "LineColMapper"
```

## Dependencies

- `lsp_max::lsp_types::Position`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
