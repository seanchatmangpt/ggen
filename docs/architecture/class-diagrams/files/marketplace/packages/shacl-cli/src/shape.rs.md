# `marketplace/packages/shacl-cli/src/shape.rs`

Source SHA-256: `988939565fc8b6bace3fabd2498a7e366e4dd6965ba8c5a656b820b5e2f81d17`

```mermaid
classDiagram
    class struct_Shape {
      <<struct>>
      +"name: String"
      +"shape_type: ShapeType"
      +"target: Option~Target~"
    }
    class enum_ShapeType {
      <<enum>>
    }
    class enum_Target {
      <<enum>>
    }
    note "Shape"
```

## Dependencies

- `crate::{Error, Result}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
