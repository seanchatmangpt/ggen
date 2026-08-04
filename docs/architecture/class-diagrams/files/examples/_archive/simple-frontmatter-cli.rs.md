# `examples/_archive/simple-frontmatter-cli.rs`

Source SHA-256: `ced574c92584ede91ed3581d2d39fa138d3bbf0edfdb4ca1adc87813ec82d2e1`

```mermaid
classDiagram
    class struct_Cli {
      <<struct>>
      +"command: Commands"
    }
    class enum_Commands {
      <<enum>>
    }
    class fn_main {
      <<fn>>
    }
    class fn_generate_frontmatter {
      <<fn>>
    }
    class fn_convert_json_to_yaml {
      <<fn>>
    }
    class fn_show_example {
      <<fn>>
    }
```

## Dependencies

- `clap::{Parser, Subcommand}`
- `serde_json::{json, Value}`
- `std::fs`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
