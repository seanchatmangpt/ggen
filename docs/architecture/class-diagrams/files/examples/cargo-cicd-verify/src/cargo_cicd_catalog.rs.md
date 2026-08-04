# `examples/cargo-cicd-verify/src/cargo_cicd_catalog.rs`

Source SHA-256: `48d4383ff07d8c6940b31f64ee0eb6a9d73aefc7ca685df316cac84aef2c8d00`

```mermaid
classDiagram
    class struct_CargoCicdCommand {
      <<struct>>
      +"noun: &'static str"
      +"verb: &'static str"
      +"doc: &'static str"
      +"source_file: &'static str"
      +"arg_count: u8"
      +"args: &'static str"
    }
    class fn_find_command {
      <<fn>>
    }
    class fn_commands_for_noun {
      <<fn>>
    }
    class fn_is_valid_command {
      <<fn>>
    }
    class fn_distinct_nouns {
      <<fn>>
    }
    class fn_parse_args {
      <<fn>>
    }
    class fn_args_are_consistent {
      <<fn>>
    }
    class fn_all_commands_share_return_contract {
      <<fn>>
    }
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
