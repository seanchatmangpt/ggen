# `crates/ggen-cli/src/cmds/utils.rs`

Source SHA-256: `834eaaa58a3a3f95203f45ebf0278493fb739d9d3173de79d20450e035ab22b2`

```mermaid
classDiagram
    class struct_EnvOutput {
      <<struct>>
      +"variables: HashMap~String"
      +"total: usize"
    }
    class struct_EnvSetOutput {
      <<struct>>
      +"key: String"
      +"value: String"
      +"success: bool"
    }
    class fn_env {
      <<fn>>
    }
    class fn_run_env {
      <<fn>>
    }
    class fn_collect_ggen_env_vars {
      <<fn>>
    }
    class struct_CommandRefEntry {
      <<struct>>
      +"label: String"
      +"description: String"
    }
    class struct_CommandsReferenceOutput {
      <<struct>>
      +"commands: Vec~CommandRefEntry~"
      +"total: usize"
    }
    class fn_commands {
      <<fn>>
    }
```

## Dependencies

- `clap_noun_verb::Result`
- `clap_noun_verb_macros::verb`
- `serde::Serialize`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
