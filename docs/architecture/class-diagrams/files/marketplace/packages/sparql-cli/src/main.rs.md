# `marketplace/packages/sparql-cli/src/main.rs`

Source SHA-256: `51f394834526e3e5aeb9d9cadec99e89d2a6483575bd2ef558d46d64b096fbca`

```mermaid
classDiagram
    class struct_Cli {
      <<struct>>
      +"command: Commands"
    }
    class enum_Commands {
      <<enum>>
    }
    class enum_QueryActions {
      <<enum>>
    }
    class enum_EndpointActions {
      <<enum>>
    }
    class enum_FederationActions {
      <<enum>>
    }
    class enum_OptimizationActions {
      <<enum>>
    }
    class fn_main {
      <<fn>>
    }
    class fn_handle_query {
      <<fn>>
    }
    class fn_handle_endpoint {
      <<fn>>
    }
    class fn_handle_federation {
      <<fn>>
    }
    class fn_handle_optimization {
      <<fn>>
    }
```

## Dependencies

- `anyhow::Result`
- `clap::Parser`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
