# `marketplace/packages/workflow-engine-cli/src/main.rs`

Source SHA-256: `4200d9adaf6c9f525276ec4e0ed20569b488dcf9993df7a0bfc7c00b7871bcae`

```mermaid
classDiagram
    class struct_Cli {
      <<struct>>
      +"noun: Noun"
    }
    class enum_Noun {
      <<enum>>
    }
    class enum_WorkflowVerb {
      <<enum>>
    }
    class enum_ProcessVerb {
      <<enum>>
    }
    class enum_TaskVerb {
      <<enum>>
    }
    class enum_InstanceVerb {
      <<enum>>
    }
```

## Dependencies

- `anyhow::Result`
- `clap::{Parser, Subcommand}`
- `tracing::{info, Level}`
- `tracing_subscriber::FmtSubscriber`
- `workflow_engine::prelude::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
