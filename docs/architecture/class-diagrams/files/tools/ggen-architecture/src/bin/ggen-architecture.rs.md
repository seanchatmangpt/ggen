# `tools/ggen-architecture/src/bin/ggen-architecture.rs`

Source SHA-256: `2ee6f198eb71ae82974fd98e7508ccd020ceb1bf2153130f9fc39b86d1ca2a7c`

```mermaid
classDiagram
    class struct_Cli {
      <<struct>>
      +"command: Command"
    }
    class enum_Command {
      <<enum>>
    }
    class enum_SelfPlayCommand {
      <<enum>>
    }
    class enum_Fortune5Command {
      <<enum>>
    }
    class fn_main {
      <<fn>>
    }
    class fn_run {
      <<fn>>
    }
    class fn_print_self_play_report {
      <<fn>>
    }
    class fn_read_json {
      <<fn>>
    }
```

## Dependencies

- `clap::{Parser, Subcommand}`
- `ggen_architecture::{ demo_scenario, run_scenario, verify_report, ArchitectureState, AutonomicController, CapacityEnvelope, DoctorReport, DoctorStatus, Fortune5Assessment, Fortune5AutonomicPlan, Fortune5Catalog, Fortune5Program, LevelFiveCrownAssessment, LevelFiveCrownProgram, SelfPlayDoctorReport, SelfPlayDoctorStatus, SelfPlayReport, SelfPlayScenario, Severity, Stimulus, }`
- `serde::de::DeserializeOwned`
- `std::{ error::Error, fs, path::{Path, PathBuf}, process::ExitCode, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
