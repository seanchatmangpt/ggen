# CLI source-file class diagrams

These diagrams follow the active CLI entrypoint declared by `crates/ggen-cli/src/lib.rs`. The file force-links `ggen-engine` command registrations, loads optional telemetry configuration, rewrites selected bare nouns to default verbs, and delegates execution to the clap-noun-verb registry.

## `crates/ggen-cli/src/lib.rs`

```mermaid
classDiagram
    class CliLibrary {
      <<module>>
      +cli_match() Result
      +inject_default_verbs(args) Vec~String~
      +run_for_node(args) NodeResult
    }
    class VersionChecker {
      <<external>>
      +check_outdated_binary()
    }
    class GgenConfig {
      <<external>>
      +telemetry
    }
    class TelemetryConfig {
      <<external>>
      +endpoint
      +service_name
      +console_output
    }
    class CommandRegistry {
      <<boundary>>
      +get()
      +run(args)
    }
    class GgenEngineRegistrations {
      <<external>>
      +sync
      +graph
      +receipt
      +doctor
      +law
    }
    class CliError {
      <<external>>
      +new(message)
    }
    CliLibrary --> VersionChecker
    CliLibrary --> GgenConfig
    GgenConfig o-- TelemetryConfig
    CliLibrary --> TelemetryConfig
    CliLibrary --> CommandRegistry
    CliLibrary ..> GgenEngineRegistrations : force-link registrations
    CliLibrary --> CliError
```

## `crates/ggen-cli/src/generated_commands.rs`

```mermaid
classDiagram
    class GeneratedCommands {
      <<generated>>
      +command_reference()
    }
    class CliCommandsOntology {
      <<external>>
      +specify_cli_commands
    }
    class GgenSync {
      <<boundary>>
      +run()
    }
    CliCommandsOntology --> GgenSync : authoritative generation path
    GgenSync --> GeneratedCommands
```

Editing authority: `.specify/cli-commands.ttl` plus the owning query/template and `ggen sync run`; this Rust file is a projection.

## `crates/ggen-cli/src/cmds/mod.rs`

```mermaid
classDiagram
    class CommandModules {
      <<module>>
      +discover_verbs()
    }
    class ClapNounVerbMacro { <<external>> }
    class CommandRegistry { <<external>> }
    CommandModules ..> ClapNounVerbMacro
    ClapNounVerbMacro --> CommandRegistry
```

## `crates/ggen-cli/src/runtime.rs`

```mermaid
classDiagram
    class RuntimeBridge {
      <<boundary>>
      +execute_async()
      +block_on()
    }
    class TokioRuntime { <<external>> }
    class CliResult { <<external>> }
    RuntimeBridge *-- TokioRuntime
    RuntimeBridge --> CliResult
```

## `crates/ggen-cli/src/runtime_helper.rs`

```mermaid
classDiagram
    class RuntimeHelper {
      <<module>>
      +run_command()
      +map_result()
    }
    class RuntimeBridge { <<external>> }
    class CliError { <<external>> }
    RuntimeHelper --> RuntimeBridge
    RuntimeHelper --> CliError
```

## `crates/ggen-cli/src/receipt_manager.rs`

```mermaid
classDiagram
    class ReceiptManager {
      <<boundary>>
      +create_receipt()
      +verify_receipt()
      +persist_receipt()
    }
    class OperationIdentity
    class Consequence
    class Receipt
    class ReceiptStore { <<external>> }
    ReceiptManager *-- OperationIdentity
    ReceiptManager *-- Consequence
    ReceiptManager --> Receipt
    ReceiptManager --> ReceiptStore
```

## `crates/ggen-cli/src/telemetry.rs`

```mermaid
classDiagram
    class TelemetryModule {
      <<boundary>>
      +init_telemetry(config) Guard
    }
    class TelemetryConfig {
      +endpoint
      +service_name
      +console_output
    }
    class TelemetryGuard
    class OtlpExporter { <<external>> }
    TelemetryModule *-- TelemetryConfig
    TelemetryModule --> OtlpExporter
    OtlpExporter --> TelemetryGuard
```

## `crates/ggen-cli/src/version_checker.rs`

```mermaid
classDiagram
    class VersionChecker {
      <<boundary>>
      +check_outdated_binary()
    }
    class CurrentVersion
    class AvailableVersion
    class NetworkClient { <<external>> }
    VersionChecker *-- CurrentVersion
    VersionChecker --> NetworkClient
    NetworkClient --> AvailableVersion
```

## `crates/ggen-cli/src/utils/error.rs`

```mermaid
classDiagram
    class Error {
      +message
      +new(message) Error
    }
    class ResultAlias {
      <<module>>
      +Result~T~
    }
    ResultAlias --> Error
```

## `crates/ggen-cli/src/config_clap.rs`

```mermaid
classDiagram
    class ConfigClap {
      <<module>>
      +manifest_path
      +load()
    }
    class GgenConfig { <<external>> }
    class Filesystem { <<external>> }
    ConfigClap --> Filesystem
    ConfigClap --> GgenConfig
```

## `crates/ggen-cli/src/conventions.rs`

```mermaid
classDiagram
    class Conventions {
      <<module>>
      +resolve_route()
      +resolve_template()
      +resolve_output()
    }
    class Route
    class TemplateIdentity
    class OutputPath
    Conventions --> Route
    Route --> TemplateIdentity
    Route --> OutputPath
```

## `crates/ggen-cli/src/pack_install.rs`

```mermaid
classDiagram
    class PackInstall {
      <<boundary>>
      +resolve()
      +verify()
      +install()
    }
    class PackIdentity
    class MarketplaceClient { <<external>> }
    class InstallReceipt
    PackInstall *-- PackIdentity
    PackInstall --> MarketplaceClient
    PackInstall --> InstallReceipt
```

## `crates/ggen-cli/src/agent.rs`

```mermaid
classDiagram
    class PackAgent {
      <<module>>
      +select_pack()
      +construct_intent()
    }
    class PackIdentity
    class InstallIntent
    class PackInstall { <<external>> }
    PackAgent *-- PackIdentity
    PackAgent --> InstallIntent
    InstallIntent ..> PackInstall : requires authorized actuation
```

## `crates/ggen-cli/src/scaffolding.rs`

```mermaid
classDiagram
    class Scaffolding {
      <<boundary>>
      +plan_project()
      +render_project()
      +write_project()
    }
    class ProjectPlan
    class TemplateRenderer { <<external>> }
    class Filesystem { <<external>> }
    Scaffolding --> ProjectPlan
    ProjectPlan --> TemplateRenderer
    TemplateRenderer --> Filesystem
```

## `crates/ggen-cli/src/validation_lib.rs`

```mermaid
classDiagram
    class ValidationLibrary {
      <<module>>
      +validate_input()
      +validate_config()
      +validate_output()
    }
    class ValidationRule
    class ValidationFailure
    ValidationLibrary *-- ValidationRule
    ValidationRule --> ValidationFailure
```

## `crates/ggen-cli/src/progress.rs`

```mermaid
classDiagram
    class ProgressReporter {
      <<boundary>>
      +start()
      +advance()
      +finish()
    }
    class ProgressSink { <<external>> }
    ProgressReporter --> ProgressSink
```

## `crates/ggen-cli/src/prelude.rs`

```mermaid
classDiagram
    class CliPrelude {
      <<module>>
      +reexport_result()
      +reexport_error()
      +reexport_runtime()
    }
    class ResultAlias { <<external>> }
    class Error { <<external>> }
    class RuntimeHelper { <<external>> }
    CliPrelude ..> ResultAlias
    CliPrelude ..> Error
    CliPrelude ..> RuntimeHelper
```

## End-to-end CLI relation

```mermaid
classDiagram
    class CliLibrary
    class ConfigClap
    class TelemetryModule
    class CommandModules
    class CommandRegistry
    class GgenEngineRegistrations
    class RuntimeBridge
    class ReceiptManager
    CliLibrary --> ConfigClap : parse configuration
    CliLibrary --> TelemetryModule : establish observability
    CliLibrary --> CommandModules : discover local verbs
    CommandModules --> CommandRegistry : register
    GgenEngineRegistrations --> CommandRegistry : distributed registration
    CliLibrary --> CommandRegistry : route admitted argv
    CommandRegistry --> RuntimeBridge : execute selected handler
    RuntimeBridge --> ReceiptManager : receipt consequential operation
```

## Evidence classification

- `OBSERVED`: module declarations, generated-file authority comment, force-link to `ggen-engine`, telemetry loading, registry routing, version handling, and bare-noun rewriting in `crates/ggen-cli/src/lib.rs`.
- `INFERRED`: internal class names and member partitions for files not individually executed or fully parsed.
- `UNKNOWN`: runtime execution, protocol behavior, and receipt persistence against this commit.
