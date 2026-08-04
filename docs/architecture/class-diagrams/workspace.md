# Workspace and root entrypoints

Each heading is anchored to one source or manifest file. Classes with the same name in different sections are distinct unless an edge explicitly connects them.

## `Cargo.toml`

```mermaid
classDiagram
    class WorkspaceManifest {
      <<module>>
      +package_version 26.8.6
      +resolver 2
      +members[]
      +workspace_dependencies[]
    }
    class RootPackage {
      <<module>>
      +name ggen
      +autobins false
      +library_projection()
    }
    class GenerateRegistryHashes {
      <<boundary>>
      +main()
    }
    WorkspaceManifest *-- RootPackage
    RootPackage o-- GenerateRegistryHashes
    WorkspaceManifest o-- GgenConfigCrate
    WorkspaceManifest o-- GgenMarketplaceCrate
    WorkspaceManifest o-- GgenCliCrate
    WorkspaceManifest o-- GgenGraphCrate
    WorkspaceManifest o-- GgenLspCrate
    WorkspaceManifest o-- GgenEngineCrate
    WorkspaceManifest o-- PraxisCoreCrate
    WorkspaceManifest o-- PraxisGraphlawCrate
    WorkspaceManifest o-- Powl2DecomposeCrate
    WorkspaceManifest o-- GenesisTypesCrate
    WorkspaceManifest o-- GenesisCoreCrate
    WorkspaceManifest o-- CpmpCrate
    WorkspaceManifest o-- GgenMcpCrate
```

## `src/lib.rs`

```mermaid
classDiagram
    class RootLibrary {
      <<module>>
      +public_api()
      +reexports()
    }
    class GgenConfigCrate { <<external>> }
    class GgenGraphCrate { <<external>> }
    class GgenMarketplaceCrate { <<external>> }
    class GgenEngineCrate { <<external>> }
    RootLibrary ..> GgenConfigCrate
    RootLibrary ..> GgenGraphCrate
    RootLibrary ..> GgenMarketplaceCrate
    RootLibrary ..> GgenEngineCrate
```

## `src/main.rs`

```mermaid
classDiagram
    class RetiredRootCliEntrypoint {
      <<module>>
      +main()
    }
    class CanonicalCli {
      <<external>>
      +cli_match()
    }
    RetiredRootCliEntrypoint ..> CanonicalCli : retained on disk; not built
```

## `scripts/generate_registry_hashes.rs`

```mermaid
classDiagram
    class RegistryHashGenerator {
      <<boundary>>
      +main()
      +scan_registry()
      +hash_entries()
      +emit_manifest()
    }
    class Filesystem { <<external>> }
    class HashFunction { <<external>> }
    RegistryHashGenerator --> Filesystem
    RegistryHashGenerator --> HashFunction
```

## `crates/ggen-config/src/lib.rs`

```mermaid
classDiagram
    class ConfigLibrary {
      <<module>>
      +config_lib
      +load()
      +validate()
    }
    class GgenConfig {
      +telemetry
      +project
      +rules
    }
    class TelemetryConfig {
      +endpoint
      +service_name
      +console_output
    }
    ConfigLibrary *-- GgenConfig
    GgenConfig o-- TelemetryConfig
```

## `crates/ggen-marketplace/src/lib.rs`

```mermaid
classDiagram
    class MarketplaceLibrary {
      <<module>>
      +discover()
      +install()
      +verify()
    }
    class PackageDescriptor
    class PackageRegistry { <<boundary>> }
    class PackageReceipt
    MarketplaceLibrary *-- PackageDescriptor
    MarketplaceLibrary --> PackageRegistry
    MarketplaceLibrary --> PackageReceipt
```

## `crates/ggen-graph/src/lib.rs`

```mermaid
classDiagram
    class GraphLibrary {
      <<module>>
      +load()
      +query()
      +serialize()
    }
    class GraphStore
    class Triple
    class Query
    class QueryResult
    GraphLibrary *-- GraphStore
    GraphStore *-- Triple
    GraphLibrary --> Query
    Query --> QueryResult
```

## `crates/ggen-engine/src/lib.rs`

```mermaid
classDiagram
    class EngineLibrary {
      <<module>>
      +sync
      +graph
      +receipt
      +doctor
      +law
    }
    class CommandRegistration
    class AdmissionDecision
    class ConstructionPlan
    class ActuationBoundary { <<boundary>> }
    class Receipt
    EngineLibrary o-- CommandRegistration
    EngineLibrary --> AdmissionDecision
    AdmissionDecision --> ConstructionPlan
    ConstructionPlan --> ActuationBoundary
    ActuationBoundary --> Receipt
```

## `crates/ggen-lsp/src/lib.rs`

```mermaid
classDiagram
    class LspLibrary {
      <<module>>
      +language_server()
      +a2a_mcp
    }
    class LanguageServer { <<boundary>> }
    class DocumentState
    class A2AAdapter
    class McpAdapter
    LspLibrary *-- LanguageServer
    LanguageServer *-- DocumentState
    LspLibrary o-- A2AAdapter
    LspLibrary o-- McpAdapter
```

## `crates/ggen-mcp/src/lib.rs`

```mermaid
classDiagram
    class McpLibrary {
      <<module>>
      +list_tools()
      +call_tool()
      +serve()
    }
    class ToolRegistry
    class ToolRequest
    class ToolResponse
    class Transport { <<boundary>> }
    McpLibrary *-- ToolRegistry
    ToolRegistry --> ToolRequest
    ToolRequest --> ToolResponse
    McpLibrary --> Transport
```

## `crates/genesis-types-v2/src/lib.rs`

```mermaid
classDiagram
    class GenesisTypes {
      <<module>>
      +observation_types
      +artifact_types
      +receipt_types
    }
    class Observation
    class AdmittedObservation
    class Artifact
    class Receipt
    GenesisTypes *-- Observation
    Observation --> AdmittedObservation
    AdmittedObservation --> Artifact
    Artifact --> Receipt
```

## `crates/genesis-core-v2/src/lib.rs`

```mermaid
classDiagram
    class GenesisCore {
      <<module>>
      +admit()
      +manufacture()
      +receipt()
    }
    class Observation { <<external>> }
    class AdmittedObservation { <<external>> }
    class Artifact { <<external>> }
    class Receipt { <<external>> }
    GenesisCore --> Observation
    GenesisCore --> AdmittedObservation
    GenesisCore --> Artifact
    GenesisCore --> Receipt
```

## `crates/praxis-graphlaw/src/lib.rs`

```mermaid
classDiagram
    class GraphlawLibrary {
      <<module>>
      +derive()
      +validate()
      +explain()
    }
    class Rule
    class Fact
    class DerivedFact
    class Refusal
    GraphlawLibrary *-- Rule
    GraphlawLibrary *-- Fact
    Rule --> DerivedFact
    Rule --> Refusal
```

## `crates/powl2-decompose/src/lib.rs`

```mermaid
classDiagram
    class PowlDecomposer {
      <<module>>
      +decompose()
      +order()
    }
    class ProcessModel
    class PartialOrder
    class Activity
    PowlDecomposer --> ProcessModel
    ProcessModel *-- PartialOrder
    PartialOrder *-- Activity
```

## Cross-file standing

The diagrams above describe ownership and dependency topology. Exact fields and methods beyond publicly observed entrypoint behavior remain `INFERRED`; runtime success remains `UNKNOWN` until the corresponding binary, library, or protocol boundary executes against this exact base.
