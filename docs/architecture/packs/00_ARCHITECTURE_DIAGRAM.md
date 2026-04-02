<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Packs System Architecture Diagram](#packs-system-architecture-diagram)
  - [High-Level System Architecture](#high-level-system-architecture)
  - [Data Flow: Install Pack Workflow](#data-flow-install-pack-workflow)
  - [Data Flow: Generate Project Workflow](#data-flow-generate-project-workflow)
  - [Dependency Graph Example](#dependency-graph-example)
  - [Error Handling Flow](#error-handling-flow)
  - [Caching Strategy](#caching-strategy)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Packs System Architecture Diagram

**Version:** 3.2.0
**Last Updated:** 2025-01-15

## High-Level System Architecture

```mermaid
flowchart TD
    subgraph UI["User Interface Layer"]
        LIST["ggen packs list"]
        INST["ggen packs install"]
        GEN["ggen packs generate"]
        VAL["ggen packs validate"]
    end

    subgraph CLI["CLI Command Handlers (ggen-cli)"]
        PARSE["Parse arguments<br/>Validate inputs<br/>Format output<br/>Handle interaction"]
    end

    subgraph DOMAIN["Domain Services (ggen-domain/packs)"]
        META["PackMetadataService<br/>list_packs, load_pack<br/>validate_pack"]
        INSTSVC["PackInstallService<br/>install_pack, resolve_deps<br/>install_packages"]
        GENSVC["PackGenService<br/>generate, render_tmpl<br/>validate_vars"]
        DEPRES["DependencyResolver<br/>resolve, detect_circular<br/>topological_sort"]
    end

    subgraph ADAPTER["Integration Adapters"]
        MKT["MarketplaceClient<br/>install_package, exists<br/>resolve_version"]
        TMPL["TemplateEngine<br/>render_file, render_string<br/>generate_tree"]
        GRAPHCLI["GraphClient<br/>query_sparql, insert_turtle<br/>load_pack_meta"]
    end

    subgraph INFRA["Infrastructure Layer (ggen-core)"]
        MKTINF["marketplace::*<br/>execute_install, search<br/>Registry"]
        GENINF["Generator<br/>Pipeline, GenContext<br/>Template"]
        GRINF["Graph<br/>query, insert_turtle<br/>SPARQL cache"]
    end

    subgraph EXT["External Systems"]
        REG["Marketplace Registry<br/>index.json, archives<br/>checksums"]
        TMPLF["Template Files<br/>*.tmpl, *.hbs<br/>Tera, Handlebars"]
        STORE["RDF Triple Store<br/>Pack metadata<br/>relationships"]
        FS["File System<br/>~/.ggen/packages/<br/>~/.ggen/cache/<br/>ggen.lock"]
    end

    LIST --> PARSE
    INST --> PARSE
    GEN --> PARSE
    VAL --> PARSE

    PARSE --> META
    PARSE --> INSTSVC
    PARSE --> GENSVC

    META --> DEPRES
    INSTSVC --> DEPRES
    GENSVC --> DEPRES

    META --> MKT
    INSTSVC --> MKT
    GENSVC --> TMPL
    META --> GRAPHCLI

    MKT --> MKTINF
    TMPL --> GENINF
    GRAPHCLI --> GRINF

    MKTINF --> REG
    GENINF --> TMPLF
    GRINF --> STORE
    MKTINF --> FS
    GENINF --> FS

    style UI fill:#e1f5ff
    style CLI fill:#e1f5ff
    style DOMAIN fill:#fff4e6
    style ADAPTER fill:#c8e6c9
    style INFRA fill:#fce4ec
    style EXT fill:#f5f5f5
```

## Data Flow: Install Pack Workflow

```mermaid
sequenceDiagram
    title ggen packs install web-api-starter
    participant User as User
    participant CLI as CLI Handler
    participant InstSvc as PackInstallService
    participant MetaSvc as PackMetadataService
    participant DepRes as DependencyResolver
    participant MkClient as MarketplaceClient
    participant MkInfra as marketplace::*
    participant FS as File System

    User->>CLI: ggen packs install web-api-starter
    CLI->>CLI: Parse args, validate input

    CLI->>InstSvc: install()

    Note over InstSvc: 1. Load Pack Manifest
    InstSvc->>MetaSvc: load_pack("web-api-starter")
    MetaSvc->>FS: Read packs/web-api-starter.toml
    FS-->>MetaSvc: TOML content
    MetaSvc-->>InstSvc: Pack struct

    Note over InstSvc: 2. Resolve Dependencies
    InstSvc->>DepRes: resolve(&pack)
    DepRes->>DepRes: Build dependency graph
    DepRes->>DepRes: DFS cycle detection
    DepRes->>DepRes: Topological sort
    DepRes-->>InstSvc: install_order[]

    Note over InstSvc: 3. Install Packages (in order)
    loop For each package in install_order
        InstSvc->>MkClient: resolve_version()
        MkClient->>MkClient: Match constraint (^1.2.0)
        MkClient-->>InstSvc: ResolvedPackage

        InstSvc->>MkClient: install_package()
        MkClient->>MkInfra: execute_install()
        MkInfra->>MkInfra: Download archive
        MkInfra->>MkInfra: Verify SHA256 checksum
        MkInfra->>FS: Extract to ~/.ggen/packages/
        MkInfra-->>MkClient: InstallResult
    end

    Note over InstSvc: 4. Update Lockfile
    InstSvc->>FS: Atomic write ggen.lock

    InstSvc-->>CLI: InstallPackOutput
    CLI-->>User: Success message + next steps
```

## Data Flow: Generate Project Workflow

```
User: ggen packs generate web-api-starter my-api --var author="Jane"
  │
  ├──> CLI Handler (ggen-crates/ggen-cli/src/cmds/packs.rs)
  │    └──> Parse args, validate input
  │
  ├──> PackGenerationService::generate()
  │    │
  │    ├──> 1. Load Pack Manifest
  │    │    └──> PackMetadataService::load_pack("web-api-starter")
  │    │         └──> Pack { templates: [...] }
  │    │
  │    ├──> 2. Resolve Variables
  │    │    ├──> User-provided: { author: "Jane" }
  │    │    ├──> Auto-detected: { project_name: "my-api", timestamp: "2025-01-15..." }
  │    │    ├──> Template defaults: { license: "MIT", port: "3000" }
  │    │    │
  │    │    └──> variables = {
  │    │         project_name: "my-api",
  │    │         author: "Jane",
  │    │         timestamp: "2025-01-15T10:30:00Z",
  │    │         license: "MIT",
  │    │         port: "3000",
  │    │         database: "postgres",
  │    │         enable_graphql: "true"
  │    │       }
  │    │
  │    ├──> 3. Validate Variables
  │    │    ├──> For each template:
  │    │    │    ├──> Check required variables present
  │    │    │    ├──> Validate types (string, int, bool)
  │    │    │    ├──> Validate patterns (regex)
  │    │    │    └──> Validate constraints (ranges, enums)
  │    │    │
  │    │    └──> ✓ All variables valid
  │    │
  │    ├──> 4. Render Templates
  │    │    │
  │    │    ├──> For template in pack.templates:
  │    │    │    │
  │    │    │    ├──> TemplateEngine::render_file()
  │    │    │    │    │
  │    │    │    │    ├──> Load template file (src/server.ts.tmpl)
  │    │    │    │    │
  │    │    │    │    ├──> Generator::generate()
  │    │    │    │    │    ├──> Parse frontmatter (YAML)
  │    │    │    │    │    ├──> Apply Tera filters ({{ name | upper }})
  │    │    │    │    │    ├──> Substitute variables
  │    │    │    │    │    └──> Return rendered content
  │    │    │    │    │
  │    │    │    │    └──> Rendered: "const PORT = 3000;\n..."
  │    │    │    │
  │    │    │    ├──> Render output path (may have variables)
  │    │    │    │    └──> "src/{{ project_name }}/server.ts"
  │    │    │    │         -> "src/my-api/server.ts"
  │    │    │    │
  │    │    │    ├──> Create output directory
  │    │    │    │    └──> mkdir -p ./my-api/src/my-api
  │    │    │    │
  │    │    │    ├──> Write file
  │    │    │    │    └──> ./my-api/src/my-api/server.ts (324 bytes)
  │    │    │    │
  │    │    │    ├──> Set permissions (if specified)
  │    │    │    │    └──> chmod 0644 server.ts
  │    │    │    │
  │    │    │    └──> ✓ server.ts rendered
  │    │    │
  │    │    └──> [Repeat for all 12 templates]
  │    │         └──> ✓ 12 files created (11.2 KB total)
  │    │
  │    ├──> 5. Run Post-Generation Hooks (optional)
  │    │    ├──> Execute pack.hooks.post_generate[]
  │    │    │    └──> "npm install" (if specified)
  │    │    │
  │    │    └──> ✓ Hooks completed
  │    │
  │    └──> Return GenerateOutput {
  │         pack_id: "web-api-starter",
  │         project_name: "my-api",
  │         files_created: 12,
  │         output_path: "./my-api",
  │         generation_time: 2.3s
  │       }
  │
  └──> CLI: Display success message + next steps
```

## Dependency Graph Example

```mermaid
flowchart TD
    ROOT["web-api-starter@1.2.0<br/><i>8 packages</i>"]
    BTS["base-typescript@1.0.0<br/><i>2 packages</i>"]
    DEV["devops-basics@0.8.0<br/><i>3 packages</i>"]
    TS["typescript@5.0.0<br/><i>leaf</i>"]
    TSC["tsconfig-base@3.0.0<br/><i>leaf</i>"]
    DC["docker-compose@2.1.0<br/><i>leaf</i>"]
    CI["ci-github-actions@1.0.0<br/><i>leaf</i>"]
    PROM["monitoring-prometheus@1.5.0<br/><i>1 package</i>"]
    MC["metrics-client@2.0.0<br/><i>leaf</i>"]

    ROOT --> BTS
    ROOT --> DEV
    BTS --> TS
    BTS --> TSC
    DEV --> DC
    DEV --> CI
    DEV --> PROM
    PROM --> MC

    style ROOT fill:#e1f5ff
    style BTS fill:#fff4e6
    style DEV fill:#fff4e6
    style TS fill:#c8e6c9
    style TSC fill:#c8e6c9
    style DC fill:#c8e6c9
    style CI fill:#c8e6c9
    style PROM fill:#fff4e6
    style MC fill:#c8e6c9
```

**Topological Sort (install order):**
| Level | Packages | Parallel |
|-------|----------|----------|
| 0 | typescript, tsconfig-base, docker-compose, ci-github-actions | 4 parallel |
| 1 | metrics-client, monitoring-prometheus | 2 parallel |
| 2 | base-typescript, devops-basics | 2 parallel |
| 3 | web-api-starter | 1 (final) |

## Error Handling Flow

```mermaid
sequenceDiagram
    title Error Handling: Network Failure During Install
    participant User as User
    participant CLI as CLI
    participant InstSvc as PackInstallService
    participant MkClient as MarketplaceClient
    participant Registry as Package Registry
    participant FS as File System

    User->>CLI: ggen packs install web-api-starter
    CLI->>InstSvc: install()

    Note over InstSvc: Packages 1-2 install OK
    InstSvc->>MkClient: install_package() [3/8: auth-jwt]

    MkClient->>Registry: Attempt 1: Download
    Registry--xMkClient: Network timeout (30s)

    Note over MkClient: Exponential backoff (2s)
    MkClient->>Registry: Attempt 2: Download
    Registry--xMkClient: Connection refused

    Note over MkClient: Exponential backoff (4s)
    MkClient->>Registry: Attempt 3: Download
    Registry--xMkClient: Network unreachable

    MkClient-->>InstSvc: Err(DownloadFailed, 3 attempts)

    Note over InstSvc: Trigger Rollback
    InstSvc->>FS: Remove express-api
    InstSvc->>FS: Remove graphql-server
    InstSvc->>FS: Restore ggen.lock backup

    InstSvc-->>CLI: Err(InstallationFailed)
    CLI-->>User: Error + rollback summary + suggestions
```

## Caching Strategy

```mermaid
flowchart TD
    subgraph L1["Level 1: In-Memory Cache (PacksCache)"]
        MANIFEST["Pack Manifests<br/>HashMap&lt;PackId, (Pack, Instant)&gt;<br/>TTL: 5 min | Size: ~10 MB"]
        EXIST["Package Existence<br/>HashMap&lt;String, (bool, Instant)&gt;<br/>TTL: 5 min | Size: ~1 MB"]
    end

    subgraph L2["Level 2: Filesystem Cache (CacheManager)"]
        TMPL["Template Rendered Output<br/>~/.ggen/cache/templates/<br/>Key: hash(path + vars)<br/>LRU max 500 MB"]
        DL["Package Downloads<br/>~/.ggen/cache/downloads/<br/>Key: name-version.zip<br/>LRU max 1 GB"]
    end

    subgraph L3["Level 3: Registry Index Cache"]
        REGCACHE["index.json<br/>~/.ggen/cache/registry/<br/>TTL: 1 hour<br/>BG refresh: 30 min"]
    end

    subgraph INV["Cache Invalidation"]
        TTL["Time-based (TTL)"]
        EVENT["Event-based (file mod)"]
        MANUAL["Manual (ggen cache clear)"]
        LRU["LRU eviction"]
    end

    L1 -->|miss| L2
    L2 -->|miss| L3
    L3 -->|miss| FETCH["Fetch from source"]

    style L1 fill:#c8e6c9
    style L2 fill:#fff4e6
    style L3 fill:#e1f5ff
    style INV fill:#fce4ec
```

**Cache Hit Flow:**
1. Check in-memory cache (fastest)
2. If miss, check filesystem cache
3. If miss, fetch from source + populate caches
4. Return result

---

**Related Documents:**
- [Architecture Overview](01_ARCHITECTURE_OVERVIEW.md)
- [Command Specification](02_COMMAND_SPECIFICATION.md)
- [Data Model](03_DATA_MODEL.md)
- [Integration Layer](04_INTEGRATION_LAYER.md)
- [User Workflows](05_USER_WORKFLOWS.md)
- [Implementation Guide](06_IMPLEMENTATION_GUIDE.md)
