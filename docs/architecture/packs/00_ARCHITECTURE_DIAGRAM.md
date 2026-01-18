# Packs System Architecture Diagram

**Version:** 3.2.0
**Last Updated:** 2025-01-15

## High-Level System Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         User Interface Layer                            │
│                                                                           │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐│
│  │ ggen packs   │  │ ggen packs   │  │ ggen packs   │  │ ggen packs   ││
│  │ list         │  │ install      │  │ generate     │  │ validate     ││
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘│
│         │                  │                  │                  │        │
└─────────┼──────────────────┼──────────────────┼──────────────────┼────────┘
          │                  │                  │                  │
┌─────────▼──────────────────▼──────────────────▼──────────────────▼────────┐
│                    CLI Command Handlers (ggen-cli)                         │
│                                                                             │
│  • Parse arguments                                                          │
│  • Validate inputs                                                          │
│  • Format output for user                                                   │
│  • Handle user interaction                                                  │
└─────────┬──────────────────┬──────────────────┬──────────────────┬────────┘
          │                  │                  │                  │
┌─────────▼──────────────────▼──────────────────▼──────────────────▼────────┐
│                   Domain Services (ggen-domain/packs)                      │
│                                                                             │
│  ┌──────────────────────┐  ┌──────────────────────┐  ┌──────────────────┐│
│  │ PackMetadataService  │  │ PackInstallService   │  │ PackGenService   ││
│  ├──────────────────────┤  ├──────────────────────┤  ├──────────────────┤│
│  │ • list_packs()       │  │ • install_pack()     │  │ • generate()     ││
│  │ • load_pack()        │  │ • resolve_deps()     │  │ • render_tmpl()  ││
│  │ • validate_pack()    │  │ • install_packages() │  │ • validate_vars()││
│  └──────────┬───────────┘  └──────────┬───────────┘  └──────────┬───────┘│
│             │                          │                          │        │
│  ┌──────────▼──────────────────────────▼──────────────────────────▼──────┐│
│  │                    DependencyResolver                                  ││
│  ├────────────────────────────────────────────────────────────────────────┤│
│  │ • resolve(pack) -> DependencyGraph                                     ││
│  │ • detect_circular_dependencies()                                       ││
│  │ • topological_sort() -> install_order[]                               ││
│  └────────────────────────────────────────────────────────────────────────┘│
└─────────┬──────────────────┬──────────────────┬──────────────────┬────────┘
          │                  │                  │                  │
          │     ┌────────────▼────────────┐     │                  │
          │     │    Adapter Layer        │     │                  │
          │     │  (Interface to infra)   │     │                  │
          │     └────────────┬────────────┘     │                  │
          │                  │                  │                  │
┌─────────▼──────────────────▼──────────────────▼──────────────────▼────────┐
│                     Integration Adapters                                   │
│                                                                             │
│  ┌────────────────────┐  ┌────────────────────┐  ┌─────────────────────┐ │
│  │ MarketplaceClient  │  │  TemplateEngine    │  │   GraphClient       │ │
│  ├────────────────────┤  ├────────────────────┤  ├─────────────────────┤ │
│  │ • install_package()│  │ • render_file()    │  │ • query_sparql()    │ │
│  │ • package_exists() │  │ • render_string()  │  │ • insert_turtle()   │ │
│  │ • resolve_version()│  │ • generate_tree()  │  │ • load_pack_meta()  │ │
│  └────────┬───────────┘  └────────┬───────────┘  └──────────┬──────────┘ │
└───────────┼──────────────────────┼─────────────────────────┼─────────────┘
            │                      │                         │
┌───────────▼──────────────────────▼─────────────────────────▼─────────────┐
│                    Infrastructure Layer (ggen-core)                        │
│                                                                             │
│  ┌────────────────────┐  ┌────────────────────┐  ┌─────────────────────┐ │
│  │ marketplace::*     │  │ Generator          │  │ Graph               │ │
│  ├────────────────────┤  ├────────────────────┤  ├─────────────────────┤ │
│  │ • execute_install()│  │ • Pipeline         │  │ • query()           │ │
│  │ • execute_search() │  │ • GenContext       │  │ • insert_turtle()   │ │
│  │ • Registry         │  │ • Template         │  │ • SPARQL cache      │ │
│  └────────┬───────────┘  └────────┬───────────┘  └──────────┬──────────┘ │
└───────────┼──────────────────────┼─────────────────────────┼─────────────┘
            │                      │                         │
┌───────────▼──────────────────────▼─────────────────────────▼─────────────┐
│                          External Systems                                  │
│                                                                             │
│  ┌────────────────────┐  ┌────────────────────┐  ┌─────────────────────┐ │
│  │ Marketplace        │  │ Template Files     │  │ RDF Triple Store    │ │
│  │ Registry           │  │ (*.tmpl, *.hbs)    │  │ (in-memory)         │ │
│  ├────────────────────┤  ├────────────────────┤  ├─────────────────────┤ │
│  │ • index.json       │  │ • Tera templates   │  │ • Pack metadata     │ │
│  │ • Package archives │  │ • Handlebars       │  │ • Relationships     │ │
│  │ • Checksums        │  │ • Variables        │  │ • SPARQL queries    │ │
│  └────────────────────┘  └────────────────────┘  └─────────────────────┘ │
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐   │
│  │                     File System                                     │   │
│  ├────────────────────────────────────────────────────────────────────┤   │
│  │ • ~/.ggen/packages/                  (installed packages)          │   │
│  │ • ~/.ggen/cache/                     (download cache)              │   │
│  │ • marketplace/packs/*.toml           (pack manifests)              │   │
│  │ • ggen.lock                          (lockfile)                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Data Flow: Install Pack Workflow

```
User: ggen packs install web-api-starter
  │
  ├──> CLI Handler (ggen-crates/ggen-cli/src/cmds/packs.rs)
  │    └──> Parse args, validate input
  │
  ├──> PackInstallService::install()
  │    │
  │    ├──> 1. Load Pack Manifest
  │    │    ├──> PackMetadataService::load_pack("web-api-starter")
  │    │    │    ├──> Read marketplace/packs/web-api-starter.toml
  │    │    │    └──> Parse TOML -> Pack struct
  │    │    │
  │    │    └──> Pack {
  │    │         id: "web-api-starter",
  │    │         packages: [express-api, graphql-server, ...],
  │    │         templates: [server.ts, schema.ts, ...],
  │    │         dependencies: [base-typescript, devops-basics]
  │    │       }
  │    │
  │    ├──> 2. Resolve Dependencies
  │    │    ├──> DependencyResolver::resolve(&pack)
  │    │    │    │
  │    │    │    ├──> Build dependency graph
  │    │    │    │    ├──> Load base-typescript pack
  │    │    │    │    ├──> Load devops-basics pack
  │    │    │    │    └──> Create nodes + edges
  │    │    │    │
  │    │    │    ├──> Validate acyclic (DFS cycle detection)
  │    │    │    │    └──> ✓ No circular dependencies
  │    │    │    │
  │    │    │    └──> Topological sort (Kahn's algorithm)
  │    │    │         └──> [base-typescript, devops-basics, web-api-starter]
  │    │    │
  │    │    └──> DependencyGraph {
  │    │         nodes: { pack_id -> DependencyNode },
  │    │         edges: [ (parent, child) ]
  │    │       }
  │    │
  │    ├──> 3. Install Packages (in dependency order)
  │    │    │
  │    │    ├──> For pack in install_order:
  │    │    │    │
  │    │    │    ├──> For package in pack.packages:
  │    │    │    │    │
  │    │    │    │    ├──> MarketplaceClient::resolve_version()
  │    │    │    │    │    ├──> Load registry index
  │    │    │    │    │    ├──> Match version constraint (^1.2.0)
  │    │    │    │    │    └──> Return ResolvedPackage {
  │    │    │    │    │         name, version, download_url, checksum
  │    │    │    │    │       }
  │    │    │    │    │
  │    │    │    │    ├──> MarketplaceClient::install_package()
  │    │    │    │    │    │
  │    │    │    │    │    ├──> marketplace::execute_install()
  │    │    │    │    │    │    ├──> Download package archive
  │    │    │    │    │    │    ├──> Verify checksum (SHA256)
  │    │    │    │    │    │    ├──> Extract to ~/.ggen/packages
  │    │    │    │    │    │    └──> Return InstallResult
  │    │    │    │    │    │
  │    │    │    │    │    └──> ✓ express-api@4.18.2 installed
  │    │    │    │    │
  │    │    │    │    └──> [Repeat for all packages]
  │    │    │    │
  │    │    │    └──> ✓ All packages installed
  │    │    │
  │    │    └──> Installed: [
  │    │         typescript@5.0.0,
  │    │         express-api@4.18.2,
  │    │         graphql-server@4.1.0,
  │    │         ... (10 total)
  │    │       ]
  │    │
  │    ├──> 4. Update Lockfile
  │    │    ├──> Load existing ggen.lock (if exists)
  │    │    ├──> Add new entries for installed packages
  │    │    ├──> Atomic write (temp + rename)
  │    │    └──> ✓ Wrote ~/.ggen/packages/ggen.lock
  │    │
  │    └──> Return InstallPackOutput {
  │         pack_id: "web-api-starter",
  │         packages_installed: [10 packages],
  │         total_time: 17.1s
  │       }
  │
  └──> CLI: Display success message + next steps
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

```
Input: web-api-starter pack

Dependency Tree:
web-api-starter@1.2.0
├── base-typescript@1.0.0
│   ├── typescript@5.0.0 (leaf)
│   └── tsconfig-base@3.0.0 (leaf)
│
└── devops-basics@0.8.0
    ├── docker-compose@2.1.0 (leaf)
    ├── ci-github-actions@1.0.0 (leaf)
    └── monitoring-prometheus@1.5.0
        └── metrics-client@2.0.0 (leaf)

Dependency Graph (nodes + edges):
Nodes:
  web-api-starter@1.2.0     [packages: 8]
  base-typescript@1.0.0      [packages: 2]
  devops-basics@0.8.0        [packages: 3]
  typescript@5.0.0           [leaf package]
  tsconfig-base@3.0.0        [leaf package]
  docker-compose@2.1.0       [leaf package]
  ci-github-actions@1.0.0    [leaf package]
  monitoring-prometheus@1.5.0 [packages: 1]
  metrics-client@2.0.0       [leaf package]

Edges (dependencies):
  web-api-starter -> base-typescript
  web-api-starter -> devops-basics
  base-typescript -> typescript
  base-typescript -> tsconfig-base
  devops-basics -> docker-compose
  devops-basics -> ci-github-actions
  devops-basics -> monitoring-prometheus
  monitoring-prometheus -> metrics-client

Topological Sort (install order):
  Level 0 (4): typescript, tsconfig-base, docker-compose, ci-github-actions
  Level 1 (2): metrics-client, monitoring-prometheus
  Level 2 (2): base-typescript, devops-basics
  Level 3 (1): web-api-starter

Install executes packages level-by-level (parallel within level).
```

## Error Handling Flow

```
Scenario: Network failure during package installation

User: ggen packs install web-api-starter
  │
  ├──> PackInstallService::install()
  │    │
  │    ├──> Install package 1/8: express-api ✓
  │    ├──> Install package 2/8: graphql-server ✓
  │    │
  │    ├──> Install package 3/8: auth-jwt
  │    │    │
  │    │    ├──> MarketplaceClient::install_package()
  │    │    │    │
  │    │    │    ├──> Attempt 1: Download from registry
  │    │    │    │    └──> ✗ Network timeout (after 30s)
  │    │    │    │
  │    │    │    ├──> Retry with exponential backoff (2s delay)
  │    │    │    │
  │    │    │    ├──> Attempt 2: Download from registry
  │    │    │    │    └──> ✗ Connection refused
  │    │    │    │
  │    │    │    ├──> Retry with exponential backoff (4s delay)
  │    │    │    │
  │    │    │    ├──> Attempt 3: Download from registry
  │    │    │    │    └──> ✗ Network unreachable
  │    │    │    │
  │    │    │    └──> Return Err(MarketplaceError::DownloadFailed {
  │    │    │         attempts: 3,
  │    │    │         reason: "Network unreachable"
  │    │    │       })
  │    │    │
  │    │    └──> ✗ Installation failed
  │    │
  │    ├──> Trigger Rollback
  │    │    ├──> Remove package 1: express-api
  │    │    │    └──> rm -rf ~/.ggen/packages/express-api
  │    │    │
  │    │    ├──> Remove package 2: graphql-server
  │    │    │    └──> rm -rf ~/.ggen/packages/graphql-server
  │    │    │
  │    │    ├──> Restore lockfile backup
  │    │    │    └──> cp ggen.lock.backup ggen.lock
  │    │    │
  │    │    └──> ✓ Rollback complete
  │    │
  │    └──> Return Err(PacksError::InstallationFailed {
  │         package: "auth-jwt",
  │         reason: "Network unreachable",
  │         attempts: 3
  │       })
  │
  └──> CLI: Display error + suggestions
       │
       ├──> ❌ Installation failed: auth-jwt
       │    └──> Network unreachable after 3 attempts
       │
       ├──> Rollback:
       │    ✓ Removed express-api@4.18.2
       │    ✓ Removed graphql-server@4.1.0
       │    ✓ Restored lockfile
       │
       └──> Suggestions:
            • Check internet connection
            • Retry: ggen packs install web-api-starter --retry
            • Use offline mode: ggen packs install web-api-starter --offline
```

## Caching Strategy

```
┌────────────────────────────────────────────────────────────────┐
│                     Multi-Level Cache                          │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Level 1: In-Memory Cache (PacksCache)                        │
│  ├─────────────────────────────────────────────────────────┐  │
│  │ Pack Manifests:  HashMap<PackId, (Pack, Instant)>       │  │
│  │   TTL: 5 minutes                                         │  │
│  │   Invalidation: On manifest file modification           │  │
│  │   Size: ~10 MB typical                                   │  │
│  │                                                           │  │
│  │ Package Existence: HashMap<String, (bool, Instant)>     │  │
│  │   TTL: 5 minutes                                         │  │
│  │   Size: ~1 MB typical                                    │  │
│  └─────────────────────────────────────────────────────────┘  │
│                                                                 │
│  Level 2: Filesystem Cache (ggen-core CacheManager)           │
│  ├─────────────────────────────────────────────────────────┐  │
│  │ Template Rendered Output:                                │  │
│  │   Location: ~/.ggen/cache/templates/                     │  │
│  │   Key: hash(template_path + variables)                   │  │
│  │   Invalidation: On template file modification            │  │
│  │   Cleanup: LRU, max 500 MB                               │  │
│  │                                                           │  │
│  │ Package Downloads:                                        │  │
│  │   Location: ~/.ggen/cache/downloads/                     │  │
│  │   Key: package_name-version.zip                          │  │
│  │   Integrity: SHA256 checksum verification                │  │
│  │   Cleanup: LRU, max 1 GB                                 │  │
│  └─────────────────────────────────────────────────────────┘  │
│                                                                 │
│  Level 3: Registry Index Cache                                │
│  ├─────────────────────────────────────────────────────────┐  │
│  │ Location: ~/.ggen/cache/registry/index.json             │  │
│  │ TTL: 1 hour                                               │  │
│  │ Update: Background refresh every 30 min                   │  │
│  │ Fallback: Use stale cache if network unavailable         │  │
│  └─────────────────────────────────────────────────────────┘  │
│                                                                 │
└────────────────────────────────────────────────────────────────┘

Cache Hit Flow:
  1. Check in-memory cache (fastest)
  2. If miss, check filesystem cache
  3. If miss, fetch from source + populate caches
  4. Return result

Cache Invalidation:
  - Time-based (TTL expiration)
  - Event-based (file modification)
  - Manual (ggen cache clear)
  - LRU eviction (size limits)
```

---

**Related Documents:**
- [Architecture Overview](01_ARCHITECTURE_OVERVIEW.md)
- [Command Specification](02_COMMAND_SPECIFICATION.md)
- [Data Model](03_DATA_MODEL.md)
- [Integration Layer](04_INTEGRATION_LAYER.md)
- [User Workflows](05_USER_WORKFLOWS.md)
- [Implementation Guide](06_IMPLEMENTATION_GUIDE.md)
