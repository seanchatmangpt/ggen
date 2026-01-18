# Packs System Architecture

## Executive Summary

The **Packs System** enables users to compose and execute full-stack projects by combining multiple marketplace templates, SPARQL queries, and validation rules into reusable, versioned bundles. Packs sit as a **composition layer** above the marketplace, orchestrating template generation, RDF/SPARQL querying, and dependency resolution.

**Key Value Proposition**: Users can generate entire applications (e.g., "startup + devops + monitoring") with a single command, rather than manually assembling individual templates.

## System Context (C4 Level 1)

```
┌─────────────────────────────────────────────────────────────────┐
│                         Packs System                            │
│                                                                 │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐    │
│  │   Pack CLI   │───▶│ Pack Engine  │───▶│  Pack Store  │    │
│  │ (ggen pack)  │    │ (Composer)   │    │  (Registry)  │    │
│  └──────────────┘    └──────────────┘    └──────────────┘    │
│         │                     │                    │            │
│         │                     │                    │            │
│         ▼                     ▼                    ▼            │
│  ┌──────────────────────────────────────────────────────┐     │
│  │           Marketplace Infrastructure                  │     │
│  ├───────────────────┬────────────────┬─────────────────┤     │
│  │  Template Engine  │  SPARQL Store  │  Validator      │     │
│  │  (ggen-core)      │  (RDF/TTL)     │  (Maturity)     │     │
│  └───────────────────┴────────────────┴─────────────────┘     │
└─────────────────────────────────────────────────────────────────┘
            │                    │                   │
            ▼                    ▼                   ▼
    ┌──────────────┐    ┌──────────────┐   ┌──────────────┐
    │  Templates/  │    │  RDF Graph   │   │  File System │
    │  Marketplace │    │  Store       │   │  Output      │
    └──────────────┘    └──────────────┘   └──────────────┘
```

**External Systems:**
- **Marketplace**: Source of templates, SPARQL queries, and package metadata
- **Template Engine (ggen-core)**: Renders templates with variable substitution
- **RDF/SPARQL**: Semantic queries and ontology-driven generation
- **Validator**: Maturity scoring and compatibility checks

**Users:**
- **Developers**: Generate full projects from packs
- **Architects**: Create custom packs for org-specific patterns
- **Package Authors**: Publish reusable packs to registry

## Container Diagram (C4 Level 2)

```
┌─────────────────────────────────────────────────────────────────┐
│                      Pack System Containers                      │
│                                                                  │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │                      Pack CLI Layer                        │ │
│  │  ggen pack list | show | install | generate | compose     │ │
│  │  ggen pack validate | benchmark | publish                 │ │
│  └────────────────────────────────────────────────────────────┘ │
│                              │                                   │
│                              ▼                                   │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │                    Pack Domain Layer                       │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐   │ │
│  │  │ Pack Service │  │  Composer    │  │  Validator   │   │ │
│  │  │ (CRUD ops)   │  │ (Multi-pack) │  │ (Compat)     │   │ │
│  │  └──────────────┘  └──────────────┘  └──────────────┘   │ │
│  └────────────────────────────────────────────────────────────┘ │
│                              │                                   │
│                              ▼                                   │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │                   Pack Storage Layer                       │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐   │ │
│  │  │ Pack Registry│  │  Pack Cache  │  │  Metadata DB │   │ │
│  │  │ (~/.ggen)    │  │ (Local)      │  │ (Manifest)   │   │ │
│  │  └──────────────┘  └──────────────┘  └──────────────┘   │ │
│  └────────────────────────────────────────────────────────────┘ │
│                              │                                   │
│                              ▼                                   │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │              Marketplace Integration Layer                 │ │
│  │  • Template resolution via ggen-marketplace                │ │
│  │  • SPARQL query execution via render_with_rdf              │ │
│  │  • Maturity scoring via marketplace_scorer                 │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

## Component Diagram (C4 Level 3)

### Pack Engine Core Components

```
┌────────────────────────────────────────────────────────────────┐
│                        Pack Engine                             │
│                                                                │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │                  Pack Composer                           │ │
│  │  • DependencyResolver: Resolve pack dependencies        │ │
│  │  • ConflictResolver: Merge overlapping templates        │ │
│  │  │  TemplateOrchestrator: Order template generation     │ │
│  │  │  VariableAggregator: Collect/merge pack variables    │ │
│  └──────────────────────────────────────────────────────────┘ │
│                              │                                 │
│                              ▼                                 │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │                 Pack Executor                            │ │
│  │  • TemplateRenderer: Execute ggen-core generation       │ │
│  │  • SPARQLExecutor: Run RDF queries via render_with_rdf  │ │
│  │  • HookManager: Execute pre/post hooks                  │ │
│  │  │  OutputCollector: Aggregate generated files          │ │
│  └──────────────────────────────────────────────────────────┘ │
│                              │                                 │
│                              ▼                                 │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │                Pack Validator                            │ │
│  │  • CompatibilityChecker: Validate pack compatibility    │ │
│  │  • DependencyValidator: Check version constraints       │ │
│  │  • TemplateValidator: Lint pack templates               │ │
│  │  │  MaturityScorer: Score pack quality (like mktplace)  │ │
│  └──────────────────────────────────────────────────────────┘ │
│                              │                                 │
│                              ▼                                 │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │              Pack Repository                             │ │
│  │  • LocalRegistry: ~/.ggen/packs/                        │ │
│  │  • RemoteRegistry: Cloud/CDN storage (future)           │ │
│  │  • CacheManager: Local pack cache                       │ │
│  │  │  ManifestParser: Read pack.toml/pack.yaml           │ │
│  └──────────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────────────┘
```

## Data Flow Diagram

### Pack Generation Flow

```
┌─────────┐      ┌─────────┐      ┌─────────┐      ┌─────────┐
│  User   │─────▶│ CLI     │─────▶│ Service │─────▶│Composer │
│         │ gen  │ parse   │ call │ load    │ init │ resolve │
└─────────┘      └─────────┘      └─────────┘      └─────────┘
                                         │               │
                                         ▼               ▼
                        ┌────────────────────────────────────┐
                        │   1. Load Pack Manifests           │
                        │   2. Resolve Dependencies          │
                        │   3. Validate Compatibility        │
                        │   4. Merge Variables               │
                        │   5. Order Templates               │
                        └────────────────────────────────────┘
                                         │
                                         ▼
                        ┌────────────────────────────────────┐
                        │        Pack Executor               │
                        │   ┌────────────────────────────┐   │
                        │   │  Template 1 (Marketplace)  │   │
                        │   │  ↓ ggen-core render        │   │
                        │   │  ↓ SPARQL queries (RDF)    │   │
                        │   │  ↓ Post-processing hooks   │   │
                        │   └────────────────────────────┘   │
                        │   ┌────────────────────────────┐   │
                        │   │  Template 2 (DevOps)       │   │
                        │   │  ↓ ggen-core render        │   │
                        │   │  ↓ SPARQL queries (RDF)    │   │
                        │   └────────────────────────────┘   │
                        │   ┌────────────────────────────┐   │
                        │   │  Template N (Monitoring)   │   │
                        │   │  ↓ ggen-core render        │   │
                        │   └────────────────────────────┘   │
                        └────────────────────────────────────┘
                                         │
                                         ▼
                        ┌────────────────────────────────────┐
                        │    Output Collector                │
                        │    • Merge file trees              │
                        │    • Resolve conflicts             │
                        │    • Generate final project        │
                        └────────────────────────────────────┘
                                         │
                                         ▼
                                 ┌───────────────┐
                                 │ Project Files │
                                 │ (File System) │
                                 └───────────────┘
```

## Layer Architecture

### Packs Layer Stack

```
┌─────────────────────────────────────────────────────────────┐
│                      Layer 4: CLI                           │
│  ggen pack <verb> [options]                                 │
│  • User-facing commands                                     │
│  • Argument parsing (clap-noun-verb)                        │
│  • Output formatting (JSON/human-readable)                  │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                   Layer 3: Pack Domain                      │
│  ggen-domain::pack::*                                       │
│  • Pack CRUD operations                                     │
│  • Multi-pack composition                                   │
│  • Validation and compatibility checking                    │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                Layer 2: Marketplace/Core                    │
│  ggen-marketplace + ggen-core                               │
│  • Template resolution and rendering                        │
│  • SPARQL query execution (render_with_rdf)                 │
│  • Maturity scoring (marketplace_scorer)                    │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                  Layer 1: Storage/RDF                       │
│  File system + RDF graph store                              │
│  • Pack manifests (~/.ggen/packs/)                          │
│  • Template files (marketplace)                             │
│  • RDF/TTL ontologies                                       │
└─────────────────────────────────────────────────────────────┘
```

**Key Insight**: Packs are a **pure composition layer** with no new template rendering logic. All generation uses existing ggen-core and marketplace infrastructure.

## Technology Stack

### Core Dependencies

| Component | Technology | Purpose |
|-----------|-----------|---------|
| Pack Manifest | TOML/YAML | Pack definition format |
| Template Engine | ggen-core | Template rendering |
| SPARQL Queries | RDF/TTL | Semantic generation |
| Validation | marketplace_scorer | Quality scoring |
| Storage | File system | Pack registry |
| CLI | clap-noun-verb | Command parsing |
| Serialization | serde | Data (de)serialization |

### Integration Points

1. **ggen-marketplace**: Template discovery, installation, validation
2. **ggen-core**: Template rendering via `TemplateEngine`
3. **render_with_rdf**: SPARQL query execution for semantic generation
4. **marketplace_scorer**: Maturity and quality scoring

## Architectural Patterns

### 1. Composition Over Inheritance

Packs **compose** marketplace templates rather than extending them. This preserves template independence and enables flexible mixing.

### 2. Dependency Injection

Pack manifests declare dependencies on:
- Marketplace packages (templates)
- Other packs (pack dependencies)
- SPARQL queries (semantic rules)

The composer resolves and injects these at generation time.

### 3. Pipeline Architecture

Generation follows a strict pipeline:
1. Load → 2. Validate → 3. Resolve → 4. Merge → 5. Execute → 6. Collect

Each stage is independently testable and can be extended with hooks.

### 4. Registry Pattern

Pack storage follows the registry pattern:
- Local registry: `~/.ggen/packs/`
- Remote registry: Future cloud/CDN integration
- Cache layer: Performance optimization

## Quality Attributes

### Performance Targets

| Operation | Target | Measurement |
|-----------|--------|-------------|
| Pack list | < 50ms | Cold cache |
| Pack show | < 100ms | With metadata |
| Pack install | < 2s | Network dependent |
| Pack generate | < 10s | For 3-pack composition |
| Pack validate | < 500ms | Including SPARQL |

### Scalability

- Support 1000+ packs in registry
- Handle 10+ pack compositions
- Process 100+ templates per pack
- Execute 50+ SPARQL queries per pack

### Reliability

- 99.9% generation success rate
- Atomic pack operations (install/uninstall)
- Rollback support for failed generations
- Dependency conflict detection

### Maintainability

- Clear separation of concerns (4 layers)
- Extensive unit/integration tests
- Comprehensive error messages
- ADRs for major decisions

## Non-Functional Requirements

### Security

- No arbitrary code execution in pack manifests
- Template sandboxing (inherited from ggen-core)
- Dependency validation (checksum verification)
- User-controlled pack sources

### Extensibility

- Plugin system for custom validators
- Hook points for pre/post generation
- Custom SPARQL query injection
- Template override mechanisms

### Observability

- Structured logging (tracing crate)
- Performance metrics (generation time, cache hits)
- Error telemetry (failure categorization)
- Generation audit logs

## Deployment Architecture

### Local Development

```
~/.ggen/
├── packs/                    # Local pack registry
│   ├── startup-pack/
│   │   ├── pack.toml        # Pack manifest
│   │   ├── templates/       # Bundled templates (optional)
│   │   └── queries/         # SPARQL queries
│   ├── devops-pack/
│   └── monitoring-pack/
├── cache/                    # Generated output cache
└── marketplace/              # Installed templates
```

### Future: Cloud Registry

```
                  ┌─────────────────┐
                  │   CDN/Registry  │
                  │  (cloud-hosted) │
                  └─────────────────┘
                          │
                          ▼
        ┌─────────────────────────────────┐
        │      Pack Distribution          │
        │  • Versioned pack bundles       │
        │  • Checksum verification        │
        │  • Dependency resolution        │
        └─────────────────────────────────┘
                          │
                          ▼
                  ┌─────────────────┐
                  │  Local Cache    │
                  │  (~/.ggen)      │
                  └─────────────────┘
```

## Migration Strategy

### Phase 1: Foundation (Current)
- Pack data structures and traits
- Basic pack CRUD operations
- Single-pack generation

### Phase 2: Composition
- Multi-pack dependency resolution
- Variable merging and conflict resolution
- Template orchestration

### Phase 3: Validation
- Pack compatibility checking
- Maturity scoring integration
- Benchmarking system

### Phase 4: Distribution
- Remote pack registry
- Pack publishing workflow
- Version management

## Constraints and Assumptions

### Constraints

1. **No Breaking Changes**: Packs must work with existing marketplace
2. **Backward Compatibility**: Existing templates work without modification
3. **Performance**: No significant overhead vs single template generation
4. **Storage**: Local pack registry < 1GB for 1000 packs

### Assumptions

1. Users have ggen-cli installed and configured
2. Marketplace templates are well-formed
3. SPARQL queries are syntactically valid
4. Network connectivity for remote pack install (future)

## Success Metrics

| Metric | Target | Measurement Method |
|--------|--------|--------------------|
| Pack adoption | 100+ packs in 6 months | Registry analytics |
| Generation success | 95% first-time success | Telemetry |
| User satisfaction | 4.5/5 rating | Survey |
| Performance | < 10s for 3-pack gen | Benchmarks |
| Documentation coverage | 100% public APIs | Doc tests |

## Next Steps

1. Implement core data structures (Pack, PackComposition)
2. Build pack CRUD operations (list, show, install)
3. Implement single-pack generation
4. Add multi-pack composition
5. Integrate validation and scoring
6. Write comprehensive tests
7. Create user documentation
