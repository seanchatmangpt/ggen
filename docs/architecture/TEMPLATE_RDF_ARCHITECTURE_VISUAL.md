# Template-RDF Architecture Visual Summary

## System Overview (ASCII Art)

```
╔═══════════════════════════════════════════════════════════════════════╗
║                    GGEN TEMPLATE-TO-FILE-TREE ENGINE                  ║
╚═══════════════════════════════════════════════════════════════════════╝

┌─────────────────────────────────────────────────────────────────────┐
│ USER INTERFACE LAYER                                                │
├─────────────────────────────────────────────────────────────────────┤
│  ggen template generate <pack>:<template>                           │
│  ggen lifecycle run generate                                        │
│  ggen market add <package>                                          │
└─────────────────────────────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────┐
│ ORCHESTRATION LAYER                                                 │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌────────────────┐    ┌──────────────┐    ┌──────────────────┐  │
│  │  Template      │───▶│  Lifecycle   │───▶│   Generator      │  │
│  │  Resolver      │    │  Manager     │    │   Pipeline       │  │
│  └────────────────┘    └──────────────┘    └──────────────────┘  │
│         │                      │                      │            │
│         │                      │                      │            │
│  ┌──────▼──────┐        ┌──────▼──────┐       ┌──────▼────────┐  │
│  │  Cache      │        │  Lockfile   │       │  File Tree    │  │
│  │  Manager    │        │  Manager    │       │  Generator    │  │
│  └─────────────┘        └─────────────┘       └───────────────┘  │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────┐
│ PROCESSING LAYER                                                    │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │ Template Processing Pipeline                                │  │
│  │                                                             │  │
│  │  1. Parse         2. Render         3. Process            │  │
│  │  ┌──────────┐    ┌──────────┐      ┌──────────┐          │  │
│  │  │YAML+Body │───▶│Frontmatter│─────▶│   RDF    │          │  │
│  │  └──────────┘    └──────────┘      │  Graph   │          │  │
│  │                         │           └──────────┘          │  │
│  │                         ▼                 │               │  │
│  │                  ┌──────────┐             │               │  │
│  │                  │  Tera    │◀────────────┘               │  │
│  │                  │ Context  │                             │  │
│  │                  └──────────┘                             │  │
│  │                         │                                 │  │
│  │                         ▼                                 │  │
│  │  4. SPARQL        5. Render         6. Generate          │  │
│  │  ┌──────────┐    ┌──────────┐      ┌──────────┐          │  │
│  │  │  Query   │───▶│  Body    │─────▶│  Files   │          │  │
│  │  │ Results  │    │ Template │      │  & Dirs  │          │  │
│  │  └──────────┘    └──────────┘      └──────────┘          │  │
│  └─────────────────────────────────────────────────────────────┘  │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────┐
│ STORAGE LAYER                                                       │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────────┐ │
│  │  RDF Graph   │  │  Template    │  │  File System             │ │
│  │  (Oxigraph)  │  │  Cache       │  │  (Output)                │ │
│  │              │  │  (LRU)       │  │                          │ │
│  │  • Triples   │  │  • Plans     │  │  • Generated Files       │ │
│  │  • SPARQL    │  │  • Results   │  │  • Directory Structure   │ │
│  │  • Prefixes  │  │  • Packages  │  │  • Checksums             │ │
│  └──────────────┘  └──────────────┘  └──────────────────────────┘ │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Data Flow Diagram

```
MARKETPLACE                TEMPLATE                   RDF/SPARQL
   LAYER                    LAYER                       LAYER
     │                        │                           │
     │ 1. Search/Install      │                           │
     ├────────────────────────▶                           │
     │                        │                           │
     │ 2. Resolve Reference   │                           │
     ├────────────────────────▶                           │
     │                        │                           │
     │                        │ 3. Parse Template         │
     │                        ├───────────────────────────▶
     │                        │                           │
     │                        │ 4. Load RDF Data          │
     │                        ├───────────────────────────▶
     │                        │                           │
     │                        │ 5. Execute SPARQL         │
     │                        ├───────────────────────────▶
     │                        │                           │
     │                        │ 6. Query Results          │
     │                        │◀───────────────────────────┤
     │                        │                           │
     │                        │ 7. Render Template        │
     │                        │                           │
     │                        │                           │
     │                        ▼                           │
     │              FILE GENERATION                       │
     │                        │                           │
     │                        │ 8. Create Directories     │
     │                        │                           │
     │                        │ 9. Write Files            │
     │                        │                           │
     │                        │ 10. Verify Checksums      │
     │                        │                           │
     │                        ▼                           │
     │              OUTPUT FILE SYSTEM                    │
     │                                                    │
     └────────────────────────────────────────────────────┘
```

## Component Interaction Matrix

```
┌──────────────────┬──────────┬──────────┬──────────┬──────────┬──────────┐
│ Component        │ Template │ Graph    │ Cache    │ Lockfile │ FileTree │
│                  │ Resolver │ Store    │ Manager  │ Manager  │ Generator│
├──────────────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
│ Template         │    •     │          │    R     │    R     │    P     │
│ Resolver         │          │          │          │          │          │
├──────────────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
│ Graph            │          │    •     │    R     │          │    U     │
│ Store            │          │          │          │          │          │
├──────────────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
│ Cache            │    U     │    U     │    •     │    R     │          │
│ Manager          │          │          │          │          │          │
├──────────────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
│ Lockfile         │    U     │          │    R     │    •     │          │
│ Manager          │          │          │          │          │          │
├──────────────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
│ FileTree         │    U     │    U     │          │          │    •     │
│ Generator        │          │          │          │          │          │
└──────────────────┴──────────┴──────────┴──────────┴──────────┴──────────┘

Legend: • = Self, R = Reads, U = Uses, P = Provides
```

## Template Processing Sequence

```
┌─────────────┐
│   START     │
└──────┬──────┘
       │
       ▼
┌─────────────────────────────────┐
│ 1. Load Template from Cache     │
│    Input: pack_id:template_path │
│    Output: Template String      │
└──────────────┬──────────────────┘
               │
               ▼
┌─────────────────────────────────┐
│ 2. Parse Frontmatter & Body     │
│    Parser: gray-matter          │
│    Output: YAML + Tera String   │
└──────────────┬──────────────────┘
               │
               ▼
┌─────────────────────────────────┐
│ 3. Render Frontmatter           │
│    Engine: Tera                 │
│    Resolve: {{ vars }}          │
└──────────────┬──────────────────┘
               │
               ▼
┌─────────────────────────────────┐
│ 4. Load RDF Data                │
│    Sources:                     │
│    - rdf_inline: [...]          │
│    - rdf: [file1.ttl, ...]      │
│    Store: Oxigraph              │
└──────────────┬──────────────────┘
               │
               ▼
┌─────────────────────────────────┐
│ 5. Execute SPARQL Queries       │
│    For each named query:        │
│    - Prepend PREFIX/BASE        │
│    - Execute (cached)           │
│    - Store results              │
└──────────────┬──────────────────┘
               │
               ▼
┌─────────────────────────────────┐
│ 6. Render Template Body         │
│    Context:                     │
│    - vars                       │
│    - sparql_results             │
│    - env vars                   │
└──────────────┬──────────────────┘
               │
               ▼
┌─────────────────────────────────┐
│ 7. Determine Output Path        │
│    From: frontmatter.to         │
│    Validate: Path security      │
└──────────────┬──────────────────┘
               │
               ▼
┌─────────────────────────────────┐
│ 8. Create Parent Directories    │
│    mkdir -p $(dirname path)     │
└──────────────┬──────────────────┘
               │
               ▼
┌─────────────────────────────────┐
│ 9. Write Output File            │
│    Mode: Create/Overwrite/Inject│
│    Calculate: SHA-256 checksum  │
└──────────────┬──────────────────┘
               │
               ▼
┌─────────────────────────────────┐
│ 10. Execute Post Hooks          │
│     sh_after commands           │
└──────────────┬──────────────────┘
               │
               ▼
        ┌──────────┐
        │   END    │
        └──────────┘
```

## RDF Graph Integration Flow

```
Template Frontmatter
─────────────────────
┌──────────────────────────────────────┐
│ prefixes:                            │
│   ex: "http://example.org/"          │
│ base: "http://example.org/myapp/"    │
│ rdf_inline:                          │
│   - "ex:Alice a ex:Person ."         │
│ rdf:                                 │
│   - "data/schema.ttl"                │
│ sparql:                              │
│   people: "SELECT ?person ..."       │
└──────────────────────────────────────┘
                │
                ▼
        ┌───────────────┐
        │  Build Prolog │
        └───────┬───────┘
                │
    ┌───────────┴───────────┐
    │                       │
    ▼                       ▼
┌─────────────┐     ┌──────────────┐
│ Load Inline │     │  Load Files  │
│    Turtle   │     │  (*.ttl)     │
└──────┬──────┘     └──────┬───────┘
       │                   │
       └─────────┬─────────┘
                 ▼
        ┌─────────────────┐
        │   RDF Graph     │
        │   (Oxigraph)    │
        └────────┬────────┘
                 │
                 ▼
        ┌─────────────────┐
        │ Execute SPARQL  │
        │   (Cached)      │
        └────────┬────────┘
                 │
                 ▼
        ┌─────────────────┐
        │ Query Results   │
        │  as JSON        │
        └────────┬────────┘
                 │
                 ▼
    ┌────────────────────────┐
    │  Available in Template │
    │  {{ sparql_results }}  │
    └────────────────────────┘
```

## Security Architecture Layers

```
┌─────────────────────────────────────────────────────────────┐
│ SECURITY LAYER 1: INPUT VALIDATION                          │
├─────────────────────────────────────────────────────────────┤
│ • Template reference syntax validation                      │
│ • Variable type checking                                    │
│ • YAML frontmatter schema validation                        │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ SECURITY LAYER 2: PATH SECURITY                             │
├─────────────────────────────────────────────────────────────┤
│ • Reject ".." components                                    │
│ • Canonical path resolution                                 │
│ • Boundary checking (output_root)                           │
│ • RDF file path validation                                  │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ SECURITY LAYER 3: CONTENT VERIFICATION                      │
├─────────────────────────────────────────────────────────────┤
│ • PQC SHA-256 checksums                                     │
│ • Package signature verification                            │
│ • Lockfile integrity checks                                 │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ SECURITY LAYER 4: RDF VALIDATION                            │
├─────────────────────────────────────────────────────────────┤
│ • SHACL shapes validation                                   │
│ • Graph consistency checks                                  │
│ • SPARQL injection prevention                               │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ SECURITY LAYER 5: OUTPUT PROTECTION                         │
├─────────────────────────────────────────────────────────────┤
│ • File permission controls                                  │
│ • Atomic file writes                                        │
│ • Backup creation (optional)                                │
│ • Rollback on errors                                        │
└─────────────────────────────────────────────────────────────┘
```

## Performance Optimization Points

```
┌────────────────────────────────────────────────────────────┐
│ OPTIMIZATION 1: PACKAGE CACHE                              │
│ └─ LRU Cache (1GB)                                         │
│    └─ Avoid repeated downloads                             │
│       └─ Checksum-based validation                         │
└────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────┐
│ OPTIMIZATION 2: SPARQL QUERY CACHE                         │
│ └─ Plan Cache (LRU 100)                                    │
│    └─ Result Cache (LRU 1000)                              │
│       └─ Epoch-based invalidation                          │
└────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────┐
│ OPTIMIZATION 3: PARALLEL PROCESSING                        │
│ └─ Independent template processing                         │
│    └─ Rayon thread pool                                    │
│       └─ CPU-bound work distribution                       │
└────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────┐
│ OPTIMIZATION 4: INCREMENTAL GENERATION                     │
│ └─ Lifecycle output tracking                               │
│    └─ Checksum comparison                                  │
│       └─ Skip unchanged files                              │
└────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────┐
│ OPTIMIZATION 5: DETERMINISTIC OUTPUT                       │
│ └─ Sorted maps/lists                                       │
│    └─ Stable RDF serialization                             │
│       └─ Reproducible checksums                            │
└────────────────────────────────────────────────────────────┘
```

## Key Metrics & Performance Targets

```
┌─────────────────────┬──────────────┬──────────────┬──────────┐
│ Operation           │ Target Time  │ Cache Hit    │ Memory   │
├─────────────────────┼──────────────┼──────────────┼──────────┤
│ Package Resolution  │ <10ms        │ 95%+         │ 10MB     │
│ Template Parse      │ <5ms         │ N/A          │ 1MB      │
│ Frontmatter Render  │ <1ms         │ N/A          │ 100KB    │
│ RDF Load (10KB TTL) │ <50ms        │ N/A          │ 500KB    │
│ SPARQL Query (100r) │ <20ms        │ 80%+         │ 2MB      │
│ Template Render     │ <10ms        │ N/A          │ 500KB    │
│ File Write (10KB)   │ <5ms         │ N/A          │ 10KB     │
│ Complete Generation │ <100ms       │ Varies       │ 20MB     │
└─────────────────────┴──────────────┴──────────────┴──────────┘

Total Memory Budget: 100MB per concurrent generation
Maximum Concurrency: 10 templates in parallel
Throughput Target: 1000 files/second (simple templates)
```

---

This visual summary provides a quick reference to the complete architecture, suitable for:
- System overview presentations
- Developer onboarding
- Architecture reviews
- Implementation planning
