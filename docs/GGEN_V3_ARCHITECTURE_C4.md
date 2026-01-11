# ggen v3 Architecture - C4 Model Diagrams

**Status**: PLANNING
**Version**: 3.0.0-alpha
**Focus**: System, Container, and Component level architecture

---

## Context Diagram (C1)

Shows ggen v3 in the broader ecosystem:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          Software Development Community                  │
└─────────────────────────────────────────────────────────────────────────┘
                                        │
                    ┌───────────────────┼───────────────────┐
                    │                   │                   │
                    ▼                   ▼                   ▼
        ┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
        │  ggen Users      │ │  ggen Contributors
        │                  │ │                  │ │  ggen Platform   │
        │ - E-commerce     │ │ - Template       │ │ (us - self-       │
        │ - Healthcare     │ │   developers     │ │  hosting)        │
        │ - Microservices  │ │ - Ontology       │ │                  │
        │ - Academics      │ │   designers      │ │                  │
        │ - SaaS           │ │ - AI researchers │ │                  │
        └──────────┬───────┘ └──────────┬───────┘ └──────────┬───────┘
                   │                    │                    │
                   └────────────────────┼────────────────────┘
                                        │
                    ┌───────────────────▼───────────────────┐
                    │                                       │
                    │         [ggen v3 System]              │
                    │                                       │
                    │  • Ontology-driven code generation    │
                    │  • Self-hosting projection engine     │
                    │  • Marketplace with 8020 bundles      │
                    │  • AI-powered ontology evolution      │
                    │                                       │
                    └───────────────────┬───────────────────┘
                                        │
            ┌───────────────────────────┼───────────────────────────┐
            │                           │                           │
            ▼                           ▼                           ▼
    ┌──────────────────┐      ┌──────────────────┐      ┌──────────────────┐
    │  Git Repository  │      │  RDF Triple      │      │  LLM Services    │
    │                  │      │  Store           │      │                  │
    │ - ggen codebase  │      │ (Oxigraph)       │      │ - OpenAI         │
    │ - Ontologies     │      │                  │      │ - Anthropic      │
    │ - Templates      │      │ - ggen_v3_core   │      │ - Ollama         │
    │                  │      │   .ttl           │      │ - Gemini         │
    │                  │      │ - User ontologies│      │ - Groq           │
    │                  │      │                  │      │                  │
    └──────────────────┘      └──────────────────┘      └──────────────────┘
```

---

## System Context Diagram (C2)

The ggen v3 system and its immediate neighbors:

```
┌──────────────────────────────────────────────────────────────────────────────┐
│                                                                              │
│                          Developer's Local Machine                           │
│                                                                              │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │                                                                        │ │
│  │  ┌───────────────────┐   ┌───────────────────────────────────────┐   │ │
│  │  │   Shell/Terminal  │   │    Text Editor / IDE                 │   │ │
│  │  │                   │   │ (Edit ontologies, templates, code)   │   │ │
│  │  │ $ ggen project    │   │                                       │   │ │
│  │  │   gen . --        │   │ • ontologies/ggen_v3_core.ttl       │   │ │
│  │  │   ontology ...    │   │ • templates/π_*/*.tmpl              │   │ │
│  │  │                   │   │ • crates/*/src/                     │   │ │
│  │  │ $ cargo test      │   │                                       │   │ │
│  │  │ $ cargo build     │   │                                       │   │ │
│  │  └─────────┬─────────┘   └───────────────┬───────────────────┘   │ │
│  │            │                             │                       │ │
│  │            └──────────────┬──────────────┘                       │ │
│  │                           │                                      │ │
│  │                    ┌──────▼──────────┐                          │ │
│  │                    │  .ggen/ working │                          │ │
│  │                    │  directory      │                          │ │
│  │                    │                 │                          │ │
│  │                    │ - ggen-v3       │                          │ │
│  │                    │   config cache  │                          │ │
│  │                    │ - Generated code│                          │ │
│  │                    │   (output)      │                          │ │
│  │                    └─────────────────┘                          │ │
│  │                                                                  │ │
│  └──────────────────────────────────────┬───────────────────────────┘ │
│                                         │                             │
│                                         ▼                             │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │         ggen v3 System (Binary / Executed)                  │    │
│  │  ┌──────────────────────────────────────────────────────┐   │    │
│  │  │  CLI Interface (32 commands via noun-verb routing)   │   │    │
│  │  │  $ ggen ai generate-ontology                         │   │    │
│  │  │  $ ggen template generate-rdf                        │   │    │
│  │  │  $ ggen project gen [--ontology X]                   │   │    │
│  │  │  $ ggen marketplace [search|install|publish]         │   │    │
│  │  │  $ ggen graph [load|query|export|diff]               │   │    │
│  │  └──────────────────────────────────────────────────────┘   │    │
│  │                                                              │    │
│  │  [Detailed inside System Container Diagram below]           │    │
│  │                                                              │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
                                │
                ┌───────────────┼───────────────┐
                │               │               │
                ▼               ▼               ▼
     ┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
     │  GitHub          │ │  RDF Triple      │ │  LLM API         │
     │  Repository      │ │  Store Cloud     │ │  Endpoints       │
     │                  │ │ (Optional)       │ │                  │
     │ - Push/pull code │ │                  │ │ - api.openai.com │
     │ - Hooks & CI/CD  │ │ - Collaborative │ │ - api.anthropic  │
     │ - Marketplace    │ │   ontology       │ │ - localhost:8000 │
     │   publishing     │ │   storage        │ │   (local Ollama) │
     │                  │ │ - Streaming      │ │                  │
     │                  │ │   SPARQL eval    │ │                  │
     └──────────────────┘ └──────────────────┘ └──────────────────┘
```

---

## Container Diagram (C2 - System Internals)

The internal containers/modules of ggen v3:

```
┌──────────────────────────────────────────────────────────────────────────────┐
│                                 ggen v3 System                               │
│                           (monorepo in Rust/Cargo)                          │
│                                                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  CLI Layer (ggen-cli crate)                                         │   │
│  │  • Command parsing (clap + noun-verb routing)                      │   │
│  │  • User interface, output formatting                               │   │
│  │  • Help text, auto-completion                                     │   │
│  │  • Generated from: π_cli projections                              │   │
│  └────────┬──────────────────────────────────────────────────────────┘   │
│           │ "execute command"                                             │
│  ┌────────▼──────────────────────────────────────────────────────────┐   │
│  │  Domain Layer (ggen-domain crate)                                │   │
│  │  • Pure business logic                                          │   │
│  │  • Reusable across all CLI commands                           │   │
│  │  • Generated from: π_domain projections                       │   │
│  │  ┌──────────────────────────────────────────────────────────┐ │   │
│  │  │ Service Implementations:                                 │ │   │
│  │  │ • ProjectService (new, gen, watch)                      │ │   │
│  │  │ • TemplateService (generate-rdf, lint, list)           │ │   │
│  │  │ • MarketplaceService (search, install, publish)        │ │   │
│  │  │ • GraphService (load, query, export, diff)             │ │   │
│  │  │ • AiService (generate-ontology, chat, analyze)         │ │   │
│  │  │ • HookService (create, monitor, validate)              │ │   │
│  │  └──────────────────────────────────────────────────────────┘ │   │
│  └─────────┬────────────────────────────────────────────────────────┘   │
│            │ "perform business logic"                                   │
│  ┌─────────▼────────────────────────────────────────────────────────┐   │
│  │  Infrastructure Layer                                            │   │
│  │  ┌───────────────────────────────────────────────────────────┐  │   │
│  │  │  Core Engine (ggen-core crate)                            │  │   │
│  │  │  • RDF graph operations (Oxigraph triple store)          │  │   │
│  │  │  • SPARQL 1.1 query execution                           │  │   │
│  │  │  • Template rendering (Tera)                            │  │   │
│  │  │  • Delta detection & change analysis                    │  │   │
│  │  │  • Three-way merge (generated + manual content)         │  │   │
│  │  │  • Lifecycle management (init→setup→build→test→deploy) │  │   │
│  │  │  • Generated from: π_core, π_tests                      │  │   │
│  │  │                                                           │  │   │
│  │  │  Internal structure:                                     │  │   │
│  │  │  ├── graph/       (RDF, SPARQL, delta)                  │  │   │
│  │  │  ├── lifecycle/   (Build system phases)                 │  │   │
│  │  │  ├── templates/   (Tera rendering)                      │  │   │
│  │  │  ├── project_generator/ (Scaffolding)                  │  │   │
│  │  │  ├── merge/       (Conflict resolution)                │  │   │
│  │  │  └── ontology/    (Autonomous system)                  │  │   │
│  │  └───────────────────────────────────────────────────────────┘  │   │
│  │                                                                   │   │
│  │  ┌───────────────────────────────────────────────────────────┐  │   │
│  │  │  AI Module (ggen-ai crate)                                │  │   │
│  │  │  • Multi-provider LLM client                             │  │   │
│  │  │  • Schema-anchored generation                           │  │   │
│  │  │  • Streaming responses                                  │  │   │
│  │  │  • Generated from: π_ai projections                     │  │   │
│  │  │  ├── OpenAI client (GPT-4o, GPT-4-turbo)              │  │   │
│  │  │  ├── Anthropic client (Claude 3.5)                     │  │   │
│  │  │  ├── Ollama client (local models)                      │  │   │
│  │  │  ├── Gemini client                                     │  │   │
│  │  │  └── Groq client                                       │  │   │
│  │  └───────────────────────────────────────────────────────────┘  │   │
│  │                                                                   │   │
│  │  ┌───────────────────────────────────────────────────────────┐  │   │
│  │  │  Marketplace Engine (ggen-marketplace crate)              │  │   │
│  │  │  • Package discovery & search (Tantivy)                 │  │   │
│  │  │  • Installation & dependency resolution                │  │   │
│  │  │  • Validation guards (8020 compliance, readiness)      │  │   │
│  │  │  • Production readiness scoring (6 dimensions)         │  │   │
│  │  │  • Signed validation receipts (ML-DSA)                 │  │   │
│  │  │  • Generated from: π_marketplace projections           │  │   │
│  │  └───────────────────────────────────────────────────────────┘  │   │
│  │                                                                   │   │
│  │  ┌───────────────────────────────────────────────────────────┐  │   │
│  │  │  Utilities (ggen-utils crate)                             │  │   │
│  │  │  • File system operations                               │  │   │
│  │  │  • Git integration                                      │  │   │
│  │  │  • Logging & diagnostics                               │  │   │
│  │  │  • Configuration management                            │  │   │
│  │  │  • Generated from: π_utils, π_docs                     │  │   │
│  │  └───────────────────────────────────────────────────────────┘  │   │
│  │                                                                   │   │
│  │  ┌───────────────────────────────────────────────────────────┐  │   │
│  │  │  Node.js Bindings (ggen-node crate) [Optional]          │  │   │
│  │  │  • WASM compiled ggen core                              │  │   │
│  │  │  • npm package for JS/TS projects                       │  │   │
│  │  │  • Generated from: π_node projections                  │  │   │
│  │  └───────────────────────────────────────────────────────────┘  │   │
│  │                                                                   │   │
│  └───────────────────────────────────────────────────────────────┘   │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │  Data & Configuration                                        │   │
│  │  ┌──────────────────────────────────────────────────────────┐   │
│  │  │  Ontology Runtime Store                                 │   │
│  │  │  • ggen_v3_core.ttl (primary ontology)                 │   │
│  │  │  • User project ontologies (*.ttl)                    │   │
│  │  │  • Loaded into Oxigraph in-memory store              │   │
│  │  │  • SPARQL queries executed against graph             │   │
│  │  └──────────────────────────────────────────────────────────┘   │
│  │                                                                  │
│  │  ┌──────────────────────────────────────────────────────────┐   │
│  │  │  Template System                                        │   │
│  │  │  • π_core/                (module scaffolding)         │   │
│  │  │  • π_domain/              (type generation)            │   │
│  │  │  • π_cli/                 (command generation)         │   │
│  │  │  • π_marketplace/         (validation guards)          │   │
│  │  │  • π_ai/                  (LLM client generation)      │   │
│  │  │  • π_tests/               (test scaffolds)             │   │
│  │  │  • π_docs/                (documentation generation)   │   │
│  │  │  • π_deployment/          (Docker/K8s configs)        │   │
│  │  │                                                         │   │
│  │  │  Each template:                                        │   │
│  │  │  • Tera syntax for variable substitution              │   │
│  │  │  • YAML/TOML frontmatter with SPARQL query            │   │
│  │  │  • Generates code, configs, docs, tests               │   │
│  │  └──────────────────────────────────────────────────────────┘   │
│  │                                                                  │
│  │  ┌──────────────────────────────────────────────────────────┐   │
│  │  │  Marketplace Registry                                  │   │
│  │  │  • 76+ sector-organized packages                       │   │
│  │  │  • Package manifests (package.toml)                   │   │
│  │  │  • Validation receipts (signed proofs)                │   │
│  │  │  • Dependency graphs                                  │   │
│  │  │  • Score metrics (production readiness)               │   │
│  │  └──────────────────────────────────────────────────────────┘   │
│  │                                                                  │
│  └──────────────────────────────────────────────────────────────┘   │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
```

---

## Component Diagram (C3 - Core Engine Internals)

Deep dive into ggen-core (the heart of the system):

```
┌──────────────────────────────────────────────────────────────────────────────┐
│                         ggen-core (Core Engine)                             │
│  The semantic projection engine: ontology → queries → templates → code        │
│                                                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  Graph Module (src/graph/)                                         │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  RDF Store Manager                                          │  │   │
│  │  │  • Oxigraph triple store (in-memory)                       │  │   │
│  │  │  • Load RDF/Turtle files                                  │  │   │
│  │  │  • Insert/delete triples                                  │  │   │
│  │  │  • Version control (content-addressed snapshots)          │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                ▲                                    │   │
│  │                                │ "execute query"                    │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  SPARQL Query Engine                                        │  │   │
│  │  │  • Parse SPARQL 1.1 queries                               │  │   │
│  │  │  • Execute queries on graph                               │  │   │
│  │  │  • Result caching (moka cache)                            │  │   │
│  │  │  • Query plans & optimization                             │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                                                    │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Delta Detection                                            │  │   │
│  │  │  • Compare two RDF graphs                                  │  │   │
│  │  │  • Identify added/removed/changed triples                │  │   │
│  │  │  • Compute affected entities (which crates/modules)       │  │   │
│  │  │  • Minimal regeneration scope                             │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                                                    │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Serialization Support                                      │  │   │
│  │  │  • RDF/Turtle export                                       │  │   │
│  │  │  • JSON-LD export                                          │  │   │
│  │  │  • N-Triples format                                        │  │   │
│  │  │  • YAML/JSON ontology projections                         │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  Template Module (src/templates/)                                  │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Frontmatter Parser                                         │  │   │
│  │  │  • Parse YAML/TOML headers                                │  │   │
│  │  │  • Extract SPARQL queries                                 │  │   │
│  │  │  • Parse metadata (target, scope, frequency)              │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                ▲                                    │   │
│  │                                │ "substitute variables"             │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Tera Template Engine                                      │  │   │
│  │  │  • Parse Tera/Jinja2 syntax                               │  │   │
│  │  │  • Variable substitution from SPARQL results             │  │   │
│  │  │  • Loop & conditional logic                              │  │   │
│  │  │  • Filter functions (indent, uppercase, etc.)            │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                                                    │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Output Writer                                              │  │   │
│  │  │  • Write rendered templates to disk                       │  │   │
│  │  │  • Preserve manual edits (three-way merge)                │  │   │
│  │  │  • Format code (rustfmt, prettier integration)            │  │   │
│  │  │  • Validation (compilation checks)                        │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  Lifecycle Module (src/lifecycle/)                                │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Phase: Init                                               │  │   │
│  │  │  • Create ggen.toml                                        │  │   │
│  │  │  • Initialize ontologies/ directory                       │  │   │
│  │  │  • Generate Cargo.toml scaffolds                          │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                ▼ (sequential)                      │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Phase: Setup                                              │  │   │
│  │  │  • Download marketplace packages                          │  │   │
│  │  │  • Resolve dependencies                                   │  │   │
│  │  │  • Link ontologies                                        │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                ▼                                    │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Phase: Generate                                           │  │   │
│  │  │  • Load ontologies into RDF store                         │  │   │
│  │  │  • Execute SPARQL queries                                 │  │   │
│  │  │  • Render all templates                                   │  │   │
│  │  │  • Write output files                                     │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                ▼                                    │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Phase: Test                                               │  │   │
│  │  │  • Run unit tests                                          │  │   │
│  │  │  • Run integration tests                                   │  │   │
│  │  │  • Validate generated code compiles                       │  │   │
│  │  │  • Check against guards                                   │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                ▼                                    │   │
│  │  ┌──────────────────────────────────────────────────────────────┐  │   │
│  │  │  Phase: Deploy                                             │  │   │
│  │  │  • Build release artifacts                                │  │   │
│  │  │  • Create container images                                │  │   │
│  │  │  • Push to registry                                       │  │   │
│  │  │  • Update deployment manifests                            │  │   │
│  │  └──────────────────────────────────────────────────────────────┘  │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  Merge Module (src/merge/)                                         │   │
│  │  • Three-way merge: (generated_old, generated_new, manual)        │   │
│  │  • Conflict detection & resolution strategies                     │   │
│  │  • Preserve user modifications across regenerations              │   │
│  │  • Audit trail of conflicts                                      │   │
│  │                                                                   │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  Ontology Module (src/ontology/)                                   │   │
│  │  • Autonomous Ontology System (Σ)                                │   │
│  │  • Self-evolution via LLM proposals                              │   │
│  │  • Multi-layer validation (static, dynamic, performance)         │   │
│  │  • Observation→Detection→Proposal→Validation→Promotion cycle    │   │
│  │  • Cryptographic receipts of evolution                          │   │
│  │                                                                   │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
└──────────────────────────────────────────────────────────────────────────────┘
```

---

## Data Flow Diagram (Generation Pipeline)

How an ontology becomes code:

```
┌─────────────┐
│  User runs  │
│ ggen proj   │
│   gen --    │
│  ontology   │
│    X.ttl    │
└──────┬──────┘
       │
       ▼
┌────────────────────────────────────────────────────────────────┐
│ 1. LOAD PHASE                                                  │
│                                                                │
│   X.ttl ─────┬──────────┐                                     │
│              │          │                                     │
│              ▼          ▼                                     │
│       ┌─────────────────────────────────┐                    │
│       │  Oxigraph RDF Triple Store      │                    │
│       │  (in-memory graph database)     │                    │
│       │  • Entity: ggen:Crate "core"    │                    │
│       │  • Entity: ggen:Module "graph"  │                    │
│       │  • Relation: core:exports graph │                    │
│       │  • Prop: graph:name "graph"     │                    │
│       │  [~1000s of triples]            │                    │
│       └─────────────────────────────────┘                    │
│                      ▲                                       │
│                      │ "add triples"                        │
│              ┌───────┴────────┐                             │
│              │                │                             │
│      Marketplace packages    User ontologies                │
│      (dependencies)                                         │
│                                                            │
└──────────────────────────────┬───────────────────────────────┘
                               │
                               ▼
┌────────────────────────────────────────────────────────────────┐
│ 2. QUERY PHASE                                                 │
│                                                                │
│  For each template file (e.g., π_core/module.tmpl):          │
│                                                                │
│  ┌─ YAML Frontmatter:                                         │
│  │  query: |                                                 │
│  │    SELECT ?moduleName ?moduleFullPath WHERE {             │
│  │      ?m a ggen:Module .                                   │
│  │      ?m ggen:name ?moduleName .                           │
│  │      ?m ggen:path ?moduleFullPath .                       │
│  │    }                                                       │
│  │  target: "src/{{ moduleName }}/mod.rs"                   │
│  └─ Execute SPARQL against graph                            │
│                                                              │
│  ┌──────────────────────────────────────────┐               │
│  │ Query Result:                            │               │
│  │ [ { moduleName: "graph",                 │               │
│  │     moduleFullPath: "src/graph" },       │               │
│  │   { moduleName: "lifecycle",             │               │
│  │     moduleFullPath: "src/lifecycle" },   │               │
│  │   ... ]                                  │               │
│  └──────────────────────────────────────────┘               │
│                                                              │
│  [1 query executed per template = 8-10 queries total]       │
│                                                              │
└──────────────────────────────────┬───────────────────────────┘
                                   │
                                   ▼
┌────────────────────────────────────────────────────────────────┐
│ 3. RENDER PHASE                                                │
│                                                                │
│  For each (query_result, template) pair:                      │
│                                                                │
│  Template (Tera):                                             │
│  ┌─────────────────────────────────┐                          │
│  │ pub mod {{ moduleName }} {       │                          │
│  │   pub use super::*;             │                          │
│  │   {% for export in exports %}   │                          │
│  │   pub use self::{{ export }};   │                          │
│  │   {% endfor %}                  │                          │
│  │ }                               │                          │
│  └─────────────────────────────────┘                          │
│             │                                                 │
│    Tera engine substitutes variables:                        │
│    moduleName = "graph"                                      │
│    exports = ["Graph", "Query", ...]                         │
│             │                                                 │
│             ▼                                                 │
│  ┌─────────────────────────────────┐                          │
│  │ pub mod graph {                 │                          │
│  │   pub use super::*;             │                          │
│  │   pub use self::Graph;          │                          │
│  │   pub use self::Query;          │                          │
│  │   ...                           │                          │
│  │ }                               │                          │
│  └─────────────────────────────────┘                          │
│                                                               │
│  [Rendered for: src/graph/mod.rs]                            │
│  [10+ crates × multiple modules = 100+ files generated]      │
│                                                              │
└──────────────────────────────────┬───────────────────────────┘
                                   │
                                   ▼
┌────────────────────────────────────────────────────────────────┐
│ 4. MERGE PHASE (if regenerating)                              │
│                                                                │
│  For each output file:                                        │
│                                                                │
│  ┌──────────────────┐                                         │
│  │ generated_old    │ (from v3.0.0)                           │
│  └──────────────────┘                                         │
│          │                                                    │
│  ┌──────────┴──────────┐                                      │
│  │  Three-way merge    │                                      │
│  ├─ generated_new      │ (from updated ontology)             │
│  └────────────┬────────┘                                      │
│  ┌──────────┴──────────┐                                      │
│  │  manual_edits       │ (hand-written by user)              │
│  └──────────┬──────────┘                                      │
│             │                                                 │
│      Conflict detection:                                      │
│      • If user edited same lines → ask user                 │
│      • If no overlap → auto-merge                           │
│             │                                                 │
│             ▼                                                 │
│  ┌─────────────────────────────────┐                          │
│  │ merged_output                   │                          │
│  │ (new generated + user edits)    │                          │
│  └─────────────────────────────────┘                          │
│                                                               │
└──────────────────────────────────┬───────────────────────────┘
                                   │
                                   ▼
┌────────────────────────────────────────────────────────────────┐
│ 5. VALIDATE PHASE                                              │
│                                                                │
│  [ cargo check ]  → Does it compile?                         │
│  [ cargo test ]   → Do tests pass?                           │
│  [ ggen guards ]  → Do guards pass?                          │
│  [ formatting ]   → Code style OK?                           │
│                                                               │
│  ✅ If all pass → Output ready                               │
│  ❌ If fails → Report error + rollback                       │
│                                                               │
└──────────────────────────────────┬───────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 6. OUTPUT                                                    │
│                                                              │
│  crates/                                                    │
│  ├── ggen-core/src/                                        │
│  │   ├── graph/mod.rs       [generated]                    │
│  │   ├── lifecycle/mod.rs   [generated]                    │
│  │   └── ...                                               │
│  ├── ggen-crates/ggen-cli/src/                                         │
│  │   ├── commands/mod.rs    [generated]                    │
│  │   └── ...                                               │
│  ├── ggen-marketplace/src/                                 │
│  │   ├── guards.rs          [generated]                    │
│  │   └── ...                                               │
│  └── ...                                                    │
│                                                             │
│  ✅ Deterministic: Same ontology → Byte-identical output   │
│                                                             │
└──────────────────────────────────────────────────────────────┘
```

---

## Projection Families (μ_* Architecture)

How different concerns are projected from one ontology:

```
┌──────────────────────────────────────────────────────────────────────────┐
│                     ggen_v3_core.ttl (Single Ontology)                   │
│  • Crates, modules, visibility                                          │
│  • Domain types (Crate, Module, Type, Field, etc.)                      │
│  • CLI commands (noun-verb pairs)                                       │
│  • Guards and validation rules                                          │
│  • Test patterns and lifecycle phases                                   │
│  • Deployment targets and container config                             │
│                                                                          │
└──────────────────────────────────────────────────────────────────────────┘
                                  │
                    ┌─────────────┼─────────────┬────────────┬──────────┐
                    │             │             │            │          │
                    ▼             ▼             ▼            ▼          ▼
         ┌─────────────────┐┌──────────────┐┌─────────┐┌──────────┐┌──────────┐
         │   π_core        ││  π_domain    ││ π_cli   ││ π_market ││  π_ai    │
         │                 ││              ││         ││ place    ││          │
         │ Module          ││ Type defs    ││ Command ││ Validation
         │ scaffolding     ││ Traits       ││ parsing ││ Guards   ││ LLM      │
         │ Cargo.toml      ││ Serialization││ help    ││ Scoring  ││ Clients  │
         │ Visibility      ││ impl blocks  ││ tests   ││ Receipts ││ Streaming│
         │ Dependencies    ││              ││         ││          ││          │
         │                 ││              ││         ││          ││          │
         └─────────────────┘└──────────────┘└─────────┘└──────────┘└──────────┘
                    │             │             │            │          │
                    └─────────────┼─────────────┴────────────┴──────────┘
                                  │
              ┌───────────────────┼───────────────────┐
              │                   │                   │
              ▼                   ▼                   ▼
    ┌──────────────────┐┌──────────────────┐┌─────────────────────┐
    │   π_tests        ││  π_docs          ││  π_deployment       │
    │                  ││                  ││                     │
    │ Unit tests       ││ API docs         ││ Docker files        │
    │ Integration tests││ Architecture     ││ K8s manifests       │
    │ Property tests   ││ Examples         ││ CI/CD workflows     │
    │ E2E tests        ││ Migration guides ││ Release checklists  │
    │ BDD scenarios    ││                  ││                     │
    │                  ││                  ││                     │
    └──────────────────┘└──────────────────┘└─────────────────────┘
                    │             │             │
                    └─────────────┼─────────────┘
                                  │
                                  ▼
                    ┌──────────────────────────────┐
                    │   Generated Artifacts        │
                    │                              │
                    │  crates/*/src/  (80% gen'd) │
                    │  tests/         (100% gen'd)│
                    │  docs/          (100% gen'd)│
                    │  docker/        (100% gen'd)│
                    │  .github/workflows/ (gen'd) │
                    │                              │
                    └──────────────────────────────┘
```

---

## Evolution & Feedback Loop (Autonomous System)

How v3 improves itself:

```
┌─────────────────────────────────────────────────────────────┐
│                                                             │
│            Autonomous Ontology Evolution (Σ)                │
│          "ggen becoming smarter about itself"              │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  1. OBSERVE                                          │  │
│  │     • Monitor generated code in production           │  │
│  │     • Collect telemetry (build times, test times)   │  │
│  │     • Track user feedback & issues                  │  │
│  │     • Analyze test coverage & failure patterns      │  │
│  │  └──────────────────────────────────────────────────┘  │
│       │                                                    │
│       ▼                                                    │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  2. DETECT                                           │  │
│  │     • Pattern mining: "Most CLI commands use X"     │  │
│  │     • Anomaly detection: "This type is unusual"     │  │
│  │     • Bottleneck analysis: "Module T is slow"       │  │
│  │     • Trend identification: "Need new projection"   │  │
│  │  └──────────────────────────────────────────────────┘  │
│       │                                                    │
│       ▼                                                    │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  3. PROPOSE (via LLM)                                │  │
│  │     • "Suggests moving X module to Y location"       │  │
│  │     • "Recommends new CLI command for Z task"        │  │
│  │     • "Proposes better API design pattern"           │  │
│  │     • Output: Ontology edits (RDF triples)           │  │
│  │  └──────────────────────────────────────────────────┘  │
│       │                                                    │
│       ▼                                                    │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  4. VALIDATE (Multi-layer)                           │  │
│  │     • Static: Ontology is valid RDF ✓               │  │
│  │     • Static: All SPARQL queries work ✓              │  │
│  │     • Dynamic: Generated code compiles ✓             │  │
│  │     • Dynamic: All tests pass ✓                      │  │
│  │     • Performance: Build time < threshold ✓          │  │
│  │     • Hard constraints: No unsafe code ✓             │  │
│  │  └──────────────────────────────────────────────────┘  │
│       │  If all pass                                      │
│       ▼                                                    │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  5. PROMOTE (Atomic)                                 │  │
│  │     • Update ggen_v3_core.ttl                        │  │
│  │     • Tag with version (v3.0.1, v3.1.0)             │  │
│  │     • Generate new codebase                          │  │
│  │     • Publish as new release                         │  │
│  │  └──────────────────────────────────────────────────┘  │
│       │                                                    │
│       ▼                                                    │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  6. RECORD (Cryptographic Receipt)                   │  │
│  │     {                                                 │  │
│  │       version: "v3.0.1",                              │  │
│  │       timestamp: "2025-11-17T14:30Z",                 │  │
│  │       proposed_changes: [...],                        │  │
│  │       validation_results: "ALL_PASSED",                │  │
│  │       signature: "ML-DSA signed by ggen key",         │  │
│  │       output_hash: "blake3(...)",                     │  │
│  │     }                                                  │  │
│  │  └──────────────────────────────────────────────────┘  │
│       │                                                    │
│       └─────────────────────────────────────────────────┐  │
│                                                         │  │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Loop Repeats (every 24h or on-demand)               │  │
│  │  ggen improved itself, users get better tools        │  │
│  │  All tracked, all signed, all auditable              │  │
│  │  └──────────────────────────────────────────────────┘  │
│                                                          │
└─────────────────────────────────────────────────────────┘
```

---

## Summary

ggen v3's architecture is built around:

1. **Ontology as Source**: `ggen_v3_core.ttl` describes the entire system
2. **SPARQL-Driven Code**: 8-10 projection families (π_*) query the ontology
3. **Deterministic Output**: Same ontology → byte-identical code every time
4. **Self-Hosting**: ggen generates itself, proving the model works
5. **Autonomous Evolution**: Observes → Detects → Proposes → Validates → Promotes
6. **Credible & Auditable**: Every evolution signed, every change tracked

This architecture eliminates ggen's own dark matter while proving to users that the projection model is production-grade.
