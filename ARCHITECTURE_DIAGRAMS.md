# GGEN Architecture Diagrams

## 1. System Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          USER INTERFACE LAYER                           │
│                      (CLI via clap-noun-verb v3.4)                      │
└─────────────────────────────────────────────────────────────────────────┘
                                      │
                 ┌────────────────────┼────────────────────┐
                 │                    │                    │
                 ▼                    ▼                    ▼
        ┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
        │   DOMAIN LAYER   │ │   DOMAIN LAYER   │ │   DOMAIN LAYER   │
        │  (ggen-domain)   │ │  (ggen-domain)   │ │  (ggen-domain)   │
        │                  │ │                  │ │                  │
        │ - Template       │ │ - Project        │ │ - Graph          │
        │ - AI             │ │ - Marketplace    │ │ - Lifecycle      │
        │ - CI/CD          │ │ - Hooks          │ │ - Audit          │
        └──────────────────┘ └──────────────────┘ └──────────────────┘
                 │                    │                    │
                 └────────────────────┼────────────────────┘
                                      │
┌─────────────────────────────────────────────────────────────────────────┐
│                         CORE ENGINE LAYER (ggen-core)                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  ┌─────────────────┐  ┌──────────────────┐  ┌────────────────────────┐  │
│  │   RDF MODULE    │  │  TEMPLATE MODULE │  │   GRAPH MODULE         │  │
│  │                 │  │                  │  │                        │  │
│  │ - GgenOntology  │  │ - Template       │  │ - Graph (Oxigraph)    │  │
│  │ - Metadata      │  │ - Frontmatter    │  │ - Query Caching       │  │
│  │ - Validation    │  │ - Tera Filters   │  │ - SPARQL Execution    │  │
│  │ - schema.ttl    │  │ - YAML Parsing   │  │ - Result Caching      │  │
│  └─────────────────┘  └──────────────────┘  └────────────────────────┘  │
│                                                                           │
│  ┌──────────────────────────┐  ┌────────────────────────────────────┐   │
│  │  GENERATION ENGINE       │  │  PROCESS MANAGEMENT                │   │
│  │                          │  │                                    │   │
│  │ - Generator              │  │ - Lifecycle (production readiness) │   │
│  │ - Pipeline               │  │ - Delta (change detection)         │   │
│  │ - GenContext             │  │ - Merge (3-way merging)            │   │
│  │ - Preprocessor           │  │ - Registry (package mgmt)          │   │
│  │ - FileTreeGenerator      │  │ - Cache (template caching)         │   │
│  └──────────────────────────┘  └────────────────────────────────────┘   │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
                                      │
                 ┌────────────────────┼────────────────────┐
                 │                    │                    │
                 ▼                    ▼                    ▼
        ┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
        │  INFRASTRUCTURE  │ │  INFRASTRUCTURE  │ │  INFRASTRUCTURE  │
        │                  │ │                  │ │                  │
        │ - ggen-ai        │ │ - ggen-marketplace
        │ - ggen-utils     │ │ - ggen-node (Node.js)
        │ - ggen-marketplace           │
        └──────────────────┘ └──────────────────┘ └──────────────────┘
```

## 2. Template Generation Pipeline

```
┌──────────────────────────────────────────────────────────────────────────┐
│                      TEMPLATE GENERATION PIPELINE                        │
└──────────────────────────────────────────────────────────────────────────┘

INPUT: Template File (*.tmpl)
  ↓
┌──────────────────────────────────────────────────────────────────────────┐
│ STAGE 1: TEMPLATE PARSING                                                │
│                                                                          │
│ - Read template file                                                    │
│ - Extract YAML frontmatter (delimiter: ---)                             │
│ - Extract template body (Tera syntax)                                   │
│ - Create Template struct { front: Frontmatter, body: String }           │
└──────────────────────────────────────────────────────────────────────────┘
  ↓
┌──────────────────────────────────────────────────────────────────────────┐
│ STAGE 2: FRONTMATTER RENDERING                                           │
│                                                                          │
│ - Render frontmatter YAML with {{ variables }} (Phase 1)                │
│ - Resolve output path: to: "generated/{{ name | snake }}.rs"            │
│ - Extract RDF prefixes and base IRI                                     │
│ - Build Tera context with variables                                     │
└──────────────────────────────────────────────────────────────────────────┘
  ↓
┌──────────────────────────────────────────────────────────────────────────┐
│ STAGE 3: RDF GRAPH PROCESSING                                            │
│                                                                          │
│ a) Load RDF data                                                        │
│    - Load from files (rdf: [...files...])                               │
│    - Load inline RDF (rdf_inline: [...triples...])                      │
│    - Register prefixes and base IRI                                     │
│    → Graph populated with triples                                       │
│                                                                          │
│ b) Execute SPARQL Queries                                               │
│    - For each query in sparql: {...}                                    │
│    - Execute SELECT query against graph                                 │
│    - Convert results to JSON (columns as keys)                          │
│    - Store in context: sparql_results.{query_name}                      │
└──────────────────────────────────────────────────────────────────────────┘
  ↓
┌──────────────────────────────────────────────────────────────────────────┐
│ STAGE 4: BODY RENDERING (Phase 2)                                        │
│                                                                          │
│ - Combine context: {{ variables }} + {{ sparql_results.* }}             │
│ - Render template body with Tera                                        │
│ - Apply custom filters (snake_case, pascal_case, etc.)                  │
│ - Apply functions (sparql_first, sparql_values, etc.)                   │
│ - Output: Rendered content (code, config, etc.)                         │
└──────────────────────────────────────────────────────────────────────────┘
  ↓
┌──────────────────────────────────────────────────────────────────────────┐
│ STAGE 5: FILE OPERATION                                                  │
│                                                                          │
│ - Resolve output path (apply prefixes/base for variable expansion)      │
│ - Handle file injection (inject: true, before/after markers)            │
│ - Three-way merge (if merging with existing file)                       │
│ - Create directories as needed                                          │
│ - Write file (or dry-run preview)                                       │
│ - Optional backup original                                              │
└──────────────────────────────────────────────────────────────────────────┘
  ↓
OUTPUT: Generated File (with code/config/docs)
```

## 3. RDF & Ontology Integration

```
┌──────────────────────────────────────────────────────────────────────────┐
│                    RDF ONTOLOGY & SEMANTIC PROJECTION                    │
└──────────────────────────────────────────────────────────────────────────┘

DOMAIN ONTOLOGY (RDF/Turtle)
┌──────────────────────────────────────┐
│ @prefix ex: <http://example.org/>   │
│                                      │
│ ex:Product a ex:Entity ;            │
│   ex:hasProperty ex:name ;           │
│   ex:hasProperty ex:price ;          │
│   ex:hasProperty ex:description .    │
│                                      │
│ ex:name a ex:Property ;              │
│   ex:type xsd:string ;               │
│   ex:required true .                 │
│                                      │
│ ex:price a ex:Property ;             │
│   ex:type xsd:decimal ;              │
│   ex:validation ">= 0.01" .          │
└──────────────────────────────────────┘
                    │
                    ▼
        ┌───────────────────────┐
        │  SPARQL QUERIES       │
        │                       │
        │ Q1: Find all entities │
        │ Q2: Find properties   │
        │ Q3: Find relationships
        │ Q4: Validate schema   │
        └───────────────────────┘
                    │
        ┌───────────┴───────────┬────────────────────┐
        │                       │                    │
        ▼                       ▼                    ▼
┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐
│  RUST TEMPLATE   │  │  TYPESCRIPT      │  │  PYTHON TEMPLATE │
│                  │  │  TEMPLATE        │  │                  │
│ pub struct      │  │ export interface │  │ @dataclass       │
│ Product {       │  │ Product {        │  │ class Product:   │
│   pub name:    │  │   name: string;  │  │   name: str      │
│   String,       │  │   price: number; │  │   price: Decimal │
│   pub price:   │  │   description?:  │  │   description: str
│   f64,          │  │   string;        │  │                  │
│   pub ...      │  │ }                │  │ }                │
│ }               │  │                  │  │                  │
└──────────────────┘  └──────────────────┘  └──────────────────┘
        │                       │                    │
        └───────────┬───────────┴────────────────────┘
                    │
                    ▼
        POLYGLOT SYNC (ZERO DRIFT)
        ┌──────────────────────┐
        │ Update ontology:     │
        │ price: "minInclusive │
        │        1.00"         │
        └──────────────────────┘
                    │
        ┌───────────┴───────────┬────────────────────┐
        │                       │                    │
        ▼                       ▼                    ▼
    All 3 languages AUTO-UPDATED with validation!
```

## 4. Crate Dependencies

```
┌────────────────────┐
│   ggen-cli         │  (CLI interface with clap-noun-verb)
└────────┬───────────┘
         │
         ▼
┌────────────────────────────────────────────────────────┐
│         ggen-domain (Pure business logic)              │
│  - No CLI dependencies                                 │
│  - Async by default                                    │
│  - Uses ggen-core, ggen-ai, ggen-marketplace           │
└────────┬───────────────────────────────────────────────┘
         │
         ├──────────────────┬──────────────────┬─────────────────┐
         │                  │                  │                 │
         ▼                  ▼                  ▼                 ▼
┌──────────────────┐┌──────────────────┐┌──────────────────┐┌───────────────┐
│   ggen-core      ││   ggen-ai        ││ggen-marketplace  ││  ggen-utils   │
│                  ││                  ││                  ││               │
│ - RDF/SPARQL    ││ - GPT            ││ - Registry        ││ - Error types │
│ - Templates     ││ - Claude         ││ - Package mgmt    ││ - Utilities   │
│ - Generator     ││ - Ollama         ││ - Distribution    ││ - Validation  │
│ - Graph         ││                  ││                  ││               │
│ - Pipeline      ││                  ││                  ││               │
│ - Lifecycle     ││                  ││                  ││               │
└──────────────────┘└──────────────────┘└──────────────────┘└───────────────┘
         │
         └──────────────────────────────────────────────┐
                                                        │
                                                        ▼
                                            ┌──────────────────────┐
                                            │   External Libraries │
                                            │                      │
                                            │ - Oxigraph (RDF)    │
                                            │ - Tera (templating) │
                                            │ - Tokio (async)     │
                                            │ - Serde (serializ.) │
                                            │ - Reqwest (HTTP)    │
                                            └──────────────────────┘
```

## 5. Data Flow: Template → Code Generation

```
USER COMMAND
│
├─ ggen template generate --template my.tmpl --vars name=MyApp
│
▼
┌─────────────────────────────┐
│ ggen-cli (Command Router)   │
│ (clap-noun-verb v3.4)       │
└────────┬────────────────────┘
         │
         ▼
┌─────────────────────────────┐
│ ggen-domain/template/       │
│ generate.rs (Domain Logic)  │
└────────┬────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────┐
│ ggen-core::Generator                            │
│                                                 │
│ 1. GenContext { template_path, vars, ... }     │
│ 2. Pipeline::new() → Tera + Graph              │
│ 3. Template::from_file(path)                   │
│    ↓ Parse YAML + body                         │
│ 4. template.render_frontmatter(&vars)          │
│    ↓ Resolve {{ vars }} in to:/from:/rdf:     │
│ 5. graph.load_rdf(rdf_files)                   │
│    ↓ Load Turtle files into Oxigraph           │
│ 6. Execute SPARQL queries                      │
│    ↓ Store results in sparql_results           │
│ 7. template.render_body(context)               │
│    ↓ Tera renders with vars + SPARQL results  │
│ 8. Handle injection/merging                    │
│    ↓ Write file with proper newlines           │
│ 9. Output file created                         │
└─────────────────────────────────────────────────┘
         │
         ▼
GENERATED CODE FILE (in memory or written)
```

## 6. Lifecycle State Machine

```
┌──────────────────────────────────────────────────────────────────────────┐
│                    ONTOLOGY EVOLUTION LIFECYCLE                          │
└──────────────────────────────────────────────────────────────────────────┘

    DEFINE                    GENERATE                  TEST
┌──────────────┐          ┌────────────┐          ┌──────────┐
│              │          │            │          │          │
│ Create RDF   ├─────────▶│  Generate  ├─────────▶│ Validate │
│ Ontology     │          │ Code from  │          │ Code     │
│              │          │ Templates  │          │          │
└──────────────┘          └────────────┘          └────┬─────┘
                                                       │
                                                       ▼
                                          ┌──────────────────────┐
                                          │     BUILD            │
                                          │                      │
                                          │ Compile/Package      │
                                          │ Code                 │
                                          └────────┬─────────────┘
                                                   │
                                                   ▼
                                          ┌──────────────────────┐
                                          │     DEPLOY           │
                                          │                      │
                                          │ Release to Prod      │
                                          └────────┬─────────────┘
                                                   │
                                                   ▼
                                          ┌──────────────────────┐
                                          │    MONITOR           │
                                          │                      │
                                          │ Track Health/Metrics │
                                          └────────┬─────────────┘
                                                   │
                    ┌──────────────────────────────┘
                    │
                    ▼ (Change Required)
           ┌────────────────────┐
           │     EVOLVE         │
           │                    │
           │ Update Ontology    │
           │ (Add field, class) │
           └────────┬───────────┘
                    │
                    └─────────────┬──────────────┐
                                  │              │
              ┌───────────────────▼┐            │
              │ DELTA DETECTION    │            │
              │                    │            │
              │ - Compare old→new  │            │
              │ - Find changes     │            │
              │ - Impact analysis  │            │
              └────────┬───────────┘            │
                       │                        │
                       ▼                        │
              ┌────────────────────┐            │
              │ SELECTIVE REGEN    │            │
              │                    │            │
              │ Only changed       │            │
              │ templates          │            │
              └────────┬───────────┘            │
                       │                        │
                       └────────────────────────┴──────┐
                                                       │
                                                       ▼ (Back to GENERATE)
                                              ┌──────────────┐
                                              │ Re-generate  │
                                              │ Code         │
                                              └──────────────┘
```

---

## Key Insights

1. **Separation of Concerns**
   - CLI (ggen-cli) routes commands
   - Domain (ggen-domain) implements business logic
   - Core (ggen-core) provides foundational components
   - Each layer can be tested independently

2. **RDF as Central Hub**
   - All ontologies stored as RDF/Turtle
   - SPARQL queries extract semantic structure
   - Templates are projections of RDF data
   - Type mapping ensures polyglot consistency

3. **Two-Phase Rendering**
   - Phase 1: Resolve variables in frontmatter
   - Phase 2: Render body with full context
   - Enables dynamic path generation and RDF loading

4. **Intelligent Caching**
   - Query plans cached in Graph
   - Results cached with epoch-based invalidation
   - Template parsing cached
   - Reduces computational overhead

5. **Deterministic Output**
   - Byte-identical regeneration
   - SPARQL result ordering matters
   - Sorted output ensures consistency
   - Clean git diffs with delta-driven regeneration

