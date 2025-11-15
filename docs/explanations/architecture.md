# Architecture

**Type: Explanation** | [← Back to Documentation](../README.md)

---

ggen's architecture separates concerns into distinct layers: CLI, domain logic, and infrastructure.

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    ggen Architecture                          │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────┐    ┌──────────────┐    ┌───────────────┐  │
│  │   CLI Layer │───>│ Domain Layer │───>│ Infrastructure│  │
│  │ (Commands)  │    │  (Business)  │    │   (Storage)   │  │
│  └─────────────┘    └──────────────┘    └───────────────┘  │
│         │                   │                     │          │
│         v                   v                     v          │
│  Auto-Discovery      Clean Logic         RDF/Templates      │
│  #[verb] attrs       No CLI deps         Frozen Sections    │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Layer Responsibilities

### CLI Layer (`ggen-cli`)

- Command parsing and argument handling
- User interface and output formatting
- Auto-discovery of commands via `#[verb]` attributes
- Thin wrappers around domain functions

**Key principle:** CLI layer is thin - all business logic lives in domain layer.

### Domain Layer (`ggen-domain`)

- Pure business logic
- No CLI dependencies
- Async by default
- Organized by functional area:
  - `ai` - AI operations
  - `graph` - RDF graph operations
  - `marketplace` - Marketplace operations
  - `template` - Template generation
  - `project` - Project management
  - `hook` - Hook management

**Key principle:** Domain logic is reusable across CLI, web APIs, and other interfaces.

### Infrastructure Layer

- **`ggen-core`**: Core RDF/SPARQL operations, lifecycle management
- **`ggen-ai`**: AI agent system, ontology generation
- **`ggen-marketplace`**: Package registry, search, installation
- **`ggen-utils`**: Shared utilities, error types, configuration

## Data Flow

### Code Generation Flow

```
1. User Command (CLI)
   ↓
2. Domain Function (ggen-domain)
   ↓
3. Load RDF Graph (ggen-core)
   ↓
4. Execute SPARQL Query (ggen-core)
   ↓
5. Render Template (ggen-core)
   ↓
6. Write Output (File System)
```

### Marketplace Flow

```
1. Search Request (CLI)
   ↓
2. Marketplace Client (ggen-domain)
   ↓
3. Registry Query (ggen-marketplace)
   ↓
4. Package Installation (ggen-marketplace)
   ↓
5. Template Available (Local Cache)
```

## Component Interactions

### RDF Processing

- **Oxigraph**: In-memory RDF triple store
- **SPARQL**: Query language for data extraction
- **SHACL**: Validation shapes

### Template System

- **Tera**: Template rendering engine
- **Frontmatter**: YAML metadata in templates
- **SPARQL Integration**: Queries embedded in templates

### Lifecycle Management

- **Phases**: init, setup, build, test, deploy
- **Hooks**: Before/after phase execution
- **State**: File-based persistence

## Design Principles

1. **Separation of Concerns**: CLI, domain, and infrastructure are distinct
2. **Zero CLI Dependencies**: Domain layer has no CLI dependencies
3. **Async by Default**: Non-blocking operations throughout
4. **Type Safety**: Rust's type system enforces correctness
5. **Determinism**: Same inputs → identical outputs

## See Also

- [Ontology-Driven Explanation](ontology-driven.md)
- [Projections Explanation](projections.md)
- [Determinism Explanation](determinism.md)

