# ggen-living-docs

> Living Documentation Ecosystem with Semantic Ontology, Automated Narrative Generation, and Interactive Storytelling

A comprehensive living documentation system that creates a **symbiotic relationship** between code and documentation through semantic understanding, automated narrative generation, and bidirectional synchronization.

## Features

### 🧬 Semantic Ontology System
- **RDF Knowledge Graph**: Code structure represented as semantic triples
- **SPARQL Queries**: Powerful querying capabilities for code relationships
- **Change Detection**: Track code evolution through ontology snapshots
- **Dependency Analysis**: Visual dependency graphs and impact analysis

### ✍️ Automated Narrative Generation
- **Template-Based Generation**: Tera templates for consistent documentation
- **Context-Aware Descriptions**: Intelligent understanding of code purpose
- **Change Summaries**: Automatic changelog and migration guide generation
- **Multiple Styles**: Technical, conversational, tutorial, or reference styles

### 🎭 Interactive Storytelling Interface
- **Web-Based Portal**: Interactive documentation exploration
- **Visual Graph Explorer**: Navigate code relationships visually
- **Real-Time Updates**: WebSocket-based live documentation updates
- **Semantic Search**: Natural language search across codebase

### 🧠 NLU Bidirectional Sync
- **Natural Language Updates**: Update documentation using plain English
- **Intent Understanding**: Parse and execute documentation commands
- **Query Interface**: Ask questions about your codebase in natural language
- **Validation**: Ensure documentation stays in sync with code

## Quick Start

### Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
ggen-living-docs = "0.1.0"
```

### Basic Usage

```rust
use ggen_living_docs::{LivingDocSystem, Config};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize the system
    let config = Config::default();
    let mut system = LivingDocSystem::new(config)?;

    // Extract code ontology
    system.extract_ontology("./src").await?;

    // Generate narratives
    let narratives = system.generate_narratives().await?;
    println!("Generated {} narratives", narratives.len());

    // Start interactive server
    system.serve("127.0.0.1:8080").await?;

    Ok(())
}
```

### CLI Usage

```bash
# Extract code ontology from source directory
ggen docs extract ./src

# Generate documentation narratives
ggen docs narrate

# Start interactive documentation server
ggen docs serve --port 8080

# Query documentation using natural language
ggen docs query "show all functions that use async"

# Update documentation using natural language
ggen docs sync "update execute_sparql documentation: Executes SPARQL queries with caching"

# Validate documentation completeness
ggen docs validate

# Install git hooks for automatic documentation evolution
ggen docs hooks install
```

## Architecture

```
┌──────────────────────────────────────────────────────────────────────┐
│                    Living Documentation Ecosystem                     │
├──────────────────────────────────────────────────────────────────────┤
│                                                                        │
│  ┌─────────────────┐      ┌──────────────────┐      ┌─────────────┐ │
│  │  Code Analysis  │─────>│  Semantic        │─────>│  Narrative  │ │
│  │  & Extraction   │      │  Ontology        │      │  Generator  │ │
│  │  (AST Parser)   │      │  (RDF Graph)     │      │  (Tera)     │ │
│  └─────────────────┘      └──────────────────┘      └─────────────┘ │
│         │                         │                         │         │
│         │                         │                         │         │
│         v                         v                         v         │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │              Interactive Storytelling Interface                  │ │
│  │  - Visual graph explorer (D3.js)                                │ │
│  │  - Real-time code-to-docs sync (WebSocket)                      │ │
│  │  - Natural language query engine                                │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                 │                                     │
│                                 v                                     │
│                    ┌──────────────────────┐                          │
│                    │  NLU Bidirectional   │                          │
│                    │  Sync Engine         │                          │
│                    │  (Docs ↔ Schemas)    │                          │
│                    └──────────────────────┘                          │
│                                                                        │
└──────────────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Semantic Ontology (`ontology`)

The ontology module manages an RDF knowledge graph representation of your codebase:

```rust
use ggen_living_docs::ontology::{CodeOntology, EntityKind};

let config = OntologyConfig::default();
let ontology = CodeOntology::new(&config)?;

// Get all functions
let functions = ontology.get_entities_by_kind(EntityKind::Function).await?;

// Query using SPARQL
let query = r#"
    SELECT ?function ?doc
    WHERE {
        ?function rdf:type code:Function .
        ?function code:documentation ?doc .
    }
"#;
let results = ontology.query(query).await?;
```

**RDF Schema Example:**

```turtle
@prefix code: <http://ggen.dev/ontology/code#> .
@prefix doc: <http://ggen.dev/ontology/docs#> .

code:execute_sparql a code:Function ;
    code:name "execute_sparql" ;
    code:filePath "src/graph/mod.rs" ;
    code:documentation "Executes SPARQL query with caching" ;
    code:async "true" ;
    doc:example code:example_basic_query .
```

### 2. Code Extractor (`extractor`)

Extracts code structure using Rust AST parsing:

```rust
use ggen_living_docs::extractor::CodeExtractor;

let config = ExtractorConfig::default();
let extractor = CodeExtractor::new(&config)?;

// Extract from directory
let entities = extractor.extract_from_directory("./src").await?;

// Extract from single file
let entities = extractor.extract_from_file("src/lib.rs").await?;
```

**Extracted Entities:**
- Modules
- Functions (with async, unsafe markers)
- Structs (with fields)
- Enums (with variants)
- Traits (with methods)
- Implementations
- Constants and statics
- Type aliases
- Macros

### 3. Narrative Generator (`narrative`)

Generates human-readable documentation from ontology:

```rust
use ggen_living_docs::narrative::NarrativeGenerator;

let config = NarrativeConfig::default();
let generator = NarrativeGenerator::new(&config)?;

// Generate function narrative
let narrative = generator.generate_function_narrative(
    "execute_sparql",
    Some("Executes SPARQL queries"),
    &vec!["query: &str".to_string()],
    Some("QueryResult")
)?;
```

**Narrative Templates:**
- Function documentation
- Struct documentation
- Module overviews
- API changelogs
- Migration guides
- Architecture diagrams

### 4. Interactive Interface (`interface`)

Web-based interactive documentation portal:

```rust
use ggen_living_docs::interface::InteractiveServer;

let config = InterfaceConfig::default();
let server = InteractiveServer::new(config, ontology)?;

// Start server
server.start("127.0.0.1:8080").await?;
```

**API Endpoints:**
- `POST /api/query` - Execute SPARQL queries
- `GET /api/entities` - List all entities
- `GET /api/graph` - Get dependency graph
- `GET /api/search?q=term` - Search documentation
- `WS /ws` - WebSocket for real-time updates

### 5. NLU Engine (`nlu`)

Natural language understanding for bidirectional sync:

```rust
use ggen_living_docs::nlu::NluEngine;

let config = NluConfig::default();
let engine = NluEngine::new(&config)?;

// Parse natural language updates
let updates = engine.parse_to_semantic_updates(
    "update execute_sparql documentation: Executes SPARQL with caching"
).await?;

// Query using natural language
let response = engine.query_ontology(
    &ontology,
    "show all async functions"
).await?;
```

**Natural Language Commands:**
- `update <entity> documentation: <text>` - Update documentation
- `<entity> calls <entity>` - Add relationship
- `<entity> implements <trait>` - Add implementation
- `show/find/list <entity type>` - Query entities
- `search <term>` - Full-text search

## Git Hooks Integration

Install git hooks for automatic documentation evolution:

```bash
ggen docs hooks install
```

**Installed Hooks:**

### pre-commit
```bash
# Extract ontology and validate documentation
ggen docs extract
ggen docs validate
```

### post-commit
```bash
# Generate narratives from changes
ggen docs narrate
```

### pre-push
```bash
# Final validation before push
ggen docs validate --strict
```

## Configuration

Configure the system via `Config`:

```rust
use ggen_living_docs::{Config, NarrativeStyle};

let config = Config {
    ontology_config: OntologyConfig {
        store_path: PathBuf::from(".ggen/docs/ontology"),
        base_uri: "http://ggen.dev/ontology/code#".to_string(),
        enable_cache: true,
        ..Default::default()
    },
    narrative_config: NarrativeConfig {
        style: NarrativeStyle::Technical,
        include_examples: true,
        include_diagrams: true,
        ..Default::default()
    },
    ..Default::default()
};
```

## Validation Reports

Get comprehensive documentation validation:

```rust
let report = system.validate().await?;

println!("Total Entities: {}", report.total_entities);
println!("Documented: {}", report.documented_entities);
println!("Coverage: {:.2}%", report.coverage_percentage);
println!("Undocumented: {:?}", report.undocumented_entities);
```

## Advanced Usage

### Custom Templates

```rust
let mut generator = NarrativeGenerator::new(&config)?;

// Add custom template
generator.templates.add_raw_template(
    "my_template",
    "# {{ name }}\n{{ documentation }}"
)?;
```

### Custom SPARQL Queries

```rust
let query = r#"
    PREFIX code: <http://ggen.dev/ontology/code#>

    SELECT ?caller ?callee
    WHERE {
        ?caller code:calls ?callee .
        ?caller code:async "true" .
    }
"#;

let results = ontology.query(query).await?;
```

### Export Ontology

```rust
// Export as RDF Turtle
system.export_ontology("docs/ontology.ttl").await?;
```

## Examples

See the `/examples` directory for:
- Basic usage
- Custom templates
- Advanced queries
- Integration patterns
- CI/CD integration

## Performance

- **Incremental Updates**: Only processes changed files
- **Caching**: RDF query results cached
- **Parallel Processing**: Multi-threaded file analysis
- **Lazy Loading**: On-demand narrative generation

## Testing

```bash
cargo test -p ggen-living-docs
```

## Contributing

Contributions welcome! Please read CONTRIBUTING.md.

## License

MIT OR Apache-2.0

## Roadmap

- [ ] Support for more languages (TypeScript, Python, Go)
- [ ] Advanced NLU with transformer models
- [ ] Real-time collaboration features
- [ ] Integration with documentation platforms
- [ ] AI-powered documentation suggestions
- [ ] Performance metrics visualization
- [ ] Code-to-diagram generation
- [ ] Documentation quality scoring

## Related Projects

- [ggen](https://github.com/ggenai/ggen) - Knowledge graph-driven code generator
- [mdBook](https://github.com/rust-lang/mdBook) - Markdown book builder
- [rustdoc](https://doc.rust-lang.org/rustdoc/) - Rust documentation generator

---

**Built with ggen** - Treating code as projections of semantic knowledge graphs
