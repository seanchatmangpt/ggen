# Living Documentation Ecosystem Architecture

## Overview

The Living Documentation Ecosystem creates a **symbiotic relationship** between code and documentation through semantic understanding, automated narrative generation, and bidirectional synchronization.

## Architecture

```
┌──────────────────────────────────────────────────────────────────────┐
│                    Living Documentation Ecosystem                     │
├──────────────────────────────────────────────────────────────────────┤
│                                                                        │
│  ┌─────────────────┐      ┌──────────────────┐      ┌─────────────┐ │
│  │  Code Analysis  │─────>│  Semantic        │─────>│  Narrative  │ │
│  │  & Extraction   │      │  Ontology        │      │  Generator  │ │
│  │                 │      │  (RDF Graph)     │      │             │ │
│  └─────────────────┘      └──────────────────┘      └─────────────┘ │
│         │                         │                         │         │
│         │                         │                         │         │
│         v                         v                         v         │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │              Interactive Storytelling Interface                  │ │
│  │  - Visual graph explorer                                        │ │
│  │  - Real-time code-to-docs sync                                  │ │
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

### 1. Semantic Ontology System

Leverages ggen's existing RDF/SPARQL infrastructure to create a **semantic model of the codebase**:

- **Code Entity Ontology**: Functions, structs, traits, modules as RDF triples
- **Relationship Graph**: Calls, dependencies, implements, uses relationships
- **Change Detection**: Diffs between ontology snapshots
- **Semantic Versioning**: Track API evolution semantically

**RDF Schema Example:**
```turtle
@prefix code: <http://ggen.dev/ontology/code#> .
@prefix doc: <http://ggen.dev/ontology/docs#> .

code:GraphModule a code:Module ;
    code:hasFunction code:execute_sparql ;
    code:exports code:GraphStorage ;
    doc:narrative "Manages RDF graph storage and SPARQL queries" .

code:execute_sparql a code:Function ;
    code:hasParameter code:query_param ;
    code:returns code:QueryResult ;
    code:complexity "O(n log n)" ;
    doc:example code:example_basic_query .
```

### 2. Automated Narrative Generation

Generates **human-readable documentation narratives** from ontology changes:

- **Template-based Generation**: Using ggen's Tera template engine
- **Context-aware Descriptions**: Understand code purpose from structure
- **Change Summaries**: "Added async support to GraphStorage"
- **API Impact Analysis**: "This change affects 12 downstream modules"

**Narrative Templates:**
- Function documentation: Purpose, parameters, return values, examples
- Module documentation: Overview, key components, usage patterns
- Change logs: What changed, why, impact, migration guide

### 3. Interactive Storytelling Interface

A **web-based interactive documentation portal**:

- **Visual Graph Explorer**: Navigate code relationships visually
- **Live Code Examples**: Runnable examples embedded in docs
- **Search with Semantic Understanding**: "Show me all async functions"
- **Timeline View**: See how code evolved over time
- **Dependency Visualizer**: Understand module relationships

**Technology Stack:**
- Backend: Actix-web server (Rust)
- Frontend: Web Components + D3.js for graph visualization
- Real-time: WebSocket for live updates
- Storage: Oxigraph RDF store

### 4. NLU Bidirectional Sync

**Natural language updates** that sync back to semantic schemas:

- **Doc Comments → RDF**: Parse doc comments into structured triples
- **Natural Language Queries**: "Update the description of execute_sparql"
- **LLM Integration**: Use ggen-ai for understanding intent
- **Validation**: Ensure doc changes align with code reality

**Sync Flow:**
```
Developer writes: /// Executes SPARQL query with caching support
                ↓
NLU Parser extracts: {
  subject: execute_sparql,
  predicate: hasFeature,
  object: caching
}
                ↓
RDF triple added: code:execute_sparql code:hasFeature code:caching
                ↓
Narrative updated: "This function supports query caching for performance"
```

## Integration Points

### Git Hooks
- **pre-commit**: Extract code ontology, validate doc sync
- **post-commit**: Generate narrative updates
- **pre-push**: Validate documentation completeness

### CI/CD Pipeline
- **docs-analyze**: Extract code structure to RDF
- **docs-generate**: Generate narratives from ontology
- **docs-validate**: Check doc-code alignment
- **docs-deploy**: Publish interactive interface

### CLI Commands
```bash
ggen docs extract          # Extract code ontology to RDF
ggen docs narrate          # Generate documentation narratives
ggen docs serve            # Start interactive documentation server
ggen docs sync             # Bidirectional sync docs ↔ code
ggen docs query "..."      # Natural language documentation query
ggen docs validate         # Validate documentation completeness
```

## Benefits

### For Developers
- Documentation stays synchronized automatically
- Understand code relationships visually
- See real-time impact of changes
- Natural language documentation updates

### For Users
- Always up-to-date documentation
- Interactive exploration of codebase
- Clear migration guides for breaking changes
- Runnable examples with every API

### For Maintainers
- Automated documentation generation
- Track API evolution semantically
- Identify undocumented code automatically
- Quality metrics dashboard

## Implementation Phases

### Phase 1: Foundation (Weeks 1-2)
- Create `ggen-living-docs` crate
- Implement code-to-RDF extractor
- Basic narrative templates
- CLI commands

### Phase 2: Narrative Engine (Weeks 3-4)
- Advanced narrative generation
- Change detection and summarization
- Template library for all code entities
- Integration with existing docs

### Phase 3: Interactive Interface (Weeks 5-6)
- Web server with Actix-web
- Graph visualization frontend
- Search and query interface
- Real-time WebSocket updates

### Phase 4: NLU Sync (Weeks 7-8)
- Natural language parser
- LLM integration for understanding
- Bidirectional sync engine
- Validation and testing

## Technical Considerations

### Performance
- Incremental ontology updates (not full extraction)
- Caching of generated narratives
- Lazy loading in interactive interface
- SPARQL query optimization

### Scalability
- Handle large codebases (>100k LOC)
- Parallel processing of modules
- Distributed RDF storage option
- CDN for static assets

### Reliability
- Validation of extracted ontology
- Rollback mechanism for bad syncs
- Comprehensive test coverage
- Error recovery strategies

## Next Steps

1. Create crate structure for `ggen-living-docs`
2. Implement Rust AST parser for code extraction
3. Define RDF schema for code entities
4. Build basic narrative generator
5. Add CLI integration points
6. Create git hooks
7. Build web interface prototype
8. Integrate NLU capabilities
