# 4. ONTOLOGY LOADING **

*Before you can speak, you must have words. Before you can generate, you must have knowledge.*

---

## Context

You have a **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** that declares what should be generated. But the manifest only points to knowledge—it does not contain it. The actual domain knowledge lives in ontologies: RDF graphs that describe entities, relationships, and constraints.

The generation pipeline needs this knowledge before it can do anything. The knowledge must be loaded, validated, and made queryable.

---

❖ ❖ ❖

**Without loaded knowledge, the pipeline is empty. With improperly loaded knowledge, the pipeline produces garbage.**

The forces:
- Ontologies may span multiple files
- Ontologies may import other ontologies
- Ontologies may have syntax errors
- Ontologies must be loaded before queries can run
- Loading must be fast enough to not block the workflow

Rushing to generate without proper loading leads to:
- Missing entities (imports not followed)
- Query failures (syntax errors in source)
- Silent corruption (partial loads)

**Therefore:**

**Load all ontology sources into a unified graph before any other pipeline operation. Validate the graph is complete and well-formed. Make loading explicit, observable, and fail-fast.**

The loading should:
- Start from the primary source declared in the manifest
- Follow all import declarations
- Parse each file with strict validation
- Merge all sources into a single queryable graph
- Fail immediately if any source cannot be loaded

---

❖ ❖ ❖

## Connections

This pattern is configured by **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** in the `[ontology]` section.

- **[INFERENCE ENRICHMENT](05-inference-enrichment.md)** operates on the loaded graph
- **[GENERATION RULES](06-generation-rules.md)** queries the loaded graph
- **[ERROR SIGNALS](12-error-signals.md)** reports loading failures (exit code 2)
- **[TIMEOUT PROTECTION](11-timeout-protection.md)** may limit loading time

---

## Implementation

### Manifest Configuration

The `[ontology]` section declares what to load:

```toml
[ontology]
# Primary ontology file (required)
source = "ontology/domain.ttl"

# Additional ontology imports (optional)
imports = [
    "ontology/common.ttl",
    "ontology/validation.ttl",
    "ontology/external/schema-org.ttl"
]

# Base IRI for relative URIs (optional)
base_iri = "https://example.org/my-service#"

# Prefix mappings for SPARQL queries (optional)
[ontology.prefixes]
schema = "https://schema.org/"
domain = "https://example.org/domain#"
```

### The Loading Process

```rust
pub fn load_ontology(&mut self) -> Result<()> {
    let graph = Graph::new()?;

    // 1. Load primary ontology source
    let source_path = self.base_path.join(&self.manifest.ontology.source);
    let content = std::fs::read_to_string(&source_path)
        .map_err(|e| Error::new(&format!(
            "Failed to read ontology '{}': {}",
            source_path.display(), e
        )))?;
    graph.insert_turtle(&content)?;

    // 2. Load all imports
    for import in &self.manifest.ontology.imports {
        let import_path = self.base_path.join(import);
        let import_content = std::fs::read_to_string(&import_path)
            .map_err(|e| Error::new(&format!(
                "Failed to read ontology import '{}': {}",
                import_path.display(), e
            )))?;
        graph.insert_turtle(&import_content)?;
    }

    // 3. Store the unified graph
    self.ontology_graph = Some(graph);
    Ok(())
}
```

### Graph Operations

Once loaded, the graph supports:

| Operation | Purpose |
|-----------|---------|
| `query(sparql)` | Execute SELECT/ASK queries |
| `construct(sparql)` | Execute CONSTRUCT queries |
| `insert_turtle(content)` | Add more triples |
| `triple_count()` | Count triples in graph |

### Error Handling

Loading can fail in several ways:

| Failure | Exit Code | Message |
|---------|-----------|---------|
| File not found | 2 | `Failed to read ontology 'path': No such file` |
| Parse error | 2 | `Failed to parse Turtle: line 42: invalid IRI` |
| Invalid UTF-8 | 2 | `Failed to read ontology 'path': invalid utf-8` |

All failures are **fatal**—the pipeline does not proceed with a partial graph.

---

## The Unified Graph

After loading, all sources exist in a single graph:

```
           ┌─────────────────┐
           │  Unified Graph  │
           └────────┬────────┘
                    │
     ┌──────────────┼──────────────┐
     ↓              ↓              ↓
┌─────────┐   ┌─────────┐   ┌─────────┐
│ domain  │   │ common  │   │ schema  │
│  .ttl   │   │  .ttl   │   │  .ttl   │
└─────────┘   └─────────┘   └─────────┘
```

Queries see all triples regardless of which file they came from. This enables:

- Cross-file relationships
- Shared vocabularies
- Modular ontology design

---

## Prefix Handling

The manifest can declare prefixes for convenience:

```toml
[ontology.prefixes]
schema = "https://schema.org/"
domain = "https://example.org/domain#"
```

These prefixes are available in SPARQL queries:

```sparql
PREFIX schema: <https://schema.org/>
PREFIX domain: <https://example.org/domain#>

SELECT ?entity ?name
WHERE {
    ?entity a domain:Entity ;
            schema:name ?name .
}
```

The prefixes are not just shortcuts—they are part of the **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** contract. They document the vocabularies in use.

---

## The Deeper Pattern

ONTOLOGY LOADING is about **establishing the ground of possibility**.

Before loading, the system has no knowledge. After loading, the system has all knowledge. This is a phase transition—from empty to full, from potential to actual.

The loading process is:
- **Explicit**: The manifest declares exactly what is loaded
- **Complete**: All sources are loaded or the operation fails
- **Atomic**: Either the full graph exists or nothing exists
- **Observable**: The caller can see what was loaded

This aligns with the philosophy of **[DETERMINISTIC OUTPUT](13-deterministic-output.md)**: the same inputs produce the same graph, which produces the same outputs.

---

## Ontology Design Implications

ONTOLOGY LOADING shapes how you design ontologies:

**Modularity**: Separate concerns into files that can be imported independently:

```
ontology/
├── domain.ttl       # Domain-specific entities
├── common.ttl       # Shared vocabularies
├── validation.ttl   # SHACL shapes
└── external/
    └── schema.ttl   # External vocabularies
```

**Self-Containment**: Each file should declare its prefixes:

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix domain: <https://example.org/domain#> .

domain:User a rdfs:Class ;
    rdfs:label "User" .
```

**Import Ordering**: Load order matters for inference. Place foundational ontologies first in the imports list.

---

## When This Pattern Breaks

ONTOLOGY LOADING struggles when:

- Ontologies are very large (loading takes too long)
- Ontologies change frequently (reloading is expensive)
- Ontologies come from remote sources (network failures)

ggen addresses these partially:

- Timeout protection limits loading time
- The unified graph is cached for the pipeline duration
- Only local files are supported (no remote fetching)

For very large ontologies, consider:
- Pre-processing into optimized formats
- Incremental loading (not yet implemented)
- Caching loaded graphs across invocations (not yet implemented)

The pattern remains: before generation, the knowledge must be present, complete, and valid.
