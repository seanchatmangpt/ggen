<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [RDF / SHACL / SPARQL Integration](#rdf--shacl--sparql-integration)
  - [RDF Data Sources](#rdf-data-sources)
    - [Supported Formats](#supported-formats)
    - [Data Graph Structure](#data-graph-structure)
  - [SHACL Validation](#shacl-validation)
    - [Supported SHACL Features](#supported-shacl-features)
    - [SHACL Shapes Example](#shacl-shapes-example)
  - [SPARQL Query Language](#sparql-query-language)
    - [Query Types](#query-types)
    - [Template Variable Queries](#template-variable-queries)
    - [Matrix Projection Queries](#matrix-projection-queries)
  - [Deterministic Processing](#deterministic-processing)
    - [Hash-Based Validation](#hash-based-validation)
    - [Processing Guarantees](#processing-guarantees)
  - [Integration Benefits](#integration-benefits)
    - [Type Safety](#type-safety)
    - [Knowledge Representation](#knowledge-representation)
    - [Tooling Integration](#tooling-integration)
  - [Best Practices](#best-practices)
    - [RDF Modeling](#rdf-modeling)
    - [SHACL Design](#shacl-design)
    - [SPARQL Optimization](#sparql-optimization)
  - [Examples](#examples)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# RDF / SHACL / SPARQL Integration

ggen uses semantic web technologies as its foundation for knowledge representation and data validation. This integration ensures type safety, data quality, and deterministic processing across all projections.

## RDF Data Sources

ggen supports multiple RDF serialization formats:

### Supported Formats
- **Turtle (.ttl)**: Human-readable RDF format with prefix support
- **N-Triples (.nt)**: Line-based plain RDF format
- **JSON-LD (.jsonld)**: JSON-based RDF representation with context

### Data Graph Structure
```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:person1 a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:age 28 ;
    ex:worksAt ex:company1 .

ex:company1 a ex:Company ;
    ex:name "TechCorp" ;
    ex:industry "Software" .
```

## SHACL Validation

ggen implements a core subset of SHACL (Shapes Constraint Language) for data validation:

### Supported SHACL Features
- **NodeShape**: Define validation rules for RDF nodes
- **PropertyShape**: Validate properties of nodes
- **Constraints**:
  - `minCount` / `maxCount`: Cardinality constraints
  - `datatype`: Data type validation (string, integer, date, etc.)
  - `class`: Class membership validation
  - `pattern`: Regular expression pattern matching

### SHACL Shapes Example
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .

ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
        sh:path ex:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:pattern "^[A-Za-z ]+$"
    ] ;
    sh:property [
        sh:path ex:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150
    ] .
```

## SPARQL Query Language

ggen uses SPARQL for data extraction and transformation:

### Query Types
- **SELECT queries**: Extract data for template variables
- **Matrix queries**: Generate projection matrices (must include ORDER BY)

### Template Variable Queries
```sparql
# Extract person names for template variables
PREFIX ex: <http://example.org/>
SELECT ?name WHERE {
    ?person a ex:Person ;
            ex:name ?name .
} ORDER BY ?name
```

### Matrix Projection Queries
```sparql
# Generate matrix for person-to-company relationships
PREFIX ex: <http://example.org/>
SELECT ?person ?company WHERE {
    ?person ex:worksAt ?company .
} ORDER BY ?person ?company
```

## Deterministic Processing

### Hash-Based Validation
ggen ensures deterministic processing by:

1. **Graph Normalization**: RDF graphs are normalized before processing
2. **Hash Calculation**: Merged data and shapes graphs are hashed
3. **Validation**: Hash comparison ensures graph consistency across runs

```rust
// Graph hashing for determinism
let data_hash = calculate_graph_hash(&data_graph);
let shapes_hash = calculate_graph_hash(&shapes_graph);
let combined_hash = combine_hashes(data_hash, shapes_hash);
```

### Processing Guarantees
- **Order Independence**: Processing order doesn't affect results
- **Idempotent Operations**: Multiple runs produce identical outputs
- **Validation Consistency**: SHACL validation results are deterministic

## Integration Benefits

### Type Safety
- **Compile-time validation** of RDF data structures
- **Runtime validation** against SHACL constraints
- **Error reporting** with detailed context and suggestions

### Knowledge Representation
- **Semantic relationships** between entities
- **Extensible schemas** through RDF/OWL ontologies
- **Standards compliance** with W3C semantic web stack

### Tooling Integration
- **SPARQL endpoints** for external data access
- **RDF validation tools** compatibility
- **Ontology editors** integration support

## Best Practices

### RDF Modeling
1. **Use meaningful URIs** for entities and relationships
2. **Define clear namespaces** and prefixes
3. **Model relationships** explicitly with appropriate predicates
4. **Include metadata** about entity creation and modification

### SHACL Design
1. **Start with target classes** for broad validation coverage
2. **Use specific property shapes** for detailed constraints
3. **Include human-readable messages** in validation errors
4. **Test shapes** against sample data before deployment

### SPARQL Optimization
1. **Always include ORDER BY** in matrix queries
2. **Use specific patterns** over broad wildcard matches
3. **Filter early** in query pipelines
4. **Limit results** when appropriate for performance

## Examples

See the `examples/` directory for complete RDF integration examples:
- `graphs/` - Sample RDF data files
- `shapes/` - SHACL constraint definitions
- `queries/` - SPARQL query templates
