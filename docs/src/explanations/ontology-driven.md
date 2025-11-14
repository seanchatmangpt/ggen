# Ontology-Driven Development

ggen uses ontology-driven development: the domain model (RDF ontology) is the single source of truth for all code generation.

## Core Concept

Traditional development:
```
Code (Rust) ← Manually sync → Code (TypeScript) ← Manually sync → Code (Python)
```

Ontology-driven development:
```
RDF Ontology (Single Source of Truth)
    ↓
    ├──→ Rust Code
    ├──→ TypeScript Code
    └──→ Python Code
```

## Why Ontology-Driven?

### Single Source of Truth

The RDF ontology defines your domain model once. All code derives from this model.

**Benefits:**
- No manual synchronization
- Guaranteed consistency across languages
- Change ontology → regenerate all code

### Semantic Precision

RDF uses semantic types (`xsd:string`, `xsd:decimal`) that map to language-specific types automatically.

**Example:**
```turtle
ex:price rdfs:range xsd:decimal .
```

Maps to:
- Rust: `f64`
- TypeScript: `number`
- Python: `float`

### Evolution Without Drift

Add a field to the ontology, regenerate all code:

```turtle
# Add rating field
ex:rating rdfs:domain ex:Product ; rdfs:range xsd:decimal .
```

All generated code now includes `rating` automatically.

## How It Works

### 1. Define Domain in RDF

```turtle
ex:User a rdfs:Class .
ex:userName rdfs:domain ex:User ; rdfs:range xsd:string .
ex:userEmail rdfs:domain ex:User ; rdfs:range xsd:string .
```

### 2. Extract with SPARQL

```sparql
SELECT ?property ?type
WHERE {
    ?property rdfs:domain ex:User .
    ?property rdfs:range ?type .
}
ORDER BY ?property
```

### 3. Project to Code

Templates use SPARQL results to generate code:

```tera
pub struct User {
{% for prop in properties %}
    pub {{ prop.name }}: {{ prop.rust_type }},
{% endfor %}
}
```

### 4. Regenerate on Change

Modify ontology → regenerate → all code updates.

## Benefits

1. **Consistency**: Same model across all languages
2. **Speed**: Generate code in seconds, not hours
3. **Accuracy**: No manual translation errors
4. **Evolution**: Change once, update everywhere
5. **Validation**: SHACL ensures ontology correctness

## Best Practices

1. **Model domain, not code**: Ontology describes business concepts
2. **Use semantic types**: `xsd:dateTime` not `xsd:string`
3. **Validate with SHACL**: Ensure ontology consistency
4. **Version ontologies**: Track changes over time
5. **Document properties**: Use `rdfs:comment` for descriptions

## See Also

- [Projections Explanation](projections.md)
- [RDF/SPARQL Reference](../reference/rdf-sparql.md)
- [Ontology-to-Code Tutorial](../tutorials/ontology-to-code.md)

