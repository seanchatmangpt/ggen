# Code Projections

Semantic projections transform a single RDF ontology into multiple language-specific code representations.

## The Core Concept

```
                   RDF Ontology (Semantic Model)
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                   ↓                   ↓
   Rust Structs      TypeScript Types      Python Classes
```

**Key insight:** The domain model (ontology) is separate from its representation (projection).

## One Ontology, Many Languages

**Traditional approach:**
```
Product.java     → Manually kept in sync with
Product.ts       → Each requires separate updates
Product.py       → Easy to drift out of sync
```

**ggen approach:**
```turtle
# product_catalog.ttl (ONE source of truth)
pc:Product a rdfs:Class .
pc:name rdfs:domain pc:Product ; rdfs:range xsd:string .
pc:price rdfs:domain pc:Product ; rdfs:range xsd:decimal .
```

Generate all languages from one ontology:
```bash
ggen template generate-rdf --ontology product_catalog.ttl --template rust-models
ggen template generate-rdf --ontology product_catalog.ttl --template typescript-types
ggen template generate-rdf --ontology product_catalog.ttl --template python-classes
```

**Result:** Four language-specific implementations, guaranteed to be in sync.

## Type Mapping

ggen maps RDF datatypes to language-specific types:

| XSD Type       | Rust       | TypeScript | Python    |
|----------------|-----------|-----------|----------|
| `xsd:string`   | `String`  | `string`  | `str`    |
| `xsd:integer`  | `i64`     | `number`  | `int`    |
| `xsd:decimal`  | `f64`     | `number`  | `float`  |
| `xsd:boolean`  | `bool`    | `boolean` | `bool`   |
| `xsd:dateTime` | `DateTime` | `Date`  | `datetime` |

## Evolution: Update Once, Regenerate Everywhere

Add a field to the ontology:

```turtle
pc:rating rdfs:domain pc:Product ; rdfs:range xsd:decimal .
```

Regenerate all languages:

```bash
ggen template generate-rdf --ontology product_catalog.ttl --template rust-models
ggen template generate-rdf --ontology product_catalog.ttl --template typescript-types
ggen template generate-rdf --ontology product_catalog.ttl --template python-classes
```

**Result:** All languages now have `rating` field. Zero manual edits.

## How Projections Work

1. **Load ontology** into Oxigraph RDF store
2. **Execute SPARQL query** defined in template frontmatter
3. **Extract variables** from query results
4. **Map types** using template helpers
5. **Render template** with mapped variables
6. **Write output** to specified file path

## Best Practices

1. **Use semantic types in ontology**: `xsd:dateTime` not `xsd:string`
2. **Leverage SPARQL for complex queries**: Extract exactly what templates need
3. **Create custom type mappings**: Domain-specific type conversions
4. **Document projection conventions**: Type mapping tables
5. **Automate regeneration in CI**: Ensure ontology changes propagate

## See Also

- [Ontology-Driven Explanation](ontology-driven.md)
- [Template Reference](../reference/templates.md)
- [Ontology-to-Code Tutorial](../tutorials/ontology-to-code.md)

