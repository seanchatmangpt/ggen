# How to Use RDF Ontologies

Guide to working with RDF ontologies, SPARQL queries, and SHACL validation.

## RDF Basics

RDF (Resource Description Framework) represents knowledge as triples: subject-predicate-object.

### Basic RDF Structure

```turtle
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:User a rdfs:Class ;
    rdfs:label "User" .

ex:userName a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "name" .
```

## Creating Ontologies

### Manual Creation

Write RDF/Turtle files directly:

```turtle
@prefix ex: <http://example.org/blog/> .

ex:Post a rdfs:Class ;
    rdfs:label "Post" .

ex:postTitle a rdf:Property ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:string ;
    rdfs:label "title" .
```

### AI Generation

Use AI to generate ontologies:

```bash
ggen ai generate-ontology \
  --prompt "Blog: Post (title, content), Comment (text, author)" \
  --output blog.ttl
```

## SPARQL Queries

SPARQL extracts data from RDF graphs.

### Basic SELECT Query

```sparql
SELECT ?class ?property ?type
WHERE {
    ?class a rdfs:Class .
    ?property rdfs:domain ?class .
    ?property rdfs:range ?type .
}
```

### Query in Templates

```tera
{% query "queries/extract-classes.rq" as classes %}
{% for class in classes %}
  {{ class.name }}
{% endfor %}
```

### Complex Queries

Find all required properties:

```sparql
SELECT ?class ?property
WHERE {
    ?shape sh:targetClass ?class .
    ?shape sh:property [
        sh:path ?property ;
        sh:minCount 1
    ] .
}
```

## SHACL Validation

Validate ontology structure with SHACL:

### SHACL Shapes

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:UserShape a sh:NodeShape ;
    sh:targetClass ex:User ;
    sh:property [
        sh:path ex:userName ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1
    ] .
```

### Validate Ontology

```bash
ggen graph validate domain.ttl --shacl shapes.ttl
```

## Type Mapping

Map RDF types to code types:

| RDF Type | Rust | TypeScript | Python |
|----------|------|------------|--------|
| `xsd:string` | `String` | `string` | `str` |
| `xsd:integer` | `i32` | `number` | `int` |
| `xsd:decimal` | `f64` | `number` | `Decimal` |
| `xsd:boolean` | `bool` | `boolean` | `bool` |
| `xsd:dateTime` | `DateTime<Utc>` | `Date` | `datetime` |

## Best Practices

1. **Use meaningful prefixes:** `ex:` for examples, domain-specific for real projects
2. **Validate with SHACL:** Ensure ontology consistency
3. **Document properties:** Use `rdfs:comment` for descriptions
4. **Version ontologies:** Track changes over time
5. **Reuse vocabularies:** Use standard RDF vocabularies when possible

## Common Patterns

### Inheritance

```turtle
ex:AdminUser a rdfs:Class ;
    rdfs:subClassOf ex:User .
```

### Relationships

```turtle
ex:postAuthor a rdf:Property ;
    rdfs:domain ex:Post ;
    rdfs:range ex:User ;
    rdfs:label "author" .
```

### Constraints

```turtle
ex:UserShape a sh:NodeShape ;
    sh:targetClass ex:User ;
    sh:property [
        sh:path ex:userEmail ;
        sh:pattern "^[^@]+@[^@]+\\.[^@]+$"
    ] .
```

## Next Steps

- **SPARQL reference:** [RDF/SPARQL Reference](../reference/rdf-sparql.md)
- **Ontology concepts:** [Ontology-Driven Explanation](../explanations/ontology-driven.md)
- **Tutorial:** [Ontology-to-Code Tutorial](../tutorials/ontology-to-code.md)

