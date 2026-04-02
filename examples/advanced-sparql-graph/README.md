# Advanced SPARQL Graph Patterns

This example demonstrates advanced SPARQL query patterns for graph traversal, pattern matching, and knowledge discovery.

> **NEW**: See also [`../sparql-construct-city/`](../sparql-construct-city/) for comprehensive CONSTRUCT examples with Citty testing

## Data

The example uses a social and professional network graph with:
- **People**: Alice, Bob, Charlie, David (with skills and employers)
- **Relationships**: knows, works at, has skill
- **Organizations**: TechCorp, StartupXYZ

## Querying Patterns

### 1. Basic Entity Retrieval
```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?person ?name ?age
WHERE {
  ?person a foaf:Person ;
    foaf:name ?name ;
    foaf:age ?age .
}
```

### 2. Relationship Navigation
```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

SELECT ?person1 ?person2
WHERE {
  ?person1 foaf:knows ?person2 .
  ?person1 ex:worksAt ?company .
  ?person2 ex:worksAt ?company .
}
```

### 3. Skill-Based Matching
```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

SELECT ?person ?skill
WHERE {
  ?person a foaf:Person ;
    ex:skill ?skill .
  FILTER (?skill IN (ex:rust, ex:python))
}
```

### 4. Network Expansion (Transitive Closure)
```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?start ?reachable (COUNT(?hop) AS ?distance)
WHERE {
  ?start foaf:knows+ ?reachable .
}
GROUP BY ?start ?reachable
ORDER BY ?distance
```

### 5. Company Skill Inventory
```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

SELECT ?company (GROUP_CONCAT(DISTINCT ?skill; separator=", ") AS ?skillSet)
WHERE {
  ?person ex:worksAt ?company .
  ?person ex:skill ?skill .
}
GROUP BY ?company
```

## Integration with ggen

Use with ggen to generate validation schemas and type definitions:

```bash
# Step 1: Query for patterns
sparql --query query.sparql --file data/example.ttl > results.json

# Step 2: Generate code from results
ggen sync --template template.tera --input results.json
```

## Advanced Techniques

### Property Paths
Navigate relationships without knowing exact depth:
```sparql
?start foaf:knows* ?anyone       # Zero or more
?start foaf:knows+ ?someone      # One or more
?start foaf:knows? ?person       # Zero or one
```

### Negation
Find unconnected entities:
```sparql
?person a foaf:Person .
FILTER NOT EXISTS {
  ?person foaf:knows ?other .
}
```

### Aggregation
Summarize related data:
```sparql
SELECT ?person (COUNT(DISTINCT ?skill) AS ?skillCount)
       (GROUP_CONCAT(?skill; separator="|") AS ?skills)
WHERE { ... }
GROUP BY ?person
```

## Testing

For comprehensive testing with Citty, see [../sparql-construct-city/](../sparql-construct-city/):

```bash
cd ../sparql-construct-city
npm test
```

This provides:
- ✅ Vitest integration
- ✅ Citty CLI testing framework
- ✅ Snapshot testing
- ✅ Pattern validation
- ✅ Performance benchmarks

## Further Reading

- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/)
- [FOAF Vocabulary](http://xmlns.com/foaf/0.1/)
- [Property Paths in SPARQL](https://www.w3.org/TR/sparql11-query/#propertypaths)
