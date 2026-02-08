# SPARQL Query Patterns

## Overview

SPARQL is the query language for RDF data. Used in μ₂ (Extract) phase of the Holographic Factory.

## Basic Query Structure

```sparql
PREFIX ex: <http://example.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?variable1 ?variable2
WHERE {
    # Triple patterns
    ?subject ?predicate ?object .
    ?subject ex:property ?variable1 .
    FILTER(?variable1 > 10)
}
ORDER BY ?variable1
LIMIT 100
```

## Common Query Patterns

### Select All Instances of a Class
```sparql
SELECT ?instance ?name
WHERE {
    ?instance a ex:Person .
    ?instance ex:name ?name .
}
```

### Property Paths
```sparql
SELECT ?person ?friend
WHERE {
    ?person ex:knows+ ?friend .  # Transitive (one or more)
}

SELECT ?person ?relative
WHERE {
    ?person ex:familyMember* ?relative .  # Reflexive transitive (zero or more)
}
```

### Optional Properties
```sparql
SELECT ?person ?name ?email
WHERE {
    ?person a ex:Person .
    ?person ex:name ?name .
    OPTIONAL { ?person ex:email ?email . }
}
```

### Filtering
```sparql
SELECT ?person ?age
WHERE {
    ?person a ex:Person .
    ?person ex:age ?age .
    FILTER(?age >= 18 && ?age <= 65)
}
```

### Aggregation
```sparql
SELECT ?department (COUNT(?employee) AS ?count) (AVG(?salary) AS ?avgSalary)
WHERE {
    ?employee ex:worksIn ?department .
    ?employee ex:salary ?salary .
}
GROUP BY ?department
HAVING (COUNT(?employee) > 5)
ORDER BY DESC(?count)
```

### Subqueries
```sparql
SELECT ?person ?avgAge
WHERE {
    ?person a ex:Person .
    {
        SELECT (AVG(?age) AS ?avgAge)
        WHERE {
            ?p a ex:Person .
            ?p ex:age ?age .
        }
    }
}
```

### Union
```sparql
SELECT ?contact ?method
WHERE {
    { ?contact ex:email ?method . }
    UNION
    { ?contact ex:phone ?method . }
}
```

### Construct (Create New Triples)
```sparql
CONSTRUCT {
    ?person ex:canVote true .
}
WHERE {
    ?person a ex:Person .
    ?person ex:age ?age .
    FILTER(?age >= 18)
}
```

### Ask (Boolean Query)
```sparql
ASK {
    ?person a ex:Person .
    ?person ex:age ?age .
    FILTER(?age < 18)
}
```

## Code Generation Patterns

### Extract Class Metadata
```sparql
SELECT ?class ?label ?comment
WHERE {
    ?class a owl:Class .
    OPTIONAL { ?class rdfs:label ?label . }
    OPTIONAL { ?class rdfs:comment ?comment . }
}
```

### Extract Properties for a Class
```sparql
SELECT ?property ?range ?cardinality
WHERE {
    ?property rdfs:domain ex:Person .
    ?property rdfs:range ?range .
    OPTIONAL {
        ?shape sh:targetClass ex:Person ;
               sh:property ?propShape .
        ?propShape sh:path ?property ;
                   sh:minCount ?cardinality .
    }
}
```

### Extract Inheritance Hierarchy
```sparql
SELECT ?class ?superClass
WHERE {
    ?class rdfs:subClassOf ?superClass .
}
ORDER BY ?class
```

### Extract Enum Values
```sparql
SELECT ?enum ?value ?label
WHERE {
    ?enum a owl:Class ;
          owl:oneOf ?list .
    ?list rdf:rest*/rdf:first ?value .
    OPTIONAL { ?value rdfs:label ?label . }
}
```

## Integration with ggen

### Template Context
```rust
// In Rust code extraction
let query = r#"
    PREFIX ex: <http://example.org/>
    SELECT ?class ?property ?range
    WHERE {
        ?class a owl:Class .
        ?property rdfs:domain ?class ;
                  rdfs:range ?range .
    }
"#;

let results = store.query(query)?;
let context = build_template_context(results);
```

### Tera Template Usage
```tera
{# templates/rust/struct.tera #}
{% for class in classes %}
pub struct {{ class.name }} {
    {% for prop in class.properties %}
    pub {{ prop.name }}: {{ prop.rust_type }},
    {% endfor %}
}
{% endfor %}
```

## Performance Tips

✅ **Use LIMIT**: Prevent unbounded results
✅ **Filter Early**: Apply filters before joins
✅ **Index Properties**: Ensure frequently queried properties are indexed
✅ **Avoid OPTIONAL** in hot paths: Slows down queries
✅ **Use Property Paths** wisely: Can be expensive on large graphs

## Debugging Queries

```bash
# Run query against RDF store
ggen query -f query.sparql

# Dry run to see what would be generated
ggen sync --dry_run true

# Check extraction phase output
ggen sync --verbose true
```

## Common Pitfalls

❌ **Cartesian Products**: Missing join conditions
```sparql
# BAD: Creates all combinations
SELECT ?person ?order
WHERE {
    ?person a ex:Person .
    ?order a ex:Order .
}

# GOOD: Explicit join
SELECT ?person ?order
WHERE {
    ?person a ex:Person .
    ?order ex:customer ?person .
}
```

❌ **Unbound Variables**: Variables not constrained
```sparql
# BAD: ?unknown never bound
SELECT ?person ?unknown
WHERE {
    ?person a ex:Person .
}
```

❌ **Missing Prefixes**: Undefined namespace
```sparql
# BAD: No PREFIX declaration
SELECT ?person
WHERE {
    ?person a ex:Person .  # What is 'ex'?
}
```

## Mental Model

```
RDF Store (Triples)
    ↓ SPARQL Query
Results (Bindings)
    ↓ Rust Processing
Template Context (JSON)
    ↓ Tera Rendering
Generated Code
```

**μ₂ (Extract)** = SPARQL queries + OWL inference + rule execution
