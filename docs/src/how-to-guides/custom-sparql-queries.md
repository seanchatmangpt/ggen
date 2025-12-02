# Writing Custom SPARQL Queries for Your Ontology

Learn how to write domain-specific SPARQL queries to extract and analyze your RDF ontologies.

## Introduction to SPARQL for ggen

**SPARQL** (SPARQL Protocol and RDF Query Language) lets you query your ontologies like SQL queries relational databases.

When to use SPARQL:
- ✅ Extract specific entities or relationships
- ✅ Find all classes with certain properties
- ✅ Analyze ontology structure
- ✅ Generate custom reports
- ✅ Create advanced templates

## Basic SPARQL Patterns

### Pattern 1: Find All Classes

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?class WHERE {
  ?class rdf:type rdfs:Class .
}
```

**In ggen**:
```bash
ggen packs sparql \
  --query "SELECT ?class WHERE { ?class rdf:type rdfs:Class }"
```

### Pattern 2: Find Properties of a Specific Class

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ex: <http://example.org/>

SELECT ?property WHERE {
  ?property rdfs:domain ex:User .
}
```

### Pattern 3: Find All Relationships

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?fromClass ?relationship ?toClass WHERE {
  ?relationship rdf:type rdf:Property .
  ?relationship rdfs:domain ?fromClass .
  ?relationship rdfs:range ?toClass .
}
```

## Common SPARQL Queries

### Query 1: Find Required vs Optional Properties

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ex: <http://example.org/>

SELECT ?class ?property (BOUND(?minCard) as ?isRequired) WHERE {
  ?class rdfs:subClassOf ?restriction .
  ?restriction owl:onProperty ?property .
  OPTIONAL {
    ?restriction owl:minCardinality ?minCard .
    FILTER (?minCard > 0)
  }
}
```

### Query 2: Find All Enumerations

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ex: <http://example.org/>

SELECT ?enum ?member WHERE {
  ?enum rdfs:subClassOf ex:Enumeration .
  ?member rdf:type ?enum .
}
ORDER BY ?enum ?member
```

### Query 3: Find Inheritance Hierarchies

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?parent ?child WHERE {
  ?child rdfs:subClassOf ?parent .
  FILTER (?parent != ?child)
}
ORDER BY ?parent ?child
```

### Query 4: Find Classes Without Documentation

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?class WHERE {
  ?class rdf:type rdfs:Class .
  FILTER NOT EXISTS { ?class rdfs:comment ?comment }
}
```

### Query 5: Find All Properties and Their Types

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?property ?domain ?range WHERE {
  ?property rdf:type rdf:Property .
  ?property rdfs:domain ?domain .
  ?property rdfs:range ?range .
}
ORDER BY ?domain ?property
```

## Advanced SPARQL Patterns

### Pattern: Multiple Conditions (AND)

```sparql
SELECT ?class WHERE {
  ?class rdf:type rdfs:Class .
  ?class rdfs:label ?label .
  ?class rdfs:comment ?comment .
  FILTER (STRLEN(?label) > 5)
}
```

### Pattern: Alternatives (OR)

```sparql
SELECT ?entity WHERE {
  {
    ?entity rdf:type rdfs:Class .
  } UNION {
    ?entity rdf:type rdf:Property .
  }
}
```

### Pattern: Counting Aggregates

```sparql
SELECT ?class (COUNT(?property) as ?propertyCount) WHERE {
  ?class rdf:type rdfs:Class .
  ?property rdfs:domain ?class .
}
GROUP BY ?class
HAVING (COUNT(?property) > 5)
```

### Pattern: String Operations

```sparql
SELECT ?class WHERE {
  ?class rdfs:label ?label .
  FILTER (
    CONTAINS(?label, "User") ||
    CONTAINS(?label, "Account")
  )
}
```

### Pattern: Ordering and Limits

```sparql
SELECT ?class ?propertyCount WHERE {
  ?class rdf:type rdfs:Class .
  OPTIONAL {
    SELECT ?class (COUNT(?property) as ?propertyCount) WHERE {
      ?property rdfs:domain ?class .
    }
    GROUP BY ?class
  }
}
ORDER BY DESC(?propertyCount)
LIMIT 10
```

## Using SPARQL in Ggen

### Execute Query

```bash
# Simple query
ggen packs sparql \
  --ontology schema.ttl \
  --query "SELECT ?class WHERE { ?class rdf:type rdfs:Class }"

# Query from file
ggen packs sparql \
  --ontology schema.ttl \
  --query-file my-query.sparql
```

### Generate Report from Query Results

```bash
# Export as JSON
ggen packs sparql \
  --ontology schema.ttl \
  --query-file classes.sparql \
  --output classes.json \
  --format json

# Export as CSV
ggen packs sparql \
  --ontology schema.ttl \
  --query-file properties.sparql \
  --output properties.csv \
  --format csv
```

## Real-World Examples

### Example 1: Generate API from All Entities

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?class ?property WHERE {
  ?class rdf:type rdfs:Class .
  ?property rdfs:domain ?class .
}
ORDER BY ?class ?property
```

Use results to generate REST API:

```bash
ggen packs sparql \
  --ontology ecommerce.ttl \
  --query-file entity-properties.sparql \
  --output api-spec.json

# Then use api-spec.json to generate API server
ggen template generate api-spec.json \
  --template rest-api-generator \
  --output api.ts
```

### Example 2: Find Breaking Changes

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ex: <http://example.org/>

SELECT ?class ?oldProperty WHERE {
  ?class rdf:type rdfs:Class .
  ?oldProperty rdfs:domain ?class .
  ?oldProperty ex:deprecated "true"^^xsd:boolean .
}
```

### Example 3: Generate Documentation Index

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?class (GROUP_CONCAT(?propertyLabel; separator=", ") as ?properties) WHERE {
  ?class rdf:type rdfs:Class .
  ?class rdfs:label ?classLabel .
  ?property rdfs:domain ?class .
  ?property rdfs:label ?propertyLabel .
}
GROUP BY ?class
ORDER BY ?classLabel
```

## Debugging SPARQL Queries

### Check Query Syntax

```bash
# Validate query
ggen packs sparql \
  --ontology schema.ttl \
  --query "SELECT ?class WHERE { ?class rdf:type rdfs:Class }" \
  --validate

# Verbose output
ggen packs sparql \
  --ontology schema.ttl \
  --query "SELECT ?class WHERE { ?class rdf:type rdfs:Class }" \
  --verbose
```

### Common Errors

**Error**: `Undefined namespace prefix`
```sparql
# ❌ Missing prefix definition
SELECT ?class WHERE {
  ?class rdf:type rdfs:Class .
}

# ✅ Define prefix
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?class WHERE {
  ?class rdf:type rdfs:Class .
}
```

**Error**: `Variable ?prop not found`
```sparql
# ❌ Missing variable in SELECT
SELECT ?class WHERE {
  ?class rdf:type rdfs:Class .
  ?class rdfs:domain ?prop .
}

# ✅ Include all variables used
SELECT ?class ?prop WHERE {
  ?class rdf:type rdfs:Class .
  ?class rdfs:domain ?prop .
}
```

## Summary

You now know how to:
- ✅ Write basic SPARQL queries
- ✅ Use common SPARQL patterns
- ✅ Query your ontologies effectively
- ✅ Export results in various formats
- ✅ Debug SPARQL queries
- ✅ Use SPARQL in ggen workflows

Your ontology queries are now powerful and flexible!
