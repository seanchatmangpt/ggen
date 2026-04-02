<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [RDF/SPARQL Reference](#rdfsparql-reference)
  - [RDF Formats](#rdf-formats)
  - [RDF Basics](#rdf-basics)
    - [Triples](#triples)
    - [Prefixes](#prefixes)
    - [Classes](#classes)
    - [Properties](#properties)
  - [SPARQL Query Syntax](#sparql-query-syntax)
    - [SELECT Queries](#select-queries)
    - [Basic Patterns](#basic-patterns)
    - [Filters](#filters)
    - [Optional Patterns](#optional-patterns)
    - [Aggregates](#aggregates)
  - [SHACL Validation](#shacl-validation)
    - [Node Shapes](#node-shapes)
    - [Property Shapes](#property-shapes)
    - [Constraints](#constraints)
  - [Type System](#type-system)
    - [XSD Types](#xsd-types)
    - [RDF Types](#rdf-types)
  - [Common Patterns](#common-patterns)
    - [Inheritance](#inheritance)
    - [Relationships](#relationships)
    - [Collections](#collections)
  - [Query Optimization](#query-optimization)
    - [Use LIMIT](#use-limit)
    - [Specific Patterns](#specific-patterns)
    - [ORDER BY for Determinism](#order-by-for-determinism)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# RDF/SPARQL Reference

Complete reference for RDF, SHACL, and SPARQL in ggen.

## RDF Formats

ggen supports multiple RDF serialization formats:

- **Turtle (.ttl)** - Human-readable, prefix support
- **N-Triples (.nt)** - Line-based plain format
- **JSON-LD (.jsonld)** - JSON-based with context

## RDF Basics

### Triples

RDF represents knowledge as triples: subject-predicate-object.

```turtle
ex:User a rdfs:Class .
ex:userName rdfs:domain ex:User .
ex:userName rdfs:range xsd:string .
```

### Prefixes

```turtle
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
```

### Classes

```turtle
ex:User a rdfs:Class ;
    rdfs:label "User" ;
    rdfs:comment "Customer account" .
```

### Properties

```turtle
ex:userName a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "name" .
```

## SPARQL Query Syntax

### SELECT Queries

Extract data from graph:

```sparql
SELECT ?class ?property ?type
WHERE {
    ?class a rdfs:Class .
    ?property rdfs:domain ?class .
    ?property rdfs:range ?type .
}
ORDER BY ?class ?property
```

### Basic Patterns

**Find all classes:**
```sparql
SELECT ?class
WHERE {
    ?class a rdfs:Class .
}
```

**Find class properties:**
```sparql
SELECT ?property ?type
WHERE {
    ?property rdfs:domain ?class .
    ?property rdfs:range ?type .
}
```

### Filters

```sparql
SELECT ?class
WHERE {
    ?class a rdfs:Class .
    FILTER(?class != rdfs:Resource)
}
```

### Optional Patterns

```sparql
SELECT ?class ?comment
WHERE {
    ?class a rdfs:Class .
    OPTIONAL {
        ?class rdfs:comment ?comment .
    }
}
```

### Aggregates

```sparql
SELECT ?class (COUNT(?property) AS ?prop_count)
WHERE {
    ?class a rdfs:Class .
    ?property rdfs:domain ?class .
}
GROUP BY ?class
```

## SHACL Validation

### Node Shapes

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

### Property Shapes

```turtle
ex:UserShape sh:property [
    sh:path ex:userEmail ;
    sh:datatype xsd:string ;
    sh:pattern "^[^@]+@[^@]+\\.[^@]+$" ;
    sh:minCount 1
] .
```

### Constraints

- `sh:minCount` / `sh:maxCount` - Cardinality
- `sh:datatype` - Data type validation
- `sh:class` - Class membership
- `sh:pattern` - Regular expression
- `sh:minInclusive` / `sh:maxInclusive` - Numeric ranges

## Type System

### XSD Types

| XSD Type | Description |
|----------|-------------|
| `xsd:string` | Character string |
| `xsd:integer` | Integer number |
| `xsd:decimal` | Decimal number |
| `xsd:boolean` | Boolean value |
| `xsd:dateTime` | Date and time |
| `xsd:date` | Date only |
| `xsd:time` | Time only |

### RDF Types

| RDF Type | Description |
|----------|-------------|
| `rdfs:Class` | Class definition |
| `rdf:Property` | Property definition |
| `rdfs:Resource` | Base resource type |

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
    rdfs:range ex:User .
```

### Collections

```turtle
ex:orderItems a rdf:Property ;
    rdfs:domain ex:Order ;
    rdfs:range rdf:Bag .
```

## Query Optimization

### Use LIMIT

```sparql
SELECT ?s ?p ?o
WHERE { ?s ?p ?o }
LIMIT 100
```

### Specific Patterns

```sparql
SELECT ?class
WHERE {
    ?class a rdfs:Class .
    ?class rdfs:label "User" .
}
```

### ORDER BY for Determinism

```sparql
SELECT ?class ?property
WHERE { ?class ?property ?value }
ORDER BY ?class ?property
```

## See Also

- [Use RDF Ontologies Guide](../how-to-guides/use-rdf-ontologies.md)
- [Ontology-Driven Explanation](../explanations/ontology-driven.md)
- [Ontology-to-Code Tutorial](../tutorials/ontology-to-code.md)

