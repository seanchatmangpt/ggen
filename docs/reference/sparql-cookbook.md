<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [SPARQL Cookbook](#sparql-cookbook)
  - [Getting Started](#getting-started)
  - [Discover Classes](#discover-classes)
  - [Extract Properties](#extract-properties)
  - [Find Constraints](#find-constraints)
  - [Identify Relationships](#identify-relationships)
  - [Find Data Properties](#find-data-properties)
  - [Count Entities](#count-entities)
  - [Find Optional Properties](#find-optional-properties)
  - [Validate Class Properties](#validate-class-properties)
  - [Find Inheritance](#find-inheritance)
  - [Type Mapping](#type-mapping)
  - [List Required Parameters](#list-required-parameters)
  - [Find Enum Values](#find-enum-values)
  - [Advanced: Conditional Generation](#advanced-conditional-generation)
  - [Query RDF in Templates](#query-rdf-in-templates)
  - [Common Filters](#common-filters)
  - [Performance Tips](#performance-tips)
    - [Use OPTIONAL sparingly](#use-optional-sparingly)
    - [Use FILTER before JOIN](#use-filter-before-join)
    - [Limit results for testing](#limit-results-for-testing)
  - [Debugging Queries](#debugging-queries)
    - [Check what's loaded](#check-whats-loaded)
    - [List all predicates](#list-all-predicates)
    - [Find broken references](#find-broken-references)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# SPARQL Cookbook

Quick reference for SPARQL patterns organized by common tasks.

## Getting Started

All examples assume this ontology is loaded:

```turtle
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "Platform user" .

ex:name a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "name" .

ex:email a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "email" ;
  sh:pattern "[a-z0-9]+@[a-z0-9]+\\.[a-z]+" .

ex:Order a rdfs:Class ;
  rdfs:label "Order" .

ex:orderedBy a rdf:Property ;
  rdfs:domain ex:Order ;
  rdfs:range ex:User ;
  rdfs:label "orderedBy" .
```

## Discover Classes

Find all classes in the ontology:

```sparql
SELECT ?class ?label
WHERE {
  ?class a rdfs:Class .
  OPTIONAL { ?class rdfs:label ?label }
}
ORDER BY ?label
```

**Usage in template:**
```tera
{% query "queries/find-classes.rq" as classes %}
{% for class in classes %}
  pub struct {{ class.label }} {
{% endfor %}
```

## Extract Properties

Find all properties of a specific class:

```sparql
SELECT ?property ?label ?type
WHERE {
  ?property rdfs:domain ?class ;
            rdfs:range ?type .
  ?class rdfs:label "User" .
  OPTIONAL { ?property rdfs:label ?label }
}
ORDER BY ?label
```

**For code generation:**
```sparql
SELECT ?className ?propertyName ?propertyType
WHERE {
  ?class a rdfs:Class ;
         rdfs:label ?className .
  ?property rdfs:domain ?class ;
            rdfs:range ?propertyType ;
            rdfs:label ?propertyName .
}
ORDER BY ?className ?propertyName
```

## Find Constraints

Extract validation constraints for a property:

```sparql
SELECT ?property ?minInclusive ?maxInclusive ?pattern ?minLength ?maxLength
WHERE {
  ?property rdfs:label "email" .
  OPTIONAL { ?property sh:minInclusive ?minInclusive }
  OPTIONAL { ?property sh:maxInclusive ?maxInclusive }
  OPTIONAL { ?property sh:pattern ?pattern }
  OPTIONAL { ?property sh:minLength ?minLength }
  OPTIONAL { ?property sh:maxLength ?maxLength }
}
```

## Identify Relationships

Find object properties (references to other classes):

```sparql
SELECT ?source ?sourceLabel ?relationship ?relationshipLabel ?target ?targetLabel
WHERE {
  ?relationship rdfs:domain ?source ;
                rdfs:range ?target ;
                rdf:type rdf:Property .
  ?source a rdfs:Class ;
          rdfs:label ?sourceLabel .
  ?target a rdfs:Class ;
          rdfs:label ?targetLabel .
  OPTIONAL { ?relationship rdfs:label ?relationshipLabel }
  # Filter to exclude datatype properties
  FILTER(?target != xsd:string && ?target != xsd:integer &&
          ?target != xsd:decimal && ?target != xsd:boolean &&
          ?target != xsd:dateTime)
}
ORDER BY ?sourceLabel ?targetLabel
```

## Find Data Properties

Find properties with primitive (datatype) ranges:

```sparql
SELECT ?class ?property ?label ?type
WHERE {
  ?property rdfs:domain ?class ;
            rdfs:range ?type ;
            rdfs:label ?label .
  ?class rdfs:label "User" .
  # Include only datatype properties
  FILTER(?type IN (xsd:string, xsd:integer, xsd:decimal,
                   xsd:boolean, xsd:dateTime, xsd:date))
}
ORDER BY ?label
```

## Count Entities

Count classes and properties:

```sparql
SELECT (COUNT(?class) AS ?classCount) (COUNT(?property) AS ?propertyCount)
WHERE {
  OPTIONAL { ?class a rdfs:Class }
  OPTIONAL { ?property rdf:type rdf:Property }
}
```

## Find Optional Properties

Identify properties that are optional (marked in comment):

```sparql
SELECT ?class ?property ?label
WHERE {
  ?property rdfs:domain ?class ;
            rdfs:label ?label ;
            rdfs:comment ?comment .
  ?class a rdfs:Class .
  FILTER CONTAINS(?comment, "optional")
}
ORDER BY ?class ?label
```

## Validate Class Properties

Check that all classes have required labels:

```sparql
SELECT ?class
WHERE {
  ?class a rdfs:Class .
  FILTER NOT EXISTS { ?class rdfs:label ?label }
}
```

**Usage:** Returns classes missing labels.

## Find Inheritance

Identify class hierarchies (subclasses):

```sparql
SELECT ?subclass ?superclass
WHERE {
  ?subclass rdfs:subClassOf ?superclass .
  FILTER (?subclass != ?superclass)
}
ORDER BY ?superclass ?subclass
```

## Type Mapping

Extract type information for code generation:

```sparql
SELECT ?property ?label ?rdfType ?rustType
WHERE {
  ?property rdfs:range ?rdfType ;
            rdfs:label ?label .
  # Map RDF types to Rust types
  BIND(
    IF(?rdfType = xsd:string, "String",
    IF(?rdfType = xsd:integer, "i32",
    IF(?rdfType = xsd:decimal, "f64",
    IF(?rdfType = xsd:boolean, "bool",
    IF(?rdfType = xsd:dateTime, "DateTime<Utc>",
    "Unknown"))))) AS ?rustType
  )
}
ORDER BY ?label
```

## List Required Parameters

Find template variables (properties without defaults):

```sparql
SELECT ?param
WHERE {
  ?param rdfs:label ?label .
  FILTER NOT EXISTS { ?param rdfs:comment "optional" }
}
```

## Find Enum Values

Extract enum values from comments:

```sparql
SELECT ?property ?value
WHERE {
  ?property rdfs:comment ?comment ;
            rdfs:label "status" .
  BIND(STRSPLIT(?comment, ",") AS ?values)
}
```

## Advanced: Conditional Generation

Generate different code based on property type:

```sparql
SELECT ?property ?label ?type
  (IF(?type = xsd:string, "string_field",
      IF(?type = xsd:integer, "int_field",
      IF(?type = xsd:dateTime, "datetime_field",
      "generic_field"))) AS ?fieldType)
WHERE {
  ?property rdfs:domain ?class ;
            rdfs:range ?type ;
            rdfs:label ?label .
  ?class rdfs:label "User" .
}
ORDER BY ?label
```

## Query RDF in Templates

Use queries directly in templates:

```tera
{# Extract all classes #}
{% query "SELECT ?class ?label WHERE {
  ?class a rdfs:Class .
  OPTIONAL { ?class rdfs:label ?label }
} ORDER BY ?label" as classes %}

{% for class in classes %}
pub struct {{ class.label }} {
  // Generated from RDF
}
{% endfor %}
```

Or use separate SPARQL files:

```tera
{% query "queries/extract-classes.rq" as classes %}

{% for class in classes %}
pub struct {{ class.label }} {
  // Generated from RDF
}
{% endfor %}
```

## Common Filters

Filter out system classes:

```sparql
SELECT ?class ?label
WHERE {
  ?class a rdfs:Class ;
         rdfs:label ?label .
  FILTER(?class != rdfs:Resource &&
          ?class != rdfs:Class &&
          !STRSTARTS(STR(?class), "http://www.w3.org/"))
}
ORDER BY ?label
```

Filter by property type:

```sparql
SELECT ?property
WHERE {
  ?property rdfs:range ?type .
  FILTER (?type = xsd:string || ?type = xsd:integer)
}
```

Filter by label pattern:

```sparql
SELECT ?class
WHERE {
  ?class rdfs:label ?label .
  FILTER CONTAINS(?label, "User")
}
```

## Performance Tips

### Use OPTIONAL sparingly

Instead of:
```sparql
# Slow - multiple matches
SELECT * WHERE {
  ?class a rdfs:Class .
  OPTIONAL { ?class rdfs:label ?label }
  OPTIONAL { ?class rdfs:comment ?comment }
  OPTIONAL { ?class rdfs:isDefinedBy ?source }
}
```

Use specific queries when needed:
```sparql
# Fast - focused
SELECT ?class ?label
WHERE {
  ?class a rdfs:Class ;
         rdfs:label ?label .
}
```

### Use FILTER before JOIN

Good:
```sparql
SELECT ?property
WHERE {
  ?property rdfs:domain ?class .
  FILTER (?class = ex:User)
}
```

### Limit results for testing

During development:
```sparql
SELECT ?class ?label
WHERE {
  ?class a rdfs:Class ;
         rdfs:label ?label .
}
LIMIT 10
```

## Debugging Queries

### Check what's loaded

```sparql
SELECT (COUNT(?s) AS ?tripleCount)
WHERE { ?s ?p ?o }
```

### List all predicates

```sparql
SELECT DISTINCT ?predicate
WHERE { ?s ?predicate ?o }
ORDER BY ?predicate
```

### Find broken references

```sparql
SELECT ?source ?property ?invalidTarget
WHERE {
  ?source ?property ?invalidTarget .
  ?property rdf:type rdf:Property .
  FILTER NOT EXISTS { ?invalidTarget a rdfs:Class }
  FILTER (?invalidTarget NOT IN (xsd:string, xsd:integer, xsd:decimal, xsd:boolean, xsd:dateTime))
}
```

## See Also

- [RDF/SPARQL Reference](rdf-sparql.md) - Complete RDF/SPARQL guide
- [Template Reference](templates.md) - Using queries in templates
- [Type Mapping Reference](type-mapping.md) - Type extraction and mapping
