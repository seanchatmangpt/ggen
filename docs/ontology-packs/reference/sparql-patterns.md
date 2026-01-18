# Reference: SPARQL Query Patterns

Common SPARQL queries for working with ontologies.

## List All Classes

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?class ?label ?comment WHERE {
  ?class rdf:type rdfs:Class .
  OPTIONAL { ?class rdfs:label ?label }
  OPTIONAL { ?class rdfs:comment ?comment }
}
ORDER BY ?label
```

## List All Properties

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?property ?label ?domain ?range WHERE {
  ?property rdf:type rdf:Property .
  OPTIONAL { ?property rdfs:label ?label }
  OPTIONAL { ?property rdfs:domain ?domain }
  OPTIONAL { ?property rdfs:range ?range }
}
ORDER BY ?label
```

## Find Class Properties

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?property ?label WHERE {
  ?property rdfs:domain ?class .
  ?class rdfs:label "Product" .
  OPTIONAL { ?property rdfs:label ?label }
}
```

## Class Hierarchy

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?parent ?child WHERE {
  ?child rdfs:subClassOf ?parent .
}
```

## Properties by Range Type

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?property ?domain WHERE {
  ?property rdfs:range <http://www.w3.org/2001/XMLSchema#string> .
  OPTIONAL { ?property rdfs:domain ?domain }
}
```

## Find Required Properties

```sparql
PREFIX schema: <https://schema.org/>
PREFIX sh: <http://www.w3.org/ns/shacl#>

SELECT ?property WHERE {
  ?shape sh:targetClass ?class .
  ?shape sh:property ?prop .
  ?prop sh:path ?property .
  ?prop sh:minCount 1 .
}
```

## Deprecated Elements

```sparql
PREFIX schema: <https://schema.org/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT ?deprecated ?replacement WHERE {
  ?deprecated owl:deprecated true .
  OPTIONAL { ?deprecated owl:equivalentClass ?replacement }
}
```

## Count Statistics

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT (COUNT(DISTINCT ?class) as ?classes)
       (COUNT(DISTINCT ?prop) as ?properties) WHERE {
  ?class rdf:type rdfs:Class .
  ?prop rdf:type rdf:Property .
}
```

## Full Example: Schema.org Product

```sparql
PREFIX schema: <https://schema.org/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

# Get Product class and all its properties
SELECT ?property ?label ?range WHERE {
  schema:Product rdfs:subPropertyOf* ?parent .
  ?property rdfs:domain ?parent .
  OPTIONAL { ?property rdfs:label ?label }
  OPTIONAL { ?property rdfs:range ?range }
}
ORDER BY ?label
```
