<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [SPARQL Cookbook](#sparql-cookbook)
  - [Pattern Categories](#pattern-categories)
  - [Basic Extraction](#basic-extraction)
    - [1. Extract All Classes](#1-extract-all-classes)
    - [2. Extract All Properties](#2-extract-all-properties)
    - [3. Get Properties for a Class](#3-get-properties-for-a-class)
  - [Class Hierarchies](#class-hierarchies)
    - [4. Class Inheritance Chain](#4-class-inheritance-chain)
    - [5. Full Inheritance Tree (Transitive)](#5-full-inheritance-tree-transitive)
    - [6. Find Leaf Classes (No Subclasses)](#6-find-leaf-classes-no-subclasses)
  - [Property Extraction](#property-extraction)
    - [7. Required vs Optional Properties](#7-required-vs-optional-properties)
    - [8. Properties with Descriptions](#8-properties-with-descriptions)
    - [9. Data Type Properties vs Object Properties](#9-data-type-properties-vs-object-properties)
  - [Type Mapping](#type-mapping)
    - [10. Map XSD Types to Primitives](#10-map-xsd-types-to-primitives)
    - [11. Find Unmapped Types](#11-find-unmapped-types)
    - [12. Enum-like Classes](#12-enum-like-classes)
  - [Validation & Constraints](#validation--constraints)
    - [13. SHACL Shape Validation](#13-shacl-shape-validation)
    - [14. Datatype Constraints](#14-datatype-constraints)
    - [15. Find Unique Identifiers](#15-find-unique-identifiers)
  - [Advanced Aggregation](#advanced-aggregation)
    - [16. Class with Property Count](#16-class-with-property-count)
    - [17. Properties Shared Between Classes](#17-properties-shared-between-classes)
    - [18. Class Relationships Network](#18-class-relationships-network)
    - [19. Classes by Category](#19-classes-by-category)
  - [Performance Tips](#performance-tips)
    - [General Optimization](#general-optimization)
    - [Debugging Queries](#debugging-queries)
  - [Quick Reference](#quick-reference)
    - [Common Prefixes](#common-prefixes)
    - [Common Patterns](#common-patterns)
    - [Operators](#operators)
  - [Real-World Examples](#real-world-examples)
    - [Example 1: Generate TypeScript Interfaces](#example-1-generate-typescript-interfaces)
    - [Example 2: Generate Validation Schema](#example-2-generate-validation-schema)
    - [Example 3: Generate GraphQL Types](#example-3-generate-graphql-types)
  - [When to Use This Cookbook](#when-to-use-this-cookbook)
  - [Further Learning](#further-learning)
  - [Contributing Patterns](#contributing-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# SPARQL Cookbook

Practical SPARQL query patterns for extracting data from RDF ontologies. Each pattern is battle-tested and ready to use in your templates.

## Pattern Categories

- [Basic Extraction](#basic-extraction) - Essential patterns for getting started
- [Class Hierarchies](#class-hierarchies) - Working with inheritance and relationships
- [Property Extraction](#property-extraction) - Detailed property analysis
- [Type Mapping](#type-mapping) - Type inference and mapping
- [Validation & Constraints](#validation--constraints) - SHACL and data quality
- [Advanced Aggregation](#advanced-aggregation) - Complex queries
- [Performance Tips](#performance-tips) - Optimize your queries

---

## Basic Extraction

### 1. Extract All Classes

Get all RDF classes from the ontology.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?class ?label
WHERE {
    ?class a rdfs:Class .
    ?class rdfs:label ?label .
}
ORDER BY ?label
```

**Use when:** You need to list all entity types in the ontology.

**Returns:** Classes and their labels (human-readable names).

**Template usage:**
```tera
{% query "queries/all-classes.rq" as classes %}
{% for class in classes %}
pub struct {{ class.label }} { }
{% endfor %}
```

---

### 2. Extract All Properties

Get all RDF properties (object and data properties).

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?property ?label ?domain ?range
WHERE {
    ?property a rdf:Property .
    ?property rdfs:label ?label .
    ?property rdfs:domain ?domain .
    ?property rdfs:range ?range .
}
ORDER BY ?label
```

**Use when:** Building a data dictionary or property catalog.

**Returns:** All properties with their type information.

---

### 3. Get Properties for a Class

Extract all properties that belong to a specific class.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?propertyName ?propertyType
WHERE {
    ?class rdfs:label "User" .
    ?property rdfs:domain ?class .
    ?property rdfs:label ?propertyName .
    ?property rdfs:range ?propertyType .
}
ORDER BY ?propertyName
```

**Use when:** Generating struct fields for a specific entity.

**Returns:** Properties and their types for that class.

**Template usage:**
```tera
{% query "queries/user-properties.rq" as props %}
pub struct User {
    {% for prop in props %}
    pub {{ prop.propertyName }}: {{ map_type(prop.propertyType) }},
    {% endfor %}
}
```

---

## Class Hierarchies

### 4. Class Inheritance Chain

Find parent classes (what a class extends).

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?subClass ?superClass
WHERE {
    ?subClass rdfs:subClassOf ?superClass .
    ?subClass rdfs:label ?subLabel .
    ?superClass rdfs:label ?superLabel .
    FILTER (?superClass != rdfs:Class)
}
ORDER BY ?subLabel
```

**Use when:** Generating struct inheritance or trait implementations.

**Returns:** Parent-child class relationships.

**Template usage:**
```tera
{% query "queries/class-hierarchy.rq" as relationships %}
{% for rel in relationships %}
// {{ rel.subClass }} extends {{ rel.superClass }}
{% endfor %}
```

---

### 5. Full Inheritance Tree (Transitive)

Get all ancestors of a class, not just direct parents.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?subClass ?allSuperClasses
WHERE {
    ?subClass rdfs:subClassOf+ ?allSuperClasses .
    FILTER (?allSuperClasses != rdfs:Class)
}
ORDER BY ?subClass
```

**Use when:** Building complete type hierarchies or inheritance chains.

**Key:** The `+` operator means "one or more" - traverses the full hierarchy.

---

### 6. Find Leaf Classes (No Subclasses)

Classes that don't have subclasses.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?class ?label
WHERE {
    ?class a rdfs:Class .
    ?class rdfs:label ?label .
    FILTER NOT EXISTS { ?subclass rdfs:subClassOf ?class . }
    FILTER (?class != rdfs:Class)
}
ORDER BY ?label
```

**Use when:** Generating concrete types (not abstract base classes).

---

## Property Extraction

### 7. Required vs Optional Properties

Distinguish between required and optional properties using SHACL.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sh: <http://www.w3.org/ns/shacl#>

SELECT ?className ?propertyName ?minCount ?maxCount
WHERE {
    ?class rdfs:label ?className .
    ?shape sh:targetClass ?class .
    ?shape sh:property [
        sh:path ?property ;
        sh:minCount ?minCount ;
        sh:maxCount ?maxCount ;
    ] .
    ?property rdfs:label ?propertyName .
}
ORDER BY ?className ?propertyName
```

**Use when:** Generating validation or nullable types.

**Returns:** min/max cardinality for each property.

**Template usage:**
```tera
{% query "queries/property-cardinality.rq" as props %}
pub struct {{ className }} {
    {% for prop in props %}
    pub {{ prop.propertyName }}: {% if prop.minCount == 0 %}Option<{% endif %}{{ prop.type }}{% if prop.minCount == 0 %}>{% endif %},
    {% endfor %}
}
```

---

### 8. Properties with Descriptions

Get property labels and comments for documentation.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dcterms: <http://purl.org/dc/terms/>

SELECT ?propertyName ?propertyLabel ?propertyComment
WHERE {
    ?property a rdf:Property .
    ?property rdfs:label ?propertyName .
    OPTIONAL { ?property rdfs:comment ?propertyComment . }
}
ORDER BY ?propertyName
```

**Use when:** Generating documented code with doc comments.

**Template usage:**
```tera
{% query "queries/property-docs.rq" as props %}
pub struct Model {
    {% for prop in props %}
    /// {{ prop.propertyComment }}
    pub {{ prop.propertyName }}: String,
    {% endfor %}
}
```

---

### 9. Data Type Properties vs Object Properties

Distinguish between attributes (data) and relationships (objects).

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT ?propertyName ?propertyType
       (IF (EXISTS { ?property a owl:DatatypeProperty }, "data", "object") AS ?type)
WHERE {
    ?property rdfs:label ?propertyName .
    ?property rdfs:range ?propertyType .
}
```

**Use when:** Deciding between primitive types and complex types.

---

## Type Mapping

### 10. Map XSD Types to Primitives

Extract all XSD type definitions and their usage.

```sparql
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?xsdType (COUNT(?usage) as ?usageCount)
WHERE {
    ?property rdfs:range ?xsdType .
    FILTER (STRSTARTS(STR(?xsdType), STR(xsd:)))
    BIND (?xsdType as ?usage)
}
GROUP BY ?xsdType
ORDER BY DESC(?usageCount)
```

**Use when:** Building comprehensive type mapping tables.

**Returns:** Which XSD types are actually used in your ontology.

---

### 11. Find Unmapped Types

Identify types that don't have standard mappings.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT DISTINCT ?rangeType
WHERE {
    ?property rdfs:range ?rangeType .
    FILTER (!STRSTARTS(STR(?rangeType), STR(xsd:)))
    FILTER (!STRSTARTS(STR(?rangeType), STR(rdfs:)))
    FILTER (?rangeType != rdfs:Literal)
}
ORDER BY ?rangeType
```

**Use when:** Finding custom types that need special handling.

---

### 12. Enum-like Classes

Classes with fixed string values.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?enumClass (GROUP_CONCAT(?memberLabel; separator=",") as ?members)
WHERE {
    ?enumClass a rdfs:Class .
    ?enumClass rdfs:label ?enumLabel .
    ?member rdf:type ?enumClass .
    ?member rdfs:label ?memberLabel .
}
GROUP BY ?enumClass ?enumLabel
ORDER BY ?enumLabel
```

**Use when:** Generating Rust enums from RDF instances.

**Template usage:**
```tera
{% query "queries/enum-classes.rq" as enums %}
{% for enum in enums %}
pub enum {{ enum.enumClass }} {
    {% for member in enum.members %}
    {{ member }},
    {% endfor %}
}
{% endfor %}
```

---

## Validation & Constraints

### 13. SHACL Shape Validation

Extract validation constraints defined in SHACL.

```sparql
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?className ?propertyName ?minLength ?maxLength ?pattern
WHERE {
    ?shape sh:targetClass ?class .
    ?class rdfs:label ?className .
    ?shape sh:property [
        sh:path ?property ;
        sh:minLength ?minLength ;
        sh:maxLength ?maxLength ;
        sh:pattern ?pattern ;
    ] .
    ?property rdfs:label ?propertyName .
}
```

**Use when:** Generating validation code or constraints.

**Returns:** Cardinality, length, and pattern constraints.

---

### 14. Datatype Constraints

Extract constraints on data types (min/max values, enumerations).

```sparql
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?propertyName ?minInclusive ?maxInclusive ?in
WHERE {
    ?property rdfs:label ?propertyName .
    OPTIONAL { ?property sh:minInclusive ?minInclusive . }
    OPTIONAL { ?property sh:maxInclusive ?maxInclusive . }
    OPTIONAL { ?property sh:in ?in . }
}
```

**Use when:** Generating validation logic or bounds checking.

---

### 15. Find Unique Identifiers

Detect which properties should be unique keys.

```sparql
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?className ?propertyName
WHERE {
    ?shape sh:targetClass ?class .
    ?class rdfs:label ?className .
    ?shape sh:property [
        sh:path ?property ;
        sh:minCount 1 ;
    ] .
    ?property rdfs:label ?propertyName .
    FILTER (?propertyName IN ("id", "uuid", "identifier"))
}
```

**Use when:** Generating primary key attributes.

---

## Advanced Aggregation

### 16. Class with Property Count

Understand complexity of classes by property count.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?className (COUNT(?property) as ?propCount)
WHERE {
    ?class rdfs:label ?className .
    ?property rdfs:domain ?class .
}
GROUP BY ?className
ORDER BY DESC(?propCount)
```

**Use when:** Identifying complex entities or performance concerns.

---

### 17. Properties Shared Between Classes

Find properties used by multiple classes (mixins/traits).

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?propertyName (COUNT(?class) as ?classCount) (GROUP_CONCAT(?className; separator=",") as ?classes)
WHERE {
    ?property rdfs:label ?propertyName .
    ?property rdfs:domain ?class .
    ?class rdfs:label ?className .
}
GROUP BY ?propertyName ?classCount
HAVING (?classCount > 1)
ORDER BY DESC(?classCount)
```

**Use when:** Extracting traits or interfaces used across multiple types.

---

### 18. Class Relationships Network

Map which classes reference which other classes.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?sourceClass ?targetClass (COUNT(?prop) as ?relationshipCount)
WHERE {
    ?sourceDef rdfs:label ?sourceClass .
    ?targetDef rdfs:label ?targetClass .
    ?prop rdfs:domain ?sourceDef .
    ?prop rdfs:range ?targetDef .
    FILTER (?sourceDef != ?targetDef)
}
GROUP BY ?sourceClass ?targetClass
ORDER BY ?sourceClass ?targetClass
```

**Use when:** Understanding entity relationships or generating GraphQL schemas.

---

### 19. Classes by Category

Group classes by a category property.

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dcat: <http://www.w3.org/ns/dcat#>

SELECT ?category (GROUP_CONCAT(?className; separator=",") as ?classes)
WHERE {
    ?class a rdfs:Class .
    ?class rdfs:label ?className .
    ?class dcat:theme ?category .
}
GROUP BY ?category
ORDER BY ?category
```

**Use when:** Organizing generated code by module/package.

---

## Performance Tips

### General Optimization

1. **Use FILTER, not OPTIONAL** for hard filters
   ```sparql
   # Slower
   OPTIONAL { ?class rdfs:subClassOf ?parent . }
   FILTER (?parent != rdfs:Class)

   # Faster
   ?class rdfs:subClassOf ?parent .
   FILTER (?parent != rdfs:Class)
   ```

2. **Add LIMIT for development**
   ```sparql
   SELECT ?class ?label
   WHERE { ?class rdfs:label ?label . }
   LIMIT 100  # Test with sample first
   ```

3. **Use specific patterns over generic traversal**
   ```sparql
   # Slow - traverses entire graph
   ?x ?p ?y .

   # Fast - specific pattern
   ?x rdfs:domain ?domain .
   ```

4. **Bind computed values**
   ```sparql
   BIND(CONCAT(?first, " ", ?last) as ?fullName)
   ```

5. **Use GROUP_CONCAT with separators efficiently**
   ```sparql
   SELECT ?class (GROUP_CONCAT(?prop; separator="|") as ?props)
   ```

---

### Debugging Queries

**Test query returns results:**
```sparql
SELECT COUNT(*)
WHERE {
    ?class a rdfs:Class .
}
```

**Check for expected patterns:**
```sparql
SELECT ?p WHERE {
    ?x ?p ?y .
}
LIMIT 1
```

**Verify property domains:**
```sparql
SELECT ?prop ?domain
WHERE {
    ?prop rdfs:domain ?domain .
}
```

---

## Quick Reference

### Common Prefixes

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX sh: <http://www.w3.org/ns/shacl#>
```

### Common Patterns

| Task | Pattern |
|------|---------|
| All classes | `?class a rdfs:Class .` |
| All properties | `?prop a rdf:Property .` |
| Class domain | `?prop rdfs:domain ?class .` |
| Property range | `?prop rdfs:range ?type .` |
| Parent class | `?class rdfs:subClassOf ?parent .` |
| Has label | `?x rdfs:label ?label .` |
| Has comment | `?x rdfs:comment ?comment .` |
| Is instance of | `?x rdf:type ?class .` |

### Operators

```sparql
# Logical
?x = ?y              # Equality
?x != ?y             # Inequality
FILTER (condition)   # Filter results

# String
CONTAINS(?str, "text")
STRSTARTS(?str, "prefix")
STRENDSWITH(?str, "suffix")
CONCAT(?a, ?b)

# Numeric
?x > 5
?x >= 5
?x < 5
?x <= 5

# Set operations
UNION                # OR queries
OPTIONAL             # Left join
MINUS                # Set difference

# Aggregation
COUNT(?x)
SUM(?x)
AVG(?x)
MIN(?x)
MAX(?x)
GROUP_CONCAT(?x; separator=",")

# Path traversal
?x parent+ ?ancestor    # One or more
?x parent* ?ancestor    # Zero or more
```

---

## Real-World Examples

### Example 1: Generate TypeScript Interfaces

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?className (GROUP_CONCAT(CONCAT(?propName, ":", ?propType); separator=";") as ?properties)
WHERE {
    ?class rdfs:label ?className .
    ?prop rdfs:domain ?class .
    ?prop rdfs:label ?propName .
    ?prop rdfs:range ?propType .
}
GROUP BY ?className
```

**Template:**
```tera
{% query "queries/ts-interfaces.rq" as interfaces %}
{% for iface in interfaces %}
export interface {{ iface.className }} {
    {% for prop in iface.properties %}
    {{ prop }};
    {% endfor %}
}
{% endfor %}
```

### Example 2: Generate Validation Schema

```sparql
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?className ?propertyName ?minCount ?maxLength
WHERE {
    ?shape sh:targetClass ?class .
    ?class rdfs:label ?className .
    ?shape sh:property [
        sh:path ?prop ;
        sh:minCount ?minCount ;
        sh:maxLength ?maxLength ;
    ] .
    ?prop rdfs:label ?propertyName .
}
```

### Example 3: Generate GraphQL Types

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?sourceClass ?targetClass ?fieldName
WHERE {
    ?sourceDef rdfs:label ?sourceClass .
    ?targetDef rdfs:label ?targetClass .
    ?prop rdfs:domain ?sourceDef .
    ?prop rdfs:range ?targetDef .
    ?prop rdfs:label ?fieldName .
}
```

---

## When to Use This Cookbook

✅ **Use when:**
- Creating new templates
- Extracting ontology data
- Validating ontology structure
- Understanding relationships
- Generating code from RDF

❌ **Not for:**
- Modifying ontologies (use RDF update queries)
- Data instance queries (queries work on schema, not data)
- Complex reasoning (would need reasoner like HermiT)

---

## Further Learning

- **SPARQL Spec:** https://www.w3.org/TR/sparql11-query/
- **SHACL Spec:** https://www.w3.org/TR/shacl/
- **Oxigraph Docs:** https://github.com/oxigraph/oxigraph
- **RDF/SPARQL Reference:** [See Reference Guide](rdf-sparql.md)

---

## Contributing Patterns

Found a useful SPARQL pattern? Add it to this cookbook!

1. Test your query against real ontologies
2. Document the use case
3. Provide template example
4. Add to appropriate section
5. Submit PR with improvements
