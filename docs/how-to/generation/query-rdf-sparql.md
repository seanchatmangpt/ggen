<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Query RDF Data with SPARQL](#how-to-query-rdf-data-with-sparql)
  - [Prerequisites](#prerequisites)
  - [Understanding SPARQL in 60 Seconds](#understanding-sparql-in-60-seconds)
  - [Step 1: Load Sample Data](#step-1-load-sample-data)
  - [Step 2: Basic Queries](#step-2-basic-queries)
    - [Query 1: Get All Products](#query-1-get-all-products)
    - [Query 2: Get Products with Details](#query-2-get-products-with-details)
  - [Step 3: Filtering Results](#step-3-filtering-results)
    - [Filter 1: Products in Stock](#filter-1-products-in-stock)
    - [Filter 2: Products Under $100](#filter-2-products-under-100)
    - [Filter 3: Products by Category](#filter-3-products-by-category)
  - [Step 4: Advanced Queries](#step-4-advanced-queries)
    - [Aggregation: Count Products](#aggregation-count-products)
    - [Optional Properties](#optional-properties)
    - [Property Paths](#property-paths)
  - [Step 5: Queries for Code Generation](#step-5-queries-for-code-generation)
    - [Extract Class Definitions](#extract-class-definitions)
    - [Extract Enumeration Values](#extract-enumeration-values)
  - [Step 6: Save Query Results for Templates](#step-6-save-query-results-for-templates)
  - [Common Patterns Cheat Sheet](#common-patterns-cheat-sheet)
    - [Pattern 1: Find All Instances of a Type](#pattern-1-find-all-instances-of-a-type)
    - [Pattern 2: Get Property Values](#pattern-2-get-property-values)
    - [Pattern 3: Filter with REGEX](#pattern-3-filter-with-regex)
    - [Pattern 4: Count Results](#pattern-4-count-results)
    - [Pattern 5: Limit Results](#pattern-5-limit-results)
    - [Pattern 6: Get Distinct Values](#pattern-6-get-distinct-values)
  - [Troubleshooting](#troubleshooting)
  - [Next Steps](#next-steps)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Query RDF Data with SPARQL

**Problem**: "I need to extract specific data from my RDF ontology for code generation"

**Solution**: Use ggen's SPARQL query capabilities to extract exactly the data you need.

**Time**: 10 minutes
**Difficulty**: Beginner to Intermediate

---

## Prerequisites

- ggen installed ([installation guide](../../getting-started/README.md#installation))
- Basic understanding of RDF triples (subject-predicate-object)
- An RDF file to query (or use the example below)

---

## Understanding SPARQL in 60 Seconds

**SPARQL** is like SQL for RDF graphs. Instead of tables with rows and columns, you query graphs with triples (subject-predicate-object).

**Basic pattern**:
```sparql
SELECT ?variable
WHERE {
  ?variable predicate object .
}
```

**Think of it as**: "Find all subjects that have this predicate and object"

---

## Step 1: Load Sample Data

Create a sample e-commerce ontology:

```bash
cat > ecommerce.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix schema: <https://schema.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Products
ex:product001 a schema:Product ;
    schema:name "Laptop" ;
    schema:sku "LAP-2024-001" ;
    schema:price "999.99"^^xsd:decimal ;
    schema:category ex:electronics ;
    schema:inStock true .

ex:product002 a schema:Product ;
    schema:name "Wireless Mouse" ;
    schema:sku "MOU-2024-042" ;
    schema:price "29.99"^^xsd:decimal ;
    schema:category ex:electronics ;
    schema:inStock true .

ex:product003 a schema:Product ;
    schema:name "Desk Chair" ;
    schema:sku "CHR-2024-013" ;
    schema:price "249.99"^^xsd:decimal ;
    schema:category ex:furniture ;
    schema:inStock false .

# Categories
ex:electronics a schema:Category ;
    schema:name "Electronics" .

ex:furniture a schema:Category ;
    schema:name "Furniture" .
EOF

# Load into ggen graph
ggen graph load --file ecommerce.ttl
```

**Expected output**:
```
✓ Loaded 15 triples from ecommerce.ttl
```

---

## Step 2: Basic Queries

### Query 1: Get All Products

**Goal**: Retrieve all product names

```bash
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>

SELECT ?name
WHERE {
  ?product a schema:Product .
  ?product schema:name ?name .
}
'
```

**Output**:
```json
[
  {"name": "Laptop"},
  {"name": "Wireless Mouse"},
  {"name": "Desk Chair"}
]
```

### Query 2: Get Products with Details

**Goal**: Get name, SKU, and price for all products

```bash
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>

SELECT ?name ?sku ?price
WHERE {
  ?product a schema:Product ;
           schema:name ?name ;
           schema:sku ?sku ;
           schema:price ?price .
}
ORDER BY ?name
'
```

**Output**:
```json
[
  {"name": "Desk Chair", "sku": "CHR-2024-013", "price": "249.99"},
  {"name": "Laptop", "sku": "LAP-2024-001", "price": "999.99"},
  {"name": "Wireless Mouse", "sku": "MOU-2024-042", "price": "29.99"}
]
```

---

## Step 3: Filtering Results

### Filter 1: Products in Stock

**Goal**: Only show products that are in stock

```bash
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>

SELECT ?name ?price
WHERE {
  ?product a schema:Product ;
           schema:name ?name ;
           schema:price ?price ;
           schema:inStock true .
}
'
```

### Filter 2: Products Under $100

**Goal**: Find affordable products

```bash
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?name ?price
WHERE {
  ?product a schema:Product ;
           schema:name ?name ;
           schema:price ?price .
  FILTER(?price < "100.00"^^xsd:decimal)
}
'
```

**Output**:
```json
[
  {"name": "Wireless Mouse", "price": "29.99"}
]
```

### Filter 3: Products by Category

**Goal**: Get all electronics

```bash
ggen graph query --sparql_query '
PREFIX ex: <http://example.org/>
PREFIX schema: <https://schema.org/>

SELECT ?name ?category_name
WHERE {
  ?product a schema:Product ;
           schema:name ?name ;
           schema:category ?category .
  ?category schema:name ?category_name .
  FILTER(?category = ex:electronics)
}
'
```

---

## Step 4: Advanced Queries

### Aggregation: Count Products

**Goal**: Count products by category

```bash
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>

SELECT ?category_name (COUNT(?product) AS ?count)
WHERE {
  ?product a schema:Product ;
           schema:category ?category .
  ?category schema:name ?category_name .
}
GROUP BY ?category_name
ORDER BY DESC(?count)
'
```

**Output**:
```json
[
  {"category_name": "Electronics", "count": 2},
  {"category_name": "Furniture", "count": 1}
]
```

### Optional Properties

**Goal**: Get products with optional description

```bash
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>

SELECT ?name ?description
WHERE {
  ?product a schema:Product ;
           schema:name ?name .
  OPTIONAL { ?product schema:description ?description }
}
'
```

**Why OPTIONAL?**: Not all products have descriptions. Without OPTIONAL, products without descriptions wouldn't appear in results.

### Property Paths

**Goal**: Navigate nested relationships

```bash
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>

SELECT ?product_name ?category_name
WHERE {
  ?product a schema:Product ;
           schema:name ?product_name ;
           schema:category/schema:name ?category_name .
}
'
```

**Explanation**: `schema:category/schema:name` follows two relationships:
1. Product → category
2. Category → name

---

## Step 5: Queries for Code Generation

### Extract Class Definitions

**Goal**: Get all classes with their properties for code generation

```bash
ggen graph query --sparql_query '
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?class ?class_label ?property ?prop_label ?range
WHERE {
  ?class a rdfs:Class ;
         rdfs:label ?class_label .
  ?property rdfs:domain ?class ;
            rdfs:label ?prop_label ;
            rdfs:range ?range .
}
ORDER BY ?class ?property
'
```

### Extract Enumeration Values

**Goal**: Get all valid values for enumerations (for Zod enums)

```bash
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?enum ?value ?label
WHERE {
  ?enum a schema:Enumeration .
  ?value a ?enum ;
         rdfs:label ?label .
}
ORDER BY ?enum ?value
'
```

---

## Step 6: Save Query Results for Templates

**Save results to JSON for use in templates**:

```bash
# Query and save to file
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>

SELECT ?name ?sku ?price
WHERE {
  ?product a schema:Product ;
           schema:name ?name ;
           schema:sku ?sku ;
           schema:price ?price .
}
' > products.json

# Use in template generation
ggen project gen \
  --template_ref product-list.tmpl \
  --vars products_file=products.json \
  --output src/products.js
```

---

## Common Patterns Cheat Sheet

### Pattern 1: Find All Instances of a Type
```sparql
SELECT ?instance
WHERE {
  ?instance a <YourType> .
}
```

### Pattern 2: Get Property Values
```sparql
SELECT ?value
WHERE {
  <YourInstance> <yourProperty> ?value .
}
```

### Pattern 3: Filter with REGEX
```sparql
SELECT ?name
WHERE {
  ?product schema:name ?name .
  FILTER(REGEX(?name, "Laptop", "i"))  # Case-insensitive match
}
```

### Pattern 4: Count Results
```sparql
SELECT (COUNT(?item) AS ?total)
WHERE {
  ?item a schema:Product .
}
```

### Pattern 5: Limit Results
```sparql
SELECT ?name
WHERE {
  ?product schema:name ?name .
}
LIMIT 10
```

### Pattern 6: Get Distinct Values
```sparql
SELECT DISTINCT ?category
WHERE {
  ?product schema:category ?category .
}
```

---

## Troubleshooting

**Problem**: "No results returned"
**Solutions**:
- Check your prefixes match the ontology namespaces
- Verify data is loaded: `ggen graph query --sparql_query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"`
- Use OPTIONAL for properties that might not exist

**Problem**: "Query syntax error"
**Solutions**:
- Check all triples end with `.`
- Ensure prefixes are defined
- Use proper quote escaping in shell commands

**Problem**: "Results are incomplete"
**Solutions**:
- Add OPTIONAL for non-required properties
- Remove overly restrictive filters
- Check LIMIT isn't cutting off results

---

## Next Steps

**Learn More SPARQL**:
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [SPARQL by Example](https://www.w3.org/2009/Talks/0615-qbe/)
- [SPARQL Tutorial](https://www.w3.org/TR/rdf-sparql-query/)

**Apply to Code Generation**:
- [Generate JavaScript + Zod](generate-javascript-zod.md)
- [Create Custom Templates](../../tutorials/core/03-custom-template-creation.md)

**Explore ggen**:
- [Ontology Extraction](../../reference/commands/complete-cli-reference.md#ontology-commands)
- [Graph Visualization](../../reference/commands/complete-cli-reference.md#graph-commands)

---

## Summary

You learned how to:
- ✅ Write basic SELECT queries
- ✅ Filter results with FILTER
- ✅ Use OPTIONAL for nullable properties
- ✅ Aggregate data with COUNT and GROUP BY
- ✅ Navigate relationships with property paths
- ✅ Extract schema information for code generation
- ✅ Save query results for templates

**Pro tip**: Start with simple queries and add complexity incrementally. Use `LIMIT 10` while testing!
