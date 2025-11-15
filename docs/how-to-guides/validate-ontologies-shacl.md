# How to Validate Ontologies with SHACL

## Overview

SHACL (Shapes Constraint Language) allows you to define constraints for your RDF ontologies and validate that instances conform to those constraints. This ensures data quality and prevents invalid data from being generated.

## Problem You're Solving

When generating code from ontologies, you want to ensure:
- Required fields are present
- Data types are correct
- Value ranges are valid (min/max)
- Relationships are properly defined
- No invalid ontologies cause broken code generation

## Prerequisites

- ggen installed and working
- An existing RDF ontology (`.ttl` file)
- Basic understanding of RDF and SPARQL
- Familiarity with [RDF/SPARQL Reference](../reference/rdf-sparql.md)

## Step 1: Define SHACL Shapes for Your Ontology

Create a SHACL shapes file that defines constraints for your ontology. For example, create `ontology-shapes.ttl`:

```turtle
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

# Shape for Product class
ex:ProductShape a sh:NodeShape ;
    sh:targetClass ex:Product ;
    sh:property [
        sh:path ex:productName ;
        sh:datatype xsd:string ;
        sh:minLength 1 ;
        sh:maxLength 255 ;
        sh:minCount 1 ;  # Required
    ] ;
    sh:property [
        sh:path ex:price ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0.01 ;
        sh:minCount 1 ;  # Required
    ] ;
    sh:property [
        sh:path ex:description ;
        sh:datatype xsd:string ;
        sh:maxLength 1000 ;
    ] .

# Shape for User class
ex:UserShape a sh:NodeShape ;
    sh:targetClass ex:User ;
    sh:property [
        sh:path ex:email ;
        sh:datatype xsd:string ;
        sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
        sh:minCount 1 ;  # Required
    ] ;
    sh:property [
        sh:path ex:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
    ] .
```

## Step 2: Validate Your Ontology Against SHACL Shapes

Use ggen to validate your ontology:

```bash
ggen graph validate \
  --ontology domain.ttl \
  --shapes ontology-shapes.ttl
```

Successful output:
```
Validation Report
================
Conforms: Yes
Violations: 0
```

If there are violations:
```
Validation Report
================
Conforms: No
Violations: 2

Violation 1:
  Source Shape: ex:ProductShape
  Focus Node: ex:product1
  Result Path: ex:price
  Message: Property sh:minInclusive value less than 0.01

Violation 2:
  Source Shape: ex:UserShape
  Focus Node: ex:user1
  Result Path: ex:email
  Message: Regex pattern does not match
```

## Step 3: Fix Violations in Your Ontology

Based on the validation report, update your ontology (`domain.ttl`):

```turtle
# Before
ex:product1 a ex:Product ;
    ex:productName "Sample Product" ;
    ex:price 0 ;  # Invalid: less than 0.01
    ex:description "A great product" .

# After
ex:product1 a ex:Product ;
    ex:productName "Sample Product" ;
    ex:price 9.99 ;  # Valid: >= 0.01
    ex:description "A great product" .
```

Revalidate:
```bash
ggen graph validate \
  --ontology domain.ttl \
  --shapes ontology-shapes.ttl
```

## Step 4: Integrate Validation into Your Workflow

### Validate Before Code Generation

Create a script to validate before generating code:

```bash
#!/bin/bash
set -e

echo "Validating ontology..."
ggen graph validate \
  --ontology domain.ttl \
  --shapes ontology-shapes.ttl || {
  echo "VALIDATION FAILED: Fix ontology before generation"
  exit 1
}

echo "Ontology valid! Generating code..."
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models
```

Run the script:
```bash
chmod +x validate-and-generate.sh
./validate-and-generate.sh
```

### Validate in CI/CD

Add to your `.github/workflows/ci.yml`:

```yaml
name: Validate Ontology

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install ggen
        run: cargo install ggen
      - name: Validate ontology
        run: |
          ggen graph validate \
            --ontology domain.ttl \
            --shapes ontology-shapes.ttl
```

### Pre-commit Hook

Prevent invalid ontologies from being committed:

```bash
ggen hook create pre-commit --name validate-ontology
```

This runs `ggen graph validate` before each commit.

## Advanced: Custom Validation Rules

Define more complex SHACL constraints:

### Closed Shapes (Disallow Extra Properties)

```turtle
ex:StrictProductShape a sh:NodeShape ;
    sh:targetClass ex:Product ;
    sh:closed true ;
    sh:ignoredProperties (rdf:type) ;
    sh:property [ ... ] .
```

### Class Constraints

```turtle
ex:OrderShape a sh:NodeShape ;
    sh:targetClass ex:Order ;
    sh:property [
        sh:path ex:customer ;
        sh:class ex:User ;  # Must be a User instance
        sh:minCount 1 ;
    ] .
```

### Relationship Validation

```turtle
ex:PostShape a sh:NodeShape ;
    sh:targetClass ex:Post ;
    sh:property [
        sh:path ex:author ;
        sh:nodeKind sh:IRI ;  # Must be an IRI reference
        sh:minCount 1 ;
    ] .
```

### Cardinality Constraints

```turtle
ex:BlogShape a sh:NodeShape ;
    sh:targetClass ex:Blog ;
    sh:property [
        sh:path ex:posts ;
        sh:minCount 1 ;
        sh:maxCount 1000 ;
    ] .
```

## Troubleshooting

### Validation Passes but Code Generation Fails

Ensure your SHACL shapes align with code generation expectations:
- Check that property paths reference actual RDF properties
- Verify datatype mappings match your template
- Validate that cardinality constraints are realistic

### Pattern Matching Issues

Test regex patterns carefully:

```turtle
# Match email addresses
sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
```

Use [regex101.com](https://regex101.com) to test patterns before adding to SHACL.

### Performance Issues with Large Ontologies

For large ontologies, validate specific shapes:

```bash
ggen graph validate \
  --ontology domain.ttl \
  --shapes ontology-shapes.ttl \
  --focus-shapes ex:ProductShape
```

## See Also

- [RDF/SPARQL Reference](../reference/rdf-sparql.md) - Complete SHACL reference
- [Use RDF Ontologies Guide](use-rdf-ontologies.md) - Working with ontologies
- [Create Templates Guide](create-templates.md) - Creating custom templates
