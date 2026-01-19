# OWL → SHACL Transformation Rules

**Complete Reference for LLM-Construct Pattern**

This document specifies the **14+ transformation rules** that map OWL (Web Ontology Language) restrictions to SHACL (Shapes Constraint Language) constraints in the LLM-Construct pipeline.

---

## Table of Contents

1. [Overview](#overview)
2. [Cardinality Constraints](#cardinality-constraints)
3. [Datatype Restrictions](#datatype-restrictions)
4. [Value Constraints](#value-constraints)
5. [String Constraints](#string-constraints)
6. [Numeric Constraints](#numeric-constraints)
7. [Enumeration Constraints](#enumeration-constraints)
8. [Class Constraints](#class-constraints)
9. [Property Path Constraints](#property-path-constraints)
10. [Composite Constraints](#composite-constraints)
11. [Implementation Notes](#implementation-notes)
12. [Examples](#examples)

---

## Overview

### Transformation Pipeline

```
OWL Ontology → OWL Extractor → OWL AST → SHACL Generator → SHACL Shapes → DSPy Mapping
```

### Design Principles

1. **Lossless Transformation**: All OWL constraints preserve semantics in SHACL
2. **SHACL Core Compliance**: Output uses only SHACL Core features (no SPARQL)
3. **Deterministic**: Same OWL input always produces identical SHACL output
4. **Composable**: Multiple restrictions on same property combine correctly

### Notation

**Input (OWL)**:
```turtle
:Class rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :property ;
    owl:RESTRICTION_TYPE VALUE
] .
```

**Output (SHACL)**:
```turtle
:PropertyShape a sh:PropertyShape ;
    sh:path :property ;
    sh:CONSTRAINT_TYPE VALUE .
```

---

## Cardinality Constraints

Control **how many values** a property can have.

### Rule 1: `owl:cardinality` → `sh:minCount` + `sh:maxCount`

**Semantics**: Property must have **exactly N** values.

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:cardinality 1
] .
```

**Output (SHACL)**:
```turtle
:ISINPropertyShape a sh:PropertyShape ;
    sh:path :hasISIN ;
    sh:minCount 1 ;
    sh:maxCount 1 .
```

**Use Cases**:
- Required unique fields (SSN, ISIN, UUID)
- 1-to-1 relationships (Person has exactly 1 birthdate)

**DSPy Mapping**:
```rust
FieldConstraints {
    required: true,       // minCount >= 1
    max_count: Some(1),   // maxCount
    ..Default::default()
}
```

---

### Rule 2: `owl:minCardinality` → `sh:minCount`

**Semantics**: Property must have **at least N** values.

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasIssuer ;
    owl:minCardinality 1
] .
```

**Output (SHACL)**:
```turtle
:IssuerPropertyShape a sh:PropertyShape ;
    sh:path :hasIssuer ;
    sh:minCount 1 .
```

**Use Cases**:
- Required fields (minCardinality 1 = required)
- Minimum list size (Product has at least 1 image)

**DSPy Mapping**:
```rust
FieldConstraints {
    required: true,  // minCount >= 1
    ..Default::default()
}
```

---

### Rule 3: `owl:maxCardinality` → `sh:maxCount`

**Semantics**: Property must have **at most N** values.

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasCouponRate ;
    owl:maxCardinality 1
] .
```

**Output (SHACL)**:
```turtle
:CouponRatePropertyShape a sh:PropertyShape ;
    sh:path :hasCouponRate ;
    sh:maxCount 1 .
```

**Use Cases**:
- Optional unique fields (Person has at most 1 nickname)
- Bounded lists (Bond has at most 3 ratings)

**DSPy Mapping**:
```rust
FieldConstraints {
    max_count: Some(1),
    ..Default::default()
}
```

---

### Rule 4: Combined Cardinality (Min + Max)

**Semantics**: Property must have **between M and N** values.

**Input (OWL)**:
```turtle
:Product rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasImage ;
    owl:minCardinality 1
] , [
    a owl:Restriction ;
    owl:onProperty :hasImage ;
    owl:maxCardinality 5
] .
```

**Output (SHACL)**:
```turtle
:ImagePropertyShape a sh:PropertyShape ;
    sh:path :hasImage ;
    sh:minCount 1 ;
    sh:maxCount 5 .
```

**Use Cases**:
- Bounded lists (1-5 images, 2-10 tags)

**DSPy Mapping**:
```rust
FieldConstraints {
    required: true,
    max_count: Some(5),
    ..Default::default()
}
```

---

## Datatype Restrictions

Enforce **data types** and **facets** (range, pattern, length).

### Rule 5: `owl:onDatatype` → `sh:datatype`

**Semantics**: Property values must be of a specific XSD datatype.

**Input (OWL)**:
```turtle
:hasISIN a owl:DatatypeProperty ;
    rdfs:range xsd:string .

# Or with restriction:
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:string
    ]
] .
```

**Output (SHACL)**:
```turtle
:ISINPropertyShape a sh:PropertyShape ;
    sh:path :hasISIN ;
    sh:datatype xsd:string .
```

**Supported XSD Types**:
- `xsd:string` → String
- `xsd:integer` → Integer
- `xsd:decimal` → Decimal/Float
- `xsd:boolean` → Boolean
- `xsd:date` → Date (ISO 8601)
- `xsd:dateTime` → DateTime
- `xsd:anyURI` → URI

**DSPy Mapping**:
```rust
FieldConstraints {
    datatype: Some("xsd:string".to_string()),
    ..Default::default()
}
```

---

## String Constraints

Control **string length** and **format**.

### Rule 6: `xsd:minLength` → `sh:minLength`

**Semantics**: String must have **at least N** characters.

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :organizationName ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:string ;
        owl:withRestrictions (
            [ xsd:minLength 1 ]
        )
    ]
] .
```

**Output (SHACL)**:
```turtle
:OrganizationNamePropertyShape a sh:PropertyShape ;
    sh:path :organizationName ;
    sh:datatype xsd:string ;
    sh:minLength 1 .
```

**Use Cases**:
- Non-empty strings (name, description)

**DSPy Mapping**:
```rust
FieldConstraints {
    min_length: Some(1),
    ..Default::default()
}
```

---

### Rule 7: `xsd:maxLength` → `sh:maxLength`

**Semantics**: String must have **at most N** characters.

**Input (OWL)**:
```turtle
:Person rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :zipCode ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:string ;
        owl:withRestrictions (
            [ xsd:maxLength 10 ]
        )
    ]
] .
```

**Output (SHACL)**:
```turtle
:ZipCodePropertyShape a sh:PropertyShape ;
    sh:path :zipCode ;
    sh:datatype xsd:string ;
    sh:maxLength 10 .
```

**Use Cases**:
- Bounded strings (ZIP code, abbreviations)

**DSPy Mapping**:
```rust
FieldConstraints {
    max_length: Some(10),
    ..Default::default()
}
```

---

### Rule 8: `xsd:length` → `sh:minLength` + `sh:maxLength`

**Semantics**: String must have **exactly N** characters.

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:string ;
        owl:withRestrictions (
            [ xsd:length 12 ]
        )
    ]
] .
```

**Output (SHACL)**:
```turtle
:ISINPropertyShape a sh:PropertyShape ;
    sh:path :hasISIN ;
    sh:datatype xsd:string ;
    sh:minLength 12 ;
    sh:maxLength 12 .
```

**Use Cases**:
- Fixed-length codes (ISIN 12 chars, SSN 11 chars)

**DSPy Mapping**:
```rust
FieldConstraints {
    min_length: Some(12),
    max_length: Some(12),
    ..Default::default()
}
```

---

### Rule 9: `xsd:pattern` → `sh:pattern`

**Semantics**: String must match **regular expression**.

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:string ;
        owl:withRestrictions (
            [ xsd:pattern "^[A-Z]{2}[A-Z0-9]{9}[0-9]$" ]
        )
    ]
] .
```

**Output (SHACL)**:
```turtle
:ISINPropertyShape a sh:PropertyShape ;
    sh:path :hasISIN ;
    sh:datatype xsd:string ;
    sh:pattern "^[A-Z]{2}[A-Z0-9]{9}[0-9]$" .
```

**Use Cases**:
- Format validation (email, phone, SSN, ISIN)
- Custom formats (license plates, product codes)

**DSPy Mapping**:
```rust
FieldConstraints {
    pattern: Some("^[A-Z]{2}[A-Z0-9]{9}[0-9]$".to_string()),
    ..Default::default()
}
```

**Common Patterns**:
```turtle
# Email
[ xsd:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ]

# Phone (US)
[ xsd:pattern "^\\+?1?\\s*\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}$" ]

# ISO Date
[ xsd:pattern "^\\d{4}-\\d{2}-\\d{2}$" ]

# UUID
[ xsd:pattern "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" ]
```

---

## Numeric Constraints

Control **numeric ranges**.

### Rule 10: `xsd:minInclusive` → `sh:minInclusive`

**Semantics**: Number must be **≥ N**.

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasCouponRate ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:decimal ;
        owl:withRestrictions (
            [ xsd:minInclusive 0.0 ]
        )
    ]
] .
```

**Output (SHACL)**:
```turtle
:CouponRatePropertyShape a sh:PropertyShape ;
    sh:path :hasCouponRate ;
    sh:datatype xsd:decimal ;
    sh:minInclusive 0.0 .
```

**Use Cases**:
- Non-negative numbers (prices, rates, counts)

**DSPy Mapping**:
```rust
FieldConstraints {
    min_value: Some(0.0),
    ..Default::default()
}
```

---

### Rule 11: `xsd:maxInclusive` → `sh:maxInclusive`

**Semantics**: Number must be **≤ N**.

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasCouponRate ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:decimal ;
        owl:withRestrictions (
            [ xsd:maxInclusive 20.0 ]
        )
    ]
] .
```

**Output (SHACL)**:
```turtle
:CouponRatePropertyShape a sh:PropertyShape ;
    sh:path :hasCouponRate ;
    sh:datatype xsd:decimal ;
    sh:maxInclusive 20.0 .
```

**Use Cases**:
- Bounded percentages (0-100%, 0-20% coupon)

**DSPy Mapping**:
```rust
FieldConstraints {
    max_value: Some(20.0),
    ..Default::default()
}
```

---

### Rule 12: `xsd:minExclusive` → `sh:minExclusive`

**Semantics**: Number must be **> N** (strictly greater than).

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasFaceValue ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:decimal ;
        owl:withRestrictions (
            [ xsd:minExclusive 0.0 ]
        )
    ]
] .
```

**Output (SHACL)**:
```turtle
:FaceValuePropertyShape a sh:PropertyShape ;
    sh:path :hasFaceValue ;
    sh:datatype xsd:decimal ;
    sh:minExclusive 0.0 .
```

**Use Cases**:
- Positive numbers (prices, amounts)

**DSPy Mapping**:
```rust
FieldConstraints {
    min_value_exclusive: Some(0.0),
    ..Default::default()
}
```

---

### Rule 13: `xsd:maxExclusive` → `sh:maxExclusive`

**Semantics**: Number must be **< N** (strictly less than).

**Input (OWL)**:
```turtle
:Probability rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasValue ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:decimal ;
        owl:withRestrictions (
            [ xsd:minInclusive 0.0 ]
            [ xsd:maxExclusive 1.0 ]
        )
    ]
] .
```

**Output (SHACL)**:
```turtle
:ProbabilityValuePropertyShape a sh:PropertyShape ;
    sh:path :hasValue ;
    sh:datatype xsd:decimal ;
    sh:minInclusive 0.0 ;
    sh:maxExclusive 1.0 .
```

**Use Cases**:
- Probabilities (0 ≤ p < 1)

**DSPy Mapping**:
```rust
FieldConstraints {
    min_value: Some(0.0),
    max_value_exclusive: Some(1.0),
    ..Default::default()
}
```

---

## Enumeration Constraints

Restrict values to **closed sets**.

### Rule 14: `owl:oneOf` → `sh:in`

**Semantics**: Value must be **one of** a specified list.

**Input (OWL)**:
```turtle
:BondType a rdfs:Datatype ;
    owl:oneOf ("Corporate" "Municipal" "Treasury") .

:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :bondType ;
    owl:allValuesFrom :BondType
] .
```

**Output (SHACL)**:
```turtle
:BondTypePropertyShape a sh:PropertyShape ;
    sh:path :bondType ;
    sh:in ("Corporate" "Municipal" "Treasury") .
```

**Use Cases**:
- Enumerations (status, category, type)
- Controlled vocabularies

**DSPy Mapping**:
```rust
FieldConstraints {
    allowed_values: Some(vec![
        "Corporate".to_string(),
        "Municipal".to_string(),
        "Treasury".to_string(),
    ]),
    ..Default::default()
}
```

**Numeric Enumerations**:
```turtle
:Priority a rdfs:Datatype ;
    owl:oneOf (1 2 3 4 5) .

# SHACL output:
:PriorityPropertyShape sh:in (1 2 3 4 5) .
```

---

## Class Constraints

Enforce **object property ranges**.

### Rule 15: `owl:allValuesFrom` (Class) → `sh:class`

**Semantics**: All values of object property must be instances of a class.

**Input (OWL)**:
```turtle
:hasIssuer a owl:ObjectProperty ;
    rdfs:domain :Bond ;
    rdfs:range :Organization .

# Or with restriction:
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasIssuer ;
    owl:allValuesFrom :Organization
] .
```

**Output (SHACL)**:
```turtle
:IssuerPropertyShape a sh:PropertyShape ;
    sh:path :hasIssuer ;
    sh:class :Organization .
```

**Use Cases**:
- Typed relationships (Bond has Issuer of type Organization)

**DSPy Mapping**:
```rust
FieldConstraints {
    semantic_type: Some("http://example.com#Organization".to_string()),
    ..Default::default()
}
```

---

### Rule 16: `owl:someValuesFrom` → `sh:qualifiedValueShape`

**Semantics**: At least one value must be of a specific class.

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasRating ;
    owl:someValuesFrom :InvestmentGradeRating
] .
```

**Output (SHACL)**:
```turtle
:RatingPropertyShape a sh:PropertyShape ;
    sh:path :hasRating ;
    sh:qualifiedValueShape [
        sh:class :InvestmentGradeRating
    ] ;
    sh:qualifiedMinCount 1 .
```

**Use Cases**:
- Existential constraints (Bond has at least 1 investment-grade rating)

**DSPy Mapping**: Requires custom validator.

---

## Property Path Constraints

Handle **complex property paths**.

### Rule 17: Property Chains (Transitive Properties)

**Input (OWL)**:
```turtle
:locatedIn a owl:ObjectProperty ;
    owl:propertyChainAxiom ( :issuedBy :hasHeadquarters ) .

# Bond :issuedBy Organization :hasHeadquarters Location
# → Bond :locatedIn Location (inferred)
```

**Output (SHACL)**:
```turtle
# Not directly expressible in SHACL Core
# Recommendation: Materialize property chain before SHACL generation
```

**Workaround**: Use SPARQL CONSTRUCT to materialize inferred triples:

```sparql
CONSTRUCT {
    ?bond :locatedIn ?location .
}
WHERE {
    ?bond :issuedBy ?org .
    ?org :hasHeadquarters ?location .
}
```

---

## Composite Constraints

Combine multiple constraints on the **same property**.

### Example: ISIN with Multiple Facets

**Input (OWL)**:
```turtle
:Bond rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:cardinality 1
] , [
    a owl:Restriction ;
    owl:onProperty :hasISIN ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:string ;
        owl:withRestrictions (
            [ xsd:length 12 ]
            [ xsd:pattern "^[A-Z]{2}[A-Z0-9]{9}[0-9]$" ]
        )
    ]
] .
```

**Output (SHACL)**:
```turtle
:ISINPropertyShape a sh:PropertyShape ;
    sh:path :hasISIN ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:minLength 12 ;
    sh:maxLength 12 ;
    sh:pattern "^[A-Z]{2}[A-Z0-9]{9}[0-9]$" .
```

**DSPy Mapping**:
```rust
FieldConstraints {
    required: true,
    max_count: Some(1),
    min_length: Some(12),
    max_length: Some(12),
    pattern: Some("^[A-Z]{2}[A-Z0-9]{9}[0-9]$".to_string()),
    datatype: Some("xsd:string".to_string()),
}
```

---

## Implementation Notes

### Unsupported OWL Constructs

The following OWL features are **not yet supported** in SHACL generation:

| OWL Construct | Reason | Workaround |
|---------------|--------|------------|
| `owl:hasValue` | No direct SHACL equivalent | Use `sh:hasValue` manually |
| `owl:disjointWith` | Class-level, not property | Validate at ontology level |
| `owl:complementOf` | Complex reasoning | Use custom SPARQL validator |
| `owl:unionOf` | Multiple types per property | Split into multiple properties |
| `owl:intersectionOf` | Complex reasoning | Materialize in SPARQL |

**Future Support**: File issues at https://github.com/seanchatmangpt/ggen/issues

### Constraint Composition Rules

When multiple restrictions apply to the **same property**, combine them:

**Rule**: Conjunctive (AND) semantics
```turtle
# OWL:
:Bond rdfs:subClassOf RESTRICTION_1, RESTRICTION_2, RESTRICTION_3 .

# SHACL:
:PropertyShape
    sh:CONSTRAINT_1 VALUE_1 ;
    sh:CONSTRAINT_2 VALUE_2 ;
    sh:CONSTRAINT_3 VALUE_3 .
```

**Exception**: Conflicting constraints (e.g., `minInclusive 10` AND `maxInclusive 5`) raise a validation error.

### SHACL Severity Levels

All generated constraints use **`sh:Violation`** severity by default.

**Custom severity** (optional):
```turtle
# In OWL:
:Bond llm:constraintSeverity llm:Warning .

# Generated SHACL:
:PropertyShape sh:severity sh:Warning .
```

---

## Examples

### Example 1: Email Address Validation

**OWL**:
```turtle
:Person rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :email ;
    owl:cardinality 1
] , [
    a owl:Restriction ;
    owl:onProperty :email ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:string ;
        owl:withRestrictions (
            [ xsd:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ]
        )
    ]
] .
```

**SHACL**:
```turtle
:EmailPropertyShape a sh:PropertyShape ;
    sh:path :email ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" .
```

---

### Example 2: Product Price (Positive Decimal)

**OWL**:
```turtle
:Product rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :price ;
    owl:minCardinality 1
] , [
    a owl:Restriction ;
    owl:onProperty :price ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:decimal ;
        owl:withRestrictions (
            [ xsd:minExclusive 0.0 ]
        )
    ]
] .
```

**SHACL**:
```turtle
:PricePropertyShape a sh:PropertyShape ;
    sh:path :price ;
    sh:datatype xsd:decimal ;
    sh:minCount 1 ;
    sh:minExclusive 0.0 .
```

---

### Example 3: Multi-Value Tags with Limits

**OWL**:
```turtle
:Article rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :tag ;
    owl:minCardinality 1
] , [
    a owl:Restriction ;
    owl:onProperty :tag ;
    owl:maxCardinality 10
] , [
    a owl:Restriction ;
    owl:onProperty :tag ;
    owl:allValuesFrom [
        a rdfs:Datatype ;
        owl:onDatatype xsd:string ;
        owl:withRestrictions (
            [ xsd:minLength 2 ]
            [ xsd:maxLength 50 ]
        )
    ]
] .
```

**SHACL**:
```turtle
:TagPropertyShape a sh:PropertyShape ;
    sh:path :tag ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:maxCount 10 ;
    sh:minLength 2 ;
    sh:maxLength 50 .
```

---

### Example 4: Enumeration with Default

**OWL**:
```turtle
:OrderStatus a rdfs:Datatype ;
    owl:oneOf ("Pending" "Approved" "Shipped" "Delivered" "Cancelled") .

:Order rdfs:subClassOf [
    a owl:Restriction ;
    owl:onProperty :status ;
    owl:cardinality 1
] , [
    a owl:Restriction ;
    owl:onProperty :status ;
    owl:allValuesFrom :OrderStatus
] .

:status llm:defaultValue "Pending" .
```

**SHACL**:
```turtle
:StatusPropertyShape a sh:PropertyShape ;
    sh:path :status ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:in ("Pending" "Approved" "Shipped" "Delivered" "Cancelled") ;
    sh:defaultValue "Pending" .
```

---

## Summary Table

| OWL Restriction | SHACL Constraint | DSPy Field |
|-----------------|------------------|------------|
| `owl:cardinality N` | `sh:minCount N`, `sh:maxCount N` | `required: true`, `max_count: Some(N)` |
| `owl:minCardinality N` | `sh:minCount N` | `required: true` (if N≥1) |
| `owl:maxCardinality N` | `sh:maxCount N` | `max_count: Some(N)` |
| `xsd:length N` | `sh:minLength N`, `sh:maxLength N` | `min_length`, `max_length` |
| `xsd:minLength N` | `sh:minLength N` | `min_length: Some(N)` |
| `xsd:maxLength N` | `sh:maxLength N` | `max_length: Some(N)` |
| `xsd:pattern R` | `sh:pattern R` | `pattern: Some(R)` |
| `xsd:minInclusive N` | `sh:minInclusive N` | `min_value: Some(N)` |
| `xsd:maxInclusive N` | `sh:maxInclusive N` | `max_value: Some(N)` |
| `xsd:minExclusive N` | `sh:minExclusive N` | `min_value_exclusive: Some(N)` |
| `xsd:maxExclusive N` | `sh:maxExclusive N` | `max_value_exclusive: Some(N)` |
| `owl:oneOf (...)` | `sh:in (...)` | `allowed_values: Some(vec![...])` |
| `owl:allValuesFrom Class` | `sh:class Class` | `semantic_type: Some(Class)` |
| `owl:onDatatype Type` | `sh:datatype Type` | `datatype: Some(Type)` |

---

## References

- **OWL 2 Datatypes**: https://www.w3.org/TR/owl2-syntax/#Datatype_Maps
- **SHACL Core Constraints**: https://www.w3.org/TR/shacl/#core-components
- **XSD Facets**: https://www.w3.org/TR/xmlschema-2/#facets
- **ggen OWL Extractor**: `/home/user/ggen/crates/ggen-ai/src/owl/extractor.rs`
- **ggen SHACL Generator**: `/home/user/ggen/crates/ggen-ai/src/owl/shacl_generator.rs`

---

**Next**: See [LLM-Construct Tutorial](/home/user/ggen/docs/tutorials/LLM_CONSTRUCT_TUTORIAL.md) for step-by-step usage examples.
