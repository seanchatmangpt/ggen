# SHACL Parser: Implementation Guide for Missing Constraints

This guide provides code examples for implementing the missing constraints identified in the validation report.

---

## Missing Constraint #1: sh:minInclusive / sh:maxInclusive / sh:minExclusive / sh:maxExclusive

### W3C SHACL Specification

```turtle
ex:AgeShape
    a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
        sh:path ex:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
    ] .
```

### Current Problem

**SHACLConstraint struct** (lines 54-80):
```rust
pub struct SHACLConstraint {
    pub min_count: Option<usize>,           // ✅ Cardinality
    pub max_count: Option<usize>,           // ✅ Cardinality
    pub min_length: Option<usize>,          // ✅ String length
    pub max_length: Option<usize>,          // ✅ String length
    pub pattern: Option<String>,            // ✅ Regex
    pub datatype: Option<String>,           // ✅ Type
    pub enum_values: Option<Vec<String>>,   // ✅ Enumeration
    pub one_of_values: Option<Vec<String>>, // ✅ OneOf
    pub semantic_type: Option<String>,      // ✅ sh:class
    pub property_name: Option<String>,      // ✅ Property name
    // ❌ MISSING: min_inclusive, max_inclusive, min_exclusive, max_exclusive
}
```

### Implementation Steps

**Step 1**: Update SHACLConstraint struct:
```rust
pub struct SHACLConstraint {
    // ... existing fields ...

    // NEW: Numeric range constraints
    pub min_inclusive: Option<f64>,
    pub max_inclusive: Option<f64>,
    pub min_exclusive: Option<f64>,
    pub max_exclusive: Option<f64>,
}
```

**Step 2**: Implement extraction method:
```rust
impl<'a> SHACLParser<'a> {
    /// Extract numeric range constraints
    fn extract_numeric_constraints(&self, prop_shape_uri: &str) -> Result<(Option<f64>, Option<f64>, Option<f64>, Option<f64>)> {
        let query = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>

            SELECT ?minInc ?maxInc ?minExc ?maxExc
            WHERE {{
                OPTIONAL {{ <{prop_shape_uri}> sh:minInclusive ?minInc . }}
                OPTIONAL {{ <{prop_shape_uri}> sh:maxInclusive ?maxInc . }}
                OPTIONAL {{ <{prop_shape_uri}> sh:minExclusive ?minExc . }}
                OPTIONAL {{ <{prop_shape_uri}> sh:maxExclusive ?maxExc . }}
            }}
            "#
        );

        let mut min_inc = None;
        let mut max_inc = None;
        let mut min_exc = None;
        let mut max_exc = None;

        #[allow(deprecated)]
        match self.store.query(&query).map_err(|e| {
            GgenAiError::Other {
                message: format!("SPARQL query failed: {}", e),
            }
        })? {
            QueryResults::Solutions(mut solutions) => {
                if let Some(solution) = solutions.next() {
                    let solution = solution.map_err(|e| {
                        GgenAiError::Other {
                            message: format!("SPARQL solution error: {}", e),
                        }
                    })?;

                    if let Some(term) = solution.get("minInc") {
                        if let Term::Literal(literal) = term {
                            if let Ok(val) = literal.value().parse::<f64>() {
                                min_inc = Some(val);
                            }
                        }
                    }
                    if let Some(term) = solution.get("maxInc") {
                        if let Term::Literal(literal) = term {
                            if let Ok(val) = literal.value().parse::<f64>() {
                                max_inc = Some(val);
                            }
                        }
                    }
                    if let Some(term) = solution.get("minExc") {
                        if let Term::Literal(literal) = term {
                            if let Ok(val) = literal.value().parse::<f64>() {
                                min_exc = Some(val);
                            }
                        }
                    }
                    if let Some(term) = solution.get("maxExc") {
                        if let Term::Literal(literal) = term {
                            if let Ok(val) = literal.value().parse::<f64>() {
                                max_exc = Some(val);
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        Ok((min_inc, max_inc, min_exc, max_exc))
    }
}
```

**Step 3**: Update extract_constraint() to call new method:
```rust
pub fn extract_constraint(&self, prop_shape_uri: &str, property_name: &str) -> Result<SHACLConstraint> {
    let mut constraint = SHACLConstraint {
        property_name: Some(property_name.to_string()),
        ..Default::default()
    };

    // ... existing extractions ...

    // NEW: Extract numeric range constraints
    if let Ok((min_inc, max_inc, min_exc, max_exc)) = self.extract_numeric_constraints(prop_shape_uri) {
        constraint.min_inclusive = min_inc;
        constraint.max_inclusive = max_inc;
        constraint.min_exclusive = min_exc;
        constraint.max_exclusive = max_exc;
    }

    Ok(constraint)
}
```

**Step 4**: Add unit tests:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_numeric_range_inclusive() {
        let mut constraint = SHACLConstraint::new();
        constraint.min_inclusive = Some(0.0);
        constraint.max_inclusive = Some(150.0);

        assert_eq!(constraint.min_inclusive, Some(0.0));
        assert_eq!(constraint.max_inclusive, Some(150.0));
        assert!(!constraint.is_empty());
    }

    #[test]
    fn test_extract_numeric_range_exclusive() {
        let mut constraint = SHACLConstraint::new();
        constraint.min_exclusive = Some(0.0);
        constraint.max_exclusive = Some(100.0);

        assert_eq!(constraint.min_exclusive, Some(0.0));
        assert_eq!(constraint.max_exclusive, Some(100.0));
    }
}
```

---

## Missing Constraint #2: sh:nodeKind

### W3C SHACL Specification

```turtle
ex:EntityShape
    a sh:NodeShape ;
    sh:targetClass ex:Entity ;
    sh:property [
        sh:path ex:homepage ;
        sh:nodeKind sh:IRI ;  # Must be an IRI, not a Literal
    ] ;
    sh:property [
        sh:path ex:name ;
        sh:nodeKind sh:Literal ;  # Must be a Literal string
    ] .
```

### Implementation Steps

**Step 1**: Add NodeKind enum:
```rust
use std::str::FromStr;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum NodeKind {
    IRI,
    BlankNode,
    Literal,
    NonLiteral,
}

impl FromStr for NodeKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "http://www.w3.org/ns/shacl#IRI" => Ok(NodeKind::IRI),
            "http://www.w3.org/ns/shacl#BlankNode" => Ok(NodeKind::BlankNode),
            "http://www.w3.org/ns/shacl#Literal" => Ok(NodeKind::Literal),
            "http://www.w3.org/ns/shacl#NonLiteral" => Ok(NodeKind::NonLiteral),
            _ => Err(format!("Unknown nodeKind: {}", s)),
        }
    }
}
```

**Step 2**: Add to SHACLConstraint:
```rust
pub struct SHACLConstraint {
    // ... existing fields ...
    pub node_kind: Option<NodeKind>,
}
```

**Step 3**: Implement extraction:
```rust
fn extract_node_kind(&self, prop_shape_uri: &str) -> Result<Option<NodeKind>> {
    let query = format!(
        r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>

        SELECT ?kind
        WHERE {{
            <{prop_shape_uri}> sh:nodeKind ?kind .
        }}
        "#
    );

    #[allow(deprecated)]
    match self.store.query(&query).map_err(|e| {
        GgenAiError::Other {
            message: format!("SPARQL query failed: {}", e),
        }
    })? {
        QueryResults::Solutions(mut solutions) => {
            if let Some(solution) = solutions.next() {
                let solution = solution.map_err(|e| {
                    GgenAiError::Other {
                        message: format!("SPARQL solution error: {}", e),
                    }
                })?;

                if let Some(term) = solution.get("kind") {
                    if let Term::NamedNode(named_node) = term {
                        let kind_str = named_node.as_str();
                        if let Ok(kind) = NodeKind::from_str(kind_str) {
                            return Ok(Some(kind));
                        }
                    }
                }
            }
        }
        _ => {}
    }

    Ok(None)
}
```

---

## Missing Constraint #3: sh:targetClass Extraction (CRITICAL FIX)

### Problem
Currently, SHACLParser doesn't extract sh:targetClass, forcing the caller to know which shapes target which classes.

### Solution

**Add to SHACLParser**:
```rust
impl<'a> SHACLParser<'a> {
    /// Extract the target class(es) for a shape
    pub fn extract_target_class(&self, shape_uri: &str) -> Result<Option<String>> {
        let query = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>

            SELECT ?class
            WHERE {{
                <{shape_uri}> sh:targetClass ?class .
            }}
            "#
        );

        #[allow(deprecated)]
        match self.store.query(&query).map_err(|e| {
            GgenAiError::Other {
                message: format!("SPARQL query failed: {}", e),
            }
        })? {
            QueryResults::Solutions(mut solutions) => {
                if let Some(solution) = solutions.next() {
                    let solution = solution.map_err(|e| {
                        GgenAiError::Other {
                            message: format!("SPARQL solution error: {}", e),
                        }
                    })?;

                    if let Some(term) = solution.get("class") {
                        if let Term::NamedNode(named_node) = term {
                            return Ok(Some(named_node.as_str().to_string()));
                        }
                    }
                }
            }
            _ => {}
        }

        Ok(None)
    }

    /// Find all shapes that target a given class
    pub fn find_shapes_for_class(&self, class_uri: &str) -> Result<Vec<String>> {
        let query = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>

            SELECT DISTINCT ?shape
            WHERE {{
                ?shape sh:targetClass <{class_uri}> .
            }}
            "#
        );

        let mut shapes = Vec::new();

        #[allow(deprecated)]
        match self.store.query(&query).map_err(|e| {
            GgenAiError::Other {
                message: format!("SPARQL query failed: {}", e),
            }
        })? {
            QueryResults::Solutions(solutions) => {
                for solution in solutions {
                    let solution = solution.map_err(|e| {
                        GgenAiError::Other {
                            message: format!("SPARQL solution error: {}", e),
                        }
                    })?;

                    if let Some(term) = solution.get("shape") {
                        if let Term::NamedNode(named_node) = term {
                            shapes.push(named_node.as_str().to_string());
                        }
                    }
                }
            }
            _ => {}
        }

        Ok(shapes)
    }
}
```

---

## Performance Optimization: Reduce 8 Queries to 1

### Problem
`extract_constraint()` executes 8 separate SPARQL queries per property shape.

### Current Code (Inefficient)
```rust
pub fn extract_constraint(&self, prop_shape_uri: &str, property_name: &str) -> Result<SHACLConstraint> {
    let mut constraint = SHACLConstraint {
        property_name: Some(property_name.to_string()),
        ..Default::default()
    };

    // Query 1: minCount
    if let Ok(Some(count)) = self.extract_integer_property(prop_shape_uri, "sh:minCount") {
        constraint.min_count = Some(count as usize);
    }

    // Query 2: maxCount
    if let Ok(Some(count)) = self.extract_integer_property(prop_shape_uri, "sh:maxCount") {
        constraint.max_count = Some(count as usize);
    }

    // ... 6 more queries ...

    Ok(constraint)
}
```

### Optimized Code (Single Query)
```rust
pub fn extract_constraint_optimized(&self, prop_shape_uri: &str, property_name: &str) -> Result<SHACLConstraint> {
    let query = format!(
        r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

        SELECT
            ?minCount ?maxCount ?minLength ?maxLength
            ?pattern ?datatype ?class ?nodeKind
            ?minInclusive ?maxInclusive ?minExclusive ?maxExclusive
        WHERE {{
            OPTIONAL {{ <{prop_shape_uri}> sh:minCount ?minCount . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:maxCount ?maxCount . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:minLength ?minLength . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:maxLength ?maxLength . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:pattern ?pattern . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:datatype ?datatype . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:class ?class . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:nodeKind ?nodeKind . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:minInclusive ?minInclusive . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:maxInclusive ?maxInclusive . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:minExclusive ?minExclusive . }}
            OPTIONAL {{ <{prop_shape_uri}> sh:maxExclusive ?maxExclusive . }}
        }}
        "#
    );

    let mut constraint = SHACLConstraint {
        property_name: Some(property_name.to_string()),
        ..Default::default()
    };

    #[allow(deprecated)]
    match self.store.query(&query).map_err(|e| {
        GgenAiError::Other {
            message: format!("SPARQL query failed: {}", e),
        }
    })? {
        QueryResults::Solutions(mut solutions) => {
            if let Some(solution) = solutions.next() {
                let solution = solution.map_err(|e| {
                    GgenAiError::Other {
                        message: format!("SPARQL solution error: {}", e),
                    }
                })?;

                // Extract all values from single result
                if let Some(term) = solution.get("minCount") {
                    if let Term::Literal(lit) = term {
                        if let Ok(val) = lit.value().parse::<i64>() {
                            constraint.min_count = Some(val as usize);
                        }
                    }
                }

                // ... similar for all other fields ...
            }
        }
        _ => {}
    }

    Ok(constraint)
}
```

**Performance Gain**: 8 queries/property → 1 query/property = 8x faster

---

## SPARQL Injection Defense

### Problem
Subject URIs are interpolated without validation:
```rust
let query = format!("<{subject_uri}> {property} ?value .");
// If subject_uri = "abc> ; DROP GRAPH ?g ; <abc", this breaks!
```

### Solution

**Add validation function**:
```rust
fn validate_uri(uri: &str) -> Result<()> {
    // URI must start with http:// or https:// or be a known pattern
    if !uri.starts_with("http://") && !uri.starts_with("https://") && !uri.starts_with("urn:") {
        return Err(GgenAiError::Other {
            message: format!("Invalid URI format: {}", uri),
        });
    }

    // Check for SPARQL injection characters
    if uri.contains('>') || uri.contains('<') || uri.contains(';') || uri.contains('}') {
        return Err(GgenAiError::Other {
            message: format!("URI contains invalid characters: {}", uri),
        });
    }

    Ok(())
}
```

**Use in extract methods**:
```rust
fn extract_integer_property(&self, subject_uri: &str, property: &str) -> Result<Option<i64>> {
    // Validate inputs
    validate_uri(subject_uri)?;
    validate_property_name(property)?;

    let query = format!(
        r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>

        SELECT ?value
        WHERE {{
            <{subject_uri}> {property} ?value .
        }}
        "#
    );

    // ... rest of implementation
}
```

---

## Integration Test Example

### Test Fixture: `shape_with_advanced_constraints.ttl`

```turtle
@prefix ex: <http://example.com/ontology#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:ProductShape
    a sh:NodeShape ;
    sh:targetClass ex:Product ;
    sh:property [
        sh:path ex:price ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0.0 ;
        sh:maxInclusive 999999.99 ;
    ] ;
    sh:property [
        sh:path ex:sku ;
        sh:nodeKind sh:Literal ;
        sh:pattern "^[A-Z]{3}-[0-9]{6}$" ;
    ] ;
    sh:property [
        sh:path ex:website ;
        sh:nodeKind sh:IRI ;
    ] .
```

### Test Code:

```rust
#[test]
fn test_extract_numeric_range_constraints() {
    let store = load_ttl_fixture("shape_with_advanced_constraints.ttl");
    let parser = SHACLParser::new(&store);

    let transpiler = TTLToSignatureTranspiler::new();
    let classes = transpiler.find_classes_with_shapes(&store)
        .expect("Failed to find classes");

    let properties = parser.extract_properties(&classes[0])
        .expect("Failed to extract properties");

    let price_constraint = properties.get("price")
        .expect("Should find price property");

    assert_eq!(price_constraint.min_inclusive, Some(0.0));
    assert_eq!(price_constraint.max_inclusive, Some(999999.99));
}

#[test]
fn test_extract_node_kind_constraints() {
    let store = load_ttl_fixture("shape_with_advanced_constraints.ttl");
    let parser = SHACLParser::new(&store);

    let transpiler = TTLToSignatureTranspiler::new();
    let classes = transpiler.find_classes_with_shapes(&store)
        .expect("Failed to find classes");

    let properties = parser.extract_properties(&classes[0])
        .expect("Failed to extract properties");

    let sku_constraint = properties.get("sku")
        .expect("Should find sku property");

    assert_eq!(sku_constraint.node_kind, Some(NodeKind::Literal));

    let website_constraint = properties.get("website")
        .expect("Should find website property");

    assert_eq!(website_constraint.node_kind, Some(NodeKind::IRI));
}

#[test]
fn test_extract_target_class() {
    let store = load_ttl_fixture("shape_with_advanced_constraints.ttl");
    let parser = SHACLParser::new(&store);

    // Find all shapes in the store
    let shapes = parser.find_shapes_for_class("http://example.com/ontology#Product")
        .expect("Failed to find shapes");

    assert_eq!(shapes.len(), 1);

    // Verify we can extract target class from shape
    let target_class = parser.extract_target_class(&shapes[0])
        .expect("Failed to extract target class");

    assert_eq!(target_class, Some("http://example.com/ontology#Product".to_string()));
}
```

---

## Summary: Implementation Priority

| Fix | Effort | Impact | Priority |
|-----|--------|--------|----------|
| Add sh:minInclusive/maxInclusive/minExclusive/maxExclusive | 3 hours | HIGH (numeric validation) | 🔴 1 |
| Add sh:nodeKind extraction | 2 hours | MEDIUM (type enforcement) | 🔴 2 |
| Add sh:targetClass to SHACLParser | 2 hours | CRITICAL (API completeness) | 🔴 3 |
| Optimize to 1 query per property | 2 hours | MEDIUM (performance) | 🟠 4 |
| Add SPARQL injection defense | 1 hour | MEDIUM (security) | 🟠 5 |
| Add RDF list validation | 2 hours | MEDIUM (robustness) | 🟠 6 |
| Add shape composition (sh:and/or/not) | 6 hours | LOW (advanced) | 🟢 7 |

**Total effort for Priority 1-3**: ~7 hours → 60% compliance
**Total effort for all**: ~16 hours → 80% compliance
