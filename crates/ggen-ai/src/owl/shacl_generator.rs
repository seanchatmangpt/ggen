//! SHACL Generator - Transform OWL restrictions into SHACL shapes
//!
//! This module generates SHACL (Shapes Constraint Language) NodeShapes and PropertyShapes
//! from OWL class definitions and restrictions.

use crate::error::Result;
use crate::owl::extractor::{DatatypeFacet, OWLClass, OWLProperty, OWLRestriction};

/// SHACL shape generated from OWL class
#[derive(Debug, Clone)]
pub struct GeneratedShape {
    pub node_shape_uri: String,
    pub target_class: String,
    pub property_shapes: Vec<PropertyShape>,
}

/// SHACL PropertyShape with all constraints
#[derive(Debug, Clone)]
pub struct PropertyShape {
    pub path: String,
    pub datatype: Option<String>,
    pub class: Option<String>,
    pub min_count: Option<u32>,
    pub max_count: Option<u32>,
    pub min_length: Option<u32>,
    pub max_length: Option<u32>,
    pub pattern: Option<String>,
    pub min_inclusive: Option<f64>,
    pub max_inclusive: Option<f64>,
    pub min_exclusive: Option<f64>,
    pub max_exclusive: Option<f64>,
}

/// SHACL Generator - transforms OWL to SHACL
pub struct SHACLGenerator;

impl SHACLGenerator {
    /// Transform OWL class to SHACL NodeShape
    pub fn generate_shape(owl_class: &OWLClass) -> Result<GeneratedShape> {
        let node_shape_uri = format!("{}Shape", owl_class.uri.as_str());
        let target_class = owl_class.uri.as_str().to_string();

        let mut property_shapes = Vec::new();

        // Generate PropertyShape for each property
        for prop in &owl_class.properties {
            let prop_shape = Self::generate_property_shape(prop, &owl_class.restrictions)?;
            property_shapes.push(prop_shape);
        }

        Ok(GeneratedShape {
            node_shape_uri,
            target_class,
            property_shapes,
        })
    }

    /// Generate PropertyShape for a single property
    fn generate_property_shape(
        property: &OWLProperty,
        restrictions: &[OWLRestriction],
    ) -> Result<PropertyShape> {
        let (path, datatype, class) = match property {
            OWLProperty::DatatypeProperty { uri, range, .. } => {
                (uri.as_str().to_string(), Some(range.as_str().to_string()), None)
            }
            OWLProperty::ObjectProperty { uri, range, .. } => {
                (uri.as_str().to_string(), None, Some(range.as_str().to_string()))
            }
        };

        let mut shape = PropertyShape {
            path,
            datatype,
            class,
            min_count: None,
            max_count: None,
            min_length: None,
            max_length: None,
            pattern: None,
            min_inclusive: None,
            max_inclusive: None,
            min_exclusive: None,
            max_exclusive: None,
        };

        // Apply restrictions from OWL
        for restriction in restrictions {
            Self::apply_restriction(&mut shape, restriction)?;
        }

        Ok(shape)
    }

    /// Apply OWL restriction to PropertyShape
    fn apply_restriction(shape: &mut PropertyShape, restriction: &OWLRestriction) -> Result<()> {
        match restriction {
            OWLRestriction::Cardinality { property, min, max } => {
                if property.as_str() == shape.path {
                    shape.min_count = *min;
                    shape.max_count = *max;
                }
            }
            OWLRestriction::DatatypeRestriction {
                property, facets, ..
            } => {
                if property.as_str() == shape.path {
                    for facet in facets {
                        match facet {
                            DatatypeFacet::MinLength(n) => shape.min_length = Some(*n),
                            DatatypeFacet::MaxLength(n) => shape.max_length = Some(*n),
                            DatatypeFacet::Length(n) => {
                                shape.min_length = Some(*n);
                                shape.max_length = Some(*n);
                            }
                            DatatypeFacet::Pattern(p) => shape.pattern = Some(p.clone()),
                            DatatypeFacet::MinInclusive(v) => shape.min_inclusive = Some(*v),
                            DatatypeFacet::MaxInclusive(v) => shape.max_inclusive = Some(*v),
                            DatatypeFacet::MinExclusive(v) => shape.min_exclusive = Some(*v),
                            DatatypeFacet::MaxExclusive(v) => shape.max_exclusive = Some(*v),
                        }
                    }
                }
            }
            OWLRestriction::ValueRestriction { .. } => {
                // Value restrictions handled in future phases
            }
        }
        Ok(())
    }

    /// Serialize generated shape to Turtle format
    pub fn to_turtle(shape: &GeneratedShape) -> Result<String> {
        let mut ttl = String::new();

        // Prefixes
        ttl.push_str("@prefix sh: <http://www.w3.org/ns/shacl#> .\n");
        ttl.push_str("@prefix : <http://example.com/> .\n\n");

        // NodeShape
        ttl.push_str(&format!(
            ":{} a sh:NodeShape ;\n",
            Self::local_name(&shape.node_shape_uri)
        ));
        ttl.push_str(&format!("    sh:targetClass <{}> ;\n", shape.target_class));

        if !shape.property_shapes.is_empty() {
            ttl.push_str("    sh:property ");

            for (i, prop_shape) in shape.property_shapes.iter().enumerate() {
                if i > 0 {
                    ttl.push_str(" ,\n                ");
                }
                ttl.push_str(&format!(
                    ":{}PropertyShape",
                    Self::local_name(&prop_shape.path)
                ));
            }
            ttl.push_str(" .\n\n");

            // Individual PropertyShape definitions
            for prop_shape in &shape.property_shapes {
                ttl.push_str(&Self::property_shape_to_turtle(prop_shape)?);
                ttl.push_str("\n");
            }
        } else {
            ttl.push_str(" .\n");
        }

        Ok(ttl)
    }

    /// Convert PropertyShape to Turtle
    fn property_shape_to_turtle(shape: &PropertyShape) -> Result<String> {
        let mut ttl = String::new();
        let name = Self::local_name(&shape.path);

        ttl.push_str(&format!(":{}PropertyShape a sh:PropertyShape ;\n", name));
        ttl.push_str(&format!("    sh:path <{}> ", shape.path));

        let mut has_constraints = false;

        if let Some(ref dt) = shape.datatype {
            ttl.push_str(&format!(";\n    sh:datatype <{}>", dt));
            has_constraints = true;
        }
        if let Some(ref cls) = shape.class {
            ttl.push_str(&format!(";\n    sh:class <{}>", cls));
            has_constraints = true;
        }
        if let Some(n) = shape.min_count {
            ttl.push_str(&format!(";\n    sh:minCount {}", n));
            has_constraints = true;
        }
        if let Some(n) = shape.max_count {
            ttl.push_str(&format!(";\n    sh:maxCount {}", n));
            has_constraints = true;
        }
        if let Some(n) = shape.min_length {
            ttl.push_str(&format!(";\n    sh:minLength {}", n));
            has_constraints = true;
        }
        if let Some(n) = shape.max_length {
            ttl.push_str(&format!(";\n    sh:maxLength {}", n));
            has_constraints = true;
        }
        if let Some(ref p) = shape.pattern {
            ttl.push_str(&format!(";\n    sh:pattern \"{}\"", p));
            has_constraints = true;
        }
        if let Some(v) = shape.min_inclusive {
            ttl.push_str(&format!(";\n    sh:minInclusive {}", v));
            has_constraints = true;
        }
        if let Some(v) = shape.max_inclusive {
            ttl.push_str(&format!(";\n    sh:maxInclusive {}", v));
            has_constraints = true;
        }
        if let Some(v) = shape.min_exclusive {
            ttl.push_str(&format!(";\n    sh:minExclusive {}", v));
            has_constraints = true;
        }
        if let Some(v) = shape.max_exclusive {
            ttl.push_str(&format!(";\n    sh:maxExclusive {}", v));
            has_constraints = true;
        }

        if !has_constraints {
            ttl.push_str(" .\n");
        } else {
            ttl.push_str(" .\n");
        }

        Ok(ttl)
    }

    /// Extract local name from URI
    fn local_name(uri: &str) -> String {
        uri.split(&['#', '/'][..])
            .last()
            .unwrap_or("")
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxigraph::model::NamedNode;

    #[test]
    fn test_generate_empty_shape() {
        let owl_class = OWLClass {
            uri: NamedNode::new("http://example.com/TestClass").expect("Invalid URI"),
            label: Some("Test Class".to_string()),
            comment: None,
            properties: Vec::new(),
            restrictions: Vec::new(),
        };

        let shape = SHACLGenerator::generate_shape(&owl_class).expect("Generation failed");
        assert_eq!(shape.target_class, "http://example.com/TestClass");
        assert!(shape.property_shapes.is_empty());
    }

    #[test]
    fn test_local_name_extraction() {
        assert_eq!(SHACLGenerator::local_name("http://example.com/TestClass"), "TestClass");
        assert_eq!(SHACLGenerator::local_name("http://example.com#TestClass"), "TestClass");
        assert_eq!(SHACLGenerator::local_name("TestClass"), "TestClass");
    }

    #[test]
    fn test_to_turtle_empty_shape() {
        let shape = GeneratedShape {
            node_shape_uri: "http://example.com/TestClassShape".to_string(),
            target_class: "http://example.com/TestClass".to_string(),
            property_shapes: Vec::new(),
        };

        let ttl = SHACLGenerator::to_turtle(&shape).expect("Serialization failed");
        assert!(ttl.contains("sh:NodeShape"));
        assert!(ttl.contains("sh:targetClass"));
    }
}
