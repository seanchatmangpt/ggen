//! SHACL Constraint Extraction
//!
//! This module extracts validation constraints from RDF SHACL shape definitions.
//!
//! SHACL (Shapes Constraint Language) provides a way to describe and validate RDF graphs.
//! This parser reads sh:property shapes and extracts constraints that can be used for
//! code generation and validation.
//!
//! ## Supported Constraints
//!
//! - **Cardinality**: sh:minCount, sh:maxCount
//! - **String length**: sh:minLength, sh:maxLength
//! - **Pattern matching**: sh:pattern (regex)
//! - **Type constraints**: sh:datatype, sh:class
//! - **Enumeration**: sh:in (list of valid values)
//! - **OneOf constraints**: sh:oneOf (alternative values)
//!
//! ## Example
//!
//! ```text
//! @prefix sh: <http://www.w3.org/ns/shacl#> .
//! @prefix ex: <http://example.com/> .
//! @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
//!
//! ex:PersonShape
//!     a sh:NodeShape ;
//!     sh:targetClass ex:Person ;
//!     sh:property [
//!         sh:path ex:name ;
//!         sh:datatype xsd:string ;
//!         sh:minLength 1 ;
//!         sh:maxLength 100 ;
//!         sh:minCount 1 ;
//!         sh:maxCount 1 ;
//!     ] ;
//!     sh:property [
//!         sh:path ex:email ;
//!         sh:datatype xsd:string ;
//!         sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
//!     ] ;
//!     sh:property [
//!         sh:path ex:status ;
//!         sh:in ("active", "inactive", "pending") ;
//!     ] .
//! ```

use crate::error::{GgenAiError, Result};
use oxigraph::model::Term;
use oxigraph::store::Store;
use oxigraph::sparql::QueryResults;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Represents extracted SHACL constraints for a property
///
/// This struct holds all extracted constraint information from a SHACL property shape.
/// Only populated fields represent constraints that were found in the RDF graph.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SHACLConstraint {
    /// Minimum occurrence count (sh:minCount)
    pub min_count: Option<usize>,
    /// Maximum occurrence count (sh:maxCount)
    pub max_count: Option<usize>,
    /// Minimum string length (sh:minLength)
    pub min_length: Option<usize>,
    /// Maximum string length (sh:maxLength)
    pub max_length: Option<usize>,
    /// Regular expression pattern (sh:pattern)
    pub pattern: Option<String>,
    /// XSD datatype (sh:datatype) - e.g., "xsd:string", "xsd:int"
    pub datatype: Option<String>,
    /// Enumeration of valid values (sh:in)
    pub enum_values: Option<Vec<String>>,
    /// Alternative valid values (sh:oneOf)
    pub one_of_values: Option<Vec<String>>,
    /// Semantic type constraint (sh:class) - e.g., "ex:Product"
    pub semantic_type: Option<String>,
    /// Property path or name
    pub property_name: Option<String>,
}

impl SHACLConstraint {
    /// Create a new empty constraint
    pub fn new() -> Self {
        Self {
            min_count: None,
            max_count: None,
            min_length: None,
            max_length: None,
            pattern: None,
            datatype: None,
            enum_values: None,
            one_of_values: None,
            semantic_type: None,
            property_name: None,
        }
    }

    /// Check if this constraint has any restrictions
    pub fn is_empty(&self) -> bool {
        self.min_count.is_none()
            && self.max_count.is_none()
            && self.min_length.is_none()
            && self.max_length.is_none()
            && self.pattern.is_none()
            && self.datatype.is_none()
            && self.enum_values.is_none()
            && self.one_of_values.is_none()
            && self.semantic_type.is_none()
    }
}

impl Default for SHACLConstraint {
    fn default() -> Self {
        Self::new()
    }
}

/// SHACL constraint parser
///
/// This parser reads SHACL shapes from an RDF graph and extracts constraints
/// that can be used for code generation and validation.
pub struct SHACLParser<'a> {
    store: &'a Store,
}

impl<'a> SHACLParser<'a> {
    /// Create a new SHACL parser with access to an RDF store
    pub fn new(store: &'a Store) -> Self {
        Self { store }
    }

    /// Extract all property shapes from a node shape
    ///
    /// # Arguments
    ///
    /// * `shape_uri` - URI of the sh:NodeShape to extract properties from
    ///
    /// # Returns
    ///
    /// A map of property names to their constraints
    pub fn extract_properties(&self, shape_uri: &str) -> Result<BTreeMap<String, SHACLConstraint>> {
        let query = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>

            SELECT ?property ?path
            WHERE {{
                <{shape_uri}> sh:property ?property .
                ?property sh:path ?path .
            }}
            "#
        );

        let mut properties = BTreeMap::new();

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

                    if let Some(property) = solution.get("property") {
                        if let Some(path) = solution.get("path") {
                            let property_uri = property.to_string();
                            let prop_name = extract_local_name(&path.to_string());

                            let constraint = self.extract_constraint(&property_uri, &prop_name)?;
                            properties.insert(prop_name, constraint);
                        }
                    }
                }
            }
            _ => {}
        }

        Ok(properties)
    }

    /// Extract constraints from a single property shape
    ///
    /// # Arguments
    ///
    /// * `prop_shape_uri` - URI of the property shape
    /// * `property_name` - Name of the property being constrained
    ///
    /// # Returns
    ///
    /// A SHACLConstraint containing all extracted constraints
    pub fn extract_constraint(&self, prop_shape_uri: &str, property_name: &str) -> Result<SHACLConstraint> {
        let mut constraint = SHACLConstraint {
            property_name: Some(property_name.to_string()),
            ..Default::default()
        };

        // Extract cardinality constraints
        if let Ok(Some(count)) = self.extract_integer_property(prop_shape_uri, "sh:minCount") {
            constraint.min_count = Some(count as usize);
        }

        if let Ok(Some(count)) = self.extract_integer_property(prop_shape_uri, "sh:maxCount") {
            constraint.max_count = Some(count as usize);
        }

        // Extract string length constraints
        if let Ok(Some(length)) = self.extract_integer_property(prop_shape_uri, "sh:minLength") {
            constraint.min_length = Some(length as usize);
        }

        if let Ok(Some(length)) = self.extract_integer_property(prop_shape_uri, "sh:maxLength") {
            constraint.max_length = Some(length as usize);
        }

        // Extract regex pattern
        if let Ok(pattern) = self.extract_string_property(prop_shape_uri, "sh:pattern") {
            constraint.pattern = pattern;
        }

        // Extract datatype
        if let Ok(datatype) = self.extract_datatype(prop_shape_uri) {
            constraint.datatype = datatype;
        }

        // Extract enumeration values
        if let Ok(values) = self.extract_enum_values(prop_shape_uri) {
            constraint.enum_values = Some(values);
        }

        // Extract oneOf values
        if let Ok(values) = self.extract_one_of_values(prop_shape_uri) {
            constraint.one_of_values = Some(values);
        }

        // Extract semantic type (class constraint)
        if let Ok(semantic_type) = self.extract_string_property(prop_shape_uri, "sh:class") {
            constraint.semantic_type = semantic_type;
        }

        Ok(constraint)
    }

    /// Extract an integer property value
    ///
    /// # Arguments
    ///
    /// * `subject_uri` - URI of the subject
    /// * `property` - SHACL property name (with prefix, e.g., "sh:minCount")
    ///
    /// # Returns
    ///
    /// Optional integer value if found
    fn extract_integer_property(&self, subject_uri: &str, property: &str) -> Result<Option<i64>> {
        let query = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>

            SELECT ?value
            WHERE {{
                <{subject_uri}> {property} ?value .
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

                    if let Some(term) = solution.get("value") {
                        if let Term::Literal(literal) = term {
                            if let Ok(int_val) = literal.value().parse::<i64>() {
                                return Ok(Some(int_val));
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        Ok(None)
    }

    /// Extract a string property value
    ///
    /// # Arguments
    ///
    /// * `subject_uri` - URI of the subject
    /// * `property` - SHACL property name (with prefix, e.g., "sh:pattern")
    ///
    /// # Returns
    ///
    /// Optional string value if found
    fn extract_string_property(&self, subject_uri: &str, property: &str) -> Result<Option<String>> {
        let query = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>

            SELECT ?value
            WHERE {{
                <{subject_uri}> {property} ?value .
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

                    if let Some(term) = solution.get("value") {
                        match term {
                            Term::Literal(literal) => {
                                return Ok(Some(literal.value().to_string()));
                            }
                            Term::NamedNode(named_node) => {
                                return Ok(Some(named_node.as_str().to_string()));
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }

        Ok(None)
    }

    /// Extract datatype and map to Rust type
    ///
    /// # Arguments
    ///
    /// * `subject_uri` - URI of the property shape
    ///
    /// # Returns
    ///
    /// Optional string representation of the Rust type
    fn extract_datatype(&self, subject_uri: &str) -> Result<Option<String>> {
        let query = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

            SELECT ?datatype
            WHERE {{
                <{subject_uri}> sh:datatype ?datatype .
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

                    if let Some(term) = solution.get("datatype") {
                        if let Term::NamedNode(named_node) = term {
                            let datatype_uri = named_node.as_str();
                            let rust_type = map_xsd_to_rust_type(datatype_uri);
                            return Ok(Some(rust_type));
                        }
                    }
                }
            }
            _ => {}
        }

        Ok(None)
    }

    /// Extract enumeration values from sh:in list
    ///
    /// # Arguments
    ///
    /// * `subject_uri` - URI of the property shape
    ///
    /// # Returns
    ///
    /// Vector of enumeration values if sh:in is present
    fn extract_enum_values(&self, subject_uri: &str) -> Result<Vec<String>> {
        let query = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

            SELECT ?value
            WHERE {{
                <{subject_uri}> sh:in ?list .
                ?list rdf:rest* ?item .
                ?item rdf:first ?value .
            }}
            "#
        );

        let mut values = Vec::new();

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

                    if let Some(term) = solution.get("value") {
                        if let Term::Literal(literal) = term {
                            values.push(literal.value().to_string());
                        }
                    }
                }
            }
            _ => {}
        }

        Ok(values)
    }

    /// Extract oneOf values
    ///
    /// # Arguments
    ///
    /// * `subject_uri` - URI of the property shape
    ///
    /// # Returns
    ///
    /// Vector of oneOf values if present
    fn extract_one_of_values(&self, subject_uri: &str) -> Result<Vec<String>> {
        let query = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

            SELECT ?value
            WHERE {{
                <{subject_uri}> sh:oneOf ?list .
                ?list rdf:rest* ?item .
                ?item rdf:first ?value .
            }}
            "#
        );

        let mut values = Vec::new();

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

                    if let Some(term) = solution.get("value") {
                        if let Term::Literal(literal) = term {
                            values.push(literal.value().to_string());
                        }
                    }
                }
            }
            _ => {}
        }

        Ok(values)
    }
}

/// Map XSD datatype URIs to Rust type names
///
/// # Arguments
///
/// * `xsd_uri` - XSD datatype URI (e.g., "http://www.w3.org/2001/XMLSchema#string")
///
/// # Returns
///
/// Rust type name as string
pub fn map_xsd_to_rust_type(xsd_uri: &str) -> String {
    match xsd_uri {
        // String types
        "http://www.w3.org/2001/XMLSchema#string" => "String",
        "http://www.w3.org/2001/XMLSchema#normalizedString" => "String",
        "http://www.w3.org/2001/XMLSchema#token" => "String",
        "http://www.w3.org/2001/XMLSchema#language" => "String",
        "http://www.w3.org/2001/XMLSchema#Name" => "String",
        "http://www.w3.org/2001/XMLSchema#NCName" => "String",
        "http://www.w3.org/2001/XMLSchema#ENTITY" => "String",
        "http://www.w3.org/2001/XMLSchema#ID" => "String",
        "http://www.w3.org/2001/XMLSchema#IDREF" => "String",
        "http://www.w3.org/2001/XMLSchema#NMTOKEN" => "String",

        // Numeric types
        "http://www.w3.org/2001/XMLSchema#byte" => "i8",
        "http://www.w3.org/2001/XMLSchema#short" => "i16",
        "http://www.w3.org/2001/XMLSchema#int" => "i32",
        "http://www.w3.org/2001/XMLSchema#integer" => "i64",
        "http://www.w3.org/2001/XMLSchema#long" => "i64",
        "http://www.w3.org/2001/XMLSchema#unsignedByte" => "u8",
        "http://www.w3.org/2001/XMLSchema#unsignedShort" => "u16",
        "http://www.w3.org/2001/XMLSchema#unsignedInt" => "u32",
        "http://www.w3.org/2001/XMLSchema#unsignedLong" => "u64",
        "http://www.w3.org/2001/XMLSchema#positiveInteger" => "u64",
        "http://www.w3.org/2001/XMLSchema#negativeInteger" => "i64",
        "http://www.w3.org/2001/XMLSchema#nonPositiveInteger" => "i64",
        "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" => "u64",

        // Floating point
        "http://www.w3.org/2001/XMLSchema#decimal" => "f64",
        "http://www.w3.org/2001/XMLSchema#double" => "f64",
        "http://www.w3.org/2001/XMLSchema#float" => "f32",

        // Boolean
        "http://www.w3.org/2001/XMLSchema#boolean" => "bool",

        // Date/Time types (mapped to chrono types)
        "http://www.w3.org/2001/XMLSchema#dateTime" => "chrono::DateTime<chrono::Utc>",
        "http://www.w3.org/2001/XMLSchema#date" => "chrono::NaiveDate",
        "http://www.w3.org/2001/XMLSchema#time" => "chrono::NaiveTime",
        "http://www.w3.org/2001/XMLSchema#duration" => "chrono::Duration",
        "http://www.w3.org/2001/XMLSchema#gYear" => "u16",
        "http://www.w3.org/2001/XMLSchema#gYearMonth" => "(u16, u8)", // (year, month)
        "http://www.w3.org/2001/XMLSchema#gDay" => "u8", // day of month
        "http://www.w3.org/2001/XMLSchema#gMonth" => "u8", // month value
        "http://www.w3.org/2001/XMLSchema#gMonthDay" => "(u8, u8)", // (month, day)

        // Binary types
        "http://www.w3.org/2001/XMLSchema#hexBinary" => "Vec<u8>",
        "http://www.w3.org/2001/XMLSchema#base64Binary" => "Vec<u8>",

        // URI/IRI types
        "http://www.w3.org/2001/XMLSchema#anyURI" => "String",

        // Default fallback
        _ => "String",
    }
    .to_string()
}

/// Extract the local name from a URI
///
/// # Arguments
///
/// * `uri` - Full URI (e.g., "http://example.com/name")
///
/// # Returns
///
/// Local name after the last # or /
pub fn extract_local_name(uri: &str) -> String {
    if let Some(pos) = uri.rfind('#') {
        uri[pos + 1..].to_string()
    } else if let Some(pos) = uri.rfind('/') {
        uri[pos + 1..].to_string()
    } else {
        uri.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_xsd_string() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#string"),
            "String"
        );
    }

    #[test]
    fn test_map_xsd_int() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#int"),
            "i32"
        );
    }

    #[test]
    fn test_map_xsd_integer() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#integer"),
            "i64"
        );
    }

    #[test]
    fn test_map_xsd_long() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#long"),
            "i64"
        );
    }

    #[test]
    fn test_map_xsd_float() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#float"),
            "f32"
        );
    }

    #[test]
    fn test_map_xsd_double() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#double"),
            "f64"
        );
    }

    #[test]
    fn test_map_xsd_boolean() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#boolean"),
            "bool"
        );
    }

    #[test]
    fn test_map_xsd_unsigned_types() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#unsignedInt"),
            "u32"
        );
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#unsignedLong"),
            "u64"
        );
    }

    #[test]
    fn test_map_xsd_binary() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#base64Binary"),
            "Vec<u8>"
        );
    }

    #[test]
    fn test_map_xsd_uri() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#anyURI"),
            "String"
        );
    }

    #[test]
    fn test_map_xsd_unknown_type_defaults_to_string() {
        assert_eq!(
            map_xsd_to_rust_type("http://example.com/UnknownType"),
            "String"
        );
    }

    #[test]
    fn test_extract_local_name_with_hash() {
        assert_eq!(extract_local_name("http://example.com/schema#name"), "name");
    }

    #[test]
    fn test_extract_local_name_with_slash() {
        assert_eq!(
            extract_local_name("http://example.com/schema/name"),
            "name"
        );
    }

    #[test]
    fn test_extract_local_name_hash_takes_precedence() {
        assert_eq!(
            extract_local_name("http://example.com/schema#path/with/slash"),
            "path/with/slash"
        );
    }

    #[test]
    fn test_extract_local_name_no_separator() {
        assert_eq!(extract_local_name("simplename"), "simplename");
    }

    #[test]
    fn test_shacl_constraint_new() {
        let constraint = SHACLConstraint::new();
        assert!(constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_default() {
        let constraint = SHACLConstraint::default();
        assert!(constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_not_empty_with_min_count() {
        let mut constraint = SHACLConstraint::new();
        constraint.min_count = Some(1);
        assert!(!constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_not_empty_with_pattern() {
        let mut constraint = SHACLConstraint::new();
        constraint.pattern = Some(r"^[a-zA-Z]+$".to_string());
        assert!(!constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_not_empty_with_enum() {
        let mut constraint = SHACLConstraint::new();
        constraint.enum_values = Some(vec!["a".to_string(), "b".to_string()]);
        assert!(!constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_not_empty_with_datatype() {
        let mut constraint = SHACLConstraint::new();
        constraint.datatype = Some("String".to_string());
        assert!(!constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_not_empty_with_min_length() {
        let mut constraint = SHACLConstraint::new();
        constraint.min_length = Some(5);
        assert!(!constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_not_empty_with_max_length() {
        let mut constraint = SHACLConstraint::new();
        constraint.max_length = Some(100);
        assert!(!constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_not_empty_with_semantic_type() {
        let mut constraint = SHACLConstraint::new();
        constraint.semantic_type = Some("ex:Product".to_string());
        assert!(!constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_not_empty_with_one_of() {
        let mut constraint = SHACLConstraint::new();
        constraint.one_of_values = Some(vec!["x".to_string()]);
        assert!(!constraint.is_empty());
    }

    #[test]
    fn test_shacl_constraint_serialization() {
        let mut constraint = SHACLConstraint::new();
        constraint.min_count = Some(1);
        constraint.max_count = Some(1);
        constraint.min_length = Some(5);
        constraint.max_length = Some(100);
        constraint.pattern = Some(r"^[a-z]+$".to_string());
        constraint.datatype = Some("String".to_string());
        constraint.enum_values = Some(vec!["a".to_string(), "b".to_string()]);
        constraint.property_name = Some("email".to_string());

        let json = serde_json::to_string(&constraint).expect("serialization failed");
        let deserialized: SHACLConstraint =
            serde_json::from_str(&json).expect("deserialization failed");

        assert_eq!(constraint, deserialized);
    }

    #[test]
    fn test_map_all_signed_integer_types() {
        let test_cases = vec![
            ("http://www.w3.org/2001/XMLSchema#byte", "i8"),
            ("http://www.w3.org/2001/XMLSchema#short", "i16"),
            ("http://www.w3.org/2001/XMLSchema#int", "i32"),
            ("http://www.w3.org/2001/XMLSchema#long", "i64"),
            ("http://www.w3.org/2001/XMLSchema#integer", "i64"),
        ];

        for (xsd, expected) in test_cases {
            assert_eq!(map_xsd_to_rust_type(xsd), expected);
        }
    }

    #[test]
    fn test_map_all_unsigned_integer_types() {
        let test_cases = vec![
            ("http://www.w3.org/2001/XMLSchema#unsignedByte", "u8"),
            ("http://www.w3.org/2001/XMLSchema#unsignedShort", "u16"),
            ("http://www.w3.org/2001/XMLSchema#unsignedInt", "u32"),
            ("http://www.w3.org/2001/XMLSchema#unsignedLong", "u64"),
        ];

        for (xsd, expected) in test_cases {
            assert_eq!(map_xsd_to_rust_type(xsd), expected);
        }
    }

    #[test]
    fn test_map_special_integer_types() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#positiveInteger"),
            "u64"
        );
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#negativeInteger"),
            "i64"
        );
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#nonPositiveInteger"),
            "i64"
        );
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#nonNegativeInteger"),
            "u64"
        );
    }

    #[test]
    fn test_map_decimal_types() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#decimal"),
            "f64"
        );
    }

    #[test]
    fn test_map_string_subtypes() {
        let test_cases = vec![
            ("http://www.w3.org/2001/XMLSchema#normalizedString", "String"),
            ("http://www.w3.org/2001/XMLSchema#token", "String"),
            ("http://www.w3.org/2001/XMLSchema#language", "String"),
            ("http://www.w3.org/2001/XMLSchema#Name", "String"),
            ("http://www.w3.org/2001/XMLSchema#NCName", "String"),
        ];

        for (xsd, expected) in test_cases {
            assert_eq!(map_xsd_to_rust_type(xsd), expected);
        }
    }

    #[test]
    fn test_shacl_parser_creation() {
        use oxigraph::store::Store;
        let store = Store::new().expect("Failed to create store");
        let _parser = SHACLParser::new(&store);
    }

    #[test]
    fn test_shacl_constraint_with_all_fields() {
        let constraint = SHACLConstraint {
            min_count: Some(1),
            max_count: Some(5),
            min_length: Some(10),
            max_length: Some(255),
            pattern: Some(r"^[A-Z][a-z]*$".to_string()),
            datatype: Some("String".to_string()),
            enum_values: Some(vec!["active".to_string(), "inactive".to_string()]),
            one_of_values: Some(vec!["option1".to_string(), "option2".to_string()]),
            semantic_type: Some("http://example.com/Person".to_string()),
            property_name: Some("status".to_string()),
        };

        assert!(!constraint.is_empty());
        assert_eq!(constraint.min_count, Some(1));
        assert_eq!(constraint.max_count, Some(5));
        assert_eq!(constraint.min_length, Some(10));
        assert_eq!(constraint.max_length, Some(255));
        assert!(constraint.pattern.is_some());
        assert!(constraint.datatype.is_some());
        assert_eq!(constraint.enum_values.as_ref().map(|v| v.len()), Some(2));
        assert_eq!(constraint.one_of_values.as_ref().map(|v| v.len()), Some(2));
        assert!(constraint.semantic_type.is_some());
        assert!(constraint.property_name.is_some());
    }

    // ===== DateTime Type Mapping Tests =====

    #[test]
    fn test_map_xsd_datetime() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#dateTime"),
            "chrono::DateTime<chrono::Utc>"
        );
    }

    #[test]
    fn test_map_xsd_date() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#date"),
            "chrono::NaiveDate"
        );
    }

    #[test]
    fn test_map_xsd_time() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#time"),
            "chrono::NaiveTime"
        );
    }

    #[test]
    fn test_map_xsd_duration() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#duration"),
            "chrono::Duration"
        );
    }

    #[test]
    fn test_map_xsd_gyear() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#gYear"),
            "u16"
        );
    }

    #[test]
    fn test_map_xsd_gyear_month() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#gYearMonth"),
            "(u16, u8)"
        );
    }

    #[test]
    fn test_map_xsd_gday() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#gDay"),
            "u8"
        );
    }

    #[test]
    fn test_map_xsd_gmonth() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#gMonth"),
            "u8"
        );
    }

    #[test]
    fn test_map_xsd_gmonth_day() {
        assert_eq!(
            map_xsd_to_rust_type("http://www.w3.org/2001/XMLSchema#gMonthDay"),
            "(u8, u8)"
        );
    }

    #[test]
    fn test_map_all_datetime_types() {
        let test_cases = vec![
            ("http://www.w3.org/2001/XMLSchema#dateTime", "chrono::DateTime<chrono::Utc>"),
            ("http://www.w3.org/2001/XMLSchema#date", "chrono::NaiveDate"),
            ("http://www.w3.org/2001/XMLSchema#time", "chrono::NaiveTime"),
            ("http://www.w3.org/2001/XMLSchema#duration", "chrono::Duration"),
            ("http://www.w3.org/2001/XMLSchema#gYear", "u16"),
            ("http://www.w3.org/2001/XMLSchema#gYearMonth", "(u16, u8)"),
            ("http://www.w3.org/2001/XMLSchema#gDay", "u8"),
            ("http://www.w3.org/2001/XMLSchema#gMonth", "u8"),
            ("http://www.w3.org/2001/XMLSchema#gMonthDay", "(u8, u8)"),
        ];

        for (xsd, expected) in test_cases {
            assert_eq!(map_xsd_to_rust_type(xsd), expected, "Failed for {}", xsd);
        }
    }
}
