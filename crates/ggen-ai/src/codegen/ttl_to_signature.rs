//! TTL to DSPy Signature Transpiler
//!
//! Converts Turtle ontologies with SHACL shapes into DSPy-compatible Signature types.
//!
//! This module implements the core traversal and shape extraction logic for converting
//! RDF ontologies defined in Turtle format into type-safe Signature specifications.
//!
//! # Features
//!
//! - **SHACL Shape Discovery**: Find all RDF classes with SHACL shapes
//! - **Property Extraction**: Traverse SHACL property shapes and extract constraints
//! - **Field Classification**: Distinguish input vs output fields via cns:outputField
//! - **Type Inference**: Extract XSD datatypes and map to Rust types
//! - **Safe Naming**: Convert IRIs to valid Rust identifiers with collision detection
//! - **Graceful Degradation**: Handle missing properties without panicking

use crate::dspy::{InputField, OutputField, Signature};
use crate::GgenAiError;
use regex::Regex;
use std::collections::HashSet;

/// TTLToSignatureTranspiler converts RDF ontologies to DSPy Signatures
///
/// Handles SHACL shape traversal, property extraction, and type inference.
#[derive(Debug)]
pub struct TTLToSignatureTranspiler {
    /// Track seen field names for collision detection
    seen_field_names: HashSet<String>,
    /// Count of generated signatures
    signature_count: usize,
}

/// Extracted SHACL property shape information
#[derive(Debug, Clone)]
pub struct PropertyShape {
    /// IRI of the property shape
    pub iri: String,
    /// Property path (local name)
    pub path: String,
    /// Description from rdfs:comment or sh:description
    pub description: Option<String>,
    /// XSD datatype if present
    pub datatype: Option<String>,
    /// Whether this is marked as an output field
    pub is_output: bool,
}

impl TTLToSignatureTranspiler {
    /// Create a new TTL to Signature transpiler
    pub fn new() -> Self {
        Self {
            seen_field_names: HashSet::new(),
            signature_count: 0,
        }
    }

    /// Extract local name from an IRI safely
    fn safe_local_name(&self, iri: &str) -> String {
        let slash_idx = iri.rfind('/');
        let hash_idx = iri.rfind('#');

        let idx = match (slash_idx, hash_idx) {
            (Some(s), Some(h)) => s.max(h),
            (Some(s), None) => s,
            (None, Some(h)) => h,
            (None, None) => return iri.to_string(),
        };

        iri.get(idx + 1..).unwrap_or("").to_string()
    }

    /// Convert a name to snake_case for use as a Rust identifier
    fn snake_case(&self, name: &str) -> String {
        let mut result = name.to_string();

        result = Regex::new(r"[-\s]+")
            .unwrap()
            .replace_all(&result, "_")
            .to_string();

        result = Regex::new(r"([a-z])([A-Z])")
            .unwrap()
            .replace_all(&result, "${1}_${2}")
            .to_string();

        result = result.to_lowercase();

        result = Regex::new(r"_+")
            .unwrap()
            .replace_all(&result, "_")
            .to_string();

        result = result.trim_matches('_').to_string();

        if result.chars().next().map_or(false, |c| c.is_numeric()) {
            result = format!("field_{}", result);
        }

        if result.is_empty() {
            result = "unnamed_field".to_string();
        }

        result
    }

    /// Check for field name collisions and resolve them
    fn check_field_collision(&mut self, mut py_name: String) -> String {
        let reserved_names = [
            "metadata", "instructions", "demos", "signature", "config",
            "int", "str", "bool", "float", "list", "dict", "set", "tuple",
            "None", "True", "False", "class", "def", "return", "import",
        ];

        if reserved_names.contains(&py_name.as_str()) {
            py_name = format!("custom_{}", py_name);
        }

        let original = py_name.clone();
        let mut counter = 1;
        while self.seen_field_names.contains(&py_name) {
            py_name = format!("{}_{}", original, counter);
            counter += 1;
        }

        self.seen_field_names.insert(py_name.clone());
        py_name
    }

    /// Extract XSD datatype and map to Rust type annotation
    fn extract_datatype(&self, datatype: &str) -> &'static str {
        match datatype {
            s if s.contains("string") => "String",
            s if s.contains("boolean") => "bool",
            s if s.contains("integer") || s.contains("int") || s.contains("long") => "i32",
            s if s.contains("float") || s.contains("double") || s.contains("decimal") => "f32",
            _ => "String",
        }
    }

    /// Find all RDF classes with SHACL shapes
    pub fn find_classes_with_shapes(
        &self,
        store: &oxigraph::store::Store,
    ) -> crate::Result<Vec<String>> {
        let mut classes = Vec::new();

        let query = "
            PREFIX sh: <http://www.w3.org/ns/shacl#>
            SELECT DISTINCT ?class WHERE {
                ?shape sh:targetClass ?class .
            }
        ";

        let query_results = store
            .query(query)
            .map_err(|e| GgenAiError::Configuration(format!("SPARQL query failed: {}", e)))?;

        for result in query_results {
            if let Ok(solutions) = result {
                for binding in &solutions.bindings {
                    if let Some((_, value)) = binding.iter().next() {
                        classes.push(value.to_string());
                    }
                }
            }
        }

        Ok(classes)
    }

    /// Find property shapes for a given class
    pub fn find_property_shapes(
        &self,
        class_iri: &str,
        store: &oxigraph::store::Store,
    ) -> crate::Result<Vec<PropertyShape>> {
        let mut prop_shapes = Vec::new();

        let query1 = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            SELECT ?propShape ?path ?description WHERE {{
                ?propShape sh:targetClass <{}> ;
                           sh:path ?path .
                OPTIONAL {{ ?propShape rdfs:comment ?description . }}
            }}
            "#,
            class_iri
        );

        if let Ok(query_results) = store.query(&query1) {
            for result in query_results {
                if let Ok(solutions) = result {
                    for binding in &solutions.bindings {
                        if let (Some(path), _) = binding.iter().take(2).collect::<(Option<_>, Option<_>)>() {
                            prop_shapes.push(PropertyShape {
                                iri: binding.iter().next().map(|(_, v)| v.to_string()).unwrap_or_default(),
                                path: self.safe_local_name(&path.to_string()),
                                description: binding
                                    .iter()
                                    .nth(2)
                                    .and_then(|(_, v)| Some(v.to_string())),
                                datatype: None,
                                is_output: false,
                            });
                        }
                    }
                }
            }
        }

        let query2 = format!(
            r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            SELECT ?propShape ?path ?description WHERE {{
                ?nodeShape sh:targetClass <{}> ;
                           sh:property ?propShape .
                ?propShape sh:path ?path .
                OPTIONAL {{ ?propShape rdfs:comment ?description . }}
            }}
            "#,
            class_iri
        );

        if let Ok(query_results) = store.query(&query2) {
            for result in query_results {
                if let Ok(solutions) = result {
                    for binding in &solutions.bindings {
                        if let (Some(path), _) = binding.iter().take(2).collect::<(Option<_>, Option<_>)>() {
                            prop_shapes.push(PropertyShape {
                                iri: binding.iter().next().map(|(_, v)| v.to_string()).unwrap_or_default(),
                                path: self.safe_local_name(&path.to_string()),
                                description: binding
                                    .iter()
                                    .nth(2)
                                    .and_then(|(_, v)| Some(v.to_string())),
                                datatype: None,
                                is_output: false,
                            });
                        }
                    }
                }
            }
        }

        Ok(prop_shapes)
    }

    /// Check if a property shape is marked as an output field
    fn is_output_field(&self, prop_shape: &PropertyShape, store: &oxigraph::store::Store) -> bool {
        let query = format!(
            r#"
            PREFIX cns: <http://cns.io/ontology#>
            ASK {{
                <{}> cns:outputField ?value .
                FILTER (?value = true || ?value = "true" || ?value = "1" || ?value = "yes")
            }}
            "#,
            prop_shape.iri
        );

        if let Ok(result) = store.query(&query) {
            if let Ok(solutions) = result.into_boolean_result() {
                if solutions {
                    return true;
                }
            }
        }

        if let Some(ref desc) = prop_shape.description {
            if desc.to_lowercase().contains("output") {
                return true;
            }
        }

        false
    }

    /// Build all signatures from an RDF store
    pub fn build_signatures(&mut self, store: &oxigraph::store::Store) -> crate::Result<Vec<Signature>> {
        let mut signatures = Vec::new();

        let classes = self.find_classes_with_shapes(store)?;

        for class_iri in classes {
            let class_name = self.safe_local_name(&class_iri);
            let signature_name = format!("{}Signature", class_name);

            self.seen_field_names.clear();

            let prop_shapes = self.find_property_shapes(&class_iri, store)?;

            if prop_shapes.is_empty() {
                continue;
            }

            let mut input_fields = Vec::new();
            let mut output_fields = Vec::new();

            for prop_shape in prop_shapes {
                let prop_name = self.safe_local_name(&prop_shape.path);
                let mut py_name = self.snake_case(&prop_name);
                py_name = self.check_field_collision(py_name);

                let description = prop_shape
                    .description
                    .unwrap_or_else(|| format!("{} property", prop_name));

                let type_annotation = match &prop_shape.datatype {
                    Some(dt) => self.extract_datatype(dt).to_string(),
                    None => "String".to_string(),
                };

                let is_output = self.is_output_field(&prop_shape, store);

                if is_output {
                    output_fields.push(OutputField::new(
                        py_name,
                        description,
                        type_annotation,
                    ));
                } else {
                    input_fields.push(InputField::new(
                        py_name,
                        description,
                        type_annotation,
                    ));
                }
            }

            if output_fields.is_empty() {
                output_fields.push(OutputField::new(
                    "result",
                    "Generated result",
                    "String",
                ));
            }

            let signature = Signature::new(signature_name, format!("DSPy Signature for {}", class_name))
                .with_inputs(input_fields)
                .with_outputs(output_fields);

            signatures.push(signature);
            self.signature_count += 1;
        }

        Ok(signatures)
    }

    /// Get the count of signatures generated
    pub fn signature_count(&self) -> usize {
        self.signature_count
    }
}

impl Default for TTLToSignatureTranspiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_local_name_with_hash() {
        let transpiler = TTLToSignatureTranspiler::new();
        let iri = "http://cns.io/ontology#MyClass";
        assert_eq!(transpiler.safe_local_name(iri), "MyClass");
    }

    #[test]
    fn test_safe_local_name_with_slash() {
        let transpiler = TTLToSignatureTranspiler::new();
        let iri = "http://example.com/ontology/MyClass";
        assert_eq!(transpiler.safe_local_name(iri), "MyClass");
    }

    #[test]
    fn test_safe_local_name_no_delimiter() {
        let transpiler = TTLToSignatureTranspiler::new();
        let iri = "SimpleClass";
        assert_eq!(transpiler.safe_local_name(iri), "SimpleClass");
    }

    #[test]
    fn test_safe_local_name_prefers_hash_over_slash() {
        let transpiler = TTLToSignatureTranspiler::new();
        let iri = "http://example.com/ontology#MyClass";
        assert_eq!(transpiler.safe_local_name(iri), "MyClass");
    }

    #[test]
    fn test_snake_case_with_hyphens() {
        let transpiler = TTLToSignatureTranspiler::new();
        let name = "my-property-name";
        assert_eq!(transpiler.snake_case(name), "my_property_name");
    }

    #[test]
    fn test_snake_case_with_camel_case() {
        let transpiler = TTLToSignatureTranspiler::new();
        let name = "MyPropertyName";
        assert_eq!(transpiler.snake_case(name), "my_property_name");
    }

    #[test]
    fn test_snake_case_mixed() {
        let transpiler = TTLToSignatureTranspiler::new();
        let name = "My-Property-Name";
        assert_eq!(transpiler.snake_case(name), "my_property_name");
    }

    #[test]
    fn test_snake_case_with_leading_digit() {
        let transpiler = TTLToSignatureTranspiler::new();
        let name = "1stProperty";
        assert_eq!(transpiler.snake_case(name), "field_1_st_property");
    }

    #[test]
    fn test_snake_case_empty_string() {
        let transpiler = TTLToSignatureTranspiler::new();
        let name = "";
        assert_eq!(transpiler.snake_case(name), "unnamed_field");
    }

    #[test]
    fn test_check_field_collision_avoids_reserved() {
        let mut transpiler = TTLToSignatureTranspiler::new();
        let name = "class".to_string();
        let result = transpiler.check_field_collision(name);
        assert_eq!(result, "custom_class");
    }

    #[test]
    fn test_check_field_collision_handles_duplicates() {
        let mut transpiler = TTLToSignatureTranspiler::new();
        let name1 = "field_name".to_string();
        let name2 = "field_name".to_string();

        let result1 = transpiler.check_field_collision(name1);
        let result2 = transpiler.check_field_collision(name2);

        assert_eq!(result1, "field_name");
        assert_eq!(result2, "field_name_1");
    }

    #[test]
    fn test_extract_datatype_string() {
        let transpiler = TTLToSignatureTranspiler::new();
        let dt = "http://www.w3.org/2001/XMLSchema#string";
        assert_eq!(transpiler.extract_datatype(dt), "String");
    }

    #[test]
    fn test_extract_datatype_integer() {
        let transpiler = TTLToSignatureTranspiler::new();
        let dt = "http://www.w3.org/2001/XMLSchema#integer";
        assert_eq!(transpiler.extract_datatype(dt), "i32");
    }

    #[test]
    fn test_extract_datatype_boolean() {
        let transpiler = TTLToSignatureTranspiler::new();
        let dt = "http://www.w3.org/2001/XMLSchema#boolean";
        assert_eq!(transpiler.extract_datatype(dt), "bool");
    }

    #[test]
    fn test_extract_datatype_float() {
        let transpiler = TTLToSignatureTranspiler::new();
        let dt = "http://www.w3.org/2001/XMLSchema#float";
        assert_eq!(transpiler.extract_datatype(dt), "f32");
    }

    #[test]
    fn test_property_shape_creation() {
        let shape = PropertyShape {
            iri: "http://example.com/shape#MyShape".to_string(),
            path: "myProperty".to_string(),
            description: Some("A test property".to_string()),
            datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
            is_output: false,
        };

        assert_eq!(shape.path, "myProperty");
        assert_eq!(shape.description, Some("A test property".to_string()));
        assert!(!shape.is_output);
    }

    #[test]
    fn test_transpiler_creation() {
        let transpiler = TTLToSignatureTranspiler::new();
        assert_eq!(transpiler.signature_count(), 0);
    }

    #[test]
    fn test_transpiler_default() {
        let transpiler = TTLToSignatureTranspiler::default();
        assert_eq!(transpiler.signature_count(), 0);
    }
}
